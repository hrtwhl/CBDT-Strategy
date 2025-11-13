# =========================================================
# SCRIPT 4: CREATE FEATURE FINGERPRINTS
#
# PURPOSE:
# This script transforms the raw weekly master table into
# the "feature vector" (or "fingerprint") for each asset
# at each point in time. This is the core input for
# the analogue-finding engine (Script 5).
#
# We will create two sets of features as described by
# Moritz Heiden:
# 1. Price Dynamics: Momentum, Volatility, Drawdown
#    (Specific to each asset)
# 2. Macro Context: Levels and Changes in macro factors
#    (Global, the same for all assets on a given date)
#
# =========================================================

# --- 1. Load Data and Libraries ---

# Clean workspace, keeping only necessary objects from sourced scripts
rm(list=setdiff(ls(), c("assets_long", "assets_wide", "macro_long", "macro_wide", "etfs")))

source("1_getETFData.R")
source("2_getMacroData.R")
load("master_table_weekly.RData")

# Load necessary libraries
required <- c("dplyr", "tidyr", "slider", "purrr")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(required, library, character.only = TRUE))

cat("--- Script 4: Create Features --- \n")
cat("Loaded 'master_table_weekly' with", nrow(master_table_weekly), "rows.\n\n")

# =========================================================
# --- 2. Configuration & Parameter Decisions ---
#
# This is a critical step. We need to define the lookback
# periods for our features. Heiden mentions 4, 8, 12-week
# *horizons* (for forward returns) but is less specific
# on feature *lookbacks*.
#
# I propose the following standard lookbacks, which we
# can easily change later.
#
# =========================================================

config <- list(
  # --- Price Dynamics Lookbacks (in weeks) ---
  # We'll calculate momentum over 1m, 3m, 6m, 12m
  price_mom_lookbacks = c(4, 12, 26, 52),

  # Volatility lookbacks (12w = 3m, 52w = 12m)
  price_vol_lookbacks = c(12, 52),

  # Drawdown lookback (relative to 1-year high)
  price_dd_lookback = 52,

  # --- Macro Context Lookbacks (in weeks) ---
  # We'll calculate 3-month and 12-month *changes*
  # in macro factors.
  macro_chg_lookbacks = c(13, 52),

  # --- Standardization ---
  # Lookback for rolling Z-score (in weeks).
  # 3 years = 156 weeks. This is a robust default.
  zscore_rolling_window = 156
)

cat("--- Configuration --- \n")
cat("Price Momentum Lookbacks:", config$price_mom_lookbacks, "\n")
cat("Macro Change Lookbacks:", config$macro_chg_lookbacks, "\n")
cat("Z-Score Rolling Window:", config$zscore_rolling_window, "weeks\n\n")


# Get asset and macro ticker lists for processing
asset_tickers <- etfs$asset
macro_tickers <- c(names(fred_series), yf_futures$series)

# =========================================================
# --- 3. Create Price Dynamics Features (Asset-Specific) ---
#
# We'll convert the wide `master_table_weekly` to a long
# format to easily calculate rolling features by asset.
# =========================================================

# 3.1 Get 1-week log returns (base for volatility)
price_data_long <- master_table_weekly %>%
  select(date, all_of(asset_tickers)) %>%
  pivot_longer(
    cols = -date,
    names_to = "asset",
    values_to = "price"
  ) %>%
  group_by(asset) %>%
  arrange(date) %>%
  mutate(
    # Log returns are better for calculations
    ret_1w = log(price) - lag(price, 1)
  )

# 3.2 Calculate features
# We use slider::slide_dbl for rolling calculations
# and `purrr::map_dfc` to dynamically create columns
# for each lookback in our config.

price_features <- price_data_long %>%
  mutate(
    # --- Momentum Features ---
    # Create columns like: mom_4w, mom_12w, ...
    map_dfc(config$price_mom_lookbacks, ~ {
      set_names(
        list(price / lag(price, .x) - 1),
        paste0("mom_", .x, "w")
      )
    }),

    # --- Volatility Features ---
    # Create columns like: vol_12w, vol_52w, ...
    map_dfc(config$price_vol_lookbacks, ~ {
      set_names(
        list(
          slider::slide_dbl(ret_1w, ~sd(.x, na.rm = TRUE), .before = .x - 1, .complete = TRUE)
        ),
        paste0("vol_", .x, "w")
      )
    }),

    # --- Drawdown Feature ---
    # (Price relative to N-week high)
    hi_n_week = slider::slide_dbl(price, ~max(.x, na.rm = TRUE), .before = config$price_dd_lookback - 1, .complete = TRUE),
    drawdown = (price / hi_n_week) - 1
  ) %>%
  # Clean up intermediate columns
  select(-price, -ret_1w, -hi_n_week) %>%
  ungroup()

cat("--- Price Dynamics Features --- \n")
cat("Created", (length(config$price_mom_lookbacks) +
                 length(config$price_vol_lookbacks) + 1),
    "price features for", length(asset_tickers), "assets.\n")
# cat("Columns:", colnames(price_features), "\n\n") # Too long to print


# =========================================================
# --- 4. Create Macro Context Features (Global) ---
#
# We will create two types of macro features:
# 1. Levels: The raw (point-in-time) values.
# 2. Changes: The (N-week) change in those values.
#
# This gives the model context on both the *current state*
# (e.g., "curve is inverted") and the *direction*
# (e.g., "curve is steepening fast").
# =========================================================

macro_features <- master_table_weekly %>%
  select(date, all_of(macro_tickers))

# 4.1 Calculate Macro Changes
# We use `purrr::map_dfc` again to create `_chg_13w`,
# `_chg_52w`, etc. for every macro ticker.
macro_changes <- map(config$macro_chg_lookbacks, ~ {
  lookback <- .x
  macro_features %>%
    select(-date) %>% # Keep only ticker columns
    mutate(
      across(everything(),
             ~ .x - lag(.x, lookback),
             .names = "{.col}_chg_{lookback}w")
    )
}) %>%
  bind_cols() # Combine the dataframes (chg_13w, chg_52w, ...)

# 4.2 Combine Levels and Changes
# Our final macro feature table includes the original levels
# AND the new "change" features.
macro_features_final <- bind_cols(macro_features, macro_changes)

cat("--- Macro Context Features --- \n")
cat("Created", (ncol(macro_features_final) - 1), "total macro features (levels + changes).\n")
cat("Head of macro features:\n")
print(head(macro_features_final[, 1:6]))
cat("\n")


# =========================================================
# --- 5. Combine & Standardize (The "Fingerprint") ---
#
# Now we join the asset-specific price features with the
# global macro features.
# =========================================================

# 5.1 Join
feature_table_unstandardized <- price_features %>%
  left_join(macro_features_final, by = "date") %>%
  # Drop all initial rows with NAs from lookback calculations
  na.omit() %>%
  arrange(asset, date)

cat("--- Combined Feature Table (Unstandardized) --- \n")
cat("Final Dimensions:", dim(feature_table_unstandardized), "\n")
cat("Date Range:", as.character(min(feature_table_unstandardized$date)),
    "to", as.character(max(feature_table_unstandardized$date)), "\n\n")

# 5.2 Standardize (Rolling Z-Score)
#
# !! --- IMPORTANT CORRECTION --- !!
#
# We are replacing the full-sample Z-score with a
# *rolling Z-score* to prevent lookahead bias.
#
# METHOD: We will standardize each feature based on its
# mean and standard deviation over the *past N weeks*.
# N = zscore_rolling_window (e.g., 156 weeks / 3 years)
#
# This ensures our model only uses information available
# at that point in time.
#
# =========================================================

# Helper function for rolling Z-score
# We use .before = window_size - 1 to get a N-week window
# (e.g., .before=155 includes the current week + 155 past weeks = 156 total)
# .complete = TRUE ensures we only start calculating after a full window.
rolling_zscore <- function(x, window_size = 156) {
  rolling_mean <- slider::slide_dbl(
    x,
    ~mean(.x, na.rm = TRUE),
    .before = window_size - 1,
    .complete = TRUE
  )
  rolling_sd <- slider::slide_dbl(
    x,
    ~sd(.x, na.rm = TRUE),
    .before = window_size - 1,
    .complete = TRUE
  )

  # Calculate Z-score
  z <- (x - rolling_mean) / rolling_sd

  # Handle cases where rolling_sd is 0 (returns NaN, which is correct)
  # Infinite can happen if x == rolling_mean and sd == 0
  z[is.infinite(z)] <- NA
  return(z)
}

cat("--- Standardizing Features (Rolling Z-Score) --- \n")
cat("Rolling window:", config$zscore_rolling_window, "weeks\n")

standardized_feature_table <- feature_table_unstandardized %>%
  # We must group by asset to scale price features
  # relative to their own history.
  group_by(asset) %>%
  arrange(date) %>% # Ensure date is sorted within group
  mutate(
    # Standardize all numeric feature columns
    across(
      .cols = where(is.numeric) & !all_of("date"),
      .fns = ~ rolling_zscore(.x, config$zscore_rolling_window)
    )
  ) %>%
  ungroup() %>%
  # The rolling z-score with .complete=TRUE will create NAs
  # for the initial "warm-up" period (first 3 years).
  # na.omit() will also drop any rows where sd=0 (NaNs).
  na.omit()

cat("--- Standardized Feature Table (Rolling Z-Score) --- \n")
cat("Final Dimensions:", dim(standardized_feature_table), "\n")
cat("Date Range:", as.character(min(standardZized_feature_table$date)),
    "to", as.character(max(standardized_feature_table$date)), "\n\n")

# =========================================================
# --- 6. Save Outputs ---
# =========================================================

# We save both tables. The standardized one is for the
# backtest, and the unstandardized one is for analysis
# and charting (it's easier to interpret).

save(
  standardized_feature_table,
  file = "standardized_feature_table.RData"
)
save(
  feature_table_unstandardized,
  file = "feature_table_unstandardized.RData"
)

cat(
  "--- Script 4 Complete --- \n",
  "'standardized_feature_table.RData' and\n",
  "'feature_table_unstandardIZED.RData' are saved.\n"
)
