
# Script that adds standardized price data for charts in later run live

# =========================================================
# SCRIPT 4: CREATE FEATURE FINGERPRINTS
#
# PURPOSE:
# This script transforms the raw weekly master table into
# the "feature vector" (or "fingerprint") for each asset
# at each point in time. This is the core input for
# the analogue-finding engine (Script 5).
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
# =========================================================

config <- list(
  # --- Price Dynamics Lookbacks (in weeks) ---
  price_mom_lookbacks = c(12, 52),
  price_vol_lookbacks = 52,
  price_dd_lookback = 52,
  
  # --- NEW: Price Z-Score Lookback ---
  # This will be our "standardized price path"
  price_zscore_lookback = 52,

  # --- Macro Context Lookbacks (in weeks) ---
  macro_chg_lookbacks = c(13, 52),

  # --- Standardization ---
  zscore_rolling_window = 156
)

cat("--- Configuration --- \n")
cat("Price Momentum Lookbacks:", config$price_mom_lookbacks, "\n")
cat("Macro Change Lookbacks:", config$macro_chg_lookbacks, "\n")
cat("Z-Score Rolling Window:", config$zscore_rolling_window, "weeks\n\n")


# Get asset and macro ticker lists for processing
asset_tickers <- etfs$asset
macro_tickers <- setdiff(
  colnames(master_table_weekly),
  c("date", asset_tickers)
)
cat("Identified", length(macro_tickers), "macro tickers.\n")


# =========================================================
# --- 3. Create Price Dynamics Features (Asset-Specific) ---
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
    ret_1w = log(price) - lag(log(price), 1)
  )

# 3.2 Calculate features
price_features <- price_data_long %>%
  mutate(
    # --- Momentum Features ---
    map_dfc(config$price_mom_lookbacks, ~ {
      set_names(
        list(price / lag(price, .x) - 1),
        paste0("mom_", .x, "w")
      )
    }),

    # --- Volatility Features ---
    map_dfc(config$price_vol_lookbacks, ~ {
      set_names(
        list(
          slider::slide_dbl(ret_1w, ~sd(.x, na.rm = TRUE), .before = .x - 1, .complete = TRUE)
        ),
        paste0("vol_", .x, "w")
      )
    }),

    # --- Drawdown Feature ---
    hi_n_week = slider::slide_dbl(price, ~max(.x, na.rm = TRUE), .before = config$price_dd_lookback - 1, .complete = TRUE),
    drawdown = (price / hi_n_week) - 1,
    
    # --- NEW: Standardized Price Level (Z-Score) ---
    # (price - rolling_mean) / rolling_sd
    price_roll_mean = slider::slide_dbl(price, ~mean(.x, na.rm = TRUE), .before = config$price_zscore_lookback - 1, .complete = TRUE),
    price_roll_sd = slider::slide_dbl(price, ~sd(.x, na.rm = TRUE), .before = config$price_zscore_lookback - 1, .complete = TRUE),
    
    price_zscore_52w = (price - price_roll_mean) / price_roll_sd
  ) %>%
  # Clean up intermediate columns
  select(-price, -ret_1w, -hi_n_week, -price_roll_mean, -price_roll_sd) %>%
  # Handle Inf values from sd=0
  mutate(price_zscore_52w = if_else(is.infinite(price_zscore_52w), 0, price_zscore_52w)) %>%
  ungroup()

cat("--- Price Dynamics Features --- \n")
cat("Created", (length(config$price_mom_lookbacks) +
                 length(config$price_vol_lookbacks) + 2), # +2 for drawdown & new zscore
    "price features for", length(asset_tickers), "assets.\n")


# =========================================================
# --- 4. Create Macro Context Features (Global) ---
# =========================================================

macro_features <- master_table_weekly %>%
  select(date, all_of(macro_tickers))

# 4.1 Calculate Macro Changes
macro_changes <- map(config$macro_chg_lookbacks, ~ {
  lookback <- .x
  macro_features %>%
    select(-date) %>% 
    mutate(
      across(everything(),
             ~ .x - lag(.x, lookback),
             .names = "{.col}_chg_{lookback}w")
    ) %>%
    # --- !! FIX !! ---
    select(ends_with(paste0("_chg_", lookback, "w")))
}) %>%
  bind_cols() 

# 4.2 Combine Levels and Changes
macro_features_final <- bind_cols(macro_features, macro_changes)

cat("--- Macro Context Features --- \n")
cat("Created", (ncol(macro_features_final) - 1), "total macro features (levels + changes).\n")
cat("Head of macro features:\n")
print(head(macro_features_final[, 1:6]))
cat("\n")


# =========================================================
# --- 5. Combine & Standardize (The "Fingerprint") ---
# =========================================================

# 5.1 Join
feature_table_unstandardized <- price_features %>%
  left_join(macro_features_final, by = "date") %>%
  na.omit() %>%
  arrange(asset, date)

cat("--- Combined Feature Table (Unstandardized) --- \n")
cat("Final Dimensions:", dim(feature_table_unstandardized), "\n")
cat("Date Range:", as.character(min(feature_table_unstandardized$date)),
    "to", as.character(max(feature_table_unstandardized$date)), "\n\n")

# 5.2 Standardize (Rolling Z-Score)
# Helper function for rolling Z-score
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

  z <- (x - rolling_mean) / rolling_sd
  
  # --- FIX: Handle NaN and Infinite values ---
  z[is.nan(z)] <- 0
  z[is.infinite(z)] <- 0
  
  return(z)
}

cat("--- Standardizing Features (Rolling Z-Score) --- \n")
cat("Rolling window:", config$zscore_rolling_window, "weeks\n")

standardized_feature_table <- feature_table_unstandardized %>%
  group_by(asset) %>%
  arrange(date) %>% 
  mutate(
    across(
      .cols = where(is.numeric) & !all_of("date"),
      .fns = ~ rolling_zscore(.x, config$zscore_rolling_window)
    )
  ) %>%
  ungroup() %>%
  na.omit()

cat("--- Standardized Feature Table (Rolling Z-Score) --- \n")
cat("Final Dimensions:", dim(standardized_feature_table), "\n")
cat("Date Range:", as.character(min(standardized_feature_table$date)),
    "to", as.character(max(standardized_feature_table$date)), "\n\n")

# =========================================================
# --- 6. Save Outputs ---
# =========================================================
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
  "'feature_table_unstandardized.RData' are saved.\n"
)

