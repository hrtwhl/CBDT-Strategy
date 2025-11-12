# --- Step 4: Build Final Model-Ready Table (Long Format) ---
# This script takes the clean 'master_table_weekly' and creates a 
# long-format table: [date, asset, features..., outcomes...]
# This is the "master" table for both backtesting and live runs.

# 1.1 Load Data and Libraries
source("3_ProcessData.R") # Provides master_table_weekly, assets_wide, macro_wide
library(dplyr)
library(tidyr)
library(purrr)
library(zoo) # For rollapplyr

# 1.2 Identify Asset vs. Macro columns
# We need assets_wide and macro_wide just for their column names
asset_cols <- names(assets_wide)[-1]
macro_cols <- names(macro_wide)[-1]

# 1.3 Define Feature/Outcome Parameters
lookback_short <- 4   # ~1 month
lookback_med   <- 12  # ~1 quarter
lookback_long  <- 52  # ~1 year
zscore_window  <- 52  # 1 year

# Helper function for a rolling z-score (from your script)
rolling_zscore <- function(x, window = zscore_window) {
  rollapplyr(
    x,
    width = window,
    FUN = function(v) {
      v_sd <- sd(v, na.rm = TRUE)
      if (is.na(v_sd)) { return(NA) }
      if (v_sd == 0) { return(0) }
      (v[length(v)] - mean(v, na.rm = TRUE)) / v_sd
    },
    fill = NA, 
    align = "right"
  )
}

cat("--- Building Long Model Table --- \n")

# 2.1 --- Pivot Assets to Long Format ---
master_table_long <- master_table_weekly %>%
  pivot_longer(
    cols = all_of(asset_cols),
    names_to = "asset",
    values_to = "price"
  ) %>%
  # IMPORTANT: Arrange by asset, then date for rolling functions
  arrange(asset, date)

cat("Pivoted to long format. Dimensions:", dim(master_table_long), "\n")
cat("Checking for macro columns. First 10 columns are:\n")
print(names(master_table_long)[1:10])
cat("\n")


# 2.2 --- Engineer Features & Outcomes (Grouped by Asset) ---
model_table_raw <- master_table_long %>%
  # Group by asset to apply rolling functions correctly
  group_by(asset) %>%
  mutate(
    # --- Asset Features ---
    ret_1wk  = (price / lag(price, 1)) - 1,
    mom_4wk  = (price / lag(price, lookback_short)) - 1,
    mom_12wk = (price / lag(price, lookback_med)) - 1,
    mom_52wk = (price / lag(price, lookback_long)) - 1,
    vol_52wk = rollapplyr(ret_1wk, width = lookback_long, FUN = sd, fill = NA), # <--- RAW VOLATILITY
    drawdown_52wk = (price / rollapplyr(price, width = lookback_long, FUN = max, fill = NA)) - 1,
    
    # --- Macro Features (Changes) ---
    across(
      .cols = all_of(macro_cols),
      .fns = list(
        change_13wk = ~ . - lag(., 13),
        change_52wk = ~ . - lag(., 52)
      ),
      .names = "{.col}__{.fn}"
    ),
    
    # --- Macro Features (Levels) ---
    across(
      .cols = all_of(macro_cols),
      .fns = ~ .,
      .names = "{.col}__level"
    ),

    # --- Outcomes (Look-forward) ---
    fwd_ret_4wk  = (lead(price, 4) / price) - 1,
    fwd_ret_8wk  = (lead(price, 8) / price) - 1,
    fwd_ret_12wk = (lead(price, 12) / price) - 1
  ) %>%
  ungroup() # Ungroup before standardization

cat("Raw features and outcomes engineered.\n")

# 2.3 --- Standardize Features (Z-Score) ---
feature_names <- names(model_table_raw)[
  (grepl("__", names(model_table_raw))) | # All macro features
  (names(model_table_raw) %in% c("mom_4wk", "mom_12wk", "mom_52wk", "vol_52wk", "drawdown_52wk"))
]
feature_names <- setdiff(feature_names, "ret_1wk") 

outcome_names <- c("fwd_ret_4wk", "fwd_ret_8wk", "fwd_ret_12wk")

model_table_standardized <- model_table_raw %>%
  group_by(asset) %>%
  mutate(
    across(
      .cols = all_of(feature_names),
      .fns = ~ rolling_zscore(., window = zscore_window),
      .names = "{.col}_z", # Append _z to standardized features
      #
      # <--- *** THIS IS THE FIX ***
      # .keep = "all" ensures the original 'vol_52wk' column is not deleted.
      .keep = "all" 
    )
  ) %>%
  ungroup()

cat("All features standardized.\n")

# 2.4 --- Final Cleanup ---
model_table_final <- model_table_standardized %>%
  select(
    date, 
    asset, 
    price,
    vol_52wk, # <--- Now this column will exist
    all_of(paste0(feature_names, "_z")), # Standardized features
    all_of(outcome_names)               # Raw outcomes
  ) %>%
  na.omit()

cat("--- Final Model Table --- \n")
cat("Final Dimensions:", dim(model_table_final), "\n")
cat("Final Date Range:", as.character(min(model_table_final$date)),
    "to", as.character(max(model_table_final$date)), "\n")
cat("Head:\n")
print(head(model_table_final))

# 2.5 Save the final table
save(model_table_final, file = "model_table_final.RData")

cat("\n\nStep 4 Complete. 'model_table_final.RData' is ready.\n")