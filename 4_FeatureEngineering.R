source("3_ProcessData.R")



# --- Steps 2 & 3: Feature Engineering & Defining Outcomes (Corrected) ---

# 2.1 Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)
library(zoo) # Essential for rolling calculations

# 2.2 Load our clean master table from Step 1
if (!exists("master_table_weekly")) {
  stop("ERROR: 'master_table_weekly' not found. Please re-run Step 1.")
}
if (!exists("assets_wide") || !exists("macro_wide")) {
  stop("ERROR: 'assets_wide' or 'macro_wide' not found. Please re-run source scripts.")
}


# 2.3 Identify Asset vs. Macro columns
asset_cols <- names(assets_wide)[-1] # All asset names (excluding 'date')
macro_cols <- names(macro_wide)[-1]  # All macro names (excluding 'date')

cat("--- Column Identification --- \n")
cat("Found", length(asset_cols), "asset columns.\n")
cat("Found", length(macro_cols), "macro columns.\n\n")


# 2.4 --- STEP 3: DEFINE OUTCOMES (Look-Forward Returns) ---
outcome_table <- master_table_weekly %>%
  select(date, all_of(asset_cols)) %>%
  mutate(across(
    .cols = all_of(asset_cols),
    .fns = list(
      fwd_ret_4wk  = ~ (lead(., 4) / .) - 1,
      fwd_ret_8wk  = ~ (lead(., 8) / .) - 1,
      fwd_ret_12wk = ~ (lead(., 12) / .) - 1
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  select(date, ends_with("fwd_ret_4wk"), ends_with("fwd_ret_8wk"), ends_with("fwd_ret_12wk"))

cat("--- Step 3: Outcome Table --- \n")
cat("Dimensions:", dim(outcome_table), "\n")
print(head(outcome_table[, 1:4]))
cat("\n")


# 2.5 --- STEP 2: ENGINEER FEATURES (The "Fingerprint") ---

# Define our rolling window widths
lookback_short <- 4   # ~1 month
lookback_med   <- 12  # ~1 quarter
lookback_long  <- 52  # ~1 year
zscore_window <- 52 # 1 year

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# --- THIS IS THE CORRECTED FUNCTION ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Helper function for a rolling z-score
rolling_zscore <- function(x, window = zscore_window) {
  rollapplyr(
    x,
    width = window,
    FUN = function(v) {
      
      # 1. Calculate sd on the window
      v_sd <- sd(v, na.rm = TRUE)
      
      # 2. --- NEW ROBUSTNESS CHECK ---
      # If sd is NA (because all values in 'v' are NA), return NA
      if (is.na(v_sd)) {
        return(NA)
      }
      
      # 3. Old check: If sd is 0 (all values are identical), return 0
      if (v_sd == 0) {
        return(0)
      }
      
      # 4. If all checks pass, calculate the z-score
      (v[length(v)] - mean(v, na.rm = TRUE)) / v_sd
    },
    fill = NA, # Fill with NA where window is not full
    align = "right"
  )
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# --- END OF CORRECTED FUNCTION ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

cat("--- Step 2: Feature Engineering (Starting) --- \n")

# 2.5.1 --- Asset Features (Price Dynamics) ---
asset_features <- master_table_weekly %>%
  select(date, all_of(asset_cols)) %>%
  mutate(across(
    .cols = all_of(asset_cols),
    .fns = ~ (. / lag(., 1)) - 1,
    .names = "{.col}__ret_1wk"
  )) %>%
  mutate(across(
    .cols = all_of(asset_cols),
    .fns = list(
      mom_4wk  = ~ (. / lag(., lookback_short)) - 1,
      mom_12wk = ~ (. / lag(., lookback_med)) - 1,
      mom_52wk = ~ (. / lag(., lookback_long)) - 1,
      drawdown_52wk = ~ . / rollapplyr(., width = lookback_long, FUN = max, fill = NA) - 1
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  mutate(across(
    .cols = all_of(paste0(asset_cols, "__ret_1wk")),
    .fns = ~ rollapplyr(., width = lookback_long, FUN = sd, fill = NA),
    .names = "{.col}__{.fn}"
  )) %>%
  rename_with(~ str_replace(., "__ret_1wk__rollapplyr", "__vol_52wk"), 
              ends_with("rollapplyr")) %>%
  select(date, matches("__"))

cat("Asset features created.\n")

# 2.5.2 --- Macro Features (Context) ---
macro_features <- master_table_weekly %>%
  select(date, all_of(macro_cols)) %>%
  mutate(across(
    .cols = all_of(macro_cols),
    .fns = list(
      change_13wk = ~ . - lag(., 13), # 3-month change
      change_52wk = ~ . - lag(., 52)  # 12-month change
    ),
    .names = "{.col}__{.fn}"
  ))

cat("Macro features created.\n")

# 2.6 --- STANDARDIZE FEATURES (Z-Score) ---
feature_table_raw <- inner_join(
  asset_features,
  macro_features,
  by = "date"
)

all_feature_names <- names(feature_table_raw)[-1]

# This is the line that failed before, it should work now
feature_table_standardized <- feature_table_raw %>%
  mutate(across(
    .cols = all_of(all_feature_names),
    .fns = ~ rolling_zscore(., window = zscore_window)
  ))

cat("All features standardized.\n")

# 2.7 --- Final Cleanup ---
feature_table <- feature_table_standardized %>%
  na.omit() # Remove all rows with ANY NAs

outcome_table_aligned <- inner_join(
  select(feature_table, date), # Just get the "good" dates
  outcome_table,
  by = "date"
) %>%
  na.omit()

feature_table_aligned <- inner_join(
  select(outcome_table_aligned, date), # Just the "good" dates from outcomes
  feature_table,
  by = "date"
)

cat("--- Steps 2 & 3 Complete --- \n")
cat("Final Aligned Feature Table Dimensions:", dim(feature_table_aligned), "\n")
cat("Final Aligned Outcome Table Dimensions:", dim(outcome_table_aligned), "\n")

cat("\nDate Range for Model-Ready Data:\n")
cat("Start:", as.character(min(feature_table_aligned$date)), "\n")
cat("End:  ", as.character(max(feature_table_aligned$date)), "\n")

# 2.8 Save final model-ready tables
save(feature_table_aligned, file = "feature_table_aligned.RData")
save(outcome_table_aligned, file = "outcome_table_aligned.RData")

cat("\nSaved 'feature_table_aligned.RData' and 'outcome_table_aligned.RData'.\n")
cat("We are now ready for Step 4: The Analogue Engine.\n")

