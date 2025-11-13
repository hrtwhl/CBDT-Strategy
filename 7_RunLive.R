# =========================================================
# SCRIPT 7: THE LIVE DECISION DASHBOARD
#
# This script is your "run" button. It sources all previous
# scripts to refresh data and features, then runs the
# analogue engine for the *single most recent date*.
#
# It prints decision tables to the console and saves
# all charts (Fan Charts, Macro Maps) to a new
# folder called '/live_charts'.
# =========================================================

cat("--- Running Live Dashboard (Script 7) ---\n")
cat("This will take a few minutes...\n\n")

# =========================================================
# 1. SETUP & CONFIGURATION
# =========================================================
cat("Step 1: Loading libraries and config...\n")

# --- Load Libraries ---
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(slider)
  library(lubridate)
  library(ggplot2)
  library(proxy)
  library(knitr)
  library(scales)
})

# --- Helper function for re-basing price paths ---
rebase_path <- function(prices) {
  if (all(is.na(prices)) | length(prices) == 0) return(rep(NA, length(prices)))
  first_valid_price <- prices[!is.na(prices)][1]
  return(prices / first_valid_price * 100)
}

# --- Define Model Configuration ---
# (Combined from scripts 4 and 5 for a self-contained report)
config_features <- list(
  price_mom_lookbacks = c(4, 12, 26, 52),
  price_vol_lookbacks = c(12, 52),
  price_dd_lookback = 52,
  macro_chg_lookbacks = c(13, 52),
  zscore_rolling_window = 156
)

config_engine <- list(
  k_analogues = 20,
  distance_metric = "Euclidean",
  fwd_horizons_wks = c(4, 8, 12),
  blend_weights = c(0.5, 0.3, 0.2),
  # --- NEW: Gates for Long vs Short ---
  long_gate_confidence = 0.60,  # Min % of positive analogues for LONG
  long_gate_magnitude = 0.0,    # Min blended median return for LONG
  short_gate_confidence = 0.60, # Min % of *negative* analogues for SHORT
  short_gate_magnitude = 0.0,   # Max blended median return for SHORT (must be < 0)
  min_history_wks = 156
)

# --- Define Charting Configuration ---
# Choose two *unstandardized* macro features
MAP_MACRO_X_AXIS <- "T10Y2Y"
MAP_MACRO_Y_AXIS <- "industrial_prod_chg_52w"

# =========================================================
# 2. RUN DATA PIPELINE (SCRIPTS 1-4)
# =========================================================
cat("Step 2: Refreshing all data (running scripts 1-4)...\n")

# Source all previous scripts to rebuild data from scratch
suppressMessages({
  source("1_getETFData.R")
  source("2_getMacroData.R")
  source("3_ProcessData.R")
  source("4_CreateFeatures.R")
})

# --- Load Data ---
load("standardized_feature_table.RData")
load("feature_table_unstandardized.RData")
load("master_table_weekly.RData")

cat("Data refresh complete.\n")

# =========================================================
# 3. RUN "LIVE" ANALOGUE ENGINE
# =========================================================
cat("Step 3: Running Analogue Engine for most recent date...\n")

# --- 1. Get Live Date and Feature Names ---
live_date <- max(standardized_feature_table$date)
max(standardized_feature_table$date)
max(master_table_weekly$date) # Sanity check
all_assets <- etfs$asset
all_feature_names <- colnames(
  standardized_feature_table %>%
  select(-date, -asset)
)

# --- 2. Pre-Calculate Forward Returns (for plotting) ---
# We need this to plot the analogue paths
fwd_returns_table <- master_table_weekly %>%
  select(date, all_of(etfs$asset)) %>%
  pivot_longer(
    cols = -date,
    names_to = "asset",
    values_to = "price"
  ) %>%
  group_by(asset) %>%
  arrange(date) %>%
  mutate(
    # Create fwd paths for 52 weeks
    map_dfc(1:52, ~ {
      set_names(
        list((lead(price, .x) / price) - 1),
        paste0("fwd_ret_", .x, "w")
      )
    }),
    # Also get 4, 8, 12 week returns for the signal
    fwd_ret_4w = (lead(price, 4) / price) - 1,
    fwd_ret_8w = (lead(price, 8) / price) - 1,
    fwd_ret_12w = (lead(price, 12) / price) - 1
  ) %>%
  select(date, asset, starts_with("fwd_ret_")) %>%
  ungroup()

# This list will store all our results
live_results_list <- list()

# --- 3. Loop over all assets for the *live date only* ---
for (current_asset in all_assets) {

  # 1. GET LIVE FINGERPRINT
  live_fingerprint_tbl <- standardized_feature_table %>%
    filter(date == live_date, asset == current_asset)

  if (nrow(live_fingerprint_tbl) == 0) next # Skip if no data

  live_fingerprint_matrix <- live_fingerprint_tbl %>%
    select(all_of(all_feature_names)) %>%
    as.matrix()

  # 2. GET HISTORICAL FINGERPRINTS
  history_fingerprints_tbl <- standardized_feature_table %>%
    filter(asset == current_asset, date < live_date)

  if (nrow(history_fingerprints_tbl) < config_engine$min_history_wks) next

  history_fingerprints_matrix <- history_fingerprints_tbl %>%
    select(all_of(all_feature_names)) %>%
    as.matrix()

  # 3. CALCULATE DISTANCE
  distances <- proxy::dist(
    x = live_fingerprint_matrix,
    y = history_fingerprints_matrix,
    method = config_engine$distance_metric
  )
  
  history_with_dist <- history_fingerprints_tbl %>%
    mutate(distance = as.vector(distances))

  # 4. FIND TOP K ANALOGUES
  top_k_analogues_tbl <- history_with_dist %>%
    arrange(distance) %>%
    slice_head(n = config_engine$k_analogues)

  # 5. GET FORWARD RETURNS FOR ANALOGUES
  analogue_fwd_returns <- top_k_analogues_tbl %>%
    inner_join(
      fwd_returns_table,
      by = c("date", "asset")
    )

  if (nrow(analogue_fwd_returns) < config_engine$k_analogues) next

  # 6. BLEND & APPLY GATES (NEW LONG/SHORT LOGIC)
  stats_4w <- analogue_fwd_returns %>%
    summarise(median_ret = median(fwd_ret_4w, na.rm = TRUE),
              pct_pos = mean(fwd_ret_4w > 0, na.rm = TRUE),
              pct_neg = mean(fwd_ret_4w < 0, na.rm = TRUE)) # NEW
  stats_8w <- analogue_fwd_returns %>%
    summarise(median_ret = median(fwd_ret_8w, na.rm = TRUE),
              pct_pos = mean(fwd_ret_8w > 0, na.rm = TRUE),
              pct_neg = mean(fwd_ret_8w < 0, na.rm = TRUE)) # NEW
  stats_12w <- analogue_fwd_returns %>%
    summarise(median_ret = median(fwd_ret_12w, na.rm = TRUE),
              pct_pos = mean(fwd_ret_12w > 0, na.rm = TRUE),
              pct_neg = mean(fwd_ret_12w < 0, na.rm = TRUE)) # NEW

  w <- config_engine$blend_weights
  
  blended_median_ret = (stats_4w$median_ret * w[1]) +
                       (stats_8w$median_ret * w[2]) +
                       (stats_12w$median_ret * w[3])
  
  blended_confidence_pos = (stats_4w$pct_pos * w[1]) +
                           (stats_8w$pct_pos * w[2]) +
                           (stats_12w$pct_pos * w[3])
  
  blended_confidence_neg = (stats_4w$pct_neg * w[1]) +  # NEW
                           (stats_8w$pct_neg * w[2]) +
                           (stats_12w$pct_neg * w[3])

  # --- Apply Long/Short/Neutral Logic ---
  is_long <- (blended_confidence_pos > config_engine$long_gate_confidence) &
             (blended_median_ret > config_engine$long_gate_magnitude)
             
  is_short <- (blended_confidence_neg > config_engine$short_gate_confidence) & # NEW
              (blended_median_ret < config_engine$short_gate_magnitude)
              
  signal <- ifelse(is_long, "Long", ifelse(is_short, "Short", "Neutral")) # NEW

  # 7. STORE RESULT (and the analogue data for plotting)
  live_results_list[[current_asset]] <- tibble(
    date = live_date,
    asset = current_asset,
    signal = signal,
    blended_median_ret = blended_median_ret,
    blended_confidence_pos = blended_confidence_pos,
    blended_confidence_neg = blended_confidence_neg,
    top_analogue_date_1 = top_k_analogues_tbl$date[1],
    # Store the *entire* analogue table in a list-column
    analogue_data = list(analogue_fwd_returns)
  )
}

cat("Analogue engine complete.\n")

# =========================================================
# 4. GENERATE CONSOLE TABLES
# =========================================================
cat("Step 4: Generating console reports...\n\n")

# --- 1. Collate Final Tables ---
live_signals <- bind_rows(live_results_list)

# --- 2. Target Portfolio Table (Long-Only, from backtest) ---
target_weights <- live_signals %>%
  filter(signal == "Long") %>%
  mutate(weight = blended_confidence_pos / sum(blended_confidence_pos, na.rm = TRUE)) %>%
  left_join(etfs %>% select(asset, label), by = "asset") %>%
  select(asset, label, signal, blended_confidence_pos, weight) %>%
  arrange(desc(weight))

# --- 3. Full Signal Roster (Long/Short/Neutral) ---
roster_table <- live_signals %>%
  left_join(etfs %>% select(asset, label), by = "asset") %>%
  select(
    asset, label, signal, blended_median_ret,
    blended_confidence_pos, blended_confidence_neg
  ) %>%
  arrange(desc(blended_confidence_pos), desc(blended_confidence_neg))

# --- 4. Print Tables to Console ---
cat("=========================================================\n")
cat(" SCHRÖDINGER'S EM LENS: DECISION REPORT\n")
cat(" Report Date:", as.character(live_date), "\n")
cat("=========================================================\n\n")

cat("--- TARGET PORTFOLIO (Long-Only, Confidence-Weighted) ---\n\n")
if (nrow(target_weights) > 0) {
  print(
    knitr::kable(
      target_weights,
      digits = 3,
      format = "pipe",
      col.names = c("Asset", "Label", "Signal", "Confidence", "Weight")
    )
  )
} else {
  cat("No assets passed the 'Long' filter. Portfolio is 100% cash.\n")
}

cat("\n\n--- FULL SIGNAL ROSTER (Long/Short/Neutral) ---\n\n")
print(
  knitr::kable(
    roster_table,
    digits = 3,
    format = "pipe",
    col.names = c("Asset", "Label", "Signal", "Median Ret", "Conf (Pos)", "Conf (Neg)")
  )
)

# =========================================================
# 5. GENERATE & SAVE PLOTS
# =========================================================
cat("\n\nStep 5: Generating and saving charts to '/live_charts'...\n")

# --- 1. Create directory ---
if (!dir.exists("live_charts")) {
  dir.create("live_charts")
}

# --- 2. Get data for plots ---
unstd_history <- feature_table_unstandardized %>%
  select(date, asset, all_of(MAP_MACRO_X_AXIS), all_of(MAP_MACRO_Y_AXIS))

price_history <- master_table_weekly %>%
  select(date, all_of(etfs$asset)) %>%
  pivot_longer(-date, names_to = "asset", values_to = "price")

# --- 3. Plotting Loop ---
for (i in 1:nrow(live_signals)) {
  
  # --- A. Get Data for This Asset ---
  asset_data <- live_signals[i, ]
  current_asset <- asset_data$asset
  asset_label <- etfs$label[etfs$asset == current_asset]
  
  cat(paste("  Generating charts for", current_asset, "...\n"))
  
  # Get the analogue data we stored
  asset_analogue_data <- asset_data$analogue_data[[1]]
  
  # --- B. Generate Analogue "Fan Chart" ---
  analogue_paths <- asset_analogue_data %>%
    select(date, starts_with("fwd_ret_")) %>%
    pivot_longer(
      cols = -date,
      names_to = "horizon",
      values_to = "return",
      names_prefix = "fwd_ret_",
      names_pattern = "(\\d+)w"
    ) %>%
    mutate(
      horizon = as.integer(horizon),
      horizon = horizon + 1 # offset for 0
    ) %>%
    bind_rows(
      tibble(date = asset_analogue_data$date, horizon = 0, return = 0)
    ) %>%
    mutate(rebased_price = (1 + return) * 100) # Rebase to 100
    
  current_path_data <- price_history %>%
    filter(asset == current_asset, date <= live_date, date >= (live_date - weeks(52))) %>%
    arrange(date) %>%
    mutate(rebased_price = rebase_path(price)) %>%
    mutate(weeks_from_end = as.numeric(date - max(date), "weeks"))

  projected_path <- analogue_paths %>%
    group_by(horizon) %>%
    summarise(
      median_price = median(rebased_price, na.rm = TRUE),
      p20 = quantile(rebased_price, 0.2, na.rm = TRUE),
      p80 = quantile(rebased_price, 0.8, na.rm = TRUE)
    )

  fan_chart <- ggplot() +
    geom_line(
      data = analogue_paths,
      aes(x = horizon, y = rebased_price, group = date),
      color = "grey70",
      alpha = 0.5
    ) +
    geom_ribbon(
      data = projected_path,
      aes(x = horizon, ymin = p20, ymax = p80),
      fill = "grey40",
      alpha = 0.3
    ) +
    geom_line(
      data = projected_path,
      aes(x = horizon, y = median_price),
      color = "black",
      linewidth = 1,
      linetype = "dashed"
    ) +
    geom_line(
      data = current_path_data,
      aes(x = weeks_from_end, y = rebased_price),
      color = "blue",
      linewidth = 1.2
    ) +
    labs(
      title = paste(asset_label, "(", current_asset, ") - Analogue Price Paths"),
      subtitle = "Blue = Current Path (Last 52w) | Grey = Analogues | Dashed = Median Projection (Next 52w)",
      x = "Weeks (0 = Today)",
      y = "Rebased Price (Start = 100)"
    ) +
    theme_minimal()
  
  # --- C. Generate Macro State Map ---
  today_macro_point <- live_features_unstd %>%
    filter(asset == current_asset, date == live_date) %>%
    select(date, all_of(MAP_MACRO_X_AXIS), all_of(MAP_MACRO_Y_AXIS))
    
  analogue_macro_points <- unstd_history %>%
    filter(
      asset == current_asset,
      date %in% asset_analogue_data$date
    )

  macro_map <- ggplot() +
    geom_point(
      data = analogue_macro_points,
      aes(x = .data[[MAP_MACRO_X_AXIS]], y = .data[[MAP_MACRO_Y_AXIS]]),
      color = "green",
      alpha = 0.7,
      size = 3
    ) +
    geom_point(
      data = today_macro_point,
      aes(x = .data[[MAP_MACRO_X_AXIS]], y = .data[[MAP_MACRO_Y_AXIS]]),
      color = "blue",
      size = 5,
      shape = 18 # Diamond
    ) +
    labs(
      title = paste(asset_label, "(", current_asset, ") - Macro State Map"),
      subtitle = "Blue Diamond = Today | Green Dots = Top 20 Analogues",
      x = MAP_MACRO_X_AXIS,
      y = MAP_MACRO_Y_AXIS
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  # --- D. Save Plots ---
  fan_chart_filename <- paste0("live_charts/", current_asset, "_fan_chart.png")
  ggsave(fan_chart_filename, fan_chart, width = 10, height = 6, dpi = 150)
  
  macro_map_filename <- paste0("live_charts/", current_asset, "_macro_map.png")
  ggsave(macro_map_filename, macro_map, width = 8, height = 6, dpi = 150)
}

cat("All charts saved.\n")

# =========================================================
# 6. APPENDIX: MODEL CONFIGURATION
# =========================================================
cat("\n\n=========================================================\n")
cat(" APPENDIX: MODEL CONFIGURATION\n")
cat("=========================================================\n\n")

cat("--- Feature Creation (Script 4) ---\n")
cat("* Price Momentum Lookbacks (wks):", paste(config_features$price_mom_lookbacks, collapse = ", "), "\n")
cat("* Price Volatility Lookbacks (wks):", paste(config_features$price_vol_lookbacks, collapse = ", "), "\n")
cat("* Drawdown Lookback (wks):", config_features$price_dd_lookback, "\n")
cat("* Macro Change Lookbacks (wks):", paste(config_features$macro_chg_lookbacks, collapse = ", "), "\n")
cat("* Rolling Z-Score Window (wks):", config_features$zscore_rolling_window, "\n")

cat("\n--- Analogue Engine (Script 5 Logic) ---\n")
cat("* Number of Analogues (k):", config_engine$k_analogues, "\n")
cat("* Forward Horizons (wks):", paste(config_engine$fwd_horizons_wks, collapse = ", "), "\n")
cat("* Horizon Blend Weights:", paste(config_engine$blend_weights, collapse = ", "), "\n")
cat("* 'Long' Confidence Gate:", percent(config_engine$long_gate_confidence), "\n")
cat("* 'Long' Magnitude Gate (Median Ret >):", percent(config_engine$long_gate_magnitude), "\n")
cat("* 'Short' Confidence Gate:", percent(config_engine$short_gate_confidence), "\n")
cat("* 'Short' Magnitude Gate (Median Ret <):", percent(config_engine$short_gate_magnitude), "\n")
cat("* Minimum History (wks):", config_engine$min_history_wks, "\n")

cat("\n\n--- Dashboard Complete ---\n")




# --- DIAGNOSTIC SNIPPET ---
#
# Please run this code in your R console.
# It will load the 'feature_table_unstandardized' (from Script 4)
# and check for features that have a rolling standard deviation
# of zero in the most recent dates.
#
# This will confirm if our hypothesis is correct.

# --- Load libraries ---
library(dplyr)
library(tidyr)
library(slider)

# --- Load the UNSTANDARDIZED feature table ---
cat("Loading 'feature_table_unstandardized.RData'...\n")
load("feature_table_unstandardized.RData")

# --- Define config (from Script 4) ---
config <- list(
  zscore_rolling_window = 156
)
window_size <- config$zscore_rolling_window

cat("Using rolling window of:", window_size, "weeks\n")

# --- Re-calculate *only* the rolling SD for all features ---
# This mimics the first part of the rolling_zscore function
rolling_sd_table <- feature_table_unstandardized %>%
  group_by(asset) %>%
  arrange(date) %>% # Ensure date is sorted
  mutate(
    across(
      .cols = where(is.numeric) & !all_of("date"),
      .fns = ~ slider::slide_dbl(
                 .x,
                 ~sd(.x, na.rm = TRUE),
                 .before = window_size - 1,
                 .complete = TRUE
               ),
      .names = "{.col}_rolling_sd"
    )
  ) %>%
  ungroup() %>%
  select(date, asset, ends_with("_rolling_sd"))

# --- Find the problem dates ---
# Filter for dates that *should* exist but are missing
# from the standardized table (i.e., after 2025-08-22)
problem_dates <- rolling_sd_table %>%
  filter(date > as.Date("2025-08-22")) %>%
  select(-asset) %>% # Macro features are the same for all assets
  distinct()

# --- Pivot to find the *culprit features* ---
problem_features <- problem_dates %>%
  pivot_longer(
    cols = -date,
    names_to = "feature_sd",
    values_to = "rolling_sd_value"
  ) %>%
  # Find features where the rolling_sd is 0 (or NA)
  filter(rolling_sd_value == 0 | is.na(rolling_sd_value)) %>%
  # Clean up the name
  mutate(feature = sub("_rolling_sd$", "", feature_sd)) %>%
  select(date, feature, rolling_sd_value) %>%
  distinct(feature) # Get a unique list of problem features

# --- Print the report ---
cat("\n--- DIAGNOSTIC REPORT ---\n")
if (nrow(problem_features) > 0) {
  cat("Hypothesis CONFIRMED.\n")
  cat("The 'standardized_feature_table' stops on 2025-08-22 because\n")
  cat("the following features have a rolling standard deviation of 0\n")
  cat("on or after that date. This creates 'NaN' values, which\n")
  cat("'na.omit()' then drops.\n\n")
  
  cat("Culprit Features:\n")
  print(problem_features$feature)
  
} else {
  cat("Hypothesis DISPROVED.\n")
  cat("No features with a rolling_sd of 0 were found after 2025-08-22.\n")
  cat("The problem must be somewhere else.\n")
}

cat("\n--- End of Diagnostic --- \n")
