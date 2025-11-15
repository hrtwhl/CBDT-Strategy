# =========================================================
# SCRIPT 5: THE ANALOGUE ENGINE (BACKTEST)
#
# PURPOSE:
# This script is the "cat". It runs the main backtest.
# It iterates through each date and asset, finds the
# closest historical analogues for its "fingerprint",
# and generates an "Alive" or "Dead" signal based on
# the historical forward returns of those analogues.
#
# =========================================================

# --- 1. Load Data and Libraries ---
#rm(list=setdiff(ls(), c("etfs"))) # Keep etfs list
source("1_getETFData.R") # Load etfs list again if cleared

# Load required libraries
required <- c("dplyr", "tidyr", "purrr", "slider", "lubridate",
              "proxy",   # For fast distance calculations
              "progress" # For a progress bar
              )
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(required, library, character.only = TRUE))

# Load our feature fingerprints
load("standardized_feature_table_monthly.RData") # <-- CHANGED
# Load the raw master table (for calculating forward returns)
load("master_table_monthly.RData") # <-- CHANGED

cat("--- Script 5: Analogue Engine (Monthly) --- \n")
cat("Loaded 'standardized_feature_table' with", nrow(standardized_feature_table), "rows.\n")
cat("Loaded 'master_table_monthly' with", nrow(master_table_monthly), "rows.\n\n")

# =========================================================
# --- 2. Configuration & Parameter Decisions (MONTHLY) ---
#
# =========================================================

config <- list(
  # --- Analogue Finding ---
  k_analogues = 20, # Find the 20 "closest" historical analogues
  distance_metric = "Euclidean", # Standard distance measure

  # --- Forward Horizons (in MONTHS) ---
  fwd_horizons_months = c(1, 2, 3), # <-- CHANGED (1, 2, 3 months)

  # --- Blending Weights (!! KEY ASSUMPTION !!) ---
  # "heavier emphasis on nearer horizons (1 > 2 > 3)"
  blend_weights = c(0.5, 0.3, 0.2), # Sums to 1.0

  # --- "Alive/Dead" Gate Thresholds (!! KEY ASSUMPTION !!) ---
  alive_gate_confidence = 0.60, # (e.g., 60%)
  alive_gate_magnitude = 0.0, # (e.g., > 0%)

  # --- Backtest Timing (in MONTHS) ---
  # 156 weeks = 3 years = 36 months
  min_history_months = 36, # <-- CHANGED
  
  # And a date to start the backtest (must be after
  # the first date in standardized_feature_table)
  backtest_start_date = as.Date("2010-01-01")
)


# Sanity check for weights
if (sum(config$blend_weights) != 1.0) {
  warning("Blend weights do not sum to 1.0. Please check config.")
}

cat("--- Backtest Configuration (Monthly) --- \n")
cat("K-Analogues:", config$k_analogues, "\n")
cat("Horizons:", config$fwd_horizons_months, "months\n")
cat("Blend Weights:", config$blend_weights, "\n")
cat("'Alive' Gate (Confidence): >", config$alive_gate_confidence, "\n")
cat("'Alive' Gate (Magnitude): >", config$alive_gate_magnitude, "\n")
cat("Backtest Start Date:", as.character(config$backtest_start_date), "\n\n")


# =========================================================
# --- 3. Pre-Calculation: Forward Returns ---
# =========================================================

cat("--- Pre-calculating forward returns... --- \n")

# Get a wide table of prices
price_table_wide <- master_table_monthly %>% # <-- CHANGED
  select(date, all_of(etfs$asset))

# Calculate all forward returns (1, 2, 3 MONTHS)
fwd_returns_table <- price_table_wide %>%
  pivot_longer(
    cols = -date,
    names_to = "asset",
    values_to = "price"
  ) %>%
  group_by(asset) %>%
  arrange(date) %>%
  # Use `lead()` to get *future* prices (1, 2, 3 months)
  mutate(
    fwd_ret_1m = (lead(price, 1) / price) - 1, # <-- CHANGED
    fwd_ret_2m = (lead(price, 2) / price) - 1, # <-- CHANGED
    fwd_ret_3m = (lead(price, 3) / price) - 1  # <-- CHANGED
  ) %>%
  select(date, asset, fwd_ret_1m, fwd_ret_2m, fwd_ret_3m) %>% # <-- CHANGED
  ungroup() %>%
  na.omit() # Remove NAs from the end of the series

cat("Forward returns table calculated.\n\n")


# =========================================================
# --- 4. The Analogue Engine: Main Backtest Loop ---
# =========================================================

# Get all unique assets and features
all_assets <- unique(standardized_feature_table$asset)
all_feature_names <- colnames(
  standardized_feature_table %>%
  select(-date, -asset)
)

# Filter for our valid backtest dates
all_test_dates <- standardized_feature_table %>%
  filter(date >= config$backtest_start_date) %>%
  distinct(date) %>%
  pull(date)

# This list will store all our results
backtest_results <- list()

cat("--- Starting Analogue Backtest Loop --- \n")
cat("Total assets:", length(all_assets), "\n")
cat("Total dates:", length(all_test_dates), "\n")

# Initialize progress bar
pb <- progress_bar$new(
  format = "  [:bar] :percent (ETA: :eta)",
  total = length(all_test_dates) * length(all_assets)
)

# --- DATE LOOP (Outer) ---
for (t_idx in seq_along(all_test_dates)) {

  today_date <- all_test_dates[t_idx]

  # --- ASSET LOOP (Inner) ---
  for (current_asset in all_assets) {

    pb$tick() # Update progress bar

    # 1. GET "TODAY'S" FINGERPRINT
    today_fingerprint_tbl <- standardized_feature_table %>%
      filter(date == today_date, asset == current_asset)
    if (nrow(today_fingerprint_tbl) == 0) next
    today_fingerprint_matrix <- today_fingerprint_tbl %>%
      select(all_of(all_feature_names)) %>%
      as.matrix()

    # 2. GET "HISTORY'S" FINGERPRINTS
    history_fingerprints_tbl <- standardized_feature_table %>%
      filter(asset == current_asset, date < today_date)

    # Skip if we don't have enough history
    if (nrow(history_fingerprints_tbl) < config$min_history_months) next # <-- CHANGED

    history_fingerprints_matrix <- history_fingerprints_tbl %>%
      select(all_of(all_feature_names)) %>%
      as.matrix()

    # 3. CALCULATE DISTANCE
    distances <- proxy::dist(
      x = today_fingerprint_matrix,
      y = history_fingerprints_matrix,
      method = config$distance_metric
    )
    history_with_dist <- history_fingerprints_tbl %>%
      mutate(distance = as.vector(distances))

    # 4. FIND TOP K ANALOGUES
    top_k_analogues <- history_with_dist %>%
      arrange(distance) %>%
      slice_head(n = config$k_analogues)

    # 5. GET FORWARD RETURNS FOR THESE ANALOGUES
    analogue_fwd_returns <- top_k_analogues %>%
      inner_join(
        fwd_returns_table,
        by = c("date", "asset")
      )
    if (nrow(analogue_fwd_returns) < config$k_analogues) next

    # 6. BLEND & APPLY "ALIVE/DEAD" GATE
    
    # Calculate stats for each horizon (1m, 2m, 3m)
    stats_1m <- analogue_fwd_returns %>%
      summarise(
        median_ret = median(fwd_ret_1m, na.rm = TRUE),
        pct_pos = mean(fwd_ret_1m > 0, na.rm = TRUE)
      )
    stats_2m <- analogue_fwd_returns %>%
      summarise(
        median_ret = median(fwd_ret_2m, na.rm = TRUE),
        pct_pos = mean(fwd_ret_2m > 0, na.rm = TRUE)
      )
    stats_3m <- analogue_fwd_returns %>%
      summarise(
        median_ret = median(fwd_ret_3m, na.rm = TRUE),
        pct_pos = mean(fwd_ret_3m > 0, na.rm = TRUE)
      )

    # Blend the stats using our config weights
    w <- config$blend_weights
    blended_median_ret = (stats_1m$median_ret * w[1]) +
                         (stats_2m$median_ret * w[2]) +
                         (stats_3m$median_ret * w[3])
    
    blended_confidence = (stats_1m$pct_pos * w[1]) +
                         (stats_2m$pct_pos * w[2]) +
                         (stats_3m$pct_pos * w[3])

    # Apply the "Alive/Dead" filter
    is_alive <- (blended_confidence > config$alive_gate_confidence) &
                (blended_median_ret > config$alive_gate_magnitude)

    # 7. STORE THE RESULT
    backtest_results[[length(backtest_results) + 1]] <- tibble(
      date = today_date,
      asset = current_asset,
      signal = ifelse(is_alive, "Alive", "Dead"),
      blended_median_ret = blended_median_ret,
      blended_confidence = blended_confidence,
      top_analogue_date = top_k_analogues$date[1],
      top_analogue_dist = top_k_analogues$distance[1]
    )

  } # --- End Asset Loop ---
} # --- End Date Loop ---

cat("\n--- Backtest Loop Complete --- \n")

# =========================================================
# --- 5. Collate and Save Results ---
# =========================================================

final_backtest_signals <- bind_rows(backtest_results)

if (nrow(final_backtest_signals) > 0) {
  cat("Successfully generated", nrow(final_backtest_signals), "signals.\n")
  cat("Head of signals:\n")
  print(head(final_backtest_signals))
  cat("\nTail of signals:\n")
  print(tail(final_backtest_signals))
  
  # Save the results
  save(
    final_backtest_signals,
    file = "final_backtest_signals_monthly.RData" # <-- CHANGED
  )
  
  cat("\n\n--- Script 5 Complete --- \n")
  cat("'final_backtest_signals_monthly.RData' has been saved.\n")
  
} else {
  cat("--- ERROR --- \n")
  cat("No signals were generated. This might be due to:\n")
  cat("1. 'backtest_start_date' being too late.\n")
  cat("2. 'min_history_months' being too large.\n")
  cat("3. No assets having data.\n")
}
