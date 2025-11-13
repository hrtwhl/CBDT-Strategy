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
load("standardized_feature_table.RData")
# Load the raw master table (for calculating forward returns)
load("master_table_weekly.RData")

cat("--- Script 5: Analogue Engine --- \n")
cat("Loaded 'standardized_feature_table' with", nrow(standardized_feature_table), "rows.\n")
cat("Loaded 'master_table_weekly' with", nrow(master_table_weekly), "rows.\n\n")

# =========================================================
# --- 2. Configuration & Parameter Decisions ---
#
# !! --- IMPORTANT DECISIONS --- !!
# These parameters define the model's behavior and are
# based on our interpretation of Heiden's articles.
# Please review and confirm these.
# =========================================================

config <- list(
  # --- Analogue Finding ---
  k_analogues = 20, # Find the 20 "closest" historical analogues
  distance_metric = "Euclidean", # Standard distance measure

  # --- Forward Horizons (Heiden is clear on this) ---
  fwd_horizons_wks = c(4, 8, 12),

  # --- Blending Weights (!! KEY ASSUMPTION !!) ---
  # "heavier emphasis on nearer horizons (4 > 8 > 12)"
  # We'll use these to create a weighted "score"
  blend_weights = c(0.5, 0.3, 0.2), # Sums to 1.0

  # --- "Alive/Dead" Gate Thresholds (!! KEY ASSUMPTION !!) ---
  # To be "Alive", the signal must pass two tests:
  # 1. Confidence: A weighted majority of analogues were positive.
  alive_gate_confidence = 0.60, # (e.g., 60%)
  # 2. Magnitude: The weighted median return was meaningfully positive.
  alive_gate_magnitude = 0.0, # (e.g., > 0%)

  # --- Backtest Timing ---
  # We need a minimum history to find analogues
  # 156 is 3 years, 104 is 2 years
  min_history_wks = 156, 
  #min_history_wks = 104,
  # And a date to start the backtest (must be after
  # the first date in standardized_feature_table)
  backtest_start_date = as.Date("2010-01-01")
)

# Sanity check for weights
if (sum(config$blend_weights) != 1.0) {
  warning("Blend weights do not sum to 1.0. Please check config.")
}

cat("--- Backtest Configuration --- \n")
cat("K-Analogues:", config$k_analogues, "\n")
cat("Horizons:", config$fwd_horizons_wks, "weeks\n")
cat("Blend Weights:", config$blend_weights, "\n")
cat("'Alive' Gate (Confidence): >", config$alive_gate_confidence, "\n")
cat("'Alive' Gate (Magnitude): >", config$alive_gate_magnitude, "\n")
cat("Backtest Start Date:", as.character(config$backtest_start_date), "\n\n")


# =========================================================
# --- 3. Pre-Calculation: Forward Returns ---
#
# To speed up the loop, we'll pre-calculate all
# future returns for all assets *once*.
# This is a critical optimization.
# =========================================================

cat("--- Pre-calculating forward returns... --- \n")

# Get a wide table of prices
price_table_wide <- master_table_weekly %>%
  select(date, all_of(etfs$asset))

# Calculate all forward returns
fwd_returns_table <- price_table_wide %>%
  pivot_longer(
    cols = -date,
    names_to = "asset",
    values_to = "price"
  ) %>%
  group_by(asset) %>%
  arrange(date) %>%
  # Use `lead()` to get *future* prices
  mutate(
    fwd_ret_4w = (lead(price, 4) / price) - 1,
    fwd_ret_8w = (lead(price, 8) / price) - 1,
    fwd_ret_12w = (lead(price, 12) / price) - 1
  ) %>%
  select(date, asset, fwd_ret_4w, fwd_ret_8w, fwd_ret_12w) %>%
  ungroup() %>%
  na.omit() # Remove NAs from the end of the series

cat("Forward returns table calculated.\n\n")


# =========================================================
# --- 4. The Analogue Engine: Main Backtest Loop ---
#
# This loop will be slow. We use a progress bar.
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
    # We need this as a matrix (1 row) for distance calculation
    today_fingerprint_tbl <- standardized_feature_table %>%
      filter(date == today_date, asset == current_asset)

    # Skip if this asset doesn't have a fingerprint for this day
    if (nrow(today_fingerprint_tbl) == 0) next

    today_fingerprint_matrix <- today_fingerprint_tbl %>%
      select(all_of(all_feature_names)) %>%
      as.matrix()

    # 2. GET "HISTORY'S" FINGERPRINTS
    # Get all fingerprints for this asset *before* today
    history_fingerprints_tbl <- standardized_feature_table %>%
      filter(asset == current_asset, date < today_date)

    # Skip if we don't have enough history
    if (nrow(history_fingerprints_tbl) < config$min_history_wks) next

    history_fingerprints_matrix <- history_fingerprints_tbl %>%
      select(all_of(all_feature_names)) %>%
      as.matrix()

    # 3. CALCULATE DISTANCE
    # This is the core "similarity" search
    distances <- proxy::dist(
      x = today_fingerprint_matrix,
      y = history_fingerprints_matrix,
      method = config$distance_metric
    )
    
    # Add distances to history table
    history_with_dist <- history_fingerprints_tbl %>%
      mutate(distance = as.vector(distances))

    # 4. FIND TOP K ANALOGUES
    top_k_analogues <- history_with_dist %>%
      arrange(distance) %>%
      slice_head(n = config$k_analogues)

    # 5. GET FORWARD RETURNS FOR THESE ANALOGUES
    # We join our pre-calculated table for speed
    analogue_fwd_returns <- top_k_analogues %>%
      inner_join(
        fwd_returns_table,
        by = c("date", "asset")
      )

    # Skip if we couldn't find fwd returns (shouldn't happen, but safe)
    if (nrow(analogue_fwd_returns) < config$k_analogues) next

    # 6. BLEND & APPLY "ALIVE/DEAD" GATE
    
    # Calculate stats for each horizon
    stats_4w <- analogue_fwd_returns %>%
      summarise(
        median_ret = median(fwd_ret_4w, na.rm = TRUE),
        pct_pos = mean(fwd_ret_4w > 0, na.rm = TRUE)
      )
    stats_8w <- analogue_fwd_returns %>%
      summarise(
        median_ret = median(fwd_ret_8w, na.rm = TRUE),
        pct_pos = mean(fwd_ret_8w > 0, na.rm = TRUE)
      )
    stats_12w <- analogue_fwd_returns %>%
      summarise(
        median_ret = median(fwd_ret_12w, na.rm = TRUE),
        pct_pos = mean(fwd_ret_12w > 0, na.rm = TRUE)
      )

    # Blend the stats using our config weights
    w <- config$blend_weights
    blended_median_ret = (stats_4w$median_ret * w[1]) +
                         (stats_8w$median_ret * w[2]) +
                         (stats_12w$median_ret * w[3])
    
    blended_confidence = (stats_4w$pct_pos * w[1]) +
                         (stats_8w$pct_pos * w[2]) +
                         (stats_12w$pct_pos * w[3])

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

# =T========================================================
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
    file = "final_backtest_signals.RData"
  )
  
  cat("\n\n--- Script 5 Complete --- \n")
  cat("'final_backtest_signals.RData' has been saved.\n")
  
} else {
  cat("--- ERROR --- \n")
  cat("No signals were generated. This might be due to:\n")
  cat("1. 'backtest_start_date' being too late.\n")
  cat("2. 'min_history_wks' being too large.\n")
  cat("3. No assets having data.\n")
}

