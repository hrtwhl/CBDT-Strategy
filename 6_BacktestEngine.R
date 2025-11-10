# --- Step 6: Backtest Engine (4-Week Rebalance) ---
#
# This script runs the full backtest based on the 4-week rebalance logic.
# 1. Loads the engine and all required data.
# 2. Determines the 4-week rebalancing dates (Fridays).
# 3. Runs the analogue engine *only* on those dates to get "Alive" signals.
# 4. Creates a daily weights table, holding signals for 4 weeks.
# 5. Lags weights by 1 day to simulate "next day" (e.g., Monday) trading.
# 6. Calculates daily portfolio returns and the benchmark returns.
# 7. Generates and plots the final equity curve.

# --- 6.1 Load Workspace & Libraries ---
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(scales)
library(tictoc)

# Load all our functions and data tables from Step 5
if (!exists("run_schrodinger_test")) {
  load("engine_workspace.RData")
}

# Source script 1 again to get the *daily* asset prices,
# which we need for the performance calculation.
# We'll store its output in 'daily_prices_df' to be clear.
source("1_getETFData.R")
daily_prices_df <- assets_wide # 'assets_wide' is the daily price table from script 1
rm(list=setdiff(ls(), c(ls(pattern = "^daily_prices_df"),
                        ls(pattern = "^feature_table_aligned"),
                        ls(pattern = "^run_schrodinger_test"),
                        ls(pattern = "^asset_cols$")))) # Clean up

cat("--- Step 6: Backtest Engine (4-Week Rebalance) --- \n")
cat("Workspace and daily prices loaded. Ready to run backtest.\n\n")


# --- 6.2 Part A: Generate Signals on Rebalance Dates ---

# --- Define Rebalance Schedule ---
warmup_period_years <- 2
backtest_start_date <- min(feature_table_aligned$date) + years(warmup_period_years)

# Get all *possible* engine dates (Fridays from our feature table)
all_engine_fridays <- feature_table_aligned %>%
  filter(date >= backtest_start_date) %>%
  pull(date) %>%
  unique() %>%
  sort()

# Select only every 4th Friday for our rebalance schedule
rebalance_dates <- all_engine_fridays[seq(1, length(all_engine_fridays), by = 4)]

cat("Backtest will run from:", as.character(min(rebalance_dates)), "\n")
cat("Total rebalance dates:", length(rebalance_dates), "\n")
cat("Total assets:", length(asset_cols), "\n")
cat("Total decisions to simulate:", length(rebalance_dates) * length(asset_cols), "\n\n")

# --- Create Rebalance Backtest Grid ---
rebalance_grid <- tidyr::expand_grid(
  date = rebalance_dates,
  asset_name = asset_cols
)

# --- Run Analogue Engine (on rebalance dates only) ---
safe_run_schrodinger_test <- purrr::possibly(run_schrodinger_test, otherwise = NULL)

cat("Starting signal generation loop... (This is the fast part)\n")
tic()
signals_table_raw <- purrr::map2_dfr(
  rebalance_grid$date,
  rebalance_grid$asset_name,
  function(d, a) {
    result <- safe_run_schrodinger_test(asset_name = a, current_date = d)
    if (is.null(result)) return(NULL)
    return(data.frame(
      date = d,
      asset = a,
      signal = result$signal
    ))
  }
)
toc()
cat("Signal generation complete.\n\n")


# --- 6.3 Part B: Calculate Daily Portfolio Weights ---

# --- Convert Signals to Weights ---
# Rule: Equal-weight all "Alive" assets on rebalance dates.
weights_on_rebalance_dates <- signals_table_raw %>%
  filter(signal == "Alive") %>% # Only keep "Alive" assets
  group_by(date) %>%
  mutate(n_alive = n()) %>% # Count how many are alive
  ungroup() %>%
  mutate(weight = 1 / n_alive) %>% # Equal weight
  select(date, asset, weight)

# --- Create a "Wide" Weights Table for Joining ---
weights_wide <- weights_on_rebalance_dates %>%
  pivot_wider(
    names_from = asset,
    values_from = weight,
    values_fill = 0 # If not in this table, weight is 0
  )

# --- Get Full Daily Calendar ---
all_daily_dates_df <- daily_prices_df %>%
  select(date) %>%
  filter(date >= min(rebalance_dates)) # Start from our first signal date

# --- "Fill-Forward" Weights to Create a Daily Holding Table ---
# This is the "hold" logic:
# 1. Join our sparse 'weights_wide' (only has 4-week Fridays)
# 2. Fill all weight columns *down* to hold positions until the next signal
daily_weights_df <- all_daily_dates_df %>%
  left_join(weights_wide, by = "date") %>%
  # Fill NA weights with the last known signal
  fill(everything(), .direction = "down") %>%
  # On the very first days, we might have NAs, set them to 0
  mutate(across(-date, ~ ifelse(is.na(.), 0, .)))

# --- 6.4 Part C: Calculate Daily Performance ---

# --- Get Daily Asset Returns ---
# First, get ALL daily returns from the original price table
all_daily_returns_df <- daily_prices_df %>%
  filter(date >= min(all_daily_dates_df$date)) %>%
  mutate(across(-date, ~ (. / lag(.)) - 1))
  # --- REMOVED na.omit() ---
  # We MUST keep NA rows to align dates correctly.

# --- Get Benchmark (EUNM) Returns ---
# Pull the benchmark returns *before* we subset
benchmark_returns <- all_daily_returns_df %>%
  select(date, bench_ret = EUNM) %>% # EUNM is our benchmark
  # --- NEW FIX ---
  # We only want to evaluate the benchmark on days it traded.
  # This removes NA benchmark returns.
  na.omit()

# --- Get Strategy Asset Returns ---
# Now, subset all_daily_returns_df to match the assets in our weights
daily_returns_df <- all_daily_returns_df %>%
  # IMPORTANT: Make sure assets are in the same order as weights
  select(date, all_of(colnames(daily_weights_df)[-1]))

# --- Lag Weights (Simulate "Trade on Monday") ---
# We use lag(1) on the daily weights table. This applies Friday's
# signal (from date 't') to Monday's return (on date 't+1').
lagged_daily_weights_df <- daily_weights_df %>%
  mutate(across(-date, lag)) %>%
  na.omit() # Remove first row

# --- Align Returns and Lagged Weights ---
# Ensure both tables start and end on the same dates
common_dates <- intersect(daily_returns_df$date, lagged_daily_weights_df$date)
daily_returns_aligned <- filter(daily_returns_df, date %in% common_dates)
weights_aligned <- filter(lagged_daily_weights_df, date %in% common_dates)

# --- Calculate Daily Portfolio Returns ---
# This is the core calculation: sum(weight * return)
# We can do this with matrix math (row-wise multiplication)
returns_matrix <- as.matrix(daily_returns_aligned[, -1])
weights_matrix <- as.matrix(weights_aligned[, -1])

# --- NEW FIX: This is the most critical part ---
# We must treat NA, NaN, and Inf returns as 0 for P/L calculation.
# !is.finite() catches all of them.
returns_matrix[!is.finite(returns_matrix)] <- 0
# --- END NEW FIX ---

# Calculate row-wise sum of (weight * return)
daily_port_returns_vector <- rowSums(returns_matrix * weights_matrix, na.rm = TRUE)

performance_table <- tibble(
  date = daily_returns_aligned$date,
  strategy_ret = daily_port_returns_vector
)

# --- Add Benchmark (EUNM) Returns ---
# We already have benchmark_returns (which is now clean of NAs),
# so we just join.
performance_table <- performance_table %>%
  left_join(benchmark_returns, by = "date")

cat("Daily performance calculation complete.\n\n")

# --- 6.5 Part D: Generate Equity Curve and Plot ---

# --- Calculate Cumulative Curves ---
equity_curve_table <- performance_table %>%
  # --- NEW FIX ---
  # We MUST filter NAs from the *benchmark* side.
  # The strategy_ret is now clean (all 0s), but we only
  # want to compound returns on days the benchmark traded.
  filter(!is.na(bench_ret)) %>%
  # --- END NEW FIX ---
  
  # Sort just in case
  arrange(date) %>%
  # Calculate cumulative "wealth" index
  mutate(
    strategy_curve = cumprod(1 + strategy_ret),
    bench_curve = cumprod(1 + bench_ret)
  )

# --- Prepare for Plotting ---
equity_curve_long <- equity_curve_table %>%
  select(date, Strategy = strategy_curve, Benchmark = bench_curve) %>%
  pivot_longer(
    cols = c("Strategy", "Benchmark"),
    names_to = "series",
    values_to = "value"
  )

# --- Generate Plot ---
equity_plot <- ggplot(equity_curve_long, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 1) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  labs(
    title = "Schrödinger's EM Macro Strategy vs. Benchmark (EUNM)",
    subtitle = "Equity Curve (Log Scale)",
    x = "Date",
    y = "Cumulative Growth (Indexed to 1)",
    color = "Portfolio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

cat("--- Backtest Complete --- \n\n")
cat("Final Equity Curve Plot:\n")
print(equity_plot)
cat("\n\n")

# --- Final Summary Stats ---
total_return_strategy <- last(equity_curve_table$strategy_curve) - 1
total_return_bench <- last(equity_curve_table$bench_curve) - 1

cat("--- Summary Performance --- \n")
cat("Strategy Total Return:", scales::percent(total_return_strategy, accuracy = 0.1), "\n")
cat("Benchmark Total Return:", scales::percent(total_return_bench, accuracy = 0.1), "\n")

# Save final objects
save(signals_table_raw, weights_on_rebalance_dates, performance_table, equity_plot,
     file = "full_backtest_results.RData")
cat("\nSaved full backtest results to 'full_backtest_results.RData'.\n")