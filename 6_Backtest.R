# --- Step 6: The Backtester (Corrected) ---
# This script runs the full historical simulation of the
# Schrödinger's.ai strategy, applied to our EM universe.
# It uses the functions from the Analogue Engine, applies
# our monthly, long-only trading rules, and calculates
# performance against a benchmark.

# 1.1 --- Setup: Load Libraries & Data ---
cat("--- Starting Backtest --- \n")
# Load libraries
library(dplyr)
library(tidyr)
library(purrr)      # For map_dfr
library(lubridate)  # For date manipulation
library(ggplot2)    # For plotting
library(PerformanceAnalytics) # For performance metrics
library(tictoc)     # To time the signal generation
library(zoo)        # For na.locf (fill forward)

# Load our core functions
source("5_AnalogueEngine.R")

# Load our model-ready data
load("model_table_final.RData") # Provides model_table_final

# Load our raw daily price data (for backtest execution)
# We source() this to get the 'assets_wide' tibble
source("1_getETFData.R") # Provides assets_wide

# Define Benchmark
BENCHMARK_TICKER <- "EUNM.DE"
if (!BENCHMARK_TICKER %in% names(assets_wide)) {
  stop("Benchmark ticker '", BENCHMARK_TICKER, "' not found in assets_wide!")
}

# 1.2 --- Define Backtest Parameters ---
ANALOGUE_K <- 20       # K-nearest analogues
ASSET_WEIGHT <- 0.6    # 60% weight to price, 40% to macro
CONF_THRESHOLD <- 0.6  # 60% of analogues must agree
RET_THRESHOLD <- 0.0   # Median blended return > 0


# 2.1 --- Generate All Historical Signals (Intensive) ---
cat("Generating all historical signals (this may take several minutes)...\n")
tic() # Start timer

# Create a grid of every (asset, date) pair from our model table
signal_grid <- model_table_final %>%
  distinct(asset, date)

# Use map2_dfr to loop over every row in signal_grid
all_signals_raw <- signal_grid %>%
  mutate(
    # <--- *** FIX 1: Suppress benign "Not enough data" warnings ***
    signal = suppressWarnings(map2_chr(asset, date, function(a, d) {
      analogues <- find_analogues(
        target_asset = a,
        target_date = d,
        model_table = model_table_final,
        k = ANALOGUE_K,
        asset_weight = ASSET_WEIGHT
      )
      classify_signal(
        analogues = analogues,
        conf_threshold = CONF_THRESHOLD,
        ret_threshold = RET_THRESHOLD
      )
    }))
  )

toc() # Stop timer
cat("Signal generation complete.\n")

# 2.2 --- Define Monthly Rebalance Schedule ---
cat("Defining monthly rebalance schedule...\n")

rebalance_fridays <- all_signals_raw %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  group_by(year, month) %>%
  summarise(rebalance_date = max(date), .groups = "drop")

monthly_signals <- all_signals_raw %>%
  filter(date %in% rebalance_fridays$rebalance_date) %>%
  select(date, asset, signal)
  
# 2.3 --- Apply "Long Only" & "Trade on Change" Rules ---
monthly_targets <- monthly_signals %>%
  mutate(target_position = ifelse(signal == "Long", 1, 0)) %>%
  group_by(asset) %>%
  mutate(prev_position = lag(target_position, default = 0)) %>%
  filter(target_position != prev_position | is.na(lag(target_position))) %>%
  select(date, asset, target_position)
  
cat("Monthly target positions defined.\n")


# 3.1 --- Prepare Daily Price Data (BUG FIX) ---
cat("Preparing daily price data (filling holidays)...\n")

# 1. Create a full daily grid from min to max date
full_date_grid <- tibble(
  date = seq.Date(from = min(assets_wide$date), to = max(assets_wide$date), by = "day")
)

# 2. Join assets_wide, creating explicit NA rows for missing days
assets_wide_filled <- full_date_grid %>%
  left_join(assets_wide, by = "date") %>%
  # 3. Fill prices forward (na.locf = last observation carried forward)
  # <--- *** FIX 2: Updated syntax for across() ***
  mutate(across(-date, .fns = \(x) zoo::na.locf(x, na.rm = FALSE))) %>%
  # Remove initial NAs before first trade
  na.omit()

# 4. Pivot to long format
daily_prices_long_filled <- assets_wide_filled %>%
  pivot_longer(
    cols = -date,
    names_to = "asset",
    values_to = "price"
  ) %>%
  arrange(asset, date)

# 5. Get the raw 52-week volatility for weighting
# (This is known on Fridays, so we fill it down)
asset_volatility <- model_table_final %>%
  select(date, asset, vol_52wk) %>% # <--- This will now work
  # Join to full grid
  right_join(distinct(daily_prices_long_filled, date, asset), by = c("date", "asset")) %>%
  arrange(asset, date) %>%
  group_by(asset) %>%
  fill(vol_52wk, .direction = "down") %>% # Carry last known vol forward
  ungroup()

cat("Daily price data prepared.\n")


# 3.2 --- Join Signals to Daily Prices (The "Core Loop") ---
# Create a complete grid of all assets and all trading days
all_days_grid <- daily_prices_long_filled %>%
  distinct(date, asset)

daily_positions <- all_days_grid %>%
  left_join(monthly_targets, by = c("date", "asset")) %>%
  group_by(asset) %>%
  fill(target_position, .direction = "down") %>%
  replace_na(list(target_position = 0)) %>%
  mutate(actual_position = lag(target_position)) %>%
  replace_na(list(actual_position = 0)) %>%
  ungroup() %>%
  
  # Join in filled prices and volatility
  left_join(daily_prices_long_filled, by = c("date", "asset")) %>%
  left_join(asset_volatility, by = c("date", "asset"))
  
cat("Daily positions and trade lag applied.\n")


# 4.1 --- Calculate Portfolio Returns (LOGIC FIX) ---
cat("Calculating portfolio returns (Inverse Volatility Weighting)...\n")

# 1. Calculate daily asset returns
daily_returns <- daily_positions %>%
  group_by(asset) %>%
  mutate(asset_return = (price / lag(price)) - 1) %>%
  replace_na(list(asset_return = 0)) %>%
  ungroup()

# 2. Calculate daily portfolio returns (Inverse Volatility Weighted)
strategy_returns <- daily_returns %>%
  # Our position is the 'actual_position' (lagged)
  filter(actual_position > 0 & !is.na(vol_52wk) & vol_52wk > 0) %>%
  
  # Calculate inverse volatility weight
  group_by(date) %>%
  mutate(
    inv_vol = 1 / vol_52wk,
    weight = inv_vol / sum(inv_vol) # Normalize weights to sum to 1
  ) %>%
  
  # Calculate the weighted return for the day
  summarise(
    n_assets = n(),
    strategy_return = weighted.mean(asset_return, w = weight),
    .groups = "drop"
  )

# 3. Get Benchmark returns
benchmark_returns <- daily_returns %>%
  filter(asset == BENCHMARK_TICKER) %>%
  select(date, benchmark_return = asset_return)
  
# 4. Combine and align
final_returns_table <- daily_prices_long_filled %>%
  distinct(date) %>%
  left_join(strategy_returns, by = "date") %>%
  left_join(benchmark_returns, by = "date") %>%
  replace_na(list(strategy_return = 0, benchmark_return = 0)) %>%
  filter(date >= min(model_table_final$date)) # Start on first model date

cat("Return calculation complete.\n")


# 5.1 --- Performance Metrics ---
cat("--- Performance Metrics --- \n")

returns_xts <- xts(
  final_returns_table[, c("strategy_return", "benchmark_return")],
  order.by = final_returns_table$date
)
colnames(returns_xts) <- c("Strategy", "Benchmark")

stats_table <- table.Stats(returns_xts)
print(stats_table)

sharpe <- SharpeRatio.annualized(returns_xts, Rf = 0)
cat("\nAnnualized Sharpe Ratio (Rf=0):\n")
print(sharpe)

max_dd <- maxDrawdown(returns_xts)
cat("\nMaximum Drawdown:\n")
print(max_dd)

# 5.2 --- Equity Curve Plot ---
cat("Generating equity curve plot...\n")

equity_curve_data <- final_returns_table %>%
  mutate(
    Strategy = cumprod(1 + strategy_return),
    Benchmark = cumprod(1 + benchmark_return)
  ) %>%
  select(date, Strategy, Benchmark) %>%
  pivot_longer(
    cols = c("Strategy", "Benchmark"),
    names_to = "Portfolio",
    values_to = "Equity"
  )

equity_plot <- ggplot(equity_curve_data, aes(x = date, y = Equity, color = Portfolio)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Strategy Equity Curve vs. Benchmark",
    subtitle = "Inverse Volatility Weighted, Monthly Rebalance, Long-Only",
    x = "Date",
    y = "Cumulative Return (Log Scale)"
  ) +
  scale_y_log10() +
  theme_minimal() +
  scale_color_manual(values = c("Strategy" = "blue", "Benchmark" = "grey40"))

print(equity_plot)

cat("--- Backtest Complete --- \n")


assets_wide |> ggplot(aes(x=date)) +
  geom_line(aes(y=EUNM, color="darkblue")) +
  theme_minimal()
  