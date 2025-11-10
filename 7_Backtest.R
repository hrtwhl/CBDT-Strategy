# ==============================================================================
# --- Step 6 (Corrected v6): MONTHLY Rebalancing, WEEKLY Mark-to-Market ---
# ==============================================================================
# This version fixes:
# 1. The lubridate/as_tibble() error.
# 2. The 'portfolio_panels' typo.

# 6.1 Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(scales)
library(lubridate) # For month/year functions

# 6.2 Load our required data
cat("--- Loading Data ---\n")
try(load("historical_signals.RData"), silent = TRUE)
if (!exists("historical_signals")) {
  stop("ERROR: 'historical_signals.RData' not found. Please re-run Step 5.")
}
try(load("master_table_weekly.RData"), silent = TRUE) 
if (!exists("master_table_weekly")) {
  try(load("master_table_weekly.RData"), silent = TRUE)
  if (!exists("master_table_weekly")) {
    stop("ERROR: 'master_table_weekly' file not found. Please re-run Step 1.")
  }
}
if (!exists("etfs")) {
  tryCatch(source("getETFData.R"), 
           error = function(e) stop("ERROR: Failed to source 'getETFData.R'"))
}
cat("All data loaded successfully.\n\n")

# 6.3 --- Define Benchmark & Strategy Assets ---
BENCHMARK_ASSET_TICKER <- "EUNM.DE"

benchmark_asset_name_clean <- etfs %>%
  filter(yf_ticker == BENCHMARK_ASSET_TICKER) %>%
  pull(asset)

if (length(benchmark_asset_name_clean) == 0) {
  stop(paste("Could not find", BENCHMARK_ASSET_TICKER, "in getETFData.R"))
}
cat("Benchmark set to:", BENCHMARK_ASSET_TICKER, "(Clean name:", benchmark_asset_name_clean, ")\n")

all_asset_cols <- etfs$asset
strategy_asset_names <- etfs %>%
  filter(asset != benchmark_asset_name_clean) %>%
  pull(asset)
cat("Strategy will trade", length(strategy_asset_names), "assets.\n\n")

# 6.4 --- Calculate "Ground Truth" 1-WEEK FORWARD Returns ---
actual_returns_table <- master_table_weekly %>%
  select(date, all_of(all_asset_cols)) %>%
  pivot_longer(cols = -date, names_to = "asset", values_to = "price") %>%
  group_by(asset) %>%
  mutate(actual_fwd_ret_1wk = (lead(price, 1) / price) - 1) %>%
  ungroup() %>%
  select(date, asset, actual_fwd_ret_1wk)

cat("Calculated actual 1-WEEK forward returns.\n")

# 6.5 --- Create MONTHLY-FILLED Signal Table ---
cat("Creating monthly-filled signal table...\n")

# 1. Get all dates in our backtest
all_dates_in_backtest <- unique(historical_signals$date)

# 2. Find the "Decision Dates" (first Friday of each month)
decision_dates <- all_dates_in_backtest %>%
  # --- --- --- THIS IS THE FIX --- --- ---
  tibble(date = .) %>% # Create a tibble with the correct column name
  # --- --- --- END OF FIX --- --- ---
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarise(date = first(date), .groups = "drop") %>%
  pull(date)

# 3. Filter our signals to *only* these decision dates
monthly_signals <- historical_signals %>%
  filter(date %in% decision_dates) %>%
  select(date, asset, signal = signal_4wk) # We only need the signal

# 4. Create a full grid of ALL dates and ALL assets
full_weekly_grid <- expand.grid(
  date = all_dates_in_backtest,
  asset = all_asset_cols,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# 5. Fill forward the monthly signals
signals_filled_weekly <- full_weekly_grid %>%
  left_join(monthly_signals, by = c("date", "asset")) %>%
  arrange(asset, date) %>%
  group_by(asset) %>%
  fill(signal, .direction = "down") %>%
  ungroup() %>%
  na.omit() 

cat("Monthly signals filled forward.\n")

# 6.6 --- Join Signals with 1-WEEK Returns ---
backtest_data <- inner_join(
  signals_filled_weekly,
  actual_returns_table,
  by = c("date", "asset")
) %>%
  na.omit() 

cat("Joined signals with 1-WEEK returns.\n")

# 6.7 --- Simulate Portfolio Returns (WEEKLY) ---
# 6.7.1 Calculate *Strategy* Returns
strategy_returns_weekly <- backtest_data %>%
  filter(asset %in% strategy_asset_names) %>%
  mutate(
    strategy_return = case_when(
      signal == "Long"    ~ 1 * actual_fwd_ret_1wk,
      signal == "Short"   ~ -1 * actual_fwd_ret_1wk,
      signal == "Neutral" ~ 0.0
    )
  ) %>%
  group_by(date) %>%
  summarise(
    strategy_return_weekly = mean(strategy_return, na.rm = TRUE)
  ) %>%
  ungroup()

# 6.7.2 Get *Benchmark* Returns (Weekly)
benchmark_returns_weekly <- actual_returns_table %>%
  filter(asset == benchmark_asset_name_clean) %>%
  select(date, bh_return_weekly = actual_fwd_ret_1wk) %>%
  na.omit()

# 6.7.3 Combine Strategy and Benchmark
portfolio_returns <- inner_join(
  strategy_returns_weekly,
  benchmark_returns_weekly,
  by = "date"
) %>%
  arrange(date)

cat("Simulated WEEKLY portfolio returns (from MONTHLY signals).\n")

# 6.8 --- Calculate Cumulative Performance for Plotting ---
final_performance_for_plot <- portfolio_returns %>%
  mutate(
    Strategy = cumprod(1 + strategy_return_weekly),
    Benchmark = cumprod(1 + bh_return_weekly)
  ) %>%
  pivot_longer(
    cols = c(Strategy, Benchmark), 
    names_to = "Portfolio", 
    values_to = "Cumulative Return"
  ) %>%
  mutate(
    Portfolio = ifelse(Portfolio == "Benchmark", 
                       paste("Buy & Hold:", BENCHMARK_ASSET_TICKER), 
                       "Analogue Strategy")
  )

cat("Calculated cumulative performance.\n\n")

# ==============================================================================
# --- 7. PLOT: Equity Curve ---
# ==============================================================================

equity_curve_plot <- ggplot(final_performance_for_plot, 
                            aes(x = date, y = `Cumulative Return`, color = Portfolio)) +
  geom_line(linewidth = 1) +
  #scale_y_log10(labels = scales::percent_format(accuracy = 1)) + 
  labs(
    title = "Analogue Strategy Performance vs. Benchmark (Log Scale)",
    subtitle = paste("MONTHLY Rebalancing | Analogue Strategy vs.", BENCHMARK_ASSET_TICKER),
    x = "Date",
    y = "Cumulative Return (Log Scale)",
    color = "Portfolio"
  ) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("equity_curve_vs_benchmark_MONTHLY.png", equity_curve_plot, width = 10, height = 6)
cat("--- Equity curve plot saved to 'equity_curve_vs_benchmark_MONTHLY.png' ---\n\n")
print(equity_curve_plot)

# ==============================================================================
# --- 8. TABLE: Performance Metrics ---
# ==============================================================================

cat("--- Performance Metrics (Monthly Rebalancing) ---\n")

periods_per_year <- 52 
n_periods <- nrow(portfolio_returns)
n_years <- n_periods / periods_per_year

# 1. Analogue Strategy Metrics
ann_ret_strat <- (prod(1 + portfolio_returns$strategy_return_weekly))^(periods_per_year/n_periods) - 1
# --- --- --- THIS IS THE FIX --- --- ---
ann_vol_strat <- sd(portfolio_returns$strategy_return_weekly) * sqrt(periods_per_year) # Was 'portfolio_panels'
# --- --- --- END OF FIX --- --- ---
sharpe_strat <- ann_ret_strat / ann_vol_strat # Assuming 0% risk-free rate

# 2. Benchmark Metrics
ann_ret_bench <- (prod(1 + portfolio_returns$bh_return_weekly))^(periods_per_year/n_periods) - 1
ann_vol_bench <- sd(portfolio_returns$bh_return_weekly) * sqrt(periods_per_year)
sharpe_bench <- ann_ret_bench / ann_vol_bench # Assuming 0% risk-free rate

# 3. Create the table
metrics_table <- tibble(
  Metric = c("Annualized Return", "Annualized Volatility", "Sharpe Ratio (Rf=0)"),
  `Analogue Strategy` = c(ann_ret_strat, ann_vol_strat, sharpe_strat),
  `Benchmark (EUNM.DE)` = c(ann_ret_bench, ann_vol_bench, sharpe_bench)
)

# 4. Corrected Print block
formatted_metrics_table <- metrics_table %>%
  mutate(across(
    .cols = where(is.numeric), 
    .fns = ~ if_else(
      Metric == "Sharpe Ratio (Rf=0)",        
      scales::number(.x, accuracy = 0.01),    
      scales::percent(.x, accuracy = 0.01)    
    )
  ))

print(formatted_metrics_table)

cat("\n--- All Steps Complete --- \n")