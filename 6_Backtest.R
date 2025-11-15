# =========================================================
# SCRIPT 6: PORTFOLIO BACKTEST
#
# PURPOSE:
# This script simulates the performance of the strategy.
# It translates the "Alive/Dead" signals into a portfolio,
# applies a sizing logic, calculates daily returns,
# and deducts transaction costs.
#
# It answers the question: "Did this strategy work?"
#
# =========================================================

# --- 1. Load Data and Libraries ---
rm(list=setdiff(ls(), c("etfs"))) # Keep etfs list
source("1_getETFData.R") # For assets_wide (daily prices) & etfs list

# Load required libraries
required <- c("dplyr", "tidyr", "lubridate", "ggplot2", "padr", "readr")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(required, library, character.only = TRUE))

# Load our core engine signals
load("final_backtest_signals.RData")
# Load daily price data
load("master_table_weekly.RData") # This is misnamed, it's from 3_ProcessData
# Let's reload the *actual* daily prices from script 1
# This is safer, as we need all trading days.
daily_prices_wide <- assets_wide
daily_prices_long <- assets_long
rm(assets_wide, assets_long) # Clean up

cat("--- Script 6: Portfolio Backtest --- \n")
cat("Loaded 'final_backtest_signals' with", nrow(final_backtest_signals), "rows.\n")
cat("Loaded daily prices with", nrow(daily_prices_wide), "rows.\n\n")


# =========================================================
# --- 2. Configuration ---
# =========================================================

config <- list(
  # Transaction cost (0.1% per trade)
  t_cost = 0.001,

  # Benchmark ticker (must be in the 'asset' column)
  benchmark_ticker = "EUNM"
)

cat("--- Backtest Configuration --- \n")
cat("Transaction Cost:", config$t_cost * 100, "%\n")
cat("Benchmark:", config$benchmark_ticker, "\n\n")


# =========================================================
# --- 3. Prepare Signals (Monthly Rebalance) ---
#
# You trade monthly. We will get the signal from the
# *last Friday of each month* to simulate you
# running the engine at month-end.
# =========================================================

# 3.1 Get month-end Friday signals
monthly_signals <- final_backtest_signals %>%
  mutate(month_year = floor_date(date, "month")) %>%
  group_by(month_year, asset) %>%
  # Get the last signal from that month
  filter(date == max(date)) %>%
  ungroup() %>%
  select(date, asset, signal, blended_confidence)

cat("Filtered weekly signals down to", nrow(monthly_signals), "monthly decision points.\n")

# 3.2 Calculate Target Weights (Confidence-Weighted)
# Here we implement the "confidence-weighted" sizing.
target_weights <- monthly_signals %>%
  # Only "Alive" assets get a weight
  filter(signal == "Alive") %>%
  group_by(date) %>%
  # Sizing logic: weight = this_confidence / sum(all_confidences)
  mutate(weight = blended_confidence / sum(blended_confidence, na.rm = TRUE)) %>%
  ungroup() %>%
  # Pivot to wide format for joining
  select(date, asset, weight) %>%
  pivot_wider(
    names_from = asset,
    values_from = weight,
    values_fill = 0 # "Dead" assets or non-Alive get 0 weight
  )

cat("Calculated target portfolio weights for", nrow(target_weights), "rebalance dates.\n\n")


# =========================================================
# --- 4. Prepare Daily Data for Backtest ---
# =========================================================

# 4.1 Calculate Daily Returns for all assets
daily_returns <- daily_prices_wide %>%
  # Ensure assets are in the same order as target_weights
  select(date, all_of(colnames(target_weights)[-1])) %>%
  arrange(date) %>%
  mutate(
    across(
      .cols = -date,
      .fns = ~ ( .x / lag(.x) ) - 1,
      .names = "{.col}_ret"
    )
  ) %>%
  select(date, ends_with("_ret")) %>%
  # Rename columns back to just asset names
  rename_with(~ sub("_ret$", "", .), .cols = ends_with("_ret"))

# 4.2 Create Full Daily Weighting Table
# This "pads" the monthly weights to a daily table,
# simulating holding positions between rebalance dates.
daily_target_weights <- daily_returns %>%
  select(date) %>% # Use the daily_returns grid (trading days only)
  left_join(target_weights, by = "date") %>%
  # Fill forward the weights from the last rebalance date
  fill(everything(), .direction = "down") %>%
  # First rows will be NA before first signal
  na.omit()

# 4.3 Create Actual Holdings (Lagged)
# We lag the weights by 1 day to simulate "next day" trading.
# Our holdings for *today* are based on *yesterday's* signal.
daily_actual_holdings <- daily_target_weights %>%
  mutate(across(-date, ~ lag(.x, 1))) %>%
  # First row is now NA
  na.omit()

cat("Created daily holdings table.\n")


# =========================================================
# --- 5. Run Vectorized Backtest ---
#
# This is a professional-grade, vectorized backtest.
# We avoid loops for speed.
# =========================================================

# 5.1 Align returns and holdings
# Ensure both tables start and end on the same day
common_dates <- inner_join(
  select(daily_actual_holdings, date),
  select(daily_returns, date),
  by = "date"
)

holdings_final <- daily_actual_holdings %>%
  right_join(common_dates, by = "date")

returns_final <- daily_returns %>%
  right_join(common_dates, by = "date")

# 5.2 Calculate Daily Gross Portfolio Returns
# (Matrix multiplication: rowSums(holdings * returns))
portfolio_gross_returns <- rowSums(
  # [date] column is [1], assets are [2:N]
  holdings_final[, -1] * returns_final[, -1],
  na.rm = TRUE
)

# 5.3 Calculate Transaction Costs
# Costs are paid on "turnover"
turnover <- rowSums(
  # abs(today's holdings - yesterday's holdings)
  abs(holdings_final[, -1] - lag(holdings_final[, -1])),
  na.rm = TRUE
)
# First day has NA turnover, set to 0
turnover[1] <- 0

daily_costs <- turnover * config$t_cost

# 5.4 Calculate Net Returns
daily_net_returns <- portfolio_gross_returns - daily_costs

# 5.5 Create Final Results Table
results <- tibble(
  date = returns_final$date,
  strategy_ret = daily_net_returns
)

# 5.6 Add Benchmark Returns
benchmark_ret <- returns_final %>%
  select(date, all_of(config$benchmark_ticker))

results <- results %>%
  left_join(benchmark_ret, by = "date") %>%
  rename(benchmark_ret = all_of(config$benchmark_ticker)) %>%
  # First row will have NAs from cost calc
  na.omit()

cat("Vectorized backtest complete.\n\n")


# =========================================================
# --- 6. Calculate Performance Metrics ---
# =========================================================

# Helper for drawdown
calculate_drawdown <- function(returns) {
  cum_returns <- cumprod(1 + returns)
  peak <- cummax(cum_returns)
  drawdown <- (cum_returns / peak) - 1
  return(min(drawdown))
}

# Calculate stats
trading_days_per_year <- 252

stats_strategy <- results %>%
  summarise(
    CAGR = (last(cumprod(1 + strategy_ret)))^(trading_days_per_year / n()) - 1,
    AnnualizedVol = sd(strategy_ret) * sqrt(trading_days_per_year),
    SharpeRatio = CAGR / AnnualizedVol,
    MaxDrawdown = calculate_drawdown(strategy_ret)
  ) %>%
  mutate(Portfolio = "Strategy")

stats_benchmark <- results %>%
  summarise(
    CAGR = (last(cumprod(1 + benchmark_ret)))^(trading_days_per_year / n()) - 1,
    AnnualizedVol = sd(benchmark_ret) * sqrt(trading_days_per_year),
    SharpeRatio = CAGR / AnnualizedVol,
    MaxDrawdown = calculate_drawdown(benchmark_ret)
  ) %>%
  mutate(Portfolio = "Benchmark")

# Combine and print
final_stats <- bind_rows(stats_strategy, stats_benchmark) %>%
  select(Portfolio, CAGR, AnnualizedVol, SharpeRatio, MaxDrawdown)

cat("--- Performance Metrics --- \n")
print(final_stats, width = 80)
cat("\n")


# =========================================================
# --- 7. Plot Equity Curve ---
# =========================================================

# 7.1 Calculate cumulative returns
equity_curve <- results %>%
  mutate(
    Strategy = cumprod(1 + strategy_ret),
    Benchmark = cumprod(1 + benchmark_ret)
  ) %>%
  select(date, Strategy, Benchmark) %>%
  pivot_longer(
    cols = -date,
    names_to = "Portfolio",
    values_to = "CumulativeReturn"
  )

# 7.2 Create ggplot
equity_plot <- ggplot(equity_curve, aes(x = date, y = CumulativeReturn, color = Portfolio)) +
  geom_line(linewidth = 1) +
  #scale_y_log10() + # Log scale is best for long-term charts
  labs(
    title = "Strategy vs. Benchmark (EUNM) - Log Scale",
    x = "Date",
    y = "Cumulative Return (Log Scale)",
    color = "Portfolio"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c("Strategy" = "#0072B2", "Benchmark" = "#D55E00"))

# 7.3 Save the plot
print(equity_plot)
ggsave("equity_curve_plot.png", equity_plot, width = 12, height = 7, dpi = 300)

cat("Equity curve plot saved to 'equity_curve_plot.png'.\n")

# Save final metrics as well
write_csv(final_stats, "final_performance_metrics.csv")
cat("Performance metrics saved to 'final_performance_metrics.csv'.\n")


cat("\n\n--- Script 6 Complete --- \n")





library(xts)
library(dplyr)
library(PerformanceAnalytics)

# assuming 'results' is your tibble shown above
# make sure date is Date class
results <- results %>% mutate(date = as.Date(date))

# convert to xts excluding the date column
results_xts <- xts::xts(select(results, -date) %>% as.data.frame() %>% as.matrix(),
                        order.by = results$date)

# sanity checks
class(results_xts)    # should include "xts" "zoo"
is.numeric(coredata(results_xts))  # TRUE

# now call the function
table.AnnualizedReturns(results_xts)
charts.PerformanceSummary(results_xts)
