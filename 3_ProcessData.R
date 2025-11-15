# Load Data and clean workspace
source("1_getETFData.R")
source("2_getMacroData.R")
#rm(list=setdiff(ls(), c("assets_long", "assets_wide", "macro_long", "macro_wide", "etfs")))


# --- Step 1: Data Pre-processing (The Monthly "Master Table") ---

# 1.1 Load necessary libraries
# (Many are loaded by the source scripts, but explicitly loading is safer)
library(dplyr)
library(tidyr)
library(lubridate)
library(padr) # Excellent for padding out time series

# 1.2 Define our master date grids
# Find the common start and end dates across both datasets
start_date <- min(min(assets_wide$date), min(macro_wide$date))
end_date <- max(max(assets_wide$date), max(macro_wide$date))

# Create a complete DAILY grid. This is essential for handling
# the mixed-frequency macro data correctly.
daily_grid <- tibble(date = seq.Date(from = start_date, to = end_date, by = "day"))

# Create our target MONTHLY grid (last trading day of month)
# We use assets_wide to ensure we only get trading days.
monthly_grid <- assets_wide %>%
  select(date) %>%
  mutate(month_year = floor_date(date, "month")) %>%
  group_by(month_year) %>%
  summarize(date = max(date), .groups = "drop") %>%
  select(date) # This is our new grid

cat("--- Date Grids Created --- \n")
cat("Daily grid:", min(daily_grid$date), "to", max(daily_grid$date), "\n")
cat("Monthly (Last Trading Day) grid:", min(monthly_grid$date), "to", max(monthly_grid$date), "\n")
cat("Total Months:", nrow(monthly_grid), "\n\n")


# 1.3 Process Macro Data (Point-in-Time)
# This is the most critical step to avoid lookahead bias.
# We align all macro data to the DAILY grid and "fill forward".
# This simulates holding the "last known value" (e.g., last month's CPI)
# until a new value is "released" (appears in the table).

macro_daily_filled <- daily_grid %>%
  # Join to the sparse macro_wide table
  left_join(macro_wide, by = "date") %>%
  # Sort by date (just in case)
  arrange(date) %>%
  # Fill forward ALL macro columns.
  # This carries the last known value forward.
  fill(everything(), .direction = "down") %>%
  # Now that we have a complete, point-in-time DAILY macro table,
  # we sample it on our target monthly grid.
  right_join(monthly_grid, by = "date")

cat("--- Macro Data Processed --- \n")
cat("Dimensions of filled macro data (Monthly):", dim(macro_daily_filled), "\n\n")


# 1.4 Process Asset Data (Monthly)
# This is much easier. We just need the asset prices on our target month-end dates.
assets_monthly <- assets_wide %>%
  right_join(monthly_grid, by = "date")

cat("--- Asset Data Processed --- \n")
cat("Dimensions of asset data (Monthly):", dim(assets_monthly), "\n\n")


# 1.5 Combine into Monthly Master Table
# We use an inner_join to ensure we only keep dates
# where we have *both* asset and (filled) macro data.
master_table_monthly <- inner_join(
  assets_monthly,
  macro_daily_filled,
  by = "date"
) %>%
  # The fill() operation at the start might leave NAs in the
  # very first rows if macro data started later than assets.
  # na.omit() is the simplest way to remove these initial incomplete rows.
  na.omit() %>%
  # Ensure final table is sorted by date
  arrange(date)

# 1.6 Final Check
# Let's inspect the result.
cat("--- Monthly Master Table --- \n")
cat("Final Dimensions:", dim(master_table_monthly), "\n")
cat("Final Date Range:", as.character(min(master_table_monthly$date)),
    "to", as.character(max(master_table_monthly$date)), "\n")
cat("Head:\n")
print(head(master_table_monthly[, 1:8])) # Print first 8 columns for brevity
cat("\nTail:\n")
print(tail(master_table_monthly[, 1:8]))

# Save the master table for the next step
save(master_table_monthly, file = "master_table_monthly.RData")

cat("\n\nStep 1 Complete. 'master_table_monthly' is ready and saved.\n")


