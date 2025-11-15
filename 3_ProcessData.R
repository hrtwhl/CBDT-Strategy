# Load Data and clean workspace
source("1_getETFData.R")
source("2_getMacroData.R")
#rm(list=setdiff(ls(), c("assets_long", "assets_wide", "macro_long", "macro_wide", "etfs")))


# --- Step 1: Data Pre-processing (The Weekly "Master Table") ---

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

# Create our target WEEKLY grid (Fridays only).
# This will be the "index" of our final master table.
weekly_grid_fridays <- daily_grid %>%
  filter(wday(date, label = TRUE) == "Fri") %>%
  select(date)

cat("--- Date Grids Created --- \n")
cat("Daily grid:", min(daily_grid$date), "to", max(daily_grid$date), "\n")
cat("Weekly (Fri) grid:", min(weekly_grid_fridays$date), "to", max(weekly_grid_fridays$date), "\n")
cat("Total Fridays:", nrow(weekly_grid_fridays), "\n\n")




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
  # we sample it on our target Fridays.
  right_join(weekly_grid_fridays, by = "date")

cat("--- Macro Data Processed --- \n")
cat("Dimensions of filled macro data (Fridays):", dim(macro_daily_filled), "\n\n")


# 1.4 Process Asset Data (Weekly)
# This is much easier. We just need the asset prices on our target Fridays.
assets_weekly_fridays <- assets_wide %>%
  right_join(weekly_grid_fridays, by = "date")

cat("--- Asset Data Processed --- \n")
cat("Dimensions of asset data (Fridays):", dim(assets_weekly_fridays), "\n\n")


# 1.5 Combine into Weekly Master Table
# We use an inner_join to ensure we only keep dates
# where we have *both* asset and (filled) macro data.
master_table_weekly <- inner_join(
  assets_weekly_fridays,
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
cat("--- Weekly Master Table --- \n")
cat("Final Dimensions:", dim(master_table_weekly), "\n")
cat("Final Date Range:", as.character(min(master_table_weekly$date)),
    "to", as.character(max(master_table_weekly$date)), "\n")
cat("Head:\n")
print(head(master_table_weekly[, 1:8])) # Print first 8 columns for brevity
cat("\nTail:\n")
print(tail(master_table_weekly[, 1:8]))

# Save the master table for the next step
save(master_table_weekly, file = "master_table_weekly.RData")

cat("\n\nStep 1 Complete. 'master_table_weekly' is ready and saved.\n")



