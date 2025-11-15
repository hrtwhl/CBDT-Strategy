# =========================================================
# PACKAGES
# =========================================================
required <- c("fredr", "dplyr", "tidyr", "purrr", "tidyquant", "stringr", "tibble")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(required, library, character.only = TRUE))

# =========================================================
# FRED API KEY
# =========================================================
# Your file should call: fredr_set_key("<YOUR_KEY>")
source("APIKey.R")

# =========================================================
# FRED SERIES (THE "CORE 6" MACRO LIST)
# =========================================================
# This list is based on our analysis of Heiden/AQR/Man
# and designed for low correlation and high EM-relevance.
fred_series <- c(
  # 1. Monetary Policy (AQR/Heiden Theme)
  yield_2y          = "DGS2",
  
  # 2. Risk Appetite (AQR/Heiden Theme)
  hy_spread         = "BAMLH0A0HYM2",
  
  # 3. Inflation (AQR/Heiden Theme)
  breakeven_infl_10y= "T10YIE",
  
  # 4. Currency / Trade (AQR/Heiden Theme)
  usd_broad_index   = "DTWEXBGS"
)

# Helper: download FRED as long table
get_fred_data <- function(named_series, start_date = "2000-01-01", freq = NULL) {
  purrr::imap_dfr(named_series, function(ticker, nice_name) {
    fredr(
      series_id = ticker,
      observation_start = as.Date(start_date),
      frequency = freq
    ) |>
      transmute(date, series = nice_name, value)
  }) |>
    mutate(source = "FRED")
}

# =========================================================
# YAHOO FUTURES (THE "CORE 6" MACRO LIST)
# =========================================================
# 5. Global Growth (Man Group Theme)
# 6. Inflation Shock (Man Group Theme)
yf_futures <- tibble::tibble(
  yf = c("HG=F", "CL=F"),
  series = c("Copper", "Oil")   # Clean names for the series
)

get_yahoo_close <- function(tickers_tbl) {
  tq_get(
    tickers_tbl$yf,
    get  = "stock.prices",
    from = as.Date("1900-01-01"),
    to   = Sys.Date()
  ) |>
    transmute(date, symbol, value = close) |>
    left_join(tickers_tbl, by = c("symbol" = "yf")) |>
    transmute(date, series, value, source = "YF") |>
    arrange(series, date)
}

# =========================================================
# DOWNLOAD & BUILD FINAL TABLES
# =========================================================
# 1) FRED (4 series)
fred_long <- get_fred_data(fred_series, start_date = "2000-01-01")

# 2) Yahoo futures (2 series)
yf_long   <- get_yahoo_close(yf_futures)

# 3) Combined LONG (this is your final macro_long with 6 series)
macro_long <- bind_rows(fred_long, yf_long)

# 4) Combined WIDE (this is your final macro_wide)
macro_wide <- macro_long %>%
  dplyr::select(date, series, value) %>%     # <- drop `source`
  dplyr::distinct() %>%                      # guard against accidental dups
  dplyr::group_by(date, series) %>%          # in case any ticker returns 2 rows/day
  dplyr::summarise(value = dplyr::last(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = series, values_from = value) %>%
  dplyr::arrange(date)


# Done:
#   - macro_long: date, series, value, source (FRED or YF)
#   - macro_wide: date + one column per series (Core 6)