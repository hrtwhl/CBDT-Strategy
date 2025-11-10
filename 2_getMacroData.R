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
# FRED SERIES
# =========================================================
fred_series <- c(
  # 1) USD & FX
  usd_broad_index   = "DTWEXBGS",
  usd_em_index      = "DTWEXEMEGS",
  usd_afe_index     = "DTWEXAFEGS",

  # 2) U.S. Rates & Curve
  fed_funds_rate    = "FEDFUNDS",
  yield_2y          = "DGS2",
  yield_10y         = "DGS10",
  term_spread_10y2y = "T10Y2Y",
  term_spread_10y3m = "T10Y3M",
  breakeven_infl_10y= "T10YIE",
  infl_exp_5y5y     = "T5YIFR",
  real_yield_10y    = "DFII10",

  # 3) Risk Appetite / Credit
  vix_index         = "VIXCLS",
  hy_spread         = "BAMLH0A0HYM2",
  bbb_spread        = "BAMLC0A4CBBB",
  stl_fsi           = "STLFSI4",
  chicago_fci       = "NFCI",

  # 5) Global / China Growth
  china_cli         = "CHNLOLITOAASTSAM",
  g7_cli            = "G7LOLITOAASTSAM",
  oecd_cli          = "OECDLOLITOTRGYSAM",

  # 6) U.S. Real Activity & Inflation
  industrial_prod   = "INDPRO",
  nonfarm_payrolls  = "PAYEMS",
  unemployment_rate = "UNRATE",
  initial_claims    = "ICSA",
  continuing_claims = "CCSA",
  real_retail_sales = "RRSFS",
  cpi_headline      = "CPIAUCSL",
  cpi_core          = "CPILFESL",
  ppi_all_commodities = "PPIACO",
  pce_index         = "PCEPI",
  pce_core          = "PCEPILFE"
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
# YAHOO FUTURES (full history, daily CLOSE)
# =========================================================
# Tickers per your list; Gold assumed GC=F
yf_futures <- tibble::tibble(
  yf = c("GD=F", "CL=F", "HG=F", "GC=F"),
  series = stringr::str_replace(yf, "=F$", "")   # drop '=F' -> CL, HG, GD, GC
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
# 1) FRED
fred_long <- get_fred_data(fred_series, start_date = "2000-01-01")

# 2) Yahoo futures
yf_long   <- get_yahoo_close(yf_futures)

# 3) Combined LONG (this is your final macro_long)
macro_long <- bind_rows(fred_long, yf_long)

# 4) Combined WIDE (this is your final macro_wide)
# 4) Combined WIDE (one row per date; allow NAs)
macro_wide <- macro_long %>%
  dplyr::select(date, series, value) %>%     # <- drop `source`
  dplyr::distinct() %>%                      # guard against accidental dups
  dplyr::group_by(date, series) %>%          # in case any ticker returns 2 rows/day
  dplyr::summarise(value = dplyr::last(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = series, values_from = value) %>%
  dplyr::arrange(date)


# Done:
#   - macro_long: date, series, value, source (FRED or YF)
#   - macro_wide: date + one column per series (FRED + futures)










