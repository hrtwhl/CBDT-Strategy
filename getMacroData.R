# =========================================================
# GET DATA
# =========================================================

library(fredr)
library(dplyr)
library(tidyr)
library(purrr)

source("APIKey.R")

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

  # 4) Commodities
  wti_oil           = "DCOILWTICO",
  brent_oil         = "DCOILBRENTEU",
  copper_price      = "PCOPPUSDM",
  all_commodities_index = "PALLFNFINDEXM",

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

get_fred_data <- function(named_series, start_date = "2000-01-01", freq = NULL) {
  # named_series: names = readable variable names, values = FRED tickers
  purrr::imap_dfr(named_series, function(ticker, nice_name) {
    fredr(
      series_id = ticker,
      observation_start = as.Date(start_date),
      frequency = freq
    ) |>
      transmute(date, series = nice_name, value)
  })
}

macro_long <- get_fred_data(fred_series, start_date = "2000-01-01")
macro_wide <- macro_long |>
  tidyr::pivot_wider(names_from = series, values_from = value) |>
  arrange(date)

View(macro_wide)

