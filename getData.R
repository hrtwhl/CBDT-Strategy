# =========================================================
# GET DATA
# =========================================================

fred_tickers <- list(

  # 1) USD & FX
  usd = c(
    "DTWEXBGS",      # Broad U.S. Dollar Index (Nominal)
    "DTWEXEMEGS",    # USD vs Emerging Market Economies
    "DTWEXAFEGS"     # USD vs Advanced Foreign Economies
  ),

  # 2) U.S. Rates Level & Curve
  rates = c(
    "FEDFUNDS",      # Effective Fed Funds Rate
    "DGS2",          # 2-Year Treasury Yield
    "DGS10",         # 10-Year Treasury Yield
    "T10Y2Y",        # 10Y - 2Y Treasury Spread
    "T10Y3M",        # 10Y - 3M Treasury Spread
    "T10YIE",        # 10-Year Breakeven Inflation
    "T5YIFR",        # 5-Year, 5-Year Forward Inflation Expectation Rate
    "DFII10"         # 10-Year Real (TIPS) Yield
  ),

  # 3) Global Risk Appetite / Credit
  risk = c(
    "VIXCLS",        # CBOE Volatility Index
    "BAMLH0A0HYM2",  # ICE BofA High Yield OAS
    "BAMLC0A4CBBB",  # ICE BofA BBB Corporate OAS
    "STLFSI4",       # St. Louis Fed Financial Stress Index
    "NFCI"           # Chicago Fed National Financial Conditions Index
  ),

  # 4) Commodities
  commodities = c(
    "DCOILWTICO",    # WTI Crude Oil Spot Price
    "DCOILBRENTEU",  # Brent Crude Oil Spot Price
    "PCOPPUSDM",     # Copper price (USD/metric ton)
    "PALLFNFINDEXM"  # Global all-commodities price index
  ),

  # 5) Global / China Growth
  global_growth = c(
    "CHNLOLITOAASTSAM",  # OECD CLI China (Amplitude-Adjusted)
    "G7LOLITOAASTSAM",   # OECD CLI G7 (Amplitude-Adjusted)
    "OECDLOLITOTRGYSAM"  # OECD CLI OECD Total (Trend-Restored)
  ),

  # 6) U.S. Real Activity & Inflation
  us_macro = c(
    "INDPRO",       # Industrial Production
    "PAYEMS",       # Nonfarm Payrolls
    "UNRATE",       # Unemployment Rate
    "ICSA",         # Initial Jobless Claims
    "CCSA",         # Continuing Jobless Claims
    "RRSFS",        # Real Retail & Food Services Sales
    "CPIAUCSL",     # CPI (All Urban Consumers)
    "CPILFESL",     # Core CPI (ex Food & Energy)
    "PPIACO",       # Producer Price Index (All Commodities)
    "PCEPI",        # PCE Price Index
    "PCEPILFE"      # Core PCE Price Index
  )
)

# Flatten if needed
fred_all <- unlist(fred_tickers)
length(fred_all)
fred_all


library(fredr)
source("APIKey.R")

# download latest available data for all tickers
macro_data <- lapply(fred_all, function(series_id) {
  fredr(series_id = series_id, observation_start = as.Date("2000-01-01"))
})

# combine to data.frame
macro_df <- purrr::map_dfr(macro_data, \(x) {
  data.frame(date = x$date, series_id = x$series_id, value = x$value)
})

str(macro_df)
