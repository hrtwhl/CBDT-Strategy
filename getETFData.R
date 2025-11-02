# ---- Packages ----
required <- c("tidyquant", "dplyr", "purrr", "tidyr", "stringr", "readr")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
library(tidyquant)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readr)

# ---- Master list (only columns needed for download) ----
etfs <- tribble(
  ~group, ~label, ~yf_ticker,
  # --- Single-Country EM Equity ETFs ---
  "Single Country", "China",        "XCS6.DE",
  "Single Country", "Taiwan",       "ITWN.MI",
  "Single Country", "India",        "QDV5.DE",
  "Single Country", "South Korea",  "XMKO.MI",
  "Single Country", "Brazil",       "DBX6.DE",
  "Single Country", "Saudi Arabia", "IUSV.DE",
  "Single Country", "South Africa", "SRSA.MI",
  "Single Country", "Mexico",       "XMEX.MI",
  "Single Country", "Malaysia",     "XCS3.DE",
  "Single Country", "Poland",       "IBCJ.DE",
  "Single Country", "Thailand",     "XCS4.DE",
  "Single Country", "Türkiye",      "ITKY.MI",
  "Single Country", "Philippines",  "XPQP.F",
  "Single Country", "Vietnam",      "XFVT.DE",
  # --- Regional / Broad EM Equity ETFs ---
  "Regional/Broad", "EM broad",     "IS3N.DE",
  "Regional/Broad", "EM Asia",      "CSEMAS.MI",
  "Regional/Broad", "EM LatAm",     "IUSC.DE",
  "Regional/Broad", "EM Small Cap", "EUNI.DE",
  # --- EM Factor / Style ETFs ---
  "Factor",         "EM Value",     "5MVL.DE",
  "Factor",         "EM Min Vol",   "EUNZ.F",
  # --- EM Bond ETFs ---
  "Bond",           "EM Sov USD",   "IUS7.DE",
  "Bond",           "EM Local",     "IUSP.DE",
  "Bond",           "EM Corp",      "IS0Q.DE"
) %>%
  distinct(yf_ticker, .keep_all = TRUE) %>%
  mutate(asset = str_replace(yf_ticker, "\\.[A-Za-z]+$", ""))  # drop '.XX' suffix

# Optional: quick check of tickers
# print(etfs)

# ---- Downloader (full history, daily) ----
# We’ll grab Adjusted (split/dividend-adjusted close) by default; you can change `price_col` below.
price_col <- "adjusted"  # one of: open, high, low, close, adjusted, volume

# Use tq_get once for all tickers; automatically returns a 'symbol' column
safe_tq <- purrr::safely(function(tks) {
  tidyquant::tq_get(tks, get = "stock.prices",
                    from = as.Date("1900-01-01"),  # effectively 'max'
                    to   = Sys.Date(),
                    complete_cases = FALSE)
})

res <- safe_tq(etfs$yf_ticker)

if (!is.null(res$error)) {
  message("Some tickers failed to download. Details below:")
  message(res$error)
}
raw_prices <- res$result
stopifnot(!is.null(raw_prices), nrow(raw_prices) > 0)

# then rebuild prices_long, prices_long_adj, prices_wide_* as before...


# ---- LONG dataframe (one row per date-asset) ----
prices_long <-
  raw_prices %>%
  # join back to map symbols -> cleaned asset names + groups/labels
  left_join(etfs, by = c("symbol" = "yf_ticker")) %>%
  transmute(
    date,
    asset,                  # cleaned (suffix removed)
    group,
    label,
    open, high, low, close, adjusted, volume
  ) %>%
  arrange(asset, date)

prices_long_all <- prices_long %>%
  tidyr::pivot_longer(
    cols = c(open, high, low, close, adjusted, volume),
    names_to   = "field",
    values_to  = "value"
  ) %>%
  arrange(asset, date, field)

# If you only want a single price column (e.g., Adjusted), keep this filtered view:
prices_long_adj <-
  prices_long %>%
  select(date, asset, group, label, price = all_of(price_col))

# ---- WIDE dataframe (columns = assets) ----
# 1) Wide for your chosen price column (e.g., Adjusted)
prices_wide_adj <-
  prices_long_adj %>%
  select(date, asset, price) %>%
  distinct() %>%
  pivot_wider(names_from = asset, values_from = price) %>%
  arrange(date)

# 2) (Optional) Wide by OHLCV (multi-level names), if you want everything wide:
prices_wide_all <-
  prices_long %>%
  pivot_longer(cols = c(open, high, low, close, adjusted, volume),
               names_to = "field", values_to = "value") %>%
  mutate(asset_field = paste(asset, field, sep = "__")) %>%  # colnames like IS0Q__adjusted
  select(date, asset_field, value) %>%
  distinct() %>%
  pivot_wider(names_from = asset_field, values_from = value) %>%
  arrange(date)

# ---- Results ----
# Long (all columns):
#   prices_long
# Long (Adjusted only):
#   prices_long_adj
# Wide (Adjusted only):
#   prices_wide_adj
# Wide (all OHLCV as separate columns per asset with suffix __field):
#   prices_wide_all


assets_long <- prices_long_adj
assets_wide <- prices_wide_adj


