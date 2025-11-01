# Install once if needed
install.packages(c("readsdmx", "dplyr", "tidyr", "stringr", "lubridate", "purrr"))

library(readsdmx)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)


# Build a BIS SDMX data URL
# - dataflow: short id like "WS_EER" (even if BIS shows "BIS,WS_EER,1.0")
# - series_key: e.g. "M.R.N.CA"
# - start, end: "YYYY-MM" or "YYYY" (optional)
bis_build_url <- function(dataflow, series_key, start = NULL, end = NULL) {
  # BIS commonly uses this SDMX REST v1 path:
  #   https://stats.bis.org/api/v1/data/{flow}/{key}
  base <- "https://stats.bis.org/api/v1"
  path <- sprintf("data/%s/%s", dataflow, series_key)

  # Add time filters if provided
  # BIS accepts `startPeriod` / `endPeriod` in YYYY-MM or YYYY
  qs <- c()
  if (!is.null(start)) qs <- c(qs, paste0("startPeriod=", start))
  if (!is.null(end))   qs <- c(qs, paste0("endPeriod=", end))
  query <- if (length(qs)) paste0("?", paste(qs, collapse = "&")) else ""

  paste0(base, "/", path, query)
}

bis_get_series <- function(dataflow, series_key, start = NULL, end = NULL) {
  url <- bis_build_url(dataflow, series_key, start, end)
  sdmx_obj <- readsdmx::read_sdmx(url)
  df <- as.data.frame(sdmx_obj)

  time_col  <- dplyr::coalesce(df$TIME_PERIOD, df$time, df$obsTime)
  value_col <- dplyr::coalesce(df$obsValue, df$OBS_VALUE, df$value)

  out <- df %>%
    mutate(
      period = time_col,
      value  = suppressWarnings(as.numeric(value_col)),
      date   = parse_period(period)
    ) %>%
    # sanity check for unparsed periods
    { 
      bad <- sum(is.na(.$date) & !is.na(.$period))
      if (bad > 0) warning(bad, " period(s) could not be parsed into a Date. Check 'period' values.")
      .
    } %>%
    arrange(date) %>%
    mutate(
      dataflow   = dataflow,
      series_key = series_key
    ) %>%
    relocate(dataflow, series_key, date, period, value)

  out
}



bis_get_many <- function(dataflow, series_keys, start = NULL, end = NULL, .id = "series_key") {
  series_keys %>%
    set_names() %>%
    map_dfr(~ bis_get_series(dataflow, .x, start, end), .id = .id)
}

# Robust period parser for BIS SDMX
parse_period <- function(x) {
  x <- as.character(x)

  # Trim whitespace just in case
  x <- trimws(x)

  parsed <- case_when(
    # Full date like 2025-09-30
    str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$") ~ suppressWarnings(ymd(x)),

    # Monthly date like 1964-01 → use first of month
    str_detect(x, "^\\d{4}-\\d{2}$") ~ suppressWarnings(ym(x)),

    # Year only like 1964 → use Jan 1
    str_detect(x, "^\\d{4}$") ~ suppressWarnings(ymd(paste0(x, "-01-01"))),

    # If you ever get quarters like 1964-Q1, uncomment next line:
    # str_detect(x, "^\\d{4}-Q[1-4]$") ~ suppressWarnings(yq(str_replace(x, "-", " "))),
    
    TRUE ~ as.Date(NA)  # anything unexpected becomes NA; we’ll check later
  )

  parsed
}


# Canada REER Narrow (Real, Monthly)
can_reer <- bis_get_series(
  dataflow   = "WS_EER",
  series_key = "M.R.N.CA",
  start      = "1964-01",   # optional
  end        = "2025-09"    # optional
)

dplyr::glimpse(can_reer)
head(can_reer, 10)
tail(can_reer, 5)



# Define BIS dataflow
dataflow <- "WS_EER"

# Define several series keys (structure: FREQ.TYPE.BASKET.COUNTRY)
series_keys <- c(
  "M.R.B.CN",    
  "M.R.B.BR",
  "M.R.B.IN", 
  "M.R.B.KR",
  "M.R.B.ZA",
  "M.R.B.SA",
  "M.R.B.MX",
  "M.R.B.AE",
  "M.R.B.MY",
  "M.R.B.ID",
  "M.R.B.PL",
  "M.R.B.TH"
)

# Download all in one go
reer_many <- bis_get_many(
  dataflow   = dataflow,
  series_keys = series_keys,
  start      = "2000-01"   # optional: only from 2000 onwards
)

# Inspect structure
dplyr::glimpse(reer_many)


summary(reer_many)

install.packages("ggplot2")
library(ggplot2)


ggplot(reer_many, aes(x = date, y = value, color = series_key)) +
  geom_line(size = 0.9) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Real Effective Exchange Rate",
    subtitle = "Broad Basket, 2020 = 100",
    caption = "Source: Bank of Internationas Settlements",
    x = NULL,
    y = NULL,
    color = "Series"
  ) +
  theme(
    plot.title.position = "plot",
    legend.position = "top",
    legend.title = element_blank() 
  )


