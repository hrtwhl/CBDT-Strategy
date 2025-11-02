#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# REER AND CPI FROM BANK OF INTERNATIONAL SETTLEMENTS
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------



# Install once if needed
install.packages(c("readsdmx", "dplyr", "tidyr", "stringr", "lubridate", "purrr"))

library(readsdmx)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)


# Build a BIS SDMX data URL
# - dataflow: short id like "WS_EER" 
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
reer <- bis_get_many(
  dataflow   = dataflow,
  series_keys = series_keys
)

# Create wide dataframe with one column per country
reer <- reer %>%
  # Extract last two letters from series_key (e.g. "M.R.B.TH" → "TH")
  mutate(country = str_extract(series_key, "[A-Z]{2}$")) %>%
  
  # Keep only relevant columns
  select(date, country, value) %>%
  
  # Pivot to wide format
  pivot_wider(names_from = country, values_from = value) %>%
  
  # Sort columns alphabetically (optional)
  select(date, sort(tidyselect::peek_vars()))

rm(list=setdiff(ls(), "reer"))


# Define BIS dataflow
dataflow <- "WS_LONG_CPI" 

series_keys <- c(
  "M.CN.771",    
  "M.BR.771", 
  "M.IN.771", 
  "M.KR.771", 
  "M.ZA.771", 
  "M.SA.771", 
  "M.MX.771", 
  "M.AE.771", 
  "M.MY.771", 
  "M.ID.771", 
  "M.PL.771", 
  "M.TH.771" 
)

# Download all in one go
cpi <- bis_get_many(
  dataflow   = dataflow,
  series_keys = series_keys,
  start = "1995-01"
)

# 1) Keep only "normal, final" points (adjust if you want a different rule)
#    OBS_STATUS: "A" = normal; OBS_CONF: "F" = final
cpi_clean <- cpi %>%
  filter(OBS_STATUS == "A", OBS_CONF == "F") %>%
  # If there are still duplicates per (date, country), pick one deterministically
  group_by(date, REF_AREA) %>%
  summarise(value = first(na.omit(value)), .groups = "drop")

# 2) Pivot to wide: date + one column per country (2-letter code from REF_AREA)
cpi <- cpi_clean %>%
  rename(country = REF_AREA) %>%
  pivot_wider(names_from = country, values_from = value) %>%
  select(date, sort(names(.)[names(.) != "date"])) 

# Done: cpi_wide has 1 + (#countries) columns


rm(list=setdiff(ls(), c("reer", "cpi")))



#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# FRED
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

# install.packages(c("fredr", "tidyverse", "timetk", "lubridate", "zoo"))
library(fredr)
library(tidyverse)
library(timetk)
library(lubridate)
library(zoo)

# ------------------------------------------------------------
# FRED API KEY
# (A) temporary for this session:
# fredr_set_key("YOUR_FRED_API_KEY")

# (B) or store in .Renviron so it loads automatically:
# usethis::edit_r_environ()
# Add a line: FRED_API_KEY=your_key_here
# Then restart R:
fredr_set_key(Sys.getenv("FRED_API_KEY"))
# ------------------------------------------------------------
