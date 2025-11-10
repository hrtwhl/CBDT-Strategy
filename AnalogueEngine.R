# --- Step 5: The Analogue Engine ---
#
# This script defines the core functions that form the "Schrödinger's Cat" model.
# It does not run the full backtest, but it defines the functions that will:
# 1. Take a specific asset and date.
# 2. Find its unique "market fingerprint" (price dynamics + macro context).
# 3. Search history for the N closest analogues (most similar past fingerprints).
# 4. Analyze the forward returns of those analogues.
# 5. "Open the box" and classify the asset as "Alive" or "Dead".

# --- 5.1 Load Data & Libraries ---

# Source all previous work to load data and helper functions
# This will give us:
# - feature_table_aligned (Our "memory" of all past fingerprints)
# - outcome_table_aligned (Our "memory" of what happened next)
# - asset_cols (The list of our assets)
# - macro_cols (The list of our macro factors)
if (!exists("feature_table_aligned")) {
  source("4_FeatureEngineering.R")
}

# We need a library for fast K-Nearest-Neighbors (k-NN) search.
# This is much faster than calculating Euclidean distance manually.
if (!require("FNN")) {
  install.packages("FNN")
  library(FNN)
}
library(dplyr)
library(stringr)

cat("--- Step 5: Analogue Engine --- \n")
cat("Data loaded. Engine functions will be defined.\n\n")


# --- 5.2 Define Model Parameters ---
# These are the "knobs" of our model, based on the articles.
N_ANALOGUES <- 50  # Number of "closest" historical matches to find

# Horizon weights (from "Macro Lens #1": "heavier emphasis on nearer horizons")
HORIZON_WEIGHTS <- c(w_4wk = 0.5, w_8wk = 0.3, w_12wk = 0.2)

# "Alive/Dead" filter thresholds (from "Macro Lens #1": "mean-infully positive", "consistent")
ALIVE_THRESHOLD_MEAN <- 0.01  # Blended mean return must be > 1%
ALIVE_THRESHOLD_CONFIDENCE <- 0.55 # Blended confidence (pct_positive) must be > 55%


# --- 5.3 Helper: Identify Feature Columns ---
# We need to know which columns in the feature_table belong to assets
# and which are the common macro factors.

# Get all asset-specific feature names (e.g., "XCS6.DE__mom_4wk")
all_asset_feature_names <- unlist(lapply(
  asset_cols,
  function(a) grep(paste0("^", a, "__"), names(feature_table_aligned), value = TRUE)
))

# Get all macro feature names (e.g., "term_spread_10y2y__change_13wk")
macro_feature_names <- setdiff(
  names(feature_table_aligned),
  c("date", all_asset_feature_names)
)

cat("Identified", length(macro_feature_names), "macro features.\n")
cat("Identified", length(all_asset_feature_names), "total asset-specific features.\n\n")


# --- 5.4 Engine Function 1: Get Feature Vectors ---
# Gets the "fingerprint" for one asset on one date, and the "historical memory"
get_feature_vectors <- function(asset_name,
                                current_date,
                                feature_table,
                                all_macro_features = macro_feature_names) {
  
  # 1. Identify all feature columns relevant to THIS asset
  asset_specific_features <- grep(paste0("^", asset_name, "__"),
                                  names(feature_table), value = TRUE)
  
  feature_names_for_asset <- c(asset_specific_features, all_macro_features)

  # 2. Get "Today's" vector
  vec_today <- feature_table %>%
    filter(date == current_date) %>%
    select(all_of(feature_names_for_asset)) %>%
    as.matrix()

  # 3. Get "History's" matrix (all dates before "today")
  mat_history <- feature_table %>%
    filter(date < current_date) %>%
    select(all_of(feature_names_for_asset)) %>%
    as.matrix()
  
  # 4. Get the dates for that history (to map back later)
  history_dates <- feature_table %>%
    filter(date < current_date) %>%
    pull(date)
    
  return(list(
    today = vec_today,
    history = mat_history,
    history_dates = history_dates
  ))
}


# --- 5.5 Engine Function 2: Find Analogues (k-NN) ---
# Finds the N closest historical matches to "today's" vector
find_analogues <- function(vec_today, mat_history, n_analogues = N_ANALOGUES) {
  
  # Use FNN::get.knnx to find the k-nearest neighbors (k-NN)
  # This calculates the Euclidean distance and finds the 'k' smallest
  nn <- FNN::get.knnx(
    data = mat_history,
    query = vec_today,
    k = n_analogues
  )

  # The output nn.index is a matrix, we just want the first row
  analogue_indices <- nn$nn.index[1, ]
  
  return(analogue_indices)
}


# --- 5.6 Engine Function 3: Get Analogue Outcomes ---
# Looks up what happened in the future for our N analogues
get_analogue_outcomes <- function(asset_name,
                                  analogue_indices,
                                  history_dates,
                                  outcome_table = outcome_table_aligned) {

  # 1. Find the specific dates of our N analogues
  analogue_dates <- history_dates[analogue_indices]

  # 2. Identify the outcome columns for THIS asset
  outcome_cols <- grep(paste0("^", asset_name, "__fwd_ret_"),
                       names(outcome_table), value = TRUE)

  # 3. Look up the outcomes for those dates
  analogue_outcomes_df <- outcome_table %>%
    filter(date %in% analogue_dates) %>%
    select(all_of(outcome_cols))
    
  # Rename for easier use (e.g., "XCS6.DE__fwd_ret_4wk" -> "fwd_ret_4wk")
  names(analogue_outcomes_df) <- str_remove(names(analogue_outcomes_df),
                                            paste0(asset_name, "__"))

  return(analogue_outcomes_df)
}


# --- 5.7 Engine Function 4: Evaluate Analogues ("Alive/Dead" Filter) ---
# This is "the cat." It looks at the outcomes and makes a decision.
evaluate_analogues <- function(analogue_outcomes_df,
                               weights = HORIZON_WEIGHTS,
                               min_mean = ALIVE_THRESHOLD_MEAN,
                               min_confidence = ALIVE_THRESHOLD_CONFIDENCE) {
  
  n_found <- nrow(analogue_outcomes_df)
  
  # Safety check: if we didn't find enough analogues, stay flat
  if (n_found < N_ANALOGUES / 2) {
    return(list(
      signal = "Dead",
      n_analogues = n_found,
      blended_mean = NA,
      blended_confidence = NA
    ))
  }
  
  # 1. Calculate stats for each horizon
  stats_4wk <- list(
    mean = mean(analogue_outcomes_df$fwd_ret_4wk, na.rm = TRUE),
    confidence = mean(analogue_outcomes_df$fwd_ret_4wk > 0, na.rm = TRUE)
  )
  stats_8wk <- list(
    mean = mean(analogue_outcomes_df$fwd_ret_8wk, na.rm = TRUE),
    confidence = mean(analogue_outcomes_df$fwd_ret_8wk > 0, na.rm = TRUE)
  )
  stats_12wk <- list(
    mean = mean(analogue_outcomes_df$fwd_ret_12wk, na.rm = TRUE),
    confidence = mean(analogue_outcomes_df$fwd_ret_12wk > 0, na.rm = TRUE)
  )
  
  # 2. Blend the horizons (per "Macro Lens #1" methodology)
  blended_mean <- (stats_4wk$mean * weights["w_4wk"]) +
                  (stats_8wk$mean * weights["w_8wk"]) +
                  (stats_12wk$mean * weights["w_12wk"])
  
  blended_confidence <- (stats_4wk$confidence * weights["w_4wk"]) +
                        (stats_8wk$confidence * weights["w_8wk"]) +
                        (stats_12wk$confidence * weights["w_12wk"])
                        
  # 3. Apply the "Alive/Dead" filter
  is_alive <- (blended_mean > min_mean) & (blended_confidence > min_confidence)
  signal <- ifelse(is_alive, "Alive", "Dead")
  
  return(list(
    signal = signal,
    n_analogues = n_found,
    blended_mean = blended_mean,
    blended_confidence = blended_confidence,
    stats_4wk = stats_4wk,
    stats_8wk = stats_8wk,
    stats_12wk = stats_12wk
  ))
}


# --- 5.8 Main Wrapper Function ---
# A single function to run the entire test for one asset-date
run_schrodinger_test <- function(asset_name,
                                 current_date,
                                 feature_table = feature_table_aligned,
                                 outcome_table = outcome_table_aligned,
                                 all_macro_features = macro_feature_names,
                                 n_analogues = N_ANALOGUES) {
  
  # 1. Get vectors
  vectors <- get_feature_vectors(
    asset_name,
    current_date,
    feature_table,
    all_macro_features
  )
  
  # 2. Find analogues
  analogue_indices <- find_analogues(
    vectors$today,
    vectors$history,
    n_analogues
  )
  
  # 3. Get outcomes
  outcomes_df <- get_analogue_outcomes(
    asset_name,
    analogue_indices,
    vectors$history_dates,
    outcome_table
  )
  
  # 4. "Open the box"
  result <- evaluate_analogues(outcomes_df)
  
  return(result)
}


# --- 5.9 TEST RUN ---
# Let's test the engine for one asset on one date to see if it works.

# We'll pick the first asset in our list
test_asset <- asset_cols[1] 

# We'll pick a date from the middle of our dataset
test_date <- feature_table_aligned$date[floor(nrow(feature_table_aligned) / 2)]

cat("--- TEST RUN --- \n")
cat("Running test for asset:", test_asset, "\n")
cat("On test date:", as.character(test_date), "\n\n")

# Run the full test
test_result <- run_schrodinger_test(test_asset, test_date)

# Print the result
cat("--- TEST RESULT --- \n")
print(test_result)
cat("\n")

cat("Step 5 Complete. Analogue engine functions are defined and tested.\n")
cat("We are now ready for Step 6: The Backtest.\n")

# Clean up environment, leaving only the main tables and engine functions
rm(list=setdiff(ls(), c("feature_table_aligned",
                        "outcome_table_aligned",
                        "asset_cols",
                        "macro_cols",
                        "macro_feature_names",
                        "all_asset_feature_names",
                        "N_ANALOGUES",
                        "HORIZON_WEIGHTS",
                        "ALIVE_THRESHOLD_MEAN",
                        "ALIVE_THRESHOLD_CONFIDENCE",
                        "run_schrodinger_test",
                        "get_feature_vectors",
                        "find_analogues",
                        "get_analogue_outcomes",
                        "evaluate_analogues")))

save.image(file = "engine_workspace.RData")
cat("Final engine workspace saved to 'engine_workspace.RData'\n")