# =============================================================================
# Distance Decay Estimation: Complete Analysis (Base R Version)
# =============================================================================
# This script estimates casino demand distance decay functions using
# Pennsylvania and Ohio revenue data with ZIP-level demographics.
# Uses only base R - no additional packages required.
#
# Author: Kahlil Philander
# Date: February 2026
# =============================================================================

cat("=======================================================================\n")
cat("DISTANCE DECAY ESTIMATION ANALYSIS\n")
cat("=======================================================================\n\n")

# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("Loading data...\n")

# Load existing geo-demand model data
allzips <- readRDS("../allzips.rds")
casinodata <- readRDS("../casinodata.rds")

cat("  Loaded", nrow(allzips), "ZIP codes\n")
cat("  Loaded", nrow(casinodata), "casino locations\n")

# Load revenue data
pa_revenue <- read.csv("data/pa_revenue_2022.csv")
oh_revenue <- read.csv("data/oh_revenue_2022.csv")

cat("  Loaded", nrow(pa_revenue), "PA properties with revenue\n")
cat("  Loaded", nrow(oh_revenue), "OH properties with revenue\n")

# Combine revenue data
revenue_data <- rbind(pa_revenue, oh_revenue)
revenue_data$gaming_positions <- revenue_data$num_slots + 6 * revenue_data$num_tables

cat("\nTotal properties with revenue:", nrow(revenue_data), "\n")
cat("Total observed revenue: $", round(sum(revenue_data$total_revenue_2022) / 1e9, 2), " billion\n")

# =============================================================================
# 2. DEFINE STUDY REGION
# =============================================================================

cat("\n--- DEFINING STUDY REGION ---\n")

# Target states
primary_states <- c("PA", "OH")
border_states <- c("NY", "NJ", "DE", "MD", "WV", "KY", "IN", "MI")
all_states <- c(primary_states, border_states)

# State name to abbreviation mapping
state_abbrev <- c(
  "Pennsylvania" = "PA", "Ohio" = "OH", "New York" = "NY",
  "New Jersey" = "NJ", "Delaware" = "DE", "Maryland" = "MD",
  "West Virginia" = "WV", "Kentucky" = "KY", "Indiana" = "IN",
  "Michigan" = "MI"
)

# Filter ZIP codes to study region
zips_study <- allzips[allzips$state.x %in% all_states, ]
zips_study <- data.frame(
  zipcode = zips_study$zipcode,
  state = zips_study$state.x,
  latitude = zips_study$latitude,
  longitude = zips_study$longitude,
  adultpop = zips_study$adultpop,
  income = zips_study$income
)

cat("Study region ZIP codes:", nrow(zips_study), "\n")

# Filter casinos to study region
casinos_study <- casinodata[casinodata$state %in% names(state_abbrev), ]
casinos_study <- data.frame(
  casino_id = casinos_study$id,
  name = casinos_study$name,
  state = state_abbrev[casinos_study$state],
  latitude = casinos_study$geocodehere_lat,
  longitude = casinos_study$geocodehere_lon
)

cat("Study region casinos:", nrow(casinos_study), "\n")

# =============================================================================
# 3. CALCULATE DISTANCES (Haversine)
# =============================================================================

cat("\n--- CALCULATING DISTANCES ---\n")

# Haversine distance function (returns miles)
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 3959  # Earth radius in miles

  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180

  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad

  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))

  return(R * c)
}

# Calculate distances from each ZIP to each casino
cat("Calculating distances for", nrow(zips_study), "ZIPs x", nrow(casinos_study), "casinos...\n")

# Create distance matrix (ZIPs x Casinos)
n_zips <- nrow(zips_study)
n_casinos <- nrow(casinos_study)

distance_matrix <- matrix(NA, nrow = n_zips, ncol = n_casinos)
colnames(distance_matrix) <- casinos_study$casino_id

for (j in 1:n_casinos) {
  distance_matrix[, j] <- haversine(
    zips_study$longitude, zips_study$latitude,
    casinos_study$longitude[j], casinos_study$latitude[j]
  )
}

cat("Distance calculation complete.\n")
cat("Distance range:", round(min(distance_matrix, na.rm=TRUE), 1), "to",
    round(max(distance_matrix, na.rm=TRUE), 1), "miles\n")

# =============================================================================
# 4. DISTANCE DECAY FUNCTIONS
# =============================================================================

decay_exponential <- function(d, beta) exp(-beta * d)
decay_power <- function(d, beta) (d + 0.1)^(-beta)
decay_gaussian <- function(d, beta) exp(-beta * d^2)

# =============================================================================
# 5. GRAVITY MODEL - PREDICT REVENUE
# =============================================================================

predict_revenue <- function(beta, decay_type = "exponential", max_dist = 150) {

  # Get casino indices that have revenue data
  revenue_casino_ids <- as.character(revenue_data$casino_id)
  all_casino_ids <- as.character(casinos_study$casino_id)

  # Gaming positions for observed casinos
  gaming_pos <- setNames(revenue_data$gaming_positions, as.character(revenue_data$casino_id))

  # For unobserved casinos, use average
  avg_gaming_pos <- mean(gaming_pos)
  all_gaming_pos <- setNames(rep(avg_gaming_pos, n_casinos), all_casino_ids)
  all_gaming_pos[names(gaming_pos)] <- gaming_pos

  # Calculate decay weights
  if (decay_type == "exponential") {
    decay_weights <- decay_exponential(distance_matrix, beta)
  } else if (decay_type == "power") {
    decay_weights <- decay_power(distance_matrix, beta)
  } else if (decay_type == "gaussian") {
    decay_weights <- decay_gaussian(distance_matrix, beta)
  }

  # Apply distance threshold
  decay_weights[distance_matrix > max_dist] <- 0

  # Calculate weighted attraction for each casino
  attraction <- sweep(decay_weights, 2, all_gaming_pos, "*")

  # Calculate denominator (sum of attractions for each ZIP)
  denom <- rowSums(attraction, na.rm = TRUE)
  denom[denom == 0] <- 1  # Avoid division by zero

  # Calculate demand share from each ZIP to each casino
  demand_share <- sweep(attraction, 1, denom, "/")

  # Weight by population and income
  pop_income <- zips_study$adultpop * (zips_study$income / 50000)
  demand_flow <- sweep(demand_share, 1, pop_income, "*")

  # Sum demand for each casino
  predicted_demand <- colSums(demand_flow, na.rm = TRUE)

  # Return predictions for observed casinos only
  return(predicted_demand[revenue_casino_ids])
}

# =============================================================================
# 6. OBJECTIVE FUNCTION (minimize SSE of revenue shares)
# =============================================================================

sse_function <- function(beta, decay_type) {
  if (beta <= 0) return(1e10)

  predicted <- predict_revenue(beta, decay_type)

  # Convert to shares
  observed_shares <- revenue_data$total_revenue_2022 / sum(revenue_data$total_revenue_2022)
  predicted_shares <- predicted / sum(predicted)

  # SSE
  sse <- sum((observed_shares - predicted_shares)^2, na.rm = TRUE)
  return(sse)
}

# =============================================================================
# 7. ESTIMATE MODELS
# =============================================================================

cat("\n")
cat("=======================================================================\n")
cat("ESTIMATING DISTANCE DECAY MODELS\n")
cat("=======================================================================\n")

results <- list()

# --- Model 1: Exponential ---
cat("\nModel 1: Exponential decay f(d) = exp(-beta * d)\n")
fit_exp <- optimize(
  f = function(b) sse_function(b, "exponential"),
  interval = c(0.001, 0.5),
  tol = 1e-6
)
results$exponential <- list(beta = fit_exp$minimum, sse = fit_exp$objective)
cat("  beta =", round(fit_exp$minimum, 5), ", SSE =", round(fit_exp$objective, 8), "\n")

# --- Model 2: Power ---
cat("\nModel 2: Power decay f(d) = d^(-beta)\n")
fit_pow <- optimize(
  f = function(b) sse_function(b, "power"),
  interval = c(0.1, 5),
  tol = 1e-6
)
results$power <- list(beta = fit_pow$minimum, sse = fit_pow$objective)
cat("  beta =", round(fit_pow$minimum, 4), ", SSE =", round(fit_pow$objective, 8), "\n")

# --- Model 3: Gaussian ---
cat("\nModel 3: Gaussian decay f(d) = exp(-beta * d^2)\n")
fit_gauss <- optimize(
  f = function(b) sse_function(b, "gaussian"),
  interval = c(0.00001, 0.01),
  tol = 1e-8
)
results$gaussian <- list(beta = fit_gauss$minimum, sse = fit_gauss$objective)
cat("  beta =", round(fit_gauss$minimum, 7), ", SSE =", round(fit_gauss$objective, 8), "\n")

# =============================================================================
# 8. MODEL COMPARISON
# =============================================================================

cat("\n")
cat("=======================================================================\n")
cat("MODEL COMPARISON\n")
cat("=======================================================================\n\n")

# Calculate R-squared (pseudo)
observed_shares <- revenue_data$total_revenue_2022 / sum(revenue_data$total_revenue_2022)
tss <- sum((observed_shares - mean(observed_shares))^2)

comparison <- data.frame(
  Model = c("Exponential", "Power", "Gaussian"),
  Beta = c(results$exponential$beta, results$power$beta, results$gaussian$beta),
  SSE = c(results$exponential$sse, results$power$sse, results$gaussian$sse),
  R_squared = 1 - c(results$exponential$sse, results$power$sse, results$gaussian$sse) / tss
)
comparison <- comparison[order(comparison$SSE), ]

print(comparison, row.names = FALSE)

# =============================================================================
# 9. BEST MODEL DETAILS
# =============================================================================

best_model <- comparison$Model[1]
cat("\n=== BEST MODEL:", best_model, "===\n")

# Get predictions from best model
if (best_model == "Exponential") {
  best_beta <- results$exponential$beta
  predicted <- predict_revenue(best_beta, "exponential")
} else if (best_model == "Power") {
  best_beta <- results$power$beta
  predicted <- predict_revenue(best_beta, "power")
} else {
  best_beta <- results$gaussian$beta
  predicted <- predict_revenue(best_beta, "gaussian")
}

cat("Beta parameter:", round(best_beta, 6), "\n")

# =============================================================================
# 10. PREDICTED VS ACTUAL
# =============================================================================

cat("\n")
cat("=======================================================================\n")
cat("PREDICTED VS ACTUAL REVENUE SHARES\n")
cat("=======================================================================\n\n")

results_table <- data.frame(
  Casino = revenue_data$name,
  State = revenue_data$state,
  Actual_Revenue_M = round(revenue_data$total_revenue_2022 / 1e6, 1),
  Actual_Share = round(revenue_data$total_revenue_2022 / sum(revenue_data$total_revenue_2022) * 100, 2),
  Predicted_Share = round(predicted / sum(predicted) * 100, 2)
)
results_table$Error_pct = results_table$Predicted_Share - results_table$Actual_Share

# Sort by actual revenue
results_table <- results_table[order(-results_table$Actual_Revenue_M), ]

print(results_table, row.names = FALSE)

# =============================================================================
# 11. SAVE RESULTS
# =============================================================================

cat("\n--- SAVING RESULTS ---\n")

# Create results directory
if (!dir.exists("results")) dir.create("results")

# Save model estimates
saveRDS(results, "results/model_estimates.rds")

# Save comparison table
write.csv(comparison, "results/model_comparison.csv", row.names = FALSE)

# Save predictions
write.csv(results_table, "results/predicted_vs_actual.csv", row.names = FALSE)

cat("Results saved to research/results/\n")

# =============================================================================
# 12. SUMMARY FOR PAPER
# =============================================================================

cat("\n")
cat("=======================================================================\n")
cat("SUMMARY FOR RESEARCH PAPER\n")
cat("=======================================================================\n\n")

cat("Distance Decay Function Comparison:\n\n")
cat("The", best_model, "decay function provides the best fit with:\n")
cat("  - Beta parameter:", round(best_beta, 6), "\n")
cat("  - R-squared:", round(comparison$R_squared[1], 4), "\n")
cat("  - SSE:", round(comparison$SSE[1], 8), "\n\n")

if (best_model == "Exponential") {
  cat("Interpretation: Demand decays at a constant rate of",
      round(best_beta * 100, 2), "% per mile.\n")
  cat("At 25 miles, demand is", round(exp(-best_beta * 25) * 100, 1), "% of local demand.\n")
  cat("At 50 miles, demand is", round(exp(-best_beta * 50) * 100, 1), "% of local demand.\n")
  cat("At 100 miles, demand is", round(exp(-best_beta * 100) * 100, 1), "% of local demand.\n")
} else if (best_model == "Power") {
  cat("Interpretation: Demand follows a power law with exponent", round(best_beta, 3), "\n")
  cat("At 25 miles, demand is", round((25.1)^(-best_beta) / (1.1)^(-best_beta) * 100, 1), "% of local demand.\n")
  cat("At 50 miles, demand is", round((50.1)^(-best_beta) / (1.1)^(-best_beta) * 100, 1), "% of local demand.\n")
}

cat("\nMean Absolute Percentage Error (MAPE):", round(mean(abs(results_table$Error_pct)), 2), "percentage points\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
