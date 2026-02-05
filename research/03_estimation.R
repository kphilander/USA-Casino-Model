# =============================================================================
# Distance Decay Estimation: Model Estimation
# =============================================================================
# This script estimates distance decay parameters using non-linear least squares.
# Compares five functional forms against observed PA/OH casino revenues.
#
# Author: Kahlil Philander
# Date: February 2026
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(minpack.lm)  # For nlsLM (robust NLS)
library(broom)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# Load distance data
zip_casino_dist <- readRDS("data/zip_casino_distances.rds")
casinos_all <- readRDS("data/casinos_study.rds")

# Load revenue data (USER MUST FILL THIS IN)
# Expected format: casino_id, total_revenue_2022, num_slots, num_tables
if (file.exists("data/pa_revenue_2022.csv")) {
  pa_revenue <- read.csv("data/pa_revenue_2022.csv")
} else {
  stop("Missing data/pa_revenue_2022.csv - please collect PA revenue data first")
}

if (file.exists("data/oh_revenue_2022.csv")) {
  oh_revenue <- read.csv("data/oh_revenue_2022.csv")
} else {
  stop("Missing data/oh_revenue_2022.csv - please collect OH revenue data first")
}

# Combine revenue data
revenue_data <- bind_rows(pa_revenue, oh_revenue) %>%
  filter(!is.na(total_revenue_2022)) %>%
  mutate(
    gaming_positions = num_slots + 6 * num_tables,
    revenue_millions = total_revenue_2022 / 1e6
  )

cat("Loaded revenue data for", nrow(revenue_data), "casinos\n")
cat("Total observed revenue: $", round(sum(revenue_data$total_revenue_2022) / 1e9, 2), " billion\n")

# =============================================================================
# 2. DISTANCE DECAY FUNCTIONS
# =============================================================================

#' Exponential decay: f(d) = exp(-beta * d)
decay_exponential <- function(d, beta) {
  exp(-beta * d)
}

#' Power decay: f(d) = d^(-beta)
#' Note: Add small constant to avoid division by zero
decay_power <- function(d, beta) {
  (d + 0.1)^(-beta)
}

#' Gaussian decay: f(d) = exp(-beta * d^2)
decay_gaussian <- function(d, beta) {
  exp(-beta * d^2)
}

#' Log-logistic decay: f(d) = 1 / (1 + (d/alpha)^beta)
decay_loglogistic <- function(d, alpha, beta) {
  1 / (1 + (d / alpha)^beta)
}

#' Combined exponential-power: f(d) = d^(-alpha) * exp(-beta * d)
decay_combined <- function(d, alpha, beta) {
  (d + 0.1)^(-alpha) * exp(-beta * d)
}

# =============================================================================
# 3. GRAVITY MODEL PREDICTION FUNCTION
# =============================================================================

#' Predict casino revenues using gravity model
#'
#' @param params Named vector of parameters (varies by model)
#' @param decay_fn Distance decay function to use
#' @param zip_casino_dist Data frame with ZIP-casino distances
#' @param revenue_data Data frame with casino characteristics
#' @param gamma Income elasticity (can be estimated or fixed)
#' @param delta Capacity elasticity (can be estimated or fixed)
#' @return Vector of predicted revenues for observed casinos
predict_revenue <- function(params, decay_fn, zip_casino_dist, revenue_data,
                            gamma = 1, delta = 1) {

  # Get observed casino IDs
  observed_ids <- revenue_data$casino_id

  # Calculate decay weights for all ZIP-casino pairs
  zip_casino_dist <- zip_casino_dist %>%
    mutate(
      decay_weight = decay_fn(distance_miles, params)
    )

  # Add casino capacity (attractiveness)
  casino_capacity <- revenue_data %>%
    select(casino_id, gaming_positions) %>%
    mutate(attractiveness = gaming_positions^delta)

  # For all casinos (including border state), assume average capacity
  all_casino_capacity <- casinos_all %>%
    select(casino_id) %>%
    left_join(casino_capacity, by = "casino_id") %>%
    mutate(
      attractiveness = ifelse(is.na(attractiveness),
                              mean(casino_capacity$attractiveness, na.rm = TRUE),
                              attractiveness)
    )

  # Add attractiveness to distance data
  zip_casino_dist <- zip_casino_dist %>%
    left_join(all_casino_capacity %>% select(casino_id, attractiveness),
              by = "casino_id")

  # Calculate denominator (sum of weighted attractions for each ZIP)
  zip_denominators <- zip_casino_dist %>%
    group_by(zipcode) %>%
    summarise(
      denom = sum(attractiveness * decay_weight, na.rm = TRUE),
      .groups = "drop"
    )

  # Calculate demand from each ZIP to each observed casino
  demand_flows <- zip_casino_dist %>%
    filter(casino_id %in% observed_ids) %>%
    left_join(zip_denominators, by = "zipcode") %>%
    mutate(
      # Demand = Pop * Income^gamma * (A_j * f(d)) / sum(A_k * f(d_k))
      demand = zip_pop * (zip_income / 50000)^gamma *
               attractiveness * decay_weight / denom
    )

  # Aggregate demand by casino
  predicted_revenue <- demand_flows %>%
    group_by(casino_id) %>%
    summarise(predicted_demand = sum(demand, na.rm = TRUE), .groups = "drop")

  # Match order to revenue_data
  predicted <- revenue_data %>%
    left_join(predicted_revenue, by = "casino_id") %>%
    pull(predicted_demand)

  return(predicted)
}

# =============================================================================
# 4. OBJECTIVE FUNCTION (Sum of Squared Errors)
# =============================================================================

#' Calculate SSE for revenue share prediction
#' @param params Parameter vector
#' @param decay_fn Distance decay function
#' @param ... Additional arguments passed to predict_revenue
sse_shares <- function(params, decay_fn, zip_casino_dist, revenue_data, ...) {

  # Predict revenues
  predicted <- predict_revenue(params, decay_fn, zip_casino_dist, revenue_data, ...)

  # Convert to shares
  observed_shares <- revenue_data$total_revenue_2022 / sum(revenue_data$total_revenue_2022)
  predicted_shares <- predicted / sum(predicted)

  # SSE
  sse <- sum((observed_shares - predicted_shares)^2)

  return(sse)
}

# =============================================================================
# 5. ESTIMATE MODELS
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("ESTIMATING DISTANCE DECAY MODELS\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")

results <- list()

# --- Model 1: Exponential ---
cat("\nModel 1: Exponential decay f(d) = exp(-beta * d)\n")
tryCatch({
  fit_exp <- optim(
    par = c(beta = 0.02),
    fn = function(p) sse_shares(p, function(d, params) decay_exponential(d, params["beta"]),
                                 zip_casino_dist, revenue_data),
    method = "Brent",
    lower = 0.001, upper = 0.5
  )
  results$exponential <- list(
    params = fit_exp$par,
    sse = fit_exp$value,
    convergence = fit_exp$convergence
  )
  cat("  beta =", round(fit_exp$par, 4), ", SSE =", round(fit_exp$value, 6), "\n")
}, error = function(e) cat("  Error:", e$message, "\n"))

# --- Model 2: Power ---
cat("\nModel 2: Power decay f(d) = d^(-beta)\n")
tryCatch({
  fit_pow <- optim(
    par = c(beta = 1.5),
    fn = function(p) sse_shares(p, function(d, params) decay_power(d, params["beta"]),
                                 zip_casino_dist, revenue_data),
    method = "Brent",
    lower = 0.1, upper = 5
  )
  results$power <- list(
    params = fit_pow$par,
    sse = fit_pow$value,
    convergence = fit_pow$convergence
  )
  cat("  beta =", round(fit_pow$par, 4), ", SSE =", round(fit_pow$value, 6), "\n")
}, error = function(e) cat("  Error:", e$message, "\n"))

# --- Model 3: Gaussian ---
cat("\nModel 3: Gaussian decay f(d) = exp(-beta * d^2)\n")
tryCatch({
  fit_gauss <- optim(
    par = c(beta = 0.001),
    fn = function(p) sse_shares(p, function(d, params) decay_gaussian(d, params["beta"]),
                                 zip_casino_dist, revenue_data),
    method = "Brent",
    lower = 0.00001, upper = 0.1
  )
  results$gaussian <- list(
    params = fit_gauss$par,
    sse = fit_gauss$value,
    convergence = fit_gauss$convergence
  )
  cat("  beta =", round(fit_gauss$par, 6), ", SSE =", round(fit_gauss$value, 6), "\n")
}, error = function(e) cat("  Error:", e$message, "\n"))

# --- Model 4: Log-logistic ---
cat("\nModel 4: Log-logistic decay f(d) = 1 / (1 + (d/alpha)^beta)\n")
tryCatch({
  fit_ll <- optim(
    par = c(alpha = 30, beta = 2),
    fn = function(p) sse_shares(p, function(d, params) decay_loglogistic(d, params["alpha"], params["beta"]),
                                 zip_casino_dist, revenue_data),
    method = "L-BFGS-B",
    lower = c(1, 0.5), upper = c(100, 5)
  )
  results$loglogistic <- list(
    params = fit_ll$par,
    sse = fit_ll$value,
    convergence = fit_ll$convergence
  )
  cat("  alpha =", round(fit_ll$par[1], 2), ", beta =", round(fit_ll$par[2], 4),
      ", SSE =", round(fit_ll$value, 6), "\n")
}, error = function(e) cat("  Error:", e$message, "\n"))

# --- Model 5: Combined ---
cat("\nModel 5: Combined decay f(d) = d^(-alpha) * exp(-beta * d)\n")
tryCatch({
  fit_comb <- optim(
    par = c(alpha = 0.5, beta = 0.01),
    fn = function(p) sse_shares(p, function(d, params) decay_combined(d, params["alpha"], params["beta"]),
                                 zip_casino_dist, revenue_data),
    method = "L-BFGS-B",
    lower = c(0, 0.001), upper = c(3, 0.2)
  )
  results$combined <- list(
    params = fit_comb$par,
    sse = fit_comb$value,
    convergence = fit_comb$convergence
  )
  cat("  alpha =", round(fit_comb$par[1], 4), ", beta =", round(fit_comb$par[2], 4),
      ", SSE =", round(fit_comb$value, 6), "\n")
}, error = function(e) cat("  Error:", e$message, "\n"))

# =============================================================================
# 6. MODEL COMPARISON
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("MODEL COMPARISON\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")

# Create comparison table
comparison <- tibble(
  model = names(results),
  n_params = sapply(results, function(x) length(x$params)),
  sse = sapply(results, function(x) x$sse)
) %>%
  mutate(
    # Calculate AIC and BIC (approximation using SSE)
    n_obs = nrow(revenue_data),
    aic = n_obs * log(sse / n_obs) + 2 * n_params,
    bic = n_obs * log(sse / n_obs) + n_params * log(n_obs),

    # Calculate pseudo-R2
    tss = var(revenue_data$total_revenue_2022 / sum(revenue_data$total_revenue_2022)) *
          (nrow(revenue_data) - 1),
    r_squared = 1 - sse / tss,

    # RMSE of shares
    rmse = sqrt(sse / n_obs)
  ) %>%
  arrange(sse)

cat("\n")
print(comparison %>% select(model, n_params, sse, aic, bic, r_squared, rmse))

# =============================================================================
# 7. BEST MODEL DETAILS
# =============================================================================

best_model <- comparison$model[1]
cat("\n=== BEST MODEL:", toupper(best_model), "===\n")
cat("Parameters:\n")
print(results[[best_model]]$params)

# =============================================================================
# 8. SAVE RESULTS
# =============================================================================

saveRDS(results, "results/model_estimates.rds")
saveRDS(comparison, "results/model_comparison.rds")

write.csv(comparison, "results/model_comparison.csv", row.names = FALSE)

cat("\n=== RESULTS SAVED ===\n")
cat("  - results/model_estimates.rds\n")
cat("  - results/model_comparison.rds\n")
cat("  - results/model_comparison.csv\n")

cat("\n=== NEXT STEP ===\n")
cat("Run 04_visualization.R to create figures\n")
