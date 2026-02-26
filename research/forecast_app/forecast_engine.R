# =============================================================================
# Forecast Engine for Shiny App
# =============================================================================
# Modified version of forecast_casino() that:
#   1) Accepts pre-loaded data bundle (env) instead of global state
#   2) Returns catchment probabilities for map visualization
#   3) Includes state auto-detection helper
# =============================================================================

# ---- Haversine distance (miles) ----
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 3959
  lon1_rad <- lon1 * pi / 180; lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180; lat2_rad <- lat2 * pi / 180
  dlon <- lon2_rad - lon1_rad; dlat <- lat2_rad - lat1_rad
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  R * 2 * asin(sqrt(a))
}

# ---- Power decay function ----
decay_power <- function(d, beta) (d + 0.1)^(-beta)

# ---- Auto-detect state/city from nearest ZIP ----
detect_location <- function(lat, lon, allzips_map) {
  dists <- (allzips_map$latitude - lat)^2 + (allzips_map$longitude - lon)^2
  nearest <- which.min(dists)
  list(
    state   = as.character(allzips_map$state[nearest]),
    city    = as.character(allzips_map$city[nearest]),
    zipcode = as.character(allzips_map$zipcode[nearest])
  )
}

# ---- Look up ZIP code centroid ----
lookup_zip <- function(zipcode, allzips_map) {
  idx <- which(allzips_map$zipcode == zipcode)
  if (length(idx) == 0) return(NULL)
  list(
    lat     = allzips_map$latitude[idx[1]],
    lon     = allzips_map$longitude[idx[1]],
    state   = as.character(allzips_map$state[idx[1]]),
    city    = as.character(allzips_map$city[idx[1]]),
    zipcode = as.character(allzips_map$zipcode[idx[1]])
  )
}

# =============================================================================
# MAIN FORECAST FUNCTION
# =============================================================================
forecast_casino <- function(lat, lon, has_hotel = 1, has_tables = 1,
                            is_cardroom = 0,
                            state = NULL, state_ggr = NULL,
                            label = "Proposed Casino", env) {

  # Unpack data from environment
  markets      <- env$markets
  zips_study   <- env$zips_study
  pop_income   <- env$pop_income
  dist_matrix  <- env$dist_matrix
  p            <- env$model_params

  n_mkts <- nrow(markets)
  n_zips <- nrow(zips_study)

  # --- Add proposed casino as a new market ---
  new_mkt <- data.frame(
    market_id  = max(markets$market_id) + 1,
    n_casinos  = 1L,
    latitude   = lat, longitude = lon,
    observed   = FALSE, revenue = 0,
    has_hotel  = as.integer(has_hotel),
    has_tables = as.integer(has_tables),
    is_cardroom = as.integer(is_cardroom),
    state      = ifelse(is.null(state), "NEW", state),
    label      = label,
    stringsAsFactors = FALSE
  )

  # Match any extra columns added at app startup (model_pred_rev, demand_index, etc.)
  for (col in setdiff(names(markets), names(new_mkt))) {
    new_mkt[[col]] <- NA
  }
  markets_new <- rbind(markets, new_mkt[, names(markets)])
  n_mkts_new  <- nrow(markets_new)

  # --- Compute distance matrix with new market ---
  dist_new <- matrix(NA, nrow = n_zips, ncol = n_mkts_new)
  dist_new[, 1:n_mkts] <- dist_matrix
  dist_new[, n_mkts_new] <- haversine(zips_study$longitude, zips_study$latitude, lon, lat)

  # --- Gravity demand indices (WITH competition) ---
  dw <- decay_power(dist_new, p$cg_beta)
  dw[dist_new > p$MAX_DIST] <- 0
  attract <- exp(p$cg_a_hotel * markets_new$has_hotel + p$cg_a_table * markets_new$has_tables)
  dw_a <- sweep(dw, 2, attract, "*")
  denom <- rowSums(dw_a, na.rm = TRUE)
  denom[denom == 0] <- 1
  probs <- sweep(dw_a, 1, denom, "/")           # P(zip_i -> market_j)
  demand_flow <- sweep(probs, 1, pop_income, "*")
  demand_idx <- colSums(demand_flow, na.rm = TRUE)

  # --- Monopoly attractiveness adjustment ---
  # In competitive markets, hotel/tables differentiate through gravity demand share.
  # In isolated markets (no competitors), share = 100% regardless of amenities.
  # Fix: scale demand from uncontested zips by attractiveness factor.
  attract_new <- exp(p$cg_a_hotel * has_hotel + p$cg_a_table * has_tables)
  if (attract_new != 1) {
    monopoly_mask <- probs[, n_mkts_new] > 0.99  # effectively uncontested zips
    if (any(monopoly_mask)) {
      mono_demand <- sum(demand_flow[monopoly_mask, n_mkts_new])
      demand_idx[n_mkts_new] <- demand_idx[n_mkts_new] + mono_demand * (attract_new - 1)
    }
  }

  # --- Baseline demand (without new casino) ---
  dw_base <- decay_power(dist_matrix, p$cg_beta)
  dw_base[dist_matrix > p$MAX_DIST] <- 0
  attract_base <- exp(p$cg_a_hotel * markets$has_hotel + p$cg_a_table * markets$has_tables)
  dw_a_base <- sweep(dw_base, 2, attract_base, "*")
  denom_base <- rowSums(dw_a_base, na.rm = TRUE)
  denom_base[denom_base == 0] <- 1
  probs_base <- sweep(dw_a_base, 1, denom_base, "/")

  # --- Predict revenue ---
  # Apply Duan smearing correction for retransformation bias: E[Rev] = exp(X'B) * smear
  # For calibrated states the smear cancels in the calibration ratio, but it corrects
  # uncalibrated predictions and ensures model_pred_rev reflects true expected revenue.
  smear <- if (!is.null(p$cg_duan_smear)) p$cg_duan_smear else 1
  cr_delta <- if (!is.null(p$cg_cardroom_delta)) p$cg_cardroom_delta else 0
  pred_ln_rev <- p$cg_intercept + p$cg_gamma * log(demand_idx) + cr_delta * markets_new$is_cardroom
  pred_rev <- exp(pred_ln_rev) * smear
  demand_idx_base_all <- colSums(sweep(probs_base, 1, pop_income, "*"), na.rm = TRUE)
  pred_ln_rev_base <- p$cg_intercept + p$cg_gamma * log(demand_idx_base_all) + cr_delta * markets$is_cardroom
  pred_rev_base <- exp(pred_ln_rev_base) * smear

  new_idx <- n_mkts_new
  new_demand <- demand_idx[new_idx]
  new_pred_rev <- pred_rev[new_idx]

  # Determine target state
  target_state <- ifelse(is.null(state), "NEW", state)
  is_existing_state <- target_state %in% env$state_revenue$state

  # --- Visit probabilities for new casino (for catchment map) ---
  zip_visit_prob <- probs[, new_idx]

  # Primary market for each ZIP (which market gets highest probability)
  zip_primary <- apply(probs, 1, which.max)

  # --- State-level totals and cannibalization ---
  # Check both: state has revenue data AND has observed markets for calibration
  state_mask <- if (is_existing_state) which(markets$state == target_state & markets$observed) else integer(0)
  if (is_existing_state && length(state_mask) > 0) {
    actual_state_total <- env$state_revenue$revenue[env$state_revenue$state == target_state]
    if (!is.null(state_ggr)) actual_state_total <- state_ggr

    # Marginal change for existing markets
    demand_idx_base <- colSums(sweep(probs_base, 1, pop_income, "*"), na.rm = TRUE)
    existing_after_rev <- numeric(length(state_mask))
    for (si in seq_along(state_mask)) {
      idx <- state_mask[si]
      d_before <- demand_idx_base[idx]
      d_after  <- demand_idx[idx]
      change_factor <- (d_after / d_before)^p$cg_gamma
      existing_after_rev[si] <- markets$revenue[idx] * change_factor
    }

    # Calibrated prediction for new casino
    model_state_total <- sum(pred_rev_base[state_mask])
    calibration_factor <- actual_state_total / model_state_total
    if (calibration_factor > 10 || calibration_factor < 0.1) {
      warning(paste0("Extreme calibration factor (", round(calibration_factor, 2),
                     ") for state ", target_state, ". Clamping to [0.1, 10]."))
      calibration_factor <- max(0.1, min(10, calibration_factor))
    }
    new_casino_calibrated <- new_pred_rev * calibration_factor
    predicted_revenue <- new_casino_calibrated
    predicted_share   <- new_casino_calibrated / (sum(existing_after_rev) + new_casino_calibrated)
    new_state_total   <- sum(existing_after_rev) + new_casino_calibrated
    expansion_dollars <- new_state_total - actual_state_total
    expansion_pct     <- (new_state_total / actual_state_total - 1) * 100

    # Cannibalization table
    canib_rows <- list()
    for (si in seq_along(state_mask)) {
      idx <- state_mask[si]
      before_rev <- markets$revenue[idx]
      after_rev  <- existing_after_rev[si]
      before_share <- before_rev / actual_state_total
      after_share  <- after_rev / new_state_total

      canib_rows[[length(canib_rows) + 1]] <- data.frame(
        Market          = markets$label[idx],
        Before_Rev_M    = round(before_rev / 1e6, 1),
        After_Rev_M     = round(after_rev / 1e6, 1),
        Change_M        = round((after_rev - before_rev) / 1e6, 1),
        Before_Share    = round(before_share * 100, 2),
        After_Share     = round(after_share * 100, 2),
        Share_Change_pp = round((after_share - before_share) * 100, 2),
        stringsAsFactors = FALSE
      )
    }
    canib_table <- if (length(canib_rows) > 0) do.call(rbind, canib_rows) else NULL
    if (!is.null(canib_table)) canib_table <- canib_table[order(canib_table$Change_M), ]

  } else {
    predicted_revenue <- new_pred_rev
    predicted_share   <- 1.0
    new_state_total   <- new_pred_rev
    expansion_dollars <- new_pred_rev
    expansion_pct     <- NA
    actual_state_total <- 0
    canib_table <- NULL
  }

  # --- Cluster proximity detection ---
  mkt_dists <- haversine(markets$longitude, markets$latitude, lon, lat)
  nearby_mask <- mkt_dists <= 5  # within 5-mile cluster radius
  in_existing_cluster <- any(nearby_mask)

  nearby_markets <- NULL
  if (in_existing_cluster) {
    nm_idx <- which(nearby_mask)
    nearby_markets <- data.frame(
      label    = markets$label[nm_idx],
      state    = markets$state[nm_idx],
      distance = round(mkt_dists[nm_idx], 1),
      revenue  = markets$revenue[nm_idx],
      observed = markets$observed[nm_idx],
      stringsAsFactors = FALSE
    )
    nearby_markets <- nearby_markets[order(nearby_markets$distance), ]
  }

  # --- Return everything ---
  list(
    label             = label,
    lat               = lat,
    lon               = lon,
    has_hotel         = has_hotel,
    has_tables        = has_tables,
    is_cardroom       = is_cardroom,
    state             = target_state,
    is_existing_state = is_existing_state,
    demand_index      = new_demand,
    predicted_revenue = predicted_revenue,
    predicted_share   = predicted_share,
    new_state_total   = new_state_total,
    old_state_total   = actual_state_total,
    expansion_dollars = expansion_dollars,
    expansion_pct     = expansion_pct,
    cannibalization_table = canib_table,
    in_existing_cluster   = in_existing_cluster,
    nearby_markets        = nearby_markets,
    # Catchment data for map visualization
    zip_visit_prob    = zip_visit_prob,
    zip_primary       = zip_primary,
    markets_new       = markets_new,
    new_market_idx    = new_idx
  )
}
