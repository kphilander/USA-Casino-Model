# =============================================================================
# Casino Revenue Forecasting using Competitive Gravity Revenue Model
# =============================================================================
# This script provides forecast_casino(), which predicts annual revenue for a
# hypothetical new casino using the competitive gravity revenue model:
#
#   D_j = Σ_i [ pop_inc_i × A_j × f(d_ij) / Σ_k A_k × f(d_ik) ]
#   ln(Rev_j) = intercept + gamma × ln(D_j)
#
# where A_j = exp(a_hotel × H_j + a_tables × T_j) and f(d) = (d + 0.1)^(-beta)
#
# The model is accretive: adding a new casino increases the competitive
# denominator (reducing nearby casinos' D_j) while contributing its own D_j.
# The state total naturally grows because the new casino taps previously
# underserved demand.
#
# Depends on: allzips.rds, casinodata.rds, data/*_revenue_2022.csv
# =============================================================================

cat("Loading forecasting data...\n")

# ---- Data paths (relative to research/ directory) ----
allzips    <- readRDS("../allzips.rds")
casinodata <- readRDS("../casinodata.rds")

# Revenue data (9 states)
pa_revenue <- read.csv("data/pa_revenue_2022.csv", stringsAsFactors = FALSE)
oh_revenue <- read.csv("data/oh_revenue_2022.csv", stringsAsFactors = FALSE)
md_revenue <- read.csv("data/md_revenue_2022.csv", stringsAsFactors = FALSE)
ny_revenue <- read.csv("data/ny_revenue_2022.csv", stringsAsFactors = FALSE)
ma_revenue <- read.csv("data/ma_revenue_2022.csv", stringsAsFactors = FALSE)
ct_revenue <- read.csv("data/ct_revenue_2022.csv", stringsAsFactors = FALSE)
in_revenue <- read.csv("data/in_revenue_2022.csv", stringsAsFactors = FALSE)
mo_revenue <- read.csv("data/mo_revenue_2022.csv", stringsAsFactors = FALSE)
ia_revenue <- read.csv("data/ia_revenue_2022.csv", stringsAsFactors = FALSE)

# CT table revenue scaling (31.8% table share from Mohegan Sun FY22 SEC filings).
# NOTE: This Mohegan-derived share is applied to both Mohegan Sun and Foxwoods;
# Foxwoods may have a different table/slot mix but does not publicly report it.
ct_revenue$table_revenue_2022 <- round(ct_revenue$slots_revenue_2022 * 0.318 / (1 - 0.318))
ct_revenue$total_revenue_2022 <- ct_revenue$slots_revenue_2022 + ct_revenue$table_revenue_2022

revenue_data <- rbind(pa_revenue, oh_revenue, md_revenue, ny_revenue,
                      ma_revenue, ct_revenue, in_revenue, mo_revenue, ia_revenue)
revenue_data <- revenue_data[!is.na(revenue_data$casino_id), ]

# ---- Study region setup (mirrors run_analysis.R) ----
primary_states <- c("PA", "OH", "MD", "NY", "MA", "CT", "IN", "MO", "IA")
border_states  <- c("NJ", "DE", "WV", "KY", "MI", "VT", "NH", "RI", "VA", "DC",
                     "WI", "IL", "MN", "SD", "NE", "KS", "AR", "TN", "OK")
all_states <- c(primary_states, border_states)

state_abbrev <- c(
  "Pennsylvania" = "PA", "Ohio" = "OH", "New York" = "NY",
  "New Jersey" = "NJ", "Delaware" = "DE", "Maryland" = "MD",
  "West Virginia" = "WV", "Kentucky" = "KY", "Indiana" = "IN",
  "Michigan" = "MI", "Massachusetts" = "MA", "Connecticut" = "CT",
  "Vermont" = "VT", "New Hampshire" = "NH", "Rhode Island" = "RI",
  "Virginia" = "VA", "District of Columbia" = "DC",
  "Missouri" = "MO", "Iowa" = "IA",
  "Wisconsin" = "WI", "Illinois" = "IL", "Minnesota" = "MN",
  "South Dakota" = "SD", "Nebraska" = "NE", "Kansas" = "KS",
  "Arkansas" = "AR", "Tennessee" = "TN", "Oklahoma" = "OK"
)

zips_study <- allzips[allzips$state.x %in% all_states, ]
zips_study <- data.frame(
  zipcode = zips_study$zipcode, state = zips_study$state.x,
  latitude = zips_study$latitude, longitude = zips_study$longitude,
  adultpop = zips_study$adultpop, income = zips_study$income
)
# Remove ZIPs with missing data (matches build_data.R filtering)
zips_study <- zips_study[!is.na(zips_study$latitude) & !is.na(zips_study$longitude) &
                         !is.na(zips_study$adultpop) & !is.na(zips_study$income), ]
pop_income <- zips_study$adultpop * (zips_study$income / 50000)

# Casino setup
casinos_study <- casinodata[casinodata$state %in% names(state_abbrev), ]
clean_names <- sapply(as.character(casinos_study$name), function(nm) {
  raw <- charToRaw(nm); raw[raw > 0x7e] <- charToRaw("'"); rawToChar(raw)
}, USE.NAMES = FALSE)
casinos_study <- data.frame(
  casino_id = casinos_study$id, name = clean_names,
  state = state_abbrev[casinos_study$state],
  latitude = casinos_study$geocodehere_lat,
  longitude = casinos_study$geocodehere_lon,
  hotel = ifelse(casinos_study$hotel == "Yes", 1, 0),
  stringsAsFactors = FALSE
)
casinos_study <- casinos_study[!is.na(casinos_study$latitude) & !is.na(casinos_study$longitude), ]

# Inject Caesars Southern Indiana
casinos_study <- rbind(casinos_study, data.frame(
  casino_id = 9999, name = "Caesars Southern Indiana",
  state = "IN", latitude = 38.1796, longitude = -85.9054, hotel = 1,
  stringsAsFactors = FALSE
))

# Hotel overrides (verified for 2022)
casinos_study$has_hotel <- casinos_study$hotel
casinos_study$has_hotel[casinos_study$casino_id == 1060] <- 0  # Presque Isle Downs
casinos_study$has_hotel[casinos_study$casino_id == 865]  <- 0  # Hard Rock Northern IN
casinos_study$has_hotel[casinos_study$casino_id == 1167] <- 1  # Rivers Schenectady
casinos_study$has_hotel[casinos_study$casino_id == 83]   <- 1  # Batavia Downs
casinos_study$has_hotel[casinos_study$casino_id == 1162] <- 1  # River City MO
casinos_study$has_hotel[casinos_study$casino_id == 557]  <- 1  # Grand Falls IA
casinos_study$has_hotel[casinos_study$casino_id == 1155] <- 1  # Rhythm City IA
casinos_study$has_hotel[casinos_study$casino_id == 1170] <- 1  # Riverside IA
casinos_study$has_hotel[casinos_study$casino_id == 366]  <- 1  # Diamond Jo Worth IA
casinos_study$has_hotel[casinos_study$casino_id == 1434] <- 1  # Wild Rose Clinton IA
casinos_study$has_hotel[casinos_study$casino_id == 1435] <- 1  # Wild Rose Emmetsburg IA

# has_tables
casinos_study$has_tables <- 1
slots_only_ids <- c()
for (i in 1:nrow(revenue_data)) {
  trev <- revenue_data$table_revenue_2022[i]
  if (!is.na(trev) && trev == 0) slots_only_ids <- c(slots_only_ids, revenue_data$casino_id[i])
}
slots_only_ids <- c(slots_only_ids,
                    oh_revenue$casino_id[oh_revenue$facility_type == "racino"],
                    ny_revenue$casino_id[ny_revenue$facility_type == "VLT"],
                    ma_revenue$casino_id[grepl("Plainridge", ma_revenue$name, ignore.case = TRUE)])
slots_only_ids <- unique(slots_only_ids[!is.na(slots_only_ids)])
casinos_study$has_tables[casinos_study$casino_id %in% slots_only_ids] <- 0

casinos_study$observed <- casinos_study$casino_id %in% revenue_data$casino_id

# ---- Haversine and decay functions ----
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 3959
  lon1_rad <- lon1 * pi / 180; lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180; lat2_rad <- lat2 * pi / 180
  dlon <- lon2_rad - lon1_rad; dlat <- lat2_rad - lat1_rad
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  R * 2 * asin(sqrt(a))
}

decay_power <- function(d, beta) (d + 0.1)^(-beta)

# ---- Build markets at optimal radius (5 mi) ----
n_casinos <- nrow(casinos_study)
casino_dist <- matrix(0, n_casinos, n_casinos)
for (i in 1:(n_casinos - 1)) {
  for (j in (i + 1):n_casinos) {
    d <- haversine(casinos_study$longitude[i], casinos_study$latitude[i],
                   casinos_study$longitude[j], casinos_study$latitude[j])
    casino_dist[i, j] <- d; casino_dist[j, i] <- d
  }
}
hc <- hclust(as.dist(casino_dist), method = "single")
casinos_study$market_id <- cutree(hc, h = 5)  # optimal radius = 5 mi

market_ids <- sort(unique(casinos_study$market_id))
n_mkts <- length(market_ids)

markets <- data.frame(
  market_id = integer(n_mkts), n_casinos = integer(n_mkts),
  latitude = numeric(n_mkts), longitude = numeric(n_mkts),
  observed = logical(n_mkts), revenue = numeric(n_mkts),
  has_hotel = integer(n_mkts), has_tables = integer(n_mkts),
  state = character(n_mkts), label = character(n_mkts),
  stringsAsFactors = FALSE
)

for (k in seq_along(market_ids)) {
  mid <- market_ids[k]
  idx <- which(casinos_study$market_id == mid)
  members_obs <- casinos_study$observed[idx]
  has_rev <- any(members_obs)
  mem_rev <- merge(casinos_study[idx, , drop = FALSE],
                   revenue_data[, c("casino_id", "total_revenue_2022")],
                   by = "casino_id", all.x = TRUE)
  mem_rev$total_revenue_2022[is.na(mem_rev$total_revenue_2022)] <- 0
  tot_rev <- sum(mem_rev$total_revenue_2022)

  if (has_rev && tot_rev > 0) {
    wts <- mem_rev$total_revenue_2022
    lat <- sum(casinos_study$latitude[idx] * wts) / sum(wts)
    lon <- sum(casinos_study$longitude[idx] * wts) / sum(wts)
  } else {
    lat <- mean(casinos_study$latitude[idx]); lon <- mean(casinos_study$longitude[idx])
  }

  lbl <- paste(casinos_study$name[idx], collapse = " + ")
  if (nchar(lbl) > 60) lbl <- paste0(substr(lbl, 1, 57), "...")

  if (has_rev && tot_rev > 0) {
    obs_mem <- mem_rev[mem_rev$total_revenue_2022 > 0, ]
    mkt_st <- obs_mem$state[which.max(obs_mem$total_revenue_2022)]
  } else {
    mkt_st <- casinos_study$state[idx[1]]
  }

  markets$market_id[k] <- mid; markets$n_casinos[k] <- length(idx)
  markets$latitude[k] <- lat; markets$longitude[k] <- lon
  markets$observed[k] <- has_rev; markets$revenue[k] <- tot_rev
  markets$has_hotel[k] <- as.integer(any(casinos_study$has_hotel[idx] == 1, na.rm = TRUE))
  markets$has_tables[k] <- as.integer(any(casinos_study$has_tables[idx] == 1, na.rm = TRUE))
  markets$state[k] <- mkt_st; markets$label[k] <- lbl
}

# ---- Compute baseline distance matrix ----
n_zips <- nrow(zips_study)
dist_matrix <- matrix(NA, nrow = n_zips, ncol = n_mkts)
for (j in 1:n_mkts) {
  dist_matrix[, j] <- haversine(zips_study$longitude, zips_study$latitude,
                                markets$longitude[j], markets$latitude[j])
}

# ---- Competitive Gravity Revenue Model Parameters ----
# From 10-State Model estimation (run_analysis.R) with is_cardroom indicator
# Best model: Power decay
cg_beta    <- 2.6845    # distance decay exponent
cg_a_hotel <- 0.232     # hotel attractiveness coefficient
cg_a_table <- 0.418     # tables attractiveness coefficient
cg_intercept <- 5.459   # OLS intercept
cg_gamma     <- 1.009   # demand elasticity
cg_cardroom_delta <- -1.851  # cardroom revenue discount [exp(-1.85) = 15.7% of full-casino]
cg_duan_smear <- 1.2800 # Duan (1983) retransformation bias correction

MAX_DIST <- 150  # miles cutoff

cat("  Loaded", nrow(zips_study), "ZIPs,", nrow(casinos_study), "casinos,",
    n_mkts, "markets (", sum(markets$observed), "observed)\n")
cat("  CG model: power decay, beta=", cg_beta, ", gamma=", cg_gamma, "\n")
cat("Forecasting engine ready.\n\n")

# =============================================================================
# FORECASTING FUNCTION
# =============================================================================

forecast_casino <- function(lat, lon, has_hotel = 1, has_tables = 1,
                            is_cardroom = 0,
                            state = NULL, state_ggr = NULL,
                            label = "Proposed Casino") {

  # --- Add proposed casino as a new market ---
  new_mkt <- data.frame(
    market_id = max(markets$market_id) + 1,
    n_casinos = 1L,
    latitude = lat, longitude = lon,
    observed = FALSE, revenue = 0,
    has_hotel = as.integer(has_hotel),
    has_tables = as.integer(has_tables),
    is_cardroom = as.integer(is_cardroom),
    state = ifelse(is.null(state), "NEW", state),
    label = label,
    stringsAsFactors = FALSE
  )

  markets_new <- rbind(markets, new_mkt)
  n_mkts_new <- nrow(markets_new)

  # --- Compute distance matrix with new market ---
  dist_new <- matrix(NA, nrow = n_zips, ncol = n_mkts_new)
  dist_new[, 1:n_mkts] <- dist_matrix  # reuse existing
  dist_new[, n_mkts_new] <- haversine(zips_study$longitude, zips_study$latitude, lon, lat)

  # --- Compute gravity demand indices (WITH competition) ---
  # D_j = Σ_i [ pop_inc_i × A_j × f(d_ij) / Σ_k A_k × f(d_ik) ]
  dw <- decay_power(dist_new, cg_beta)
  dw[dist_new > MAX_DIST] <- 0
  attract <- exp(cg_a_hotel * markets_new$has_hotel + cg_a_table * markets_new$has_tables)
  dw_a <- sweep(dw, 2, attract, "*")
  denom <- rowSums(dw_a, na.rm = TRUE)
  denom[denom == 0] <- 1
  probs <- sweep(dw_a, 1, denom, "/")
  demand_flow <- sweep(probs, 1, pop_income, "*")
  demand_idx <- colSums(demand_flow, na.rm = TRUE)

  # --- Also compute baseline demand (without new casino) ---
  dw_base <- decay_power(dist_matrix, cg_beta)
  dw_base[dist_matrix > MAX_DIST] <- 0
  attract_base <- exp(cg_a_hotel * markets$has_hotel + cg_a_table * markets$has_tables)
  dw_a_base <- sweep(dw_base, 2, attract_base, "*")
  denom_base <- rowSums(dw_a_base, na.rm = TRUE)
  denom_base[denom_base == 0] <- 1
  probs_base <- sweep(dw_a_base, 1, denom_base, "/")
  demand_flow_base <- sweep(probs_base, 1, pop_income, "*")
  demand_idx_base <- colSums(demand_flow_base, na.rm = TRUE)

  # --- Predict revenue for all markets ---
  # ln(Rev_j) = intercept + gamma × ln(D_j) + delta × is_cardroom
  # Apply Duan smearing correction for retransformation bias
  pred_ln_rev <- cg_intercept + cg_gamma * log(demand_idx) + cg_cardroom_delta * markets_new$is_cardroom
  pred_rev <- exp(pred_ln_rev) * cg_duan_smear

  pred_ln_rev_base <- cg_intercept + cg_gamma * log(demand_idx_base) + cg_cardroom_delta * markets$is_cardroom
  pred_rev_base <- exp(pred_ln_rev_base) * cg_duan_smear

  # New casino's predicted revenue and demand index
  new_idx <- n_mkts_new
  new_demand <- demand_idx[new_idx]
  new_pred_rev <- pred_rev[new_idx]

  # Determine target state
  target_state <- ifelse(is.null(state), "NEW", state)
  is_existing_state <- target_state %in% markets$state[markets$observed]

  # --- Compute state-level totals ---
  if (is_existing_state) {
    # Existing state: use MARGINAL approach
    # For existing markets: apply proportional change from demand shift
    #   after_rev_j = actual_rev_j × (D_j_after / D_j_before)^gamma
    # For new casino: calibrate raw prediction to state's observed level
    state_mask <- which(markets$state == target_state & markets$observed)

    actual_state_total <- sum(markets$revenue[state_mask])
    if (!is.null(state_ggr)) actual_state_total <- state_ggr

    # Marginal change for existing markets
    existing_after_rev <- numeric(length(state_mask))
    for (si in seq_along(state_mask)) {
      idx <- state_mask[si]
      d_before <- demand_idx_base[idx]
      d_after  <- demand_idx[idx]  # same index in the augmented vector
      change_factor <- (d_after / d_before)^cg_gamma
      existing_after_rev[si] <- markets$revenue[idx] * change_factor
    }

    # Calibrated prediction for new casino
    # Calibration: ratio of actual state total to model-predicted state total
    model_state_total <- sum(pred_rev_base[state_mask])
    calibration_factor <- actual_state_total / model_state_total
    if (calibration_factor > 10 || calibration_factor < 0.1) {
      warning(paste0("Extreme calibration factor (", round(calibration_factor, 2),
                     ") for state ", target_state, ". Clamping to [0.1, 10]."))
      calibration_factor <- max(0.1, min(10, calibration_factor))
    }
    new_casino_raw <- new_pred_rev
    new_casino_calibrated <- new_casino_raw * calibration_factor

    # State totals
    new_state_total <- sum(existing_after_rev) + new_casino_calibrated
    predicted_revenue <- new_casino_calibrated
    predicted_share   <- new_casino_calibrated / new_state_total

    expansion_dollars <- new_state_total - actual_state_total
    expansion_pct <- (new_state_total / actual_state_total - 1) * 100

    # --- Build cannibalization table ---
    canib_rows <- list()
    for (si in seq_along(state_mask)) {
      idx <- state_mask[si]
      before_rev <- markets$revenue[idx]
      after_rev  <- existing_after_rev[si]
      before_share <- before_rev / actual_state_total
      after_share  <- after_rev / new_state_total

      canib_rows[[length(canib_rows) + 1]] <- data.frame(
        Market = markets$label[idx],
        Before_Rev_M = round(before_rev / 1e6, 1),
        After_Rev_M = round(after_rev / 1e6, 1),
        Change_M = round((after_rev - before_rev) / 1e6, 1),
        Before_Share = round(before_share * 100, 2),
        After_Share = round(after_share * 100, 2),
        Share_Change_pp = round((after_share - before_share) * 100, 2),
        stringsAsFactors = FALSE
      )
    }

    canib_table <- do.call(rbind, canib_rows)
    canib_table <- canib_table[order(canib_table$Change_M), ]

  } else {
    # New state: no calibration, use raw model predictions
    predicted_revenue <- new_pred_rev
    predicted_share   <- 1.0
    new_state_total   <- new_pred_rev
    expansion_dollars <- new_pred_rev
    expansion_pct     <- NA
    actual_state_total <- 0
    canib_table <- NULL
  }

  # --- Print summary ---
  cat("=================================================================\n")
  cat("  REVENUE FORECAST:", label, "\n")
  cat("=================================================================\n")
  cat(sprintf("  Location: (%.4f, %.4f)\n", lat, lon))
  cat(sprintf("  Hotel: %s  |  Tables: %s\n",
              ifelse(has_hotel, "Yes", "No"), ifelse(has_tables, "Yes", "No")))
  cat(sprintf("  State: %s\n\n", target_state))

  cat(sprintf("  Demand index (D_j):     %s\n", format(round(new_demand), big.mark = ",")))
  cat(sprintf("  Predicted revenue:      $%s\n", format(round(predicted_revenue), big.mark = ",")))

  if (is_existing_state) {
    cat(sprintf("  Within-state share:     %.2f%%\n", predicted_share * 100))
    cat(sprintf("\n  State total (before):   $%s\n", format(round(actual_state_total), big.mark = ",")))
    cat(sprintf("  State total (after):    $%s\n", format(round(new_state_total), big.mark = ",")))
    cat(sprintf("  Market expansion:       $%s (+%.1f%%)\n",
                format(round(expansion_dollars), big.mark = ","), expansion_pct))

    if (!is.null(canib_table) && nrow(canib_table) > 0) {
      cat("\n  --- Cannibalization Impact ---\n")
      cat(sprintf("  %-45s  %10s  %10s  %9s  %8s\n",
                  "Market", "Before($M)", "After($M)", "Change$M", "Chg(pp)"))
      cat(sprintf("  %-45s  %10s  %10s  %9s  %8s\n",
                  strrep("-", 45), strrep("-", 10), strrep("-", 10),
                  strrep("-", 9), strrep("-", 8)))
      for (r in 1:nrow(canib_table)) {
        nm <- canib_table$Market[r]
        if (nchar(nm) > 45) nm <- paste0(substr(nm, 1, 42), "...")
        cat(sprintf("  %-45s  %10.1f  %10.1f  %+9.1f  %+8.2f\n",
                    nm, canib_table$Before_Rev_M[r], canib_table$After_Rev_M[r],
                    canib_table$Change_M[r], canib_table$Share_Change_pp[r]))
      }
      cat(sprintf("\n  %-45s  %10.1f  %10.1f  %+9.1f\n",
                  label, 0, round(predicted_revenue / 1e6, 1),
                  round(predicted_revenue / 1e6, 1)))
    }
  }

  cat("=================================================================\n\n")

  # --- Return results ---
  invisible(list(
    label = label,
    lat = lat, lon = lon,
    has_hotel = has_hotel, has_tables = has_tables,
    state = target_state,
    demand_index = new_demand,
    predicted_revenue = predicted_revenue,
    predicted_share = predicted_share,
    new_state_total = new_state_total,
    old_state_total = actual_state_total,
    expansion_dollars = expansion_dollars,
    expansion_pct = expansion_pct,
    cannibalization_table = canib_table
  ))
}
