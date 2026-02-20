# =============================================================================
# Distance Decay Estimation: Market-Level Analysis with Supply-Side Controls
# =============================================================================
# This script estimates casino demand distance decay functions using
# revenue data from PA, OH, MD, NY, MA, CT, IN, MO, and IA with
# ZIP-level demographics.
#
# Key methodological choices:
#   1. Nearby casinos are clustered into "markets" so that co-located
#      properties (e.g. two Philadelphia casinos) compete as a unit.
#   2. Supply-side attractiveness controls: has_hotel and has_tables
#      enter the gravity model as A_j = exp(alpha_hotel * hotel_j +
#      alpha_tables * tables_j).
#   3. Three distance decay functional forms: exponential, power, gaussian.
#   4. Three-parameter optimisation (beta, alpha_hotel, alpha_tables)
#      via optim() with L-BFGS-B.
#   5. Estimation objective: WITHIN-STATE share errors. The model predicts
#      each venue's share of its own state's total GGR. This allows the
#      model to be applied to states that only release statewide GGR
#      totals, to estimate venue-level revenue.
#
# Uses only base R - no additional packages required.
#
# Author: Kahlil Philander
# Date: February 2026
# =============================================================================

cat("=======================================================================\n")
cat("DISTANCE DECAY ESTIMATION ANALYSIS (MARKET-LEVEL)\n")
cat("  9-State Model with Supply-Side Attractiveness Controls\n")
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

# Load revenue data -- 9 states
pa_revenue <- read.csv("data/pa_revenue_2022.csv")
oh_revenue <- read.csv("data/oh_revenue_2022.csv")
md_revenue <- read.csv("data/md_revenue_2022.csv")
ny_revenue <- read.csv("data/ny_revenue_2022.csv")
ma_revenue <- read.csv("data/ma_revenue_2022.csv")
ct_revenue <- read.csv("data/ct_revenue_2022.csv")
in_revenue <- read.csv("data/in_revenue_2022.csv")
mo_revenue <- read.csv("data/mo_revenue_2022.csv")
ia_revenue <- read.csv("data/ia_revenue_2022.csv")

cat("  Loaded", nrow(pa_revenue), "PA properties with revenue\n")
cat("  Loaded", nrow(oh_revenue), "OH properties with revenue\n")
cat("  Loaded", nrow(md_revenue), "MD properties with revenue\n")
cat("  Loaded", nrow(ny_revenue), "NY properties with revenue\n")
cat("  Loaded", nrow(ma_revenue), "MA properties with revenue\n")
cat("  Loaded", nrow(ct_revenue), "CT properties with revenue\n")
cat("  Loaded", nrow(in_revenue), "IN properties with revenue\n")
cat("  Loaded", nrow(mo_revenue), "MO properties with revenue\n")
cat("  Loaded", nrow(ia_revenue), "IA properties with revenue\n")

# --- CT Table Revenue Scaling ---
# CT only reports slot win. We scale using Mohegan Sun's table revenue share
# from Mohegan Tribal Gaming Authority's FY2022 earnings decks and SEC filings.
# Mohegan Sun FY2022 (Oct 2021 - Sep 2022) quarterly operating metrics:
#   Q2 FY22 (Jan-Mar 2022): Slot $101,043K, Table $49,525K  (from Q2 FY22 deck p6)
#   Q3 FY22 (Apr-Jun 2022): Slot $104,065K, Table $52,886K  (from Q3 FY22 deck p6)
#   Q4 FY22 (Jul-Sep 2022): Slot $106,738K, Table $43,313K  (from Q4 FY22 deck p6)
#   Q1 FY22 (Oct-Dec 2021): Slot ~$114,275K, Table ~$53,400K (estimated from 10-Q
#                            Q1 gaming of $169,341K, less ~1% other, split by Q2-Q4 share)
# Full FY22: Slot ~$426.1M, Table ~$199.1M, Total gaming $631.5M
# Table/(Slot+Table) = 31.8%, cross-validated: FY23=31.0%, FY24=32.2%
# Formula: total_gaming = slot_revenue / (1 - table_share)
# Source: Mohegan Tribal Gaming Authority Supplemental Earnings Decks + 10-K/10-Q (SEC)
ct_table_share <- 0.318  # table revenue as share of (slot + table) revenue
cat("\n  CT table share (Mohegan Sun FY22):", ct_table_share, "\n")
ct_revenue$table_revenue_2022 <- round(ct_revenue$slots_revenue_2022 * ct_table_share / (1 - ct_table_share))
ct_revenue$total_revenue_2022 <- ct_revenue$slots_revenue_2022 + ct_revenue$table_revenue_2022
cat("  Mohegan Sun CT scaled total: $", format(ct_revenue$total_revenue_2022[ct_revenue$casino_id == 905], big.mark = ","), "\n")
cat("  Foxwoods CT scaled total: $", format(ct_revenue$total_revenue_2022[ct_revenue$casino_id == 497], big.mark = ","), "\n")

# Combine revenue data
revenue_data <- rbind(pa_revenue, oh_revenue, md_revenue, ny_revenue,
                      ma_revenue, ct_revenue, in_revenue, mo_revenue, ia_revenue)

# Verify no NA casino_ids remain
n_na <- sum(is.na(revenue_data$casino_id))
if (n_na > 0) {
  cat("\n  WARNING:", n_na, "properties with NA casino_id -- dropping:\n")
  for (i in which(is.na(revenue_data$casino_id))) {
    cat("    -", revenue_data$name[i], "(", revenue_data$city[i], ",", revenue_data$state[i],
        ") $", format(revenue_data$total_revenue_2022[i], big.mark = ","), "\n")
  }
  revenue_data <- revenue_data[!is.na(revenue_data$casino_id), ]
}

cat("\nTotal properties with revenue:", nrow(revenue_data), "\n")
cat("Total observed revenue: $", format(round(sum(revenue_data$total_revenue_2022) / 1e9, 2)), " billion\n")

# =============================================================================
# 2. DEFINE STUDY REGION
# =============================================================================

cat("\n--- DEFINING STUDY REGION ---\n")

# Primary states (9) -- states with observed revenue data
primary_states <- c("PA", "OH", "MD", "NY", "MA", "CT", "IN", "MO", "IA")

# Border states -- neighboring states that contain competing casinos
# but for which we don't have observed revenue
border_states <- c("NJ", "DE", "WV", "KY", "MI",
                    "VT", "NH", "RI", "VA", "DC",
                    "WI", "IL", "MN", "SD", "NE", "KS", "AR", "TN", "OK")
all_states <- c(primary_states, border_states)

# State name to abbreviation mapping (must include all states in study region)
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

# Clean casino names: strip any non-ASCII bytes (e.g. Windows-1252 curly quotes)
clean_names <- sapply(as.character(casinos_study$name), function(nm) {
  raw <- charToRaw(nm)
  raw[raw > 0x7e] <- charToRaw("'")  # replace non-ASCII with plain apostrophe
  rawToChar(raw)
}, USE.NAMES = FALSE)

casinos_study <- data.frame(
  casino_id = casinos_study$id,
  name = clean_names,
  state = state_abbrev[casinos_study$state],
  latitude = casinos_study$geocodehere_lat,
  longitude = casinos_study$geocodehere_lon,
  hotel = ifelse(casinos_study$hotel == "Yes", 1, 0),
  stringsAsFactors = FALSE
)

# Drop casinos with missing coordinates (can't compute distances)
n_before <- nrow(casinos_study)
casinos_study <- casinos_study[!is.na(casinos_study$latitude) & !is.na(casinos_study$longitude), ]
n_dropped <- n_before - nrow(casinos_study)
if (n_dropped > 0) cat("  Dropped", n_dropped, "casinos with missing coordinates\n")

# --- Inject Caesars Southern Indiana ---
# This property (formerly Horseshoe Southern Indiana) is not in casinodata.rds.
# Located at 11999 Casino Center Dr SE, Elizabeth, IN 47117 (Harrison County).
# Has hotel (503 rooms) and table games (~70 tables). Revenue from IGC: $248.2M.
# Using synthetic casino_id = 9999 (matched in in_revenue_2022.csv).
# Has hotel (503 rooms) and table games (~70).
caesars_si <- data.frame(
  casino_id = 9999,
  name = "Caesars Southern Indiana",
  state = "IN",
  latitude = 38.1796,
  longitude = -85.9054,
  hotel = 1,
  stringsAsFactors = FALSE
)
casinos_study <- rbind(casinos_study, caesars_si)
cat("  Injected Caesars Southern Indiana (id=9999) at 38.1796, -85.9054\n")

cat("Study region casinos:", nrow(casinos_study), "\n")

# =============================================================================
# 3. SUPPLY-SIDE CONTROLS: has_hotel AND has_tables
# =============================================================================

cat("\n--- BUILDING SUPPLY-SIDE CONTROLS ---\n")

# has_hotel: already loaded from casinodata
casinos_study$has_hotel <- casinos_study$hotel

# has_tables: Default to 1 for all casinos. Set to 0 for facilities known
# to have NO table games based on revenue data (table_revenue_2022 == 0).
# This includes: Ohio racinos (slots-only VLTs), NY VLT facilities,
# Plainridge Park MA, etc.
casinos_study$has_tables <- 1  # default

# Identify slots-only facilities from revenue data
slots_only_ids <- c()
for (i in 1:nrow(revenue_data)) {
  trev <- revenue_data$table_revenue_2022[i]
  # If table revenue is 0 or NA with a note indicating slots-only
  if (!is.na(trev) && trev == 0) {
    slots_only_ids <- c(slots_only_ids, revenue_data$casino_id[i])
  }
}

# Also check for NA table revenue + known slots-only from facility type
# OH racinos are slots-only VLTs
oh_racino_ids <- oh_revenue$casino_id[oh_revenue$facility_type == "racino"]
slots_only_ids <- c(slots_only_ids, oh_racino_ids)

# NY VLT facilities are slots-only
ny_vlt_ids <- ny_revenue$casino_id[ny_revenue$facility_type == "VLT"]
slots_only_ids <- c(slots_only_ids, ny_vlt_ids)

# Plainridge Park Casino MA is a slots-only facility (racino)
plainridge_id <- ma_revenue$casino_id[grepl("Plainridge", ma_revenue$name, ignore.case = TRUE)]
slots_only_ids <- c(slots_only_ids, plainridge_id)

# De-duplicate
slots_only_ids <- unique(slots_only_ids[!is.na(slots_only_ids)])

# Set has_tables = 0 for those casinos
casinos_study$has_tables[casinos_study$casino_id %in% slots_only_ids] <- 0

n_no_hotel <- sum(casinos_study$has_hotel == 0, na.rm = TRUE)
n_no_tables <- sum(casinos_study$has_tables == 0, na.rm = TRUE)
cat("  Casinos with hotel:", sum(casinos_study$has_hotel == 1, na.rm = TRUE), "\n")
cat("  Casinos without hotel:", n_no_hotel, "\n")
cat("  Casinos with tables:", sum(casinos_study$has_tables == 1, na.rm = TRUE), "\n")
cat("  Casinos without tables (slots-only):", n_no_tables, "\n")

if (length(slots_only_ids) > 0) {
  cat("  Slots-only facility IDs:", paste(slots_only_ids, collapse = ", "), "\n")
}

# =============================================================================
# 4. HAVERSINE DISTANCE FUNCTION
# =============================================================================

haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 3959  # Earth radius in miles
  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180
  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  cc <- 2 * asin(sqrt(a))
  return(R * cc)
}

# =============================================================================
# 5. CASINO-TO-CASINO DISTANCE MATRIX (computed once, reused across radii)
# =============================================================================

n_casinos <- nrow(casinos_study)
casino_dist <- matrix(0, n_casinos, n_casinos)
for (i in 1:(n_casinos - 1)) {
  for (j in (i + 1):n_casinos) {
    d <- haversine(casinos_study$longitude[i], casinos_study$latitude[i],
                   casinos_study$longitude[j], casinos_study$latitude[j])
    casino_dist[i, j] <- d
    casino_dist[j, i] <- d
  }
}
hc <- hclust(as.dist(casino_dist), method = "single")

casinos_study$observed <- casinos_study$casino_id %in% revenue_data$casino_id

n_zips <- nrow(zips_study)
pop_income <- zips_study$adultpop * (zips_study$income / 50000)

# =============================================================================
# 6. DISTANCE DECAY FUNCTIONS
# =============================================================================

decay_exponential <- function(d, beta) exp(-beta * d)
decay_power       <- function(d, beta) (d + 0.1)^(-beta)
decay_gaussian    <- function(d, beta) exp(-beta * d^2)

# =============================================================================
# 7. CORE ESTIMATION FUNCTION (reusable for different revenue subsets)
#    Gravity model: D_ij = Pop_i * (Inc_i/50k) * A_j * f(d_ij) / sum_k[A_k*f(d_ik)]
#    Attractiveness: A_j = exp(a_hotel*H + a_tables*T)
#    3-parameter optim: (beta, alpha_hotel, alpha_tables)
#    OBJECTIVE: Minimise sum of within-state share SSE across all states.
#      For each state s: SSE_s = sum_j [(actual_ws_j - pred_ws_j)^2]
#      where ws_j = venue_j / state_total for actual and predicted.
#
#    Also fits a Log-Linear (LN) model as a two-step alternative:
#      Step 1: compute demand index D_j = sum_i [Pop_i*(Inc_i/50k)*f(d_ij)] for each market
#      Step 2: ln(Rev_j) = alpha + gamma*ln(D_j) + b_hotel*H + b_tables*T + e
#    The LN model uses OLS on log-revenue, which is standard in the trade/gravity literature.
#    LN predictions are converted to within-state shares for comparison.
# =============================================================================

run_estimation <- function(rev_data, model_label, casinos_df, zips_df, pop_inc,
                           radii = c(5, 10), save_prefix = "") {
  # Mark which casinos are observed in this run
  casinos_df$observed <- casinos_df$casino_id %in% rev_data$casino_id

  n_c <- nrow(casinos_df)
  n_z <- nrow(zips_df)

  # Casino-to-casino distance matrix + hierarchical clustering
  cdist <- matrix(0, n_c, n_c)
  for (i in 1:(n_c - 1)) {
    for (j in (i + 1):n_c) {
      d <- haversine(casinos_df$longitude[i], casinos_df$latitude[i],
                     casinos_df$longitude[j], casinos_df$latitude[j])
      cdist[i, j] <- d
      cdist[j, i] <- d
    }
  }
  hc_local <- hclust(as.dist(cdist), method = "single")

  # --- Grid search function for a given radius (gravity share model) ---
  estimate_for_radius <- function(radius, verbose = FALSE) {
    mkt_ids_vec <- cutree(hc_local, h = radius)
    uids <- sort(unique(mkt_ids_vec))
    nm <- length(uids)

    mkt_lat <- mkt_lon <- mkt_rev <- numeric(nm)
    mkt_obs <- logical(nm)
    mkt_nc  <- integer(nm)
    mkt_hotel <- mkt_tables <- integer(nm)
    mkt_state <- character(nm)  # state assignment for within-state shares

    for (k in seq_along(uids)) {
      idx <- which(mkt_ids_vec == uids[k])
      members_obs <- casinos_df$observed[idx]
      has_rev <- any(members_obs)

      mem_rev <- merge(
        casinos_df[idx, , drop = FALSE],
        rev_data[, c("casino_id", "total_revenue_2022")],
        by = "casino_id", all.x = TRUE
      )
      mem_rev$total_revenue_2022[is.na(mem_rev$total_revenue_2022)] <- 0
      tot_rev <- sum(mem_rev$total_revenue_2022)

      if (has_rev && tot_rev > 0) {
        wts <- mem_rev$total_revenue_2022
        mkt_lat[k] <- sum(casinos_df$latitude[idx] * wts) / sum(wts)
        mkt_lon[k] <- sum(casinos_df$longitude[idx] * wts) / sum(wts)
      } else {
        mkt_lat[k] <- mean(casinos_df$latitude[idx])
        mkt_lon[k] <- mean(casinos_df$longitude[idx])
      }

      mkt_rev[k] <- tot_rev
      mkt_obs[k] <- has_rev
      mkt_nc[k]  <- length(idx)
      mkt_hotel[k]  <- as.integer(any(casinos_df$has_hotel[idx] == 1, na.rm = TRUE))
      mkt_tables[k] <- as.integer(any(casinos_df$has_tables[idx] == 1, na.rm = TRUE))

      # Assign state: use the state of the highest-revenue observed casino
      if (has_rev && tot_rev > 0) {
        obs_mem <- mem_rev[mem_rev$total_revenue_2022 > 0, ]
        mkt_state[k] <- obs_mem$state[which.max(obs_mem$total_revenue_2022)]
      } else {
        mkt_state[k] <- casinos_df$state[idx[1]]
      }
    }

    dist_mat <- matrix(NA_real_, nrow = n_z, ncol = nm)
    for (j in 1:nm) {
      dist_mat[, j] <- haversine(
        zips_df$longitude, zips_df$latitude,
        mkt_lon[j], mkt_lat[j]
      )
    }

    obs_rev <- mkt_rev[mkt_obs]
    obs_idx <- which(mkt_obs)
    obs_state <- mkt_state[mkt_obs]

    # Pre-compute within-state actual shares
    obs_ws_shares <- numeric(length(obs_rev))
    for (s in unique(obs_state)) {
      s_idx <- which(obs_state == s)
      obs_ws_shares[s_idx] <- obs_rev[s_idx] / sum(obs_rev[s_idx])
    }

    # params = c(beta, a_hotel, a_tables)
    predict_fn <- function(params, decay_type, max_dist = 150) {
      beta <- params[1]; a_hotel <- params[2]; a_tables <- params[3]
      if (decay_type == "exponential") { dw <- decay_exponential(dist_mat, beta)
      } else if (decay_type == "power") { dw <- decay_power(dist_mat, beta)
      } else { dw <- decay_gaussian(dist_mat, beta) }
      dw[dist_mat > max_dist] <- 0
      attract <- exp(a_hotel * mkt_hotel + a_tables * mkt_tables)
      dw_a <- sweep(dw, 2, attract, "*")
      denom <- rowSums(dw_a, na.rm = TRUE); denom[denom == 0] <- 1
      ds <- sweep(dw_a, 1, denom, "/")
      df <- sweep(ds, 1, pop_inc, "*")
      pd <- colSums(df, na.rm = TRUE)
      return(pd[obs_idx])
    }

    # SSE on within-state shares: sum over states of within-state SSE
    sse_fn <- function(params, decay_type) {
      if (params[1] <= 0) return(1e10)
      pred <- predict_fn(params, decay_type)
      # Compute within-state predicted shares
      pred_ws <- numeric(length(pred))
      for (s in unique(obs_state)) {
        s_idx <- which(obs_state == s)
        pred_ws[s_idx] <- pred[s_idx] / sum(pred[s_idx])
      }
      sum((obs_ws_shares - pred_ws)^2, na.rm = TRUE)
    }

    fit_model <- function(decay_type) {
      if (decay_type == "exponential") {
        lower <- c(0.001, -2, -2); upper <- c(2, 5, 5); start <- c(0.05, 0.1, 0.1)
      } else if (decay_type == "power") {
        lower <- c(0.1, -2, -2); upper <- c(20, 5, 5); start <- c(2.0, 0.1, 0.1)
      } else {
        lower <- c(1e-5, -2, -2); upper <- c(0.5, 5, 5); start <- c(0.001, 0.1, 0.1)
      }
      result <- tryCatch({
        optim(start, fn = function(p) sse_fn(p, decay_type),
              method = "L-BFGS-B", lower = lower, upper = upper,
              control = list(factr = 1e2, maxit = 1000))
      }, error = function(e) {
        tryCatch({
          optim(c(start[1] * 2, 0, 0), fn = function(p) sse_fn(p, decay_type),
                method = "L-BFGS-B", lower = lower, upper = upper,
                control = list(factr = 1e2, maxit = 1000))
        }, error = function(e2) { list(par = start, value = 1e10, convergence = 1) })
      })
      result
    }

    fit_exp   <- fit_model("exponential")
    fit_pow   <- fit_model("power")
    fit_gauss <- fit_model("gaussian")
    best_sse <- min(fit_exp$value, fit_pow$value, fit_gauss$value)

    if (verbose) {
      cat(sprintf("  Radius %3d mi | %2d mkts (%2d obs) | Exp SSE=%.6f | Pow SSE=%.6f | Gau SSE=%.6f | Best=%.6f\n",
                  radius, nm, sum(mkt_obs), fit_exp$value, fit_pow$value, fit_gauss$value, best_sse))
    }

    list(radius = radius, n_markets = nm, n_observed = sum(mkt_obs),
         exponential = list(par = fit_exp$par, sse = fit_exp$value, conv = fit_exp$convergence),
         power       = list(par = fit_pow$par, sse = fit_pow$value, conv = fit_pow$convergence),
         gaussian    = list(par = fit_gauss$par, sse = fit_gauss$value, conv = fit_gauss$convergence),
         best_sse    = best_sse)
  }

  # --- Run grid search ---
  cat("\n")
  cat("=======================================================================\n")
  cat("MODEL:", model_label, "\n")
  cat("  Properties:", nrow(rev_data), "| Revenue: $",
      format(round(sum(rev_data$total_revenue_2022) / 1e9, 2)), "B\n")
  cat("=======================================================================\n\n")

  grid_results <- vector("list", length(radii))
  for (g in seq_along(radii)) {
    grid_results[[g]] <- estimate_for_radius(radii[g], verbose = TRUE)
  }

  grid_df <- data.frame(
    Radius_mi = sapply(grid_results, `[[`, "radius"),
    Markets   = sapply(grid_results, `[[`, "n_markets"),
    Obs_Mkts  = sapply(grid_results, `[[`, "n_observed"),
    Exp_SSE   = sapply(grid_results, function(x) x$exponential$sse),
    Pow_SSE   = sapply(grid_results, function(x) x$power$sse),
    Gau_SSE   = sapply(grid_results, function(x) x$gaussian$sse),
    Best_SSE  = sapply(grid_results, `[[`, "best_sse"),
    stringsAsFactors = FALSE
  )

  best_row <- which.min(grid_df$Best_SSE)
  opt_radius <- grid_df$Radius_mi[best_row]
  cat(sprintf("\n  >> Best radius: %d mi (SSE = %.6f)\n\n", opt_radius, grid_df$Best_SSE[best_row]))

  # --- Rebuild markets at optimal radius ---
  casinos_df$market_id <- cutree(hc_local, h = opt_radius)

  market_ids <- sort(unique(casinos_df$market_id))
  n_mkts <- length(market_ids)

  markets <- data.frame(
    market_id = integer(n_mkts), n_casinos = integer(n_mkts),
    latitude = numeric(n_mkts), longitude = numeric(n_mkts),
    observed = logical(n_mkts), revenue = numeric(n_mkts),
    has_hotel = integer(n_mkts), has_tables = integer(n_mkts),
    state = character(n_mkts),
    label = character(n_mkts), stringsAsFactors = FALSE
  )

  for (k in seq_along(market_ids)) {
    mid <- market_ids[k]
    members <- casinos_df[casinos_df$market_id == mid, ]
    has_revenue <- any(members$observed)
    member_rev <- merge(members, rev_data, by = "casino_id", all.x = TRUE)
    member_rev$total_revenue_2022[is.na(member_rev$total_revenue_2022)] <- 0
    total_rev <- sum(member_rev$total_revenue_2022)

    if (has_revenue && total_rev > 0) {
      wts <- member_rev$total_revenue_2022
      lat <- sum(members$latitude * wts) / sum(wts)
      lon <- sum(members$longitude * wts) / sum(wts)
    } else {
      lat <- mean(members$latitude); lon <- mean(members$longitude)
    }

    lbl <- paste(members$name, collapse = " + ")
    if (nchar(lbl) > 60) lbl <- paste0(substr(lbl, 1, 57), "...")

    # Assign state: use state of highest-revenue observed casino
    if (has_revenue && total_rev > 0) {
      obs_mem <- member_rev[member_rev$total_revenue_2022 > 0, ]
      mkt_st <- obs_mem$state.x[which.max(obs_mem$total_revenue_2022)]
    } else {
      mkt_st <- members$state[1]
    }

    markets$market_id[k] <- mid; markets$n_casinos[k] <- nrow(members)
    markets$latitude[k] <- lat; markets$longitude[k] <- lon
    markets$observed[k] <- has_revenue; markets$revenue[k] <- total_rev
    markets$has_hotel[k]  <- as.integer(any(members$has_hotel == 1, na.rm = TRUE))
    markets$has_tables[k] <- as.integer(any(members$has_tables == 1, na.rm = TRUE))
    markets$state[k] <- mkt_st
    markets$label[k] <- lbl
  }

  n_obs_mkts <- sum(markets$observed)
  cat("  Markets:", n_mkts, "(", n_obs_mkts, "observed,", n_mkts - n_obs_mkts, "border)\n")

  # Show multi-casino markets
  multi <- markets[markets$n_casinos > 1 & markets$observed, ]
  if (nrow(multi) > 0) {
    cat("\n  Multi-casino observed markets:\n")
    for (r in 1:nrow(multi)) {
      members <- casinos_df[casinos_df$market_id == multi$market_id[r], ]
      cat(sprintf("    Market %d (%d casinos):", multi$market_id[r], nrow(members)))
      cat(paste(members$name, collapse = ", "), "\n")
    }
  }

  # --- Final estimation at optimal radius ---
  dist_matrix <- matrix(NA, nrow = n_z, ncol = n_mkts)
  for (j in 1:n_mkts) {
    dist_matrix[, j] <- haversine(
      zips_df$longitude, zips_df$latitude,
      markets$longitude[j], markets$latitude[j]
    )
  }

  # =====================================================
  # GRAVITY SHARE MODEL (non-linear, 3 parameters)
  # =====================================================
  # params = c(beta, a_hotel, a_tables)
  predict_mkt <- function(params, decay_type = "exponential", max_dist = 150) {
    beta <- params[1]; a_hotel <- params[2]; a_tables <- params[3]
    if (decay_type == "exponential") { dw <- decay_exponential(dist_matrix, beta)
    } else if (decay_type == "power") { dw <- decay_power(dist_matrix, beta)
    } else { dw <- decay_gaussian(dist_matrix, beta) }
    dw[dist_matrix > max_dist] <- 0
    attract <- exp(a_hotel * markets$has_hotel + a_tables * markets$has_tables)
    dw <- sweep(dw, 2, attract, "*")
    denom <- rowSums(dw, na.rm = TRUE); denom[denom == 0] <- 1
    ds <- sweep(dw, 1, denom, "/")
    df <- sweep(ds, 1, pop_inc, "*")
    pd <- colSums(df, na.rm = TRUE)
    obs_idx <- which(markets$observed)
    return(pd[obs_idx])
  }

  obs_mkt_rev    <- markets$revenue[markets$observed]
  obs_mkt_state  <- markets$state[markets$observed]

  # Pre-compute within-state actual shares
  obs_ws_shares <- numeric(length(obs_mkt_rev))
  for (s in unique(obs_mkt_state)) {
    s_idx <- which(obs_mkt_state == s)
    obs_ws_shares[s_idx] <- obs_mkt_rev[s_idx] / sum(obs_mkt_rev[s_idx])
  }

  sse_fn2 <- function(params, decay_type) {
    if (params[1] <= 0) return(1e10)
    pred <- predict_mkt(params, decay_type)
    # Within-state predicted shares
    pred_ws <- numeric(length(pred))
    for (s in unique(obs_mkt_state)) {
      s_idx <- which(obs_mkt_state == s)
      pred_ws[s_idx] <- pred[s_idx] / sum(pred[s_idx])
    }
    sum((obs_ws_shares - pred_ws)^2, na.rm = TRUE)
  }

  cat("\n  --- Gravity Share Models (3 params: beta, hotel, tables) ---\n")
  results <- list()
  for (dt in c("exponential", "power", "gaussian")) {
    if (dt == "exponential") {
      lower <- c(0.001, -2, -2); upper <- c(2, 5, 5); start <- c(0.05, 0.1, 0.1)
    } else if (dt == "power") {
      lower <- c(0.1, -2, -2); upper <- c(20, 5, 5); start <- c(2.0, 0.1, 0.1)
    } else {
      lower <- c(1e-5, -2, -2); upper <- c(0.5, 5, 5); start <- c(0.001, 0.1, 0.1)
    }
    fit <- tryCatch({
      optim(start, fn = function(p) sse_fn2(p, dt),
            method = "L-BFGS-B", lower = lower, upper = upper,
            control = list(factr = 1e2, maxit = 1000))
    }, error = function(e) list(par = start, value = 1e10, convergence = 1))
    results[[dt]] <- list(par = fit$par, sse = fit$value, conv = fit$convergence)
    cat(sprintf("  %-12s: beta=%.4f, a_H=%.3f, a_T=%.3f, SSE=%.8f\n",
                dt, fit$par[1], fit$par[2], fit$par[3], fit$value))
  }

  tss <- sum((obs_ws_shares - mean(obs_ws_shares))^2)
  comparison <- data.frame(
    Model = c("Exponential", "Power", "Gaussian"),
    Beta = c(results$exponential$par[1], results$power$par[1], results$gaussian$par[1]),
    Alpha_Hotel = c(results$exponential$par[2], results$power$par[2], results$gaussian$par[2]),
    Alpha_Tables = c(results$exponential$par[3], results$power$par[3], results$gaussian$par[3]),
    SSE = c(results$exponential$sse, results$power$sse, results$gaussian$sse),
    R_squared = 1 - c(results$exponential$sse, results$power$sse, results$gaussian$sse) / tss,
    stringsAsFactors = FALSE
  )
  comparison <- comparison[order(comparison$SSE), ]

  # =====================================================
  # LOG-LINEAR (LN) MODEL
  # =====================================================
  # Two-step: first compute demand index, then OLS on log-revenue
  # Step 1: D_j = sum_i [Pop_i * (Inc_i/50k) * f(d_ij)]  (no attractiveness -- that goes into step 2)
  # Step 2: ln(Rev_j) = alpha + gamma * ln(D_j) + b_hotel*H + b_tables*T + e
  # We search over beta to maximise R2 of the log-linear regression.

  cat("\n  --- Log-Linear (LN) Models ---\n")

  obs_idx_ln <- which(markets$observed)
  obs_rev_ln <- markets$revenue[obs_idx_ln]
  obs_hotel_ln  <- markets$has_hotel[obs_idx_ln]
  obs_tables_ln <- markets$has_tables[obs_idx_ln]

  ln_results <- list()

  for (dt in c("exponential", "power", "gaussian")) {
    # Objective: find beta that maximises R2 of the OLS log-linear model
    ln_obj <- function(beta) {
      if (beta <= 0) return(1e10)
      if (dt == "exponential") { dw <- decay_exponential(dist_matrix, beta)
      } else if (dt == "power") { dw <- decay_power(dist_matrix, beta)
      } else { dw <- decay_gaussian(dist_matrix, beta) }
      dw[dist_matrix > 150] <- 0
      # Demand index (no attractiveness in this step)
      demand_flow <- sweep(dw, 1, pop_inc, "*")
      demand_idx <- colSums(demand_flow, na.rm = TRUE)
      di_obs <- demand_idx[obs_idx_ln]
      if (any(di_obs <= 0)) return(1e10)

      y <- log(obs_rev_ln)
      X <- cbind(1, log(di_obs), obs_hotel_ln, obs_tables_ln)
      # OLS: beta_hat = (X'X)^-1 X'y
      fit <- tryCatch({
        XtX_inv <- solve(crossprod(X))
        b_hat <- XtX_inv %*% crossprod(X, y)
        y_hat <- X %*% b_hat
        ss_res <- sum((y - y_hat)^2)
        ss_tot <- sum((y - mean(y))^2)
        list(r2 = 1 - ss_res / ss_tot, coefs = as.numeric(b_hat), ss_res = ss_res)
      }, error = function(e) list(r2 = -Inf, coefs = rep(NA, 4), ss_res = Inf))

      return(-fit$r2)  # minimise negative R2
    }

    if (dt == "exponential") {
      search_range <- c(0.001, 1)
    } else if (dt == "power") {
      search_range <- c(0.1, 10)
    } else {
      search_range <- c(1e-6, 0.1)
    }

    opt <- optimize(ln_obj, interval = search_range, tol = 1e-6)
    beta_ln <- opt$minimum

    # Get final OLS coefficients at optimal beta
    if (dt == "exponential") { dw <- decay_exponential(dist_matrix, beta_ln)
    } else if (dt == "power") { dw <- decay_power(dist_matrix, beta_ln)
    } else { dw <- decay_gaussian(dist_matrix, beta_ln) }
    dw[dist_matrix > 150] <- 0
    demand_flow <- sweep(dw, 1, pop_inc, "*")
    demand_idx <- colSums(demand_flow, na.rm = TRUE)
    di_obs <- demand_idx[obs_idx_ln]

    y <- log(obs_rev_ln)
    X <- cbind(1, log(di_obs), obs_hotel_ln, obs_tables_ln)
    XtX_inv <- solve(crossprod(X))
    b_hat <- as.numeric(XtX_inv %*% crossprod(X, y))
    y_hat <- X %*% b_hat
    ss_res <- sum((y - y_hat)^2)
    ss_tot <- sum((y - mean(y))^2)
    r2 <- 1 - ss_res / ss_tot
    n_obs <- length(y); k_params <- 4
    adj_r2 <- 1 - (1 - r2) * (n_obs - 1) / (n_obs - k_params)

    # Standard errors
    sigma2 <- ss_res / (n_obs - k_params)
    se_hat <- sqrt(diag(XtX_inv) * sigma2)
    t_vals <- b_hat / se_hat

    ln_results[[dt]] <- list(
      beta_decay = beta_ln,
      coefs = b_hat,  # c(intercept, gamma, b_hotel, b_tables)
      se = se_hat,
      t_vals = t_vals,
      r2 = r2,
      adj_r2 = adj_r2,
      n = n_obs,
      demand_idx = di_obs,
      y_hat = as.numeric(y_hat)
    )

    cat(sprintf("  LN-%-11s: beta=%.4f, gamma=%.3f, hotel=%.3f, tables=%.3f, R2=%.4f (adj=%.4f)\n",
                dt, beta_ln, b_hat[2], b_hat[3], b_hat[4], r2, adj_r2))
  }

  # Best LN model
  ln_r2s <- sapply(ln_results, function(x) x$r2)
  best_ln_type <- names(which.max(ln_r2s))
  best_ln <- ln_results[[best_ln_type]]

  cat(sprintf("\n  Best LN model: %s (beta=%.4f, R2=%.4f, adj-R2=%.4f)\n",
              best_ln_type, best_ln$beta_decay, best_ln$r2, best_ln$adj_r2))
  cat("    Coefficients (t-values):\n")
  cat(sprintf("      Intercept: %7.3f  (%.2f)\n", best_ln$coefs[1], best_ln$t_vals[1]))
  cat(sprintf("      ln(D_j):   %7.3f  (%.2f)  [demand elasticity]\n", best_ln$coefs[2], best_ln$t_vals[2]))
  cat(sprintf("      Hotel:     %7.3f  (%.2f)\n", best_ln$coefs[3], best_ln$t_vals[3]))
  cat(sprintf("      Tables:    %7.3f  (%.2f)\n", best_ln$coefs[4], best_ln$t_vals[4]))

  # =====================================================
  # SELECT BEST OVERALL (gravity share vs LN)
  # =====================================================

  # For comparable fit, use the best gravity model
  best_gravity_model <- comparison$Model[1]
  best_gravity_params <- c(comparison$Beta[1], comparison$Alpha_Hotel[1],
                            comparison$Alpha_Tables[1])
  predicted <- predict_mkt(best_gravity_params, tolower(best_gravity_model))

  # Within-state predicted shares for gravity model
  pred_shares <- numeric(length(predicted))
  for (s in unique(obs_mkt_state)) {
    s_idx <- which(obs_mkt_state == s)
    pred_shares[s_idx] <- predicted[s_idx] / sum(predicted[s_idx])
  }

  cat(sprintf("\n  Best gravity share model: %s (beta=%.4f, a_H=%.3f, a_T=%.3f, R2=%.4f)\n",
              best_gravity_model, best_gravity_params[1], best_gravity_params[2],
              best_gravity_params[3], comparison$R_squared[1]))
  cat("  (R2 computed on within-state shares)\n")

  # LN model predictions (in levels, for property-level comparison)
  ln_pred_rev <- exp(best_ln$y_hat)  # predicted revenue in dollars
  # Within-state predicted shares for LN model
  ln_pred_shares <- numeric(length(ln_pred_rev))
  for (s in unique(obs_mkt_state)) {
    s_idx <- which(obs_mkt_state == s)
    ln_pred_shares[s_idx] <- ln_pred_rev[s_idx] / sum(ln_pred_rev[s_idx])
  }

  # --- Build property-level results (using best gravity model) ---
  # All shares are WITHIN-STATE: each property's share of its own state's total GGR
  obs_mkts <- markets[markets$observed, ]

  # Compute state totals for within-state shares
  state_totals <- tapply(rev_data$total_revenue_2022, rev_data$state, sum)

  property_rows <- list()
  for (k in seq_len(nrow(obs_mkts))) {
    mid <- obs_mkts$market_id[k]
    mkt_ws_pred_share <- pred_shares[k]     # already within-state
    mkt_ws_actual_share <- obs_ws_shares[k]  # already within-state
    mkt_actual_rev <- obs_mkts$revenue[k]
    mkt_st <- obs_mkts$state[k]

    members <- casinos_df[casinos_df$market_id == mid & casinos_df$observed, ]
    mem_rev <- merge(members, rev_data[, c("casino_id", "name", "city", "state",
                                            "total_revenue_2022")],
                     by = "casino_id")

    if (mkt_actual_rev > 0) {
      within_mkt_share <- mem_rev$total_revenue_2022 / mkt_actual_rev
    } else {
      within_mkt_share <- rep(1 / nrow(mem_rev), nrow(mem_rev))
    }

    st_total <- as.numeric(state_totals[mkt_st])

    for (p in seq_len(nrow(mem_rev))) {
      # Within-state actual share for this property
      prop_actual_ws <- mem_rev$total_revenue_2022[p] / st_total

      # Within-state predicted shares: market's within-state share × within-market split
      prop_pred_ws <- mkt_ws_pred_share * within_mkt_share[p]

      # LN model: same approach
      ln_mkt_ws_share <- ln_pred_shares[k]  # already within-state
      ln_prop_ws <- ln_mkt_ws_share * within_mkt_share[p]

      property_rows[[length(property_rows) + 1]] <- data.frame(
        Property   = mem_rev$name.y[p],
        City       = mem_rev$city[p],
        State      = mem_rev$state.y[p],
        Revenue_M  = round(mem_rev$total_revenue_2022[p] / 1e6, 1),
        State_Total_M = round(st_total / 1e6, 0),
        Actual_State_Pct = round(prop_actual_ws * 100, 2),
        Pred_State_Pct   = round(prop_pred_ws * 100, 2),
        LN_Pred_State_Pct = round(ln_prop_ws * 100, 2),
        stringsAsFactors = FALSE
      )
    }
  }

  prop_table <- do.call(rbind, property_rows)
  prop_table$Error_pp <- round(prop_table$Pred_State_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$LN_Error_pp <- round(prop_table$LN_Pred_State_Pct - prop_table$Actual_State_Pct, 2)
  prop_table <- prop_table[order(-prop_table$Revenue_M), ]

  mkt_mape <- round(mean(abs(pred_shares * 100 - obs_ws_shares * 100)), 2)
  prop_mape <- round(mean(abs(prop_table$Error_pp)), 2)
  ln_prop_mape <- round(mean(abs(prop_table$LN_Error_pp)), 2)
  cat(sprintf("  Gravity: Market within-state MAPE=%.2f pp, Prop MAPE=%.2f pp\n", mkt_mape, prop_mape))
  cat(sprintf("  LN:      Prop MAPE=%.2f pp\n", ln_prop_mape))

  # Return all results
  list(
    label      = model_label,
    radius     = opt_radius,
    comparison = comparison,
    best_model = best_gravity_model,
    best_params = best_gravity_params,
    results    = results,
    ln_results = ln_results,
    best_ln    = best_ln,
    best_ln_type = best_ln_type,
    markets    = markets,
    obs_mkts   = obs_mkts,
    obs_ws_shares = obs_ws_shares,   # within-state actual shares
    obs_mkt_state = obs_mkt_state,   # state for each observed market
    pred_shares = pred_shares,       # within-state predicted shares (gravity)
    ln_pred_shares = ln_pred_shares, # within-state predicted shares (LN)
    prop_table = prop_table,
    mkt_mape   = mkt_mape,
    prop_mape  = prop_mape,
    ln_prop_mape = ln_prop_mape,
    grid_df    = grid_df,
    casinos_df = casinos_df,
    rev_data   = rev_data
  )
}

# =============================================================================
# 8. TABLE PRINTING HELPER
# =============================================================================

print_table <- function(df, title = NULL) {
  if (!is.null(title)) {
    cat("\n")
    cat(strrep("=", 90), "\n")
    cat(" ", title, "\n")
    cat(strrep("=", 90), "\n")
  }

  fmt <- as.data.frame(lapply(df, function(x) {
    if (is.numeric(x)) format(x, big.mark = ",", trim = TRUE)
    else as.character(x)
  }), stringsAsFactors = FALSE)

  nms <- names(df)
  widths <- sapply(seq_along(nms), function(i) {
    max(nchar(nms[i]), max(nchar(fmt[[i]]), na.rm = TRUE))
  })
  aligns <- sapply(df, is.numeric)

  pad <- function(s, w, right = FALSE) {
    gap <- w - nchar(s); gap[gap < 0] <- 0
    if (right) paste0(strrep(" ", gap), s)
    else paste0(s, strrep(" ", gap))
  }

  header <- paste(sapply(seq_along(nms), function(i)
    pad(nms[i], widths[i], aligns[i])), collapse = "  ")
  rule <- paste(sapply(widths, function(w) strrep("-", w)), collapse = "  ")

  cat(header, "\n"); cat(rule, "\n")
  for (r in 1:nrow(fmt)) {
    row <- paste(sapply(seq_along(nms), function(i)
      pad(fmt[[i]][r], widths[i], aligns[i])), collapse = "  ")
    cat(row, "\n")
  }
  cat(rule, "\n")
}

# =============================================================================
# 9. RUN MODEL A: ALL 9 STATES (WITH IOWA)
#    Iowa tribal casinos (Blackbird Bend id=126, Meskwaki id=883,
#    Prairie Flower id=1053) remain in casinos_study as unobserved
#    supply-side controls (they compete for demand but have no revenue data).
# =============================================================================

cat("\n")
cat("#######################################################################\n")
cat("# MODEL A: 9-STATE MODEL (WITH IOWA)                                #\n")
cat("#   Iowa tribal casinos as supply controls (not observed)            #\n")
cat("#######################################################################\n")

# Iowa tribal casino IDs for reference
ia_tribal_ids <- c(126, 883, 1053)  # Blackbird Bend, Meskwaki, Prairie Flower
cat("\n  Iowa tribal casinos as controls (ids:", paste(ia_tribal_ids, collapse=", "), ")\n")
cat("  These casinos compete for demand but have no observed revenue.\n")

model_a <- run_estimation(
  rev_data    = revenue_data,
  model_label = "9-State Model (with Iowa)",
  casinos_df  = casinos_study,
  zips_df     = zips_study,
  pop_inc     = pop_income,
  radii       = c(5, 10),
  save_prefix = "9state"
)

# =============================================================================
# 10. RUN MODEL B: 8 STATES (WITHOUT IOWA)
# =============================================================================

cat("\n")
cat("#######################################################################\n")
cat("# MODEL B: 8-STATE MODEL (WITHOUT IOWA)                             #\n")
cat("#   Iowa removed from observed data; all Iowa casinos become         #\n")
cat("#   supply controls (border state treatment).                        #\n")
cat("#######################################################################\n")

revenue_data_no_ia <- revenue_data[revenue_data$state != "IA", ]
cat("\n  Dropped", nrow(revenue_data) - nrow(revenue_data_no_ia), "Iowa properties from observed data\n")

model_b <- run_estimation(
  rev_data    = revenue_data_no_ia,
  model_label = "8-State Model (without Iowa)",
  casinos_df  = casinos_study,
  zips_df     = zips_study,
  pop_inc     = pop_income,
  radii       = c(5, 10),
  save_prefix = "8state"
)

# =============================================================================
# 11. COMPARISON TABLE
# =============================================================================

cat("\n")
cat("=======================================================================\n")
cat("  MODEL COMPARISON: WITH vs WITHOUT IOWA\n")
cat("  (All R2 and MAPE computed on within-state shares)\n")
cat("=======================================================================\n\n")

# --- Gravity Share Model Comparison ---
comp_gravity <- data.frame(
  Spec   = c(model_a$label, model_b$label),
  N      = c(nrow(model_a$rev_data), nrow(model_b$rev_data)),
  Decay  = c(model_a$best_model, model_b$best_model),
  Beta   = round(c(model_a$best_params[1], model_b$best_params[1]), 4),
  a_H    = round(c(model_a$best_params[2], model_b$best_params[2]), 4),
  a_T    = round(c(model_a$best_params[3], model_b$best_params[3]), 4),
  R2     = round(c(model_a$comparison$R_squared[1], model_b$comparison$R_squared[1]), 4),
  MAPE   = c(model_a$prop_mape, model_b$prop_mape),
  stringsAsFactors = FALSE
)
names(comp_gravity) <- c("Specification", "N", "Decay", "Beta",
                          "a_Hotel", "a_Tables", "WS R2", "WS Prop MAPE")
print_table(comp_gravity, "GRAVITY SHARE MODEL COMPARISON (within-state shares)")

a_h_a <- model_a$best_params[2]; a_t_a <- model_a$best_params[3]
a_h_b <- model_b$best_params[2]; a_t_b <- model_b$best_params[3]
cat("\n  Attractiveness multipliers (9-State / 8-State):\n")
cat(sprintf("    Hotel-only:         %.3fx / %.3fx\n", exp(a_h_a), exp(a_h_b)))
cat(sprintf("    Tables-only:        %.3fx / %.3fx\n", exp(a_t_a), exp(a_t_b)))
cat(sprintf("    Full (H+T):         %.3fx / %.3fx\n", exp(a_h_a + a_t_a), exp(a_h_b + a_t_b)))

# --- Log-Linear Model Comparison ---
comp_ln <- data.frame(
  Spec   = c(model_a$label, model_b$label),
  N      = c(nrow(model_a$rev_data), nrow(model_b$rev_data)),
  Decay  = c(model_a$best_ln_type, model_b$best_ln_type),
  Beta   = round(c(model_a$best_ln$beta_decay, model_b$best_ln$beta_decay), 4),
  Gamma  = round(c(model_a$best_ln$coefs[2], model_b$best_ln$coefs[2]), 4),
  b_H    = round(c(model_a$best_ln$coefs[3], model_b$best_ln$coefs[3]), 4),
  b_T    = round(c(model_a$best_ln$coefs[4], model_b$best_ln$coefs[4]), 4),
  R2     = round(c(model_a$best_ln$r2, model_b$best_ln$r2), 4),
  Adj_R2 = round(c(model_a$best_ln$adj_r2, model_b$best_ln$adj_r2), 4),
  MAPE   = c(model_a$ln_prop_mape, model_b$ln_prop_mape),
  stringsAsFactors = FALSE
)
names(comp_ln) <- c("Specification", "N", "Decay", "Beta", "gamma",
                     "Hotel", "Tables", "R2(ln)", "Adj-R2(ln)", "WS Prop MAPE")
print_table(comp_ln, "LOG-LINEAR (LN) MODEL COMPARISON (WS Prop MAPE on within-state shares)")

# --- LN Coefficients detail for Model A ---
cat("\n  LN Model Coefficients (9-State, best decay):\n")
ln_a <- model_a$best_ln
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "Variable", "Coef", "SE", "t-val"))
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "--------", "----", "--", "-----"))
vnames <- c("Intercept", "ln(Demand)", "Hotel", "Tables")
for (v in 1:4) {
  cat(sprintf("    %-15s  %8.4f  %8.4f  %8.2f\n", vnames[v], ln_a$coefs[v], ln_a$se[v], ln_a$t_vals[v]))
}

# =============================================================================
# 12. CROSS-VALIDATION (LOOCV AND LEAVE-ONE-STATE-OUT)
#     Uses Model A (9-state) with the best gravity and LN specifications.
#     Both CV procedures re-estimate parameters on the training fold,
#     then predict the held-out market(s).
# =============================================================================

cat("\n")
cat("#######################################################################\n")
cat("# CROSS-VALIDATION                                                   #\n")
cat("#######################################################################\n")

# Use Model A's settings: optimal radius, best decay types
cv_radius    <- model_a$radius
cv_grav_type <- tolower(model_a$best_model)   # e.g. "power"
cv_ln_type   <- model_a$best_ln_type          # e.g. "exponential"

# --- Shared helper: build markets from revenue data at given radius ---
build_markets <- function(rev_data_cv, casinos_cv, hc_cv, radius_cv,
                          zips_cv, pop_inc_cv) {
  casinos_cv$observed <- casinos_cv$casino_id %in% rev_data_cv$casino_id
  casinos_cv$market_id <- cutree(hc_cv, h = radius_cv)

  mids <- sort(unique(casinos_cv$market_id))
  nm <- length(mids)

  m_lat <- m_lon <- m_rev <- numeric(nm)
  m_obs <- logical(nm)
  m_hotel <- m_tables <- integer(nm)
  m_state <- character(nm)

  for (k in seq_along(mids)) {
    idx <- which(casinos_cv$market_id == mids[k])
    members_obs <- casinos_cv$observed[idx]
    has_rev <- any(members_obs)
    mem <- merge(casinos_cv[idx, , drop = FALSE],
                 rev_data_cv[, c("casino_id", "total_revenue_2022")],
                 by = "casino_id", all.x = TRUE)
    mem$total_revenue_2022[is.na(mem$total_revenue_2022)] <- 0
    tr <- sum(mem$total_revenue_2022)
    if (has_rev && tr > 0) {
      wts <- mem$total_revenue_2022
      m_lat[k] <- sum(casinos_cv$latitude[idx] * wts) / sum(wts)
      m_lon[k] <- sum(casinos_cv$longitude[idx] * wts) / sum(wts)
      obs_mem <- mem[mem$total_revenue_2022 > 0, ]
      m_state[k] <- obs_mem$state[which.max(obs_mem$total_revenue_2022)]
    } else {
      m_lat[k] <- mean(casinos_cv$latitude[idx])
      m_lon[k] <- mean(casinos_cv$longitude[idx])
      m_state[k] <- casinos_cv$state[idx[1]]
    }
    m_rev[k] <- tr; m_obs[k] <- has_rev
    m_hotel[k]  <- as.integer(any(casinos_cv$has_hotel[idx] == 1, na.rm = TRUE))
    m_tables[k] <- as.integer(any(casinos_cv$has_tables[idx] == 1, na.rm = TRUE))
  }

  n_z <- nrow(zips_cv)
  dmat <- matrix(NA_real_, nrow = n_z, ncol = nm)
  for (j in 1:nm) {
    dmat[, j] <- haversine(zips_cv$longitude, zips_cv$latitude, m_lon[j], m_lat[j])
  }

  list(market_id = mids, n = nm, lat = m_lat, lon = m_lon,
       rev = m_rev, obs = m_obs, hotel = m_hotel, tables = m_tables,
       state = m_state, dist_mat = dmat, casinos = casinos_cv)
}

# --- Gravity share prediction given markets + params ---
predict_gravity <- function(mkt, params, decay_type, pop_inc_cv) {
  beta <- params[1]; a_h <- params[2]; a_t <- params[3]
  dm <- mkt$dist_mat
  if (decay_type == "exponential") { dw <- decay_exponential(dm, beta)
  } else if (decay_type == "power") { dw <- decay_power(dm, beta)
  } else { dw <- decay_gaussian(dm, beta) }
  dw[dm > 150] <- 0
  attract <- exp(a_h * mkt$hotel + a_t * mkt$tables)
  dw <- sweep(dw, 2, attract, "*")
  denom <- rowSums(dw, na.rm = TRUE); denom[denom == 0] <- 1
  ds <- sweep(dw, 1, denom, "/")
  df <- sweep(ds, 1, pop_inc_cv, "*")
  colSums(df, na.rm = TRUE)
}

# --- Fit gravity on training markets, return params ---
# Uses within-state share SSE as objective
fit_gravity_cv <- function(mkt, decay_type, pop_inc_cv) {
  obs_idx <- which(mkt$obs)
  obs_rev <- mkt$rev[obs_idx]
  obs_state <- mkt$state[obs_idx]

  # Within-state actual shares
  obs_ws <- numeric(length(obs_rev))
  for (s in unique(obs_state)) {
    s_idx <- which(obs_state == s)
    obs_ws[s_idx] <- obs_rev[s_idx] / sum(obs_rev[s_idx])
  }

  sse_fn <- function(params) {
    if (params[1] <= 0) return(1e10)
    pd <- predict_gravity(mkt, params, decay_type, pop_inc_cv)
    pd_obs <- pd[obs_idx]
    # Within-state predicted shares
    pred_ws <- numeric(length(pd_obs))
    for (s in unique(obs_state)) {
      s_idx <- which(obs_state == s)
      pred_ws[s_idx] <- pd_obs[s_idx] / sum(pd_obs[s_idx])
    }
    sum((obs_ws - pred_ws)^2, na.rm = TRUE)
  }

  if (decay_type == "exponential") {
    lower <- c(0.001, -2, -2); upper <- c(2, 5, 5); start <- c(0.05, 0.1, 0.1)
  } else if (decay_type == "power") {
    lower <- c(0.1, -2, -2); upper <- c(20, 5, 5); start <- c(2.0, 0.1, 0.1)
  } else {
    lower <- c(1e-5, -2, -2); upper <- c(0.5, 5, 5); start <- c(0.001, 0.1, 0.1)
  }

  fit <- tryCatch({
    optim(start, fn = sse_fn, method = "L-BFGS-B",
          lower = lower, upper = upper,
          control = list(factr = 1e2, maxit = 1000))
  }, error = function(e) list(par = start, value = 1e10))
  fit$par
}

# --- Fit LN model on training markets, return beta_decay + OLS coefs ---
fit_ln_cv <- function(mkt, decay_type, pop_inc_cv) {
  obs_idx <- which(mkt$obs)
  obs_rev <- mkt$rev[obs_idx]
  obs_hotel <- mkt$hotel[obs_idx]
  obs_tables <- mkt$tables[obs_idx]

  ln_obj <- function(beta) {
    if (beta <= 0) return(1e10)
    dm <- mkt$dist_mat
    if (decay_type == "exponential") { dw <- decay_exponential(dm, beta)
    } else if (decay_type == "power") { dw <- decay_power(dm, beta)
    } else { dw <- decay_gaussian(dm, beta) }
    dw[dm > 150] <- 0
    demand_flow <- sweep(dw, 1, pop_inc_cv, "*")
    di <- colSums(demand_flow, na.rm = TRUE)
    di_obs <- di[obs_idx]
    if (any(di_obs <= 0)) return(1e10)
    y <- log(obs_rev)
    X <- cbind(1, log(di_obs), obs_hotel, obs_tables)
    fit <- tryCatch({
      XtX_inv <- solve(crossprod(X))
      b_hat <- XtX_inv %*% crossprod(X, y)
      y_hat <- X %*% b_hat
      ss_res <- sum((y - y_hat)^2); ss_tot <- sum((y - mean(y))^2)
      list(r2 = 1 - ss_res / ss_tot)
    }, error = function(e) list(r2 = -Inf))
    -fit$r2
  }

  if (decay_type == "exponential") { sr <- c(0.001, 1)
  } else if (decay_type == "power") { sr <- c(0.1, 10)
  } else { sr <- c(1e-6, 0.1) }

  opt <- optimize(ln_obj, interval = sr, tol = 1e-6)
  beta_ln <- opt$minimum

  # Get OLS coefficients at optimal beta
  dm <- mkt$dist_mat
  if (decay_type == "exponential") { dw <- decay_exponential(dm, beta_ln)
  } else if (decay_type == "power") { dw <- decay_power(dm, beta_ln)
  } else { dw <- decay_gaussian(dm, beta_ln) }
  dw[dm > 150] <- 0
  demand_flow <- sweep(dw, 1, pop_inc_cv, "*")
  di <- colSums(demand_flow, na.rm = TRUE)

  di_obs <- di[obs_idx]
  y <- log(obs_rev)
  X <- cbind(1, log(di_obs), obs_hotel, obs_tables)
  b_hat <- as.numeric(solve(crossprod(X)) %*% crossprod(X, y))

  list(beta = beta_ln, coefs = b_hat, demand_idx_all = di)
}

# --- Pre-compute hierarchical clustering (shared across folds) ---
n_c <- nrow(casinos_study)
cdist_cv <- matrix(0, n_c, n_c)
for (i in 1:(n_c - 1)) {
  for (j in (i + 1):n_c) {
    d <- haversine(casinos_study$longitude[i], casinos_study$latitude[i],
                   casinos_study$longitude[j], casinos_study$latitude[j])
    cdist_cv[i, j] <- d; cdist_cv[j, i] <- d
  }
}
hc_cv <- hclust(as.dist(cdist_cv), method = "single")

# =====================================================
# 12a. LEAVE-ONE-MARKET-OUT CROSS-VALIDATION (LOOCV)
# =====================================================
# For each observed market j:
#   1. Remove market j's revenue from training data
#   2. Re-estimate model on remaining N-1 markets
#   3. Predict ALL markets (including j) using trained model
#   4. Record predicted share for market j
# The held-out market remains in the supply set (its casinos still compete
# for demand), but its revenue is unknown to the estimator.

cat("\n--- LEAVE-ONE-MARKET-OUT CROSS-VALIDATION (LOOCV) ---\n")
cat("  Evaluating within-state share predictions\n")
cat("  Using", cv_grav_type, "decay (gravity) and", cv_ln_type, "decay (LN)\n")
cat("  Radius:", cv_radius, "mi\n")

# Build full market structure to identify observed markets
full_mkt <- build_markets(revenue_data, casinos_study, hc_cv, cv_radius,
                          zips_study, pop_income)
obs_mkt_idx <- which(full_mkt$obs)
n_obs <- length(obs_mkt_idx)

cat("  Observed markets:", n_obs, "\n")
cat("  Running LOOCV folds...\n")

loocv_grav_pred <- loocv_ln_pred <- numeric(n_obs)
loocv_actual_rev <- full_mkt$rev[obs_mkt_idx]
loocv_mkt_state <- full_mkt$state[obs_mkt_idx]

# Pre-compute within-state actual shares for LOOCV evaluation
loocv_actual_ws <- numeric(n_obs)
for (s in unique(loocv_mkt_state)) {
  s_idx <- which(loocv_mkt_state == s)
  loocv_actual_ws[s_idx] <- loocv_actual_rev[s_idx] / sum(loocv_actual_rev[s_idx])
}

for (fold in 1:n_obs) {
  held_out_mkt <- obs_mkt_idx[fold]
  ho_state <- full_mkt$state[held_out_mkt]

  # Identify casino_ids in the held-out market
  casinos_study$market_id <- cutree(hc_cv, h = cv_radius)
  held_out_casino_ids <- casinos_study$casino_id[
    casinos_study$market_id == full_mkt$market_id[held_out_mkt] &
    casinos_study$casino_id %in% revenue_data$casino_id
  ]

  # Training data: remove held-out market's properties
  train_rev <- revenue_data[!(revenue_data$casino_id %in% held_out_casino_ids), ]

  # Build markets with training revenue (held-out casinos remain in supply)
  train_mkt <- build_markets(train_rev, casinos_study, hc_cv, cv_radius,
                             zips_study, pop_income)

  # --- Gravity: train, then predict ---
  grav_params <- fit_gravity_cv(train_mkt, cv_grav_type, pop_income)
  grav_demand <- predict_gravity(train_mkt, grav_params, cv_grav_type, pop_income)

  # The held-out market index in train_mkt
  ho_idx_train <- which(train_mkt$market_id == full_mkt$market_id[held_out_mkt])
  if (length(ho_idx_train) == 1) {
    # Within-state predicted share: demand_j / sum(demand_k for all k in same state)
    same_state_idx <- which(train_mkt$state == ho_state &
                             (train_mkt$obs | train_mkt$market_id == full_mkt$market_id[held_out_mkt]))
    state_total_demand <- sum(grav_demand[same_state_idx])
    loocv_grav_pred[fold] <- grav_demand[ho_idx_train] / state_total_demand
  }

  # --- LN: train, then predict ---
  ln_fit <- fit_ln_cv(train_mkt, cv_ln_type, pop_income)
  di_ho <- ln_fit$demand_idx_all[ho_idx_train]
  if (length(ho_idx_train) == 1 && di_ho > 0) {
    hotel_ho <- train_mkt$hotel[ho_idx_train]
    tables_ho <- train_mkt$tables[ho_idx_train]
    ln_pred_rev_ho <- exp(ln_fit$coefs[1] + ln_fit$coefs[2] * log(di_ho) +
                          ln_fit$coefs[3] * hotel_ho + ln_fit$coefs[4] * tables_ho)

    # Within-state LN predicted share
    # Get predicted revenue for all observed same-state markets + held-out
    same_state_obs_idx <- which(train_mkt$obs & train_mkt$state == ho_state)
    ln_state_rev <- numeric(length(same_state_obs_idx))
    for (h in seq_along(same_state_obs_idx)) {
      di_h <- ln_fit$demand_idx_all[same_state_obs_idx[h]]
      if (di_h > 0) {
        ln_state_rev[h] <- exp(ln_fit$coefs[1] + ln_fit$coefs[2] * log(di_h) +
                                ln_fit$coefs[3] * train_mkt$hotel[same_state_obs_idx[h]] +
                                ln_fit$coefs[4] * train_mkt$tables[same_state_obs_idx[h]])
      }
    }
    loocv_ln_pred[fold] <- ln_pred_rev_ho / (sum(ln_state_rev) + ln_pred_rev_ho)
  }

  if (fold %% 10 == 0 || fold == n_obs) {
    cat(sprintf("    Fold %d/%d complete\n", fold, n_obs))
  }
}

# LOOCV metrics on within-state shares
loocv_grav_err <- (loocv_grav_pred - loocv_actual_ws) * 100  # in pp
loocv_ln_err   <- (loocv_ln_pred - loocv_actual_ws) * 100

loocv_grav_mape <- round(mean(abs(loocv_grav_err)), 2)
loocv_ln_mape   <- round(mean(abs(loocv_ln_err)), 2)
loocv_grav_rmse <- round(sqrt(mean(loocv_grav_err^2)), 2)
loocv_ln_rmse   <- round(sqrt(mean(loocv_ln_err^2)), 2)

# LOOCV R-squared (on within-state shares)
loocv_tss <- sum((loocv_actual_ws * 100 - mean(loocv_actual_ws * 100))^2)
loocv_grav_r2 <- round(1 - sum(loocv_grav_err^2) / loocv_tss, 4)
loocv_ln_r2   <- round(1 - sum(loocv_ln_err^2) / loocv_tss, 4)

cat("\n  LOOCV Results (within-state shares, percentage points):\n")
cat(sprintf("    Gravity (%s): MAPE=%.2f pp, RMSE=%.2f pp, R2=%.4f\n",
            cv_grav_type, loocv_grav_mape, loocv_grav_rmse, loocv_grav_r2))
cat(sprintf("    LN      (%s): MAPE=%.2f pp, RMSE=%.2f pp, R2=%.4f\n",
            cv_ln_type, loocv_ln_mape, loocv_ln_rmse, loocv_ln_r2))

# Compare in-sample vs LOOCV (gravity only -- both use within-state share R2)
# Note: LN in-sample R2 is on ln(revenue) via OLS, not on within-state shares,
# so the shrinkage comparison is not apples-to-apples for LN.
cat("\n  In-sample vs LOOCV comparison (within-state share R2):\n")
cat(sprintf("    Gravity: In-sample WS R2=%.4f, LOOCV WS R2=%.4f (shrinkage=%.4f)\n",
            model_a$comparison$R_squared[1], loocv_grav_r2,
            model_a$comparison$R_squared[1] - loocv_grav_r2))
cat(sprintf("    LN:      In-sample OLS R2(ln)=%.4f, LOOCV WS R2=%.4f (different metrics)\n",
            model_a$best_ln$r2, loocv_ln_r2))

# =====================================================
# 12b. LEAVE-ONE-STATE-OUT CROSS-VALIDATION (LOSO)
# =====================================================
# For each of the 9 primary states:
#   1. Remove all properties in that state from training revenue
#   2. Those casinos remain in the supply set (border treatment)
#   3. Re-estimate model on remaining states
#   4. Predict the held-out state's properties
# This tests whether the model can forecast revenue in an entirely new state.

cat("\n--- LEAVE-ONE-STATE-OUT CROSS-VALIDATION (LOSO) ---\n")
cat("  Predicting within-state venue allocation for held-out state\n")

loso_states <- sort(unique(revenue_data$state))
cat("  States:", paste(loso_states, collapse = ", "), "\n")

loso_results <- list()

for (st in loso_states) {
  # Training: remove this state's revenue
  train_rev <- revenue_data[revenue_data$state != st, ]
  n_held_out <- sum(revenue_data$state == st)

  # Build markets
  train_mkt <- build_markets(train_rev, casinos_study, hc_cv, cv_radius,
                             zips_study, pop_income)

  # --- Gravity ---
  grav_params <- fit_gravity_cv(train_mkt, cv_grav_type, pop_income)
  grav_demand <- predict_gravity(train_mkt, grav_params, cv_grav_type, pop_income)

  # Identify held-out state's markets (markets containing observed casinos from this state)
  casinos_study$market_id <- cutree(hc_cv, h = cv_radius)
  state_casino_ids <- revenue_data$casino_id[revenue_data$state == st]
  state_mkt_ids <- unique(casinos_study$market_id[
    casinos_study$casino_id %in% state_casino_ids
  ])

  # Map those market IDs to indices in train_mkt
  ho_indices <- match(state_mkt_ids, train_mkt$market_id)
  ho_indices <- ho_indices[!is.na(ho_indices)]

  # Get actual revenue for each held-out market
  ho_actual_rev <- numeric(length(ho_indices))
  for (h in seq_along(ho_indices)) {
    mid <- train_mkt$market_id[ho_indices[h]]
    member_ids <- casinos_study$casino_id[casinos_study$market_id == mid &
                                          casinos_study$casino_id %in% state_casino_ids]
    ho_actual_rev[h] <- sum(revenue_data$total_revenue_2022[
      revenue_data$casino_id %in% member_ids])
  }

  # Within-state actual shares
  state_actual_rev <- sum(ho_actual_rev)
  ho_actual_ws <- ho_actual_rev / state_actual_rev

  # Gravity: within-state predicted shares (demand of each market / sum of state's market demands)
  ho_grav_demand <- grav_demand[ho_indices]
  grav_ws_pred <- ho_grav_demand / sum(ho_grav_demand)
  grav_ws_err <- (grav_ws_pred - ho_actual_ws) * 100  # in pp

  # --- LN ---
  ln_fit <- fit_ln_cv(train_mkt, cv_ln_type, pop_income)
  ln_pred_rev_ho <- numeric(length(ho_indices))
  for (h in seq_along(ho_indices)) {
    di_h <- ln_fit$demand_idx_all[ho_indices[h]]
    if (di_h > 0) {
      ln_pred_rev_ho[h] <- exp(ln_fit$coefs[1] + ln_fit$coefs[2] * log(di_h) +
                                ln_fit$coefs[3] * train_mkt$hotel[ho_indices[h]] +
                                ln_fit$coefs[4] * train_mkt$tables[ho_indices[h]])
    }
  }
  # LN within-state predicted shares
  ln_ws_pred <- ln_pred_rev_ho / sum(ln_pred_rev_ho)
  ln_ws_err <- (ln_ws_pred - ho_actual_ws) * 100  # in pp

  loso_results[[st]] <- list(
    state = st, n_properties = n_held_out, n_markets = length(ho_indices),
    actual_rev_M = round(state_actual_rev / 1e6, 1),
    grav_ws_mape = round(mean(abs(grav_ws_err)), 2),
    ln_ws_mape = round(mean(abs(ln_ws_err)), 2),
    ho_actual_ws = ho_actual_ws,
    grav_ws_pred = grav_ws_pred,
    ln_ws_pred = ln_ws_pred
  )

  cat(sprintf("  %-2s: %2d props (%d mkts), State GGR=$%6.0fM | Grav within-state MAPE=%.2f pp | LN MAPE=%.2f pp\n",
              st, n_held_out, length(ho_indices),
              state_actual_rev / 1e6,
              loso_results[[st]]$grav_ws_mape,
              loso_results[[st]]$ln_ws_mape))
}

# LOSO summary table
loso_df <- do.call(rbind, lapply(loso_results, function(x) {
  data.frame(State = x$state, N = x$n_properties, Mkts = x$n_markets,
             State_GGR_M = x$actual_rev_M,
             Grav_WS_MAPE = x$grav_ws_mape,
             LN_WS_MAPE = x$ln_ws_mape,
             stringsAsFactors = FALSE)
}))
print_table(loso_df, "LEAVE-ONE-STATE-OUT: WITHIN-STATE SHARE MAPE (pp)")

# Overall LOSO within-state MAPE (average across states)
loso_grav_overall <- round(mean(sapply(loso_results, function(x) x$grav_ws_mape)), 2)
loso_ln_overall   <- round(mean(sapply(loso_results, function(x) x$ln_ws_mape)), 2)

cat(sprintf("\n  LOSO mean within-state MAPE: Gravity=%.2f pp, LN=%.2f pp\n",
            loso_grav_overall, loso_ln_overall))

# =====================================================
# 12c. CV SUMMARY TABLE
# =====================================================

cat("\n")
cv_summary <- data.frame(
  Metric = c("In-Sample WS R2", "In-Sample WS MAPE (pp)",
             "LOOCV WS R2", "LOOCV WS MAPE (pp)", "LOOCV WS RMSE (pp)",
             "LOSO Mean WS MAPE (pp)"),
  Gravity = c(
    round(model_a$comparison$R_squared[1], 4),
    model_a$mkt_mape,
    loocv_grav_r2,
    loocv_grav_mape,
    loocv_grav_rmse,
    loso_grav_overall
  ),
  LN = c(
    paste0(round(model_a$best_ln$r2, 4), "*"),
    model_a$ln_prop_mape,
    loocv_ln_r2,
    loocv_ln_mape,
    loocv_ln_rmse,
    loso_ln_overall
  ),
  stringsAsFactors = FALSE
)
print_table(cv_summary, "CROSS-VALIDATION SUMMARY (within-state shares)")
cat("  * LN in-sample R2 is OLS R2 on ln(revenue), not on within-state shares\n")

# =============================================================================
# 13. DETAILED OUTPUT TABLES (for Model A as primary)
# =============================================================================

cat("\n")

# Property-level table for Model A (includes both gravity and LN predictions)
# All shares are within-state: each property's share of its own state's GGR
prop_a <- model_a$prop_table[, c("Property", "City", "State", "Revenue_M", "State_Total_M",
                                  "Actual_State_Pct", "Pred_State_Pct", "Error_pp",
                                  "LN_Pred_State_Pct", "LN_Error_pp")]
names(prop_a) <- c("Property", "City", "St", "Rev ($M)", "St Total ($M)",
                    "Actual WS%", "Grav WS%", "Grav Err", "LN WS%", "LN Err")
print_table(prop_a, "TABLE: WITHIN-STATE SHARE -- PROPERTY LEVEL (Model A: 9-State)")

# Property-level table for Model B
prop_b <- model_b$prop_table[, c("Property", "City", "State", "Revenue_M", "State_Total_M",
                                  "Actual_State_Pct", "Pred_State_Pct", "Error_pp",
                                  "LN_Pred_State_Pct", "LN_Error_pp")]
names(prop_b) <- c("Property", "City", "St", "Rev ($M)", "St Total ($M)",
                    "Actual WS%", "Grav WS%", "Grav Err", "LN WS%", "LN Err")
print_table(prop_b, "TABLE: WITHIN-STATE SHARE -- PROPERTY LEVEL (Model B: 8-State)")

# =============================================================================
# 14. SCATTER PLOT: PREDICTED VS ACTUAL REVENUE SHARE (both models)
# =============================================================================

cat("\n--- GENERATING SCATTER PLOTS ---\n")

# Function to create a labeled scatter plot
make_scatter <- function(model_result, filename, title_suffix) {
  pt <- model_result$prop_table
  actual_pct <- pt$Actual_State_Pct
  pred_pct   <- pt$Pred_State_Pct
  rev_m      <- pt$Revenue_M
  labels     <- pt$Property
  states     <- pt$State

  # Short labels: truncate long names
  short_labels <- sapply(labels, function(nm) {
    if (nchar(nm) > 28) paste0(substr(nm, 1, 25), "...") else nm
  })

  # State-based colors
  state_cols <- c(
    "PA" = "#1f77b4", "OH" = "#ff7f0e", "MD" = "#2ca02c", "NY" = "#d62728",
    "MA" = "#9467bd", "CT" = "#8c564b", "IN" = "#e377c2", "MO" = "#7f7f7f",
    "IA" = "#bcbd22"
  )
  pt_colors <- state_cols[states]
  pt_colors[is.na(pt_colors)] <- "black"

  # Transparent versions for points
  pt_colors_alpha <- sapply(pt_colors, function(col) {
    rgb_vals <- col2rgb(col) / 255
    rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = 0.7)
  })

  # Point sizes scaled by revenue (sqrt scale for area proportionality)
  pt_cex <- 0.8 + 1.8 * sqrt((rev_m - min(rev_m)) / (max(rev_m) - min(rev_m)))

  max_val <- max(c(actual_pct, pred_pct)) * 1.08

  png(filename, width = 1400, height = 1100, res = 130)
  par(mar = c(5, 5, 4, 8), xpd = FALSE)

  plot(actual_pct, pred_pct,
       pch = 21, bg = pt_colors_alpha, col = pt_colors, lwd = 0.5,
       cex = pt_cex,
       xlim = c(0, max_val), ylim = c(0, max_val),
       xlab = "Actual Within-State Revenue Share (%)",
       ylab = "Predicted Within-State Revenue Share (%)",
       main = paste0("Predicted vs Actual Within-State Share\n", model_result$label),
       cex.main = 1.0, cex.lab = 1.0, las = 1)

  # 45-degree line
  abline(0, 1, lty = 1, col = "gray60", lwd = 1)
  # +/- 1pp error bands
  abline(1, 1, lty = 3, col = "gray80", lwd = 0.8)
  abline(-1, 1, lty = 3, col = "gray80", lwd = 0.8)

  # Identify which points to label:
  # 1. Top 10 by revenue (always label)
  # 2. Any with absolute error > 0.85pp (notable outliers)
  err_abs <- abs(pt$Error_pp)
  top10_idx <- which(rank(-rev_m) <= 10)
  outlier_idx <- which(err_abs > 0.85)
  label_idx <- sort(unique(c(top10_idx, outlier_idx)))

  # Label positioning: try to avoid overlaps
  for (i in label_idx) {
    x <- actual_pct[i]; y <- pred_pct[i]
    err <- pt$Error_pp[i]

    # Position based on which side of the 45-deg line and location
    if (err > 0.3) {
      # Overpredicted: label to the left
      pos_val <- 2
    } else if (err < -0.3) {
      # Underpredicted: label to the right
      pos_val <- 4
    } else if (x > 3) {
      # Large casino near line: label above
      pos_val <- 3
    } else {
      pos_val <- 4
    }

    text(x, y, short_labels[i],
         cex = 0.48, pos = pos_val, offset = 0.35, col = pt_colors[i],
         font = 1)
  }

  # Legend: state colors (outside plot area)
  par(xpd = TRUE)
  present_states <- sort(unique(states))
  legend_cols <- state_cols[present_states]
  legend(max_val * 1.02, max_val,
         legend = present_states,
         col = legend_cols, pt.bg = legend_cols,
         pch = 21, cex = 0.7, pt.cex = 1.2,
         title = "State", bg = "white", bty = "n")
  par(xpd = FALSE)

  # Model info box
  best <- model_result$comparison[1, ]
  legend("bottomright",
         legend = c(
           paste0(best$Model, " decay (beta = ", round(best$Beta, 3), ")"),
           bquote(paste("R"^"2", " = ", .(round(best$R_squared, 3)))),
           paste0("MAPE = ", model_result$prop_mape, " pp"),
           paste0("N = ", nrow(pt), " properties")
         ),
         cex = 0.65, bg = "white", box.col = "gray80")

  # Note about error bands
  text(max_val * 0.6, 0.3, "Dashed lines: +/- 1 pp error bands",
       cex = 0.5, col = "gray50")

  dev.off()
  cat("  Saved:", filename, "\n")
}

make_scatter(model_a, "results/scatter_9state_with_iowa.png",
             "9-State (with Iowa)")
make_scatter(model_b, "results/scatter_8state_no_iowa.png",
             "8-State (without Iowa)")

# =============================================================================
# 15. SAVE ALL RESULTS
# =============================================================================

cat("\n--- SAVING RESULTS ---\n")

if (!dir.exists("results")) dir.create("results")

# Model A outputs
saveRDS(model_a$results, "results/model_estimates_9state.rds")
saveRDS(model_a$markets, "results/markets_9state.rds")
write.csv(model_a$comparison, "results/model_comparison_9state.csv", row.names = FALSE)
write.csv(model_a$grid_df, "results/grid_search_9state.csv", row.names = FALSE)

prop_csv_a <- model_a$prop_table
names(prop_csv_a) <- c("Property", "City", "State", "Revenue_M", "State_Total_M",
                        "Actual_WS_Pct", "Gravity_WS_Pct", "LN_WS_Pct",
                        "Gravity_Error_pp", "LN_Error_pp")
write.csv(prop_csv_a, "results/within_state_shares_9state.csv", row.names = FALSE)

# Model B outputs
saveRDS(model_b$results, "results/model_estimates_8state.rds")
saveRDS(model_b$markets, "results/markets_8state.rds")
write.csv(model_b$comparison, "results/model_comparison_8state.csv", row.names = FALSE)
write.csv(model_b$grid_df, "results/grid_search_8state.csv", row.names = FALSE)

prop_csv_b <- model_b$prop_table
names(prop_csv_b) <- c("Property", "City", "State", "Revenue_M", "State_Total_M",
                        "Actual_WS_Pct", "Gravity_WS_Pct", "LN_WS_Pct",
                        "Gravity_Error_pp", "LN_Error_pp")
write.csv(prop_csv_b, "results/within_state_shares_8state.csv", row.names = FALSE)

# Comparison
write.csv(comp_gravity, "results/model_comparison_gravity.csv", row.names = FALSE)
write.csv(comp_ln, "results/model_comparison_ln.csv", row.names = FALSE)

# Cross-validation
write.csv(loso_df, "results/loso_cv_results.csv", row.names = FALSE)
write.csv(cv_summary, "results/cv_summary.csv", row.names = FALSE)

cat("  All results saved to results/ directory\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
