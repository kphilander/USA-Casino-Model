# =============================================================================
# Build pre-computed data bundle for Casino Revenue Forecasting Shiny App
# =============================================================================
# Run once from the research/ directory:
#   source("build_data.R")
#
# Creates forecast_app/forecast_data.rds containing everything the app needs.
# Now covers ALL 50 US states using AGA 2024 state-level revenue for calibration.
# =============================================================================

cat("Building forecast data bundle (nationwide)...\n")

# ---- Load raw data ----
allzips    <- readRDS("../allzips.rds")
casinodata <- readRDS("../casinodata.rds")

# AGA 2024 state-level land-based casino revenue
aga_revenue <- read.csv("data/aga_revenue_2024.csv", stringsAsFactors = FALSE)
cat("  AGA revenue data:", nrow(aga_revenue), "states, $",
    round(sum(aga_revenue$land_based_casino_rev_M) / 1000, 1), "B total\n")

# ---- Full state abbreviation mapping ----
state_abbrev <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
  "District of Columbia" = "DC", "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI",
  "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA",
  "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME",
  "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN",
  "Mississippi" = "MS", "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE",
  "Nevada" = "NV", "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM",
  "New York" = "NY", "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH",
  "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI",
  "South Carolina" = "SC", "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX",
  "Utah" = "UT", "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA",
  "West Virginia" = "WV", "Wisconsin" = "WI", "Wyoming" = "WY"
)

# ---- ALL ZIPs nationwide (plain data frame for model computation) ----
# Filter to continental US (exclude territories but keep DC)
valid_states <- c(state.abb, "DC")
zips_study <- allzips[allzips$state.x %in% valid_states, ]
zips_study <- data.frame(
  zipcode = zips_study$zipcode, state = zips_study$state.x,
  latitude = zips_study$latitude, longitude = zips_study$longitude,
  adultpop = zips_study$adultpop, income = zips_study$income
)
# Remove ZIPs with missing data
zips_study <- zips_study[!is.na(zips_study$latitude) & !is.na(zips_study$longitude) &
                         !is.na(zips_study$adultpop) & !is.na(zips_study$income), ]
pop_income <- zips_study$adultpop * (zips_study$income / 50000)

cat("  ZIPs:", nrow(zips_study), "\n")

# ---- ALL ZIPs for map rendering (sf with geometry) ----
library(sf)
allzips_map <- allzips[allzips$state.x %in% valid_states,
                       c("zipcode", "state.x", "latitude", "longitude",
                         "city.x", "adultpop", "income", "geometry")]
names(allzips_map)[names(allzips_map) == "state.x"] <- "state"
names(allzips_map)[names(allzips_map) == "city.x"]  <- "city"
allzips_map <- st_as_sf(allzips_map)

cat("  Map ZIPs:", nrow(allzips_map), "\n")

# ---- ALL casinos nationwide (commercial + tribal) ----
# Filter to casinos with geocoded locations in known states
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
  tribal = casinos_study$tribal,
  stringsAsFactors = FALSE
)
casinos_study <- casinos_study[!is.na(casinos_study$latitude) & !is.na(casinos_study$longitude), ]

# Inject Caesars Southern Indiana (missing from base data)
casinos_study <- rbind(casinos_study, data.frame(
  casino_id = 9999, name = "Caesars Southern Indiana",
  state = "IN", latitude = 38.1796, longitude = -85.9054,
  hotel = 1, tribal = "Commercial", stringsAsFactors = FALSE
))

# Inject WA cardrooms not in base casinodata.rds
wa_inject <- data.frame(
  casino_id = c(10001, 10002, 10003, 10004),
  name      = c("Fortune Casino Renton", "Caribbean Cardroom",
                 "Clearwater Saloon and Casino", "New Phoenix"),
  state     = rep("WA", 4),
  latitude  = c(47.4799, 47.6848, 47.4235, 45.8626),
  longitude = c(-122.2034, -122.1965, -120.3103, -122.6713),
  hotel     = rep(0, 4),
  tribal    = rep("Commercial", 4),
  stringsAsFactors = FALSE
)
casinos_study <- rbind(casinos_study, wa_inject)

# Fix WA tribal misclassifications (tagged "Commercial" in casinodata but are tribal)
tribal_fix_ids <- c(943, 1117, 12)  # Muckleshoot, Quil Ceda Creek, 7 Cedars
casinos_study$tribal[casinos_study$casino_id %in% tribal_fix_ids] <- "Tribal"
cat("  Injected 4 WA cardrooms + fixed", length(tribal_fix_ids), "WA tribal misclassifications\n")

# Hotel overrides (verified for 9-state analysis)
casinos_study$has_hotel <- casinos_study$hotel
casinos_study$has_hotel[casinos_study$casino_id == 1060] <- 0
casinos_study$has_hotel[casinos_study$casino_id == 865]  <- 0
casinos_study$has_hotel[casinos_study$casino_id == 1167] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 83]   <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1162] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 557]  <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1155] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1170] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 366]  <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1434] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1435] <- 1

# has_tables: default 1, override known slots-only from 9-state revenue data
casinos_study$has_tables <- 1

# Load 9-state property-level data to identify slots-only facilities
# AND build combined revenue_data for revenue-weighted market centroids
revenue_files <- c("pa", "oh", "md", "ny", "ma", "ct", "in", "mo", "ia", "wa")
slots_only_ids <- c()
revenue_data_list <- list()
for (f in revenue_files) {
  rev <- read.csv(paste0("data/", f, "_revenue_2022.csv"), stringsAsFactors = FALSE)
  rev <- rev[!is.na(rev$casino_id), ]
  # CT table revenue scaling (31.8% table share from Mohegan FY22 SEC filings)
  if (f == "ct") {
    rev$table_revenue_2022 <- round(rev$slots_revenue_2022 * 0.318 / (1 - 0.318))
    rev$total_revenue_2022 <- rev$slots_revenue_2022 + rev$table_revenue_2022
  }
  revenue_data_list[[f]] <- rev[, c("casino_id", "state", "total_revenue_2022")]
  # Slots-only if table revenue is 0
  for (i in 1:nrow(rev)) {
    trev <- rev$table_revenue_2022[i]
    if (!is.na(trev) && trev == 0) slots_only_ids <- c(slots_only_ids, rev$casino_id[i])
  }
  # Special facility types
  if (f == "oh") slots_only_ids <- c(slots_only_ids, rev$casino_id[rev$facility_type == "racino"])
  if (f == "ny") slots_only_ids <- c(slots_only_ids, rev$casino_id[rev$facility_type == "VLT"])
  if (f == "ma") slots_only_ids <- c(slots_only_ids, rev$casino_id[grepl("Plainridge", rev$name, ignore.case = TRUE)])
}
slots_only_ids <- unique(slots_only_ids[!is.na(slots_only_ids)])
casinos_study$has_tables[casinos_study$casino_id %in% slots_only_ids] <- 0
revenue_data <- do.call(rbind, revenue_data_list)
revenue_data <- revenue_data[!is.na(revenue_data$total_revenue_2022), ]

# is_cardroom: WA and CA commercial venues are cardrooms by state law (no slot machines)
casinos_study$is_cardroom <- 0
wa_revenue <- read.csv("data/wa_revenue_2022.csv", stringsAsFactors = FALSE)
wa_cardroom_ids <- wa_revenue$casino_id
casinos_study$is_cardroom[casinos_study$casino_id %in% wa_cardroom_ids] <- 1
# Remaining WA commercial (non-tribal) = unobserved cardrooms
wa_comm_mask <- casinos_study$state == "WA" & casinos_study$tribal == "Commercial" &
                casinos_study$is_cardroom == 0
casinos_study$is_cardroom[wa_comm_mask] <- 1
# CA: all commercial venues are cardrooms
ca_comm_mask <- casinos_study$state == "CA" & casinos_study$tribal == "Commercial"
casinos_study$is_cardroom[ca_comm_mask] <- 1
cat("  Cardrooms (is_cardroom=1):", sum(casinos_study$is_cardroom),
    "( WA:", sum(casinos_study$is_cardroom[casinos_study$state == "WA"]),
    ", CA:", sum(casinos_study$is_cardroom[casinos_study$state == "CA"]), ")\n")

cat("  Casinos:", nrow(casinos_study), "(", sum(casinos_study$tribal == "Commercial"),
    "commercial,", sum(casinos_study$tribal == "Tribal"), "tribal )\n")

# ---- Convenience gaming filter ----
# Remove properties that are bars, gas stations, truck stops, etc. with
# incidental gaming — not actual casinos/cardrooms. AGA commercial GGR
# excludes convenience gaming revenue, so including these venues distorts
# both supply-side competition and revenue allocation.
#
# Methodology: state-specific rules + name-pattern matching.
# See run_analysis.R Section 3b for the 10-model-state methodology.
# Below extends the filter to ALL remaining US states.

casinos_study$is_convenience <- 0

# --- STUDY REGION STATES (from run_analysis.R) ---

# OK: Gasinos, Travel Plazas, Trading Posts, Gaming Centers (without "Casino")
ok_conv <- casinos_study$state == "OK" &
  grepl("(?i)(gasino|travel plaza|travel stop|travel center|trading post)", casinos_study$name, perl = TRUE)
ok_gc <- casinos_study$state == "OK" &
  grepl("Gaming Center", casinos_study$name, ignore.case = TRUE) &
  !grepl("Casino", casinos_study$name, ignore.case = TRUE)
casinos_study$is_convenience[ok_conv | ok_gc] <- 1

# CA: Yokut Gas Station (1468), Bear River Pump & Play (89)
casinos_study$is_convenience[casinos_study$casino_id %in% c(1468, 89)] <- 1

# NH: All properties are charitable gaming (not commercial casinos)
casinos_study$is_convenience[casinos_study$state == "NH"] <- 1

# PA: Hollywood Casino OTB (639) — satellite betting parlor
casinos_study$is_convenience[casinos_study$casino_id == 639] <- 1

# --- REMAINING US STATES ---

# MONTANA: ALL properties are bars/taverns/saloons with gaming machines.
# Montana has NO traditional casinos. State law allows up to 20 video
# gaming machines per liquor-licensed establishment. The AGA explicitly
# excludes Montana from its commercial casino counts.
casinos_study$is_convenience[casinos_study$state == "MT"] <- 1

# NEVADA: NV has ~200 full (nonrestricted) casinos but also ~2,450
# restricted gaming locations (bars, taverns, grocery stores, truck stops)
# with up to 15 slot machines each. The AGA excludes restricted locations.
# Flag identifiable chain tavern gaming + truck stops:
nv_mask <- casinos_study$state == "NV"
# Chain tavern gaming operations (all are restricted licensees):
nv_chains <- nv_mask & grepl("(?i)^(Dotty'?s|PT'?s |Sierra Gold|Jackpot Joanie|Jackpot Crossing|Sean Patrick|Village Pub|Wildfire)", casinos_study$name, perl = TRUE)
casinos_study$is_convenience[nv_chains] <- 1
# Truck stops and travel plazas:
nv_truck <- nv_mask & grepl("(?i)(travel plaza|travel stop|travel center|flying j|pilot travel|love'?s travel)", casinos_study$name, perl = TRUE)
casinos_study$is_convenience[nv_truck] <- 1
# Alamo Casino chain (truck stop casinos at Petro/TA stops):
nv_alamo <- nv_mask & grepl("^Alamo Casino", casinos_study$name)
casinos_study$is_convenience[nv_alamo] <- 1
# Explicitly named bars/pubs/lounges that are restricted gaming:
nv_bars <- nv_mask & casinos_study$casino_id %in% c(
  107,   # Big Dogs Draft House
  335,   # Crossroads Video Poker Lounge
  890,   # Michael Gaughan Airport Slots (airport slot concession)
  903    # Moapa Paiute Travel Plaza and Casino
)
casinos_study$is_convenience[nv_bars] <- 1

# LOUISIANA: Whitelist approach — only riverboats, racinos, tribals, and
# Harrah's NOLA are actual casinos. Everything else is video poker bars,
# Cash Magic truck stops, or truck plaza gaming (LA allows up to 50 VLTs
# per qualified truck stop/bar). AGA commercial GGR excludes these.
la_legit_ids <- c(
  35, 92, 139, 140,        # Amelia Belle, Belle of BR, Boomtown Bossier, Boomtown Harvey
  322, 339, 1016,           # Coushatta, Cypress Bayou, Paragon (tribal)
  363, 446, 541, 602,       # Diamond Jacks, Eldorado, Golden Nugget, Harrah's NOLA
  629, 653, 678,            # Hollywood BR, Horseshoe Bossier, Isle of Capri
  749, 827, 870,            # L'Auberge BR, L'Auberge LC, Margaritaville
  458, 460,                 # Evangeline Downs, Fair Grounds (racinos)
  1196, 1371                # Sam's Town, Treasure Chest
)
la_convenience <- casinos_study$state == "LA" & !casinos_study$casino_id %in% la_legit_ids
casinos_study$is_convenience[la_convenience] <- 1

# NORTH DAKOTA: All 20 "commercial" properties are bars, hotels, and
# restaurants with charitable gaming (pull-tabs, blackjack with $25 max).
# ND has NO commercial casinos. Only the 5 tribal casinos are real.
nd_commercial <- casinos_study$state == "ND" & casinos_study$tribal == "Commercial"
casinos_study$is_convenience[nd_commercial] <- 1

# NEW MEXICO: Travel centers with gaming
nm_tc <- casinos_study$state == "NM" &
  grepl("(?i)travel center", casinos_study$name, perl = TRUE)
casinos_study$is_convenience[nm_tc] <- 1

# FLORIDA: Cruise ship (not a land-based casino)
casinos_study$is_convenience[casinos_study$casino_id == 191] <- 1  # Carnival Cruise Lines Vista

# WYOMING: Smokeshop with gaming (not a casino)
casinos_study$is_convenience[casinos_study$casino_id == 18] <- 1   # 789 Smokeshop & Casino

# --- Print summary ---
conv_by_state <- tapply(casinos_study$is_convenience, casinos_study$state,
                        function(x) sum(x == 1))
conv_by_state <- conv_by_state[conv_by_state > 0]
n_conv <- sum(casinos_study$is_convenience == 1)
cat("  Convenience gaming flagged:", n_conv, "properties\n")
for (st in names(sort(conv_by_state, decreasing = TRUE))) {
  cat("    ", st, ":", conv_by_state[st], "\n")
}
casinos_study <- casinos_study[casinos_study$is_convenience == 0, ]
cat("  After filtering:", nrow(casinos_study), "casinos\n")

# ---- Haversine function ----
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 3959
  lon1_rad <- lon1 * pi / 180; lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180; lat2_rad <- lat2 * pi / 180
  dlon <- lon2_rad - lon1_rad; dlat <- lat2_rad - lat1_rad
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  R * 2 * asin(sqrt(a))
}

# ---- Build markets (5-mile clustering) ----
cat("  Computing casino distance matrix...\n")
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
casinos_study$market_id <- cutree(hc, h = 5)

market_ids <- sort(unique(casinos_study$market_id))
n_mkts <- length(market_ids)

markets <- data.frame(
  market_id = integer(n_mkts), n_casinos = integer(n_mkts),
  latitude = numeric(n_mkts), longitude = numeric(n_mkts),
  observed = logical(n_mkts), revenue = numeric(n_mkts),
  has_hotel = integer(n_mkts), has_tables = integer(n_mkts),
  is_cardroom = integer(n_mkts),
  state = character(n_mkts), label = character(n_mkts),
  stringsAsFactors = FALSE
)

for (k in seq_along(market_ids)) {
  mid <- market_ids[k]
  idx <- which(casinos_study$market_id == mid)

  # Revenue-weighted centroid (matches run_analysis.R and forecast_new_site.R)
  mem_rev <- merge(casinos_study[idx, , drop = FALSE],
                   revenue_data[, c("casino_id", "total_revenue_2022")],
                   by = "casino_id", all.x = TRUE)
  mem_rev$total_revenue_2022[is.na(mem_rev$total_revenue_2022)] <- 0
  tot_rev <- sum(mem_rev$total_revenue_2022)
  has_rev <- tot_rev > 0

  if (has_rev) {
    wts <- mem_rev$total_revenue_2022
    lat <- sum(casinos_study$latitude[idx] * wts) / sum(wts)
    lon <- sum(casinos_study$longitude[idx] * wts) / sum(wts)
  } else {
    lat <- mean(casinos_study$latitude[idx])
    lon <- mean(casinos_study$longitude[idx])
  }

  lbl <- paste(casinos_study$name[idx], collapse = " + ")
  if (nchar(lbl) > 60) lbl <- paste0(substr(lbl, 1, 57), "...")

  # State: highest-revenue observed casino (matches run_analysis.R and forecast_new_site.R)
  if (has_rev) {
    obs_mem <- mem_rev[mem_rev$total_revenue_2022 > 0, ]
    mkt_st <- obs_mem$state[which.max(obs_mem$total_revenue_2022)]
  } else {
    mkt_st <- casinos_study$state[idx[1]]
  }

  markets$market_id[k] <- mid
  markets$n_casinos[k] <- length(idx)
  markets$latitude[k] <- lat
  markets$longitude[k] <- lon
  markets$has_hotel[k] <- as.integer(any(casinos_study$has_hotel[idx] == 1, na.rm = TRUE))
  markets$has_tables[k] <- as.integer(any(casinos_study$has_tables[idx] == 1, na.rm = TRUE))
  # Market is_cardroom: 1 only if ALL casinos in cluster are cardrooms
  markets$is_cardroom[k] <- as.integer(all(casinos_study$is_cardroom[idx] == 1))
  markets$state[k] <- mkt_st
  markets$label[k] <- lbl
  markets$observed[k] <- FALSE
  markets$revenue[k] <- 0
  markets$has_commercial[k] <- as.integer(any(casinos_study$tribal[idx] == "Commercial"))

  # Commercial attractiveness share within cluster
  # A_i = exp(alpha_H * hotel_i + alpha_T * tables_i) per casino
  cas_attract <- exp(0.232 * casinos_study$has_hotel[idx] + 0.418 * casinos_study$has_tables[idx])
  comm_mask <- casinos_study$tribal[idx] == "Commercial"
  total_attract <- sum(cas_attract)
  comm_attract  <- sum(cas_attract[comm_mask])
  markets$commercial_share[k] <- if (total_attract > 0) comm_attract / total_attract else 0
}

cat("  Markets:", n_mkts, "\n")

# ---- Compute baseline distance matrix ----
cat("  Computing ZIP-to-market distance matrix...\n")
n_zips <- nrow(zips_study)
dist_matrix <- matrix(NA_real_, nrow = n_zips, ncol = n_mkts)
for (j in 1:n_mkts) {
  dist_matrix[, j] <- haversine(zips_study$longitude, zips_study$latitude,
                                markets$longitude[j], markets$latitude[j])
}
cat("  Distance matrix:", n_zips, "x", n_mkts, "\n")

# ---- Allocate AGA state revenue to markets using gravity model ----
cat("  Allocating AGA state revenue to markets...\n")

decay_power <- function(d, beta) (d + 0.1)^(-beta)

cg_beta    <- 2.6738
cg_a_hotel <- 0.207
cg_a_table <- 0.452
MAX_DIST   <- 150

# Compute baseline gravity demand indices for all markets
dw <- decay_power(dist_matrix, cg_beta)
dw[dist_matrix > MAX_DIST] <- 0
attract <- exp(cg_a_hotel * markets$has_hotel + cg_a_table * markets$has_tables)
dw_a <- sweep(dw, 2, attract, "*")
denom <- rowSums(dw_a, na.rm = TRUE)
denom[denom == 0] <- 1
probs <- sweep(dw_a, 1, denom, "/")
demand_flow <- sweep(probs, 1, pop_income, "*")
demand_idx <- colSums(demand_flow, na.rm = TRUE)

# States with AGA revenue data
observed_states <- aga_revenue$state_abbrev

markets$tribal_pseudo <- 0  # pseudo-allocated tribal revenue (additive, beyond AGA total)

for (i in 1:nrow(aga_revenue)) {
  st <- aga_revenue$state_abbrev[i]
  st_rev <- aga_revenue$land_based_casino_rev_M[i] * 1e6  # convert to dollars

  # All clusters in this state
  st_mask_all <- which(markets$state == st)
  if (length(st_mask_all) == 0) next

  st_demand <- demand_idx[st_mask_all]
  st_comm_frac <- markets$commercial_share[st_mask_all]

  # Step 1: Commercial allocation — normalized so sum = state GGR.
  # Each cluster's commercial demand = demand × commercial_share.
  # The full AGA total is distributed proportionally to commercial demand.
  st_commercial_demand <- st_demand * st_comm_frac
  total_commercial_demand <- sum(st_commercial_demand)
  if (total_commercial_demand == 0) next

  # Revenue per unit of demand (commercial calibration rate for this state)
  rev_per_demand <- st_rev / total_commercial_demand

  for (j in seq_along(st_mask_all)) {
    idx <- st_mask_all[j]
    comm_frac <- st_comm_frac[j]

    # Commercial allocation (only for clusters with commercial casinos)
    if (comm_frac > 0) {
      markets$revenue[idx]  <- st_commercial_demand[j] * rev_per_demand
      markets$observed[idx] <- TRUE
    }

    # Step 2: Tribal pseudo-estimate — additional revenue beyond AGA total,
    # derived using the same revenue-per-demand rate from commercial calibration.
    tribal_demand <- st_demand[j] * (1 - comm_frac)
    if (tribal_demand > 0) {
      markets$tribal_pseudo[idx] <- tribal_demand * rev_per_demand
    }
  }
}

# ---- Allocate CT property-level revenue directly to markets ----
# CT is all tribal (not in AGA), so the AGA loop above doesn't touch CT markets.
# We have property-level revenue from the 10-state estimation data.
ct_rev <- revenue_data[revenue_data$state == "CT", ]
if (nrow(ct_rev) > 0) {
  ct_casinos <- casinos_study[casinos_study$casino_id %in% ct_rev$casino_id, ]
  ct_market_ids <- unique(ct_casinos$market_id)
  for (mid in ct_market_ids) {
    mkt_idx <- which(markets$market_id == mid)
    if (length(mkt_idx) == 0) next
    cas_in_mkt <- casinos_study[casinos_study$market_id == mid, ]
    mkt_rev <- sum(ct_rev$total_revenue_2022[ct_rev$casino_id %in% cas_in_mkt$casino_id], na.rm = TRUE)
    if (mkt_rev > 0) {
      markets$revenue[mkt_idx] <- mkt_rev
      markets$observed[mkt_idx] <- TRUE
    }
  }
  cat("  CT property-level revenue allocated to", length(ct_market_ids), "markets: $",
      round(sum(ct_rev$total_revenue_2022, na.rm = TRUE) / 1e6), "M\n")
}

# ---- Allocate WA cardroom property-level revenue directly to markets ----
# WA cardrooms have observed revenue from WSGC; override any AGA-based allocation.
wa_rev <- revenue_data[revenue_data$state == "WA", ]
if (nrow(wa_rev) > 0) {
  wa_casinos <- casinos_study[casinos_study$casino_id %in% wa_rev$casino_id, ]
  wa_market_ids <- unique(wa_casinos$market_id)
  for (mid in wa_market_ids) {
    mkt_idx <- which(markets$market_id == mid)
    if (length(mkt_idx) == 0) next
    cas_in_mkt <- casinos_study[casinos_study$market_id == mid, ]
    mkt_rev <- sum(wa_rev$total_revenue_2022[wa_rev$casino_id %in% cas_in_mkt$casino_id], na.rm = TRUE)
    if (mkt_rev > 0) {
      markets$revenue[mkt_idx] <- mkt_rev
      markets$observed[mkt_idx] <- TRUE
    }
  }
  cat("  WA cardroom property-level revenue allocated to", length(wa_market_ids), "markets: $",
      round(sum(wa_rev$total_revenue_2022, na.rm = TRUE) / 1e6), "M\n")
}

# Summary
comm_alloc_total <- sum(markets$revenue[markets$state %in% observed_states])
tribal_pseudo_total <- sum(markets$tribal_pseudo)
cat("  Commercial allocation: $", round(comm_alloc_total / 1e9, 2), "B",
    "(should = AGA total $", round(sum(aga_revenue$land_based_casino_rev_M) / 1000, 2), "B)\n")
cat("  Tribal pseudo-estimate: $", round(tribal_pseudo_total / 1e9, 2), "B (additional)\n")

cat("  Observed markets:", sum(markets$observed), "across",
    length(unique(markets$state[markets$observed])), "states\n")
cat("  Total allocated revenue: $", round(sum(markets$revenue) / 1e9, 1), "B\n")

# ---- State revenue lookup table ----
state_revenue <- data.frame(
  state = aga_revenue$state_abbrev,
  revenue = aga_revenue$land_based_casino_rev_M * 1e6,
  stringsAsFactors = FALSE
)

# Add CT from property-level data (CT is tribal gaming, not in AGA report)
if (!"CT" %in% state_revenue$state) {
  ct_total <- sum(revenue_data$total_revenue_2022[revenue_data$state == "CT"], na.rm = TRUE)
  if (ct_total > 0) {
    state_revenue <- rbind(state_revenue,
                           data.frame(state = "CT", revenue = ct_total, stringsAsFactors = FALSE))
    cat("  Added CT to state_revenue from property-level data: $",
        round(ct_total / 1e6), "M\n")
  }
}

# Add/update WA from property-level cardroom data
wa_total <- sum(revenue_data$total_revenue_2022[revenue_data$state == "WA"], na.rm = TRUE)
if (wa_total > 0) {
  if ("WA" %in% state_revenue$state) {
    state_revenue$revenue[state_revenue$state == "WA"] <- wa_total
    cat("  Updated WA state_revenue from property-level cardroom data: $",
        round(wa_total / 1e6), "M\n")
  } else {
    state_revenue <- rbind(state_revenue,
                           data.frame(state = "WA", revenue = wa_total, stringsAsFactors = FALSE))
    cat("  Added WA to state_revenue from property-level cardroom data: $",
        round(wa_total / 1e6), "M\n")
  }
}

# ---- Model parameters ----
# From 10-state CG model (power decay) with is_cardroom indicator
model_params <- list(
  cg_beta      = 2.6738,
  cg_a_hotel   = 0.207,
  cg_a_table   = 0.452,
  cg_intercept = 5.624,
  cg_gamma     = 0.995,
  cg_cardroom_delta = -1.851,
  MAX_DIST     = 150,
  # Duan (1983) smearing estimator for retransformation bias correction.
  # When predicting from ln(Rev) = intercept + gamma*ln(D) + delta*is_cardroom,
  # the correct retransformation is E[Rev] = exp(X'B) * duan_smear.
  # Computed from OLS residuals of the 10-state CG model (N=104 markets).
  # For calibrated states this cancels out (same factor in numerator/denominator),
  # but matters for uncalibrated states where raw model predictions are used.
  cg_duan_smear = 1.2833
)

# ---- Save bundle ----
forecast_bundle <- list(
  allzips_map    = allzips_map,
  zips_study     = zips_study,
  pop_income     = pop_income,
  casinos_study  = casinos_study,
  markets        = markets,
  dist_matrix    = dist_matrix,
  state_revenue  = state_revenue,
  model_params   = model_params,
  state_abbrev   = state_abbrev
)

output_path <- "forecast_app/forecast_data.rds"
saveRDS(forecast_bundle, output_path)
cat("\nSaved forecast data bundle to:", output_path, "\n")
cat("File size:", round(file.info(output_path)$size / 1e6, 1), "MB\n")
cat("Done.\n")
