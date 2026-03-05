# =============================================================================
# Distance Decay Estimation: Market-Level Analysis with Supply-Side Controls
# =============================================================================
# This script estimates casino demand distance decay functions using
# property-level revenue data from 16 states (PA, OH, MD, NY, MA, CT, IN,
# MO, IA, WA, IL, NJ, LA, MI, ME, KS) with
# ZIP-level demographics.
#
# Key methodological choices:
#   1. Nearby casinos are clustered into "markets" so that co-located
#      properties (e.g. two Philadelphia casinos) compete as a unit.
#   2. Supply-side attractiveness controls: three specifications estimated.
#      Binary: A_j = exp(alpha_hotel * hotel_j + alpha_tables * tables_j)
#      Rooms:  A_j = rooms_j^alpha_R * exp(alpha_tables * tables_j)
#              = exp(alpha_R * ln(rooms) * I(rooms>0) + alpha_T * tables)
#      Dest:   A_j = exp(alpha_H * hotel + alpha_D * dest_resort + alpha_T * tables)
#              where dest_resort = 1 if >=1500 rooms OR >=500 rooms in cluster/tourist city
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
cat("  21-State Model with Supply-Side Attractiveness Controls\n")
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

# Load revenue data -- 10 study states (2024 data)
pa_revenue <- read.csv("data/pa_revenue_2024.csv")
oh_revenue <- read.csv("data/oh_revenue_2024.csv")
md_revenue <- read.csv("data/md_revenue_2024.csv")
ny_revenue <- read.csv("data/ny_revenue_2024.csv")
ma_revenue <- read.csv("data/ma_revenue_2024.csv")
ct_revenue <- read.csv("data/ct_revenue_2024.csv")
in_revenue <- read.csv("data/in_revenue_2024.csv")
mo_revenue <- read.csv("data/mo_revenue_2024.csv")
ia_revenue <- read.csv("data/ia_revenue_2024.csv")
wa_revenue <- read.csv("data/wa_revenue_2024.csv")

# Rename _2024 columns to _2022 so all downstream code works unchanged
# (the internal column name is a legacy convention; all data is now 2024$)
study_state_dfs <- list(pa_revenue, oh_revenue, md_revenue, ny_revenue,
                        ma_revenue, ct_revenue, in_revenue, mo_revenue,
                        ia_revenue, wa_revenue)
for (k in seq_along(study_state_dfs)) {
  df <- study_state_dfs[[k]]
  names(df)[names(df) == "total_revenue_2024"]  <- "total_revenue_2022"
  names(df)[names(df) == "table_revenue_2024"]  <- "table_revenue_2022"
  names(df)[names(df) == "slots_revenue_2024"]  <- "slots_revenue_2022"
  study_state_dfs[[k]] <- df
}
pa_revenue <- study_state_dfs[[1]];  oh_revenue <- study_state_dfs[[2]]
md_revenue <- study_state_dfs[[3]];  ny_revenue <- study_state_dfs[[4]]
ma_revenue <- study_state_dfs[[5]];  ct_revenue <- study_state_dfs[[6]]
in_revenue <- study_state_dfs[[7]];  mo_revenue <- study_state_dfs[[8]]
ia_revenue <- study_state_dfs[[9]];  wa_revenue <- study_state_dfs[[10]]

cat("  Loaded", nrow(pa_revenue), "PA properties with revenue (2024)\n")
cat("  Loaded", nrow(oh_revenue), "OH properties with revenue (2024)\n")
cat("  Loaded", nrow(md_revenue), "MD properties with revenue (2024)\n")
cat("  Loaded", nrow(ny_revenue), "NY properties with revenue (2024)\n")
cat("  Loaded", nrow(ma_revenue), "MA properties with revenue (2024)\n")
cat("  Loaded", nrow(ct_revenue), "CT properties with revenue (2024)\n")
cat("  Loaded", nrow(in_revenue), "IN properties with revenue (2024)\n")
cat("  Loaded", nrow(mo_revenue), "MO properties with revenue (2024)\n")
cat("  Loaded", nrow(ia_revenue), "IA properties with revenue (2024)\n")
cat("  Loaded", nrow(wa_revenue), "WA properties with revenue (2024)\n")

# Load revenue data -- 6 override states with actual government property-level GGR
# Only states where the regulator publishes property-specific revenue.
# Excluded: MS, AR, DE, RI, WV (only have estimated/allocated splits, not actual).
il_revenue <- read.csv("data/il_revenue.csv")
nj_revenue <- read.csv("data/nj_revenue.csv")
la_revenue <- read.csv("data/la_revenue.csv")
mi_revenue <- read.csv("data/mi_revenue.csv")
me_revenue <- read.csv("data/me_revenue.csv")
ks_revenue <- read.csv("data/ks_revenue.csv")

cat("  Loaded", nrow(il_revenue), "IL properties with revenue (2024)\n")
cat("  Loaded", nrow(nj_revenue), "NJ properties with revenue (2024)\n")
cat("  Loaded", nrow(la_revenue), "LA properties with revenue (2024)\n")
cat("  Loaded", nrow(mi_revenue), "MI properties with revenue (2024)\n")
cat("  Loaded", nrow(me_revenue), "ME properties with revenue (2024)\n")
cat("  Loaded", nrow(ks_revenue), "KS properties with revenue (2024)\n")

# --- Harmonize column names for override states ---
# Override CSVs use unsuffixed names; study CSVs use _2022 suffix.
# Rename override columns to match so rbind and downstream code works.
override_state_dfs <- list(il_revenue, nj_revenue, la_revenue, mi_revenue,
                           me_revenue, ks_revenue)
for (k in seq_along(override_state_dfs)) {
  df <- override_state_dfs[[k]]
  names(df)[names(df) == "total_revenue"]  <- "total_revenue_2022"
  names(df)[names(df) == "table_revenue"]  <- "table_revenue_2022"
  names(df)[names(df) == "slots_revenue"]  <- "slots_revenue_2022"
  override_state_dfs[[k]] <- df
}
il_revenue <- override_state_dfs[[1]];  nj_revenue <- override_state_dfs[[2]]
la_revenue <- override_state_dfs[[3]];  mi_revenue <- override_state_dfs[[4]]
me_revenue <- override_state_dfs[[5]];  ks_revenue <- override_state_dfs[[6]]

# All 16 states now use 2024$ revenue data -- no CPI inflation needed
cat("\n  All revenue data is in 2024$ (no inflation adjustment required)\n")

# --- CT Table Revenue Scaling ---
# CT only reports slot win. We scale using Mohegan Sun's table revenue share
# from Mohegan Tribal Gaming Authority's FY2024 earnings decks and SEC filings.
# FY24 table share: 32.2%. Average of FY22 (31.8%), FY23 (31.0%), FY24 (32.2%) = 31.7%.
# Using FY24 value for consistency with 2024$ data vintage.
# Formula: total_gaming = slot_revenue / (1 - table_share)
# Source: Mohegan Tribal Gaming Authority Supplemental Earnings Decks + 10-K/10-Q (SEC)
# NOTE: This Mohegan-derived share is applied to both Mohegan Sun and Foxwoods;
# Foxwoods may have a different table/slot mix but does not publicly report it.
ct_table_share <- 0.322  # table revenue as share of (slot + table) revenue
cat("\n  CT table share (Mohegan Sun FY24):", ct_table_share, "\n")
ct_revenue$table_revenue_2022 <- round(ct_revenue$slots_revenue_2022 * ct_table_share / (1 - ct_table_share))
ct_revenue$total_revenue_2022 <- ct_revenue$slots_revenue_2022 + ct_revenue$table_revenue_2022
cat("  Mohegan Sun CT scaled total: $", format(ct_revenue$total_revenue_2022[ct_revenue$casino_id == 905], big.mark = ","), "\n")
cat("  Foxwoods CT scaled total: $", format(ct_revenue$total_revenue_2022[ct_revenue$casino_id == 497], big.mark = ","), "\n")

# Combine revenue data (16 states with actual property-level GGR, all in 2024$)
# Use common columns for rbind (override CSVs have extra 'revenue_year' column)
common_cols <- c("casino_id", "name", "city", "state", "facility_type",
                 "slots_revenue_2022", "table_revenue_2022", "total_revenue_2022",
                 "num_slots", "num_tables", "source")

# Ensure all dataframes have the common columns (fill missing with NA)
safe_select <- function(df) {
  for (col in common_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df[, common_cols, drop = FALSE]
}

revenue_data <- rbind(
  safe_select(pa_revenue), safe_select(oh_revenue), safe_select(md_revenue),
  safe_select(ny_revenue), safe_select(ma_revenue), safe_select(ct_revenue),
  safe_select(in_revenue), safe_select(mo_revenue), safe_select(ia_revenue),
  safe_select(wa_revenue),
  safe_select(il_revenue), safe_select(nj_revenue), safe_select(la_revenue),
  safe_select(mi_revenue), safe_select(me_revenue), safe_select(ks_revenue)
)

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

# Drop rows with NA revenue (tribal casinos in override states with no public data)
n_na_rev <- sum(is.na(revenue_data$total_revenue_2022))
if (n_na_rev > 0) {
  cat("\n  Dropping", n_na_rev, "properties with NA revenue (tribal, no public data):\n")
  for (i in which(is.na(revenue_data$total_revenue_2022))) {
    cat("    -", revenue_data$name[i], "(", revenue_data$city[i], ",", revenue_data$state[i], ")\n")
  }
  revenue_data <- revenue_data[!is.na(revenue_data$total_revenue_2022), ]
}

cat("\nTotal properties with revenue:", nrow(revenue_data), "\n")
cat("Total observed revenue: $", format(round(sum(revenue_data$total_revenue_2022) / 1e9, 2)), " billion\n")

# =============================================================================
# 2. DEFINE STUDY REGION
# =============================================================================

cat("\n--- DEFINING STUDY REGION ---\n")

# Primary states (16) -- states with actual government-reported property-level GGR
# 10 study states + 6 override states with real regulatory data
primary_states <- c("PA", "OH", "MD", "NY", "MA", "CT", "IN", "MO", "IA", "WA",
                    "IL", "NJ", "LA", "MI", "ME", "KS")

# Border states -- neighboring states that contain competing casinos
# and/or demand populations, but for which we don't have observed revenue.
# MS, AR, DE, RI, WV have casinos but only estimated (not actual) property-level data.
border_states <- c("KY", "VA", "DC", "VT", "NH",
                    "WI", "MN", "SD", "NE", "OK",
                    "OR", "ID", "CA", "TN", "TX", "AL", "CO",
                    "MS", "AR", "DE", "RI", "WV")
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
  "Washington" = "WA", "Oregon" = "OR", "Idaho" = "ID", "California" = "CA",
  "Wisconsin" = "WI", "Illinois" = "IL", "Minnesota" = "MN",
  "South Dakota" = "SD", "Nebraska" = "NE", "Kansas" = "KS",
  "Arkansas" = "AR", "Tennessee" = "TN", "Oklahoma" = "OK",
  "Louisiana" = "LA", "Mississippi" = "MS", "Maine" = "ME",
  "Texas" = "TX", "Alabama" = "AL", "Colorado" = "CO"
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
  city = as.character(casinos_study$city),
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

# --- Inject properties not in casinodata.rds ---
# These opened after 2022 or are small venues not tracked in the AGA database.

# Caesars Southern Indiana (formerly Horseshoe Southern IN)
# Located at 11999 Casino Center Dr SE, Elizabeth, IN 47117.
# Has hotel (503 rooms) and table games (~70 tables).
caesars_si <- data.frame(
  casino_id = 9999,
  name = "Caesars Southern Indiana",
  city = "Elizabeth",
  state = "IN",
  latitude = 38.1796,
  longitude = -85.9054,
  hotel = 1,
  stringsAsFactors = FALSE
)

# Terre Haute Casino (opened 2024, IN)
terre_haute <- data.frame(
  casino_id = 9998,
  name = "Terre Haute Casino",
  city = "Terre Haute",
  state = "IN",
  latitude = 39.4667,
  longitude = -87.4139,
  hotel = 0,
  stringsAsFactors = FALSE
)

# PA mini-casinos (opened after 2022)
pa_new <- data.frame(
  casino_id = c(9003, 9004, 9005),
  name = c("Hollywood Casino York", "Hollywood Casino Morgantown", "Parx Casino Shippensburg"),
  city = c("York", "Morgantown", "Shippensburg"),
  state = rep("PA", 3),
  latitude  = c(39.9526, 40.1551, 40.0512),
  longitude = c(-76.7275, -75.8855, -77.5199),
  hotel = rep(0, 3),
  stringsAsFactors = FALSE
)

# NY new racinos (opened after 2022)
ny_new <- data.frame(
  casino_id = c(9001, 9002),
  name = c("Nassau OTB VGM Facility", "Resorts World Hudson Valley"),
  city = c("Plainview", "Newburgh"),
  state = rep("NY", 2),
  latitude  = c(40.7770, 41.5034),
  longitude = c(-73.4674, -74.0104),
  hotel = rep(0, 2),
  stringsAsFactors = FALSE
)

casinos_study <- rbind(casinos_study, caesars_si, terre_haute, pa_new, ny_new)
cat("  Injected 7 properties not in casinodata.rds (Caesars SI, Terre Haute IN,\n")
cat("    3 PA mini-casinos, 2 NY racinos)\n")

# --- Inject LA racetracks not in casinodata.rds ---
# Delta Downs and Louisiana Downs are major pari-mutuel racetracks with slot
# machines (racinos) reporting revenue to LGCB but not tracked in AGA casinodata.
# Delta Downs: 1,600+ slot machines at 2717 Delta Downs Dr, Vinton, LA 70668
# Louisiana Downs: 800+ slot machines at 8000 E Texas St, Bossier City, LA 71111
la_racetracks <- data.frame(
  casino_id = c(10100, 10101),
  name = c("Delta Downs Racetrack", "Louisiana Downs"),
  city = c("Vinton", "Bossier City"),
  state = c("LA", "LA"),
  latitude  = c(30.1919, 32.5102),
  longitude = c(-93.5810, -93.7127),
  hotel = c(1, 0),
  stringsAsFactors = FALSE
)
casinos_study <- rbind(casinos_study, la_racetracks)
cat("  Injected 2 LA racetracks not in base data (Delta Downs, Louisiana Downs)\n")

# --- Inject WA cardrooms not in casinodata.rds ---
# House-banked card rooms with revenue data from WSGC.
wa_inject <- data.frame(
  casino_id = c(10001, 10004, 10005, 10006, 10007, 10008, 10009, 10010, 10011),
  name = c("Fortune Casino Renton", "New Phoenix",
           "Aces Poker Lakewood", "Caribbean Cardroom",
           "Clearwater Saloon and Casino", "Imperial Palace Tukwila",
           "Dragon Tiger Casino", "Maverick Casino", "Grand Casino"),
  city = c("Renton", "Tukwila", "Lakewood", "Kirkland", "Ellensburg",
           "Tukwila", "Tukwila", "Lakewood", "Lakewood"),
  state = rep("WA", 9),
  latitude  = c(47.4720, 45.8620, 47.1718, 47.6769, 47.4240,
                47.4740, 47.4740, 47.1718, 47.1718),
  longitude = c(-122.2060, -122.6710, -122.5185, -122.1780, -120.3100,
                -122.2610, -122.2610, -122.5185, -122.5185),
  hotel = rep(0, 9),
  stringsAsFactors = FALSE
)
casinos_study <- rbind(casinos_study, wa_inject)
cat("  Injected", nrow(wa_inject), "WA cardrooms not in base data\n")

# --- Fix WA tribal misclassifications in casinodata.rds ---
# These are tagged "Commercial" but are actually tribal casinos.
wa_tribal_misclass <- c(943, 1117, 12)  # Muckleshoot, Quil Ceda Creek, 7 Cedars
cat("  Fixed", length(wa_tribal_misclass), "WA tribal misclassifications (ids:",
    paste(wa_tribal_misclass, collapse=", "), ")\n")

cat("Study region casinos:", nrow(casinos_study), "\n")

# =============================================================================
# 3. SUPPLY-SIDE CONTROLS: has_hotel, hotel_rooms, AND has_tables
# =============================================================================

cat("\n--- BUILDING SUPPLY-SIDE CONTROLS ---\n")

# --- Hotel room counts (continuous) from World Casino Directory scrape ---
# Loaded from verified wcd_hotel_rooms.csv (100% web-verified, 6 correction rounds)
hotel_rooms_data <- read.csv("data/wcd_hotel_rooms.csv", stringsAsFactors = FALSE)
hotel_rooms_data <- hotel_rooms_data[, c("casino_id", "hotel_rooms")]
hotel_rooms_data$hotel_rooms[is.na(hotel_rooms_data$hotel_rooms)] <- 0
casinos_study <- merge(casinos_study, hotel_rooms_data, by = "casino_id", all.x = TRUE)
casinos_study$hotel_rooms[is.na(casinos_study$hotel_rooms)] <- 0

# Assign room counts for injected casinos not in wcd_hotel_rooms.csv
casinos_study$hotel_rooms[casinos_study$casino_id == 9999] <- 503   # Caesars Southern Indiana
casinos_study$hotel_rooms[casinos_study$casino_id == 9998] <- 0     # Terre Haute (no hotel)
casinos_study$hotel_rooms[casinos_study$casino_id %in% c(9003, 9004, 9005)] <- 0  # PA mini-casinos
casinos_study$hotel_rooms[casinos_study$casino_id %in% c(9001, 9002)] <- 0  # NY racinos
casinos_study$hotel_rooms[casinos_study$casino_id == 10100] <- 207  # Delta Downs (207-room hotel)
casinos_study$hotel_rooms[casinos_study$casino_id == 10101] <- 0    # Louisiana Downs (no hotel)
casinos_study$hotel_rooms[casinos_study$casino_id %in% c(10001, 10004:10011)] <- 0  # WA cardrooms

# Compute ln(hotel_rooms) for power-function specification
# ln(rooms) when rooms > 0, 0 when rooms = 0 (so exp(a_rooms * 0) = 1)
casinos_study$ln_hotel_rooms <- ifelse(casinos_study$hotel_rooms > 0,
                                        log(casinos_study$hotel_rooms), 0)

cat("  Loaded hotel room counts:", sum(casinos_study$hotel_rooms > 0),
    "casinos with rooms (range:", min(casinos_study$hotel_rooms[casinos_study$hotel_rooms > 0]),
    "-", max(casinos_study$hotel_rooms), ")\n")

# has_hotel: already loaded from casinodata, then override verified corrections.
# The casinodata.rds hotel field has known inaccuracies for our 2024 study period.
casinos_study$has_hotel <- casinos_study$hotel

# --- Hotel overrides (verified against 2024 operational status) ---
# Change hotel=1 → 0 (no on-site hotel during 2024):
casinos_study$has_hotel[casinos_study$casino_id == 1060] <- 0  # Presque Isle Downs, Erie PA — no on-site hotel

# Change hotel=0 → 1 (on-site hotel operational during 2024):
casinos_study$has_hotel[casinos_study$casino_id == 1167] <- 1  # Rivers Casino Schenectady NY — Landing Hotel (2017)
casinos_study$has_hotel[casinos_study$casino_id == 83]   <- 1  # Batavia Downs Gaming NY — Hotel (2016)
casinos_study$has_hotel[casinos_study$casino_id == 1162] <- 1  # River City Casino MO — 200-room hotel (2013)
casinos_study$has_hotel[casinos_study$casino_id == 557]  <- 1  # Grand Falls Casino IA — 163-room hotel (2011)
casinos_study$has_hotel[casinos_study$casino_id == 1155] <- 1  # Rhythm City Casino IA — 106-room hotel (2016)
casinos_study$has_hotel[casinos_study$casino_id == 1170] <- 1  # Riverside Casino IA — 201-room hotel (2006)
casinos_study$has_hotel[casinos_study$casino_id == 366]  <- 1  # Diamond Jo Worth IA — 102-room hotel (2006)
casinos_study$has_hotel[casinos_study$casino_id == 1434] <- 1  # Wild Rose Clinton IA — 60-room hotel
casinos_study$has_hotel[casinos_study$casino_id == 1435] <- 1  # Wild Rose Emmetsburg IA — 70-room hotel
casinos_study$has_hotel[casinos_study$casino_id == 865]  <- 1  # Hard Rock Northern IN — hotel opened 2024
cat("  Applied 11 hotel overrides (1 set to 0, 10 set to 1)\n")

# --- Consistency check: has_hotel vs hotel_rooms ---
inconsistent <- casinos_study$has_hotel == 1 & casinos_study$hotel_rooms == 0
n_incon <- sum(inconsistent, na.rm = TRUE)
if (n_incon > 0) {
  cat("  WARNING:", n_incon, "casinos have has_hotel=1 but hotel_rooms=0:\n")
  incon_df <- casinos_study[which(inconsistent), c("casino_id", "name", "state", "has_hotel", "hotel_rooms")]
  for (i in 1:nrow(incon_df)) {
    cat("    ", incon_df$casino_id[i], "-", incon_df$name[i], "(", incon_df$state[i], ")\n")
  }
} else {
  cat("  Consistency check passed: all has_hotel=1 casinos have hotel_rooms > 0\n")
}

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

# NY racinos (VGM/VLT facilities) are slots-only
ny_racino_ids <- ny_revenue$casino_id[ny_revenue$facility_type == "racino"]
slots_only_ids <- c(slots_only_ids, ny_racino_ids)

# Plainridge Park Casino MA is a slots-only facility (racino)
plainridge_id <- ma_revenue$casino_id[grepl("Plainridge", ma_revenue$name, ignore.case = TRUE)]
slots_only_ids <- c(slots_only_ids, plainridge_id)

# LA racetracks are slots-only (racinos with pari-mutuel + slot machines, no tables)
la_racino_ids <- la_revenue$casino_id[la_revenue$facility_type == "racino"]
slots_only_ids <- c(slots_only_ids, la_racino_ids)

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

# is_cardroom: Identifies venues that operate only table/card games without
# slot machines. In WA and CA, state law prohibits commercial (non-tribal)
# casinos from operating slot machines — all commercial venues are cardrooms.
casinos_study$is_cardroom <- 0

# WA cardrooms: use revenue data IDs as authoritative (avoids tribal misclassification)
wa_cardroom_ids <- wa_revenue$casino_id
casinos_study$is_cardroom[casinos_study$casino_id %in% wa_cardroom_ids] <- 1

# Remaining WA/CA commercial venues not in revenue data are also cardrooms
# (unobserved supply controls). Exclude known misclassified tribals.
wa_tribal_misclass <- c(943, 1117, 12)  # Muckleshoot, Quil Ceda Creek, 7 Cedars
for (st in c("WA", "CA")) {
  st_unflagged <- casinos_study$state == st & casinos_study$is_cardroom == 0
  # Only flag if NOT a known tribal casino
  is_tribal <- casinos_study$casino_id %in% wa_tribal_misclass
  # Use casinodata tribal field — but it's not in casinos_study anymore.
  # Check if original casinodata has this venue as Tribal
  orig_tribal <- casinodata$tribal[match(casinos_study$casino_id, casinodata$id)]
  is_real_tribal <- !is.na(orig_tribal) & orig_tribal == "Tribal"
  is_real_tribal <- is_real_tribal | is_tribal  # also exclude known misclassifications
  casinos_study$is_cardroom[st_unflagged & !is_real_tribal] <- 1
}

n_cardroom <- sum(casinos_study$is_cardroom == 1, na.rm = TRUE)
cat("  Cardrooms (is_cardroom=1):", n_cardroom, "\n")
cat("    WA:", sum(casinos_study$is_cardroom == 1 & casinos_study$state == "WA"), "\n")
cat("    CA:", sum(casinos_study$is_cardroom == 1 & casinos_study$state == "CA"), "\n")

# =============================================================================
# 3b. CONVENIENCE GAMING FILTER
# =============================================================================
# Flag properties that are not actual casinos/cardrooms but rather bars,
# restaurants, gas stations, truck stops, bowling alleys, or other convenience
# gaming locations that happen to have a few gaming machines or tables.
# These distort the gravity model because:
#   (a) They inflate competitive supply in their area
#   (b) AGA state GGR figures exclude convenience gaming revenue, so
#       allocating AGA totals across convenience venues is incorrect
#
# Methodology: state-specific rules + name-pattern matching.
# Properties are flagged (is_convenience=1) and excluded from the model.

cat("\n--- CONVENIENCE GAMING FILTER ---\n")
casinos_study$is_convenience <- 0

# --- Oklahoma: "Gasinos" (gas station gaming), Travel Plazas, Trading Posts ---
# Oklahoma tribal gaming includes many small convenience-gaming locations:
# gas stations with gaming machines ("Gasinos"), travel plazas/truck stops
# with gaming, and trading posts with a few machines.
# These are NOT traditional casinos and are not included in AGA commercial GGR.
ok_convenience_patterns <- "(?i)(gasino|travel plaza|travel stop|travel center|trading post)"
ok_mask <- casinos_study$state == "OK" &
  grepl(ok_convenience_patterns, casinos_study$name, perl = TRUE)
casinos_study$is_convenience[ok_mask] <- 1

# Also flag OK "Gaming Center" properties that are just small slot parlors
# (but NOT properties with "Casino" in their name, which are larger operations)
ok_gaming_center <- casinos_study$state == "OK" &
  grepl("Gaming Center", casinos_study$name, ignore.case = TRUE) &
  !grepl("Casino", casinos_study$name, ignore.case = TRUE)
casinos_study$is_convenience[ok_gaming_center] <- 1

cat("  OK convenience:", sum(casinos_study$is_convenience == 1 & casinos_study$state == "OK"),
    "properties flagged\n")
if (any(ok_mask | ok_gaming_center)) {
  flagged_ok <- casinos_study$name[casinos_study$state == "OK" & casinos_study$is_convenience == 1]
  cat("    ", paste(flagged_ok, collapse = "\n     "), "\n")
}

# --- California: Yokut Gas Station is a gas station, not a casino ---
casinos_study$is_convenience[casinos_study$casino_id == 1468] <- 1  # Yokut Gas Station
# Bear River Pump & Play is a gas station/convenience store with a few machines
casinos_study$is_convenience[casinos_study$casino_id == 89] <- 1    # Bear River Pump & Play
cat("  CA convenience:", sum(casinos_study$is_convenience == 1 & casinos_study$state == "CA"),
    "properties flagged\n")

# --- New Hampshire: All properties are charitable gaming operations ---
# NH has no commercial casinos. Its 8 "casinos" are small charitable gaming
# venues (billiard clubs, poker rooms, sports bars) authorized under NH law.
# These are not recognized by the AGA as commercial casinos and generate
# minimal gaming revenue. Flagging all NH properties.
nh_mask <- casinos_study$state == "NH"
casinos_study$is_convenience[nh_mask] <- 1
cat("  NH convenience:", sum(nh_mask), "properties flagged (all charitable gaming)\n")

# --- Pennsylvania: Hollywood Casino Off Track Betting ---
# ID 639 is an OTB/satellite betting parlor, not a full casino.
casinos_study$is_convenience[casinos_study$casino_id == 639] <- 1
cat("  PA convenience: 1 property flagged (Hollywood Casino OTB)\n")

# --- Louisiana: Cash Magic, Truck Plazas, and small bar/convenience gaming ---
# Louisiana has many small gaming venues (truck stops, gas stations, bars) with
# video poker terminals. These are tracked in AGA casinodata but are NOT
# reported in LGCB/LSP monthly gaming revenue reports (those only cover
# riverboats, land-based, and racetracks). Flag as convenience.
la_convenience_patterns <- "(?i)(Cash Magic|Truck Plaza|Lucky Longhorn|Lucky Magnolia|Poker Palace|Forest Gold|Quarters Travel|Carnival Club|Crescent City)"
la_conv_mask <- casinos_study$state == "LA" &
  grepl(la_convenience_patterns, casinos_study$name, perl = TRUE)
casinos_study$is_convenience[la_conv_mask] <- 1
# Also flag Amite Truck Plaza by ID
casinos_study$is_convenience[casinos_study$casino_id == 45] <- 1
n_la_conv <- sum(casinos_study$is_convenience == 1 & casinos_study$state == "LA")
cat("  LA convenience:", n_la_conv, "properties flagged (Cash Magic, truck plazas, etc.)\n")

# --- Minnesota: Small cardroom operations at racetracks ---
# Canterbury Park and Running Aces are primarily racetracks with small card rooms.
# They are not traditional casinos. However, they do draw some gaming demand,
# so we keep them in the model as supply competitors.

# --- Summary ---
n_convenience <- sum(casinos_study$is_convenience == 1)
cat("  TOTAL convenience flagged:", n_convenience, "properties\n")

# Remove convenience gaming properties from the study dataset
casinos_study <- casinos_study[casinos_study$is_convenience == 0, ]
cat("  Casinos after removing convenience gaming:", nrow(casinos_study), "\n")

# --- Destination Resort Flag ---
# Rule 1: >= 1,500 rooms (independent mega-resort)
# Rule 2: >= 500 rooms in major cluster or urban tourist destination
dest_tourist <- list(
  list(city = "Atlantic City", state = "NJ"),
  list(city = "New Orleans", state = "LA"),
  list(city = c("Boston","Everett"), state = "MA"),
  list(city = "Niagara Falls", state = "NY"),
  list(city = c("Biloxi","Gulfport","Bay Saint Louis"), state = "MS")
)
dest_cluster <- list(
  list(city = c("Lake Charles","Westlake"), state = "LA"),
  list(city = c("Bossier City","Shreveport"), state = "LA"),
  list(city = c("Tunica","Lula"), state = "MS"),
  list(city = c("Mashantucket","Uncasville","Ledyard","Montville"), state = "CT")
)
all_dest <- c(dest_tourist, dest_cluster)

in_dest_fn <- function(city, state) {
  for (d in all_dest) {
    if (state %in% d$state & city %in% d$city) return(TRUE)
  }
  return(FALSE)
}

rule1 <- casinos_study$hotel_rooms >= 1500
rule2 <- casinos_study$hotel_rooms >= 500 &
  mapply(in_dest_fn, casinos_study$city, casinos_study$state)
casinos_study$destination_resort <- as.integer(rule1 | rule2)

n_dest <- sum(casinos_study$destination_resort == 1)
cat("  Destination resorts flagged:", n_dest, "of", nrow(casinos_study), "casinos\n")
if (n_dest > 0) {
  dest_props <- casinos_study[casinos_study$destination_resort == 1, ]
  dest_props <- dest_props[order(-dest_props$hotel_rooms), ]
  for (i in 1:nrow(dest_props)) {
    cat(sprintf("    - %s (%s, %s) %d rooms\n",
                dest_props$name[i], dest_props$city[i], dest_props$state[i],
                dest_props$hotel_rooms[i]))
  }
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
    mkt_hotel <- mkt_tables <- mkt_cardroom <- integer(nm)
    mkt_hotel_rooms <- numeric(nm)  # continuous hotel rooms (sum across cluster)
    mkt_dest_resort <- integer(nm)  # destination resort flag
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
      mkt_hotel_rooms[k] <- sum(casinos_df$hotel_rooms[idx], na.rm = TRUE)
      mkt_tables[k] <- as.integer(any(casinos_df$has_tables[idx] == 1, na.rm = TRUE))
      # is_cardroom: 1 only if ALL casinos in cluster are cardrooms
      mkt_cardroom[k] <- as.integer(all(casinos_df$is_cardroom[idx] == 1))
      # destination_resort: use casinos_df flag if present, else derive from rules
      if ("destination_resort" %in% names(casinos_df)) {
        mkt_dest_resort[k] <- as.integer(any(casinos_df$destination_resort[idx] == 1, na.rm = TRUE))
      } else {
        mkt_dest_resort[k] <- 0L
      }

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

    # Pre-compute distance mask for sparse decay computation
    # (only ~10-15% of ZIP-market pairs are within 150 miles)
    dist_mask <- dist_mat <= 150
    dist_vals <- dist_mat[dist_mask]

    obs_rev <- mkt_rev[mkt_obs]
    obs_idx <- which(mkt_obs)
    obs_state <- mkt_state[mkt_obs]

    # Pre-compute within-state actual shares and index groups
    obs_ws_shares <- numeric(length(obs_rev))
    state_groups <- split(seq_along(obs_state), obs_state)
    for (idx in state_groups) {
      obs_ws_shares[idx] <- obs_rev[idx] / sum(obs_rev[idx])
    }

    # Compute ln(hotel_rooms) for power-function specification
    mkt_ln_hotel_rooms <- ifelse(mkt_hotel_rooms > 0, log(mkt_hotel_rooms), 0)

    # params = c(beta, a_hotel, a_tables) -- BINARY specification
    # Uses pre-computed dist_mask/dist_vals for sparse decay (only entries within 150mi)
    predict_fn <- function(params, decay_type, max_dist = 150) {
      beta <- params[1]; a_hotel <- params[2]; a_tables <- params[3]
      dw <- matrix(0, nrow = n_z, ncol = nm)
      if (decay_type == "exponential") { dw[dist_mask] <- decay_exponential(dist_vals, beta)
      } else if (decay_type == "power") { dw[dist_mask] <- decay_power(dist_vals, beta)
      } else { dw[dist_mask] <- decay_gaussian(dist_vals, beta) }
      attract <- exp(a_hotel * mkt_hotel + a_tables * mkt_tables)
      dw_a <- sweep(dw, 2, attract, "*")
      denom <- rowSums(dw_a, na.rm = TRUE); denom[denom == 0] <- 1
      ds <- sweep(dw_a, 1, denom, "/")
      df <- sweep(ds, 1, pop_inc, "*")
      pd <- colSums(df, na.rm = TRUE)
      return(pd[obs_idx])
    }

    # params = c(beta, a_rooms, a_tables) -- POWER-FUNCTION (rooms) specification
    # A_j = rooms_j^{a_rooms} * exp(a_tables * T_j) = exp(a_rooms * ln(rooms) + a_tables * T)
    predict_fn_rooms <- function(params, decay_type, max_dist = 150) {
      beta <- params[1]; a_rooms <- params[2]; a_tables <- params[3]
      dw <- matrix(0, nrow = n_z, ncol = nm)
      if (decay_type == "exponential") { dw[dist_mask] <- decay_exponential(dist_vals, beta)
      } else if (decay_type == "power") { dw[dist_mask] <- decay_power(dist_vals, beta)
      } else { dw[dist_mask] <- decay_gaussian(dist_vals, beta) }
      attract <- exp(a_rooms * mkt_ln_hotel_rooms + a_tables * mkt_tables)
      dw_a <- sweep(dw, 2, attract, "*")
      denom <- rowSums(dw_a, na.rm = TRUE); denom[denom == 0] <- 1
      ds <- sweep(dw_a, 1, denom, "/")
      df <- sweep(ds, 1, pop_inc, "*")
      pd <- colSums(df, na.rm = TRUE)
      return(pd[obs_idx])
    }

    # params = c(beta, a_hotel, a_dest, a_tables) -- DESTINATION specification
    # A_j = exp(a_hotel * H + a_dest * dest_resort + a_tables * T)
    predict_fn_dest <- function(params, decay_type, max_dist = 150) {
      beta <- params[1]; a_hotel <- params[2]; a_dest <- params[3]; a_tables <- params[4]
      dw <- matrix(0, nrow = n_z, ncol = nm)
      if (decay_type == "exponential") { dw[dist_mask] <- decay_exponential(dist_vals, beta)
      } else if (decay_type == "power") { dw[dist_mask] <- decay_power(dist_vals, beta)
      } else { dw[dist_mask] <- decay_gaussian(dist_vals, beta) }
      attract <- exp(a_hotel * mkt_hotel + a_dest * mkt_dest_resort + a_tables * mkt_tables)
      dw_a <- sweep(dw, 2, attract, "*")
      denom <- rowSums(dw_a, na.rm = TRUE); denom[denom == 0] <- 1
      ds <- sweep(dw_a, 1, denom, "/")
      df <- sweep(ds, 1, pop_inc, "*")
      pd <- colSums(df, na.rm = TRUE)
      return(pd[obs_idx])
    }

    # SSE on within-state shares: sum over states of within-state SSE
    # Uses pre-computed state_groups to avoid repeated which() calls
    sse_fn <- function(params, decay_type) {
      if (params[1] <= 0) return(1e10)
      pred <- predict_fn(params, decay_type)
      pred_ws <- numeric(length(pred))
      for (idx in state_groups) {
        pred_ws[idx] <- pred[idx] / sum(pred[idx])
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

    # --- ROOMS specification: SSE and fit ---
    sse_fn_rooms <- function(params, decay_type) {
      if (params[1] <= 0) return(1e10)
      pred <- predict_fn_rooms(params, decay_type)
      pred_ws <- numeric(length(pred))
      for (idx in state_groups) {
        pred_ws[idx] <- pred[idx] / sum(pred[idx])
      }
      sum((obs_ws_shares - pred_ws)^2, na.rm = TRUE)
    }

    fit_model_rooms <- function(decay_type) {
      if (decay_type == "exponential") {
        lower <- c(0.001, -1, -2); upper <- c(2, 3, 5); start <- c(0.05, 0.15, 0.1)
      } else if (decay_type == "power") {
        lower <- c(0.1, -1, -2); upper <- c(20, 3, 5); start <- c(2.0, 0.15, 0.1)
      } else {
        lower <- c(1e-5, -1, -2); upper <- c(0.5, 3, 5); start <- c(0.001, 0.15, 0.1)
      }
      result <- tryCatch({
        optim(start, fn = function(p) sse_fn_rooms(p, decay_type),
              method = "L-BFGS-B", lower = lower, upper = upper,
              control = list(factr = 1e2, maxit = 1000))
      }, error = function(e) {
        tryCatch({
          optim(c(start[1] * 2, 0.1, 0), fn = function(p) sse_fn_rooms(p, decay_type),
                method = "L-BFGS-B", lower = lower, upper = upper,
                control = list(factr = 1e2, maxit = 1000))
        }, error = function(e2) { list(par = start, value = 1e10, convergence = 1) })
      })
      result
    }

    # --- DESTINATION specification: SSE and fit (4 params: beta, a_hotel, a_dest, a_tables) ---
    sse_fn_dest <- function(params, decay_type) {
      if (params[1] <= 0) return(1e10)
      pred <- predict_fn_dest(params, decay_type)
      pred_ws <- numeric(length(pred))
      for (idx in state_groups) {
        pred_ws[idx] <- pred[idx] / sum(pred[idx])
      }
      sum((obs_ws_shares - pred_ws)^2, na.rm = TRUE)
    }

    fit_model_dest <- function(decay_type) {
      if (decay_type == "exponential") {
        lower <- c(0.001, -2, -2, -2); upper <- c(2, 5, 5, 5); start <- c(0.05, 0.1, 0.1, 0.1)
      } else if (decay_type == "power") {
        lower <- c(0.1, -2, -2, -2); upper <- c(20, 5, 5, 5); start <- c(2.0, 0.1, 0.1, 0.1)
      } else {
        lower <- c(1e-5, -2, -2, -2); upper <- c(0.5, 5, 5, 5); start <- c(0.001, 0.1, 0.1, 0.1)
      }
      result <- tryCatch({
        optim(start, fn = function(p) sse_fn_dest(p, decay_type),
              method = "L-BFGS-B", lower = lower, upper = upper,
              control = list(factr = 1e2, maxit = 1000))
      }, error = function(e) {
        tryCatch({
          optim(c(start[1] * 2, 0, 0, 0), fn = function(p) sse_fn_dest(p, decay_type),
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

    # Rooms specification
    fit_exp_r   <- fit_model_rooms("exponential")
    fit_pow_r   <- fit_model_rooms("power")
    fit_gauss_r <- fit_model_rooms("gaussian")
    best_sse_rooms <- min(fit_exp_r$value, fit_pow_r$value, fit_gauss_r$value)

    # Destination specification
    fit_exp_d   <- fit_model_dest("exponential")
    fit_pow_d   <- fit_model_dest("power")
    fit_gauss_d <- fit_model_dest("gaussian")
    best_sse_dest <- min(fit_exp_d$value, fit_pow_d$value, fit_gauss_d$value)

    if (verbose) {
      cat(sprintf("  Radius %3d mi | %2d mkts (%2d obs)\n", radius, nm, sum(mkt_obs)))
      cat(sprintf("    Binary:  Exp=%.6f Pow=%.6f Gau=%.6f Best=%.6f\n",
                  fit_exp$value, fit_pow$value, fit_gauss$value, best_sse))
      cat(sprintf("    Rooms:   Exp=%.6f Pow=%.6f Gau=%.6f Best=%.6f\n",
                  fit_exp_r$value, fit_pow_r$value, fit_gauss_r$value, best_sse_rooms))
      cat(sprintf("    Dest:    Exp=%.6f Pow=%.6f Gau=%.6f Best=%.6f\n",
                  fit_exp_d$value, fit_pow_d$value, fit_gauss_d$value, best_sse_dest))
    }

    list(radius = radius, n_markets = nm, n_observed = sum(mkt_obs),
         exponential = list(par = fit_exp$par, sse = fit_exp$value, conv = fit_exp$convergence),
         power       = list(par = fit_pow$par, sse = fit_pow$value, conv = fit_pow$convergence),
         gaussian    = list(par = fit_gauss$par, sse = fit_gauss$value, conv = fit_gauss$convergence),
         best_sse    = best_sse,
         # Rooms specification results
         exponential_rooms = list(par = fit_exp_r$par, sse = fit_exp_r$value, conv = fit_exp_r$convergence),
         power_rooms       = list(par = fit_pow_r$par, sse = fit_pow_r$value, conv = fit_pow_r$convergence),
         gaussian_rooms    = list(par = fit_gauss_r$par, sse = fit_gauss_r$value, conv = fit_gauss_r$convergence),
         best_sse_rooms    = best_sse_rooms,
         # Destination specification results
         exponential_dest = list(par = fit_exp_d$par, sse = fit_exp_d$value, conv = fit_exp_d$convergence),
         power_dest       = list(par = fit_pow_d$par, sse = fit_pow_d$value, conv = fit_pow_d$convergence),
         gaussian_dest    = list(par = fit_gauss_d$par, sse = fit_gauss_d$value, conv = fit_gauss_d$convergence),
         best_sse_dest    = best_sse_dest)
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
    hotel_rooms = numeric(n_mkts),
    destination_resort = integer(n_mkts),
    is_cardroom = integer(n_mkts),
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
    markets$hotel_rooms[k] <- sum(members$hotel_rooms, na.rm = TRUE)
    markets$has_tables[k] <- as.integer(any(members$has_tables == 1, na.rm = TRUE))
    # destination_resort: 1 if any member is a destination resort
    if ("destination_resort" %in% names(members)) {
      markets$destination_resort[k] <- as.integer(any(members$destination_resort == 1, na.rm = TRUE))
    }
    # is_cardroom: 1 only if ALL casinos in cluster are cardrooms
    markets$is_cardroom[k] <- as.integer(all(members$is_cardroom == 1))
    markets$state[k] <- mkt_st
    markets$label[k] <- lbl
  }

  # Compute ln(hotel_rooms) for power-function specification
  markets$ln_hotel_rooms <- ifelse(markets$hotel_rooms > 0, log(markets$hotel_rooms), 0)

  n_obs_mkts <- sum(markets$observed)
  n_cardroom_mkts <- sum(markets$is_cardroom == 1 & markets$observed)
  cat("  Cardroom markets (observed):", n_cardroom_mkts, "\n")
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

  # Pre-compute distance mask for sparse decay computation
  dist_mask2 <- dist_matrix <= 150
  dist_vals2 <- dist_matrix[dist_mask2]

  # =====================================================
  # GRAVITY SHARE MODEL (non-linear, 3 parameters)
  # =====================================================
  # params = c(beta, a_hotel, a_tables)
  predict_mkt <- function(params, decay_type = "exponential", max_dist = 150) {
    beta <- params[1]; a_hotel <- params[2]; a_tables <- params[3]
    dw <- matrix(0, nrow = n_z, ncol = n_mkts)
    if (decay_type == "exponential") { dw[dist_mask2] <- decay_exponential(dist_vals2, beta)
    } else if (decay_type == "power") { dw[dist_mask2] <- decay_power(dist_vals2, beta)
    } else { dw[dist_mask2] <- decay_gaussian(dist_vals2, beta) }
    attract <- exp(a_hotel * markets$has_hotel + a_tables * markets$has_tables)
    dw <- sweep(dw, 2, attract, "*")
    denom <- rowSums(dw, na.rm = TRUE); denom[denom == 0] <- 1
    ds <- sweep(dw, 1, denom, "/")
    df <- sweep(ds, 1, pop_inc, "*")
    pd <- colSums(df, na.rm = TRUE)
    obs_idx <- which(markets$observed)
    return(pd[obs_idx])
  }

  # params = c(beta, a_rooms, a_tables) -- POWER-FUNCTION (rooms) specification
  predict_mkt_rooms <- function(params, decay_type = "exponential", max_dist = 150) {
    beta <- params[1]; a_rooms <- params[2]; a_tables <- params[3]
    dw <- matrix(0, nrow = n_z, ncol = n_mkts)
    if (decay_type == "exponential") { dw[dist_mask2] <- decay_exponential(dist_vals2, beta)
    } else if (decay_type == "power") { dw[dist_mask2] <- decay_power(dist_vals2, beta)
    } else { dw[dist_mask2] <- decay_gaussian(dist_vals2, beta) }
    attract <- exp(a_rooms * markets$ln_hotel_rooms + a_tables * markets$has_tables)
    dw <- sweep(dw, 2, attract, "*")
    denom <- rowSums(dw, na.rm = TRUE); denom[denom == 0] <- 1
    ds <- sweep(dw, 1, denom, "/")
    df <- sweep(ds, 1, pop_inc, "*")
    pd <- colSums(df, na.rm = TRUE)
    obs_idx <- which(markets$observed)
    return(pd[obs_idx])
  }

  # params = c(beta, a_hotel, a_dest, a_tables) -- DESTINATION specification
  predict_mkt_dest <- function(params, decay_type = "exponential", max_dist = 150) {
    beta <- params[1]; a_hotel <- params[2]; a_dest <- params[3]; a_tables <- params[4]
    dw <- matrix(0, nrow = n_z, ncol = n_mkts)
    if (decay_type == "exponential") { dw[dist_mask2] <- decay_exponential(dist_vals2, beta)
    } else if (decay_type == "power") { dw[dist_mask2] <- decay_power(dist_vals2, beta)
    } else { dw[dist_mask2] <- decay_gaussian(dist_vals2, beta) }
    attract <- exp(a_hotel * markets$has_hotel + a_dest * markets$destination_resort + a_tables * markets$has_tables)
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
  obs_is_cardroom <- markets$is_cardroom[markets$observed]

  # Pre-compute within-state actual shares
  obs_ws_shares <- numeric(length(obs_mkt_rev))
  for (s in unique(obs_mkt_state)) {
    s_idx <- which(obs_mkt_state == s)
    obs_ws_shares[s_idx] <- obs_mkt_rev[s_idx] / sum(obs_mkt_rev[s_idx])
  }
  state_groups2 <- split(seq_along(obs_mkt_state), obs_mkt_state)

  sse_fn2 <- function(params, decay_type) {
    if (params[1] <= 0) return(1e10)
    pred <- predict_mkt(params, decay_type)
    pred_ws <- numeric(length(pred))
    for (idx in state_groups2) {
      pred_ws[idx] <- pred[idx] / sum(pred[idx])
    }
    sum((obs_ws_shares - pred_ws)^2, na.rm = TRUE)
  }

  sse_fn2_rooms <- function(params, decay_type) {
    if (params[1] <= 0) return(1e10)
    pred <- predict_mkt_rooms(params, decay_type)
    pred_ws <- numeric(length(pred))
    for (idx in state_groups2) {
      pred_ws[idx] <- pred[idx] / sum(pred[idx])
    }
    sum((obs_ws_shares - pred_ws)^2, na.rm = TRUE)
  }

  sse_fn2_dest <- function(params, decay_type) {
    if (params[1] <= 0) return(1e10)
    pred <- predict_mkt_dest(params, decay_type)
    pred_ws <- numeric(length(pred))
    for (idx in state_groups2) {
      pred_ws[idx] <- pred[idx] / sum(pred[idx])
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

  # --- Gravity Share Models: ROOMS specification (3 params: beta, a_rooms, a_tables) ---
  cat("\n  --- Gravity Share Models ROOMS (3 params: beta, a_rooms, a_tables) ---\n")
  results_rooms <- list()
  for (dt in c("exponential", "power", "gaussian")) {
    if (dt == "exponential") {
      lower <- c(0.001, -1, -2); upper <- c(2, 3, 5); start <- c(0.05, 0.15, 0.1)
    } else if (dt == "power") {
      lower <- c(0.1, -1, -2); upper <- c(20, 3, 5); start <- c(2.0, 0.15, 0.1)
    } else {
      lower <- c(1e-5, -1, -2); upper <- c(0.5, 3, 5); start <- c(0.001, 0.15, 0.1)
    }
    fit <- tryCatch({
      optim(start, fn = function(p) sse_fn2_rooms(p, dt),
            method = "L-BFGS-B", lower = lower, upper = upper,
            control = list(factr = 1e2, maxit = 1000))
    }, error = function(e) list(par = start, value = 1e10, convergence = 1))
    results_rooms[[dt]] <- list(par = fit$par, sse = fit$value, conv = fit$convergence)
    cat(sprintf("  %-12s: beta=%.4f, a_R=%.3f, a_T=%.3f, SSE=%.8f\n",
                dt, fit$par[1], fit$par[2], fit$par[3], fit$value))
  }

  comparison_rooms <- data.frame(
    Model = c("Exponential", "Power", "Gaussian"),
    Beta = c(results_rooms$exponential$par[1], results_rooms$power$par[1], results_rooms$gaussian$par[1]),
    Alpha_Rooms = c(results_rooms$exponential$par[2], results_rooms$power$par[2], results_rooms$gaussian$par[2]),
    Alpha_Tables = c(results_rooms$exponential$par[3], results_rooms$power$par[3], results_rooms$gaussian$par[3]),
    SSE = c(results_rooms$exponential$sse, results_rooms$power$sse, results_rooms$gaussian$sse),
    R_squared = 1 - c(results_rooms$exponential$sse, results_rooms$power$sse, results_rooms$gaussian$sse) / tss,
    stringsAsFactors = FALSE
  )
  comparison_rooms <- comparison_rooms[order(comparison_rooms$SSE), ]

  # --- Gravity Share Models: DESTINATION specification (4 params: beta, a_hotel, a_dest, a_tables) ---
  cat("\n  --- Gravity Share Models DEST (4 params: beta, a_hotel, a_dest, a_tables) ---\n")
  n_dest_mkts <- sum(markets$destination_resort[markets$observed] == 1)
  cat(sprintf("  Destination resort markets (observed): %d\n", n_dest_mkts))
  results_dest <- list()
  for (dt in c("exponential", "power", "gaussian")) {
    if (dt == "exponential") {
      lower <- c(0.001, -2, -2, -2); upper <- c(2, 5, 5, 5); start <- c(0.05, 0.1, 0.1, 0.1)
    } else if (dt == "power") {
      lower <- c(0.1, -2, -2, -2); upper <- c(20, 5, 5, 5); start <- c(2.0, 0.1, 0.1, 0.1)
    } else {
      lower <- c(1e-5, -2, -2, -2); upper <- c(0.5, 5, 5, 5); start <- c(0.001, 0.1, 0.1, 0.1)
    }
    fit <- tryCatch({
      optim(start, fn = function(p) sse_fn2_dest(p, dt),
            method = "L-BFGS-B", lower = lower, upper = upper,
            control = list(factr = 1e2, maxit = 1000))
    }, error = function(e) list(par = start, value = 1e10, convergence = 1))
    results_dest[[dt]] <- list(par = fit$par, sse = fit$value, conv = fit$convergence)
    cat(sprintf("  %-12s: beta=%.4f, a_H=%.3f, a_D=%.3f, a_T=%.3f, SSE=%.8f\n",
                dt, fit$par[1], fit$par[2], fit$par[3], fit$par[4], fit$value))
  }

  comparison_dest <- data.frame(
    Model = c("Exponential", "Power", "Gaussian"),
    Beta = c(results_dest$exponential$par[1], results_dest$power$par[1], results_dest$gaussian$par[1]),
    Alpha_Hotel = c(results_dest$exponential$par[2], results_dest$power$par[2], results_dest$gaussian$par[2]),
    Alpha_Dest = c(results_dest$exponential$par[3], results_dest$power$par[3], results_dest$gaussian$par[3]),
    Alpha_Tables = c(results_dest$exponential$par[4], results_dest$power$par[4], results_dest$gaussian$par[4]),
    SSE = c(results_dest$exponential$sse, results_dest$power$sse, results_dest$gaussian$sse),
    R_squared = 1 - c(results_dest$exponential$sse, results_dest$power$sse, results_dest$gaussian$sse) / tss,
    stringsAsFactors = FALSE
  )
  comparison_dest <- comparison_dest[order(comparison_dest$SSE), ]

  # --- Binary vs Rooms vs Destination comparison ---
  cat("\n  --- GRAVITY SHARE: Binary vs Rooms vs Destination Comparison ---\n")
  cat(sprintf("  %-20s  %8s  %10s  %10s  %10s  %10s  %8s\n", "Specification", "Beta", "a_H", "a_R/a_D", "a_T", "SSE", "R2"))
  cat(sprintf("  %-20s  %8s  %10s  %10s  %10s  %10s  %8s\n", "-------------", "----", "---", "------", "---", "---", "--"))
  cat(sprintf("  %-20s  %8.4f  %10.3f  %10s  %10.3f  %10.6f  %8.4f\n",
              paste0("Binary (", comparison$Model[1], ")"),
              comparison$Beta[1], comparison$Alpha_Hotel[1], "--",
              comparison$Alpha_Tables[1], comparison$SSE[1], comparison$R_squared[1]))
  cat(sprintf("  %-20s  %8.4f  %10s  %10.3f  %10.3f  %10.6f  %8.4f\n",
              paste0("Rooms (", comparison_rooms$Model[1], ")"),
              comparison_rooms$Beta[1], "--", comparison_rooms$Alpha_Rooms[1],
              comparison_rooms$Alpha_Tables[1], comparison_rooms$SSE[1], comparison_rooms$R_squared[1]))
  cat(sprintf("  %-20s  %8.4f  %10.3f  %10.3f  %10.3f  %10.6f  %8.4f\n",
              paste0("Dest (", comparison_dest$Model[1], ")"),
              comparison_dest$Beta[1], comparison_dest$Alpha_Hotel[1], comparison_dest$Alpha_Dest[1],
              comparison_dest$Alpha_Tables[1], comparison_dest$SSE[1], comparison_dest$R_squared[1]))
  best_rooms_a <- comparison_rooms$Alpha_Rooms[1]
  cat(sprintf("\n  Implied multipliers (rooms^%.3f):\n", best_rooms_a))
  for (nr in c(14, 50, 100, 200, 500, 1000, 2798)) {
    cat(sprintf("    %4d rooms -> binary: %.2fx  rooms3: %.2fx\n",
                nr, exp(comparison$Alpha_Hotel[1]), nr^best_rooms_a))
  }
  cat(sprintf("\n  Destination resort attractiveness boost: exp(%.3f) = %.2fx on top of hotel\n",
              comparison_dest$Alpha_Dest[1], exp(comparison_dest$Alpha_Dest[1])))

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
  obs_cardroom_ln <- markets$is_cardroom[obs_idx_ln]
  obs_ln_rooms <- markets$ln_hotel_rooms[obs_idx_ln]
  obs_dest_ln <- markets$destination_resort[obs_idx_ln]

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
      X <- cbind(1, log(di_obs), obs_hotel_ln, obs_tables_ln, obs_cardroom_ln)
      # OLS: beta_hat = (X'X)^-1 X'y
      fit <- tryCatch({
        XtX_inv <- solve(crossprod(X))
        b_hat <- XtX_inv %*% crossprod(X, y)
        y_hat <- X %*% b_hat
        ss_res <- sum((y - y_hat)^2)
        ss_tot <- sum((y - mean(y))^2)
        list(r2 = 1 - ss_res / ss_tot, coefs = as.numeric(b_hat), ss_res = ss_res)
      }, error = function(e) list(r2 = -Inf, coefs = rep(NA, 5), ss_res = Inf))

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
    X <- cbind(1, log(di_obs), obs_hotel_ln, obs_tables_ln, obs_cardroom_ln)
    XtX_inv <- solve(crossprod(X))
    b_hat <- as.numeric(XtX_inv %*% crossprod(X, y))
    y_hat <- X %*% b_hat
    ss_res <- sum((y - y_hat)^2)
    ss_tot <- sum((y - mean(y))^2)
    r2 <- 1 - ss_res / ss_tot
    n_obs <- length(y); k_params <- 5
    adj_r2 <- 1 - (1 - r2) * (n_obs - 1) / (n_obs - k_params)

    # Standard errors
    sigma2 <- ss_res / (n_obs - k_params)
    se_hat <- sqrt(diag(XtX_inv) * sigma2)
    t_vals <- b_hat / se_hat

    ln_results[[dt]] <- list(
      beta_decay = beta_ln,
      coefs = b_hat,  # c(intercept, gamma, b_hotel, b_tables, b_cardroom)
      se = se_hat,
      t_vals = t_vals,
      r2 = r2,
      adj_r2 = adj_r2,
      n = n_obs,
      demand_idx = di_obs,
      y_hat = as.numeric(y_hat)
    )

    cat(sprintf("  LN-%-11s: beta=%.4f, gamma=%.3f, hotel=%.3f, tables=%.3f, cardroom=%.3f, R2=%.4f (adj=%.4f)\n",
                dt, beta_ln, b_hat[2], b_hat[3], b_hat[4], b_hat[5], r2, adj_r2))
  }

  # Best LN model
  ln_r2s <- sapply(ln_results, function(x) x$r2)
  best_ln_type <- names(which.max(ln_r2s))
  best_ln <- ln_results[[best_ln_type]]

  cat(sprintf("\n  Best LN model: %s (beta=%.4f, R2=%.4f, adj-R2=%.4f)\n",
              best_ln_type, best_ln$beta_decay, best_ln$r2, best_ln$adj_r2))
  cat("    Coefficients (t-values):\n")
  cat(sprintf("      Intercept:   %7.3f  (%.2f)\n", best_ln$coefs[1], best_ln$t_vals[1]))
  cat(sprintf("      ln(D_j):     %7.3f  (%.2f)  [demand elasticity]\n", best_ln$coefs[2], best_ln$t_vals[2]))
  cat(sprintf("      Hotel:       %7.3f  (%.2f)\n", best_ln$coefs[3], best_ln$t_vals[3]))
  cat(sprintf("      Tables:      %7.3f  (%.2f)\n", best_ln$coefs[4], best_ln$t_vals[4]))
  cat(sprintf("      is_cardroom: %7.3f  (%.2f)\n", best_ln$coefs[5], best_ln$t_vals[5]))

  # --- Log-Linear (LN) Models: ROOMS specification ---
  # Replace binary has_hotel with ln(hotel_rooms) in OLS
  cat("\n  --- Log-Linear (LN) Models ROOMS ---\n")

  ln_results_rooms <- list()

  for (dt in c("exponential", "power", "gaussian")) {
    ln_obj_rooms <- function(beta) {
      if (beta <= 0) return(1e10)
      if (dt == "exponential") { dw <- decay_exponential(dist_matrix, beta)
      } else if (dt == "power") { dw <- decay_power(dist_matrix, beta)
      } else { dw <- decay_gaussian(dist_matrix, beta) }
      dw[dist_matrix > 150] <- 0
      demand_flow <- sweep(dw, 1, pop_inc, "*")
      demand_idx <- colSums(demand_flow, na.rm = TRUE)
      di_obs <- demand_idx[obs_idx_ln]
      if (any(di_obs <= 0)) return(1e10)

      y <- log(obs_rev_ln)
      X <- cbind(1, log(di_obs), obs_ln_rooms, obs_tables_ln, obs_cardroom_ln)
      fit <- tryCatch({
        XtX_inv <- solve(crossprod(X))
        b_hat <- XtX_inv %*% crossprod(X, y)
        y_hat <- X %*% b_hat
        ss_res <- sum((y - y_hat)^2)
        ss_tot <- sum((y - mean(y))^2)
        list(r2 = 1 - ss_res / ss_tot, coefs = as.numeric(b_hat), ss_res = ss_res)
      }, error = function(e) list(r2 = -Inf, coefs = rep(NA, 5), ss_res = Inf))

      return(-fit$r2)
    }

    if (dt == "exponential") {
      search_range <- c(0.001, 1)
    } else if (dt == "power") {
      search_range <- c(0.1, 10)
    } else {
      search_range <- c(1e-6, 0.1)
    }

    opt <- optimize(ln_obj_rooms, interval = search_range, tol = 1e-6)
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
    X <- cbind(1, log(di_obs), obs_ln_rooms, obs_tables_ln, obs_cardroom_ln)
    XtX_inv <- solve(crossprod(X))
    b_hat <- as.numeric(XtX_inv %*% crossprod(X, y))
    y_hat <- X %*% b_hat
    ss_res <- sum((y - y_hat)^2)
    ss_tot <- sum((y - mean(y))^2)
    r2 <- 1 - ss_res / ss_tot
    n_obs <- length(y); k_params <- 5
    adj_r2 <- 1 - (1 - r2) * (n_obs - 1) / (n_obs - k_params)

    sigma2 <- ss_res / (n_obs - k_params)
    se_hat <- sqrt(diag(XtX_inv) * sigma2)
    t_vals <- b_hat / se_hat

    ln_results_rooms[[dt]] <- list(
      beta_decay = beta_ln,
      coefs = b_hat,  # c(intercept, gamma, b_ln_rooms, b_tables, b_cardroom)
      se = se_hat,
      t_vals = t_vals,
      r2 = r2,
      adj_r2 = adj_r2,
      n = n_obs,
      demand_idx = di_obs,
      y_hat = as.numeric(y_hat)
    )

    cat(sprintf("  LN-%-11s: beta=%.4f, gamma=%.3f, ln_rooms=%.3f, tables=%.3f, cardroom=%.3f, R2=%.4f (adj=%.4f)\n",
                dt, beta_ln, b_hat[2], b_hat[3], b_hat[4], b_hat[5], r2, adj_r2))
  }

  # Best LN rooms model
  ln_r2s_rooms <- sapply(ln_results_rooms, function(x) x$r2)
  best_ln_type_rooms <- names(which.max(ln_r2s_rooms))
  best_ln_rooms <- ln_results_rooms[[best_ln_type_rooms]]

  cat(sprintf("\n  Best LN-rooms model: %s (beta=%.4f, R2=%.4f, adj-R2=%.4f)\n",
              best_ln_type_rooms, best_ln_rooms$beta_decay, best_ln_rooms$r2, best_ln_rooms$adj_r2))
  cat("    Coefficients (t-values):\n")
  cat(sprintf("      Intercept:     %7.3f  (%.2f)\n", best_ln_rooms$coefs[1], best_ln_rooms$t_vals[1]))
  cat(sprintf("      ln(D_j):       %7.3f  (%.2f)  [demand elasticity]\n", best_ln_rooms$coefs[2], best_ln_rooms$t_vals[2]))
  cat(sprintf("      ln(rooms):     %7.3f  (%.2f)\n", best_ln_rooms$coefs[3], best_ln_rooms$t_vals[3]))
  cat(sprintf("      Tables:        %7.3f  (%.2f)\n", best_ln_rooms$coefs[4], best_ln_rooms$t_vals[4]))
  cat(sprintf("      is_cardroom:   %7.3f  (%.2f)\n", best_ln_rooms$coefs[5], best_ln_rooms$t_vals[5]))

  # --- Log-Linear (LN) Models: DESTINATION specification ---
  # Add destination_resort alongside has_hotel in OLS: 6 params total
  cat("\n  --- Log-Linear (LN) Models DESTINATION ---\n")

  ln_results_dest <- list()

  for (dt in c("exponential", "power", "gaussian")) {
    ln_obj_dest <- function(beta) {
      if (beta <= 0) return(1e10)
      if (dt == "exponential") { dw <- decay_exponential(dist_matrix, beta)
      } else if (dt == "power") { dw <- decay_power(dist_matrix, beta)
      } else { dw <- decay_gaussian(dist_matrix, beta) }
      dw[dist_matrix > 150] <- 0
      demand_flow <- sweep(dw, 1, pop_inc, "*")
      demand_idx <- colSums(demand_flow, na.rm = TRUE)
      di_obs <- demand_idx[obs_idx_ln]
      if (any(di_obs <= 0)) return(1e10)

      y <- log(obs_rev_ln)
      X <- cbind(1, log(di_obs), obs_hotel_ln, obs_dest_ln, obs_tables_ln, obs_cardroom_ln)
      fit <- tryCatch({
        XtX_inv <- solve(crossprod(X))
        b_hat <- XtX_inv %*% crossprod(X, y)
        y_hat <- X %*% b_hat
        ss_res <- sum((y - y_hat)^2)
        ss_tot <- sum((y - mean(y))^2)
        list(r2 = 1 - ss_res / ss_tot, coefs = as.numeric(b_hat), ss_res = ss_res)
      }, error = function(e) list(r2 = -Inf, coefs = rep(NA, 6), ss_res = Inf))

      return(-fit$r2)
    }

    if (dt == "exponential") {
      search_range <- c(0.001, 1)
    } else if (dt == "power") {
      search_range <- c(0.1, 10)
    } else {
      search_range <- c(1e-6, 0.1)
    }

    opt <- optimize(ln_obj_dest, interval = search_range, tol = 1e-6)
    beta_ln <- opt$minimum

    if (dt == "exponential") { dw <- decay_exponential(dist_matrix, beta_ln)
    } else if (dt == "power") { dw <- decay_power(dist_matrix, beta_ln)
    } else { dw <- decay_gaussian(dist_matrix, beta_ln) }
    dw[dist_matrix > 150] <- 0
    demand_flow <- sweep(dw, 1, pop_inc, "*")
    demand_idx <- colSums(demand_flow, na.rm = TRUE)
    di_obs <- demand_idx[obs_idx_ln]

    y <- log(obs_rev_ln)
    X <- cbind(1, log(di_obs), obs_hotel_ln, obs_dest_ln, obs_tables_ln, obs_cardroom_ln)
    XtX_inv <- solve(crossprod(X))
    b_hat <- as.numeric(XtX_inv %*% crossprod(X, y))
    y_hat <- X %*% b_hat
    ss_res <- sum((y - y_hat)^2)
    ss_tot <- sum((y - mean(y))^2)
    r2 <- 1 - ss_res / ss_tot
    n_obs <- length(y); k_params <- 6
    adj_r2 <- 1 - (1 - r2) * (n_obs - 1) / (n_obs - k_params)

    sigma2 <- ss_res / (n_obs - k_params)
    se_hat <- sqrt(diag(XtX_inv) * sigma2)
    t_vals <- b_hat / se_hat

    ln_results_dest[[dt]] <- list(
      beta_decay = beta_ln,
      coefs = b_hat,  # c(intercept, gamma, b_hotel, b_dest, b_tables, b_cardroom)
      se = se_hat,
      t_vals = t_vals,
      r2 = r2,
      adj_r2 = adj_r2,
      n = n_obs,
      demand_idx = di_obs,
      y_hat = as.numeric(y_hat)
    )

    cat(sprintf("  LN-%-11s: beta=%.4f, gamma=%.3f, hotel=%.3f, dest=%.3f, tables=%.3f, cardroom=%.3f, R2=%.4f (adj=%.4f)\n",
                dt, beta_ln, b_hat[2], b_hat[3], b_hat[4], b_hat[5], b_hat[6], r2, adj_r2))
  }

  # Best LN destination model
  ln_r2s_dest <- sapply(ln_results_dest, function(x) x$r2)
  best_ln_type_dest <- names(which.max(ln_r2s_dest))
  best_ln_dest <- ln_results_dest[[best_ln_type_dest]]

  cat(sprintf("\n  Best LN-dest model: %s (beta=%.4f, R2=%.4f, adj-R2=%.4f)\n",
              best_ln_type_dest, best_ln_dest$beta_decay, best_ln_dest$r2, best_ln_dest$adj_r2))
  cat("    Coefficients (t-values):\n")
  cat(sprintf("      Intercept:      %7.3f  (%.2f)\n", best_ln_dest$coefs[1], best_ln_dest$t_vals[1]))
  cat(sprintf("      ln(D_j):        %7.3f  (%.2f)  [demand elasticity]\n", best_ln_dest$coefs[2], best_ln_dest$t_vals[2]))
  cat(sprintf("      Hotel:          %7.3f  (%.2f)\n", best_ln_dest$coefs[3], best_ln_dest$t_vals[3]))
  cat(sprintf("      Dest Resort:    %7.3f  (%.2f)\n", best_ln_dest$coefs[4], best_ln_dest$t_vals[4]))
  cat(sprintf("      Tables:         %7.3f  (%.2f)\n", best_ln_dest$coefs[5], best_ln_dest$t_vals[5]))
  cat(sprintf("      is_cardroom:    %7.3f  (%.2f)\n", best_ln_dest$coefs[6], best_ln_dest$t_vals[6]))

  cat("\n  --- LN Model: Binary vs Rooms vs Destination Comparison ---\n")
  cat(sprintf("  %-16s  R2=%.4f  adj-R2=%.4f (hotel=%.3f, t=%.2f)\n",
              paste0("Binary (", best_ln_type, ")"),
              best_ln$r2, best_ln$adj_r2, best_ln$coefs[3], best_ln$t_vals[3]))
  cat(sprintf("  %-16s  R2=%.4f  adj-R2=%.4f (ln_rooms=%.3f, t=%.2f)\n",
              paste0("Rooms (", best_ln_type_rooms, ")"),
              best_ln_rooms$r2, best_ln_rooms$adj_r2, best_ln_rooms$coefs[3], best_ln_rooms$t_vals[3]))
  cat(sprintf("  %-16s  R2=%.4f  adj-R2=%.4f (hotel=%.3f t=%.2f, dest=%.3f t=%.2f)\n",
              paste0("Dest (", best_ln_type_dest, ")"),
              best_ln_dest$r2, best_ln_dest$adj_r2,
              best_ln_dest$coefs[3], best_ln_dest$t_vals[3],
              best_ln_dest$coefs[4], best_ln_dest$t_vals[4]))

  # =====================================================
  # COMPETITIVE GRAVITY REVENUE MODEL
  # =====================================================
  # Uses gravity demand index (with competition in denominator) to predict
  # absolute revenue: ln(Rev_j) = alpha + gamma * ln(D_j) + delta * is_cardroom + epsilon
  # where D_j = sum_i [pop_inc_i * A_j * f(d_ij) / sum_k A_k * f(d_ik)]
  # Hotel/tables enter through attractiveness A_j in the demand index.
  # is_cardroom enters the OLS to capture the revenue discount for table-only venues.
  # Parameters: beta, a_hotel, a_tables (demand index) + intercept, gamma, delta (OLS)

  cat("\n  --- Competitive Gravity Revenue Models ---\n")

  comp_grav_results <- list()

  # Skip gaussian for CG: it consistently underperforms and can cause
  # memory/segfault issues during L-BFGS-B gradient computation
  for (dt in c("exponential", "power")) {
    comp_obj <- function(params) {
      if (params[1] <= 0) return(1e10)
      pred_d <- predict_mkt(params, dt)
      if (any(pred_d <= 0)) return(1e10)
      y <- log(obs_mkt_rev)
      X <- cbind(1, log(pred_d), obs_is_cardroom)
      fit <- tryCatch({
        XtX_inv <- solve(crossprod(X))
        b_hat <- XtX_inv %*% crossprod(X, y)
        y_hat <- X %*% b_hat
        ss_res <- sum((y - y_hat)^2)
        ss_tot <- sum((y - mean(y))^2)
        list(r2 = 1 - ss_res / ss_tot)
      }, error = function(e) list(r2 = -Inf))
      return(-fit$r2)
    }

    if (dt == "exponential") {
      lower <- c(0.001, -2, -2); upper <- c(2, 5, 5); start <- c(0.05, 0.1, 0.1)
    } else if (dt == "power") {
      lower <- c(0.1, -2, -2); upper <- c(20, 5, 5); start <- c(2.0, 0.1, 0.1)
    } else {
      lower <- c(1e-5, -2, -2); upper <- c(0.5, 5, 5); start <- c(0.001, 0.1, 0.1)
    }

    fit <- tryCatch({
      optim(start, fn = comp_obj, method = "L-BFGS-B",
            lower = lower, upper = upper,
            control = list(factr = 1e2, maxit = 1000))
    }, error = function(e) list(par = start, value = 1e10, convergence = 1))

    # Get final OLS at optimal params (wrapped in tryCatch for numerical safety)
    cg_iter <- tryCatch({
      pred_d <- predict_mkt(fit$par, dt)
      y <- log(obs_mkt_rev)
      X <- cbind(1, log(pred_d), obs_is_cardroom)
      XtX_inv <- solve(crossprod(X))
      b_hat <- as.numeric(XtX_inv %*% crossprod(X, y))
      y_hat <- X %*% b_hat
      ss_res <- sum((y - y_hat)^2)
      ss_tot <- sum((y - mean(y))^2)
      r2 <- 1 - ss_res / ss_tot
      n_obs <- length(y); k_params <- 3
      adj_r2 <- 1 - (1 - r2) * (n_obs - 1) / (n_obs - k_params)
      sigma2 <- ss_res / (n_obs - k_params)
      se_hat <- sqrt(diag(XtX_inv) * sigma2)
      t_vals <- b_hat / se_hat

      # Within-state share R2 from this model's demand indices
      comp_pred_shares <- numeric(length(pred_d))
      for (s in unique(obs_mkt_state)) {
        s_idx <- which(obs_mkt_state == s)
        comp_pred_shares[s_idx] <- pred_d[s_idx] / sum(pred_d[s_idx])
      }
      ws_ss_res <- sum((obs_ws_shares - comp_pred_shares)^2)
      ws_tss <- sum((obs_ws_shares - mean(obs_ws_shares))^2)
      ws_r2 <- 1 - ws_ss_res / ws_tss

      # Duan (1983) smearing estimator for retransformation bias correction
      residuals_ols <- as.numeric(y - y_hat)
      duan_smear <- mean(exp(residuals_ols))

      list(par = fit$par, coefs = b_hat, se = se_hat, t_vals = t_vals,
           r2 = r2, adj_r2 = adj_r2, ws_r2 = ws_r2, n = n_obs, sigma2 = sigma2,
           duan_smear = duan_smear, demand_idx = pred_d, y_hat = as.numeric(y_hat),
           pred_shares = comp_pred_shares, conv = fit$convergence)
    }, error = function(e) {
      cat(sprintf("  CG-%-11s: FAILED (%s)\n", dt, conditionMessage(e)))
      NULL
    })
    if (is.null(cg_iter)) next

    comp_grav_results[[dt]] <- cg_iter

    cat(sprintf("  CG-%-11s: beta=%.4f, a_H=%.3f, a_T=%.3f, R2(ln)=%.4f, WS-R2=%.4f\n",
                dt, cg_iter$par[1], cg_iter$par[2], cg_iter$par[3], cg_iter$r2, cg_iter$ws_r2))
  }

  # Best competitive gravity model
  cg_r2s <- sapply(comp_grav_results, function(x) x$r2)
  best_cg_type <- names(which.max(cg_r2s))
  best_cg <- comp_grav_results[[best_cg_type]]

  cat(sprintf("\n  Best competitive gravity revenue model: %s (R2(ln)=%.4f, adj=%.4f, WS-R2=%.4f)\n",
              best_cg_type, best_cg$r2, best_cg$adj_r2, best_cg$ws_r2))
  cat("    Demand index params:\n")
  cat(sprintf("      beta=%.4f, a_hotel=%.3f (mult=%.2fx), a_tables=%.3f (mult=%.2fx)\n",
              best_cg$par[1], best_cg$par[2], exp(best_cg$par[2]),
              best_cg$par[3], exp(best_cg$par[3])))
  cat("    OLS coefficients (t-values):\n")
  cat(sprintf("      Intercept:   %7.3f  (%.2f)\n", best_cg$coefs[1], best_cg$t_vals[1]))
  cat(sprintf("      ln(D_j):     %7.3f  (%.2f)  [demand elasticity]\n", best_cg$coefs[2], best_cg$t_vals[2]))
  cat(sprintf("      is_cardroom: %7.3f  (%.2f)  [exp(delta)=%.4f => %.1f%% of full-casino rev]\n",
              best_cg$coefs[3], best_cg$t_vals[3], exp(best_cg$coefs[3]), exp(best_cg$coefs[3]) * 100))
  cat(sprintf("    Duan smearing factor: %.4f\n", best_cg$duan_smear))

  # CG predictions in levels and within-state shares
  cg_pred_rev <- exp(best_cg$y_hat)
  cg_pred_shares <- best_cg$pred_shares

  # --- Competitive Gravity Revenue Models: ROOMS specification ---
  cat("\n  --- Competitive Gravity Revenue Models ROOMS ---\n")

  comp_grav_results_rooms <- list()

  for (dt in c("exponential", "power")) {
    comp_obj_rooms <- function(params) {
      if (params[1] <= 0) return(1e10)
      pred_d <- predict_mkt_rooms(params, dt)
      if (any(pred_d <= 0)) return(1e10)
      y <- log(obs_mkt_rev)
      X <- cbind(1, log(pred_d), obs_is_cardroom)
      fit <- tryCatch({
        XtX_inv <- solve(crossprod(X))
        b_hat <- XtX_inv %*% crossprod(X, y)
        y_hat <- X %*% b_hat
        ss_res <- sum((y - y_hat)^2)
        ss_tot <- sum((y - mean(y))^2)
        list(r2 = 1 - ss_res / ss_tot)
      }, error = function(e) list(r2 = -Inf))
      return(-fit$r2)
    }

    if (dt == "exponential") {
      lower <- c(0.001, -1, -2); upper <- c(2, 3, 5); start <- c(0.05, 0.15, 0.1)
    } else if (dt == "power") {
      lower <- c(0.1, -1, -2); upper <- c(20, 3, 5); start <- c(2.0, 0.15, 0.1)
    } else {
      lower <- c(1e-5, -1, -2); upper <- c(0.5, 3, 5); start <- c(0.001, 0.15, 0.1)
    }

    fit <- tryCatch({
      optim(start, fn = comp_obj_rooms, method = "L-BFGS-B",
            lower = lower, upper = upper,
            control = list(factr = 1e2, maxit = 1000))
    }, error = function(e) list(par = start, value = 1e10, convergence = 1))

    # Get final OLS at optimal params (wrapped in tryCatch for numerical safety)
    cg_iter_r <- tryCatch({
      pred_d <- predict_mkt_rooms(fit$par, dt)
      y <- log(obs_mkt_rev)
      X <- cbind(1, log(pred_d), obs_is_cardroom)
      XtX_inv <- solve(crossprod(X))
      b_hat <- as.numeric(XtX_inv %*% crossprod(X, y))
      y_hat <- X %*% b_hat
      ss_res <- sum((y - y_hat)^2)
      ss_tot <- sum((y - mean(y))^2)
      r2 <- 1 - ss_res / ss_tot
      n_obs <- length(y); k_params <- 3
      adj_r2 <- 1 - (1 - r2) * (n_obs - 1) / (n_obs - k_params)
      sigma2 <- ss_res / (n_obs - k_params)
      se_hat <- sqrt(diag(XtX_inv) * sigma2)
      t_vals <- b_hat / se_hat

      comp_pred_shares_r <- numeric(length(pred_d))
      for (s in unique(obs_mkt_state)) {
        s_idx <- which(obs_mkt_state == s)
        comp_pred_shares_r[s_idx] <- pred_d[s_idx] / sum(pred_d[s_idx])
      }
      ws_ss_res <- sum((obs_ws_shares - comp_pred_shares_r)^2)
      ws_tss <- sum((obs_ws_shares - mean(obs_ws_shares))^2)
      ws_r2 <- 1 - ws_ss_res / ws_tss

      residuals_ols <- as.numeric(y - y_hat)
      duan_smear <- mean(exp(residuals_ols))

      list(par = fit$par, coefs = b_hat, se = se_hat, t_vals = t_vals,
           r2 = r2, adj_r2 = adj_r2, ws_r2 = ws_r2, n = n_obs, sigma2 = sigma2,
           duan_smear = duan_smear, demand_idx = pred_d, y_hat = as.numeric(y_hat),
           pred_shares = comp_pred_shares_r, conv = fit$convergence)
    }, error = function(e) {
      cat(sprintf("  CG-%-11s: FAILED (%s)\n", dt, conditionMessage(e)))
      NULL
    })
    if (is.null(cg_iter_r)) next

    comp_grav_results_rooms[[dt]] <- cg_iter_r

    cat(sprintf("  CG-%-11s: beta=%.4f, a_R=%.3f, a_T=%.3f, R2(ln)=%.4f, WS-R2=%.4f\n",
                dt, cg_iter_r$par[1], cg_iter_r$par[2], cg_iter_r$par[3], cg_iter_r$r2, cg_iter_r$ws_r2))
  }

  # Best CG rooms model
  cg_r2s_rooms <- sapply(comp_grav_results_rooms, function(x) x$r2)
  best_cg_type_rooms <- names(which.max(cg_r2s_rooms))
  best_cg_rooms <- comp_grav_results_rooms[[best_cg_type_rooms]]

  cat(sprintf("\n  Best CG-rooms model: %s (R2(ln)=%.4f, adj=%.4f, WS-R2=%.4f)\n",
              best_cg_type_rooms, best_cg_rooms$r2, best_cg_rooms$adj_r2, best_cg_rooms$ws_r2))
  cat("    Demand index params:\n")
  cat(sprintf("      beta=%.4f, a_rooms=%.3f, a_tables=%.3f\n",
              best_cg_rooms$par[1], best_cg_rooms$par[2], best_cg_rooms$par[3]))
  cat("    OLS coefficients (t-values):\n")
  cat(sprintf("      Intercept:   %7.3f  (%.2f)\n", best_cg_rooms$coefs[1], best_cg_rooms$t_vals[1]))
  cat(sprintf("      ln(D_j):     %7.3f  (%.2f)  [demand elasticity]\n", best_cg_rooms$coefs[2], best_cg_rooms$t_vals[2]))
  cat(sprintf("      is_cardroom: %7.3f  (%.2f)  [exp(delta)=%.4f => %.1f%% of full-casino rev]\n",
              best_cg_rooms$coefs[3], best_cg_rooms$t_vals[3], exp(best_cg_rooms$coefs[3]), exp(best_cg_rooms$coefs[3]) * 100))
  cat(sprintf("    Duan smearing factor: %.4f\n", best_cg_rooms$duan_smear))

  # --- Competitive Gravity Revenue Models: DESTINATION specification ---
  cat("\n  --- Competitive Gravity Revenue Models DESTINATION ---\n")

  comp_grav_results_dest <- list()

  for (dt in c("exponential", "power")) {
    comp_obj_dest <- function(params) {
      if (params[1] <= 0) return(1e10)
      pred_d <- predict_mkt_dest(params, dt)
      if (any(pred_d <= 0)) return(1e10)
      y <- log(obs_mkt_rev)
      X <- cbind(1, log(pred_d), obs_is_cardroom)
      fit <- tryCatch({
        XtX_inv <- solve(crossprod(X))
        b_hat <- XtX_inv %*% crossprod(X, y)
        y_hat <- X %*% b_hat
        ss_res <- sum((y - y_hat)^2)
        ss_tot <- sum((y - mean(y))^2)
        list(r2 = 1 - ss_res / ss_tot)
      }, error = function(e) list(r2 = -Inf))
      return(-fit$r2)
    }

    if (dt == "exponential") {
      lower <- c(0.001, -2, -2, -2); upper <- c(2, 5, 5, 5); start <- c(0.05, 0.1, 0.1, 0.1)
    } else if (dt == "power") {
      lower <- c(0.1, -2, -2, -2); upper <- c(20, 5, 5, 5); start <- c(2.0, 0.1, 0.1, 0.1)
    } else {
      lower <- c(1e-5, -2, -2, -2); upper <- c(0.5, 5, 5, 5); start <- c(0.001, 0.1, 0.1, 0.1)
    }

    fit <- tryCatch({
      optim(start, fn = comp_obj_dest, method = "L-BFGS-B",
            lower = lower, upper = upper,
            control = list(factr = 1e2, maxit = 1000))
    }, error = function(e) list(par = start, value = 1e10, convergence = 1))

    # Get final OLS at optimal params (wrapped in tryCatch for numerical safety)
    cg_iter_d <- tryCatch({
      pred_d <- predict_mkt_dest(fit$par, dt)
      y <- log(obs_mkt_rev)
      X <- cbind(1, log(pred_d), obs_is_cardroom)
      XtX_inv <- solve(crossprod(X))
      b_hat <- as.numeric(XtX_inv %*% crossprod(X, y))
      y_hat <- X %*% b_hat
      ss_res <- sum((y - y_hat)^2)
      ss_tot <- sum((y - mean(y))^2)
      r2 <- 1 - ss_res / ss_tot
      n_obs <- length(y); k_params <- 3
      adj_r2 <- 1 - (1 - r2) * (n_obs - 1) / (n_obs - k_params)
      sigma2 <- ss_res / (n_obs - k_params)
      se_hat <- sqrt(diag(XtX_inv) * sigma2)
      t_vals <- b_hat / se_hat

      comp_pred_shares_d <- numeric(length(pred_d))
      for (s in unique(obs_mkt_state)) {
        s_idx <- which(obs_mkt_state == s)
        comp_pred_shares_d[s_idx] <- pred_d[s_idx] / sum(pred_d[s_idx])
      }
      ws_ss_res <- sum((obs_ws_shares - comp_pred_shares_d)^2)
      ws_tss <- sum((obs_ws_shares - mean(obs_ws_shares))^2)
      ws_r2 <- 1 - ws_ss_res / ws_tss

      residuals_ols <- as.numeric(y - y_hat)
      duan_smear <- mean(exp(residuals_ols))

      list(par = fit$par, coefs = b_hat, se = se_hat, t_vals = t_vals,
           r2 = r2, adj_r2 = adj_r2, ws_r2 = ws_r2, n = n_obs, sigma2 = sigma2,
           duan_smear = duan_smear, demand_idx = pred_d, y_hat = as.numeric(y_hat),
           pred_shares = comp_pred_shares_d, conv = fit$convergence)
    }, error = function(e) {
      cat(sprintf("  CG-%-11s: FAILED (%s)\n", dt, conditionMessage(e)))
      NULL
    })
    if (is.null(cg_iter_d)) next

    comp_grav_results_dest[[dt]] <- cg_iter_d

    cat(sprintf("  CG-%-11s: beta=%.4f, a_H=%.3f, a_D=%.3f, a_T=%.3f, R2(ln)=%.4f, WS-R2=%.4f\n",
                dt, cg_iter_d$par[1], cg_iter_d$par[2], cg_iter_d$par[3], cg_iter_d$par[4], cg_iter_d$r2, cg_iter_d$ws_r2))
  }

  # Best CG destination model
  cg_r2s_dest <- sapply(comp_grav_results_dest, function(x) x$r2)
  best_cg_type_dest <- names(which.max(cg_r2s_dest))
  best_cg_dest <- comp_grav_results_dest[[best_cg_type_dest]]

  cat(sprintf("\n  Best CG-dest model: %s (R2(ln)=%.4f, adj=%.4f, WS-R2=%.4f)\n",
              best_cg_type_dest, best_cg_dest$r2, best_cg_dest$adj_r2, best_cg_dest$ws_r2))
  cat("    Demand index params:\n")
  cat(sprintf("      beta=%.4f, a_hotel=%.3f (%.2fx), a_dest=%.3f (%.2fx), a_tables=%.3f (%.2fx)\n",
              best_cg_dest$par[1], best_cg_dest$par[2], exp(best_cg_dest$par[2]),
              best_cg_dest$par[3], exp(best_cg_dest$par[3]),
              best_cg_dest$par[4], exp(best_cg_dest$par[4])))
  cat("    OLS coefficients (t-values):\n")
  cat(sprintf("      Intercept:   %7.3f  (%.2f)\n", best_cg_dest$coefs[1], best_cg_dest$t_vals[1]))
  cat(sprintf("      ln(D_j):     %7.3f  (%.2f)  [demand elasticity]\n", best_cg_dest$coefs[2], best_cg_dest$t_vals[2]))
  cat(sprintf("      is_cardroom: %7.3f  (%.2f)  [exp(delta)=%.4f => %.1f%% of full-casino rev]\n",
              best_cg_dest$coefs[3], best_cg_dest$t_vals[3], exp(best_cg_dest$coefs[3]), exp(best_cg_dest$coefs[3]) * 100))
  cat(sprintf("    Duan smearing factor: %.4f\n", best_cg_dest$duan_smear))
  cat(sprintf("    Dest resort total multiplier: hotel(%.2fx) + dest(%.2fx) = %.2fx\n",
              exp(best_cg_dest$par[2]), exp(best_cg_dest$par[3]),
              exp(best_cg_dest$par[2] + best_cg_dest$par[3])))

  cat("\n  --- CG Model: Binary vs Rooms vs Destination Comparison ---\n")
  cat(sprintf("  %-16s  R2(ln)=%.4f  WS-R2=%.4f  (a_H=%.3f, mult=%.2fx)\n",
              paste0("Binary (", best_cg_type, ")"),
              best_cg$r2, best_cg$ws_r2, best_cg$par[2], exp(best_cg$par[2])))
  cat(sprintf("  %-16s  R2(ln)=%.4f  WS-R2=%.4f  (a_R=%.3f)\n",
              paste0("Rooms (", best_cg_type_rooms, ")"),
              best_cg_rooms$r2, best_cg_rooms$ws_r2, best_cg_rooms$par[2]))
  cat(sprintf("  %-16s  R2(ln)=%.4f  WS-R2=%.4f  (a_H=%.3f, a_D=%.3f)\n",
              paste0("Dest (", best_cg_type_dest, ")"),
              best_cg_dest$r2, best_cg_dest$ws_r2, best_cg_dest$par[2], best_cg_dest$par[3]))
  a_r_cg <- best_cg_rooms$par[2]
  cat(sprintf("  Implied CG multipliers:\n"))
  for (nr in c(14, 50, 100, 200, 500, 1000, 2798)) {
    cat(sprintf("    %4d rooms -> binary: %.2fx  rooms3: %.2fx\n",
                nr, exp(best_cg$par[2]), nr^a_r_cg))
  }

  # CG rooms predictions
  cg_pred_rev_rooms <- exp(best_cg_rooms$y_hat)
  cg_pred_shares_rooms <- best_cg_rooms$pred_shares

  # CG destination predictions
  cg_pred_rev_dest <- exp(best_cg_dest$y_hat)
  cg_pred_shares_dest <- best_cg_dest$pred_shares

  # =====================================================
  # COMPREHENSIVE BINARY vs ROOMS vs DESTINATION COMPARISON
  # =====================================================
  cat("\n  =============================================================\n")
  cat("  MODEL COMPARISON: BINARY vs ROOMS vs DESTINATION\n")
  cat("  =============================================================\n")
  cat(sprintf("  %-30s  %12s  %12s  %12s\n", "", "Binary", "Rooms", "Dest"))
  cat(sprintf("  %-30s  %12s  %12s  %12s\n", "------------------------------", "------------", "------------", "------------"))
  cat(sprintf("  %-30s  %12.4f  %12.4f  %12.4f\n", "Gravity Share R2:",
              comparison$R_squared[1], comparison_rooms$R_squared[1], comparison_dest$R_squared[1]))
  cat(sprintf("  %-30s  %12.4f  %12.4f  %12.4f\n", "CG Model R2(ln):",
              best_cg$r2, best_cg_rooms$r2, best_cg_dest$r2))
  cat(sprintf("  %-30s  %12.4f  %12.4f  %12.4f\n", "CG Model WS-R2:",
              best_cg$ws_r2, best_cg_rooms$ws_r2, best_cg_dest$ws_r2))
  cat(sprintf("  %-30s  %12.4f  %12.4f  %12.4f\n", "LN Model R2:",
              best_ln$r2, best_ln_rooms$r2, best_ln_dest$r2))
  cat(sprintf("  %-30s  %12.4f  %12.4f  %12.4f\n", "LN Model adj-R2:",
              best_ln$adj_r2, best_ln_rooms$adj_r2, best_ln_dest$adj_r2))
  cat(sprintf("  %-30s  %12.4f  %12.4f  %12.4f\n", "Distance decay (beta):",
              best_cg$par[1], best_cg_rooms$par[1], best_cg_dest$par[1]))
  cat(sprintf("  %-30s  a_H=%-7.3f  a_R=%-7.3f  a_H=%-7.3f\n", "Hotel param(s):",
              best_cg$par[2], best_cg_rooms$par[2], best_cg_dest$par[2]))
  cat(sprintf("  %-30s  %12s  %12s  a_D=%-7.3f\n", "Dest param:",
              "--", "--", best_cg_dest$par[3]))
  cat(sprintf("  %-30s  a_T=%-7.3f  a_T=%-7.3f  a_T=%-7.3f\n", "Tables param:",
              best_cg$par[3], best_cg_rooms$par[3], best_cg_dest$par[4]))
  cat("  Implied hotel multipliers:\n")
  a_h_bin <- best_cg$par[2]
  a_r <- best_cg_rooms$par[2]
  a_h_dest <- best_cg_dest$par[2]
  a_d_dest <- best_cg_dest$par[3]
  cat(sprintf("    %-22s  %12.2f  %12.2f  %12.2f\n", "50-room motel:", exp(a_h_bin), 50^a_r, exp(a_h_dest)))
  cat(sprintf("    %-22s  %12.2f  %12.2f  %12.2f\n", "200-room hotel:", exp(a_h_bin), 200^a_r, exp(a_h_dest)))
  cat(sprintf("    %-22s  %12.2f  %12.2f  %12.2f\n", "500-room resort:", exp(a_h_bin), 500^a_r, exp(a_h_dest)))
  cat(sprintf("    %-22s  %12.2f  %12.2f  %12.2f\n", "2000-room mega:", exp(a_h_bin), 2000^a_r, exp(a_h_dest + a_d_dest)))
  cat(sprintf("    (Dest resort casinos get hotel + dest boost = %.2fx)\n", exp(a_h_dest + a_d_dest)))

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

  # Rooms gravity share predictions
  best_gravity_model_rooms <- comparison_rooms$Model[1]
  best_gravity_params_rooms <- c(comparison_rooms$Beta[1], comparison_rooms$Alpha_Rooms[1],
                                  comparison_rooms$Alpha_Tables[1])
  predicted_rooms <- predict_mkt_rooms(best_gravity_params_rooms, tolower(best_gravity_model_rooms))
  pred_shares_rooms <- numeric(length(predicted_rooms))
  for (s in unique(obs_mkt_state)) {
    s_idx <- which(obs_mkt_state == s)
    pred_shares_rooms[s_idx] <- predicted_rooms[s_idx] / sum(predicted_rooms[s_idx])
  }

  # LN rooms predictions
  ln_pred_rev_rooms <- exp(best_ln_rooms$y_hat)
  ln_pred_shares_rooms <- numeric(length(ln_pred_rev_rooms))
  for (s in unique(obs_mkt_state)) {
    s_idx <- which(obs_mkt_state == s)
    ln_pred_shares_rooms[s_idx] <- ln_pred_rev_rooms[s_idx] / sum(ln_pred_rev_rooms[s_idx])
  }

  # Destination gravity share predictions
  best_gravity_model_dest <- comparison_dest$Model[1]
  best_gravity_params_dest <- c(comparison_dest$Beta[1], comparison_dest$Alpha_Hotel[1],
                                comparison_dest$Alpha_Dest[1], comparison_dest$Alpha_Tables[1])
  predicted_dest <- predict_mkt_dest(best_gravity_params_dest, tolower(best_gravity_model_dest))
  pred_shares_dest <- numeric(length(predicted_dest))
  for (s in unique(obs_mkt_state)) {
    s_idx <- which(obs_mkt_state == s)
    pred_shares_dest[s_idx] <- predicted_dest[s_idx] / sum(predicted_dest[s_idx])
  }

  # LN destination predictions
  ln_pred_rev_dest <- exp(best_ln_dest$y_hat)
  ln_pred_shares_dest <- numeric(length(ln_pred_rev_dest))
  for (s in unique(obs_mkt_state)) {
    s_idx <- which(obs_mkt_state == s)
    ln_pred_shares_dest[s_idx] <- ln_pred_rev_dest[s_idx] / sum(ln_pred_rev_dest[s_idx])
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

      # Competitive gravity model
      cg_mkt_ws_share <- cg_pred_shares[k]
      cg_prop_ws <- cg_mkt_ws_share * within_mkt_share[p]
      cg_prop_rev <- cg_pred_rev[k] * within_mkt_share[p]

      # Rooms-based models (3-param)
      rooms_grav_prop_ws <- pred_shares_rooms[k] * within_mkt_share[p]
      rooms_ln_prop_ws <- ln_pred_shares_rooms[k] * within_mkt_share[p]
      rooms_cg_prop_ws <- cg_pred_shares_rooms[k] * within_mkt_share[p]
      rooms_cg_prop_rev <- cg_pred_rev_rooms[k] * within_mkt_share[p]

      # Destination-based models (4-param)
      dest_grav_prop_ws <- pred_shares_dest[k] * within_mkt_share[p]
      dest_ln_prop_ws <- ln_pred_shares_dest[k] * within_mkt_share[p]
      dest_cg_prop_ws <- cg_pred_shares_dest[k] * within_mkt_share[p]
      dest_cg_prop_rev <- cg_pred_rev_dest[k] * within_mkt_share[p]

      property_rows[[length(property_rows) + 1]] <- data.frame(
        Casino_ID  = mem_rev$casino_id[p],
        Property   = mem_rev$name.y[p],
        City       = mem_rev$city.y[p],
        State      = mem_rev$state.y[p],
        Revenue_M  = round(mem_rev$total_revenue_2022[p] / 1e6, 1),
        State_Total_M = round(st_total / 1e6, 0),
        Actual_State_Pct = round(prop_actual_ws * 100, 2),
        Pred_State_Pct   = round(prop_pred_ws * 100, 2),
        LN_Pred_State_Pct = round(ln_prop_ws * 100, 2),
        CG_Pred_State_Pct = round(cg_prop_ws * 100, 2),
        CG_Pred_Rev_M     = round(cg_prop_rev / 1e6, 1),
        Rooms_Grav_Pct   = round(rooms_grav_prop_ws * 100, 2),
        Rooms_LN_Pct     = round(rooms_ln_prop_ws * 100, 2),
        Rooms_CG_Pct     = round(rooms_cg_prop_ws * 100, 2),
        Rooms_CG_Rev_M   = round(rooms_cg_prop_rev / 1e6, 1),
        Dest_Grav_Pct    = round(dest_grav_prop_ws * 100, 2),
        Dest_LN_Pct      = round(dest_ln_prop_ws * 100, 2),
        Dest_CG_Pct      = round(dest_cg_prop_ws * 100, 2),
        Dest_CG_Rev_M    = round(dest_cg_prop_rev / 1e6, 1),
        stringsAsFactors = FALSE
      )
    }
  }

  prop_table <- do.call(rbind, property_rows)
  prop_table$Error_pp <- round(prop_table$Pred_State_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$LN_Error_pp <- round(prop_table$LN_Pred_State_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$CG_Error_pp <- round(prop_table$CG_Pred_State_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$Rooms_Grav_Error_pp <- round(prop_table$Rooms_Grav_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$Rooms_LN_Error_pp <- round(prop_table$Rooms_LN_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$Rooms_CG_Error_pp <- round(prop_table$Rooms_CG_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$Dest_Grav_Error_pp <- round(prop_table$Dest_Grav_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$Dest_LN_Error_pp <- round(prop_table$Dest_LN_Pct - prop_table$Actual_State_Pct, 2)
  prop_table$Dest_CG_Error_pp <- round(prop_table$Dest_CG_Pct - prop_table$Actual_State_Pct, 2)
  prop_table <- prop_table[order(-prop_table$Revenue_M), ]

  mkt_mape <- round(mean(abs(pred_shares * 100 - obs_ws_shares * 100)), 2)
  prop_mape <- round(mean(abs(prop_table$Error_pp)), 2)
  ln_prop_mape <- round(mean(abs(prop_table$LN_Error_pp)), 2)
  cg_prop_mape <- round(mean(abs(prop_table$CG_Error_pp)), 2)
  rooms_grav_prop_mape <- round(mean(abs(prop_table$Rooms_Grav_Error_pp)), 2)
  rooms_ln_prop_mape <- round(mean(abs(prop_table$Rooms_LN_Error_pp)), 2)
  rooms_cg_prop_mape <- round(mean(abs(prop_table$Rooms_CG_Error_pp)), 2)
  dest_grav_prop_mape <- round(mean(abs(prop_table$Dest_Grav_Error_pp)), 2)
  dest_ln_prop_mape <- round(mean(abs(prop_table$Dest_LN_Error_pp)), 2)
  dest_cg_prop_mape <- round(mean(abs(prop_table$Dest_CG_Error_pp)), 2)
  cat("\n  --- Property-Level MAPE (pp) ---\n")
  cat(sprintf("  %-20s  %8s  %8s  %8s\n", "", "Binary", "Rooms", "Dest"))
  cat(sprintf("  %-20s  %8s  %8s  %8s\n", "--------------------", "--------", "--------", "--------"))
  cat(sprintf("  %-20s  %8.2f  %8.2f  %8.2f\n", "Gravity Share:", prop_mape, rooms_grav_prop_mape, dest_grav_prop_mape))
  cat(sprintf("  %-20s  %8.2f  %8.2f  %8.2f\n", "LN:", ln_prop_mape, rooms_ln_prop_mape, dest_ln_prop_mape))
  cat(sprintf("  %-20s  %8.2f  %8.2f  %8.2f\n", "CG:", cg_prop_mape, rooms_cg_prop_mape, dest_cg_prop_mape))

  # Return all results
  list(
    label      = model_label,
    radius     = opt_radius,
    # Binary specification results
    comparison = comparison,
    best_model = best_gravity_model,
    best_params = best_gravity_params,
    results    = results,
    ln_results = ln_results,
    best_ln    = best_ln,
    best_ln_type = best_ln_type,
    comp_grav_results = comp_grav_results,
    best_cg    = best_cg,
    best_cg_type = best_cg_type,
    # Rooms (3-param power-function) specification results
    comparison_rooms = comparison_rooms,
    best_model_rooms = best_gravity_model_rooms,
    best_params_rooms = best_gravity_params_rooms,
    results_rooms = results_rooms,
    ln_results_rooms = ln_results_rooms,
    best_ln_rooms = best_ln_rooms,
    best_ln_type_rooms = best_ln_type_rooms,
    comp_grav_results_rooms = comp_grav_results_rooms,
    best_cg_rooms = best_cg_rooms,
    best_cg_type_rooms = best_cg_type_rooms,
    # Destination (4-param hotel+dest) specification results
    comparison_dest = comparison_dest,
    best_model_dest = best_gravity_model_dest,
    best_params_dest = best_gravity_params_dest,
    results_dest = results_dest,
    ln_results_dest = ln_results_dest,
    best_ln_dest = best_ln_dest,
    best_ln_type_dest = best_ln_type_dest,
    comp_grav_results_dest = comp_grav_results_dest,
    best_cg_dest = best_cg_dest,
    best_cg_type_dest = best_cg_type_dest,
    # Shared
    markets    = markets,
    obs_mkts   = obs_mkts,
    obs_ws_shares = obs_ws_shares,
    obs_mkt_state = obs_mkt_state,
    pred_shares = pred_shares,
    ln_pred_shares = ln_pred_shares,
    cg_pred_shares = cg_pred_shares,
    cg_pred_rev = cg_pred_rev,
    pred_shares_rooms = pred_shares_rooms,
    ln_pred_shares_rooms = ln_pred_shares_rooms,
    cg_pred_shares_rooms = cg_pred_shares_rooms,
    cg_pred_rev_rooms = cg_pred_rev_rooms,
    pred_shares_dest = pred_shares_dest,
    ln_pred_shares_dest = ln_pred_shares_dest,
    cg_pred_shares_dest = cg_pred_shares_dest,
    cg_pred_rev_dest = cg_pred_rev_dest,
    prop_table = prop_table,
    mkt_mape   = mkt_mape,
    prop_mape  = prop_mape,
    ln_prop_mape = ln_prop_mape,
    cg_prop_mape = cg_prop_mape,
    rooms_grav_prop_mape = rooms_grav_prop_mape,
    rooms_ln_prop_mape = rooms_ln_prop_mape,
    rooms_cg_prop_mape = rooms_cg_prop_mape,
    dest_grav_prop_mape = dest_grav_prop_mape,
    dest_ln_prop_mape = dest_ln_prop_mape,
    dest_cg_prop_mape = dest_cg_prop_mape,
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
# 9. RUN MODEL A: ALL 21 STATES
#    Uses all states with property-level revenue data.
#    All revenue standardized to 2024$ (study states inflated from 2022$).
#    Tribal casinos (IA, WA) and unmatched properties remain as unobserved
#    supply-side controls.
# =============================================================================

cat("\n")
cat("#######################################################################\n")
cat("# MODEL A: 21-STATE MODEL (ALL PROPERTY-REVENUE STATES)             #\n")
cat("#   All 16 states with 2024$ revenue data                           #\n")
cat("#   6 override states (2024 data)                                   #\n")
cat("#   Tribal casinos as supply controls (not observed)                #\n")
cat("#   WA/CA cardrooms flagged with is_cardroom indicator              #\n")
cat("#######################################################################\n")

# Allow external scripts to exclude specific states from Model A
if (!exists("EXCLUDE_STATES")) EXCLUDE_STATES <- character(0)
if (length(EXCLUDE_STATES) > 0) {
  cat("\n  *** EXCLUDING STATES:", paste(EXCLUDE_STATES, collapse=", "), "***\n")
  revenue_data <- revenue_data[!revenue_data$state %in% EXCLUDE_STATES, ]
  primary_states <- setdiff(primary_states, EXCLUDE_STATES)
}

cat("\n  Primary states:", paste(primary_states, collapse=", "), "\n")
cat("  Border states:", paste(border_states, collapse=", "), "\n")
cat("  Total properties with revenue:", nrow(revenue_data), "\n")

n_states_label <- length(primary_states)
model_a <- run_estimation(
  rev_data    = revenue_data,
  model_label = paste0(n_states_label, "-State Model (all property-revenue states)"),
  casinos_df  = casinos_study,
  zips_df     = zips_study,
  pop_inc     = pop_income,
  radii       = c(5, 10),
  save_prefix = "21state"
)

# Save model_a for lightweight plotting scripts
if (!dir.exists("results")) dir.create("results")
saveRDS(model_a, "results/model_a.rds")

# =============================================================================
# 10. RUN MODEL B: 10 ORIGINAL STUDY STATES ONLY (SENSITIVITY CHECK)
# =============================================================================

# Allow external scripts to skip Model B for faster execution
if (!exists("SKIP_MODEL_B")) SKIP_MODEL_B <- FALSE

if (!SKIP_MODEL_B) {
cat("\n")
cat("#######################################################################\n")
cat("# MODEL B: 10-STATE MODEL (ORIGINAL STUDY STATES ONLY)              #\n")
cat("#   Sensitivity check: original 10 states with 2022 data             #\n")
cat("#   (inflated to 2024$) for comparison with 16-state model.          #\n")
cat("#######################################################################\n")

original_study_states <- c("PA", "OH", "MD", "NY", "MA", "CT", "IN", "MO", "IA", "WA")
revenue_data_10state <- revenue_data[revenue_data$state %in% original_study_states, ]
cat("\n  Kept", nrow(revenue_data_10state), "properties from original 10 study states\n")

model_b <- run_estimation(
  rev_data    = revenue_data_10state,
  model_label = "10-State Model (original study states)",
  casinos_df  = casinos_study,
  zips_df     = zips_study,
  pop_inc     = pop_income,
  radii       = c(5, 10),
  save_prefix = "10state_original"
)

# =============================================================================
# 11. COMPARISON TABLE
# =============================================================================

cat("\n")
cat("=======================================================================\n")
cat("  MODEL COMPARISON: 21-STATE vs 10-STATE\n")
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
cat("\n  Attractiveness multipliers (21-State / 10-State):\n")
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
  b_CR   = round(c(model_a$best_ln$coefs[5], model_b$best_ln$coefs[5]), 4),
  R2     = round(c(model_a$best_ln$r2, model_b$best_ln$r2), 4),
  Adj_R2 = round(c(model_a$best_ln$adj_r2, model_b$best_ln$adj_r2), 4),
  MAPE   = c(model_a$ln_prop_mape, model_b$ln_prop_mape),
  stringsAsFactors = FALSE
)
names(comp_ln) <- c("Specification", "N", "Decay", "Beta", "gamma",
                     "Hotel", "Tables", "Cardroom", "R2(ln)", "Adj-R2(ln)", "WS Prop MAPE")
print_table(comp_ln, "LOG-LINEAR (LN) MODEL COMPARISON (WS Prop MAPE on within-state shares)")

# --- Competitive Gravity Revenue Model Comparison ---
comp_cg <- data.frame(
  Spec   = c(model_a$label, model_b$label),
  N      = c(nrow(model_a$rev_data), nrow(model_b$rev_data)),
  Decay  = c(model_a$best_cg_type, model_b$best_cg_type),
  Beta   = round(c(model_a$best_cg$par[1], model_b$best_cg$par[1]), 4),
  a_H    = round(c(model_a$best_cg$par[2], model_b$best_cg$par[2]), 4),
  a_T    = round(c(model_a$best_cg$par[3], model_b$best_cg$par[3]), 4),
  Gamma  = round(c(model_a$best_cg$coefs[2], model_b$best_cg$coefs[2]), 4),
  Delta  = round(c(model_a$best_cg$coefs[3], model_b$best_cg$coefs[3]), 4),
  R2_ln  = round(c(model_a$best_cg$r2, model_b$best_cg$r2), 4),
  WS_R2  = round(c(model_a$best_cg$ws_r2, model_b$best_cg$ws_r2), 4),
  MAPE   = c(model_a$cg_prop_mape, model_b$cg_prop_mape),
  stringsAsFactors = FALSE
)
names(comp_cg) <- c("Specification", "N", "Decay", "Beta", "a_Hotel", "a_Tables",
                     "gamma", "cardroom", "R2(ln)", "WS-R2", "WS Prop MAPE")
print_table(comp_cg, "COMPETITIVE GRAVITY REVENUE MODEL COMPARISON")

cat("\n  CG Model Coefficients (21-State, best decay):\n")
cg_a <- model_a$best_cg
cat(sprintf("    Demand index: beta=%.4f, a_hotel=%.3f (%.2fx), a_tables=%.3f (%.2fx)\n",
            cg_a$par[1], cg_a$par[2], exp(cg_a$par[2]), cg_a$par[3], exp(cg_a$par[3])))
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "OLS Variable", "Coef", "SE", "t-val"))
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "--------", "----", "--", "-----"))
cg_vnames <- c("Intercept", "ln(D_j)", "is_cardroom")
for (v in seq_along(cg_vnames)) {
  cat(sprintf("    %-15s  %8.4f  %8.4f  %8.2f\n", cg_vnames[v], cg_a$coefs[v], cg_a$se[v], cg_a$t_vals[v]))
}
cat(sprintf("    Cardroom rev ratio: exp(%.3f) = %.4f => cardrooms earn ~%.1f%% of equivalent full-casino\n",
            cg_a$coefs[3], exp(cg_a$coefs[3]), exp(cg_a$coefs[3]) * 100))

# --- LN Coefficients detail for Model A ---
cat("\n  LN Model Coefficients (21-State, best decay):\n")
ln_a <- model_a$best_ln
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "Variable", "Coef", "SE", "t-val"))
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "--------", "----", "--", "-----"))
vnames <- c("Intercept", "ln(Demand)", "Hotel", "Tables", "is_cardroom")
for (v in seq_along(vnames)) {
  cat(sprintf("    %-15s  %8.4f  %8.4f  %8.2f\n", vnames[v], ln_a$coefs[v], ln_a$se[v], ln_a$t_vals[v]))
}

# --- ROOMS (power-function) specification comparison ---
cat("\n")
cat("=======================================================================\n")
cat("  ROOMS (POWER-FUNCTION) SPECIFICATION: 21-STATE vs 10-STATE\n")
cat("=======================================================================\n\n")

# Gravity Share Rooms
comp_gravity_rooms <- data.frame(
  Spec   = c(model_a$label, model_b$label),
  N      = c(nrow(model_a$rev_data), nrow(model_b$rev_data)),
  Decay  = c(model_a$best_model_rooms, model_b$best_model_rooms),
  Beta   = round(c(model_a$best_params_rooms[1], model_b$best_params_rooms[1]), 4),
  a_R    = round(c(model_a$best_params_rooms[2], model_b$best_params_rooms[2]), 4),
  a_T    = round(c(model_a$best_params_rooms[3], model_b$best_params_rooms[3]), 4),
  R2     = round(c(model_a$comparison_rooms$R_squared[1], model_b$comparison_rooms$R_squared[1]), 4),
  MAPE   = c(model_a$rooms_grav_prop_mape, model_b$rooms_grav_prop_mape),
  stringsAsFactors = FALSE
)
names(comp_gravity_rooms) <- c("Specification", "N", "Decay", "Beta",
                                "a_Rooms", "a_Tables", "WS R2", "WS Prop MAPE")
print_table(comp_gravity_rooms, "GRAVITY SHARE MODEL (ROOMS) COMPARISON")

# CG Rooms
comp_cg_rooms <- data.frame(
  Spec   = c(model_a$label, model_b$label),
  N      = c(nrow(model_a$rev_data), nrow(model_b$rev_data)),
  Decay  = c(model_a$best_cg_type_rooms, model_b$best_cg_type_rooms),
  Beta   = round(c(model_a$best_cg_rooms$par[1], model_b$best_cg_rooms$par[1]), 4),
  a_R    = round(c(model_a$best_cg_rooms$par[2], model_b$best_cg_rooms$par[2]), 4),
  a_T    = round(c(model_a$best_cg_rooms$par[3], model_b$best_cg_rooms$par[3]), 4),
  Gamma  = round(c(model_a$best_cg_rooms$coefs[2], model_b$best_cg_rooms$coefs[2]), 4),
  Delta  = round(c(model_a$best_cg_rooms$coefs[3], model_b$best_cg_rooms$coefs[3]), 4),
  R2_ln  = round(c(model_a$best_cg_rooms$r2, model_b$best_cg_rooms$r2), 4),
  WS_R2  = round(c(model_a$best_cg_rooms$ws_r2, model_b$best_cg_rooms$ws_r2), 4),
  MAPE   = c(model_a$rooms_cg_prop_mape, model_b$rooms_cg_prop_mape),
  stringsAsFactors = FALSE
)
names(comp_cg_rooms) <- c("Specification", "N", "Decay", "Beta", "a_Rooms", "a_Tables",
                           "gamma", "cardroom", "R2(ln)", "WS-R2", "WS Prop MAPE")
print_table(comp_cg_rooms, "COMPETITIVE GRAVITY REVENUE MODEL (ROOMS) COMPARISON")

# CG Rooms coefficients for Model A
cat("\n  CG-Rooms Model Coefficients (21-State, best decay):\n")
cg_r <- model_a$best_cg_rooms
cat(sprintf("    Demand index: beta=%.4f, a_rooms=%.3f, a_tables=%.3f\n",
            cg_r$par[1], cg_r$par[2], cg_r$par[3]))
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "OLS Variable", "Coef", "SE", "t-val"))
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "--------", "----", "--", "-----"))
for (v in seq_along(cg_vnames)) {
  cat(sprintf("    %-15s  %8.4f  %8.4f  %8.2f\n", cg_vnames[v], cg_r$coefs[v], cg_r$se[v], cg_r$t_vals[v]))
}

# LN Rooms
cat("\n  LN-Rooms Model Coefficients (21-State, best decay):\n")
ln_r <- model_a$best_ln_rooms
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "Variable", "Coef", "SE", "t-val"))
cat(sprintf("    %-15s  %8s  %8s  %8s\n", "--------", "----", "--", "-----"))
vnames_r <- c("Intercept", "ln(Demand)", "ln(Rooms)", "Tables", "is_cardroom")
for (v in seq_along(vnames_r)) {
  cat(sprintf("    %-15s  %8.4f  %8.4f  %8.2f\n", vnames_r[v], ln_r$coefs[v], ln_r$se[v], ln_r$t_vals[v]))
}

# --- FINAL BINARY vs ROOMS SUMMARY ---
cat("\n")
cat("=======================================================================\n")
cat("  FINAL COMPARISON: BINARY (has_hotel) vs ROOMS (power-function)\n")
cat("  Using 21-State Model A (best decay for each specification)\n")
cat("=======================================================================\n\n")
cat(sprintf("  %-30s  %12s  %12s\n", "", "Binary", "Rooms"))
cat(sprintf("  %-30s  %12s  %12s\n", "------------------------------", "------------", "------------"))
cat(sprintf("  %-30s  %12.4f  %12.4f\n", "Gravity Share R2:",
            model_a$comparison$R_squared[1], model_a$comparison_rooms$R_squared[1]))
cat(sprintf("  %-30s  %12.2f  %12.2f\n", "Gravity Prop MAPE (pp):",
            model_a$prop_mape, model_a$rooms_grav_prop_mape))
cat(sprintf("  %-30s  %12.4f  %12.4f\n", "LN R2:",
            model_a$best_ln$r2, model_a$best_ln_rooms$r2))
cat(sprintf("  %-30s  %12.2f  %12.2f\n", "LN Prop MAPE (pp):",
            model_a$ln_prop_mape, model_a$rooms_ln_prop_mape))
cat(sprintf("  %-30s  %12.4f  %12.4f\n", "CG R2(ln):",
            model_a$best_cg$r2, model_a$best_cg_rooms$r2))
cat(sprintf("  %-30s  %12.4f  %12.4f\n", "CG WS-R2:",
            model_a$best_cg$ws_r2, model_a$best_cg_rooms$ws_r2))
cat(sprintf("  %-30s  %12.2f  %12.2f\n", "CG Prop MAPE (pp):",
            model_a$cg_prop_mape, model_a$rooms_cg_prop_mape))
cat("\n  CG Implied hotel multipliers:\n")
a_h_bin_final <- model_a$best_cg$par[2]
a_r_final <- model_a$best_cg_rooms$par[2]
cat(sprintf("  %-30s  %12s  %12s\n", "", "Binary", "Rooms^a"))
for (nr in c(14, 50, 100, 200, 500, 1000, 2798)) {
  cat(sprintf("  %-30s  %12.2f  %12.2f\n",
              paste0(nr, "-room:"),
              exp(a_h_bin_final), nr^a_r_final))
}
cat(sprintf("\n  Rooms exponent (a_R) = %.4f (CG model)\n", a_r_final))
cat(sprintf("  Interpretation: rooms^%.3f captures diminishing returns to hotel scale\n", a_r_final))

# =============================================================================
# 12. CROSS-VALIDATION (LOOCV AND LEAVE-ONE-STATE-OUT)
#     Uses Model A (16-state) with the best gravity and LN specifications.
#     Both CV procedures re-estimate parameters on the training fold,
#     then predict the held-out market(s).
# =============================================================================
# NOTE: Set run_cv <- TRUE to run cross-validation (slow). Skipped for quick iteration.
run_cv <- FALSE
if (run_cv) {

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
  m_hotel <- m_tables <- m_cardroom <- integer(nm)
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
    # is_cardroom: 1 only if ALL casinos in cluster are cardrooms
    if ("is_cardroom" %in% names(casinos_cv)) {
      m_cardroom[k] <- as.integer(all(casinos_cv$is_cardroom[idx] == 1))
    }
  }

  n_z <- nrow(zips_cv)
  dmat <- matrix(NA_real_, nrow = n_z, ncol = nm)
  for (j in 1:nm) {
    dmat[, j] <- haversine(zips_cv$longitude, zips_cv$latitude, m_lon[j], m_lat[j])
  }

  list(market_id = mids, n = nm, lat = m_lat, lon = m_lon,
       rev = m_rev, obs = m_obs, hotel = m_hotel, tables = m_tables,
       cardroom = m_cardroom, state = m_state, dist_mat = dmat,
       casinos = casinos_cv)
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
  obs_cardroom <- mkt$cardroom[obs_idx]

  # Check if cardroom has variation (avoids singular matrix when e.g. WA held out)
  has_cardroom_var <- length(unique(obs_cardroom)) > 1

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
    if (has_cardroom_var) {
      X <- cbind(1, log(di_obs), obs_hotel, obs_tables, obs_cardroom)
    } else {
      X <- cbind(1, log(di_obs), obs_hotel, obs_tables)
    }
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
  if (has_cardroom_var) {
    X <- cbind(1, log(di_obs), obs_hotel, obs_tables, obs_cardroom)
    b_hat <- as.numeric(solve(crossprod(X)) %*% crossprod(X, y))
  } else {
    X <- cbind(1, log(di_obs), obs_hotel, obs_tables)
    b4 <- as.numeric(solve(crossprod(X)) %*% crossprod(X, y))
    b_hat <- c(b4, 0)  # pad cardroom coef = 0
  }

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
    cardroom_ho <- train_mkt$cardroom[ho_idx_train]
    ln_pred_rev_ho <- exp(ln_fit$coefs[1] + ln_fit$coefs[2] * log(di_ho) +
                          ln_fit$coefs[3] * hotel_ho + ln_fit$coefs[4] * tables_ho +
                          ln_fit$coefs[5] * cardroom_ho)

    # Within-state LN predicted share
    # Get predicted revenue for all observed same-state markets + held-out
    same_state_obs_idx <- which(train_mkt$obs & train_mkt$state == ho_state)
    ln_state_rev <- numeric(length(same_state_obs_idx))
    for (h in seq_along(same_state_obs_idx)) {
      di_h <- ln_fit$demand_idx_all[same_state_obs_idx[h]]
      if (di_h > 0) {
        ln_state_rev[h] <- exp(ln_fit$coefs[1] + ln_fit$coefs[2] * log(di_h) +
                                ln_fit$coefs[3] * train_mkt$hotel[same_state_obs_idx[h]] +
                                ln_fit$coefs[4] * train_mkt$tables[same_state_obs_idx[h]] +
                                ln_fit$coefs[5] * train_mkt$cardroom[same_state_obs_idx[h]])
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
# For each of the 10 primary states:
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
                                ln_fit$coefs[4] * train_mkt$tables[ho_indices[h]] +
                                ln_fit$coefs[5] * train_mkt$cardroom[ho_indices[h]])
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

} # end if (run_cv)

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
    "IA" = "#bcbd22", "WA" = "#17becf"
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

# Cross-validation (only if run_cv was TRUE)
if (run_cv) {
  write.csv(loso_df, "results/loso_cv_results.csv", row.names = FALSE)
  write.csv(cv_summary, "results/cv_summary.csv", row.names = FALSE)
}

cat("  All results saved to results/ directory\n")

} # end if (!SKIP_MODEL_B)

cat("\n=== ANALYSIS COMPLETE ===\n")
