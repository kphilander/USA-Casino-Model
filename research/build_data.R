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

# ---- CPI-U inflation adjustment to 2026 dollars ----
# BLS CPI-U Annual Averages (All Urban Consumers, US City Average, 1982-84=100)
# Source: https://www.bls.gov/cpi/ and https://fred.stlouisfed.org/series/CPIAUCNS
cpi_annual <- c("2022" = 292.655, "2023" = 304.702, "2024" = 313.7)
cpi_2026   <- 326.6   # Jan 2026 CPI-U from FRED (proxy for 2026 annual avg)
cat(sprintf("  CPI-U factors: 2022->2026 = %.4f, 2024->2026 = %.4f\n",
            cpi_2026 / cpi_annual[["2022"]], cpi_2026 / cpi_annual[["2024"]]))

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
allzips_map <- allzips_map[!is.na(allzips_map$latitude) & !is.na(allzips_map$longitude), ]
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
  city = as.character(casinos_study$city),
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
  city = "Elizabeth", state = "IN", latitude = 38.1796, longitude = -85.9054,
  hotel = 1, tribal = "Commercial", stringsAsFactors = FALSE
))

# Inject Terre Haute Casino (opened 2024, IN)
casinos_study <- rbind(casinos_study, data.frame(
  casino_id = 9998, name = "Terre Haute Casino",
  city = "Terre Haute", state = "IN", latitude = 39.4667, longitude = -87.4139,
  hotel = 0, tribal = "Commercial", stringsAsFactors = FALSE
))

# Inject PA mini-casinos (opened after 2022)
casinos_study <- rbind(casinos_study, data.frame(
  casino_id = c(9003, 9004, 9005),
  name = c("Hollywood Casino York", "Hollywood Casino Morgantown", "Parx Casino Shippensburg"),
  city = c("York", "Morgantown", "Shippensburg"),
  state = rep("PA", 3),
  latitude  = c(39.9526, 40.1551, 40.0512),
  longitude = c(-76.7275, -75.8855, -77.5199),
  hotel = rep(0, 3), tribal = rep("Commercial", 3), stringsAsFactors = FALSE
))

# Inject NY new racinos (opened after 2022)
casinos_study <- rbind(casinos_study, data.frame(
  casino_id = c(9001, 9002),
  name = c("Nassau OTB VGM Facility", "Resorts World Hudson Valley"),
  city = c("Plainview", "Newburgh"),
  state = rep("NY", 2),
  latitude  = c(40.7770, 41.5034),
  longitude = c(-73.4674, -74.0104),
  hotel = rep(0, 2), tribal = rep("Commercial", 2), stringsAsFactors = FALSE
))

# Inject WA cardrooms not in base casinodata.rds
wa_inject <- data.frame(
  casino_id = c(10001, 10004, 10005, 10006, 10007, 10008, 10009, 10010, 10011),
  name = c("Fortune Casino Renton", "New Phoenix",
           "Aces Poker Lakewood", "Caribbean Cardroom",
           "Clearwater Saloon and Casino", "Imperial Palace Tukwila",
           "Dragon Tiger Casino", "Maverick Casino", "Grand Casino"),
  city = c("Renton", "La Center", "Lakewood", "Kirkland", "Ellensburg",
           "Tukwila", "Tukwila", "Lakewood", "Lakewood"),
  state = rep("WA", 9),
  latitude  = c(47.4720, 45.8620, 47.1718, 47.6769, 47.4240,
                47.4740, 47.4740, 47.1718, 47.1718),
  longitude = c(-122.2060, -122.6710, -122.5185, -122.1780, -120.3100,
                -122.2610, -122.2610, -122.5185, -122.5185),
  hotel = rep(0, 9),
  tribal = rep("Commercial", 9),
  stringsAsFactors = FALSE
)
casinos_study <- rbind(casinos_study, wa_inject)

# Inject LA racetracks not in base casinodata.rds
la_racetracks <- data.frame(
  casino_id = c(10100, 10101),
  name = c("Delta Downs Racetrack", "Louisiana Downs"),
  city = c("Vinton", "Bossier City"),
  state = c("LA", "LA"),
  latitude  = c(30.1919, 32.5102),
  longitude = c(-93.5810, -93.7127),
  hotel = c(1, 0),
  tribal = c("Commercial", "Commercial"),
  stringsAsFactors = FALSE
)
casinos_study <- rbind(casinos_study, la_racetracks)

# Fix WA tribal misclassifications (tagged "Commercial" in casinodata but are tribal)
tribal_fix_ids <- c(943, 1117, 12)  # Muckleshoot, Quil Ceda Creek, 7 Cedars
casinos_study$tribal[casinos_study$casino_id %in% tribal_fix_ids] <- "Tribal"
cat("  Injected Terre Haute IN, 3 PA mini-casinos, 2 NY racinos, 9 WA cardrooms, 2 LA racetracks\n")
cat("  Fixed", length(tribal_fix_ids), "WA tribal misclassifications\n")

# ---- Geocode corrections ----
# HERE API geocoding has systemic errors: it often matched casino names to
# unrelated streets (e.g. "Binion's Gambling Hall" -> "Hall Ave" 13 mi away).
# Corrections verified Feb 2026 via property addresses and satellite imagery.
coord_fixes <- data.frame(
  id = c(
    # CRITICAL: >5 mi error, high-revenue or wrong state
    872,   # Mark Twain Casino — 313 mi! Placed in Michigan instead of Missouri
    1370,  # Treasure Bay Casino — 158 mi! Madison MS instead of Biloxi
    1153,  # Resorts World NYC — 9.6 mi, $645M revenue, wrong borough
    938,   # MotorCity Casino Detroit — 8.7 mi, $386M revenue
    116,   # Binion's Gambling Hall — 13.8 mi, Mountain's Edge instead of Fremont
    1374,  # Treasure Island LV — 8.7 mi, matched "Treasure Island Rd"
    491,   # Four Queens — 9.1 mi, matched Queensridge subdivision
    1268,  # Silverton Casino — 12.9 mi, matched "Silverton Dr"
    534,   # Golden Gate Casino — 6.9 mi, matched "Golden Gate Ln"
    1042,  # Plaza Hotel Casino — 6.0 mi
    1025,  # Parx Casino — 6.6 mi, $599M revenue
    887,   # MGM Grand Detroit — 5.2 mi, $604M revenue
    656,   # Horseshoe Hammond — 6.1 mi, $346M revenue
    865,   # Majestic Star Gary — 5.4 mi, $419M revenue
    528,   # Gold Strike Tunica — 11.4 mi, $160M at town centroid
    657,   # Horseshoe Tunica — 11.1 mi, $120M at town centroid
    1261,  # Silver Slipper Bay St Louis — 6.5 mi
    334,   # Cromwell LV — 5.2 mi, matched "Vegas Dr"
    # IMPORTANT: LV Strip cluster (15 casinos at same point 36.1189,-115.1728)
    58,    # Aria
    73,    # Bally's LV
    481,   # Flamingo LV
    599,   # Harrah's LV
    826,   # Luxor
    988,   # Orleans (off-Strip, 2.5 mi error)
    1011,  # Palazzo
    1279,  # Sahara/SLS LV
    1398,  # Venetian
    1424,  # Westgate LV
    1462,  # Wynn Encore
    # IMPORTANT: LV downtown/other
    1321,  # STRAT — 1.9 mi from actual
    1388,  # Tuscany Suites — 4.1 mi from actual
    294,   # Circa Resort
    1012,  # Palms Casino — 4.1 mi
    1017,  # Paris LV — 3.0 mi
    184,   # California Hotel Casino — 4.2 mi
    442,   # El Cortez — 3.1 mi
    544,   # Golden Nugget LV — 2.5 mi
    780,   # Live! Casino MD — 2.7 mi, $705M revenue
    1172,  # Riverwalk Casino Vicksburg — 3.6 mi
    863,   # Magnolia Bluffs Natchez — 3.4 mi
    # IMPORTANT: Biloxi/MS coast centroid cluster
    543,   # Golden Nugget Biloxi — 2.2 mi
    1008,  # Palace Casino Biloxi — 2.7 mi
    596,   # Harrah's Gulf Coast — 1.8 mi
    674,   # Island View Gulfport — 1.3 mi
    1212,  # Scarlet Pearl D'Iberville — 1.5 mi
    # Iowa
    366    # Diamond Jo Worth — 6.6 mi, lon off (43.4469,-93.3533 vs actual 43.4502,-93.2225)
  ),
  lat = c(
    # CRITICAL
    40.0358, 30.3940, 40.6731, 42.3387, 36.1716, 36.1246, 36.1699, 36.0415,
    36.1711, 36.1718, 40.1186, 42.3333, 41.6943, 41.6390, 34.8465, 34.8454,
    30.2394, 36.1150,
    # LV Strip cluster
    36.1073, 36.1085, 36.1097, 36.1179, 36.0955, 36.1026, 36.1240, 36.1424,
    36.1195, 36.1356, 36.1265,
    # LV downtown/other
    36.1475, 36.1131, 36.1720, 36.1087, 36.1125, 36.1705, 36.1691, 36.1702,
    39.1562, 32.2993, 31.5633,
    # Biloxi cluster
    30.3912, 30.3964, 30.3931, 30.3636, 30.4250,
    # Iowa
    43.4502
  ),
  lon = c(
    # CRITICAL
    -91.4998, -88.9553, -73.8330, -83.0676, -115.1443, -115.1720, -115.1437,
    -115.1837, -115.1465, -115.1465, -74.9528, -83.0600, -87.5070, -87.4280,
    -90.3326, -90.3298, -89.4256, -115.1719,
    # LV Strip cluster
    -115.1770, -115.1700, -115.1690, -115.1677, -115.1761, -115.2019, -115.1680,
    -115.1567, -115.1680, -115.1522, -115.1657,
    # LV downtown/other
    -115.1567, -115.1599, -115.1454, -115.1903, -115.1707, -115.1400, -115.1389,
    -115.1450, -76.7279, -90.9122, -91.4068,
    # Biloxi cluster
    -88.8610, -88.8606, -88.8660, -89.1027, -88.8906,
    # Iowa
    -93.2225
  ),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(coord_fixes)) {
  idx <- which(casinos_study$casino_id == coord_fixes$id[i])
  if (length(idx) > 0) {
    casinos_study$latitude[idx]  <- coord_fixes$lat[i]
    casinos_study$longitude[idx] <- coord_fixes$lon[i]
  }
}
cat("  Geocode corrections applied:", nrow(coord_fixes), "casinos\n")

# Hotel overrides (verified for 10-state 2024 analysis)
casinos_study$has_hotel <- casinos_study$hotel
casinos_study$has_hotel[casinos_study$casino_id == 1060] <- 0
casinos_study$has_hotel[casinos_study$casino_id == 865]  <- 1  # Hard Rock Northern IN hotel now open
casinos_study$has_hotel[casinos_study$casino_id == 1167] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 83]   <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1162] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 557]  <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1155] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1170] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 366]  <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1434] <- 1
casinos_study$has_hotel[casinos_study$casino_id == 1435] <- 1

# ---- Hotel/tables corrections: non-study states ----
# Verified Feb 2026 via property websites, state gaming commission data,
# and industry knowledge. Study states (PA/OH/MD/NY/MA/CT/IN/MO/IA/WA)
# were already verified from property-level revenue data above.

# -- Hotel corrections: set has_hotel = 1 (currently 0 in database) --

# AR: All 3 casinos expanded with hotels post-2019
casinos_study$has_hotel[casinos_study$casino_id %in% c(966, 1165, 1289)] <- 1
# WV: Mountaineer Casino (350+ room hotel)
casinos_study$has_hotel[casinos_study$casino_id == 941] <- 1
# WY: Wind River Hotel & Casino (90-room hotel)
casinos_study$has_hotel[casinos_study$casino_id == 1450] <- 1
# LA: Cypress Bayou (339), Eldorado Shreveport (446), Paragon (1016)
casinos_study$has_hotel[casinos_study$casino_id %in% c(339, 446, 1016)] <- 1
# MS: Scarlet Pearl (1212, 300-room hotel)
casinos_study$has_hotel[casinos_study$casino_id == 1212] <- 1
# MN: Jackpot Junction (700, 276-room hotel)
casinos_study$has_hotel[casinos_study$casino_id == 700] <- 1
# ND: 4 of 5 tribal casinos have hotels
casinos_study$has_hotel[casinos_study$casino_id %in% c(8, 344, 1054, 1294)] <- 1
# CO: Monarch Casino (909, 516-room hotel opened 2021)
casinos_study$has_hotel[casinos_study$casino_id == 909] <- 1
# NM: Buffalo Thunder (166), Inn of Mountain Gods (667), Route 66 (1182), Santa Ana Star (1204)
casinos_study$has_hotel[casinos_study$casino_id %in% c(166, 667, 1182, 1204)] <- 1
# OK: Major tribal resorts with hotels
casinos_study$has_hotel[casinos_study$casino_id %in% c(
  61,    # Artesian Hotel Casino
  275,   # Choctaw Casino & Resort Durant
  430,   # Downstream Casino Resort
  555,   # Grand Casino Hotel & Resort
  665,   # Indigo Sky Casino & Resort
  818,   # Lucky Star Casino Concho
  1164,  # River Spirit Casino Resort
  1173,  # Riverwind Casino
  1363,  # Tonkawa Hotel & Casino
  1457   # WinStar World Casino & Resort
)] <- 1
# SD: Tribal and deadwood hotels
casinos_study$has_hotel[casinos_study$casino_id %in% c(
  345, 487, 564, 1058, 1186, 1266
)] <- 1
# OR: Spirit Mountain (896), Seven Feathers (1235), Three Rivers (1295)
casinos_study$has_hotel[casinos_study$casino_id %in% c(896, 1235, 1295)] <- 1
# ID: Coeur d'Alene Casino (306), Shoshone-Bannock Fort Hall (1245)
casinos_study$has_hotel[casinos_study$casino_id %in% c(306, 1245)] <- 1
# WI: Ho-Chunk (628), Potawatomi (1046), Lake of the Torches (762)
casinos_study$has_hotel[casinos_study$casino_id %in% c(628, 1046, 762)] <- 1
# CA tribal: Major resorts with hotels
casinos_study$has_hotel[casinos_study$casino_id %in% c(
  79,    # Barona Resort and Casino (397 rooms)
  124,   # Black Oak Casino Resort (157 rooms)
  175,   # Cache Creek Casino Resort (200+ rooms)
  290,   # Chukchansi Gold Resort (192 rooms)
  291,   # Chumash Casino Resort (320 rooms)
  569,   # Graton Resort & Casino (200 rooms)
  1115,  # Quechan Casino Resort (166 rooms)
  1199,  # Yaamava'/San Manuel (432 rooms, opened 2022)
  1291,  # Spa Resort Casino Palm Springs
  1335   # Table Mountain Casino (new facility with hotel, 2023)
)] <- 1
# NV: Hotels missing from database
casinos_study$has_hotel[casinos_study$casino_id %in% c(
  1152,  # Resorts World Las Vegas (3,506 rooms, opened 2021)
  440,   # Edgewater Laughlin (~1,000 rooms)
  709,   # Jailhouse Casino & Motel Ely
  1121,  # Railroad Pass Hotel & Casino (~120 rooms)
  1137,  # Red Garter Hotel Virginia City
  1421   # Wendover Nugget Hotel (~500 rooms)
)] <- 1

# -- Hotel corrections: set has_hotel = 0 (currently 1 in database) --

# WY: Shoshone Rose Casino (no hotel)
casinos_study$has_hotel[casinos_study$casino_id == 1241] <- 0
# KS: Boot Hill Casino (no hotel despite "Resort" in some listings)
casinos_study$has_hotel[casinos_study$casino_id == 141] <- 0
# LA: Boomtown New Orleans (140, no hotel), Evangeline Downs (458, racino no hotel)
casinos_study$has_hotel[casinos_study$casino_id %in% c(140, 458)] <- 0
# MS: Tropicana Greenville (1378, no hotel)
casinos_study$has_hotel[casinos_study$casino_id == 1378] <- 0
# MN: Seven Clans Red Lake (1233, no true hotel)
casinos_study$has_hotel[casinos_study$casino_id == 1233] <- 0
# NV: Properties without hotels (incorrectly flagged as having one)
casinos_study$has_hotel[casinos_study$casino_id %in% c(
  230,   # Casino Fandango Carson City
  336,   # Crystal Bay Club Casino
  369,   # Diamond's Casino Reno
  520,   # Gold Dust West Carson City
  1048   # Pony Express Casino
)] <- 0

# ---- Load and merge hotel room counts ----
# Verified hotel room counts from WorldCasinoDirectory.com scraping + 6 rounds
# of manual verification. 695 casinos with room counts (229 with rooms > 0).
hotel_rooms_data <- read.csv("data/wcd_hotel_rooms.csv", stringsAsFactors = FALSE)

# Apply all 6 rounds of manual corrections
override_files <- list.files("data", pattern = "^manual_room_overrides_v[0-9]+\\.csv$",
                             full.names = TRUE)
for (ovf in override_files) {
  ov <- read.csv(ovf, stringsAsFactors = FALSE, comment.char = "#")
  if (nrow(ov) == 0) next
  for (i in 1:nrow(ov)) {
    idx_ov <- which(hotel_rooms_data$casino_id == ov$casino_id[i])
    if (length(idx_ov) > 0) {
      hotel_rooms_data$hotel_rooms[idx_ov] <- ov$hotel_rooms_override[i]
    }
  }
  cat(sprintf("  Applied %s: %d overrides\n", basename(ovf), nrow(ov)))
}

casinos_study <- merge(casinos_study, hotel_rooms_data[, c("casino_id", "hotel_rooms")],
                       by = "casino_id", all.x = TRUE)
casinos_study$hotel_rooms[is.na(casinos_study$hotel_rooms)] <- 0

# Set hotel_rooms for injected casinos (not in WCD data)
casinos_study$hotel_rooms[casinos_study$casino_id == 9999] <- 503   # Caesars Southern Indiana
casinos_study$hotel_rooms[casinos_study$casino_id == 9998] <- 0     # Terre Haute Casino
casinos_study$hotel_rooms[casinos_study$casino_id == 10100] <- 0    # Delta Downs
casinos_study$hotel_rooms[casinos_study$casino_id == 10101] <- 0    # Louisiana Downs

# Compute log transform for power-function specification
casinos_study$ln_hotel_rooms <- ifelse(casinos_study$hotel_rooms > 0,
                                        log(casinos_study$hotel_rooms), 0)

cat("  Hotel rooms loaded:", sum(casinos_study$hotel_rooms > 0), "casinos with rooms,",
    "median:", median(casinos_study$hotel_rooms[casinos_study$hotel_rooms > 0]), "\n")

# has_tables: default 1, override known slots-only from 9-state revenue data
casinos_study$has_tables <- 1

# Load 9-state property-level data to identify slots-only facilities
# AND build combined revenue_data for revenue-weighted market centroids
revenue_files <- c("pa", "oh", "md", "ny", "ma", "ct", "in", "mo", "ia", "wa")
slots_only_ids <- c()
revenue_data_list <- list()
for (f in revenue_files) {
  rev <- read.csv(paste0("data/", f, "_revenue_2024.csv"), stringsAsFactors = FALSE)
  rev <- rev[!is.na(rev$casino_id), ]
  # Rename 2024 columns to _2022 suffix (downstream code uses _2022 throughout)
  names(rev)[names(rev) == "total_revenue_2024"]  <- "total_revenue_2022"
  names(rev)[names(rev) == "table_revenue_2024"]  <- "table_revenue_2022"
  names(rev)[names(rev) == "slots_revenue_2024"]  <- "slots_revenue_2022"
  # CT table revenue scaling: only slots GGR is publicly reported for tribal casinos.
  # Table game revenue is estimated using 32.2% table share derived from Mohegan Sun's
  # FY24 SEC filings (the only CT tribal casino with public segment disclosure).
  if (f == "ct") {
    rev$table_revenue_2022 <- round(rev$slots_revenue_2022 * 0.322 / (1 - 0.322))
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
  if (f == "ny") slots_only_ids <- c(slots_only_ids, rev$casino_id[rev$facility_type == "racino"])
  if (f == "ma") slots_only_ids <- c(slots_only_ids, rev$casino_id[grepl("Plainridge", rev$name, ignore.case = TRUE)])
}
slots_only_ids <- unique(slots_only_ids[!is.na(slots_only_ids)])
casinos_study$has_tables[casinos_study$casino_id %in% slots_only_ids] <- 0
revenue_data <- do.call(rbind, revenue_data_list)
revenue_data <- revenue_data[!is.na(revenue_data$total_revenue_2022), ]

# ---- Additional state property-level revenue data ----
# Only states where government regulators publish actual property-level GGR.
# Estimated/allocated splits (MS, AR, DE, RI, WV) are excluded — those states
# are projected by the gravity model using AGA state totals instead.
extra_rev_files <- list.files("data",
  pattern = "^(il|nj|la|mi|me|ks)_revenue",
  full.names = TRUE)
extra_rev_files <- extra_rev_files[grepl("\\.csv$", extra_rev_files)]
extra_rev_data <- list()
for (f in extra_rev_files) {
  rev <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(rev)) next
  rev <- rev[!is.na(rev$casino_id), ]
  # Normalize column name: new files use total_revenue; old use total_revenue_2022
  rev_col <- intersect(c("total_revenue", "total_revenue_2022"), names(rev))
  if (length(rev_col) == 0) next
  rev$total_revenue_2022 <- rev[[rev_col[1]]]
  # Only keep records with casino_ids in casinos_study
  rev <- rev[rev$casino_id %in% casinos_study$casino_id, ]
  if (nrow(rev) == 0) next
  extra_rev_data[[f]] <- rev[, c("casino_id", "state", "total_revenue_2022")]
  # Update has_tables from actual table revenue data
  tbl_col <- intersect(c("table_revenue", "table_revenue_2022"), names(rev))
  if (length(tbl_col) > 0) {
    for (i in 1:nrow(rev)) {
      trev <- rev[[tbl_col[1]]][i]
      if (!is.na(trev) && trev == 0) {
        casinos_study$has_tables[casinos_study$casino_id == rev$casino_id[i]] <- 0
      }
    }
  }
  st <- toupper(gsub("_revenue.*", "", basename(f)))
  cat(sprintf("  Loaded %s: %d properties, $%.1fM total\n",
              basename(f), nrow(rev), sum(rev$total_revenue_2022, na.rm = TRUE) / 1e6))
}
if (length(extra_rev_data) > 0) {
  extra_df <- do.call(rbind, extra_rev_data)
  extra_df <- extra_df[!is.na(extra_df$total_revenue_2022), ]
  revenue_data <- rbind(revenue_data, extra_df)
  cat("  Extra state revenue records added:", nrow(extra_df), "\n")
}

# ---- Tables corrections: non-study states (slots/VLT-only facilities) ----
# These facilities have gaming machines only; no live table games.
# Verified Feb 2026 via property websites and state gaming commission data.

non_study_slots_only <- c(
  # NE: Iron Horse Bar slots only
  671,
  # WY: Little Wind (779), Shoshone Rose (1241) — slots only
  779, 1241,
  # FL: Calder Casino (182) — slots only, no table games
  182,
  # NJ: Hard Rock Meadowlands (584) — sports betting only, not a casino
  584,
  # LA: Evangeline Downs (458), Fair Grounds (460) — racinos, slots/VLT only
  458, 460,
  # AZ: Desert Diamond Why (361) — slots only, no tables
  #     We-Ko-Pa (1420) — resort only, minimal/no gaming
  361, 1420,
  # NM: Billy the Kid (114), Downs at ABQ (429), Kicks 66 (734) — racinos/slots only
  114, 429, 734,
  # NV Strip/Downtown: Slots-only bars/taverns that passed convenience filter
  832,   # Longhorn Casino (bar/slots)
  1244,  # Silver Sevens (slots focused)
  1326,  # Terrible's Hotel & Casino (slots focused)
  # NV Reno/Rural: Small bars/taverns/restaurants — slots only (no table games)
  68,    # Aztec Inn
  112,   # Big Wheel Casino
  113,   # Bighorn Casino
  137,   # Bonanza Lounge & Casino
  150,   # Boulder Lodge Casino
  218,   # Cash on Delivery Casino
  243,   # Casino Valle Verde
  254,   # Charlie's Lakeside Casino
  255,   # Charlies Down Under
  292,   # Churchill Springs Casino
  524,   # Gold N' Silver Inn
  548,   # Golden West Restaurant Casino
  575,   # Greens Gaming & Dining
  650,   # Hooters Spring Valley
  670,   # Irene's Casino
  716,   # Joe's Tavern Casino
  760,   # Lakeside Casino & RV Park
  821,   # Lucky Strike Casino Carson City
  871,   # Mark Twain Casino
  908,   # Molly's Buffalo
  947,   # Nevada Nugget
  971,   # Oasis Springs Casino
  1262,  # Silver Springs Nugget
  1269,  # Siri's Casino
  1272,  # Skinny Dugans Casino & Lounge
  1278,  # Slot World Casino
  1282,  # Snow Mountain Smoke Shop
  1299,  # Sportsmans Royal Manor
  1312,  # StageStop Restaurant Lounge & Casino
  1346,  # Terrible's Casino Searchlight
  1429,  # Wig Wam Restaurant & Casino
  1452   # Wingers Roadhouse Casino
)
casinos_study$has_tables[casinos_study$casino_id %in% non_study_slots_only] <- 0

cat("  Non-study-state hotel corrections applied\n")
cat("  Non-study-state tables corrections:", length(non_study_slots_only), "slots-only properties flagged\n")

# is_cardroom: WA and CA commercial venues are cardrooms by state law (no slot machines)
casinos_study$is_cardroom <- 0
wa_revenue <- read.csv("data/wa_revenue_2024.csv", stringsAsFactors = FALSE)
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
  363, 446, 541, 602,       # Diamond Jacks, Eldorado/Bally's, Golden Nugget, Caesars NOLA
  629, 653, 678,            # Queen BR, Horseshoe Bossier, Horseshoe LC (ex-Isle of Capri)
  749, 827, 870,            # L'Auberge BR, L'Auberge LC, Margaritaville
  458, 460, 10100, 10101,   # Evangeline Downs, Fair Grounds, Delta Downs, Louisiana Downs
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

# CLOSED/DEMOLISHED: No longer operating, remove from competition
closed_ids <- c(
  # MS Tunica closures
  4,     # 1st Jackpot Casino Tunica
  72,    # Bally's Casino Tunica
  479,   # Fitz Casino Tunica
  537,   # Golden Harbor Casino
  643,   # Hollywood Casino Tunica
  680,   # Isle of Capri Lula
  1151,  # Resorts Casino Tunica
  1384,  # Tunica Roadhouse
  # LA
  363,   # Diamond Jacks Bossier City (closed)
  # NV Strip/Downtown closures
  186,   # Castaways (closed)
  438,   # Eastside Cannery (closed)
  468,   # Fiesta Henderson (closed)
  469,   # Fiesta Rancho (closed)
  585,   # Greek Isles/IP Vacations (closed)
  902,   # Mirage (closed 2024 for Hard Rock redevelopment)
  1349,  # Texas Station (closed)
  1381,  # Tropicana Las Vegas (closed/demolished 2024)
  # NV Reno/Tahoe/Rural closures
  1337,  # Tahoe Biltmore (demolished)
  1249,  # Sierra Sids (closed)
  # SD
  893    # Old West Casino (closed)
)
casinos_study$is_convenience[casinos_study$casino_id %in% closed_ids] <- 1

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

# ---- Destination resort flag ----
# Rule 1: >= 1500 rooms (independent mega-resort)
# Rule 2: >= 500 rooms AND in a major casino cluster or urban tourist destination
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
cat("  Destination resorts flagged:", sum(casinos_study$destination_resort), "of",
    nrow(casinos_study), "casinos\n")

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
  has_hotel = integer(n_mkts), hotel_rooms = numeric(n_mkts),
  destination_resort = integer(n_mkts),
  has_tables = integer(n_mkts),
  is_cardroom = integer(n_mkts),
  state = character(n_mkts), label = character(n_mkts),
  stringsAsFactors = FALSE
)

for (k in seq_along(market_ids)) {
  mid <- market_ids[k]
  idx <- which(casinos_study$market_id == mid)

  # Revenue-weighted centroid (matches run_analysis.R and forecast_new_site.R)
  # NOTE: merge() reorders rows by casino_id, so use mem_rev's own lat/lon
  # columns (not casinos_study[idx]) to keep weights aligned with coordinates.
  mem_rev <- merge(casinos_study[idx, , drop = FALSE],
                   revenue_data[, c("casino_id", "total_revenue_2022")],
                   by = "casino_id", all.x = TRUE)
  mem_rev$total_revenue_2022[is.na(mem_rev$total_revenue_2022)] <- 0
  tot_rev <- sum(mem_rev$total_revenue_2022)
  has_rev <- tot_rev > 0

  if (has_rev) {
    wts <- mem_rev$total_revenue_2022
    lat <- sum(mem_rev$latitude * wts) / sum(wts)
    lon <- sum(mem_rev$longitude * wts) / sum(wts)
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
  markets$hotel_rooms[k] <- sum(casinos_study$hotel_rooms[idx], na.rm = TRUE)
  markets$destination_resort[k] <- as.integer(any(casinos_study$destination_resort[idx] == 1, na.rm = TRUE))
  markets$has_tables[k] <- as.integer(any(casinos_study$has_tables[idx] == 1, na.rm = TRUE))
  # Market is_cardroom: 1 only if ALL casinos in cluster are cardrooms
  markets$is_cardroom[k] <- as.integer(all(casinos_study$is_cardroom[idx] == 1))
  markets$state[k] <- mkt_st
  markets$label[k] <- lbl
  markets$observed[k] <- FALSE
  markets$revenue[k] <- 0
  markets$has_commercial[k] <- as.integer(any(casinos_study$tribal[idx] == "Commercial"))

  # Commercial attractiveness share within cluster
  # A_i = exp(alpha_H * has_hotel_i + alpha_T * tables_i) per casino
  cas_attract <- exp(0.7660 * casinos_study$has_hotel[idx] +
                     0.5800 * casinos_study$has_tables[idx])
  comm_mask <- casinos_study$tribal[idx] == "Commercial"
  total_attract <- sum(cas_attract)
  comm_attract  <- sum(cas_attract[comm_mask])
  markets$commercial_share[k] <- if (total_attract > 0) comm_attract / total_attract else 0
}

# Compute ln_hotel_rooms for markets (power-function specification)
markets$ln_hotel_rooms <- ifelse(markets$hotel_rooms > 0, log(markets$hotel_rooms), 0)

cat("  Markets:", n_mkts, "(", sum(markets$hotel_rooms > 0), "with hotel rooms )\n")

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

cg_beta    <- 2.8985
cg_a_hotel <- 0.7660
cg_a_table <- 0.5800
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

# Snapshot gravity-proportional AGA allocation before property-level overrides.
# This preserves the demand-based state-share estimate for comparison in the app
# (displayed as "State-Share Estimate" alongside reported revenue).
markets$gravity_rev <- markets$revenue

# ---- Allocate property-level revenue directly to markets ----
# For states with property-level data outside the 8 calibrated study states,
# override gravity-proportional AGA allocation with actual observed revenue.
# The 8 study states (PA, OH, MD, NY, MA, IN, MO, IA) keep AGA 2024 + gravity
# allocation since the CG model was calibrated on those states.
# CT (tribal, not in AGA) and WA (cardrooms) are always overridden.
# New extra states (IL, NJ, LA, MS, MI, FL, WV, AR, DE, RI, ME, KS, NE)
# benefit from property-level accuracy over gravity estimation.
calibrated_study_states <- c("PA", "OH", "MD", "NY", "MA", "IN", "MO", "IA")
override_states <- setdiff(unique(revenue_data$state), calibrated_study_states)

for (st in override_states) {
  st_rev <- revenue_data[revenue_data$state == st, ]
  st_casinos <- casinos_study[casinos_study$casino_id %in% st_rev$casino_id, ]
  if (nrow(st_casinos) == 0) next

  # Zero out gravity-allocated revenue for ALL markets in this state first.
  # Without this, markets without property data retain inflated gravity estimates
  # (e.g., NJ Meadowlands kept $2.8B gravity allocation while AC got $2.8B from
  # property data, doubling NJ total to $5.6B vs correct $3.1B).
  st_all_mkts <- which(markets$state == st)
  markets$revenue[st_all_mkts] <- 0
  markets$observed[st_all_mkts] <- FALSE

  st_market_ids <- unique(st_casinos$market_id)
  n_mkts_updated <- 0
  for (mid in st_market_ids) {
    mkt_idx <- which(markets$market_id == mid)
    if (length(mkt_idx) == 0) next
    cas_in_mkt <- casinos_study[casinos_study$market_id == mid, ]
    mkt_rev <- sum(st_rev$total_revenue_2022[st_rev$casino_id %in% cas_in_mkt$casino_id],
                   na.rm = TRUE)
    if (mkt_rev > 0) {
      markets$revenue[mkt_idx] <- mkt_rev
      markets$observed[mkt_idx] <- TRUE
      n_mkts_updated <- n_mkts_updated + 1
    }
  }
  n_zeroed <- length(st_all_mkts) - n_mkts_updated
  cat(sprintf("  %s property-level revenue: %d markets updated, $%.0fM",
              st, n_mkts_updated,
              sum(st_rev$total_revenue_2022, na.rm = TRUE) / 1e6))
  if (n_zeroed > 0) cat(sprintf(" (%d markets zeroed)", n_zeroed))
  cat("\n")
}

# Markets not observed (no AGA data OR zeroed by override) should have NA gravity_rev
# so the app shows "N/A" instead of a misleading "$0M" state-share estimate.
markets$gravity_rev[!markets$observed] <- NA_real_

# For override states, gravity_rev should reflect the property-level market totals,
# not the stale pre-override AGA gravity allocation.  This keeps the State-Share
# Estimate in the UI consistent with the cluster total display.
override_observed <- markets$state %in% override_states & markets$observed
markets$gravity_rev[override_observed] <- markets$revenue[override_observed]

# Tag revenue data source for each market (used in cannibalization display).
# Priority: Reported (property-level) > State-Share Est. (AGA gravity) > Model Est.
markets$revenue_source <- NA_character_
markets$revenue_source[markets$observed & markets$state %in% calibrated_study_states] <- "State-Share Est."
markets$revenue_source[override_observed] <- "Reported"

# Summary
aga_total <- sum(aga_revenue$land_based_casino_rev_M) * 1e6
calibrated_rev <- sum(markets$revenue[markets$state %in% calibrated_study_states])
override_rev   <- sum(markets$revenue[markets$state %in% override_states])
tribal_pseudo_total <- sum(markets$tribal_pseudo)
cat(sprintf("  Calibrated states (AGA gravity): $%.2fB\n", calibrated_rev / 1e9))
cat(sprintf("  Override states (property-level): $%.2fB\n", override_rev / 1e9))
cat(sprintf("  Total allocated: $%.2fB (AGA total: $%.2fB)\n",
    (calibrated_rev + override_rev) / 1e9, aga_total / 1e9))
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

# Update state_revenue for override states to match actual market revenue sums.
# For calibrated study states, state_revenue = AGA total (used as calibration target).
# For override states, state_revenue must equal the property-level sum so that
# the forecast engine's calibration factor (actual / model) is consistent.
for (st in override_states) {
  st_total <- sum(markets$revenue[markets$state == st & markets$observed], na.rm = TRUE)
  if (st_total <= 0) next
  if (st %in% state_revenue$state) {
    state_revenue$revenue[state_revenue$state == st] <- st_total
    cat("  Updated", st, "state_revenue to match market totals: $",
        round(st_total / 1e6), "M\n")
  } else {
    state_revenue <- rbind(state_revenue,
                           data.frame(state = st, revenue = st_total,
                                      stringsAsFactors = FALSE))
    cat("  Added", st, "to state_revenue from property-level data: $",
        round(st_total / 1e6), "M\n")
  }
}

# ---- Inflate all revenue to 2026 dollars (CPI-U adjusted) ----
# AGA state totals (2024) and override property data (2024) share the same factor.
# Study-state gravity allocation also derives from AGA 2024 totals.
cpi_factor_2024 <- cpi_2026 / cpi_annual[["2024"]]
markets$revenue       <- markets$revenue * cpi_factor_2024
markets$gravity_rev   <- ifelse(is.na(markets$gravity_rev), NA_real_,
                                markets$gravity_rev * cpi_factor_2024)
markets$tribal_pseudo <- markets$tribal_pseudo * cpi_factor_2024
state_revenue$revenue <- state_revenue$revenue * cpi_factor_2024
cat(sprintf("  Inflated to 2026$: markets & state_revenue (factor %.4f from 2024)\n",
            cpi_factor_2024))

# ---- Model parameters ----
# From 16-state CG model (power decay) with binary hotel specification
# and is_cardroom indicator. Estimated on 186 properties, all revenue in 2024$.
# Attractiveness: A_j = exp(a_hotel * has_hotel_j + a_table * has_tables_j)
model_params <- list(
  cg_beta           = 2.8985,
  cg_a_hotel        = 0.7660,
  cg_a_table        = 0.5800,
  # Intercept adjusted from 2024 calibration to 2026 dollars:
  # original 6.685 + log(CPI_2026 / CPI_2024) shifts all predictions to 2026$
  cg_intercept      = 6.685 + log(cpi_2026 / cpi_annual[["2024"]]),
  cg_gamma          = 0.9160,
  cg_cardroom_delta = -2.0090,
  MAX_DIST          = 150,
  # Duan (1983) smearing estimator for retransformation bias correction.
  # When predicting from ln(Rev) = intercept + gamma*ln(D) + delta*is_cardroom,
  # the correct retransformation is E[Rev] = exp(X'B) * duan_smear.
  # Computed from OLS residuals of the 16-state CG model (N=138 markets).
  # For calibrated states this cancels out (same factor in numerator/denominator),
  # but matters for uncalibrated states where raw model predictions are used.
  cg_duan_smear     = 1.3726,
  # Destination resort revenue premium: applied as a multiplier in forecast engine only.
  # Derived from LN destination model coefficient (1.391, t=3.99), which captures the
  # revenue premium for destination resorts after controlling for gravity-based demand,
  # hotel status, tables, and cardroom status.
  cg_dest_premium   = exp(1.391)
)

# ---- Store actual revenue per property (for display in app) ----
# Attach property-level revenue and its data year to casinos_study.
# This enables the app to show "Actual Revenue: $X (YYYY)" for known properties.
casinos_study$actual_revenue <- NA_real_
casinos_study$revenue_year   <- NA_integer_

# 10 study states: revenue is from 2024
study_states_2024 <- c("PA", "OH", "MD", "NY", "MA", "CT", "IN", "MO", "IA", "WA")
for (i in 1:nrow(revenue_data)) {
  if (!revenue_data$state[i] %in% study_states_2024) next
  idx <- which(casinos_study$casino_id == revenue_data$casino_id[i])
  if (length(idx) > 0 && !is.na(revenue_data$total_revenue_2022[i])) {
    casinos_study$actual_revenue[idx] <- revenue_data$total_revenue_2022[i]
    casinos_study$revenue_year[idx]   <- 2024L
  }
}

# Extra state files: get year from revenue_year column or filename
for (f in extra_rev_files) {
  rev <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(rev)) next
  rev <- rev[!is.na(rev$casino_id), ]
  rev_col <- intersect(c("total_revenue", "total_revenue_2022"), names(rev))
  if (length(rev_col) == 0) next
  for (i in 1:nrow(rev)) {
    idx <- which(casinos_study$casino_id == rev$casino_id[i])
    if (length(idx) == 0) next
    val <- rev[[rev_col[1]]][i]
    if (is.na(val)) next
    casinos_study$actual_revenue[idx] <- val
    # Determine year
    if ("revenue_year" %in% names(rev) && !is.na(rev$revenue_year[i])) {
      casinos_study$revenue_year[idx] <- as.integer(rev$revenue_year[i])
    } else {
      yr_match <- regmatches(basename(f), regexpr("[0-9]{4}", basename(f)))
      if (length(yr_match) > 0) casinos_study$revenue_year[idx] <- as.integer(yr_match)
    }
  }
}

n_with_rev <- sum(!is.na(casinos_study$actual_revenue))
cat("  Properties with actual revenue:", n_with_rev, "/", nrow(casinos_study), "\n")
cat("  Revenue years:", paste(sort(unique(na.omit(casinos_study$revenue_year))),
    collapse = ", "), "\n")

# Inflate actual_revenue to 2026$ using each property's data year
n_inflated <- 0
for (i in which(!is.na(casinos_study$actual_revenue))) {
  yr <- as.character(casinos_study$revenue_year[i])
  if (!is.na(yr) && yr %in% names(cpi_annual)) {
    casinos_study$actual_revenue[i] <- casinos_study$actual_revenue[i] *
      cpi_2026 / cpi_annual[[yr]]
    n_inflated <- n_inflated + 1
  }
}
cat(sprintf("  Inflated %d property actual_revenue values to 2026$\n", n_inflated))

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
