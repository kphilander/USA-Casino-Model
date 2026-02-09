# =============================================================================
# Distance Decay Estimation: Distance Calculations
# =============================================================================
# This script calculates distances between all ZIP codes and all casinos
# in the study region.
#
# Author: Kahlil Philander
# Date: February 2026
# =============================================================================

library(dplyr)
library(tidyr)
library(geosphere)  # For Haversine distance

# =============================================================================
# 1. LOAD PREPARED DATA
# =============================================================================

zips <- readRDS("data/zips_study.rds")
casinos_all <- readRDS("data/casinos_study.rds")

cat("Loaded", nrow(zips), "ZIP codes\n")
cat("Loaded", nrow(casinos_all), "casinos\n")

# =============================================================================
# 2. HAVERSINE DISTANCE FUNCTION
# =============================================================================

#' Calculate Haversine distance between two points
#' @param lon1, lat1 Coordinates of point 1
#' @param lon2, lat2 Coordinates of point 2
#' @return Distance in miles
haversine_miles <- function(lon1, lat1, lon2, lat2) {
  # Convert to meters then to miles
  dist_meters <- distHaversine(
    cbind(lon1, lat1),
    cbind(lon2, lat2)
  )
  dist_meters / 1609.344  # Convert to miles
}

# =============================================================================
# 3. CALCULATE ALL ZIP-CASINO DISTANCES
# =============================================================================

cat("\nCalculating distances for", nrow(zips), "x", nrow(casinos_all), "=",
    nrow(zips) * nrow(casinos_all), "pairs...\n")

# Create all combinations of ZIPs and casinos
zip_casino_pairs <- expand_grid(
  zip_idx = 1:nrow(zips),
  casino_idx = 1:nrow(casinos_all)
)

# Add coordinates
zip_casino_pairs <- zip_casino_pairs %>%
  mutate(
    zipcode = zips$zipcode[zip_idx],
    zip_state = zips$state[zip_idx],
    zip_lat = zips$latitude[zip_idx],
    zip_lon = zips$longitude[zip_idx],
    zip_pop = zips$adultpop[zip_idx],
    zip_income = zips$income[zip_idx],

    casino_id = casinos_all$casino_id[casino_idx],
    casino_name = casinos_all$name[casino_idx],
    casino_state = casinos_all$state[casino_idx],
    casino_lat = casinos_all$latitude[casino_idx],
    casino_lon = casinos_all$longitude[casino_idx]
  )

# Calculate distances
cat("Computing Haversine distances...\n")
zip_casino_pairs <- zip_casino_pairs %>%
  mutate(
    distance_miles = haversine_miles(zip_lon, zip_lat, casino_lon, casino_lat)
  )

cat("Distance calculation complete.\n")
cat("Distance range:", round(min(zip_casino_pairs$distance_miles), 1), "to",
    round(max(zip_casino_pairs$distance_miles), 1), "miles\n")

# =============================================================================
# 4. APPLY DISTANCE THRESHOLD
# =============================================================================

# Define maximum travel distance (in miles)
MAX_DISTANCE <- 150  # Typical casino catchment area

# Filter to pairs within threshold
zip_casino_accessible <- zip_casino_pairs %>%
  filter(distance_miles <= MAX_DISTANCE)

cat("\nAfter", MAX_DISTANCE, "mile threshold:\n")
cat("  Pairs retained:", nrow(zip_casino_accessible),
    "(", round(100 * nrow(zip_casino_accessible) / nrow(zip_casino_pairs), 1), "%)\n")

# Check coverage - each ZIP should have at least one accessible casino
zips_with_casino <- zip_casino_accessible %>%
  group_by(zipcode) %>%
  summarise(n_casinos = n(), .groups = "drop")

cat("  ZIPs with ≥1 casino:", nrow(zips_with_casino), "\n")
cat("  Avg casinos per ZIP:", round(mean(zips_with_casino$n_casinos), 1), "\n")

# =============================================================================
# 5. CREATE COMPETITION SET FOR EACH ZIP
# =============================================================================

# For each ZIP, identify all accessible casinos (the choice set)
zip_choice_sets <- zip_casino_accessible %>%
  group_by(zipcode, zip_state, zip_pop, zip_income) %>%
  summarise(
    n_accessible_casinos = n(),
    nearest_casino = casino_name[which.min(distance_miles)],
    nearest_distance = min(distance_miles),
    casino_ids = list(casino_id),
    casino_distances = list(distance_miles),
    .groups = "drop"
  )

cat("\n=== CHOICE SET SUMMARY ===\n")
cat("ZIPs with choice sets:", nrow(zip_choice_sets), "\n")
cat("Accessible casinos per ZIP:\n")
print(summary(zip_choice_sets$n_accessible_casinos))

# =============================================================================
# 6. SAVE DISTANCE DATA
# =============================================================================

# Save full distance matrix (within threshold)
saveRDS(zip_casino_accessible, "data/zip_casino_distances.rds")

# Save choice sets
saveRDS(zip_choice_sets, "data/zip_choice_sets.rds")

# Also save as sparse format for efficiency
distance_matrix <- zip_casino_accessible %>%
  select(zipcode, casino_id, distance_miles) %>%
  pivot_wider(
    names_from = casino_id,
    values_from = distance_miles,
    names_prefix = "casino_"
  )

saveRDS(distance_matrix, "data/distance_matrix_sparse.rds")

cat("\n=== SAVED DISTANCE DATA ===\n")
cat("  - zip_casino_distances.rds (all pairs within", MAX_DISTANCE, "miles)\n")
cat("  - zip_choice_sets.rds (choice set summary per ZIP)\n")
cat("  - distance_matrix_sparse.rds (wide format)\n")

# =============================================================================
# 7. SUMMARY STATISTICS
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("DISTANCE SUMMARY STATISTICS\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")

# Distance distribution
cat("\nDistance distribution (miles):\n")
print(summary(zip_casino_accessible$distance_miles))

# By state
cat("\nMean distance to nearest casino by state:\n")
nearest_by_state <- zip_choice_sets %>%
  group_by(zip_state) %>%
  summarise(
    n_zips = n(),
    mean_nearest_dist = round(mean(nearest_distance), 1),
    median_nearest_dist = round(median(nearest_distance), 1),
    .groups = "drop"
  ) %>%
  arrange(mean_nearest_dist)

print(nearest_by_state)

# =============================================================================
# 8. VISUALIZATION DATA
# =============================================================================

# Create data for mapping
map_data <- list(
  zips = zips %>%
    left_join(zip_choice_sets %>% select(zipcode, nearest_distance), by = "zipcode"),
  casinos = casinos_all,
  threshold = MAX_DISTANCE
)

saveRDS(map_data, "data/map_data.rds")

cat("\n=== NEXT STEP ===\n")
cat("Run 03_estimation.R to estimate distance decay parameters\n")
