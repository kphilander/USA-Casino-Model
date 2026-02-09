# =============================================================================
# Distance Decay Estimation: Data Preparation
# =============================================================================
# This script prepares data for estimating casino demand distance decay functions
# using Pennsylvania and Ohio revenue data with ZIP-level demographics.
#
# Author: Kahlil Philander
# Date: February 2026
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(sf)

# =============================================================================
# 1. LOAD EXISTING DATA
# =============================================================================

# Load existing geo-demand model data
allzips <- readRDS("../allzips.rds")
casinodata <- readRDS("../casinodata.rds")

cat("Loaded", nrow(allzips), "ZIP codes\n")
cat("Loaded", nrow(casinodata), "casino locations\n")

# =============================================================================
# 2. DEFINE STUDY REGION
# =============================================================================

# Target states for analysis
primary_states <- c("PA", "OH")  # States where we observe revenue
border_states <- c("NY", "NJ", "DE", "MD", "WV", "KY", "IN", "MI")
all_states <- c(primary_states, border_states)

# Filter ZIP codes to study region
zips_study <- allzips %>%
  filter(state.x %in% all_states) %>%
  select(
    zipcode,
    state = state.x,
    city = city.x,
    latitude,
    longitude,
    adultpop,
    income,
    pop_density
  )

cat("\nStudy region ZIP codes:", nrow(zips_study), "\n")
cat("By state:\n")
print(table(zips_study$state))

# =============================================================================
# 3. PREPARE CASINO LOCATIONS
# =============================================================================

# State name to abbreviation mapping
state_abbrev <- c(
  "Pennsylvania" = "PA", "Ohio" = "OH", "New York" = "NY",
  "New Jersey" = "NJ", "Delaware" = "DE", "Maryland" = "MD",
  "West Virginia" = "WV", "Kentucky" = "KY", "Indiana" = "IN",
  "Michigan" = "MI"
)

# Filter casinos to study region
casinos_study <- casinodata %>%
  filter(state %in% names(state_abbrev)) %>%
  mutate(state_abbrev = state_abbrev[state]) %>%
  select(
    casino_id = id,
    name,
    city,
    state_full = state,
    state = state_abbrev,
    latitude = geocodehere_lat,
    longitude = geocodehere_lon,
    tribal,
    hotel
  )

cat("\nStudy region casinos:", nrow(casinos_study), "\n")
cat("By state:\n")
print(table(casinos_study$state))

# Identify PA and OH casinos (where we observe revenue)
casinos_observed <- casinos_study %>%
  filter(state %in% primary_states)

cat("\nCasinos with observed revenue:", nrow(casinos_observed), "\n")

# =============================================================================
# 4. REVENUE DATA TEMPLATE
# =============================================================================
# This section creates a template for the revenue data that needs to be collected
# from PA Gaming Control Board and Ohio Casino Control Commission

# PA Casino Revenue Template (2022)
pa_revenue_template <- casinos_study %>%
  filter(state == "PA") %>%
  select(casino_id, name, city) %>%
  mutate(
    slots_revenue_2022 = NA_real_,
    table_revenue_2022 = NA_real_,
    total_revenue_2022 = NA_real_,
    num_slots = NA_integer_,
    num_tables = NA_integer_,
    source = "PGCB"
  )

cat("\n=== PA REVENUE DATA TEMPLATE ===\n")
cat("Fill in from: https://gamingcontrolboard.pa.gov/news-and-transparency/revenue\n\n")
print(pa_revenue_template %>% select(name, city, slots_revenue_2022, table_revenue_2022, num_slots, num_tables))

# OH Casino/Racino Revenue Template (2022)
oh_revenue_template <- casinos_study %>%
  filter(state == "OH") %>%
  select(casino_id, name, city) %>%
  mutate(
    slots_revenue_2022 = NA_real_,
    table_revenue_2022 = NA_real_,
    total_revenue_2022 = NA_real_,
    num_slots = NA_integer_,
    num_tables = NA_integer_,
    facility_type = ifelse(grepl("Hollywood Gaming|Belterra|MGM|Miami Valley|Scioto|Thistledown", name),
                           "racino", "casino"),
    source = ifelse(facility_type == "casino", "OCCC", "OH Lottery")
  )

cat("\n=== OH REVENUE DATA TEMPLATE ===\n")
cat("Casinos from: https://casinocontrol.ohio.gov/\n")
cat("Racinos from: Ohio Lottery Commission\n\n")
print(oh_revenue_template %>% select(name, city, facility_type, slots_revenue_2022, table_revenue_2022, num_slots, num_tables))

# =============================================================================
# 5. SAVE TEMPLATES FOR DATA COLLECTION
# =============================================================================

# Save templates as CSV for manual data entry
write_csv(pa_revenue_template, "data/pa_revenue_template.csv")
write_csv(oh_revenue_template, "data/oh_revenue_template.csv")

cat("\n=== SAVED TEMPLATES ===\n")
cat("Templates saved to research/data/ directory\n")
cat("1. pa_revenue_template.csv - Fill with PGCB 2022 data\n")
cat("2. oh_revenue_template.csv - Fill with OCCC/Lottery 2022 data\n")

# =============================================================================
# 6. BORDER STATE CASINO DATA
# =============================================================================
# For competing casinos, we need locations but not revenue
# We assume standard capacity for distance decay calculation

casinos_border <- casinos_study %>%
  filter(state %in% border_states) %>%
  mutate(
    # Assume average gaming positions for border state casinos
    # This can be refined with actual data if available
    num_slots_estimated = case_when(
      state == "NJ" ~ 3000,  # Atlantic City casinos are large
      state == "MI" & grepl("Detroit", city) ~ 2500,  # Detroit casinos
      tribal == "Tribal" ~ 1500,  # Tribal casinos tend to be medium
      TRUE ~ 1000  # Default for smaller properties
    ),
    num_tables_estimated = case_when(
      state == "NJ" ~ 150,
      state == "MI" & grepl("Detroit", city) ~ 100,
      tribal == "Tribal" ~ 50,
      TRUE ~ 30
    )
  )

cat("\n=== BORDER STATE CASINOS ===\n")
cat("Using estimated gaming positions for competition modeling\n")
print(table(casinos_border$state))

# =============================================================================
# 7. SAVE PREPARED DATA
# =============================================================================

# Create data directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")

# Save prepared datasets
saveRDS(zips_study, "data/zips_study.rds")
saveRDS(casinos_study, "data/casinos_study.rds")
saveRDS(casinos_observed, "data/casinos_observed.rds")
saveRDS(casinos_border, "data/casinos_border.rds")

cat("\n=== DATA PREPARATION COMPLETE ===\n")
cat("Saved to research/data/:\n")
cat("  - zips_study.rds (", nrow(zips_study), " ZIPs)\n")
cat("  - casinos_study.rds (", nrow(casinos_study), " casinos)\n")
cat("  - casinos_observed.rds (", nrow(casinos_observed), " PA/OH properties)\n")
cat("  - casinos_border.rds (", nrow(casinos_border), " competing casinos)\n")

# =============================================================================
# 8. DATA COLLECTION CHECKLIST
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("DATA COLLECTION CHECKLIST\n")
cat("=" %>% rep(70) %>% paste(collapse=""), "\n")
cat("\n")
cat("[ ] 1. PA 2022 Revenue Data (PGCB)\n")
cat("    - Download from: gamingcontrolboard.pa.gov/news-and-transparency/revenue\n")
cat("    - Need: Monthly or annual slot revenue by property\n")
cat("    - Need: Monthly or annual table revenue by property\n")
cat("    - Need: Number of slot machines by property\n")
cat("    - Need: Number of table games by property\n")
cat("\n")
cat("[ ] 2. OH 2022 Casino Revenue Data (OCCC)\n")
cat("    - Download from: casinocontrol.ohio.gov\n")
cat("    - Properties: JACK Cleveland, Hollywood Columbus,\n")
cat("                  Hollywood Toledo, Hard Rock Cincinnati\n")
cat("    - Need: Annual slot and table revenue\n")
cat("    - Need: Gaming positions\n")
cat("\n")
cat("[ ] 3. OH 2022 Racino Revenue Data (OH Lottery)\n")
cat("    - Source: Ohio Lottery Commission\n")
cat("    - Properties: 8 racinos with VLTs\n")
cat("    - Need: Annual VLT revenue\n")
cat("    - Need: Number of VLTs\n")
cat("\n")
cat("[ ] 4. Fill in templates and save as:\n")
cat("    - data/pa_revenue_2022.csv\n")
cat("    - data/oh_revenue_2022.csv\n")
cat("\n")
cat("Once data is collected, run 02_distance_calculations.R\n")
