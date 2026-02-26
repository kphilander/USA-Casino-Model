#!/usr/bin/env Rscript
# diagnostic_convenience_gaming.R
# Audit casinodata.rds for convenience gaming venues (bars, restaurants, VLT parlors)
# that should not be treated as actual casinos in the gravity model.

library(dplyr)

casinodata <- readRDS("../casinodata.rds")

cat("=== CASINO DATABASE OVERVIEW ===\n")
cat("Total properties:", nrow(casinodata), "\n")
cat("Columns:", paste(colnames(casinodata), collapse=", "), "\n\n")

# State name -> abbreviation for readability
state_abb_map <- c(
  "Pennsylvania" = "PA", "Ohio" = "OH", "New York" = "NY",
  "New Jersey" = "NJ", "Delaware" = "DE", "Maryland" = "MD",
  "West Virginia" = "WV", "Kentucky" = "KY", "Indiana" = "IN",
  "Missouri" = "MO", "Iowa" = "IA", "Massachusetts" = "MA",
  "Connecticut" = "CT", "Washington" = "WA",
  "Vermont" = "VT", "New Hampshire" = "NH", "Rhode Island" = "RI",
  "Virginia" = "VA", "District of Columbia" = "DC",
  "Wisconsin" = "WI", "Illinois" = "IL", "Minnesota" = "MN",
  "South Dakota" = "SD", "Nebraska" = "NE", "Kansas" = "KS",
  "Arkansas" = "AR", "Tennessee" = "TN", "Oklahoma" = "OK",
  "Oregon" = "OR", "Idaho" = "ID", "California" = "CA",
  "Michigan" = "MI", "Nevada" = "NV", "Montana" = "MT",
  "Louisiana" = "LA", "Colorado" = "CO", "Arizona" = "AZ",
  "New Mexico" = "NM", "North Dakota" = "ND", "Wyoming" = "WY",
  "Mississippi" = "MS", "Alabama" = "AL", "Florida" = "FL",
  "Texas" = "TX", "North Carolina" = "NC", "South Carolina" = "SC",
  "Georgia" = "GA", "Maine" = "ME"
)

casinodata$state_abbr <- state_abb_map[casinodata$state]

cat("=== PROPERTIES PER STATE (ALL STATES IN DB) ===\n")
state_counts <- casinodata %>%
  group_by(state, state_abbr, tribal) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(state)
print(as.data.frame(state_counts), row.names = FALSE)

cat("\n=== SUMMARY BY STATE (totals) ===\n")
state_totals <- casinodata %>%
  group_by(state_abbr) %>%
  summarise(
    total = n(),
    commercial = sum(tribal == "Commercial", na.rm = TRUE),
    tribal = sum(tribal == "Tribal", na.rm = TRUE),
    other = sum(!tribal %in% c("Commercial", "Tribal"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total))
print(as.data.frame(state_totals), row.names = FALSE)

# Study region
study_states <- c("PA", "OH", "MD", "NY", "MA", "CT", "IN", "MO", "IA", "WA",
                  "NJ", "DE", "WV", "KY", "MI", "VT", "NH", "RI", "VA", "DC",
                  "WI", "IL", "MN", "SD", "NE", "KS", "AR", "TN", "OK",
                  "OR", "ID", "CA")

cat("\n=== STUDY REGION PROPERTIES ===\n")
study <- casinodata %>% filter(state_abbr %in% study_states)
cat("Total in study region:", nrow(study), "\n\n")

study_summary <- study %>%
  group_by(state_abbr) %>%
  summarise(
    total = n(),
    commercial = sum(tribal == "Commercial", na.rm = TRUE),
    tribal_count = sum(tribal == "Tribal", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total))
print(as.data.frame(study_summary), row.names = FALSE)

# HIGH RISK STATES: Print all property names for manual review
high_risk <- c("IL", "WV", "SD", "OR", "PA", "WA", "CA", "OK", "NE", "KY", "NH")

for (st in high_risk) {
  props <- casinodata %>%
    filter(state_abbr == st) %>%
    select(id, name, tribal, hotel) %>%
    arrange(name)
  if (nrow(props) > 0) {
    cat("\n=== ", st, " — ", nrow(props), " properties ===\n")
    print(as.data.frame(props), row.names = FALSE)
  }
}

# Look for suspicious name patterns across ALL study region properties
cat("\n=== SUSPICIOUS NAME PATTERNS (study region) ===\n")
suspicious_patterns <- c("bar ", "grill", "tavern", "pub ", "saloon",
                         "restaurant", "lounge", "cafe", "diner",
                         "truck stop", "gas ", "fuel", "convenience",
                         "liquor", "fraternal", "legion", "vfw",
                         "elks", "moose", "eagles", "veterans",
                         "bowling", "laundromat", "grocery", "store")

for (pat in suspicious_patterns) {
  matches <- study %>%
    filter(grepl(pat, name, ignore.case = TRUE)) %>%
    select(id, name, state_abbr, tribal)
  if (nrow(matches) > 0) {
    cat("\nPattern '", pat, "' — ", nrow(matches), " matches:\n")
    print(as.data.frame(matches), row.names = FALSE)
  }
}

cat("\n=== DONE ===\n")
