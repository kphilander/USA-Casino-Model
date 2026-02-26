#!/usr/bin/env Rscript
# diagnostic_remaining_states.R
# Audit casinodata.rds for convenience gaming in states OUTSIDE the study region.

library(dplyr)
casinodata <- readRDS("../casinodata.rds")

state_abb_map <- c(
  "Arizona"="AZ","Colorado"="CO","Florida"="FL","Louisiana"="LA",
  "Maine"="ME","Mississippi"="MS","Montana"="MT","Nevada"="NV",
  "New Mexico"="NM","North Carolina"="NC","North Dakota"="ND","Wyoming"="WY",
  # study region (for exclusion)
  "Pennsylvania"="PA","Ohio"="OH","New York"="NY","New Jersey"="NJ",
  "Delaware"="DE","Maryland"="MD","West Virginia"="WV","Kentucky"="KY",
  "Indiana"="IN","Missouri"="MO","Iowa"="IA","Massachusetts"="MA",
  "Connecticut"="CT","Washington"="WA","Vermont"="VT","New Hampshire"="NH",
  "Rhode Island"="RI","Virginia"="VA","District of Columbia"="DC",
  "Wisconsin"="WI","Illinois"="IL","Minnesota"="MN","South Dakota"="SD",
  "Nebraska"="NE","Kansas"="KS","Arkansas"="AR","Tennessee"="TN",
  "Oklahoma"="OK","Oregon"="OR","Idaho"="ID","California"="CA","Michigan"="MI"
)
casinodata$state_abbr <- state_abb_map[casinodata$state]

study_states <- c("PA","OH","MD","NY","MA","CT","IN","MO","IA","WA",
                  "NJ","DE","WV","KY","MI","VT","NH","RI","VA","DC",
                  "WI","IL","MN","SD","NE","KS","AR","TN","OK","OR","ID","CA")

remaining <- casinodata %>% filter(!state_abbr %in% study_states & !is.na(state_abbr))

cat("=== REMAINING STATES (not in study region) ===\n")
cat("Total properties:", nrow(remaining), "\n\n")

remaining_summary <- remaining %>%
  group_by(state_abbr) %>%
  summarise(total=n(), commercial=sum(tribal=="Commercial",na.rm=T),
            tribal=sum(tribal=="Tribal",na.rm=T), .groups="drop") %>%
  arrange(desc(total))
print(as.data.frame(remaining_summary), row.names=FALSE)

# Print ALL properties for each remaining state
for (st in remaining_summary$state_abbr) {
  props <- remaining %>%
    filter(state_abbr == st) %>%
    select(id, name, tribal, hotel, city) %>%
    arrange(name)
  cat("\n=== ", st, " — ", nrow(props), " properties ===\n")
  print(as.data.frame(props), row.names=FALSE)
}

cat("\n=== DONE ===\n")
