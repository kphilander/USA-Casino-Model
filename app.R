library(dplyr)
library(sf)
library(tigris)

#Import data
allzips <- readRDS("allzips.rds")

allzips$latitude <- round((allzips$latitude), digits = 3)
allzips$longitude <- round((allzips$longitude), digits = 3)

#Data for pop-up
cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    "Location Desirability Rank" = desirabilityrank,
    "Economic Size (Income x Adult Pop. in millions)" = zip_income,
    "Adults per Sq. Mile" = pop_density,
    "Adult Mean Income ($)" = income,
    Lat = latitude,
    Long = longitude
  )



