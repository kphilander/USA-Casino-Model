# Extract hotel classification for all 92 study properties
# Working directory: research/

# 1. Load casinodata
casinodata <- readRDS("../casinodata.rds")

# 2. Load all 9 state revenue CSVs
pa <- read.csv("data/pa_revenue_2022.csv", stringsAsFactors = FALSE)
oh <- read.csv("data/oh_revenue_2022.csv", stringsAsFactors = FALSE)
md <- read.csv("data/md_revenue_2022.csv", stringsAsFactors = FALSE)
ny <- read.csv("data/ny_revenue_2022.csv", stringsAsFactors = FALSE)
ma <- read.csv("data/ma_revenue_2022.csv", stringsAsFactors = FALSE)
ct <- read.csv("data/ct_revenue_2022.csv", stringsAsFactors = FALSE)
in_rev <- read.csv("data/in_revenue_2022.csv", stringsAsFactors = FALSE)
mo <- read.csv("data/mo_revenue_2022.csv", stringsAsFactors = FALSE)
ia <- read.csv("data/ia_revenue_2022.csv", stringsAsFactors = FALSE)

# 3. Combine — use only shared columns to avoid rbind issues
shared_cols <- c("casino_id", "name", "city", "state")
revenue_data <- rbind(
  pa[, shared_cols], oh[, shared_cols], md[, shared_cols],
  ny[, shared_cols], ma[, shared_cols], ct[, shared_cols],
  in_rev[, shared_cols], mo[, shared_cols], ia[, shared_cols]
)

# Drop any NA casino_id rows
revenue_data <- revenue_data[!is.na(revenue_data$casino_id), ]

cat("Total properties in combined revenue data:", nrow(revenue_data), "\n\n")

# 4. Build lookup from casinodata: id -> hotel field
hotel_lookup <- data.frame(
  casino_id = casinodata$id,
  hotel_in_db = as.character(casinodata$hotel),
  stringsAsFactors = FALSE
)

# Merge
merged <- merge(revenue_data, hotel_lookup, by = "casino_id", all.x = TRUE)

# 5. Handle Caesars Southern Indiana (id=9999, not in casinodata.rds)
merged$hotel_in_db[merged$casino_id == 9999] <- "Yes"

# 6. Compute has_hotel (0/1) — same logic as run_analysis.R line 164
merged$has_hotel <- ifelse(merged$hotel_in_db == "Yes", 1, 0)

# Sort by state then name
merged <- merged[order(merged$state, merged$name), ]

# Print full table
cat(sprintf("%-8s  %-50s  %-20s  %-5s  %-12s  %s\n",
            "ID", "Name", "City", "State", "hotel_in_db", "has_hotel"))
cat(paste(rep("-", 110), collapse = ""), "\n")

for (i in seq_len(nrow(merged))) {
  cat(sprintf("%-8s  %-50s  %-20s  %-5s  %-12s  %d\n",
              merged$casino_id[i],
              merged$name[i],
              merged$city[i],
              merged$state[i],
              merged$hotel_in_db[i],
              merged$has_hotel[i]))
}

cat("\n--- Summary ---\n")
cat("Total properties:", nrow(merged), "\n")
cat("has_hotel = 1:", sum(merged$has_hotel == 1), "\n")
cat("has_hotel = 0:", sum(merged$has_hotel == 0), "\n")
cat("hotel_in_db NA (unmatched):", sum(is.na(merged$hotel_in_db)), "\n")
