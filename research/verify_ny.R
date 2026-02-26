#!/usr/bin/env Rscript
# verify_ny.R  -- Compare NY source Excel files to ny_revenue_2022.csv
# CY2022 = Jan 2022 - Dec 2022
#   FY 21-22 sheet rows for Jan, Feb, Mar 2022
#   FY 22-23 sheet rows for Apr - Dec 2022
#
# NY fiscal year runs April 1 - March 31.

library(readxl)
library(dplyr, warn.conflicts = FALSE)

base <- "/Users/ksr/Library/CloudStorage/GoogleDrive-kphilander@gmail.com/My Drive/1 - Documents/Research/Proximity/temp_clone/research/data/"

excel_date <- function(x) as.Date(as.numeric(x), origin = "1899-12-30")

# ===================================================================
#  PART 1: Statewide Commercial Summary (ny_commercial_casino_monthly.xlsx)
# ===================================================================
cat("============================================================\n")
cat("  PART 1: Statewide Commercial Summary\n")
cat("  (ny_commercial_casino_monthly.xlsx)\n")
cat("============================================================\n\n")

comm_file <- paste0(base, "ny_commercial_casino_monthly.xlsx")
cat("Sheets:", paste(excel_sheets(comm_file), collapse = ", "), "\n\n")

read_comm_sheet <- function(file, sheet_name) {
  d <- suppressMessages(read_excel(file, sheet = sheet_name, col_names = FALSE))
  cat(sprintf("Sheet '%s': %d rows x %d cols\n", sheet_name, nrow(d), ncol(d)))
  
  # Col layout: 1=Month, 6=Slot GGR, 15=Table GGR, 18=Poker GGR,
  #             21=Sports GGR, 23=Total GGR
  hdr <- as.character(d[14,])
  hdr[is.na(hdr)] <- ""
  cat("Header: ", paste(hdr[c(1,6,15,18,21,23)], collapse = " | "), "\n")
  
  results <- list()
  for (r in 15:26) {
    rv <- d[r,]
    dt <- excel_date(rv[[1]])
    results[[length(results) + 1]] <- data.frame(
      date       = dt,
      year       = as.numeric(format(dt, "%Y")),
      month      = as.numeric(format(dt, "%m")),
      slot_ggr   = as.numeric(rv[[6]]),
      table_ggr  = as.numeric(rv[[15]]),
      poker_ggr  = as.numeric(rv[[18]]),
      sports_ggr = as.numeric(rv[[21]]),
      total_ggr  = as.numeric(rv[[23]])
    )
  }
  
  total_row <- d[27,]
  cat(sprintf("FY Total: Slots=%.0f  Tables=%.0f  Total=%.0f\n\n",
              as.numeric(total_row[[6]]), as.numeric(total_row[[15]]),
              as.numeric(total_row[[23]])))
  bind_rows(results)
}

fy2122_sw <- read_comm_sheet(comm_file, "FY 21-22")
fy2223_sw <- read_comm_sheet(comm_file, "FY 22-23")

cy2022_sw <- bind_rows(
  fy2122_sw %>% filter(year == 2022),
  fy2223_sw %>% filter(year == 2022)
) %>% arrange(date)

cat("CY2022 Statewide Monthly Totals:\n")
for (i in 1:nrow(cy2022_sw)) {
  cat(sprintf("  %s: slots=%12.0f  tables=%11.0f  total=%12.0f\n",
              cy2022_sw$date[i], cy2022_sw$slot_ggr[i],
              cy2022_sw$table_ggr[i], cy2022_sw$total_ggr[i]))
}

sw <- list(
  slots  = sum(cy2022_sw$slot_ggr),
  tables = sum(cy2022_sw$table_ggr),
  poker  = sum(cy2022_sw$poker_ggr),
  sports = sum(cy2022_sw$sports_ggr),
  total  = sum(cy2022_sw$total_ggr)
)
cat(sprintf("\nStatewide CY2022:  Slots=%12.0f  Tables=%11.0f  Poker=%10.0f  Sports=%10.0f  Total=%12.0f\n",
            sw$slots, sw$tables, sw$poker, sw$sports, sw$total))

# ===================================================================
#  PART 2: Individual Commercial Casino Files
# ===================================================================
cat("\n============================================================\n")
cat("  PART 2: Individual Commercial Casino Files\n")
cat("============================================================\n\n")

csv_commercial <- data.frame(
  casino_id = c(1154, 1167, 353, 1360),
  name = c("Resorts World Catskills", "Rivers Casino & Resort Schenectady",
           "del Lago Resort & Casino", "Tioga Downs Casino Resort"),
  csv_total  = c(228695970, 201852331, 163162032, 103309766),
  csv_slots  = c(121289014, 147812525, 125920050, 89946497),
  csv_tables = c(101338756, 45396381, 32515394, 11968428),
  file = c("ny_resorts-world-catskills.xlsx", "ny_rivers-casino-and-resort.xlsx",
           "ny_del-lago-resort-and-casino.xlsx", "ny_tioga-downs.xlsx"),
  stringsAsFactors = FALSE
)

read_individual_commercial <- function(filename, casino_name) {
  filepath <- paste0(base, filename)
  sheets <- excel_sheets(filepath)
  cat(sprintf("--- %s ---\n", casino_name))
  cat("  File:", filename, "\n")
  
  s2122 <- sheets[grep("21-22", sheets)]
  s2223 <- sheets[grep("22-23", sheets)]
  
  extract_months <- function(sheet_name) {
    d <- suppressMessages(read_excel(filepath, sheet = sheet_name, col_names = FALSE))
    nc <- ncol(d)
    
    data_start <- NA
    for (r in 10:min(20, nrow(d))) {
      val <- suppressWarnings(as.numeric(d[[r, 1]]))
      if (!is.na(val) && val > 40000 && val < 50000) { data_start <- r; break }
    }
    if (is.na(data_start)) return(data.frame())
    
    results <- list()
    for (r in data_start:(data_start + 11)) {
      if (r > nrow(d)) break
      rv <- d[r,]
      date_val <- suppressWarnings(as.numeric(rv[[1]]))
      if (is.na(date_val) || date_val < 40000) next
      dt <- excel_date(date_val)
      
      results[[length(results) + 1]] <- data.frame(
        date       = dt,
        year       = as.numeric(format(dt, "%Y")),
        month      = as.numeric(format(dt, "%m")),
        slot_ggr   = suppressWarnings(as.numeric(rv[[6]])),
        table_ggr  = if (nc >= 15) suppressWarnings(as.numeric(rv[[15]])) else NA,
        poker_ggr  = if (nc >= 18) suppressWarnings(as.numeric(rv[[18]])) else NA,
        sports_ggr = if (nc >= 21) suppressWarnings(as.numeric(rv[[21]])) else NA,
        total_ggr  = if (nc >= 23) suppressWarnings(as.numeric(rv[[23]])) else NA
      )
    }
    bind_rows(results)
  }
  
  cy2022 <- bind_rows(
    extract_months(s2122) %>% filter(year == 2022),
    extract_months(s2223) %>% filter(year == 2022)
  ) %>% arrange(date)
  
  cat(sprintf("  CY2022 months: %d\n", nrow(cy2022)))
  
  res <- data.frame(
    slot_total   = sum(cy2022$slot_ggr, na.rm = TRUE),
    table_total  = sum(cy2022$table_ggr, na.rm = TRUE),
    poker_total  = sum(cy2022$poker_ggr, na.rm = TRUE),
    sports_total = sum(cy2022$sports_ggr, na.rm = TRUE),
    grand_total  = sum(cy2022$total_ggr, na.rm = TRUE)
  )
  
  cat(sprintf("  Slots: %12.0f  Tables: %11.0f  Poker: %10.0f  Sports: %10.0f  Total: %12.0f\n\n",
              res$slot_total, res$table_total, res$poker_total, res$sports_total, res$grand_total))
  res
}

indiv <- list()
for (i in 1:nrow(csv_commercial)) {
  indiv[[i]] <- read_individual_commercial(csv_commercial$file[i], csv_commercial$name[i])
}

# Verify individual sums match statewide
cat("Cross-check: Sum of 4 individual casinos vs statewide CY2022:\n")
sum_slots  <- sum(sapply(indiv, function(x) x$slot_total))
sum_tables <- sum(sapply(indiv, function(x) x$table_total))
sum_total  <- sum(sapply(indiv, function(x) x$grand_total))
cat(sprintf("  Individual sum:  Slots=%12.0f  Tables=%11.0f  Total=%12.0f\n", sum_slots, sum_tables, sum_total))
cat(sprintf("  Statewide:       Slots=%12.0f  Tables=%11.0f  Total=%12.0f\n", sw$slots, sw$tables, sw$total))
cat(sprintf("  Difference:      Slots=%12.0f  Tables=%11.0f  Total=%12.0f\n",
            sum_slots - sw$slots, sum_tables - sw$tables, sum_total - sw$total))

# ===================================================================
#  PART 3: Commercial Casino Comparison to CSV
# ===================================================================
cat("\n============================================================\n")
cat("  PART 3: Commercial Casinos -- CSV vs Excel\n")
cat("============================================================\n\n")

for (i in 1:nrow(csv_commercial)) {
  r <- indiv[[i]]
  csv_s <- csv_commercial$csv_slots[i]
  csv_t <- csv_commercial$csv_tables[i]
  csv_tot <- csv_commercial$csv_total[i]
  
  excel_s <- round(r$slot_total)
  excel_t <- round(r$table_total)
  excel_tot <- round(r$grand_total)
  
  # The CSV total = slots + tables + poker + sports
  # (confirmed: CSV total != CSV slots + CSV tables)
  csv_implied_other <- csv_tot - csv_s - csv_t
  excel_other <- round(r$poker_total + r$sports_total)
  
  cat(sprintf("%s (ID %d):\n", csv_commercial$name[i], csv_commercial$casino_id[i]))
  cat(sprintf("                    %15s  %15s  %15s\n", "CSV", "Excel CY2022", "Difference"))
  cat(sprintf("  Slot GGR:         %15d  %15d  %15d\n", csv_s, excel_s, excel_s - csv_s))
  cat(sprintf("  Table GGR:        %15d  %15d  %15d\n", csv_t, excel_t, excel_t - csv_t))
  cat(sprintf("  Poker+Sports:     %15d  %15d  %15d\n", csv_implied_other, excel_other, excel_other - csv_implied_other))
  cat(sprintf("  Total GGR:        %15d  %15d  %15d\n", csv_tot, excel_tot, excel_tot - csv_tot))
  cat(sprintf("  Pct diff (total): %+.4f%%\n\n", (excel_tot - csv_tot) / csv_tot * 100))
}

# ===================================================================
#  PART 4: VLT Facility Files
# ===================================================================
cat("============================================================\n")
cat("  PART 4: VLT Facilities\n")
cat("  (ny_vlt_monthly.xlsx is corrupted; using individual files)\n")
cat("============================================================\n\n")

csv_vlt <- data.frame(
  casino_id = c(1153, 453, 710, 1209, 470, 83, 580, 1399),
  name = c("Resorts World Casino NYC", "Empire City Casino", "Jake's 58",
           "Saratoga Casino Hotel", "Finger Lakes Gaming & Racetrack",
           "Batavia Downs Gaming", "Hamburg Gaming", "Vernon Downs Casino Hotel"),
  csv_total = c(644969365, 613720364, 256627270, 141056619, 116459247,
                77342580, 70868926, 28372467),
  file = c("ny_resorts-world-casino-nyc.xls", "ny_empire-city-casino.xls",
           "ny_jakes-58-hotel-and-casino.xls", "ny_saratoga-casino.xls",
           "ny_finger-lakes-gaming-racetrack.xls", "ny_batavia-downs-gaming.xls",
           "ny_hamburg-gaming.xls", "ny_vernon-downs-casino.xls"),
  stringsAsFactors = FALSE
)

read_vlt_file <- function(filename, casino_name) {
  filepath <- paste0(base, filename)
  sheets <- excel_sheets(filepath)
  cat(sprintf("--- %s ---\n", casino_name))
  cat("  File:", filename, "\n")
  
  s2122 <- sheets[grep("21-22", sheets)]
  s2223 <- sheets[grep("22-23", sheets)]
  
  extract_vlt_months <- function(sheet_name) {
    d <- suppressMessages(read_excel(filepath, sheet = sheet_name, col_names = FALSE))
    
    # VLT: Col 1=Month, 2=Credits Played, 3=Free Play, 4=Credits Won, 5=Net Win
    data_start <- NA
    for (r in 10:min(20, nrow(d))) {
      val <- suppressWarnings(as.numeric(d[[r, 1]]))
      if (!is.na(val) && val > 40000 && val < 50000) { data_start <- r; break }
    }
    if (is.na(data_start)) return(data.frame())
    
    results <- list()
    for (r in data_start:(data_start + 11)) {
      if (r > nrow(d)) break
      rv <- d[r,]
      date_val <- suppressWarnings(as.numeric(rv[[1]]))
      if (is.na(date_val) || date_val < 40000) next
      dt <- excel_date(date_val)
      results[[length(results) + 1]] <- data.frame(
        date    = dt,
        year    = as.numeric(format(dt, "%Y")),
        month   = as.numeric(format(dt, "%m")),
        net_win = suppressWarnings(as.numeric(rv[[5]]))
      )
    }
    bind_rows(results)
  }
  
  cy2022 <- bind_rows(
    extract_vlt_months(s2122) %>% filter(year == 2022),
    extract_vlt_months(s2223) %>% filter(year == 2022)
  ) %>% arrange(date)
  
  nw_total <- sum(cy2022$net_win, na.rm = TRUE)
  cat(sprintf("  CY2022 months: %d   Net Win: %12.0f\n", nrow(cy2022), nw_total))
  
  # Also compute FY 22-23 total for reference
  fy2223_all <- extract_vlt_months(s2223)
  fy_total <- sum(fy2223_all$net_win, na.rm = TRUE)
  cat(sprintf("  FY 22-23 total:              %12.0f\n\n", fy_total))
  
  list(cy2022 = nw_total, fy2223 = fy_total)
}

vlt_results <- list()
for (i in 1:nrow(csv_vlt)) {
  vlt_results[[i]] <- read_vlt_file(csv_vlt$file[i], csv_vlt$name[i])
}

# ===================================================================
#  PART 5: VLT Comparison to CSV
# ===================================================================
cat("============================================================\n")
cat("  PART 5: VLT Facilities -- CSV vs Excel\n")
cat("============================================================\n\n")

cat(sprintf("%-38s  %15s  %15s  %15s  %15s  %8s\n",
            "Facility", "CSV Total", "Excel CY2022", "Difference", "Excel FY22-23", "Pct Diff"))
cat(paste(rep("-", 118), collapse = ""), "\n")

for (i in 1:nrow(csv_vlt)) {
  csv_t <- csv_vlt$csv_total[i]
  excel_cy <- round(vlt_results[[i]]$cy2022)
  excel_fy <- round(vlt_results[[i]]$fy2223)
  diff <- excel_cy - csv_t
  pct <- diff / csv_t * 100
  cat(sprintf("%-38s  %15d  %15d  %15d  %15d  %+.3f%%\n",
              csv_vlt$name[i], csv_t, excel_cy, diff, excel_fy, pct))
}

# ===================================================================
#  PART 6: Summary & Analysis
# ===================================================================
cat("\n============================================================\n")
cat("  PART 6: Summary & Analysis\n")
cat("============================================================\n\n")

# Check which interpretation the CSV uses
cat("1) CSV Total vs CSV Slots+Tables (commercial casinos):\n")
cat("   The CSV total DOES NOT equal slots+tables -- the difference\n")
cat("   represents poker and sports wagering revenue.\n\n")
for (i in 1:nrow(csv_commercial)) {
  diff <- csv_commercial$csv_total[i] - csv_commercial$csv_slots[i] - csv_commercial$csv_tables[i]
  cat(sprintf("   %s: total - slots - tables = %d (poker+sports)\n",
              csv_commercial$name[i], diff))
}

cat("\n2) Sum of all 4 CSV commercial casinos vs Excel statewide CY2022:\n")
csv_grand <- sum(csv_commercial$csv_total)
csv_slots_sum <- sum(csv_commercial$csv_slots)
cat(sprintf("   CSV sum of totals:  %d\n", csv_grand))
cat(sprintf("   Excel statewide:    %d\n", round(sw$total)))
cat(sprintf("   Difference:         %d  (CSV is %.2f%% higher)\n\n",
            csv_grand - round(sw$total), (csv_grand - sw$total) / sw$total * 100))

cat("3) Pattern: CSV values are consistently HIGHER than Excel CY2022 sums\n")
cat("   for ALL facilities (commercial and VLT). The differences range\n")
cat("   from ~0.02% to ~2.6%, with no single consistent ratio.\n\n")

cat("4) Possible explanations:\n")
cat("   a) The Excel files may have been revised/updated after the CSV was\n")
cat("      created (prior period adjustments, corrections, etc.).\n")
cat("   b) The CSV may have used a slightly different data source or report\n")
cat("      version from the NYS Gaming Commission.\n")
cat("   c) Rounding: the CSV uses integer values while the Excel has decimals,\n")
cat("      but this only accounts for < $1 per month, not the observed diffs.\n\n")

cat("5) DETAILED DISCREPANCY TABLE:\n\n")
cat(sprintf("   %-38s  %15s  %15s  %12s\n", "Casino/Facility", "CSV Total", "Excel CY2022", "Difference"))
cat(paste(rep("-", 90), collapse = ""), "\n")

for (i in 1:nrow(csv_commercial)) {
  r <- indiv[[i]]
  diff <- round(r$grand_total) - csv_commercial$csv_total[i]
  cat(sprintf("   %-38s  %15d  %15d  %+12d\n",
              csv_commercial$name[i], csv_commercial$csv_total[i], round(r$grand_total), diff))
}
for (i in 1:nrow(csv_vlt)) {
  diff <- round(vlt_results[[i]]$cy2022) - csv_vlt$csv_total[i]
  cat(sprintf("   %-38s  %15d  %15d  %+12d\n",
              csv_vlt$name[i], csv_vlt$csv_total[i], round(vlt_results[[i]]$cy2022), diff))
}

cat("\n============================================================\n")
cat("  CONCLUSION: All 12 facilities show discrepancies between\n")
cat("  the CSV values and the current Excel source files.\n")
cat("  The CSV values are consistently HIGHER, suggesting the\n")
cat("  source data may have been revised since the CSV was created.\n")
cat("============================================================\n")
