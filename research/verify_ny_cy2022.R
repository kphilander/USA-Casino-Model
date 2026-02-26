library(readxl)

data_dir <- "data"

# NY uses fiscal years Apr-Mar. For CY2022:
#   Jan-Mar 2022 → FY 2021/2022 sheet (last 3 months)
#   Apr-Dec 2022 → FY 2022/2023 sheet (first 9 months)

# Excel date serial to date helper
excel_to_date <- function(x) as.Date(x, origin = "1899-12-30")

# Function to extract monthly data from a sheet
extract_monthly <- function(fpath, sheet_name, col_indices) {
  df <- read_excel(fpath, sheet = sheet_name, col_names = FALSE)

  # Find the data rows (rows 14-25 typically)
  # Row 13 is the header with "Month"
  results <- list()
  for (i in 14:min(25, nrow(df))) {
    month_serial <- as.numeric(df[[1]][i])
    if (is.na(month_serial) || month_serial == 0) next
    month_date <- excel_to_date(month_serial)

    slot_ggr <- as.numeric(df[[col_indices["slot_ggr"]]][i])
    table_ggr <- as.numeric(df[[col_indices["table_ggr"]]][i])
    poker_ggr <- as.numeric(df[[col_indices["poker_ggr"]]][i])
    sports_ggr <- as.numeric(df[[col_indices["sports_ggr"]]][i])
    total_ggr <- as.numeric(df[[col_indices["total_ggr"]]][i])

    if (is.na(slot_ggr)) slot_ggr <- 0
    if (is.na(table_ggr)) table_ggr <- 0
    if (is.na(poker_ggr)) poker_ggr <- 0
    if (is.na(sports_ggr)) sports_ggr <- 0
    if (is.na(total_ggr)) total_ggr <- 0

    results[[length(results) + 1]] <- data.frame(
      date = month_date,
      year = as.integer(format(month_date, "%Y")),
      month = as.integer(format(month_date, "%m")),
      slot_ggr = slot_ggr,
      table_ggr = table_ggr,
      poker_ggr = poker_ggr,
      sports_ggr = sports_ggr,
      total_ggr = total_ggr
    )
  }
  do.call(rbind, results)
}

# CSV data
ny_csv <- read.csv(file.path(data_dir, "ny_revenue_2022.csv"), stringsAsFactors = FALSE)

cat("=" , rep("=", 79), "\n", sep="")
cat("NEW YORK CY2022 VERIFICATION FROM INDIVIDUAL EXCEL FILES\n")
cat("=" , rep("=", 79), "\n\n")

# =========================================
# COMMERCIAL CASINOS (4) — .xlsx files
# =========================================
commercial_casinos <- list(
  list(name = "Resorts World Catskills",
       file = "ny_resorts-world-catskills.xlsx",
       fy2122_sheet = "21-22 RW Catskills Monthly",
       fy2223_sheet = "22-23 RW Catskills Monthly"),
  list(name = "Rivers Casino & Resort Schenectady",
       file = "ny_rivers-casino-and-resort.xlsx",
       fy2122_sheet = NULL,  # will find dynamically
       fy2223_sheet = NULL),
  list(name = "del Lago Resort & Casino",
       file = "ny_del-lago-resort-and-casino.xlsx",
       fy2122_sheet = NULL,
       fy2223_sheet = NULL),
  list(name = "Tioga Downs Casino Resort",
       file = "ny_tioga-downs.xlsx",
       fy2122_sheet = NULL,
       fy2223_sheet = NULL)
)

for (casino in commercial_casinos) {
  fpath <- file.path(data_dir, casino$file)
  cat("=== ", casino$name, " ===\n", sep="")

  sheets <- excel_sheets(fpath)
  cat("Available sheets: ", paste(sheets, collapse=", "), "\n")

  # Find FY 21-22 and 22-23 sheets
  fy2122 <- sheets[grepl("21-22|2021.2022", sheets, ignore.case = TRUE)]
  fy2223 <- sheets[grepl("22-23|2022.2023", sheets, ignore.case = TRUE)]

  if (length(fy2122) == 0 || length(fy2223) == 0) {
    cat("  WARNING: Could not find both required FY sheets\n")
    cat("  FY21-22:", fy2122, "\n")
    cat("  FY22-23:", fy2223, "\n\n")
    next
  }

  cat("  Using FY21-22 sheet: '", fy2122[1], "'\n", sep="")
  cat("  Using FY22-23 sheet: '", fy2223[1], "'\n", sep="")

  # Detect column structure by reading headers
  # For commercial casinos, structure is:
  # Col 1: Month, 2: NA, 3: Credits Played, 4: Promo Credits, 5: Credits Won,
  # 6: Slot GGR, 7: NA, 8: Avg Daily ETGs, 9: Win/Unit, 10: (blank)
  # 11: NA, 12: Avg Daily Tables, 13: Table Drop, 14: Promo Credits,
  # 15: Table GGR, 16: NA, 17: Avg Daily Poker, 18: Poker GGR
  # 19: NA, 20: Sports Handle, 21: Sports GGR, 22: NA, 23: Total GGR

  # But column count may vary. Let's detect:
  for (sh in c(fy2122[1], fy2223[1])) {
    df_test <- read_excel(fpath, sheet = sh, col_names = FALSE)
    ncols <- ncol(df_test)
    cat("  Sheet '", sh, "': ", ncols, " cols\n", sep="")

    # Print header row(s) to identify columns
    if (nrow(df_test) >= 13) {
      row13 <- as.character(df_test[13, ])
      row13[is.na(row13)] <- "NA"
      cat("  Header row 13: ", paste(row13, collapse=" | "), "\n", sep="")
    }
  }

  # For all commercial casino files, the standard layout has Total GGR as the last column
  # and the relevant columns are:
  # Slot GGR = col 6, Table GGR = col 15, Poker GGR = col 18, Total GGR = last col

  tryCatch({
    # Read both FY sheets
    all_months <- list()

    for (sh in c(fy2122[1], fy2223[1])) {
      df <- read_excel(fpath, sheet = sh, col_names = FALSE)
      ncols <- ncol(df)

      # Total GGR is always last column
      total_col <- ncols

      # Identify slot GGR (col 6), table GGR, poker GGR, sports GGR
      # Standard: slot=6, table=15, poker=18, sports_handle=20, sports_ggr=21, total=23
      # But if ncols=22 (no separate sports cols), adjust
      # Let's use the header row to identify

      for (i in 14:min(25, nrow(df))) {
        month_serial <- suppressWarnings(as.numeric(df[[1]][i]))
        if (is.na(month_serial) || month_serial == 0) next

        month_date <- excel_to_date(month_serial)
        yr <- as.integer(format(month_date, "%Y"))
        mn <- as.integer(format(month_date, "%m"))

        slot_ggr <- suppressWarnings(as.numeric(df[[6]][i]))
        total_ggr <- suppressWarnings(as.numeric(df[[total_col]][i]))

        # Table GGR: for 23-col layout it's col 15; for 22-col it might be col 14
        if (ncols >= 23) {
          table_ggr <- suppressWarnings(as.numeric(df[[15]][i]))
          poker_ggr <- suppressWarnings(as.numeric(df[[18]][i]))
          sports_ggr <- suppressWarnings(as.numeric(df[[21]][i]))
        } else if (ncols == 22) {
          table_ggr <- suppressWarnings(as.numeric(df[[15]][i]))
          poker_ggr <- suppressWarnings(as.numeric(df[[18]][i]))
          sports_ggr <- suppressWarnings(as.numeric(df[[20]][i]))
        } else {
          table_ggr <- NA
          poker_ggr <- NA
          sports_ggr <- NA
        }

        if (is.na(slot_ggr)) slot_ggr <- 0
        if (is.na(table_ggr)) table_ggr <- 0
        if (is.na(poker_ggr)) poker_ggr <- 0
        if (is.na(sports_ggr)) sports_ggr <- 0
        if (is.na(total_ggr)) total_ggr <- 0

        all_months[[length(all_months) + 1]] <- data.frame(
          date = month_date, year = yr, month = mn,
          slot_ggr = slot_ggr, table_ggr = table_ggr,
          poker_ggr = poker_ggr, sports_ggr = sports_ggr,
          total_ggr = total_ggr
        )
      }
    }

    all_data <- do.call(rbind, all_months)

    # Filter CY2022
    cy2022 <- all_data[all_data$year == 2022, ]

    cat("\n  CY2022 monthly data (", nrow(cy2022), " months):\n", sep="")
    for (j in 1:nrow(cy2022)) {
      cat("    ", format(cy2022$date[j], "%b %Y"), ": Slot=$",
          formatC(round(cy2022$slot_ggr[j]), format="d", big.mark=","),
          " Table=$", formatC(round(cy2022$table_ggr[j]), format="d", big.mark=","),
          " Total=$", formatC(round(cy2022$total_ggr[j]), format="d", big.mark=","),
          "\n", sep="")
    }

    # Sum CY2022
    slot_sum <- round(sum(cy2022$slot_ggr))
    table_sum <- round(sum(cy2022$table_ggr))
    poker_sum <- round(sum(cy2022$poker_ggr))
    sports_sum <- round(sum(cy2022$sports_ggr))
    total_sum <- round(sum(cy2022$total_ggr))

    cat("\n  CY2022 TOTALS:\n")
    cat("    Slot GGR:     $", formatC(slot_sum, format="d", big.mark=","), "\n", sep="")
    cat("    Table GGR:    $", formatC(table_sum, format="d", big.mark=","), "\n", sep="")
    cat("    Poker GGR:    $", formatC(poker_sum, format="d", big.mark=","), "\n", sep="")
    cat("    Sports GGR:   $", formatC(sports_sum, format="d", big.mark=","), "\n", sep="")
    cat("    Total GGR:    $", formatC(total_sum, format="d", big.mark=","), "\n", sep="")

    # Compare to CSV
    csv_row <- ny_csv[grep(substr(casino$name, 1, 15), ny_csv$name), ]
    if (nrow(csv_row) > 0) {
      cat("\n  CSV VALUES:\n")
      cat("    Slot:  $", formatC(csv_row$slots_revenue_2022[1], format="d", big.mark=","), "\n", sep="")
      cat("    Table: $", formatC(csv_row$table_revenue_2022[1], format="d", big.mark=","), "\n", sep="")
      cat("    Total: $", formatC(csv_row$total_revenue_2022[1], format="d", big.mark=","), "\n", sep="")

      cat("\n  DIFFERENCES:\n")
      cat("    Slot:  ", slot_sum - csv_row$slots_revenue_2022[1], "\n", sep="")
      cat("    Table: ", table_sum - csv_row$table_revenue_2022[1], "\n", sep="")
      cat("    Total: ", total_sum - csv_row$total_revenue_2022[1], "\n", sep="")

      # Note: CSV total may include sports + poker in addition to slots + tables
      cat("    Slot+Table+Poker+Sports = $",
          formatC(slot_sum + table_sum + poker_sum + sports_sum, format="d", big.mark=","), "\n", sep="")
    }

  }, error = function(e) {
    cat("  ERROR: ", conditionMessage(e), "\n")
  })
  cat("\n")
}

# =========================================
# VLT FACILITIES (8) — .xls files
# =========================================
cat("\n", rep("=", 80), "\n", sep="")
cat("VLT FACILITIES\n")
cat(rep("=", 80), "\n\n", sep="")

vlt_casinos <- list(
  list(name = "Resorts World Casino NYC", file = "ny_resorts-world-casino-nyc.xls"),
  list(name = "Empire City Casino", file = "ny_empire-city-casino.xls"),
  list(name = "Jake's 58", file = "ny_jakes-58-hotel-and-casino.xls"),
  list(name = "Saratoga Casino Hotel", file = "ny_saratoga-casino.xls"),
  list(name = "Finger Lakes Gaming", file = "ny_finger-lakes-gaming-racetrack.xls"),
  list(name = "Batavia Downs Gaming", file = "ny_batavia-downs-gaming.xls"),
  list(name = "Hamburg Gaming", file = "ny_hamburg-gaming.xls"),
  list(name = "Vernon Downs Casino Hotel", file = "ny_vernon-downs-casino.xls")
)

for (casino in vlt_casinos) {
  fpath <- file.path(data_dir, casino$file)
  cat("=== ", casino$name, " ===\n", sep="")

  tryCatch({
    sheets <- excel_sheets(fpath)
    cat("  Sheets: ", paste(sheets, collapse=", "), "\n", sep="")

    # Find FY 21-22 and 22-23 sheets
    fy2122 <- sheets[grepl("21-22|2021.2022", sheets, ignore.case = TRUE)]
    fy2223 <- sheets[grepl("22-23|2022.2023", sheets, ignore.case = TRUE)]

    if (length(fy2122) == 0 || length(fy2223) == 0) {
      cat("  WARNING: Could not find both required FY sheets\n")
      cat("  Available: ", paste(sheets, collapse=", "), "\n\n")
      next
    }

    all_months <- list()

    for (sh in c(fy2122[1], fy2223[1])) {
      df <- read_excel(fpath, sheet = sh, col_names = FALSE)
      ncols <- ncol(df)
      cat("  Sheet '", sh, "': ", nrow(df), " rows x ", ncols, " cols\n", sep="")

      # VLT files typically have fewer columns — just VLT net win
      # Print header to identify structure
      if (nrow(df) >= 10) {
        row8 <- as.character(df[8, ])
        row8[is.na(row8)] <- ""
        cat("  Row 8: ", paste(row8[row8 != ""], collapse=" | "), "\n", sep="")

        if (nrow(df) >= 13) {
          row13 <- as.character(df[13, ])
          row13[is.na(row13)] <- "NA"
          cat("  Header: ", paste(row13[1:min(ncols, 12)], collapse=" | "), "\n", sep="")
        }
      }

      # VLT net win is typically the last meaningful column
      # Read monthly data
      for (i in 14:min(25, nrow(df))) {
        month_serial <- suppressWarnings(as.numeric(df[[1]][i]))
        if (is.na(month_serial) || month_serial == 0) next

        month_date <- excel_to_date(month_serial)
        yr <- as.integer(format(month_date, "%Y"))
        mn <- as.integer(format(month_date, "%m"))

        # Net win — try last column, then col before NA
        total_val <- suppressWarnings(as.numeric(df[[ncols]][i]))
        if (is.na(total_val)) total_val <- 0

        all_months[[length(all_months) + 1]] <- data.frame(
          date = month_date, year = yr, month = mn,
          net_win = total_val
        )
      }
    }

    all_data <- do.call(rbind, all_months)
    cy2022 <- all_data[all_data$year == 2022, ]

    cat("  CY2022 months: ", nrow(cy2022), "\n", sep="")
    vlt_total <- round(sum(cy2022$net_win))
    cat("  CY2022 VLT Net Win: $", formatC(vlt_total, format="d", big.mark=","), "\n", sep="")

    # CSV comparison
    csv_row <- ny_csv[grep(substr(casino$name, 1, 12), ny_csv$name, ignore.case = TRUE), ]
    if (nrow(csv_row) > 0) {
      csv_total <- csv_row$total_revenue_2022[1]
      cat("  CSV Total:          $", formatC(csv_total, format="d", big.mark=","), "\n", sep="")
      diff <- vlt_total - csv_total
      pct <- round(diff / csv_total * 100, 2)
      cat("  Difference:         $", formatC(diff, format="d", big.mark=","), " (", pct, "%)\n", sep="")
      if (abs(diff) < 2) {
        cat("  STATUS: EXACT MATCH\n")
      } else if (abs(pct) < 0.01) {
        cat("  STATUS: MATCH (rounding)\n")
      } else {
        cat("  STATUS: MISMATCH — investigate\n")
      }
    }

  }, error = function(e) {
    cat("  ERROR: ", conditionMessage(e), "\n")
  })
  cat("\n")
}
