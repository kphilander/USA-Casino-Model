library(readxl)

data_dir <- "data"
excel_to_date <- function(x) as.Date(x, origin = "1899-12-30")

ny_csv <- read.csv(file.path(data_dir, "ny_revenue_2022.csv"), stringsAsFactors = FALSE)

cat("=" , rep("=", 79), "\n", sep="")
cat("NY VLT VERIFICATION — Net Win (col 5) from individual Excel files\n")
cat("=" , rep("=", 79), "\n\n")

vlt_casinos <- list(
  list(name = "Resorts World Casino NYC", file = "ny_resorts-world-casino-nyc.xls", csv_name = "Resorts World Casino NYC"),
  list(name = "Empire City Casino", file = "ny_empire-city-casino.xls", csv_name = "Empire City Casino"),
  list(name = "Jake's 58", file = "ny_jakes-58-hotel-and-casino.xls", csv_name = "Jake's 58"),
  list(name = "Saratoga Casino Hotel", file = "ny_saratoga-casino.xls", csv_name = "Saratoga Casino Hotel"),
  list(name = "Finger Lakes Gaming", file = "ny_finger-lakes-gaming-racetrack.xls", csv_name = "Finger Lakes Gaming"),
  list(name = "Batavia Downs Gaming", file = "ny_batavia-downs-gaming.xls", csv_name = "Batavia Downs Gaming"),
  list(name = "Hamburg Gaming", file = "ny_hamburg-gaming.xls", csv_name = "Hamburg Gaming"),
  list(name = "Vernon Downs Casino Hotel", file = "ny_vernon-downs-casino.xls", csv_name = "Vernon Downs Casino Hotel")
)

for (casino in vlt_casinos) {
  fpath <- file.path(data_dir, casino$file)
  cat("=== ", casino$name, " ===\n", sep="")

  tryCatch({
    sheets <- excel_sheets(fpath)

    fy2122 <- sheets[grepl("21-22", sheets)]
    fy2223 <- sheets[grepl("22-23", sheets)]

    if (length(fy2122) == 0 || length(fy2223) == 0) {
      cat("  WARNING: Missing FY sheets\n\n")
      next
    }

    all_months <- list()

    for (sh in c(fy2122[1], fy2223[1])) {
      df <- read_excel(fpath, sheet = sh, col_names = FALSE)
      ncols <- ncol(df)
      nrows <- nrow(df)

      cat("  Sheet '", sh, "': ", nrows, "x", ncols, "\n", sep="")

      # Print all rows with numeric month serials and their col 1, 2, 5 values
      for (i in 1:nrows) {
        month_serial <- suppressWarnings(as.numeric(df[[1]][i]))
        if (!is.na(month_serial) && month_serial > 40000 && month_serial < 50000) {
          month_date <- excel_to_date(month_serial)
          played <- suppressWarnings(as.numeric(df[[2]][i]))
          net_win <- suppressWarnings(as.numeric(df[[5]][i]))
          commission <- suppressWarnings(as.numeric(df[[ncols]][i]))

          if (is.na(net_win)) net_win <- 0
          if (is.na(played)) played <- 0
          if (is.na(commission)) commission <- 0

          yr <- as.integer(format(month_date, "%Y"))
          mn <- as.integer(format(month_date, "%m"))

          cat("    Row", sprintf("%2d", i), " ", format(month_date, "%b %Y"),
              ": Played=$", formatC(round(played), format="d", big.mark=","),
              " NetWin=$", formatC(round(net_win), format="d", big.mark=","),
              " Commission=$", formatC(round(commission), format="d", big.mark=","),
              "\n", sep="")

          all_months[[length(all_months) + 1]] <- data.frame(
            date = month_date, year = yr, month = mn,
            played = played, net_win = net_win, commission = commission,
            sheet = sh
          )
        }
      }
    }

    if (length(all_months) == 0) {
      cat("  No monthly data found\n\n")
      next
    }

    all_data <- do.call(rbind, all_months)
    cy2022 <- all_data[all_data$year == 2022, ]

    cat("\n  CY2022 months: ", nrow(cy2022), "\n", sep="")

    net_win_total <- round(sum(cy2022$net_win))
    played_total <- round(sum(cy2022$played))
    commission_total <- round(sum(cy2022$commission))

    cat("  CY2022 Played:     $", formatC(played_total, format="d", big.mark=","), "\n", sep="")
    cat("  CY2022 Net Win:    $", formatC(net_win_total, format="d", big.mark=","), "\n", sep="")
    cat("  CY2022 Commission: $", formatC(commission_total, format="d", big.mark=","), "\n", sep="")

    # CSV comparison
    csv_row <- ny_csv[ny_csv$name == casino$csv_name, ]
    if (nrow(csv_row) == 0) {
      csv_row <- ny_csv[grep(substr(casino$csv_name, 1, 10), ny_csv$name, ignore.case=TRUE), ]
    }
    if (nrow(csv_row) > 0) {
      csv_total <- csv_row$total_revenue_2022[1]
      cat("  CSV Total:         $", formatC(csv_total, format="d", big.mark=","), "\n", sep="")

      diff_nw <- net_win_total - csv_total
      pct_nw <- round(diff_nw / csv_total * 100, 2)
      cat("  Net Win vs CSV:    $", formatC(diff_nw, format="d", big.mark=","), " (", pct_nw, "%)\n", sep="")

      diff_played <- played_total - csv_total
      pct_played <- round(diff_played / csv_total * 100, 2)
      cat("  Played vs CSV:     $", formatC(diff_played, format="d", big.mark=","), " (", pct_played, "%)\n", sep="")

      if (abs(pct_nw) < 1) {
        cat("  STATUS: NET WIN MATCHES\n")
      } else if (abs(pct_played) < 1) {
        cat("  STATUS: PLAYED MATCHES — CSV may use 'played' metric\n")
      } else {
        cat("  STATUS: NEEDS INVESTIGATION\n")
      }
    }
  }, error = function(e) {
    cat("  ERROR: ", conditionMessage(e), "\n")
  })
  cat("\n")
}
