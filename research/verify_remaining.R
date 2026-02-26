library(readxl)

data_dir <- "data"
csv_dir <- "data"

cat("=" , rep("=", 79), "\n", sep="")
cat("VERIFICATION SCRIPT: NY, IN, PA remaining properties\n")
cat("=" , rep("=", 79), "\n\n", sep="")

# =====================================================
# NEW YORK — Individual Casino Excel Files
# =====================================================
cat("\n### NEW YORK — COMMERCIAL CASINOS (4) ###\n\n")

ny_commercial <- list(
  "Resorts World Catskills" = "ny_resorts-world-catskills.xlsx",
  "Rivers Casino & Resort Schenectady" = "ny_rivers-casino-and-resort.xlsx",
  "del Lago Resort & Casino" = "ny_del-lago-resort-and-casino.xlsx",
  "Tioga Downs Casino Resort" = "ny_tioga-downs.xlsx"
)

ny_csv <- read.csv(file.path(csv_dir, "ny_revenue_2022.csv"), stringsAsFactors = FALSE)
cat("NY CSV data:\n")
print(ny_csv[, c("name", "slots_revenue_2022", "table_revenue_2022", "total_revenue_2022")])
cat("\n")

for (casino_name in names(ny_commercial)) {
  fname <- ny_commercial[[casino_name]]
  fpath <- file.path(data_dir, fname)
  cat("--- ", casino_name, " (", fname, ") ---\n", sep="")

  tryCatch({
    sheets <- excel_sheets(fpath)
    cat("  Sheets:", paste(sheets, collapse=", "), "\n")

    for (sh in sheets) {
      df <- read_excel(fpath, sheet = sh, col_names = FALSE)
      cat("  Sheet '", sh, "': ", nrow(df), " rows x ", ncol(df), " cols\n", sep="")
      # Print first 30 rows to inspect structure
      cat("  First 30 rows:\n")
      for (i in 1:min(30, nrow(df))) {
        vals <- as.character(df[i, ])
        vals[is.na(vals)] <- ""
        cat("    Row", sprintf("%2d", i), ": ", paste(vals, collapse=" | "), "\n")
      }
      # Look for 2022 data — search for "2022" or year references
      for (i in 1:nrow(df)) {
        row_str <- paste(as.character(df[i, ]), collapse=" ")
        if (grepl("2022", row_str) || grepl("Total|Annual|Grand|CY|FY", row_str, ignore.case=TRUE)) {
          vals <- as.character(df[i, ])
          vals[is.na(vals)] <- ""
          cat("  ** MATCH Row", i, ": ", paste(vals, collapse=" | "), "\n")
        }
      }
    }
  }, error = function(e) {
    cat("  ERROR: ", conditionMessage(e), "\n")
  })
  cat("\n")
}

cat("\n### NEW YORK — VLT FACILITIES (8) ###\n\n")

ny_vlt <- list(
  "Resorts World Casino NYC" = "ny_resorts-world-casino-nyc.xls",
  "Empire City Casino" = "ny_empire-city-casino.xls",
  "Jakes 58" = "ny_jakes-58-hotel-and-casino.xls",
  "Saratoga Casino Hotel" = "ny_saratoga-casino.xls",
  "Finger Lakes Gaming" = "ny_finger-lakes-gaming-racetrack.xls",
  "Batavia Downs" = "ny_batavia-downs-gaming.xls",
  "Hamburg Gaming" = "ny_hamburg-gaming.xls",
  "Vernon Downs" = "ny_vernon-downs-casino.xls"
)

for (casino_name in names(ny_vlt)) {
  fname <- ny_vlt[[casino_name]]
  fpath <- file.path(data_dir, fname)
  cat("--- ", casino_name, " (", fname, ") ---\n", sep="")

  tryCatch({
    sheets <- excel_sheets(fpath)
    cat("  Sheets:", paste(sheets, collapse=", "), "\n")

    for (sh in sheets) {
      df <- read_excel(fpath, sheet = sh, col_names = FALSE)
      cat("  Sheet '", sh, "': ", nrow(df), " rows x ", ncol(df), " cols\n", sep="")
      # Print first 30 rows
      cat("  First 30 rows:\n")
      for (i in 1:min(30, nrow(df))) {
        vals <- as.character(df[i, ])
        vals[is.na(vals)] <- ""
        cat("    Row", sprintf("%2d", i), ": ", paste(vals, collapse=" | "), "\n")
      }
      # Search for relevant rows
      for (i in 1:nrow(df)) {
        row_str <- paste(as.character(df[i, ]), collapse=" ")
        if (grepl("2022", row_str) || grepl("Total|Annual|Grand|CY|FY", row_str, ignore.case=TRUE)) {
          vals <- as.character(df[i, ])
          vals[is.na(vals)] <- ""
          cat("  ** MATCH Row", i, ": ", paste(vals, collapse=" | "), "\n")
        }
      }
    }
  }, error = function(e) {
    cat("  ERROR: ", conditionMessage(e), "\n")
  })
  cat("\n")
}

# =====================================================
# INDIANA — Attempt to parse monthly Excel files
# =====================================================
cat("\n### INDIANA — MONTHLY EXCEL FILES ###\n\n")

in_csv <- read.csv(file.path(csv_dir, "in_revenue_2022.csv"), stringsAsFactors = FALSE)
cat("IN CSV data:\n")
print(in_csv[, c("name", "slots_revenue_2022", "table_revenue_2022", "total_revenue_2022")])
cat("\n")

# Try reading just Jan and Dec to understand format
for (month in c("01", "06", "12")) {
  fname <- paste0("in_2022_", month, "_revenue.xlsx")
  fpath <- file.path(data_dir, fname)
  cat("--- Indiana ", month, "/2022 (", fname, ") ---\n", sep="")

  tryCatch({
    sheets <- excel_sheets(fpath)
    cat("  Sheets:", paste(sheets, collapse=", "), "\n")

    df <- read_excel(fpath, sheet = 1, col_names = FALSE)
    cat("  Rows: ", nrow(df), ", Cols: ", ncol(df), "\n", sep="")
    cat("  All rows:\n")
    for (i in 1:min(50, nrow(df))) {
      vals <- as.character(df[i, ])
      vals[is.na(vals)] <- ""
      cat("    Row", sprintf("%2d", i), ": ", paste(vals, collapse=" | "), "\n")
    }
  }, error = function(e) {
    cat("  ERROR: ", conditionMessage(e), "\n")
  })
  cat("\n")
}

# =====================================================
# PENNSYLVANIA — CSV internal check detail
# =====================================================
cat("\n### PENNSYLVANIA — CSV CHECK ###\n\n")
pa_csv <- read.csv(file.path(csv_dir, "pa_revenue_2022.csv"), stringsAsFactors = FALSE)
cat("PA CSV: all 14 properties\n")
pa_csv$check <- pa_csv$slots_revenue_2022 + pa_csv$table_revenue_2022
pa_csv$diff <- pa_csv$total_revenue_2022 - pa_csv$check
print(pa_csv[, c("name", "slots_revenue_2022", "table_revenue_2022", "total_revenue_2022", "check", "diff")])
cat("\nAll diffs = 0?", all(pa_csv$diff == 0), "\n")
cat("PA state total:", format(sum(pa_csv$total_revenue_2022), big.mark=","), "\n")
