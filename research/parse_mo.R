library(readxl)

# Parse Missouri monthly Excel files for CY2022 AGR by casino
# Also extract table win and slot win from TABLE STATS and SLOT STATS sheets

months_files <- c(
  "01" = "data/mo_jan22.xlsx",
  "02" = "data/mo_feb22.xlsx",
  "03" = "data/mo_mar22.xls",
  "04" = "data/mo_apr22.xls",
  "05" = "data/mo_may22.xls",
  "06" = "data/mo_jun22.xls",
  "07" = "data/mo_jul22.xls",
  "08" = "data/mo_aug22.xls",
  "09" = "data/mo_sep22.xls",
  "10" = "data/mo_oct22.xls",
  "11" = "data/mo_nov22.xls",
  "12" = "data/mo_dec22.xls"
)

extract_month_data <- function(f, target_month, sheet_name) {
  d <- suppressMessages(read_excel(f, sheet = sheet_name, col_names = FALSE))

  # The target date for the month
  target_date <- as.Date(paste0("2022-", target_month, "-01"))

  # Track which casino block we're in
  current_casino <- NA
  results <- list()

  for (i in 1:nrow(d)) {
    val1 <- as.character(d[[1]][i])

    # Skip NA and header rows
    if (is.na(val1) || val1 == "") next

    # Check if this is a casino name row (not a date row, not TOTAL)
    if (!grepl("^TOTAL|^MISSOURI|^FISCAL|^MONTH|^BOAT|^NA$|^\\(", val1) &&
        !grepl("^[0-9]{4}-", val1)) {
      # Check if column 2 looks like a date (meaning this is a data row, col1 is a casino name)
      val2 <- d[[2]][i]
      is_date <- FALSE
      if (!is.na(val2)) {
        dt <- tryCatch({
          if (inherits(val2, "POSIXct") || inherits(val2, "Date")) {
            as.Date(val2)
          } else if (is.numeric(val2)) {
            as.Date(val2 - 1, origin = "1899-12-30")
          } else {
            as.Date(as.character(val2))
          }
        }, error = function(e) NA)
        if (!is.na(dt)) is_date <- TRUE
      }

      if (is_date) {
        # This row has casino name in col1 and date in col2 - it's the first month row
        current_casino <- val1

        if (dt == target_date) {
          # Extract the win/AGR value from the appropriate column
          if (sheet_name == "TABLE STATS") {
            win <- as.numeric(d[[4]][i])  # TABLE WIN column
          } else if (sheet_name == "SLOT STATS") {
            win <- as.numeric(d[[4]][i])  # SLOT WIN column
          } else {
            # MONTHLY STATS - AGR is typically col 11
            win <- as.numeric(d[[11]][i])
          }

          if (!is.na(win)) {
            results[[length(results) + 1]] <- data.frame(
              casino = current_casino, value = win, stringsAsFactors = FALSE
            )
          }
        }
      } else {
        # Not a date in col2 - this is a casino header
        current_casino <- val1
      }
    }

    # Check if col 2 is the target date for non-first rows
    if (!is.na(val1) && is.na(current_casino)) next
    val2 <- d[[2]][i]
    if (is.na(val2)) next

    dt <- tryCatch({
      if (inherits(val2, "POSIXct") || inherits(val2, "Date")) {
        as.Date(val2)
      } else if (is.numeric(val2)) {
        as.Date(val2 - 1, origin = "1899-12-30")
      } else {
        as.Date(as.character(val2))
      }
    }, error = function(e) NA)

    if (!is.na(dt) && dt == target_date && !is.na(current_casino)) {
      if (sheet_name == "TABLE STATS") {
        win <- as.numeric(d[[4]][i])
      } else if (sheet_name == "SLOT STATS") {
        win <- as.numeric(d[[4]][i])
      } else {
        win <- as.numeric(d[[11]][i])
      }

      if (!is.na(win)) {
        # Avoid duplicates
        already <- any(sapply(results, function(r) r$casino == current_casino))
        if (!already) {
          results[[length(results) + 1]] <- data.frame(
            casino = current_casino, value = win, stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(results) > 0) do.call(rbind, results) else NULL
}

# Extract AGR from MONTHLY STATS for each month
all_agr <- list()
all_table <- list()
all_slot <- list()

for (mn in names(months_files)) {
  f <- months_files[mn]
  cat("Processing", f, "month", mn, "...\n")

  # AGR from MONTHLY STATS (sheet 1)
  agr <- tryCatch(extract_month_data(f, mn, 1), error = function(e) {
    cat("  Error in MONTHLY STATS:", e$message, "\n"); NULL
  })
  if (!is.null(agr)) {
    agr$month <- mn
    agr$type <- "agr"
    all_agr[[mn]] <- agr
  }

  # Table win from TABLE STATS
  tw <- tryCatch(extract_month_data(f, mn, "TABLE STATS"), error = function(e) {
    cat("  Error in TABLE STATS:", e$message, "\n"); NULL
  })
  if (!is.null(tw)) {
    tw$month <- mn
    tw$type <- "table"
    all_table[[mn]] <- tw
  }

  # Slot win from SLOT STATS
  sw <- tryCatch(extract_month_data(f, mn, "SLOT STATS"), error = function(e) {
    cat("  Error in SLOT STATS:", e$message, "\n"); NULL
  })
  if (!is.null(sw)) {
    sw$month <- mn
    sw$type <- "slot"
    all_slot[[mn]] <- sw
  }
}

agr_df <- do.call(rbind, all_agr)
table_df <- do.call(rbind, all_table)
slot_df <- do.call(rbind, all_slot)

cat("\nAGR records:", nrow(agr_df), "\n")
cat("Table records:", nrow(table_df), "\n")
cat("Slot records:", nrow(slot_df), "\n")

# Aggregate by casino
if (nrow(agr_df) > 0) {
  cy_agr <- aggregate(value ~ casino, data = agr_df, FUN = sum)
  names(cy_agr)[2] <- "total_agr"
  cy_agr <- cy_agr[order(-cy_agr$total_agr), ]

  cat("\n=== MISSOURI CY2022 AGR (Total) ===\n")
  for (i in 1:nrow(cy_agr)) {
    n_months <- sum(agr_df$casino == cy_agr$casino[i])
    cat(sprintf("%-40s $%15s  (%d months)\n", cy_agr$casino[i],
                format(round(cy_agr$total_agr[i]), big.mark = ","), n_months))
  }
  cat(sprintf("\n%-40s $%15s\n", "GRAND TOTAL",
              format(round(sum(cy_agr$total_agr)), big.mark = ",")))
}

if (nrow(table_df) > 0) {
  cy_table <- aggregate(value ~ casino, data = table_df, FUN = sum)
  names(cy_table)[2] <- "table_win"
  cat("\n=== TABLE WIN ===\n")
  for (i in 1:nrow(cy_table)) {
    cat(sprintf("%-40s $%15s\n", cy_table$casino[i],
                format(round(cy_table$table_win[i]), big.mark = ",")))
  }
}

if (nrow(slot_df) > 0) {
  cy_slot <- aggregate(value ~ casino, data = slot_df, FUN = sum)
  names(cy_slot)[2] <- "slot_win"
  cat("\n=== SLOT WIN ===\n")
  for (i in 1:nrow(cy_slot)) {
    cat(sprintf("%-40s $%15s\n", cy_slot$casino[i],
                format(round(cy_slot$slot_win[i]), big.mark = ",")))
  }
}
