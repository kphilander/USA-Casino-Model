# Revenue Verification Script — Robust version
# Cross-validates CSV revenue data against source Excel files
# Working directory: research/

library(readxl)
options(scipen=999)

cat("=", rep("=", 79), "\n", sep="")
cat("REVENUE VERIFICATION: Cross-validating CSV data against source files\n")
cat("=", rep("=", 79), "\n\n")

######################################################################
# INDIANA — 12 monthly Excel files
######################################################################
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("INDIANA (12 monthly Excel files from IGC)\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

in_csv <- read.csv("data/in_revenue_2022.csv", stringsAsFactors=FALSE)

# Read each file with col_names=FALSE to handle inconsistent headers
months <- sprintf("%02d", 1:12)
in_casino_totals <- list()

for (m in months) {
  f <- paste0("data/in_2022_", m, "_revenue.xlsx")
  if (!file.exists(f)) { cat(sprintf("  MISSING: %s\n", f)); next }

  df <- suppressMessages(read_excel(f, col_names=FALSE))

  # Convert all to character for pattern matching
  df_char <- as.data.frame(lapply(df, as.character), stringsAsFactors=FALSE)

  # Find rows that look like casino data (contain a known casino name)
  casino_keywords <- c("Ameristar", "Hard Rock", "Horseshoe", "Caesars",
                        "Harrah", "Bally", "Hollywood", "Blue Chip",
                        "Belterra", "French Lick", "Rising Star", "Hoosier")

  # Find the "Win" header row to identify columns
  for (r in 1:min(5, nrow(df_char))) {
    row_text <- paste(df_char[r,], collapse=" ")
    if (grepl("Win|Wagering|Total Win", row_text, ignore.case=TRUE)) {
      cat(sprintf("  Month %s header row %d: %s\n", m, r, row_text))
      break
    }
  }

  # Look for casino rows — typically col 1 has the name
  for (r in 1:nrow(df_char)) {
    name_val <- df_char[r, 1]
    if (is.na(name_val)) next

    # Check if this row matches a casino keyword
    matched <- FALSE
    for (kw in casino_keywords) {
      if (grepl(kw, name_val, ignore.case=TRUE)) {
        matched <- TRUE
        break
      }
    }

    if (matched) {
      # Find the last numeric column value (likely the total)
      nums <- suppressWarnings(as.numeric(gsub("[,$]", "", df_char[r, ])))
      valid_nums <- nums[!is.na(nums)]

      if (length(valid_nums) > 0) {
        # The "Total Win" is typically the last large number in the row
        total_win <- max(valid_nums)  # largest number = total

        # Clean casino name
        clean_name <- trimws(name_val)

        if (is.null(in_casino_totals[[clean_name]])) {
          in_casino_totals[[clean_name]] <- 0
        }
        in_casino_totals[[clean_name]] <- in_casino_totals[[clean_name]] + total_win
      }
    }
  }
}

cat("\nSummed monthly totals vs CSV:\n\n")
cat(sprintf("  %-45s %-18s %-18s %s\n", "Casino", "CSV Total", "Monthly Sum", "Status"))
cat(sprintf("  %s\n", paste(rep("-", 100), collapse="")))

for (i in 1:nrow(in_csv)) {
  csv_name <- in_csv$name[i]
  csv_total <- in_csv$total_revenue_2022[i]

  # Try to find matching monthly sum
  best_match <- NA
  best_name <- ""
  for (mn in names(in_casino_totals)) {
    # Fuzzy match: check if first significant word matches
    if (grepl(substr(csv_name, 1, 10), mn, ignore.case=TRUE) ||
        grepl(substr(mn, 1, 10), csv_name, ignore.case=TRUE)) {
      best_match <- in_casino_totals[[mn]]
      best_name <- mn
      break
    }
  }

  if (!is.na(best_match)) {
    diff <- abs(csv_total - best_match)
    pct_diff <- diff / csv_total * 100
    status <- if(pct_diff < 1) "MATCH" else sprintf("DIFF %.1f%%", pct_diff)
    cat(sprintf("  %-45s $%-17s $%-17s %s\n",
                csv_name,
                formatC(csv_total, format="d", big.mark=","),
                formatC(round(best_match), format="d", big.mark=","),
                status))
  } else {
    cat(sprintf("  %-45s $%-17s %-18s NO MATCH FOUND\n",
                csv_name,
                formatC(csv_total, format="d", big.mark=","),
                ""))
  }
}

cat(sprintf("\n  IN CSV state total:    $%s\n", formatC(sum(in_csv$total_revenue_2022), format="d", big.mark=",")))
cat(sprintf("  IN Monthly sum total:  $%s\n", formatC(round(sum(unlist(in_casino_totals))), format="d", big.mark=",")))

# List all found casino names for debugging
cat("\n  All casino names found in monthly files:\n")
for (n in sort(names(in_casino_totals))) {
  cat(sprintf("    %s: $%s\n", n, formatC(round(in_casino_totals[[n]]), format="d", big.mark=",")))
}

######################################################################
# NEW YORK — Monthly summary files
######################################################################
cat("\n\n", paste(rep("=", 80), collapse=""), "\n")
cat("NEW YORK\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

ny_csv <- read.csv("data/ny_revenue_2022.csv", stringsAsFactors=FALSE)

# Commercial Casino Monthly
cat("--- Commercial Casinos ---\n")
ny_comm <- suppressMessages(read_excel("data/ny_commercial_casino_monthly.xlsx", col_names=FALSE))
cat("Dimensions:", nrow(ny_comm), "x", ncol(ny_comm), "\n")
# Print first 20 rows to understand structure
cat("First 20 rows:\n")
for (r in 1:min(20, nrow(ny_comm))) {
  vals <- sapply(ny_comm[r,], function(x) if(is.na(x)) "NA" else as.character(x))
  cat(sprintf("  Row %2d: %s\n", r, paste(vals[1:min(8, length(vals))], collapse=" | ")))
}

# VLT Monthly
cat("\n--- VLT Facilities ---\n")
ny_vlt <- suppressMessages(read_excel("data/ny_vlt_monthly.xlsx", col_names=FALSE))
cat("Dimensions:", nrow(ny_vlt), "x", ncol(ny_vlt), "\n")
cat("First 20 rows:\n")
for (r in 1:min(20, nrow(ny_vlt))) {
  vals <- sapply(ny_vlt[r,], function(x) if(is.na(x)) "NA" else as.character(x))
  cat(sprintf("  Row %2d: %s\n", r, paste(vals[1:min(8, length(vals))], collapse=" | ")))
}

# Individual casino files — just print the first one to understand structure
cat("\n--- Individual Casino File Structure ---\n")
rwc <- suppressMessages(read_excel("data/ny_resorts-world-catskills.xlsx", col_names=FALSE))
cat("Resorts World Catskills file:\n")
cat("Dimensions:", nrow(rwc), "x", ncol(rwc), "\n")
for (r in 1:min(25, nrow(rwc))) {
  vals <- sapply(rwc[r,], function(x) if(is.na(x)) "NA" else as.character(x))
  cat(sprintf("  Row %2d: %s\n", r, paste(vals[1:min(10, length(vals))], collapse=" | ")))
}

# Individual VLT file — print first one
cat("\n--- Individual VLT File Structure ---\n")
rwnyc <- suppressMessages(read_excel("data/ny_resorts-world-casino-nyc.xls", col_names=FALSE))
cat("Resorts World Casino NYC file:\n")
cat("Dimensions:", nrow(rwnyc), "x", ncol(rwnyc), "\n")
for (r in 1:min(25, nrow(rwnyc))) {
  vals <- sapply(rwnyc[r,], function(x) if(is.na(x)) "NA" else as.character(x))
  cat(sprintf("  Row %2d: %s\n", r, paste(vals[1:min(10, length(vals))], collapse=" | ")))
}

######################################################################
# MISSOURI — Monthly Excel files
######################################################################
cat("\n\n", paste(rep("=", 80), collapse=""), "\n")
cat("MISSOURI (FY2022 = Jul 2021 - Jun 2022)\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

mo_csv <- read.csv("data/mo_revenue_2022.csv", stringsAsFactors=FALSE)

# Read the first MO monthly file to understand structure
mo_jan <- suppressMessages(read_excel("data/mo_jan22.xlsx", col_names=FALSE))
cat("MO Jan22 structure:\n")
cat("Dimensions:", nrow(mo_jan), "x", ncol(mo_jan), "\n")
for (r in 1:min(25, nrow(mo_jan))) {
  vals <- sapply(mo_jan[r,], function(x) if(is.na(x)) "NA" else as.character(x))
  cat(sprintf("  Row %2d: %s\n", r, paste(vals[1:min(8, length(vals))], collapse=" | ")))
}

######################################################################
# STATE TOTALS
######################################################################
cat("\n\n", paste(rep("=", 80), collapse=""), "\n")
cat("ALL STATE TOTALS FROM CSVs\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

all_states <- list(
  PA = read.csv("data/pa_revenue_2022.csv", stringsAsFactors=FALSE),
  OH = read.csv("data/oh_revenue_2022.csv", stringsAsFactors=FALSE),
  MD = read.csv("data/md_revenue_2022.csv", stringsAsFactors=FALSE),
  NY = read.csv("data/ny_revenue_2022.csv", stringsAsFactors=FALSE),
  MA = read.csv("data/ma_revenue_2022.csv", stringsAsFactors=FALSE),
  CT = read.csv("data/ct_revenue_2022.csv", stringsAsFactors=FALSE),
  IN_st = read.csv("data/in_revenue_2022.csv", stringsAsFactors=FALSE),
  MO = read.csv("data/mo_revenue_2022.csv", stringsAsFactors=FALSE),
  IA = read.csv("data/ia_revenue_2022.csv", stringsAsFactors=FALSE)
)

grand_total <- 0
grand_n <- 0
for (s in names(all_states)) {
  df <- all_states[[s]]
  n <- nrow(df)
  total <- sum(df$total_revenue_2022, na.rm=TRUE)
  grand_total <- grand_total + total
  grand_n <- grand_n + n
  cat(sprintf("  %-8s %2d properties   Total = $%s\n", s, n, formatC(total, format="d", big.mark=",")))

  # Print each property
  for (i in 1:n) {
    slots <- if(!is.na(df$slots_revenue_2022[i])) formatC(df$slots_revenue_2022[i], format="d", big.mark=",") else "NA"
    tables <- if(!is.na(df$table_revenue_2022[i])) formatC(df$table_revenue_2022[i], format="d", big.mark=",") else "NA"
    cat(sprintf("    ID=%-5s %-45s Slots=%-15s Tables=%-15s Total=$%s\n",
                df$casino_id[i],
                df$name[i],
                slots, tables,
                formatC(df$total_revenue_2022[i], format="d", big.mark=",")))
  }

  # Verify slots + tables = total where both are available
  if (any(!is.na(df$slots_revenue_2022) & !is.na(df$table_revenue_2022))) {
    has_both <- !is.na(df$slots_revenue_2022) & !is.na(df$table_revenue_2022)
    computed <- df$slots_revenue_2022[has_both] + df$table_revenue_2022[has_both]
    actual <- df$total_revenue_2022[has_both]
    mismatches <- which(computed != actual)
    if (length(mismatches) > 0) {
      cat("    WARNING: Slots+Tables != Total for:\n")
      for (j in mismatches) {
        idx <- which(has_both)[j]
        cat(sprintf("      %s: slots(%s) + tables(%s) = %s but total = %s\n",
                    df$name[idx],
                    formatC(df$slots_revenue_2022[idx], format="d", big.mark=","),
                    formatC(df$table_revenue_2022[idx], format="d", big.mark=","),
                    formatC(computed[j], format="d", big.mark=","),
                    formatC(actual[idx], format="d", big.mark=",")))
      }
    } else {
      cat("    CHECK: All slots+tables = total ✓\n")
    }
  }
  cat("\n")
}

cat(sprintf("  GRAND TOTAL: %d properties   $%s\n", grand_n, formatC(grand_total, format="d", big.mark=",")))
