# Comprehensive Revenue Verification
# Uses pdftools for PDFs, readxl for Excel
# Working directory: research/

library(readxl)
library(pdftools)
options(scipen=999)

cat("=", paste(rep("=", 79), collapse=""), "\n", sep="")
cat("COMPREHENSIVE REVENUE VERIFICATION\n")
cat("=", paste(rep("=", 79), collapse=""), "\n\n", sep="")

######################################################################
# IOWA — PDF Annual Report
######################################################################
cat(paste(rep("=", 80), collapse=""), "\n")
cat("IOWA (IRGC CY2022 Annual Report PDF)\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

ia_csv <- read.csv("data/ia_revenue_2022.csv", stringsAsFactors=FALSE)
ia_text <- pdf_text("data/ia_fy2022_annual_report.pdf")
cat("Iowa PDF pages:", length(ia_text), "\n\n")

# Search for revenue tables in each page
for (p in 1:length(ia_text)) {
  page <- ia_text[p]
  # Look for pages with casino revenue data
  if (grepl("Ameristar|Prairie Meadows|Horseshoe|Diamond Jo", page, ignore.case=TRUE)) {
    cat(sprintf("--- Page %d (contains casino names) ---\n", p))
    cat(page, "\n\n")
  }
}

######################################################################
# MASSACHUSETTS — Annual Report PDFs
######################################################################
cat(paste(rep("=", 80), collapse=""), "\n")
cat("MASSACHUSETTS (MGC Annual Reports)\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

ma_csv <- read.csv("data/ma_revenue_2022.csv", stringsAsFactors=FALSE)

# FY2022 report (Jul 2021 - Jun 2022) → first half of CY2022 = Jan-Jun 2022
ma_fy22 <- pdf_text("data/ma_annual_report_2022.pdf")
cat("MA FY22 report pages:", length(ma_fy22), "\n")
for (p in 1:length(ma_fy22)) {
  page <- ma_fy22[p]
  if (grepl("Encore|MGM|Plainridge|gross gaming|GGR", page, ignore.case=TRUE)) {
    cat(sprintf("\n--- FY22 Page %d ---\n", p))
    cat(page, "\n")
  }
}

# FY2023 report (Jul 2022 - Jun 2023) → second half of CY2022 = Jul-Dec 2022
ma_fy23 <- pdf_text("data/ma_annual_report_2023.pdf")
cat("\nMA FY23 report pages:", length(ma_fy23), "\n")
for (p in 1:length(ma_fy23)) {
  page <- ma_fy23[p]
  if (grepl("Encore|MGM|Plainridge|gross gaming|GGR", page, ignore.case=TRUE)) {
    cat(sprintf("\n--- FY23 Page %d ---\n", p))
    cat(page, "\n")
  }
}

# Also check monthly report
ma_dec <- pdf_text("data/ma_dec2022_ggr.pdf")
cat("\nMA Dec 2022 monthly report pages:", length(ma_dec), "\n")
for (p in 1:length(ma_dec)) {
  cat(sprintf("\n--- Dec22 Page %d ---\n", p))
  cat(ma_dec[p], "\n")
}

######################################################################
# MISSOURI — Annual Report PDF
######################################################################
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("MISSOURI (MGC FY2022 Annual Report PDF)\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

mo_csv <- read.csv("data/mo_revenue_2022.csv", stringsAsFactors=FALSE)
mo_text <- pdf_text("data/mo_ar_2022.pdf")
cat("MO Annual Report pages:", length(mo_text), "\n")
for (p in 1:length(mo_text)) {
  page <- mo_text[p]
  if (grepl("Ameristar|River City|Hollywood|Argosy|Harrah|revenue|AGR", page, ignore.case=TRUE)) {
    cat(sprintf("\n--- Page %d ---\n", p))
    cat(page, "\n")
  }
}

######################################################################
# OHIO — Monthly PDF
######################################################################
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("OHIO (Monthly PDF)\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

oh_csv <- read.csv("data/oh_revenue_2022.csv", stringsAsFactors=FALSE)
oh_text <- pdf_text("data/oh_monthly.pdf")
cat("OH PDF pages:", length(oh_text), "\n")
for (p in 1:length(oh_text)) {
  page <- oh_text[p]
  if (grepl("Hollywood|JACK|Hard Rock|revenue|casino", page, ignore.case=TRUE)) {
    cat(sprintf("\n--- Page %d ---\n", p))
    cat(page, "\n")
  }
}

######################################################################
# CONNECTICUT — Mohegan earnings deck
######################################################################
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("CONNECTICUT (Mohegan Q4 FY22 Earnings Deck)\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

ct_csv <- read.csv("data/ct_revenue_2022.csv", stringsAsFactors=FALSE)
ct_text <- pdf_text("data/mohegan_q4fy22_earnings_deck.pdf")
cat("Mohegan earnings deck pages:", length(ct_text), "\n")
for (p in 1:length(ct_text)) {
  page <- ct_text[p]
  if (grepl("table|slot|revenue|gaming|31|32|33", page, ignore.case=TRUE)) {
    cat(sprintf("\n--- Page %d ---\n", p))
    cat(page, "\n")
  }
}

######################################################################
# INDIANA — Read one monthly file carefully
######################################################################
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("INDIANA — Detailed parse of December 2022\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

in_dec <- suppressMessages(read_excel("data/in_2022_12_revenue.xlsx", col_names=FALSE))
cat("Dec 2022 file: ", nrow(in_dec), " rows x ", ncol(in_dec), " cols\n\n")

# Print ALL rows raw
for (r in 1:nrow(in_dec)) {
  vals <- sapply(in_dec[r,], function(x) {
    if(is.na(x)) "NA"
    else if(is.numeric(x)) formatC(x, format="f", digits=0, big.mark=",")
    else as.character(x)
  })
  cat(sprintf("Row %2d: %s\n", r, paste(vals, collapse=" | ")))
}

######################################################################
# NEW YORK — Read individual casino file carefully
######################################################################
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("NEW YORK — Detailed parse of Resorts World Catskills\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

rwc <- suppressMessages(read_excel("data/ny_resorts-world-catskills.xlsx", col_names=FALSE))
cat("RWC file:", nrow(rwc), "x", ncol(rwc), "\n\n")
for (r in 1:nrow(rwc)) {
  vals <- sapply(rwc[r,], function(x) {
    if(is.na(x)) "NA"
    else if(is.numeric(x)) formatC(x, format="f", digits=2, big.mark=",")
    else as.character(x)
  })
  cat(sprintf("Row %2d: %s\n", r, paste(vals[1:min(12, length(vals))], collapse=" | ")))
}

# Also read one VLT file
cat("\n--- Resorts World Casino NYC (VLT) ---\n")
rwnyc <- suppressMessages(read_excel("data/ny_resorts-world-casino-nyc.xls", col_names=FALSE))
cat("RWNYC file:", nrow(rwnyc), "x", ncol(rwnyc), "\n\n")
for (r in 1:nrow(rwnyc)) {
  vals <- sapply(rwnyc[r,], function(x) {
    if(is.na(x)) "NA"
    else if(is.numeric(x)) formatC(x, format="f", digits=2, big.mark=",")
    else as.character(x)
  })
  cat(sprintf("Row %2d: %s\n", r, paste(vals[1:min(10, length(vals))], collapse=" | ")))
}

cat("\n\nDone.\n")
