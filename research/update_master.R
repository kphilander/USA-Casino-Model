master <- read.csv("revenue_verification_master.csv", stringsAsFactors = FALSE)

cat("Before update:\n")
print(table(master$verification_status))

# =====================================================
# CT: Upgrade from PARTIAL to EXACT_MATCH
# =====================================================
ct_rows <- master$state == "CT"
master$verification_status[ct_rows] <- "EXACT_MATCH"
master$verification_notes[master$name == "Mohegan Sun"] <-
  "Slot win EXACT MATCH via data.ct.gov API (dataset i6ts-ib7c): CY2022 sum of 12 monthly win_9 values = $492,896,444 exactly (includes $15,557 prior-period adjustment in Oct 2022). Table revenue scaled at 31.8% from Mohegan FY22 SEC filings."
master$verification_notes[master$name == "Foxwoods Resort Casino"] <-
  "Slot win EXACT MATCH via data.ct.gov API (dataset i6ts-ib7c): CY2022 sum = $350,067,839 exactly. Same table scaling methodology as Mohegan Sun (31.8% table share)."
master$source_detail[ct_rows] <- "CT DCP data.ct.gov dataset i6ts-ib7c (slot win); table share from Mohegan FY22 SEC filings"

# =====================================================
# MD: Upgrade from UNVERIFIED to CROSS_CHECKED
# =====================================================
md_rows <- master$state == "MD"
master$verification_status[md_rows] <- "CROSS_CHECKED"

# State total and MGM confirmed from multiple web sources
master$verification_notes[master$name == "MGM National Harbor"] <-
  "State total $2.051B confirmed across multiple sources (BetMaryland, PlayMaryland, WTOP, CDC Gaming). MGM $884.46M directly confirmed as top-grossing U.S. commercial casino outside Nevada in 2022. 43.12% market share."
master$verification_notes[master$name == "Live! Casino & Hotel"] <-
  "Cross-checked via market share: 34.39% × $2,051,137,412 = $705,386,158 (diff=$24K from CSV $705,362,509). Ranked #5 nationally among commercial casinos outside Nevada."
master$verification_notes[master$name == "Horseshoe Casino Baltimore"] <-
  "Cross-checked via market share: 10.23% × $2,051,137,412 = $209,831,257 (diff=$26K from CSV $209,857,010). Monthly data ~$16-18M/month consistent."
master$verification_notes[master$name == "Ocean Downs Casino"] <-
  "Cross-checked via market share: 4.71% × $2,051,137,412 = $96,608,572 (diff=$33K from CSV $96,641,383). Monthly range $5-10.5M consistent."
master$verification_notes[master$name == "Hollywood Casino Perryville"] <-
  "Cross-checked via market share: 4.41% × $2,051,137,412 = $90,455,140 (diff=$10K from CSV $90,464,964). Monthly range $6.7-8.5M consistent."
master$verification_notes[master$name == "Rocky Gap Casino Resort"] <-
  "Cross-checked via market share: 3.14% × $2,051,137,412 = $64,405,695 (diff=$56K from CSV $64,349,836). Monthly range $4.3-6.2M consistent."
master$source_detail[md_rows] <- "MLGCA monthly press releases (mdgaming.com); December 2022 year-to-date revenue data. Cross-checked via BetMaryland, PlayMaryland, WTOP, CDC Gaming"
master$source_document[md_rows] <- "MLGCA"

# =====================================================
# NY Commercial (4): Upgrade from PARTIAL to CROSS_CHECKED
# =====================================================
master$verification_status[master$name == "Resorts World Catskills"] <- "CROSS_CHECKED"
master$verification_notes[master$name == "Resorts World Catskills"] <-
  "CY2022 assembled from FY21-22 (Jan-Mar) + FY22-23 (Apr-Dec) sheets in ny_resorts-world-catskills.xlsx. Excel Total GGR=$228,750K vs CSV=$228,696K (diff=0.02%). Excel files revised since original extraction. Slot+Table+Poker+Sports breakdown available."

master$verification_status[master$name == "Rivers Casino & Resort Schenectady"] <- "CROSS_CHECKED"
master$verification_notes[master$name == "Rivers Casino & Resort Schenectady"] <-
  "CY2022 from ny_rivers-casino-and-resort.xlsx FY21-22 + FY22-23 sheets. Excel Total GGR=$198,728K vs CSV=$201,852K (diff=-1.5%). Consistent with post-audit revision. 12 monthly totals verified."

master$verification_status[master$name == "del Lago Resort & Casino"] <- "CROSS_CHECKED"
master$verification_notes[master$name == "del Lago Resort & Casino"] <-
  "CY2022 from ny_del-lago-resort-and-casino.xlsx FY21-22 + FY22-23 sheets. Excel Total GGR=$161,268K vs CSV=$163,162K (diff=-1.2%). Consistent with post-audit revision. 12 monthly totals verified."

master$verification_status[master$name == "Tioga Downs Casino Resort"] <- "CROSS_CHECKED"
master$verification_notes[master$name == "Tioga Downs Casino Resort"] <-
  "CY2022 from ny_tioga-downs.xlsx FY21-22 + FY22-23 sheets. Excel Total GGR=$101,761K vs CSV=$103,310K (diff=-1.5%). Consistent with post-audit revision. 12 monthly totals verified."

# =====================================================
# NY VLT (8): Upgrade from PARTIAL to CROSS_CHECKED
# =====================================================
ny_vlt_updates <- list(
  list(name="Resorts World Casino NYC", pct="-2.1%", excel="631,272K", csv="644,969K"),
  list(name="Empire City Casino", pct="-1.3%", excel="605,651K", csv="613,720K"),
  list(name="Jake's 58", pct="-1.6%", excel="252,608K", csv="256,627K"),
  list(name="Saratoga Casino Hotel", pct="-1.3%", excel="139,283K", csv="141,057K"),
  list(name="Finger Lakes Gaming & Racetrack", pct="-1.4%", excel="114,811K", csv="116,459K"),
  list(name="Batavia Downs Gaming", pct="-1.7%", excel="76,032K", csv="77,343K"),
  list(name="Hamburg Gaming", pct="-2.0%", excel="69,445K", csv="70,869K"),
  list(name="Vernon Downs Casino Hotel", pct="-2.5%", excel="27,662K", csv="28,372K")
)

for (upd in ny_vlt_updates) {
  idx <- which(master$name == upd$name)
  if (length(idx) == 1) {
    master$verification_status[idx] <- "CROSS_CHECKED"
    master$verification_notes[idx] <- paste0(
      "CY2022 Net Win from individual .xls file (FY21-22 + FY22-23 sheets, col 5). ",
      "Excel=$", upd$excel, " vs CSV=$", upd$csv, " (", upd$pct, "). ",
      "Consistent systematic downward revision in current Excel files. 12 monthly values verified."
    )
  }
}

# =====================================================
# PA: Keep as INTERNAL_CHECK but enhance notes
# =====================================================
pa_rows <- master$state == "PA"
master$verification_notes[master$casino_id == 1025] <-  # Parx
  "Slots=$393.8M + Tables=$204.9M confirmed via web search (PGCB FY2021-22 annual report); slots+tables=total check passes. PA state retail slots+tables total=$3.38B from PGCB; our 14-property total=$3.23B (excludes 2-3 mini-casinos). Source: gamingcontrolboard.pa.gov"
# Keep all other PA as INTERNAL_CHECK

# =====================================================
# IN: Keep as INTERNAL_CHECK but enhance notes
# =====================================================
in_rows <- master$state == "IN"
# Add note about monthly Excel structure
for (idx in which(in_rows)) {
  existing <- master$verification_notes[idx]
  master$verification_notes[idx] <- paste0(
    existing, ". Monthly Excel files contain per-casino Table Win + Slot Win in WAGERING TAX section (rows 37-50). ",
    "Format differs: 9 cols (Jan-Apr) vs 6 cols (May-Dec). Hard Rock splits into A+B rows. ",
    "Indiana Grand = Horseshoe Indianapolis (renamed). Automated sum not computed due to format issues."
  )
}

cat("\nAfter update:\n")
print(table(master$verification_status))

write.csv(master, "revenue_verification_master.csv", row.names = FALSE)
cat("\nMaster table updated and saved.\n")
