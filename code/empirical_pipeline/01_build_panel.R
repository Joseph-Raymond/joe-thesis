# Chapter 3 empirical pipeline, panel construction
#
# Builds the one reproducible panel that Chapter3_outline.md Sections 2-4
# read from (chapter3_plan.md Section 7, "Phase 1"). The share-based objects
# (H_bar, H_LR, Phi, vessel_mean_share) are built at the Fishery-class level
# (e.g. "S03T"), matching how permit_link.R already computes HHI, since a
# revenue share is inherently per fishery, not per permit. The permit-COUNT
# objects (unused.count.share and friends) are built BOTH at the
# Fishery-class level and, separately, at the individual permit-serial
# level, since a vessel holding two serials of the same Fishery class
# ("permit stacking") and fishing only one looks fully used at the
# fishery-class level but has one idle permit at the serial level. Both
# versions are carried side by side in vessel_year/owner_year
# (*.permit-suffixed columns are the serial-level version) rather than
# picking one, see NOTES_prior_prototype.md for how this was decided.
#
# Reuses the cleaning steps in code/Permit_Linking/permit_link.R lines 17-21
# and code/data load module.R's catch_data_temp block, but keeps a copy of
# the permit register BEFORE the valid-vessel-ID filter, because Section 3's
# Table 3 needs to compare the wedge with and without permits that have no
# vessel attached.
#
# Saves intermediate data/ch3_panel.rdata with these objects.
#   vessel_fishery_year   long panel, one row per vessel x year x fishery
#   vessel_year           one row per vessel x year
#   vessel_share_panel    long panel, one row per vessel x year x fishery,
#                          realized revenue share only, active years only,
#                          zero-filled for a fishery the vessel fished in some
#                          other year of its own panel but not this one
#   vessel_mean_share     one row per vessel x fishery, share averaged over
#                          the vessel's active years (the H_LR weight vector)
#   vessel_summary        one row per vessel, collapsed over its active years
#   vessel_period_summary one row per vessel x calendar period (N_PERIODS,
#                          default 3), H_bar/H_LR/Phi/rev.cv computed within
#                          each period rather than over the whole panel
#   owner_fishery_year, owner_year, owner_summary   same three at the
#                                                     File.Number (owner) level,
#                                                     owner_summary carries
#                                                     prime.fishery the same
#                                                     way vessel_summary does,
#                                                     owner_year carries hhi
#                                                     the same way vessel_year
#                                                     does (used by
#                                                     01b_build_rolling_panel_owner.R's
#                                                     own self-consistency check)
#   owner_mean_share       owner-level mirror of vessel_mean_share, one row
#                          per File.Number x fishery, share averaged over the
#                          owner's active years, reused by
#                          05_table4_figure3_owner.R as the owner-level
#                          passive buy-and-hold benchmark's portfolio weights
#   owner_share_panel      owner-level mirror of vessel_share_panel, needed
#                          standalone (not just collapsed into owner_summary)
#                          by 01b_build_rolling_panel_owner.R the same way
#                          01b_build_rolling_panel.R needs vessel_share_panel
#   owner_period_summary  owner-level analogue of vessel_period_summary
#   period_bounds          the two period breakpoints actually used, computed
#                          once from the observed year range, not hardcoded
#   match_diag             data-quality diagnostics for Table 2
#   fleet_mean_revenue_owner   owner-level mirror of fleet_mean_revenue, one
#                          row per Batch.Year x fishery, mean revenue among
#                          fished owners that fishery-year, needed standalone
#                          (not just embedded as a column inside
#                          owner_fishery_year) for the same passive-benchmark
#                          join 05_table4_figure3_owner.R performs
#
# Run 00_setup.R first (or just source it, which this script does).

source("code/empirical_pipeline/00_setup.R")
deflator <- load_deflator()

# ============================================================================
# 1. Permit register (held set)
# ============================================================================

load(file.path(intermediate_dir, "permit_clean.rdata"))

# permit_clean.rdata loads an object literally called permit_clean.
# Same rename/derive steps as permit_link.R lines 17-20. CHECK the "...1"
# column still exists on the server's copy, it is a leftover row-index column
# from how permit_clean.rdata was written and may not always be present.
if ("...1" %in% names(permit_clean)) permit_clean <- permit_clean %>% select(-"...1")

lookup <- c(Vessel.ADFG.Number = "Vessel.ADFG", Batch.Year = "Year")
permit_register_raw <- permit_clean %>%
  rename(all_of(lookup)) %>%
  mutate(
    Vessel.ADFG.Number       = as.integer(Vessel.ADFG.Number),
    CFEC.Permit.Serial.Number = as.integer(substr(Permit.Number, 1, 5)),
    CFEC.Permit.Check         = substr(Permit.Number, 6, 6)
  ) %>%
  filter(Batch.Year >= MIN_YEAR, Fishery != "")

# Permit.Status diagnostic and cancelled-permit exclusion. chapter3_plan.md
# Section 1's data dictionary documents Permit.Status ("Current Owner" is
# the filter it calls for), but no script in this pipeline had ever actually
# applied it, permit_register_raw was carrying every status value straight
# through untouched. A cancelled permit is not a real held opportunity, it
# is a register row for access that no longer exists, so counting it toward
# n.held.fishery would inflate both the held side of the wedge and the
# unused share built from it, in the direction that makes the chapter's own
# headline number look bigger than it should. Column existence is checked
# rather than assumed, since it has not been directly confirmed against the
# server's copy of permit_clean.rdata from inside this script before now.
if ("Permit.Status" %in% names(permit_register_raw)) {
  cat("\n===== Permit.Status distribution, permit register rows before any status filter =====\n")
  print(permit_register_raw %>% count(Permit.Status, sort = TRUE))

  # REVISED after the first real run. The printed distribution above turned
  # out to carry exactly two values, "Current Owner" and "Former Owner", no
  # "cancelled" string anywhere, so an earlier grepl("cancel", ...) version
  # of this filter matched zero rows and was a silent no-op (confirmed,
  # Table 3's numbers were unchanged from before this filter existed). That
  # earlier version was written off a direct-inspection report of a
  # "permit cancelled" status that does not appear to exist under this exact
  # field on the server copy actually loaded here, CHECK with whoever
  # reported that whether it was a different field, a different data pull,
  # or CFEC's own public-facing terminology rather than this raw column.
  # "Former Owner" now drives the filter instead, since it is what
  # chapter3_plan.md's own data dictionary documents the design intent
  # against ("Current Owner" is the filter it calls for, chapter3_plan.md
  # Section 1), and it lines up with the within-year resale diagnostic a few
  # lines below, a permit resold mid-year plausibly carries one "Former
  # Owner" row for the seller and one "Current Owner" row for the buyer in
  # the same annual extract, which is a real prior ownership record, not a
  # permit that stopped existing, so excluding it is still the right call
  # for the SAME reason a cancelled permit would be excluded, it is not a
  # live, currently-held opportunity for whoever the row's File.Number
  # names. Matched with != "Current Owner" (not a positive match on "Former
  # Owner") so that any additional status value that might appear in a
  # fuller extract than this run saw is excluded by default rather than
  # silently kept, CHECK the printed distribution above whenever this next
  # runs against a materially different pull of permit_clean.rdata.
  permit_register_raw <- permit_register_raw %>%
    mutate(is.not.current.owner = is.na(Permit.Status) | Permit.Status != "Current Owner")

  cat("Permit register rows excluded for Permit.Status != \"Current Owner\":",
      sum(permit_register_raw$is.not.current.owner), "of", nrow(permit_register_raw),
      "(", round(100 * mean(permit_register_raw$is.not.current.owner), 2), "% )\n")

  permit_register_raw <- permit_register_raw %>% filter(!is.not.current.owner) %>% select(-is.not.current.owner)
} else {
  warning("Permit.Status column not found on permit_register_raw, cancelled permits are NOT being ",
          "excluded from the held set. chapter3_plan.md documents this column, CHECK the real column ",
          "name against the server copy of permit_clean.rdata and update Section 1 of this script.")
}

# Permit.Type distribution, reported as a diagnostic rather than folded into
# a transferable/non-transferable filter or column. Permanent and
# interim-use permits are generally transferable on the open market under
# AS 16.43.170, moratorium and vessel-moratorium permits are generally far
# more restricted (often tied to the vessel itself or to narrow hardship or
# family transfers only), but the exact rule for every Permit.Type value
# actually present in this register has not been verified against CFEC's
# own regulations or program documentation from inside this script, so
# nothing downstream should assert a transferable/non-transferable split
# from this print alone without that check.
if ("Permit.Type" %in% names(permit_register_raw)) {
  cat("\n===== Permit.Type distribution, permit register rows (transferability varies by type, not classified here) =====\n")
  print(permit_register_raw %>% count(Permit.Type, sort = TRUE))
} else {
  warning("Permit.Type column not found on permit_register_raw, no transferability breakdown printed. ",
          "CHECK the real column name against the server copy of permit_clean.rdata.")
}

# Within-year resale diagnostic. The register can carry more than one row
# for the same permit in the same year when it changes hands mid-year
# (chapter3_plan.md's data dictionary lists Permit.Sequence for exactly this
# reason, and the old exploratory permit_link.R collapses on
# max(Permit.Sequence) for the same reason, see its lines 60-64). This
# script does NOT use Permit.Sequence anywhere, on purpose rather than by
# oversight, held_vessel_fishery (Section 2b), held_owner_fishery (Section
# 7), and the permit-serial-level counts (n.held.permit, both grains)
# already collapse through distinct()/group_by()+summarise() on the vessel
# or owner dimension, so a permit resold three times within a year still
# only counts once toward n.held.fishery for whichever vessel or owner held
# it. What is NOT protected this way is any diagnostic built directly off
# nrow(permit_register_raw), the raw row count Table 2 reports and the
# missing-vessel-ID share computed from it, since those count every
# transaction record literally. This block exists to size that gap, not
# because the held-set logic itself needs Permit.Sequence.
if ("Permit.Sequence" %in% names(permit_register_raw)) {
  resale_diag <- permit_register_raw %>%
    group_by(Batch.Year, CFEC.Permit.Serial.Number) %>%
    summarise(n.rows = n(), n.distinct.owners = n_distinct(File.Number), .groups = "drop")
  cat("\n===== Within-year permit resale diagnostic (Permit.Sequence) =====\n")
  cat("Permit x year cells with more than one register row:", sum(resale_diag$n.rows > 1),
      "of", nrow(resale_diag), "(", round(100 * mean(resale_diag$n.rows > 1), 2), "% )\n")
  cat("Of those, cells where the extra rows reflect a genuine change of owner",
      "(n.distinct.owners > 1, i.e. an actual resale rather than a same-owner re-issue):",
      sum(resale_diag$n.rows > 1 & resale_diag$n.distinct.owners > 1), "\n")
  cat("Raw permit register rows:", nrow(permit_register_raw), " vs distinct (Batch.Year, CFEC.Permit.Serial.Number) permit-years:",
      nrow(resale_diag), ", the gap is what a raw-row-count diagnostic (Table 2) inherits from resale multiplicity\n")
} else {
  warning("Permit.Sequence column not found on permit_register_raw, cannot size how much of the raw ",
          "row count reflects within-year permit resale rather than one row per permit-year. ",
          "CHECK the real column name against the server copy of permit_clean.rdata.")
}

# Junk gear code exclusion. REVISED same day. The first version of this
# filter included "08" on the strength of Permit_Variance.R's precedent and
# S08P's absence from SALMON_GEAR_DIGITS above (an incomplete convenience
# lookup, not an authority) plus S08P's apparent 1.00 unused owner-share.
# Direct cross-check against Context_papers/CFEC codes/Current CFEC Fishery
# Codes.txt shows "08" is FISH WHEEL, a real, active, limited-entry salmon
# gear (S 08P, SALMON, FISH WHEEL, UPPER YUKON, Limited, 1976-2021), so that
# first version deleted 4,046 real held owner-years based on a false
# premise, S08P's apparent 100% unused share is itself the bug being
# investigated (see the register-vs-ticket Fishery presence check further
# below), not evidence the fishery is administrative. "08" has been removed
# from JUNK_GEAR_CODES (00_setup.R) accordingly. "77" (Hatchery Permit,
# special harvest area) and "99" (Experimental/Special Permit) are confirmed
# non-commercial in the same dictionary, "13" rests on Permit_Variance.R's
# comment alone (a single 1982-only Southeast dip-net experimental code per
# that file), not independently re-checked here.
permit_register_raw <- permit_register_raw %>%
  mutate(fishery.gear.digits = substr(Fishery, 2, 3))

cat("\nPermit register rows with a junk gear code (", paste(JUNK_GEAR_CODES, collapse = ", "), "):",
    sum(permit_register_raw$fishery.gear.digits %in% JUNK_GEAR_CODES), "of", nrow(permit_register_raw), "\n")
cat("Fishery codes affected (up to 15 shown):\n")
print(permit_register_raw %>% filter(fishery.gear.digits %in% JUNK_GEAR_CODES) %>% count(Fishery, sort = TRUE) %>% head(15))

permit_register_raw <- permit_register_raw %>%
  filter(!(fishery.gear.digits %in% JUNK_GEAR_CODES)) %>%
  select(-fishery.gear.digits)

# Data-gap gear exclusion, separate from the junk-gear-code filter above and
# for a different reason, see EXCLUDED_GEAR_DIGITS_DATA_GAP's definition in
# 00_setup.R for the full evidence trail (CFEC Report 25-4N citation, the
# ~73% lifetime ghost-holder rate, the S08P total ticket-side absence).
# These are real, valid, often major fisheries, the register side of this
# panel is trustworthy for them (permit counts matched CFEC's own published
# figures closely), it is specifically the FISHED side that cannot be
# trusted for them in this extract. Excluding them here means Table 3's
# wedge, and everything downstream that treats a permit as "held"
# (Table 1, Table 2's vessel-ID-missing share, the turnover/activation
# scripts), is no longer computed on a population known to be unmeasurable
# rather than silently pooling it into numbers that get reported as fact.
# This does NOT touch vessel-level H-bar/H_LR/Phi, those are built from the
# fished side only and never reference permit_register_raw at all.
permit_register_raw <- permit_register_raw %>%
  mutate(fishery.gear.digits.datagap = substr(Fishery, 2, 3))

cat("\nPermit register rows excluded for a known register-vs-ticket data gap (gear",
    paste(EXCLUDED_GEAR_DIGITS_DATA_GAP, collapse = ", "), "):",
    sum(permit_register_raw$fishery.gear.digits.datagap %in% EXCLUDED_GEAR_DIGITS_DATA_GAP),
    "of", nrow(permit_register_raw), "\n")
cat("Fishery codes affected (up to 20 shown):\n")
print(permit_register_raw %>% filter(fishery.gear.digits.datagap %in% EXCLUDED_GEAR_DIGITS_DATA_GAP) %>%
        count(Fishery, sort = TRUE) %>% head(20))

permit_register_raw <- permit_register_raw %>%
  filter(!(fishery.gear.digits.datagap %in% EXCLUDED_GEAR_DIGITS_DATA_GAP)) %>%
  select(-fishery.gear.digits.datagap)

# Non-harvest fishery code exclusion, a third and different reason again,
# see NON_HARVEST_FISHERY_CODES's definition in 00_setup.R for the full
# evidence trail. These five codes are not harvest permits at all (a vessel
# entry/moratorium slot in certain limitation programs, held separately
# from the actual harvest permit under the fishery's base code), so no fish
# ticket was ever going to exist under them and held-vs-fished is not a
# meaningful comparison here the way it is for gear 04/08/18 above. This is
# why E91QV, W22BV, and W2ABV sat at a 1.00 unused share in
# table3_unused_share_by_fishery.tex even after that fix.
cat("\nPermit register rows on a non-harvest fishery code (",
    paste(NON_HARVEST_FISHERY_CODES, collapse = ", "), "):",
    sum(permit_register_raw$Fishery %in% NON_HARVEST_FISHERY_CODES),
    "of", nrow(permit_register_raw), "\n")

permit_register_raw <- permit_register_raw %>%
  filter(!(Fishery %in% NON_HARVEST_FISHERY_CODES))

# has.vessel.id is FALSE for a permit register row with no vessel attached
# (NA, 0, or 99999) or an owner-only holding. permit_link.R drops these
# outright. Kept here because Table 3 (04_table3.R) needs both versions and
# because chapter3_plan.md Section 6 treats this as central, not a
# robustness footnote, a permit held with no boat is the cleanest example of
# holding without fishing in the whole dataset.
permit_register_raw <- permit_register_raw %>%
  mutate(has.vessel.id = !is.na(Vessel.ADFG.Number) & !(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))

cat("Permit register rows:", nrow(permit_register_raw),
    " missing/sentinel vessel ID:", sum(!permit_register_raw$has.vessel.id), "\n")

# Vessel-matched register, what permit_link.R uses. This is the basis for
# every vessel-level object below. The owner-inclusive version (all rows,
# keyed on File.Number) is built separately in Section 5 of this script.
permit_register <- permit_register_raw %>% filter(has.vessel.id)

# held_vessel_fishery is built in Section 2b below, after the trailing-year
# coverage check, not here, since that check can trim permit_register's own
# year range and held_vessel_fishery needs to reflect the trimmed version.

# ============================================================================
# 2. Fish tickets (fished set and realized revenue)
# ============================================================================

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

# Diagnostic, run before anything else touches catch_data_temp, on the
# rawest possible version of the object. Investigating why S04T (Bristol Bay
# set gillnet) and its gear-04 siblings show 91 distinct real, non-sentinel,
# non-NA vessels and 1,233 real ticket rows total across the WHOLE 31-year
# panel (decisive_check_codes diagnostic further below), when CFEC's own
# published Report 25-4N ("CFEC Salmon Set Gillnet Permits and DNR Shore
# Fishery Leases...", Context_papers/set gillnet report 25-04N.pdf) puts
# 1,500-1,950 set gillnet permit holders landing catches EVERY SINGLE YEAR
# across just five of these fisheries. Three different exploratory scripts
# in code/ (outside this pipeline) save to this exact intermediate data/
# catch_data_temp.rdata path, code/permit_cleaning.R and code/prod_reg.R
# both do a plain unfiltered bind_rows(catch_data), code/Revenue.R applies a
# group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1,
# 1) == "S")) vessel-keyed salmon-only filter but saves the RESULT to
# my_data.rds, not to this path, so it should not be the live source of
# this file, though which of the three scripts last wrote the copy
# currently sitting in intermediate data/ cannot be confirmed from code
# alone, hence checking directly here rather than assuming. If this object
# turns out to hold far fewer distinct vessels or far fewer non-salmon rows
# than a genuinely unfiltered 1991-2021 AKFIN pull should, some filter
# similar to Revenue.R's is live after all, just via a path not found by
# grep. If vessel/species diversity looks normal, the shortfall is upstream
# of every R script in this repo, most likely the original CFEC/AKFIN data
# pull itself never contained many of these shore-based, vessel-less
# landings (the AKFIN user guide's own words, "The Comprehensive FT is best
# used for catcher vessels").
cat("\n===== Raw catch_data_temp, before any cleaning in this script =====\n")
cat("Rows:", nrow(catch_data_temp), " Columns:", ncol(catch_data_temp), "\n")
cat("Distinct Vessel.ADFG.Number values:", n_distinct(catch_data_temp$Vessel.ADFG.Number), "\n")
cat("Distinct first-letter-of-CFEC.Permit.Fishery species codes, with row counts:\n")
print(catch_data_temp %>% mutate(species = substr(CFEC.Permit.Fishery, 1, 1)) %>% count(species, sort = TRUE))
cat("Column names:\n")
print(names(catch_data_temp))

# Same fixes as data load module.R / permit_link.R.
catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339

# Diagnostic, run before the sentinel-vessel-ID filter below discards these
# rows for good. held_owner_fishery (Section 5) deliberately keeps permits
# with no vessel of record, so a permit with no vessel attached still counts
# as held. This checks whether the fished side is symmetric, a fishery
# landed mostly by shore-based deliveries (e.g. set gillnet) may routinely
# log Vessel.ADFG.Number as 0, in which case that revenue disappears here
# and every owner holding that permit reads as never-fished, indistinguishable
# from someone who actually left the permit idle. 04b_table_unused_by_fishery.R
# surfaced several gear-04 (set gillnet) fisheries, including Bristol Bay
# (S04T), at exactly 1.00 unused share, which this diagnostic is meant to
# either confirm or rule out as the cause.
sentinel_diag <- catch_data_temp %>%
  filter(Vessel.ADFG.Number %in% BAD_VESSEL_IDS, Batch.Year >= MIN_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
  filter(Fishery != "", !is.na(CFEC.Value..Detail.), CFEC.Value..Detail. > 0)

cat("\n===== Ticket rows with positive recorded revenue about to be dropped for a sentinel Vessel.ADFG.Number (0 or 99999) =====\n")
cat("Rows:", nrow(sentinel_diag), "\n")
cat("Top 15 Fishery codes by dropped revenue-positive row count:\n")
print(sentinel_diag %>% count(Fishery, sort = TRUE) %>% head(15))
cat("Top 15 Fishery codes by total dropped revenue (nominal, undeflated):\n")
print(sentinel_diag %>% group_by(Fishery) %>% summarise(revenue = sum(CFEC.Value..Detail.), .groups = "drop") %>%
        arrange(desc(revenue)) %>% head(15))

# Two competing explanations for the sentinel rows above, not yet
# distinguished. chapter3_plan.md Section 1's own data dictionary lists
# several other vessel-identifier-shaped columns on the raw ticket extract
# beyond Vessel.ADFG.Number, including AKR.Vessel.ADFG.Number,
# CFEC.Vessel.ADFG.Number, and CFEC.Permit.Vessel.ADFG.Number
# (Tender.Vessel.ADFG.Number is deliberately excluded below, that is the
# transport/tender boat, not the catching vessel, so not a substitute). The
# AKFIN Comprehensive Fish Ticket user guide (context_data/data info/
# UserGuide_Comprehensive_FT3.0.pdf) independently documents the same
# pattern from the source-system side, a raw ADFG_H_ADFG_NUMBER field plus
# two separate correction passes on top of it (CFEC_CORRECTED_ADFG and
# AKFIN's own N_ADFG), and also documents a first-class flag for exactly
# the question the diagnostic above answers indirectly, CFEC_ADFG_STATUS,
# 'V' a vessel was used, 'N' a vessel was not used, with the guide's own
# gloss "some fisheries do not require a vessel." None of these are read
# anywhere in this pipeline and none are confirmed present in this extract
# under the AKFIN name, the R column names here were inferred from code
# alone (chapter3_plan.md's own caveat), so this prints whatever the real
# names turn out to be rather than guessing a literal string, the
# Permit.Status "cancel" lesson from earlier in this file.
vessel_like_cols <- names(catch_data_temp)[grepl("vessel|adfg|status", names(catch_data_temp), ignore.case = TRUE)]
cat("\n===== Columns on catch_data_temp matching 'vessel', 'adfg', or 'status' =====\n")
print(vessel_like_cols)

status_like_cols <- vessel_like_cols[grepl("status", vessel_like_cols, ignore.case = TRUE)]
if (length(status_like_cols) > 0) {
  for (col in status_like_cols) {
    cat("\n----- Value counts for", col, ", all rows -----\n")
    print(catch_data_temp %>% count(.data[[col]], sort = TRUE))
    cat("\n----- Value counts for", col, ", restricted to rows with a sentinel Vessel.ADFG.Number (0 or 99999) -----\n")
    print(catch_data_temp %>% filter(Vessel.ADFG.Number %in% BAD_VESSEL_IDS) %>% count(.data[[col]], sort = TRUE))
  }
} else {
  cat("\nNo column name matched 'status' among vessel/adfg-like columns, CFEC_ADFG_STATUS (or its ",
      "equivalent) may not have survived into this extract or uses a different naming convention, ",
      "CHECK against real headers.\n")
}

alt_vessel_cols <- setdiff(
  vessel_like_cols[grepl("vessel", vessel_like_cols, ignore.case = TRUE) & !grepl("tender", vessel_like_cols, ignore.case = TRUE)],
  "Vessel.ADFG.Number"
)
if (length(alt_vessel_cols) > 0) {
  for (col in alt_vessel_cols) {
    recovered <- catch_data_temp %>%
      filter(Vessel.ADFG.Number %in% BAD_VESSEL_IDS, !is.na(.data[[col]]), !(.data[[col]] %in% BAD_VESSEL_IDS))
    cat("\nAmong rows with a sentinel Vessel.ADFG.Number, rows with a non-missing, non-sentinel value in",
        col, ":", nrow(recovered), "of", sum(catch_data_temp$Vessel.ADFG.Number %in% BAD_VESSEL_IDS, na.rm = TRUE), "\n")
  }
} else {
  cat("\nNo alternate vessel-ID-shaped column found besides Vessel.ADFG.Number and the tender field.\n")
}

# Diagnostic for the leading hypothesis behind S04T/S04X's near-100% owner-
# level unused share (04b_table_unused_by_fishery.R), that hypothesis being
# a hole in the filter directly below this block, not the "0 or 99999
# sentinel" case, an NA vessel ID. `NA %in% BAD_VESSEL_IDS` evaluates to
# FALSE in R, so `!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS)` is TRUE for an
# NA vessel ID and the filter below silently KEEPS those rows, unlike the
# register side (Section 1's has.vessel.id), which does test !is.na()
# explicitly. Those surviving NA rows then all fall into one NA group under
# group_by(Vessel.ADFG.Number, Batch.Year, Fishery) below, so an entire
# fishery-year of vessel-less landings collapses to a single row credited to
# whichever File.Number happens to be first() in that group, at most one
# owner per fishery-year can ever read as fished. Bristol Bay set gillnet,
# Kotzebue gillnet, and Upper Yukon fish wheel are all beach- or
# skiff-based fisheries where a blank ADFG number is plausible. Run before
# the sentinel filter below so the NA rows are still present to count.
na_vessel_diag <- catch_data_temp %>%
  filter(is.na(Vessel.ADFG.Number), Batch.Year >= MIN_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
  filter(Fishery != "")

cat("\n===== Ticket rows with an NA Vessel.ADFG.Number (kept by the sentinel filter below as written) =====\n")
cat("Rows:", nrow(na_vessel_diag), "of", nrow(catch_data_temp), "\n")
cat("Top 20 Fishery codes by NA-vessel row count:\n")
print(na_vessel_diag %>% count(Fishery, sort = TRUE) %>% head(20))
cat("Top 20 Fishery codes by NA-vessel total revenue (nominal, undeflated):\n")
print(na_vessel_diag %>% group_by(Fishery) %>% summarise(revenue = sum(CFEC.Value..Detail., na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(revenue)) %>% head(20))

decisive_check_codes <- c("S04T", "S04X", "S04P", "S08P", "S03T")
cat("\nPer-fishery breakdown, flagged codes, all still counting NA as a vessel value here:\n")
for (code in decisive_check_codes) {
  code_rows <- catch_data_temp %>%
    filter(Batch.Year >= MIN_YEAR) %>%
    mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
    filter(Fishery == code)
  n.na <- sum(is.na(code_rows$Vessel.ADFG.Number))
  n.sentinel <- sum(code_rows$Vessel.ADFG.Number %in% BAD_VESSEL_IDS, na.rm = TRUE)
  n.real <- nrow(code_rows) - n.na - n.sentinel
  n.distinct.real.vessels <- n_distinct(code_rows$Vessel.ADFG.Number[!is.na(code_rows$Vessel.ADFG.Number) & !(code_rows$Vessel.ADFG.Number %in% BAD_VESSEL_IDS)])
  cat(code, ": total rows =", nrow(code_rows), ", NA vessel =", n.na, ", sentinel vessel =", n.sentinel,
      ", real non-sentinel rows =", n.real, ", distinct real vessels =", n.distinct.real.vessels, "\n")
}

catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)

# Captured before the zero-fill below overwrites every NA, otherwise this
# diagnostic would trivially read 0 no matter how much revenue was missing.
share_revenue_zero_filled <- mean(is.na(catch_data_temp[["CFEC.Value..Detail."]]))

# Whether the rows about to be zero-filled look like real landings with an
# unrecorded price (positive landed weight), rather than genuinely empty
# rows. Answers a specific question about whether zero-filling is neutral or
# whether it manufactures false zeros, see the match_diag block below for
# how this reads. CHECK Pounds..Detail. is the right weight field,
# chapter3_plan.md Section 1 also lists Whole.Pounds..Detail. and
# CFEC.Whole.Pounds..Detail. as alternatives worth checking if this one is
# not present or does not behave as expected.
share_zero_fill_has_positive_pounds <- catch_data_temp %>%
  filter(is.na(CFEC.Value..Detail.)) %>%
  summarise(share = mean(Pounds..Detail. > 0, na.rm = TRUE)) %>%
  pull(share)

catch_data_temp[["CFEC.Value..Detail."]][is.na(catch_data_temp[["CFEC.Value..Detail."]])] <- 0

# Diagnostic for S04P and S08P specifically, whose Fishery code never
# appears anywhere in the ticket-side data at all (register vs ticket-side
# presence check further below), unlike S04T/S04X which do appear but at
# near-zero volume. chapter3_plan.md's own data dictionary (Section 1) notes
# CFEC.Permit.Fishery "is blank when ticket permit information could not be
# matched to the CFEC permit file", and the filter below drops any row where
# it is blank or NA, which means a permit CFEC failed to match on the ticket
# gets silently deleted from the fished side while the register side (built
# from a different, complete source) still counts it as held. The plan doc
# also lists a second, separate ticket column, Permit.Fishery (no space,
# chapter3_plan.md Section 1), that this pipeline has never used. This
# checks whether that second column recovers what the first one drops.
cat("\n===== Blank/NA CFEC.Permit.Fishery rows, about to be dropped by the filter below =====\n")
blank_permit_fishery <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, is.na(CFEC.Permit.Fishery) | strip_fishery_space(CFEC.Permit.Fishery) == "")
cat("Rows:", nrow(blank_permit_fishery), "of", sum(catch_data_temp$Batch.Year >= MIN_YEAR), "\n")

if ("Permit.Fishery" %in% names(catch_data_temp)) {
  recovered_permit_fishery <- blank_permit_fishery %>%
    filter(!is.na(Permit.Fishery), strip_fishery_space(Permit.Fishery) != "")
  cat("Of those, rows where Permit.Fishery (the second, unused ticket-side fishery column) is non-blank:",
      nrow(recovered_permit_fishery), "\n")
  cat("Top 20 Permit.Fishery codes among the recovered rows:\n")
  print(recovered_permit_fishery %>% mutate(Fishery = strip_fishery_space(Permit.Fishery)) %>%
          count(Fishery, sort = TRUE) %>% head(20))
  cat("Row counts for the flagged codes, CFEC.Permit.Fishery vs Permit.Fishery, year >= MIN_YEAR:\n")
  for (code in c("S04T", "S04X", "S04P", "S08P")) {
    n.cfec   <- sum(strip_fishery_space(catch_data_temp$CFEC.Permit.Fishery) == code, na.rm = TRUE)
    n.permit <- sum(strip_fishery_space(catch_data_temp$Permit.Fishery) == code, na.rm = TRUE)
    cat(code, ": CFEC.Permit.Fishery =", n.cfec, ", Permit.Fishery =", n.permit, "\n")
  }
} else {
  cat("Permit.Fishery column not found on catch_data_temp, cannot check whether it recovers these rows,",
      "CHECK the real column name against the server copy of catch_data_temp.rdata.\n")
}

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
  filter(Fishery != "")

# Residency diagnostic, reported here rather than in Section 1 because
# chapter3_plan.md's own data dictionary lists CFEC.Permit.Holder.Residency
# as a FISH TICKET column, not a permit register column (Section 1's own
# register dictionary lists Zip.Code/city/state but no residency field as
# such). So residency is only observable for a permit-owner-year that
# generated at least one ticket, not for the held side directly, unlike
# Permit.Status/Permit.Type this is a diagnostic on catch_data_temp, not on
# permit_register_raw. CHECK the exact column name against the server copy,
# not yet confirmed from inside this script. Also carries a known confound
# from chapter3_plan.md's own R6 discussion, worth keeping in mind before
# reading anything behavioral into a residency split, unmatched permit
# serials already skew out-of-state, so non-local holders also have worse
# permit-match quality and therefore more mismeasured portfolios, the
# residency split is not clean behavioral heterogeneity on its own.
if ("CFEC.Permit.Holder.Residency" %in% names(catch_data_temp)) {
  cat("\n===== CFEC.Permit.Holder.Residency distribution, fish ticket rows =====\n")
  print(catch_data_temp %>% count(CFEC.Permit.Holder.Residency, sort = TRUE))
} else {
  warning("CFEC.Permit.Holder.Residency column not found on catch_data_temp, no residency breakdown ",
          "printed. CHECK the real column name against the server copy of catch_data_temp.rdata.")
}

fished_vessel_fishery_year <- catch_data_temp %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  summarise(
    revenue      = sum(CFEC.Value..Detail., na.rm = TRUE),
    # File.Number here must be the PERMIT HOLDER's filing number
    # (CFEC.Permit.Holder.Filing.Number), not the vessel owner's
    # (CFEC.Vessel.Owner.Filing.Number). The owner-level panel below joins
    # this to held_owner_fishery, which is keyed on the permit register's own
    # File.Number, i.e. the permit holder. The permit holder and the vessel
    # owner are not always the same person, so joining on the vessel-owner ID
    # would silently misattribute fished revenue whenever they differ. CHECK
    # this field name against real headers, chapter3_plan.md Section 1 lists
    # it but it has not been confirmed on the server.
    File.Number  = first(CFEC.Permit.Holder.Filing.Number),
    .groups = "drop"
  )

# ============================================================================
# 2b. Trailing-year coverage check (held vs fished)
# ============================================================================
#
# permit_register (Section 1, held permits) and catch_data_temp (fished fish
# tickets, just above) come from two different source pulls that are not
# guaranteed to share the same final year. chapter3_plan.md Section 1 notes
# the permit and vessel registers run through 2022 while the fish-ticket
# pull's true end year is uncertain and may stop earlier, or its last file
# may be a mid-revision partial extract rather than a finished one. A held
# permit in a year the ticket data barely covers reads as held-but-never-
# fished by construction, Section 4's full_join below defaults fished to
# FALSE whenever no ticket row exists for that vessel-fishery-year, so a
# coverage gap at the end of catch_data_temp would mechanically push
# unused.count.share and unused.value.share toward 1 fleet-wide in that
# year, a data-pull artifact rather than operators actually idling their
# whole fleet at once. This is the leading suspect for an end-of-series jump
# in Figure 1, checked here directly rather than left to be diagnosed off
# the plot alone, and trimmed at the source so every downstream table and
# figure sees the same corrected year range, not just Figure 1's plot.
#
# Walking backward from the max observed Batch.Year, a year is flagged as
# coverage-incomplete if its count of distinct vessels appearing anywhere in
# catch_data_temp falls below half the mean of the three years before it.
# Only a run CONTIGUOUS with the final year is dropped, a genuine mid-panel
# closure (a fishery shutting down, one bad season fleet-wide) should not
# trigger this and is left alone, only a collapse that persists through the
# last observed year points at a data-pull artifact rather than a real
# fishing-effort decline.
#
# CHECK the printed coverage table once this runs on the server against real
# data. The half-of-trailing-baseline threshold is a judgment call, not a
# fact, tighten or loosen it (or just hardcode MAX_YEAR directly below) if
# it drops a year that turns out to be genuine or keeps one that is not.

year_fished_counts <- catch_data_temp %>%
  distinct(Vessel.ADFG.Number, Batch.Year) %>%
  count(Batch.Year, name = "n.vessels.with.tickets")

year_held_counts <- permit_register %>%
  distinct(Vessel.ADFG.Number, Batch.Year) %>%
  count(Batch.Year, name = "n.vessels.held")

trailing_mean <- function(x, k = 3) {
  vapply(seq_along(x), function(i) {
    lo <- max(1, i - k)
    hi <- i - 1
    if (hi < lo) return(NA_real_)
    mean(x[lo:hi])
  }, numeric(1))
}

year_coverage <- year_held_counts %>%
  full_join(year_fished_counts, by = "Batch.Year") %>%
  arrange(Batch.Year) %>%
  mutate(
    n.vessels.with.tickets = replace_na(n.vessels.with.tickets, 0),
    baseline.n.tickets     = trailing_mean(n.vessels.with.tickets),
    coverage.collapsed     = !is.na(baseline.n.tickets) & n.vessels.with.tickets < 0.5 * baseline.n.tickets
  )

cat("Held vs ticketed vessel counts, last 5 years of coverage\n")
print(tail(select(year_coverage, Batch.Year, n.vessels.held, n.vessels.with.tickets), 5))

max_year_observed <- max(year_coverage$Batch.Year)
collapsed_years <- year_coverage$Batch.Year[year_coverage$coverage.collapsed]
drop_years <- c()
y <- max_year_observed
while (y %in% collapsed_years) {
  drop_years <- c(drop_years, y)
  y <- y - 1
}

if (length(drop_years) > 0) {
  cat("Dropping trailing year(s) with collapsed ticket coverage relative to their own 3-year baseline, ",
      "these read as held-but-unfished fleet-wide from a data-pull gap rather than real behavior, ",
      paste(sort(drop_years), collapse = ", "), "\n")
} else {
  cat("No trailing coverage collapse detected, held and ticketed year ranges look consistent through ",
      max_year_observed, "\n")
}

MAX_YEAR <- if (length(drop_years) > 0) min(drop_years) - 1 else max_year_observed

permit_register_raw <- permit_register_raw %>% filter(Batch.Year <= MAX_YEAR)
permit_register <- permit_register %>% filter(Batch.Year <= MAX_YEAR)
catch_data_temp <- catch_data_temp %>% filter(Batch.Year <= MAX_YEAR)
fished_vessel_fishery_year <- fished_vessel_fishery_year %>% filter(Batch.Year <= MAX_YEAR)

held_vessel_fishery <- permit_register %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  mutate(held = TRUE)

# ============================================================================
# 3. Match-rate diagnostics (feeds Table 2, 02_table1_table2.R)
# ============================================================================

# Ticket-to-register match rate, the share of fish ticket rows whose
# (Vessel.ADFG.Number, Batch.Year, CFEC.Permit.Serial.Number) actually joins
# to a real row in permit_register. NOT the same as CFEC.Permit.Serial.Number
# being non-missing on the ticket, on real data that field turned out to be
# populated on every single row, so mean(!is.na(...)) is guaranteed to read
# exactly 1.0 regardless of true match quality, it was measuring field
# completeness, not register matching. permit_link.R's comment about
# unmatched serials skewing out-of-state is about this join failing, not
# about the ticket field being blank. semi_join is a hash join, this is
# efficient even at ~16M ticket rows.
ticket_serial_match_rate <- catch_data_temp %>%
  select(Vessel.ADFG.Number, Batch.Year, CFEC.Permit.Serial.Number) %>%
  semi_join(permit_register, by = c("Vessel.ADFG.Number", "Batch.Year", "CFEC.Permit.Serial.Number")) %>%
  nrow() / nrow(catch_data_temp)

match_diag <- tibble(
  metric = c(
    "ticket_serial_match_rate",
    "share_permits_missing_vessel_id",
    "share_revenue_zero_filled",
    "share_zero_fill_has_positive_pounds"
  ),
  value = c(
    ticket_serial_match_rate,
    mean(!permit_register_raw$has.vessel.id),
    share_revenue_zero_filled,
    share_zero_fill_has_positive_pounds
  )
)
print(match_diag)

# ============================================================================
# 4. Vessel x fishery x year panel (held, fished, revenue)
# ============================================================================

vessel_fishery_year <- held_vessel_fishery %>%
  full_join(fished_vessel_fishery_year, by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  mutate(
    held   = replace_na(held, FALSE),
    fished = !is.na(revenue) & revenue > 0,
    revenue = replace_na(revenue, 0)
  )

# fished.unmatched: fished a fishery with no corresponding held permit that
# year. Should be rare or zero if the permit-ticket join is clean, a sizeable
# nonzero share here means the register-to-ticket match is lossy, not that
# fishers are landing without a permit. Worth checking before trusting the
# wedge numbers downstream.
n_fished_unmatched <- vessel_fishery_year %>% filter(fished & !held) %>% nrow()
cat("Vessel-fishery-years fished without a matched held permit:", n_fished_unmatched,
    "(", round(100 * n_fished_unmatched / sum(vessel_fishery_year$fished), 2), "% of fished rows)\n")

vessel_fishery_year <- deflate(vessel_fishery_year, "revenue", deflator)

# A second, cheaper way the same end-of-series jump could show up, checked
# defensively since it costs almost nothing here. If cpi_deflator.csv exists
# but has no row for the panel's final year(s), deflate()'s join leaves
# revenue as NA for every row in that year, which na.rm = TRUE then silently
# turns into 0 wherever it gets summed downstream, again reading as a
# fleet-wide collapse that is really just a missing CPI row.
if (!is.null(deflator)) {
  years_missing_deflator <- setdiff(unique(vessel_fishery_year$Batch.Year), deflator$Year)
  if (length(years_missing_deflator) > 0) {
    warning(
      "cpi_deflator.csv has no row for Batch.Year ",
      paste(sort(years_missing_deflator), collapse = ", "),
      ", revenue silently drops to NA/0 for that year via na.rm downstream, ",
      "which would look like exactly the kind of end-of-series jump Figure 1 is showing"
    )
  }
}

# Fleet-mean revenue per active (fishing) vessel, by fishery-year. Used as
# the stand-in for what an idle vessel forgoes (Figure 1's value-share line)
# and again in 05_table4_figure3.R for the passive benchmark. No leave-one-out
# adjustment is needed for the forgone-value use, a vessel that did not fish
# fishery j contributes nothing to fishery j's mean that year regardless.
fleet_mean_revenue <- vessel_fishery_year %>%
  filter(fished) %>%
  group_by(Batch.Year, Fishery) %>%
  summarise(fleet_mean_revenue = mean(revenue, na.rm = TRUE), n_active = n(), .groups = "drop")

# fishery.year.active, TRUE if ANY vessel anywhere landed anything under
# this Fishery in this Batch.Year, independent of who held what. Every row
# in fleet_mean_revenue above already required at least one fished vessel to
# exist, so distinct(Batch.Year, Fishery) from it is exactly the set of
# fishery-years with real fleet-wide activity. Built once here and reused
# unchanged for owner_fishery_year in Section 7, so both grains share one
# definition of "this permit was a real, fishable option that year" rather
# than each computing its own. Added because a permit for a fishery nobody
# in the fleet ever lands under (D91H, Cook Inlet Dungeness crab, is the
# case that prompted this, 1,756 register rows, 100 owners, zero ticket-side
# rows in any year) is not a genuine backup option no matter who nominally
# holds it, counting it the same as a fishable-but-idle permit overstates
# ex-ante portfolio breadth and the held-vs-fished wedge alike. See
# 04_table3.R's "fishable fishery-years only" rows.
fishery_year_active_lookup <- fleet_mean_revenue %>%
  distinct(Batch.Year, Fishery) %>%
  mutate(fishery.year.active = TRUE)

vessel_fishery_year <- vessel_fishery_year %>%
  left_join(fleet_mean_revenue, by = c("Batch.Year", "Fishery")) %>%
  left_join(fishery_year_active_lookup, by = c("Batch.Year", "Fishery")) %>%
  mutate(fishery.year.active = replace_na(fishery.year.active, FALSE))

# ============================================================================
# 4b. Vessel x permit-serial x year panel (permit-stacking-aware alternative)
# ============================================================================
#
# Everything above tracks held/fished at the Fishery-class level (e.g. all of
# a vessel's "S03T" permits collapse into one held/fished fact). That is the
# only sensible unit for anything share-based (H_bar, H_LR, Phi), but it can
# understate the unused-permit count for a vessel that "stacks" permits,
# holds two serials of the same Fishery class and fishes only one. This
# section rebuilds the permit-COUNT objects (not the share objects) at the
# individual permit-serial level instead, so Figure 1 and Table 3 can show
# both versions side by side rather than picking one. See
# NOTES_prior_prototype.md for why this was left open rather than decided.

# How common stacking actually is, checked directly rather than assumed.
# If this comes back near zero, the two versions below will barely differ
# and the simpler Fishery-class view is fine to lead with in the writeup.
stacking_check <- permit_register %>%
  count(Vessel.ADFG.Number, Batch.Year, Fishery, name = "n.serials") %>%
  filter(n.serials > 1)
cat("Vessel-fishery-years with more than one held permit serial (stacking):",
    nrow(stacking_check), "\n")

held_vessel_permit <- permit_register %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery, CFEC.Permit.Serial.Number) %>%
  mutate(held = TRUE)

# Landings with no matched permit serial (NA) cannot be attributed to a
# specific held permit and are dropped here, unlike the Fishery-class
# version above, which never needed the serial number at all since it is
# built straight off CFEC.Permit.Fishery. That gap is already tracked by
# ticket_serial_match_rate in match_diag, not double-counted as new missingness.
# File.Number here is the permit holder's filing number, same reasoning as
# fished_vessel_fishery_year above, kept so Table 3's owner-level comparison
# (Section 7) can reuse this at the permit-serial level too, not just Figure
# 1's vessel-level comparison.
fished_vessel_permit_year <- catch_data_temp %>%
  filter(!is.na(CFEC.Permit.Serial.Number)) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery, CFEC.Permit.Serial.Number) %>%
  summarise(
    revenue = sum(CFEC.Value..Detail., na.rm = TRUE),
    File.Number = first(CFEC.Permit.Holder.Filing.Number),
    .groups = "drop"
  )

vessel_permit_year <- held_vessel_permit %>%
  full_join(fished_vessel_permit_year,
            by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery", "CFEC.Permit.Serial.Number")) %>%
  mutate(
    held   = replace_na(held, FALSE),
    fished = !is.na(revenue) & revenue > 0
  )

vessel_year_permit_level <- vessel_permit_year %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  summarise(
    n.held.permit     = sum(held),
    n.fished.permit   = sum(held & fished),
    n.unfished.permit = sum(held & !fished),
    .groups = "drop"
  ) %>%
  mutate(
    unused.count.share.permit = if_else(n.held.permit > 0, n.unfished.permit / n.held.permit, NA_real_)
  )

# ============================================================================
# 5. Vessel-year summary (Figure 1, Figure 2, Table 3 inputs)
# ============================================================================

vessel_year <- vessel_fishery_year %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  summarise(
    n.held.fishery     = sum(held),
    n.fished.fishery   = sum(held & fished),
    n.unfished.fishery = sum(held & !fished),
    fished.value       = sum(revenue[held & fished], na.rm = TRUE),
    # forgone.value falls back to 0 (not NA) for a held-not-fished fishery
    # that also had zero fleet-wide participation that year, an empty
    # fishery contributes no forgone option value by construction.
    forgone.value      = sum(replace_na(fleet_mean_revenue[held & !fished], 0)),
    vessel.year.rev    = sum(revenue, na.rm = TRUE),
    hhi                = sum((revenue[fished] / sum(revenue[fished]))^2, na.rm = TRUE),
    # "Active" variants, held restricted to fishery-years with real fleet-
    # wide activity from anyone (fishery.year.active, Section 4). See
    # 04_table3.R's "fishable fishery-years only" rows for why, an unfished
    # permit in a fishery nobody landed anything under that year is not a
    # real held option.
    n.held.fishery.active     = sum(held & fishery.year.active),
    n.unfished.fishery.active = sum(held & fishery.year.active & !fished),
    forgone.value.active      = sum(replace_na(fleet_mean_revenue[held & fishery.year.active & !fished], 0)),
    .groups = "drop"
  ) %>%
  mutate(
    unused.count.share = if_else(n.held.fishery > 0, n.unfished.fishery / n.held.fishery, NA_real_),
    unused.value.share = if_else((forgone.value + fished.value) > 0,
                                  forgone.value / (forgone.value + fished.value), NA_real_),
    unused.count.share.active = if_else(n.held.fishery.active > 0,
                                         n.unfished.fishery.active / n.held.fishery.active, NA_real_),
    unused.value.share.active = if_else((forgone.value.active + fished.value) > 0,
                                         forgone.value.active / (forgone.value.active + fished.value), NA_real_)
  ) %>%
  # Adds the permit-serial-level count columns from Section 4b alongside the
  # Fishery-class-level ones above, unused.count.share (fishery-class) versus
  # unused.count.share.permit (serial-level) are the two versions to compare.
  left_join(vessel_year_permit_level, by = c("Vessel.ADFG.Number", "Batch.Year"))

cat("vessel_year rows:", nrow(vessel_year), "\n")
cat("Mean unused.count.share (fishery-class):", round(mean(vessel_year$unused.count.share, na.rm = TRUE), 4),
    " vs (permit-serial):", round(mean(vessel_year$unused.count.share.permit, na.rm = TRUE), 4), "\n")

# ============================================================================
# 6. Vessel summary (Table 4, Figure 3 inputs), Hbar/H_LR/Phi/CV
# ============================================================================
#
# H_bar_i = mean_t sum_j s_ijt^2
# H_LR_i  = sum_j (mean_t s_ijt)^2
# Phi_i   = H_bar_i - H_LR_i = sum_j Var_t(s_ijt)
# matching chapter3_plan.md Section R2 / Chapter3_outline.md Section 4 exactly.
# The mean and variance are taken over the vessel's own active years only
# (vessel.year.rev > 0), and shares are zero-filled for years the vessel was
# active but did not fish a fishery it fished in some other year of its own
# panel, so a fishery's long-run share reflects true within-vessel
# instability rather than just entry/exit from the panel.

active_vessel_years <- vessel_year %>% filter(vessel.year.rev > 0) %>%
  select(Vessel.ADFG.Number, Batch.Year, vessel.year.rev)

# Raw (not yet zero-filled) realized shares, active vessel-years only. Kept
# as its own object because the period-specific decomposition below
# zero-fills against this same base but grouped by (vessel, period) instead
# of (vessel) alone, reusing the share computation without reusing the
# whole-panel zero-fill.
vessel_share_raw <- vessel_fishery_year %>%
  filter(fished) %>%
  inner_join(active_vessel_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  mutate(share = revenue / vessel.year.rev) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, share)

# group_by(Vessel.ADFG.Number) before complete() matters, it fills each
# vessel's own ever-fished fishery set across its own years, not the union of
# every fishery any vessel in the fleet ever fished.
vessel_share_panel <- vessel_share_raw %>%
  group_by(Vessel.ADFG.Number) %>%
  complete(Fishery, Batch.Year, fill = list(share = 0)) %>%
  ungroup() %>%
  # complete() above also fills Batch.Year x Fishery combos for years the
  # vessel was not active at all (not in active_vessel_years). Drop those,
  # an inactive year is an entry/exit gap, not a zero-share portfolio choice.
  semi_join(active_vessel_years, by = c("Vessel.ADFG.Number", "Batch.Year"))

# Per vessel-fishery long-run mean share, s_bar_ij. This is the weight vector
# that defines H_LR_i = sum_j s_bar_ij^2, and it is saved on its own (not
# just folded into vessel_summary) because 05_table4_figure3.R reuses it as
# the passive buy-and-hold portfolio weights for Figure 3.
vessel_mean_share <- vessel_share_panel %>%
  group_by(Vessel.ADFG.Number, Fishery) %>%
  summarise(mean.share.fishery = mean(share), .groups = "drop")

vessel_summary <- vessel_share_panel %>%
  left_join(vessel_mean_share, by = c("Vessel.ADFG.Number", "Fishery")) %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(
    n.years  = n_distinct(Batch.Year),
    H_bar    = mean(tapply(share, Batch.Year, function(s) sum(s^2))),
    # H_LR = sum_j (mean_t s_ijt)^2 is a sum over FISHERIES, not over
    # distinct VALUES, sum(unique(mean.share.fishery)^2) silently collapsed
    # any two fisheries that happened to share the exact same long-run mean
    # share into one term, understating H_LR (and inflating Phi = H_bar -
    # H_LR) for exactly the vessels this chapter cares most about, a vessel
    # that alternates between two fisheries in an even split gets identical
    # means by construction. mean.share.fishery is already constant within
    # a Fishery here (joined in from vessel_mean_share above), so
    # [!duplicated(Fishery)] picks exactly one row per fishery regardless
    # of whether the VALUES happen to tie.
    H_LR     = sum(mean.share.fishery[!duplicated(Fishery)]^2),
    .groups = "drop"
  ) %>%
  mutate(Phi = H_bar - H_LR) %>%
  left_join(
    active_vessel_years %>%
      group_by(Vessel.ADFG.Number) %>%
      summarise(rev.cv = sd(vessel.year.rev) / mean(vessel.year.rev), .groups = "drop"),
    by = "Vessel.ADFG.Number"
  )

# prime.fishery = the fishery with the most total realized revenue across the
# vessel's whole panel, matching permit_link.R's prime.fishery construction
# (used there per year, here once per vessel since vessel_summary is a
# cross-section). Needed as the fixed effect in Table 4. Ranked on summed
# revenue directly (not shares, which are already normalized per year and so
# cannot be summed meaningfully across years for a magnitude ranking).
prime_fishery <- vessel_fishery_year %>%
  filter(fished) %>%
  group_by(Vessel.ADFG.Number, Fishery) %>%
  summarise(total.rev = sum(revenue), .groups = "drop") %>%
  group_by(Vessel.ADFG.Number) %>%
  slice_max(total.rev, n = 1, with_ties = FALSE) %>%
  select(Vessel.ADFG.Number, prime.fishery = Fishery)

vessel_summary <- vessel_summary %>%
  left_join(prime_fishery, by = "Vessel.ADFG.Number") %>%
  mutate(meets.min.years = n.years >= MIN_ACTIVE_YEARS)

cat("vessel_summary rows:", nrow(vessel_summary),
    " meeting MIN_ACTIVE_YEARS =", MIN_ACTIVE_YEARS, ":", sum(vessel_summary$meets.min.years), "\n")

# ============================================================================
# 6b. Period-specific decomposition, H_bar/H_LR/Phi within three calendar
#     periods rather than over each vessel's whole panel
# ============================================================================
#
# Same H_bar/H_LR/Phi definitions as Section 6, but computed separately
# within each of N_PERIODS (3) roughly-equal calendar periods, rather than
# over a vessel's entire active history. The two period breakpoints are
# computed once from the observed year range in the data (not hardcoded),
# so the same two thresholds bucket every vessel, but they stay correct if
# the panel's coverage changes.
#
# Zero-filling happens WITHIN each (vessel, period) group, not across a
# vessel's whole panel, a fishery the vessel fished in Period 1 but not
# Period 2 gets a zero share in Period 2's calculation, not a share pulled
# in from Period 1. That is what makes H_LR/Phi here answer "how
# concentrated/unstable was this vessel within this specific era," rather
# than the whole-panel version's "how concentrated/unstable was this vessel
# ever."

year_min <- min(vessel_year$Batch.Year)
year_max <- max(vessel_year$Batch.Year)
n_years_total <- year_max - year_min + 1

period_break_1 <- year_min + floor(n_years_total / N_PERIODS) - 1
period_break_2 <- year_min + floor(2 * n_years_total / N_PERIODS) - 1

period_bounds <- tibble(
  period = c("Period 1", "Period 2", "Period 3"),
  start  = c(year_min, period_break_1 + 1, period_break_2 + 1),
  end    = c(period_break_1, period_break_2, year_max)
)
cat("Period boundaries (fixed for this run, computed from the observed year range):\n")
print(period_bounds)

period_of <- function(batch_year) {
  case_when(
    batch_year <= period_break_1 ~ "Period 1",
    batch_year <= period_break_2 ~ "Period 2",
    TRUE ~ "Period 3"
  )
}

vessel_share_raw_period <- vessel_share_raw %>% mutate(period = period_of(Batch.Year))

# complete() cannot introduce a (vessel, period, Batch.Year) combination that
# was not already present in vessel_share_raw_period, the same reasoning as
# the whole-panel version above (Section 6), it only cross-joins Fishery and
# Batch.Year values that already exist within each (vessel, period) group.
# No extra semi_join safety net needed here for the same reason it turned
# out to be a no-op there.
vessel_period_share_panel <- vessel_share_raw_period %>%
  group_by(Vessel.ADFG.Number, period) %>%
  complete(Fishery, Batch.Year, fill = list(share = 0)) %>%
  ungroup()

vessel_period_summary <- vessel_period_share_panel %>%
  group_by(Vessel.ADFG.Number, period, Fishery) %>%
  mutate(mean.share.fishery = mean(share)) %>%
  group_by(Vessel.ADFG.Number, period) %>%
  summarise(
    n.years.period = n_distinct(Batch.Year),
    H_bar          = mean(tapply(share, Batch.Year, function(s) sum(s^2))),
    # See vessel_summary above for why this sums over distinct fisheries
    # ([!duplicated(Fishery)]), not sum(unique(mean.share.fishery)^2),
    # which collapses fisheries that tie on long-run mean share.
    H_LR           = sum(mean.share.fishery[!duplicated(Fishery)]^2),
    .groups = "drop"
  ) %>%
  mutate(Phi = H_bar - H_LR) %>%
  left_join(
    active_vessel_years %>%
      mutate(period = period_of(Batch.Year)) %>%
      group_by(Vessel.ADFG.Number, period) %>%
      summarise(rev.cv = sd(vessel.year.rev) / mean(vessel.year.rev), .groups = "drop"),
    by = c("Vessel.ADFG.Number", "period")
  ) %>%
  mutate(meets.min.years.period = n.years.period >= MIN_ACTIVE_YEARS_PERIOD)

cat("vessel_period_summary rows:", nrow(vessel_period_summary),
    " meeting MIN_ACTIVE_YEARS_PERIOD =", MIN_ACTIVE_YEARS_PERIOD, ":",
    sum(vessel_period_summary$meets.min.years.period), "\n")

# ============================================================================
# 7. Owner-level panel (File.Number), same three objects
# ============================================================================
#
# Built from permit_register_raw (not the vessel-ID-filtered permit_register)
# because a permit with no vessel attached still belongs to an owner. This is
# what makes the owner-level wedge in 04_table3.R different from, and larger
# than, the vessel-level one.

# held.vessel.matched marks whether at least one of the underlying permit
# rows for this owner-fishery-year had a real vessel attached. Carried
# through to owner_year below so 04_table3.R can compare the wedge with and
# without permits that have no vessel identifier, per Chapter3_outline.md
# Section 3 Table 3, without re-loading and re-cleaning the register.
# Diagnostic. 04b_table_unused_by_fishery.R found several gear-04 (set
# gillnet) fisheries, S04T largest among them, sitting at exactly 1.00 (a
# table value rounded to 2 decimals, not necessarily exact) unused owner-
# share, and the sentinel-vessel-ID diagnostic in Section 2 already ruled
# out ticket dropping as the cause, none of those codes appear anywhere in
# that diagnostic's dropped-row or dropped-revenue lists. REVISED same day,
# a plain %in% presence check below (kept for the whitespace/gear-04 sweep
# further down) turned out to be too coarse to trust on its own, TRUE means
# "at least one row anywhere in 31 years", which is compatible with the code
# being fished at trivial volume, not proof the fishery is fine. This
# version counts rows and distinct File.Numbers instead of returning a bare
# boolean, and cross-references against the NA-vessel-ID diagnostic already
# printed in Section 2 (search "decisive_check_codes"), which is the
# leading hypothesis for S04T/S04X specifically. chapter3_plan.md documents
# register-side Fishery as already unspaced ("S03T") while the ticket side
# needs strip_fishery_space() applied (CFEC.Permit.Fishery, e.g. "S 03T"), a
# claim confirmed true below (zero register-side codes carry whitespace),
# so a whitespace mismatch is not the explanation for what follows.
# D91H added 2026-09-17, 04b_table_unused_by_fishery.R found it at exactly
# 1.00 unused owner-share, 1,755 owner-years across all 30 held years, 100
# distinct owners, an order of magnitude bigger than the other five codes
# that share its 1.00 share (A11A, D09H, F06B, H7DK, K09X). Unlike those
# five, D91H (Dungeness crab, pot gear, Cook Inlet) is a real, currently
# licensed "Limited" status fishery per Current CFEC Fishery Codes.txt, not
# an obviously closed or administrative one, so this print is here to see
# whether it shows the same total ticket-side absence gear 04/08/18 did
# (a linkage problem) or whether the fleet-wide closure check further below
# clears it as a genuine closure instead.
check_codes <- c("S04T", "S04X", "S04P", "S08P", "S03T", "D91H")
cat("\n===== Register-side vs ticket-side Fishery code volume, flagged codes =====\n")
for (code in check_codes) {
  register_rows <- permit_register_raw %>% filter(Fishery == code)
  ticket_rows   <- fished_vessel_fishery_year %>% filter(Fishery == code)
  cat(code, ": register rows =", nrow(register_rows), ", distinct register File.Number =",
      n_distinct(register_rows$File.Number),
      " | ticket-side rows =", nrow(ticket_rows), ", distinct ticket-side File.Number =",
      n_distinct(ticket_rows$File.Number), ", ticket-side revenue =", round(sum(ticket_rows$revenue, na.rm = TRUE)), "\n")
}

held_file_numbers <- unique(permit_register_raw$File.Number[permit_register_raw$Fishery %in% check_codes])
fished_file_numbers <- unique(fished_vessel_fishery_year$File.Number[fished_vessel_fishery_year$Fishery %in% check_codes])
cat("Across the 5 flagged codes together, distinct held File.Number:", length(held_file_numbers),
    ", distinct fished File.Number:", length(fished_file_numbers),
    ", intersection:", length(intersect(held_file_numbers, fished_file_numbers)), "\n")

register_fisheries <- unique(permit_register_raw$Fishery)
cat("\nRegister-side Fishery values containing whitespace:",
    sum(grepl(" ", register_fisheries, fixed = TRUE)), "of", length(register_fisheries), "distinct codes\n")

gear04_register <- register_fisheries[substr(register_fisheries, 2, 3) == "04"]
cat("Register-side Fishery codes with gear digits '04' (up to 15 shown):\n")
print(head(gear04_register, 15))
cat("Of those, also present in the ticket-side (post-strip) Fishery set:",
    sum(gear04_register %in% unique(fished_vessel_fishery_year$Fishery)), "of", length(gear04_register), "\n")

held_owner_fishery <- permit_register_raw %>%
  filter(!is.na(File.Number)) %>%
  group_by(File.Number, Batch.Year, Fishery) %>%
  summarise(held = TRUE, held.vessel.matched = any(has.vessel.id), .groups = "drop")

fished_owner_fishery_year <- fished_vessel_fishery_year %>%
  filter(!is.na(File.Number)) %>%
  group_by(File.Number, Batch.Year, Fishery) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")

owner_fishery_year <- held_owner_fishery %>%
  full_join(fished_owner_fishery_year, by = c("File.Number", "Batch.Year", "Fishery")) %>%
  mutate(
    held   = replace_na(held, FALSE),
    held.vessel.matched = replace_na(held.vessel.matched, FALSE),
    fished = !is.na(revenue) & revenue > 0,
    revenue = replace_na(revenue, 0)
  ) %>%
  deflate("revenue", deflator) %>%
  # fishery.year.active reused unchanged from Section 4's
  # fishery_year_active_lookup, see that definition for the full reasoning.
  # Same underlying fishery-year activity fact either way, so vessel and
  # owner grain share one definition rather than each computing its own.
  left_join(fishery_year_active_lookup, by = c("Batch.Year", "Fishery")) %>%
  mutate(fishery.year.active = replace_na(fishery.year.active, FALSE))

# Fleet-wide fishery-year closure check, a finer test than the pooled
# unused.share in 04b_table_unused_by_fishery.R. That statistic asks "did
# THIS REGISTERED HOLDER fish THIS fishery in THIS year", pooled across all
# 31 years. This asks a different question, "did ANYONE, held or not, land
# anything under this fishery code in this specific year", independent of
# the register entirely. A fishery genuinely closed (or not yet opened, or
# already retired) in a given year cannot function as backup capacity that
# year no matter how the held side is measured, and pooling years together
# can hide exactly this, a fishery active for half the panel and dead for
# the other half would not necessarily stand out in the pooled number.
# held_owner_fishery already reflects every current exclusion
# (JUNK_GEAR_CODES, EXCLUDED_GEAR_DIGITS_DATA_GAP, NON_HARVEST_FISHERY_CODES,
# all applied to permit_register_raw in Section 1), so this only looks at
# fisheries currently left in.

# A fishery held in only 1-2 distinct years can't show a meaningful "share
# of years closed", floor matches the spirit of 04b's MIN_HELD_FOR_RANKING
# without reusing that name, this is counting distinct YEARS held, not
# owner-year rows.
MIN_YEARS_HELD_FOR_CLOSURE_CHECK <- 3

fishery_year_held_cells <- owner_fishery_year %>%
  filter(held) %>%
  distinct(Fishery, Batch.Year, fishery.year.active)

fishery_closure_summary <- fishery_year_held_cells %>%
  group_by(Fishery) %>%
  summarise(
    n.years.held         = n(),
    n.years.fleet.closed = sum(!fishery.year.active),
    closed.year.share    = n.years.fleet.closed / n.years.held,
    .groups = "drop"
  ) %>%
  filter(n.years.held >= MIN_YEARS_HELD_FOR_CLOSURE_CHECK)

cat("\n===== Fisheries where EVERY held year saw zero fleet-wide landings (n.years.held >=",
    MIN_YEARS_HELD_FOR_CLOSURE_CHECK, ") =====\n")
print(fishery_closure_summary %>% filter(closed.year.share == 1) %>% arrange(desc(n.years.held)), n = Inf)

cat("\n===== Fisheries with a MIX of active and fleet-wide-closed years (partial closure, n.years.held >=",
    MIN_YEARS_HELD_FOR_CLOSURE_CHECK, ") =====\n")
print(fishery_closure_summary %>% filter(closed.year.share > 0, closed.year.share < 1) %>%
        arrange(desc(closed.year.share)), n = 30)

cat("\nTotal held (Fishery, Batch.Year) cells:", nrow(fishery_year_held_cells),
    ", with zero fleet-wide landings from anyone:", sum(!fishery_year_held_cells$fishery.year.active),
    "(", round(100 * mean(!fishery_year_held_cells$fishery.year.active), 1), "% )\n")

fleet_mean_revenue_owner <- owner_fishery_year %>%
  filter(fished) %>%
  group_by(Batch.Year, Fishery) %>%
  summarise(fleet_mean_revenue = mean(revenue, na.rm = TRUE), .groups = "drop")

owner_fishery_year <- owner_fishery_year %>%
  left_join(fleet_mean_revenue_owner, by = c("Batch.Year", "Fishery"))

# Owner-level permit-serial counts, the Table 3 analogue of Section 4b's
# vessel-level permit-stacking check. Owner-inclusive ("with" version) only,
# matching held_owner_fishery above, not crossed with the matched/unmatched
# vessel-ID split, that would be a four-way comparison for a question this
# pipeline has not been asked to resolve yet.
held_owner_permit <- permit_register_raw %>%
  filter(!is.na(File.Number)) %>%
  distinct(File.Number, Batch.Year, Fishery, CFEC.Permit.Serial.Number) %>%
  mutate(held = TRUE)

fished_owner_permit_year <- fished_vessel_permit_year %>%
  filter(!is.na(File.Number)) %>%
  group_by(File.Number, Batch.Year, Fishery, CFEC.Permit.Serial.Number) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")

owner_year_permit_level <- held_owner_permit %>%
  full_join(fished_owner_permit_year,
            by = c("File.Number", "Batch.Year", "Fishery", "CFEC.Permit.Serial.Number")) %>%
  mutate(held = replace_na(held, FALSE), fished = !is.na(revenue) & revenue > 0) %>%
  group_by(File.Number, Batch.Year) %>%
  summarise(
    n.held.permit     = sum(held),
    n.unfished.permit = sum(held & !fished),
    .groups = "drop"
  ) %>%
  mutate(unused.count.share.permit = if_else(n.held.permit > 0, n.unfished.permit / n.held.permit, NA_real_))

owner_year <- owner_fishery_year %>%
  group_by(File.Number, Batch.Year) %>%
  summarise(
    # "with" columns, includes permits with no vessel attached
    n.held.fishery     = sum(held),
    n.fished.fishery   = sum(held & fished),
    n.unfished.fishery = sum(held & !fished),
    fished.value       = sum(revenue[held & fished], na.rm = TRUE),
    forgone.value      = sum(replace_na(fleet_mean_revenue[held & !fished], 0)),
    # "without" columns, restricted to permits matched to a real vessel,
    # what permit_link.R and the vessel-level panel both already do
    n.held.fishery.matched     = sum(held & held.vessel.matched),
    n.unfished.fishery.matched = sum(held & held.vessel.matched & !fished),
    forgone.value.matched      = sum(replace_na(fleet_mean_revenue[held & held.vessel.matched & !fished], 0)),
    # "Active" variants, held restricted to fishery-years with real fleet-
    # wide activity from anyone (fishery.year.active, Section 4/joined onto
    # owner_fishery_year above). See 04_table3.R's "fishable fishery-years
    # only" rows, the D91H case (a real Cook Inlet Dungeness crab permit,
    # zero ticket-side presence in 30 years) is why this exists, a permit
    # for a fishery nobody ever lands under is not a genuine backup option.
    n.held.fishery.active     = sum(held & fishery.year.active),
    n.unfished.fishery.active = sum(held & fishery.year.active & !fished),
    forgone.value.active      = sum(replace_na(fleet_mean_revenue[held & fishery.year.active & !fished], 0)),
    owner.year.rev     = sum(revenue, na.rm = TRUE),
    # Owner-level mirror of vessel_year's own hhi column (Section 5, same
    # formula), added purely so 01b_build_rolling_panel_owner.R can run the
    # identical hhi_year.rolling-vs-hhi self-consistency check 01b_ already
    # runs at the vessel level, rather than skipping it for owners for lack
    # of a comparison column. Purely additive, does not touch any other
    # column's formula or values above.
    hhi                = sum((revenue[fished] / sum(revenue[fished]))^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    unused.count.share = if_else(n.held.fishery > 0, n.unfished.fishery / n.held.fishery, NA_real_),
    unused.value.share = if_else((forgone.value + fished.value) > 0,
                                  forgone.value / (forgone.value + fished.value), NA_real_),
    unused.count.share.matched = if_else(n.held.fishery.matched > 0,
                                          n.unfished.fishery.matched / n.held.fishery.matched, NA_real_),
    unused.value.share.matched = if_else((forgone.value.matched + fished.value) > 0,
                                          forgone.value.matched / (forgone.value.matched + fished.value), NA_real_),
    unused.count.share.active = if_else(n.held.fishery.active > 0,
                                         n.unfished.fishery.active / n.held.fishery.active, NA_real_),
    unused.value.share.active = if_else((forgone.value.active + fished.value) > 0,
                                         forgone.value.active / (forgone.value.active + fished.value), NA_real_)
  ) %>%
  left_join(owner_year_permit_level, by = c("File.Number", "Batch.Year"))

cat("Mean owner-level unused.count.share (fishery-class):",
    round(mean(owner_year$unused.count.share, na.rm = TRUE), 4),
    " vs (permit-serial):", round(mean(owner_year$unused.count.share.permit, na.rm = TRUE), 4), "\n")

# Diagnostic. 04b_table_unused_by_fishery.R's unrounded >0.95 listing (once
# run) shows this is not confined to S04T/S04X/S04P/S08P, nearly every gear
# "04" (set gillnet) fishery in the state sits at 0.95+ unused share (Cook
# Inlet S04H, Lower Yukon S04Y, Kuskokwim S04W, Yakutat S04D, Norton Sound
# S04Z, on top of the four already flagged), together a large share of the
# whole sample, while the neighboring gear "03" (drift gillnet) code S03T
# matches ticket data fine ($billions in confirmed revenue). Neither the
# NA-vessel-ID hypothesis (Section 2, ruled out directly, only 25 NA-vessel
# rows exist in the whole panel, all under S03T) nor the blank-
# CFEC.Permit.Fishery hypothesis (also Section 2, CFEC.Permit.Fishery and
# Permit.Fishery show identical counts for these codes) explains it, so
# whatever real ticket rows exist for these owners under these codes appear
# to be genuinely rare, not filtered out. This checks a live alternative,
# CFEC's own Report 12-02-N documents "permit stacking" for Bristol Bay set
# gillnet specifically, operators legally holding a permit they do not
# personally fish that year. If these owners generate real revenue under
# SOME fishery code that year, that is consistent with stacking/an active
# choice among permits actually held (an economic finding, not a bug). If
# they generate zero revenue anywhere, that points back at a coding or
# matching problem instead.
gear04_owner_check <- owner_fishery_year %>%
  filter(held, !fished, substr(Fishery, 2, 3) == "04") %>%
  distinct(File.Number, Batch.Year, Fishery) %>%
  left_join(owner_year %>% select(File.Number, Batch.Year, owner.year.rev, n.fished.fishery),
            by = c("File.Number", "Batch.Year"))

cat("\n===== Owner-fishery-years held-but-unfished under a gear '04' code, whether the owner fished ANYTHING that year =====\n")
cat("Cells:", nrow(gear04_owner_check), "\n")
cat("Of those, owner had positive revenue somewhere that year (owner.year.rev > 0):",
    sum(gear04_owner_check$owner.year.rev > 0, na.rm = TRUE),
    "(", round(100 * mean(gear04_owner_check$owner.year.rev > 0, na.rm = TRUE), 2), "% )\n")
cat("Of those, owner fished at least one OTHER fishery that same year (n.fished.fishery > 0):",
    sum(gear04_owner_check$n.fished.fishery > 0, na.rm = TRUE),
    "(", round(100 * mean(gear04_owner_check$n.fished.fishery > 0, na.rm = TRUE), 2), "% )\n")
cat("Breakdown by Fishery code (up to 20 shown):\n")
print(gear04_owner_check %>% group_by(Fishery) %>%
        summarise(n.cells = n(), share.owner.fished.something = mean(owner.year.rev > 0, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(n.cells)) %>% head(20))

# Diagnostic. Every relocation hypothesis tested so far (sentinel/NA vessel
# ID, blank CFEC.Permit.Fishery, whitespace, permit stacking, revenue under
# a DIFFERENT fishery code via gear04_owner_check just above) has been
# refuted by real counts, and the raw catch_data_temp column/row/species
# inventory printed in Section 2 shows a healthy, complete, unfiltered pull,
# not a hidden upstream filter. What is left is narrower, these File.Numbers
# generate no activity anywhere under CFEC.Permit.Holder.Filing.Number, full
# stop. This prints actual example records for a handful of persistent S04T
# holders who never once show revenue, rather than another aggregate count,
# a formatting or ID-continuity quirk (an off-by-one, a leading zero, a
# historical filing-number remapping across CFEC's own systems) might be
# visible by eye on real rows in a way no join-based test can surface. Also
# checks CFEC.Vessel.Owner.Filing.Number, not just
# CFEC.Permit.Holder.Filing.Number, in case the vessel owner identity, not
# the permit holder identity, is what actually links to any real ticket
# activity this person has.
sample_holders <- permit_register_raw %>%
  filter(Fishery == "S04T") %>%
  inner_join(owner_year %>% filter(owner.year.rev == 0), by = c("File.Number", "Batch.Year")) %>%
  count(File.Number, sort = TRUE) %>%
  head(5) %>%
  pull(File.Number)

cat("\n===== Sample of persistent S04T holders with zero owner revenue, searched for ANY footprint anywhere in catch_data_temp =====\n")
for (fn in sample_holders) {
  cat("\n--- File.Number", fn, "---\n")
  cat("Register rows for this File.Number under S04T:\n")
  print(permit_register_raw %>% filter(File.Number == fn, Fishery == "S04T") %>%
          select(Batch.Year, Fishery, Vessel.ADFG.Number, has.vessel.id))
  n.as.permit.holder <- sum(catch_data_temp$CFEC.Permit.Holder.Filing.Number == fn, na.rm = TRUE)
  n.as.vessel.owner  <- sum(catch_data_temp$CFEC.Vessel.Owner.Filing.Number == fn, na.rm = TRUE)
  cat("Rows anywhere in catch_data_temp with CFEC.Permit.Holder.Filing.Number ==", fn, ":", n.as.permit.holder, "\n")
  cat("Rows anywhere in catch_data_temp with CFEC.Vessel.Owner.Filing.Number ==", fn, ":", n.as.vessel.owner, "\n")
  if (n.as.permit.holder > 0) {
    print(catch_data_temp %>% filter(CFEC.Permit.Holder.Filing.Number == fn) %>%
            select(Batch.Year, CFEC.Permit.Fishery, Vessel.ADFG.Number, CFEC.Value..Detail.) %>% head(5))
  }
  if (n.as.vessel.owner > 0) {
    print(catch_data_temp %>% filter(CFEC.Vessel.Owner.Filing.Number == fn) %>%
            select(Batch.Year, CFEC.Permit.Fishery, Vessel.ADFG.Number, CFEC.Value..Detail.) %>% head(5))
  }
}

# Refined diagnostic following the sample above. That sample was a genuine
# mix, not a uniform failure, one holder turned out to have real,
# substantial S04T revenue in a DIFFERENT year than the one that flagged
# them as zero-revenue, a mundane "idle this one year" pattern consistent
# with CFEC Report 25-4N's own ~18.5% latency benchmark, not a bug. Another
# showed zero footprint anywhere, as permit holder or vessel owner, across
# all 35 years of register data searched, a much more concerning "true
# ghost" pattern. gear04_owner_check earlier only tested same-YEAR
# activity, this checks LIFETIME activity instead, does this File.Number
# generate ticket revenue under its own identity in ANY year at all, not
# just the specific year flagged as unused, to separate "sometimes idle,
# sometimes active" (real, unremarkable behavior, should not worry anyone)
# from "never once appears, in any role, in the entire 31-year panel" (the
# pattern actually worth concern, and the one to size before concluding
# anything about how much of Table 3's wedge is real).
gear04_holders <- permit_register_raw %>%
  filter(substr(Fishery, 2, 3) == "04") %>%
  distinct(File.Number)

lifetime_activity <- gear04_holders %>%
  mutate(
    ever.permit.holder = File.Number %in% catch_data_temp$CFEC.Permit.Holder.Filing.Number,
    ever.vessel.owner  = File.Number %in% catch_data_temp$CFEC.Vessel.Owner.Filing.Number
  )

cat("\n===== Lifetime footprint check, ALL gear-04 register holders (any role, any fishery, any year, ever) =====\n")
cat("Distinct gear-04 register holders:", nrow(lifetime_activity), "\n")
cat("Never once a permit holder on any ticket, any year, any fishery:",
    sum(!lifetime_activity$ever.permit.holder),
    "(", round(100 * mean(!lifetime_activity$ever.permit.holder), 2), "% )\n")
cat("Never once a permit holder OR a vessel owner on any ticket, any year, any fishery (true zero footprint):",
    sum(!lifetime_activity$ever.permit.holder & !lifetime_activity$ever.vessel.owner),
    "(", round(100 * mean(!lifetime_activity$ever.permit.holder & !lifetime_activity$ever.vessel.owner), 2), "% )\n")

# Diagnostic, generalizing the gear-04 check just above. Gear "08" looked
# like a junk code by description alone earlier in this file and turned out
# to be a real, active gear (fish wheel), so guessing gear-by-gear from
# CFEC's dictionary descriptions is exactly the wrong way to decide what
# else belongs on an exclusion list, gear-code meanings are not standardized
# across species letters in CFEC's system anyway. This instead runs the
# identical lifetime ghost-holder test across EVERY 2-digit gear code
# actually present in the register, so any other code with the same
# near-total "never once appears anywhere, any role, any year" signature
# gear 04 has shows up on evidence, not assumption.
gear_ghost_by_code <- permit_register_raw %>%
  mutate(gear.digits = substr(Fishery, 2, 3)) %>%
  distinct(File.Number, gear.digits) %>%
  mutate(
    ever.permit.holder = File.Number %in% catch_data_temp$CFEC.Permit.Holder.Filing.Number,
    ever.vessel.owner  = File.Number %in% catch_data_temp$CFEC.Vessel.Owner.Filing.Number,
    ghost = !ever.permit.holder & !ever.vessel.owner
  ) %>%
  group_by(gear.digits) %>%
  summarise(n.holders = n(), ghost.share = round(mean(ghost), 4), .groups = "drop") %>%
  filter(n.holders >= 30) %>%
  arrange(desc(ghost.share))

cat("\n===== Lifetime ghost-holder share BY GEAR DIGIT, every code with at least 30 distinct holders =====\n")
print(gear_ghost_by_code, n = Inf)

# Attribute check on the population chapter3_writeup.tex Section 3 calls the
# clearest case of holding without fishing, no vessel on record for this
# permit AND the owner had zero ticket revenue anywhere that year. That
# claim is currently only prose, nothing in this pipeline had isolated the
# population and looked at it directly. The concern worth ruling out is
# whether the Permit.Status filter above (Section 1) actually caught every
# dropped or cancelled permit, or whether some of these rows are stale
# register entries rather than genuine held-but-idle access. If filtering
# missed something, it should show up here as a Permit.Status or
# Permit.Type value that is not "Current Owner" or otherwise looks off,
# concentrated in this population rather than spread evenly across the
# whole panel.
zero_revenue_owner_years <- owner_year %>%
  filter(owner.year.rev == 0) %>%
  distinct(File.Number, Batch.Year)

both_bars_permits <- permit_register_raw %>%
  filter(!has.vessel.id) %>%
  semi_join(zero_revenue_owner_years, by = c("File.Number", "Batch.Year"))

cat("\n===== Attributes of permits with no vessel on record AND zero owner revenue that year =====\n")
cat("Permit register rows in this population:", nrow(both_bars_permits),
    "of", sum(!permit_register_raw$has.vessel.id), "vessel-unmatched rows overall",
    "(", round(100 * nrow(both_bars_permits) / sum(!permit_register_raw$has.vessel.id), 2), "% )\n")
if ("Permit.Status" %in% names(both_bars_permits)) {
  cat("Permit.Status within this population (should read entirely \"Current Owner\" if the Section 1",
      "filter is catching everything it should) --\n")
  print(both_bars_permits %>% count(Permit.Status, sort = TRUE))
} else {
  warning("Permit.Status not found on permit_register_raw, cannot check it for the ",
          "no-vessel/zero-revenue population.")
}
if ("Permit.Type" %in% names(both_bars_permits)) {
  cat("Permit.Type within this population (reported as a diagnostic, transferability varies by",
      "type and has not been separately verified, see the Section 1 print of this same column) --\n")
  print(both_bars_permits %>% count(Permit.Type, sort = TRUE))
} else {
  warning("Permit.Type not found on permit_register_raw, cannot check it for the ",
          "no-vessel/zero-revenue population.")
}

active_owner_years <- owner_year %>% filter(owner.year.rev > 0) %>%
  select(File.Number, Batch.Year, owner.year.rev)

owner_share_raw <- owner_fishery_year %>%
  filter(fished) %>%
  inner_join(active_owner_years, by = c("File.Number", "Batch.Year")) %>%
  mutate(share = revenue / owner.year.rev) %>%
  select(File.Number, Batch.Year, Fishery, share)

owner_share_panel <- owner_share_raw %>%
  group_by(File.Number) %>%
  complete(Fishery, Batch.Year, fill = list(share = 0)) %>%
  ungroup() %>%
  semi_join(active_owner_years, by = c("File.Number", "Batch.Year"))

# Per owner-fishery long-run mean share, s_bar_ij. Pulled out into its own
# saved object here rather than built via an inline mutate() inside
# owner_summary's own pipe (the way this used to be constructed), purely so
# this mirrors vessel_mean_share exactly, INCLUDING being available on its
# own for 05_table4_figure3_owner.R to reuse as the owner-level passive
# buy-and-hold benchmark's portfolio weights, the same role vessel_mean_share
# plays for 05_table4_figure3.R (see Section 6's comment on vessel_mean_share
# for why that has to be a standalone object rather than an inline mutate()
# too). Numerically identical to the old inline version either way, a
# left_join of a (File.Number, Fishery) mean back onto its own group is the
# same value as computing that mean inline within the same grouping, only
# the construction path changes.
owner_mean_share <- owner_share_panel %>%
  group_by(File.Number, Fishery) %>%
  summarise(mean.share.fishery = mean(share), .groups = "drop")

owner_summary <- owner_share_panel %>%
  left_join(owner_mean_share, by = c("File.Number", "Fishery")) %>%
  group_by(File.Number) %>%
  summarise(
    n.years = n_distinct(Batch.Year),
    H_bar   = mean(tapply(share, Batch.Year, function(s) sum(s^2))),
    # See vessel_summary in Section 6 for why this sums over distinct
    # fisheries ([!duplicated(Fishery)]), not sum(unique(mean.share.fishery)^2).
    H_LR    = sum(mean.share.fishery[!duplicated(Fishery)]^2),
    .groups = "drop"
  ) %>%
  mutate(Phi = H_bar - H_LR) %>%
  left_join(
    active_owner_years %>%
      group_by(File.Number) %>%
      summarise(rev.cv = sd(owner.year.rev) / mean(owner.year.rev), .groups = "drop"),
    by = "File.Number"
  )

# prime.fishery = the fishery with the most total realized revenue across
# the owner's whole panel, the exact owner-level mirror of vessel_summary's
# own prime_fishery construction in Section 6 (ranked on summed revenue
# directly, not shares, for the same reason given there). Named
# prime_fishery_owner, not prime_fishery, so it does not collide with the
# vessel-level object of the same underlying idea already in scope earlier
# in this same script run. Needed as the fixed effect in
# 05_table4_figure3_owner.R's Table 4 regression, exactly the role
# prime.fishery plays for vessels there.
prime_fishery_owner <- owner_fishery_year %>%
  filter(fished) %>%
  group_by(File.Number, Fishery) %>%
  summarise(total.rev = sum(revenue), .groups = "drop") %>%
  group_by(File.Number) %>%
  slice_max(total.rev, n = 1, with_ties = FALSE) %>%
  select(File.Number, prime.fishery = Fishery)

owner_summary <- owner_summary %>%
  left_join(prime_fishery_owner, by = "File.Number") %>%
  mutate(meets.min.years = n.years >= MIN_ACTIVE_YEARS)

cat("owner_summary rows:", nrow(owner_summary),
    " meeting MIN_ACTIVE_YEARS:", sum(owner_summary$meets.min.years), "\n")

# ============================================================================
# 7b. Period-specific decomposition, owner level (mirrors Section 6b)
# ============================================================================
#
# Reuses period_of() and the same two thresholds from Section 6b, the intent
# is one shared set of calendar periods for the whole pipeline, not
# separately-computed owner-level periods that could drift from the
# vessel-level ones.

owner_share_raw_period <- owner_share_raw %>% mutate(period = period_of(Batch.Year))

owner_period_share_panel <- owner_share_raw_period %>%
  group_by(File.Number, period) %>%
  complete(Fishery, Batch.Year, fill = list(share = 0)) %>%
  ungroup()

owner_period_summary <- owner_period_share_panel %>%
  group_by(File.Number, period, Fishery) %>%
  mutate(mean.share.fishery = mean(share)) %>%
  group_by(File.Number, period) %>%
  summarise(
    n.years.period = n_distinct(Batch.Year),
    H_bar          = mean(tapply(share, Batch.Year, function(s) sum(s^2))),
    # See vessel_summary in Section 6 for why this sums over distinct
    # fisheries ([!duplicated(Fishery)]), not sum(unique(mean.share.fishery)^2).
    H_LR           = sum(mean.share.fishery[!duplicated(Fishery)]^2),
    .groups = "drop"
  ) %>%
  mutate(Phi = H_bar - H_LR) %>%
  left_join(
    active_owner_years %>%
      mutate(period = period_of(Batch.Year)) %>%
      group_by(File.Number, period) %>%
      summarise(rev.cv = sd(owner.year.rev) / mean(owner.year.rev), .groups = "drop"),
    by = c("File.Number", "period")
  ) %>%
  mutate(meets.min.years.period = n.years.period >= MIN_ACTIVE_YEARS_PERIOD)

cat("owner_period_summary rows:", nrow(owner_period_summary),
    " meeting MIN_ACTIVE_YEARS_PERIOD:", sum(owner_period_summary$meets.min.years.period), "\n")

# ============================================================================
# 8. Save
# ============================================================================

save(
  vessel_fishery_year, vessel_year, vessel_summary, vessel_mean_share, vessel_share_panel,
  vessel_period_summary,
  owner_fishery_year, owner_year, owner_summary, owner_mean_share, owner_share_panel,
  owner_period_summary,
  period_bounds,
  match_diag, fleet_mean_revenue, fleet_mean_revenue_owner,
  MAX_YEAR,
  file = panel_path
)
cat("Saved panel to", panel_path, "\n")
