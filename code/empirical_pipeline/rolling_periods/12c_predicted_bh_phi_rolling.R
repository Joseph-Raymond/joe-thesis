# Chapter 3 empirical pipeline, rolling-window "predicted buy-and-hold Phi"
# benchmark, the empirical analog of Chapter 2's Phi^BH decomposition term
#
# CONCEPT. Chapter 2's simulation (sim_core.py) decomposes a buy-and-hold
# (BH) operator's time-averaged concentration H_bar into a long-run,
# structural piece H_LR (concentration of the operator's OWN long-run mean
# revenue shares) and an instability piece Phi = H_bar - H_LR, the
# within-window wobble left over after the long-run mix is netted out. For a
# genuine BH operator, Phi is generated PURELY by pass-through of exogenous
# year-to-year fleet-wide rates into a FIXED portfolio, since a BH operator
# never itself reallocates effort across fisheries. This script builds the
# empirical analog Phi^BH_{i,s} for each (vessel, window.start), asking what
# a vessel-window's Phi would have been had that vessel fixed its EFFORT
# (days fished per fishery) at the window's own average across the window's
# active years, letting only the fleet's own leave-one-out per-day rate
# carry year-to-year variation into revenue and hence into shares. The
# behavioral gap between a REALIZED and a PREDICTED (BH) Phi is the direct
# empirical analog of Chapter 2's Phi^regime - Phi^BH gap, the part of a
# vessel's realized instability attributable to its own reallocation choices
# rather than to exogenous pass-through alone.
#
# NOT THE SAME QUESTION 12b_predicted_bh_revenue_rolling.R ASKS.
# 12b_/13b_ fix a vessel's effort and ask what DOLLAR LEVEL that effort would
# have earned in a single held-out target year (window.end), a prediction-
# accuracy exercise validated against actual.revenue. This script fixes a
# vessel's effort and asks what SHAPE (share-of-portfolio, not dollar level)
# that effort would have produced across EVERY active year of the window,
# feeding a full within-window Hbar/H_LR/Phi decomposition exactly parallel
# to vessel_window_summary.rolling's own realized construction (01b_'s
# Section 3.0 shortcut, reused directly below, see Section 6). There is no
# held-out year here at all, every active year of the window contributes,
# because Phi is a WITHIN-window instability measure, not a single-year
# forecast target.
#
# SECTIONS 1-2 (day/revenue/fleet-rate machinery) ARE DUPLICATED FROM 12b_,
# NOT SOURCED FROM A SHARED HELPER, on purpose. This pipeline already has an
# established convention of duplicating catch_data_temp reload/cleaning
# across independently-purposed scripts (06_, 08b_, 09b_, 12b_ each reload
# and re-clean it themselves rather than share one cleaned copy, see 12b_'s
# own header note), and 12c_ is exactly one more instance of that same
# convention, not an exception to it. Keeping 12c_ fully self-contained also
# means a future edit to 12b_'s Section 1/2 (e.g. Task B's decomposition
# columns, added in that other script) cannot silently change 12c_'s own
# Phi^BH numbers out from under it, and vice versa, each script's
# correctness can be verified by reading it top to bottom on its own, the
# same property 12b_'s header note argues for its own duplication.
#
# TWO REVENUE SOURCES, reused exactly as 12b_'s own header explains, for the
# same reasons. This script's own cleaned, deflated revenue.clean (Section
# 1) feeds every rate/ratio computation (Sections 1-4), while
# vessel_fishery_year's own "fished"/"revenue" columns (already deflated by
# 01_build_panel.R, reused as-is, never recomputed here) decide which
# (vessel, fishery, year) triples qualify as "active" for the fixed-
# portfolio lookback (Section 3), feed the realized-revenue coverage
# diagnostic (Section 6), and feed the support-matched realized Phi
# construction (Section 7). See 12b_'s header note for the full
# justification, identical here.
#
# FLEET RATE FOR YEARS THE VESSEL ITSELF SKIPPED A FISHERY, a genuine
# extension beyond a literal copy of 12b_'s Section 2, required to make this
# script's central feature well-defined. 12b_ only ever predicts INTO a
# year/fishery the vessel was itself actively fishing (target_fisheries.rolling
# is built from vessel_fishery_year %>% filter(fished)), so its leave-one-out
# fleet rate fleet_rate_loo.rolling, keyed to active_bh.rolling, is always
# well-posed there, there is always a "self" to leave out. This script's
# whole point is different, it must predict fishery j's revenue in EVERY
# active year of the window, INCLUDING years the vessel's realized behavior
# skipped fishery j entirely (Section 4), and leave-one-out is not merely
# unavailable there, it is conceptually ill-posed, there is no observation
# of vessel i's in that year's fleet sum to leave out in the first place.
# Section 2 below therefore builds a SECOND rate table, fleet_rate_full.rolling,
# the plain (non-leave-one-out) fleet mean rate per (Fishery, Batch.Year),
# reusing fleet_fishery_year_bh.rolling (already computed there for
# fleet_rate_loo.rolling's own denominator) at no extra cost. Section 4's
# prediction step uses fleet.rate.excl.i wherever the vessel itself HAS A ROW
# in active_bh.rolling for that (fishery, year), i.e. it was itself active
# there, REGARDLESS of whether that row's own leave-one-out rate clears the
# BH_MIN_OTHER_ACTIVE_VESSELS floor (a thin-fleet cell where the vessel WAS
# active correctly resolves to NA there, exactly matching 12b_'s own
# behavior for a thin target year, see Section 4's own comment on the
# n.active == BH_MIN_OTHER_ACTIVE_VESSELS boundary bug this fixes, per a
# methodological review), and falls back to the plain fleet mean ONLY for
# years/fisheries the vessel has NO row in active_bh.rolling at all (it
# skipped that fishery-year entirely, so there is nothing of its own to
# leak into, or need excluding from, the plain mean).
#
# FIXED PORTFOLIO, WHOLE-WINDOW BASIS, NOT A 5-YEAR LOOKBACK. Unlike 12b_'s
# Section 3 (which deliberately holds out window.end as a single prediction
# target and so only ever averages avg.days/vessel.ratio over the window's
# first 5 years), this script has no held-out year to protect, Phi is a
# whole-window property, so J*_{i,s}, avg.days_ij, and vessel.ratio_ij are
# all built from ALL of the window's own active years (up to 6, not 5), the
# same whole-window-average logic H_LR_{i,s} itself already uses (01b_'s
# S_ijw / n.years.window shortcut, Section 4 of 01b_). See Section 3 below.
#
# YEAR-COVERAGE FLOOR (Section 5), a safeguard against a year's predicted
# share vector being dominated by whichever one or two of J*_{i,s}'s
# fisheries happened to have a computable fleet rate that year, while the
# rest of the (typically larger-weight) portfolio silently drops out of that
# year's normalization. A year only survives into the Hbar^BH/H_LR^BH
# average if the J*_{i,s} fisheries with a DEFINED predicted_ijt that year
# together account for at least BH_MIN_YEAR_COVERAGE_SHARE (50%) of J*_{i,s}'s
# own typical (mean-across-its-own-defined-years) predicted dollar weight,
# see Section 5 for the exact construction. This is deliberately a
# WEIGHT-based floor, not a simple "at least half of J*'s COUNT of fisheries
# defined" floor, a portfolio with one large fishery and three small ones
# should not be excluded just because the three small ones went undefined
# one year, and should not be kept just because the three small ones stayed
# defined while the one large one dropped out. A year sitting right at the
# 0.5 boundary still gets renormalized to sum to 1 over whatever survives,
# which can inflate that year's own HHI contribution relative to what the
# FULL J* portfolio would have shown, Section 5 now prints the distribution
# of year.coverage.share among surviving years, and a sensitivity count
# against a stricter 0.75 floor, so this is visible rather than hidden
# behind the single pass/fail cutoff, per a methodological review.
#
# H_LR^BH's OWN ALGEBRAIC SHORTCUT (Section 6) is the exact same one 01b_
# uses for the realized H_LR, a fishery's predicted share in a year it has
# no defined prediction (even a SURVIVING year) contributes exactly 0 to
# that fishery's own numerator sum, and the shared denominator across every
# J*_{i,s} fishery is n.surviving.years (the vessel-window's own surviving-
# year count), never a per-fishery count of the years it individually
# cleared. See Section 6's own comment for the direct comparison to 01b_'s
# S_ijw / n.years.window construction. Section 7 below reuses the IDENTICAL
# shortcut on REALIZED (not predicted) shares, restricted to the same
# support, for the support-matched comparator.
#
# TWO COMPARATORS, PRIMARY AND SECONDARY (Section 7-8), added per a
# methodological review. Raw realized Phi (vessel_window_summary.rolling's
# own Phi, built over the vessel's FULL realized portfolio across ALL
# n.years.window active years) and Phi.BH (this script's own Section 6,
# built over J*_{i,s} ONLY, across n.surviving.years ONLY, and, within a
# surviving year, only the J*_{i,s} fisheries with a DEFINED predicted.ijt
# that particular year) do NOT share a support whenever J* drops a realized
# fishery, a year fails the coverage floor, or a J* fishery is undefined in
# an otherwise-surviving year, and the review demonstrated concretely that
# this alone (with ZERO true reallocation) can manufacture a spurious
# Phi - Phi.BH gap of EITHER SIGN, not just positive, a later, independent
# reproduction of the same class of confound produced a NEGATIVE spurious
# gap, the sign depends on the specific portfolio/coverage pattern, not on
# the direction of the mismatch itself. Phi.matched (Section 7) is a
# REALIZED analog of Phi.BH computed on the IDENTICAL support, matched at
# the exact (fishery, year) CELL level (not just "same fisheries on average,
# same years on average"), using the same S_ijw/n shortcut, so
# Phi.gap.matched = Phi.matched - Phi.BH is now the PRIMARY behavioral-gap
# comparator (what 13c_'s figures/tables are built on). Raw Phi and
# Phi.gap = Phi - Phi.BH are kept as a SECONDARY column pair so a reader can
# still see the un-matched picture. BLIND SPOT, Phi.matched is computed
# entirely INSIDE J*_{i,s}, so reallocation TOWARD a fishery outside
# J*_{i,s} is invisible to it, same as it is to Phi.BH, read coverage.BH
# alongside Phi.matched for that reason, see Section 7's own header note.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year, read-only)
# and intermediate data/ch3_rolling.rdata (window_grid.rolling,
# vessel_window_eligibility.rolling, vessel_year_window_eligible.rolling,
# vessel_window_summary.rolling, all read-only, all built by
# 01b_build_rolling_panel.R), plus a fresh reload of
# intermediate data/catch_data_temp.rdata for trip/day grain (Section 1,
# duplicated from 12b_, see above). 12b_predicted_bh_revenue_rolling.R
# itself is not read, edited, or depended on in any way.
#
# Saves predicted_bh_phi_vessel_window.rolling (one row per
# Vessel.ADFG.Number x window.start) to
# intermediate data/ch3_predicted_bh_phi.rdata, columns n.fisheries.J.star,
# mean.lookback.BH, n.surviving.years, H_bar.BH, H_LR.BH, Phi.BH,
# coverage.BH, H_bar.matched, H_LR.matched, Phi.matched,
# n.realized.matched.years, Phi.gap.matched (PRIMARY behavioral gap), the
# realized H_bar/H_LR/Phi/Phi.gap (SECONDARY, raw/un-matched),
# n.years.window, n.fisheries.fished.window (both from
# vessel_window_summary.rolling, carried through so a reader can see when
# the realized support diverges from J*/n.surviving.years), and
# is.specialist.window.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

if (!exists("vessel_fishery_year") || !exists("MAX_YEAR")) load(panel_path)

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
if (!exists("window_grid.rolling") || !exists("vessel_window_eligibility.rolling") ||
    !exists("vessel_year_window_eligible.rolling") || !exists("vessel_window_summary.rolling")) {
  load(rolling_panel_path)
}

# See 12b_'s identical header note, deflator rebuilt here for the same
# reason, this script's own revenue.clean (Section 1) is built fresh from a
# raw catch_data_temp reload and starts out nominal.
deflator <- load_deflator()

# ============================================================================
# Local constants
# ============================================================================
#
# BH_ prefix, this script's own local scope, matching 12b_'s established
# pattern (see that script's own comment on this convention). The first
# three duplicate 12b_'s own values exactly (same underlying questions, see
# each one's comment there), not re-derived or re-justified here beyond a
# pointer back to 12b_'s fuller comment, the last two are new to this script.

# Same still-unresolved ~50-year-trip data issue 12b_'s own comment
# describes at length, duplicated verbatim here since Section 1 below is a
# duplicate of 12b_'s own Section 1.
BH_MAX_TRIP_DURATION_DAYS <- 365

# Leave-one-out floor, checked AFTER excluding the focal vessel from the
# leave-one-out branch (Section 2/4), and reused UNCHANGED as the floor on
# the plain fleet-mean branch's own n.active (the vessel itself is never one
# of those n.active vessels in that branch, by construction, see Section 4's
# own comment, so there is no "other" to subtract there either).
BH_MIN_OTHER_ACTIVE_VESSELS <- 5

# Floor on n.ratio.years (Section 3), matching 12b_'s BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION
# exactly (same constant, same value, same underlying justification, see
# that script's own comment), reused here as the floor that decides J*_{i,s}
# membership, a fishery needs at least this many WHOLE-WINDOW years with a
# valid vessel-to-fleet ratio to enter the fixed portfolio at all.
BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION <- 3

# The year-coverage floor described at length in this script's own header
# note (Section 5). A year's J*_{i,s} fisheries with a DEFINED predicted_ijt
# that year must together carry at least this share of J*_{i,s}'s own
# typical predicted dollar weight for that year to enter the
# Hbar^BH/H_LR^BH average. 0.5 (half) chosen as the natural reading of the
# task's own "at least half" framing, not independently derived, a
# genuinely arbitrary-but-documented threshold in the same spirit as
# BH_MIN_OTHER_ACTIVE_VESSELS.
BH_MIN_YEAR_COVERAGE_SHARE <- 0.5

# NEW, per a methodological review. A stricter alternative floor, used ONLY
# for Section 5's own sensitivity diagnostic (how many of the years
# surviving the 0.5 floor would ALSO survive this stricter one), not for the
# main construction anywhere, which remains governed solely by
# BH_MIN_YEAR_COVERAGE_SHARE above. Lets a reader gauge how much of the
# surviving-year sample sits close to the 0.5 boundary (where the
# HHI-inflation-from-renormalization concern the header note raises is
# largest) without actually re-running the whole script a second time.
BH_MIN_YEAR_COVERAGE_SHARE_STRICT <- 0.75

cat("12c_predicted_bh_phi_rolling.R loaded, BH_MAX_TRIP_DURATION_DAYS =",
    BH_MAX_TRIP_DURATION_DAYS, ", BH_MIN_OTHER_ACTIVE_VESSELS =", BH_MIN_OTHER_ACTIVE_VESSELS,
    ", BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION =", BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION,
    ", BH_MIN_YEAR_COVERAGE_SHARE =", BH_MIN_YEAR_COVERAGE_SHARE,
    ", BH_MIN_YEAR_COVERAGE_SHARE_STRICT (sensitivity only) =", BH_MIN_YEAR_COVERAGE_SHARE_STRICT, "\n")

# ============================================================================
# 1. Day construction, per vessel x Batch.Year x Fishery
# ============================================================================
#
# Verbatim duplicate of 12b_predicted_bh_revenue_rolling.R's own Section 1,
# see this script's header note on why duplicated rather than shared, and
# see 12b_'s own inline comments (not repeated here line by line) for the
# full justification of every step, get.trip()'s cross-fishery day
# double-count, the canonicalize-before-exclude ordering, max() over
# inconsistent Date.Fishing.Began-implied durations, and the whole-trip
# (not whole-fishery-leg) exclusion rule.

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
  filter(Fishery != "")

cat("Ticket rows entering the BH-effort Phi day/revenue construction -", nrow(catch_data_temp), "\n")

catch_data_temp <- get.trip(catch_data_temp)
catch_data_temp <- catch_data_temp %>% mutate(trip.duration = as.numeric(trip.length) + 1)

trip_duration_raw.rolling <- catch_data_temp %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery, trip.id, trip.duration)

n_multi_duration_cells.rolling <- trip_duration_raw.rolling %>%
  count(Vessel.ADFG.Number, Batch.Year, Fishery, trip.id) %>%
  filter(n > 1) %>%
  nrow()
n_trip_fishery_cells.rolling <- trip_duration_raw.rolling %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery, trip.id) %>%
  nrow()
cat("Trip x fishery cells carrying more than one distinct raw trip.duration value -",
    n_multi_duration_cells.rolling, "of", n_trip_fishery_cells.rolling, "trip x fishery cells\n")

max_or_na.rolling <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

trip_duration_canonical.rolling <- trip_duration_raw.rolling %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery, trip.id) %>%
  summarise(trip.duration = max_or_na.rolling(trip.duration), .groups = "drop")

n_trips_total.rolling <- n_distinct(catch_data_temp$trip.id)
bad_trip_ids.rolling <- trip_duration_canonical.rolling %>%
  filter(is.na(trip.duration) | trip.duration <= 0 | trip.duration > BH_MAX_TRIP_DURATION_DAYS) %>%
  distinct(trip.id) %>%
  pull(trip.id)

revenue_all_tickets.rolling <- sum(catch_data_temp[["CFEC.Value..Detail."]], na.rm = TRUE)
revenue_excluded_tickets.rolling <- catch_data_temp %>%
  filter(trip.id %in% bad_trip_ids.rolling) %>%
  summarise(rev = sum(CFEC.Value..Detail., na.rm = TRUE)) %>%
  pull(rev)

cat("Trips excluded for an unusable canonical trip.duration -",
    length(bad_trip_ids.rolling), "of", n_trips_total.rolling, "trips (",
    round(100 * length(bad_trip_ids.rolling) / n_trips_total.rolling, 4), "% ), representing",
    round(100 * revenue_excluded_tickets.rolling / revenue_all_tickets.rolling, 4),
    "% of ticket-line revenue in this universe\n")

catch_data_clean.rolling <- catch_data_temp %>% filter(!(trip.id %in% bad_trip_ids.rolling))

vessel_fishery_year_days.rolling <- trip_duration_canonical.rolling %>%
  filter(!(trip.id %in% bad_trip_ids.rolling)) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  summarise(fishing.days = sum(trip.duration, na.rm = TRUE), .groups = "drop")

vessel_fishery_year_revenue_clean.rolling <- catch_data_clean.rolling %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  summarise(revenue.clean = sum(CFEC.Value..Detail., na.rm = TRUE), .groups = "drop")

vessel_fishery_year_bh.rolling <- vessel_fishery_year_days.rolling %>%
  full_join(vessel_fishery_year_revenue_clean.rolling,
            by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  mutate(
    fishing.days  = replace_na(fishing.days, 0),
    revenue.clean = replace_na(revenue.clean, 0)
  ) %>%
  deflate("revenue.clean", deflator) %>%
  mutate(
    rev.per.day = if_else(fishing.days > 0, revenue.clean / fishing.days, NA_real_)
  )

if (!is.null(deflator)) {
  years_missing_deflator.rolling <- setdiff(unique(vessel_fishery_year_bh.rolling$Batch.Year), deflator$Year)
  if (length(years_missing_deflator.rolling) > 0) {
    warning(
      "cpi_deflator.csv has no row for Batch.Year ",
      paste(sort(years_missing_deflator.rolling), collapse = ", "),
      ", revenue.clean silently drops to NA/0 for that year via na.rm downstream in this script's own ",
      "rev.per.day/fleet-rate machinery"
    )
  }
}

cat("Vessel x fishery x year cells with cleaned days/revenue -", nrow(vessel_fishery_year_bh.rolling),
    ", with fishing.days == 0 despite a surviving revenue row (should be ~0) -",
    sum(vessel_fishery_year_bh.rolling$fishing.days == 0 & vessel_fishery_year_bh.rolling$revenue.clean > 0), "\n")

# ============================================================================
# 2. Fleet-side per-day rate, leave-one-out AND plain fleet mean
# ============================================================================
#
# fleet_rate_loo.rolling is a verbatim duplicate of 12b_'s own Section 2.
# fleet_rate_full.rolling is NEW, see this script's header note on why a
# plain (non-leave-one-out) fleet mean rate is required here and not in
# 12b_. Both share the identical BH_MIN_OTHER_ACTIVE_VESSELS floor and the
# identical "active" definition (revenue.clean > 0, fishing.days > 0, THIS
# script's own cleaned universe, not vessel_fishery_year$fished, see 12b_'s
# own comment on why). n.other.active is kept in fleet_rate_loo.rolling
# (not dropped), Section 4 below needs it to tell "vessel present but thin
# fleet" apart from "vessel absent entirely", see that section's own comment.

active_bh.rolling <- vessel_fishery_year_bh.rolling %>%
  filter(revenue.clean > 0, fishing.days > 0)

fleet_fishery_year_bh.rolling <- active_bh.rolling %>%
  group_by(Fishery, Batch.Year) %>%
  summarise(sum.rev.per.day = sum(rev.per.day, na.rm = TRUE), n.active = n(), .groups = "drop")

fleet_rate_loo.rolling <- active_bh.rolling %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, rev.per.day) %>%
  left_join(fleet_fishery_year_bh.rolling, by = c("Fishery", "Batch.Year")) %>%
  mutate(
    n.other.active    = n.active - 1L,
    fleet.rate.excl.i = if_else(n.other.active >= BH_MIN_OTHER_ACTIVE_VESSELS,
                                 (sum.rev.per.day - rev.per.day) / n.other.active,
                                 NA_real_)
  ) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, fleet.rate.excl.i, n.other.active)

cat("Vessel x fishery x year cells with a defined leave-one-out fleet rate -",
    sum(is.finite(fleet_rate_loo.rolling$fleet.rate.excl.i)), "of", nrow(fleet_rate_loo.rolling),
    "active cells\n")

# fleet_rate_full.rolling, the plain fleet mean, INCLUDES every active
# vessel in that (fishery, year) cell, itself potentially among them, see
# Section 4's own comment on why that is only ever a problem if this rate is
# misapplied to a cell the vessel is ALSO present in, which Section 4 now
# guards against explicitly (a methodological review found the previous
# version of that guard was not sufficient, see Section 4's own comment).
# Floored on n.active (not n.other.active, there is no "other" being
# subtracted here) at the same BH_MIN_OTHER_ACTIVE_VESSELS threshold, so
# "thin fleet that year" reads the same way regardless of which branch a
# given cell ultimately uses.
fleet_rate_full.rolling <- fleet_fishery_year_bh.rolling %>%
  mutate(fleet.rate.mean = if_else(n.active >= BH_MIN_OTHER_ACTIVE_VESSELS,
                                    sum.rev.per.day / n.active, NA_real_)) %>%
  select(Fishery, Batch.Year, fleet.rate.mean, n.active)

cat("Fishery x year cells with a defined plain fleet-mean rate -",
    sum(is.finite(fleet_rate_full.rolling$fleet.rate.mean)), "of", nrow(fleet_rate_full.rolling), "fishery-years\n")

# ============================================================================
# 3. Fixed portfolio J*_{i,s}, whole-window avg.days_ij and vessel.ratio_ij
# ============================================================================
#
# Whole-window basis (up to 6 active years), not a 5-year lookback, see this
# script's header note. The single semi_join against
# vessel_window_eligibility.rolling below is the ONE eligibility restriction
# point in this script, matching 12b_'s own "one semi_join, no competing
# eligibility rule" convention.

lookback_windows_full.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  roll_expand_to_windows("Batch.Year", window_grid.rolling) %>%
  semi_join(vessel_window_eligibility.rolling, by = c("Vessel.ADFG.Number", "window.start"))

cat("Vessel x fishery x year x window whole-window active rows -", nrow(lookback_windows_full.rolling), "\n")

# avg.days_ij, mean fishing.days across ALL qualifying whole-window active
# years, LEFT JOIN + replace_na(0) exactly mirroring 12b_'s own avg_days_bh.rolling
# logic (a qualifying year can still have zero surviving CLEAN days).
avg_days_full.rolling <- lookback_windows_full.rolling %>%
  select(Vessel.ADFG.Number, Fishery, window.start, Batch.Year) %>%
  left_join(vessel_fishery_year_bh.rolling %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, fishing.days),
            by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  mutate(fishing.days = replace_na(fishing.days, 0)) %>%
  group_by(Vessel.ADFG.Number, Fishery, window.start) %>%
  summarise(avg.days = mean(fishing.days), .groups = "drop")

# vessel.ratio_ij, mean of (rev.per.day / fleet.rate.excl.i) across
# qualifying whole-window years where BOTH are defined, exactly mirroring
# 12b_'s own lookback_ratio_input.rolling / vessel_ratio_bh.rolling logic,
# just over the full window rather than the first 5 years.
lookback_ratio_input_full.rolling <- lookback_windows_full.rolling %>%
  select(Vessel.ADFG.Number, Fishery, window.start, Batch.Year) %>%
  inner_join(vessel_fishery_year_bh.rolling %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, rev.per.day),
             by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  filter(is.finite(rev.per.day)) %>%
  inner_join(fleet_rate_loo.rolling %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, fleet.rate.excl.i),
             by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  filter(is.finite(fleet.rate.excl.i)) %>%
  mutate(ratio.year = rev.per.day / fleet.rate.excl.i)

vessel_ratio_full.rolling <- lookback_ratio_input_full.rolling %>%
  group_by(Vessel.ADFG.Number, Fishery, window.start) %>%
  summarise(vessel.ratio = mean(ratio.year), n.ratio.years = n(), .groups = "drop")

# J*_{i,s}, the fixed portfolio, fisheries clearing the
# BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION floor on n.ratio.years (a valid ratio
# in at least 3 of the window's own active years). avg.days is guaranteed
# defined wherever vessel.ratio is (n.ratio.years's own years are a subset
# of avg.days's, exactly as 12b_'s own comment on this point notes), left_join
# used anyway as the same defensive-guard convention this pipeline uses
# throughout rather than an assumed inner_join.
J_star.rolling <- vessel_ratio_full.rolling %>%
  filter(n.ratio.years >= BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION) %>%
  left_join(avg_days_full.rolling, by = c("Vessel.ADFG.Number", "Fishery", "window.start")) %>%
  select(Vessel.ADFG.Number, window.start, Fishery, avg.days, vessel.ratio, n.ratio.years)

cat("Vessel x fishery x window whole-window cells with a defined vessel.ratio -",
    nrow(vessel_ratio_full.rolling), ", of which clear the J* floor (n.ratio.years >=",
    BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION, ") -", nrow(J_star.rolling), "\n")
cat("J* fishery rows with a missing avg.days after the left_join (should be 0) -",
    sum(is.na(J_star.rolling$avg.days)), "\n")

# ============================================================================
# 4. Prediction grid, every (vessel, J* fishery, active year) triple
# ============================================================================
#
# vessel_year_window_eligible.rolling (loaded from ch3_rolling.rdata, built
# once by 01b_) is the VESSEL-level active-year set (vessel.year.rev > 0 in
# ANY fishery that year, the same "active year" 01b_'s own H_bar/H_LR
# machinery averages/sums over), not the fishery-specific set
# lookback_windows_full.rolling above is. Cross-joining J*_{i,s} against
# THIS table (not against lookback_windows_full.rolling) is exactly what
# lets a year the vessel's realized behavior skipped fishery j still enter
# the prediction grid for j, so long as the vessel was active in SOME
# fishery that year, see this script's header note.
candidate_grid.rolling <- J_star.rolling %>%
  inner_join(
    vessel_year_window_eligible.rolling %>% select(Vessel.ADFG.Number, window.start, Batch.Year),
    by = c("Vessel.ADFG.Number", "window.start"), relationship = "many-to-many"
  )

cat("Candidate (vessel, J* fishery, active year) prediction cells -", nrow(candidate_grid.rolling), "\n")

# FIX, per a methodological review. The previous version distinguished the
# leave-one-out branch from the fallback branch purely on
# is.finite(fleet.rate.excl.i), which conflates two structurally DIFFERENT
# reasons that could be NA, "vessel has no row in fleet_rate_loo.rolling at
# all" (truly absent, skipped that fishery-year) and "vessel HAS a row but
# n.other.active < BH_MIN_OTHER_ACTIVE_VESSELS" (present, just a thin
# fleet). Because fleet_rate_full.rolling's own floor is on TOTAL n.active
# (which, when the vessel itself is present, already counts it), a cell
# with the vessel present at exactly n.active == BH_MIN_OTHER_ACTIVE_VESSELS
# (n.other.active == BH_MIN_OTHER_ACTIVE_VESSELS - 1, one short of the
# leave-one-out floor) used to silently fall through to fleet.rate.mean, a
# fleet mean that INCLUDES the focal vessel's own dollars at roughly 1/n.active
# weight, contaminating the "exogenous pass-through" rate with the vessel's
# own realized performance and biasing the behavioral gap toward
# UNDERSTATEMENT, exactly the wrong direction for this construction's stated
# purpose. Fixed by joining n.other.active through (Section 2 already keeps
# it) and gating on vessel.present.at.cell = !is.na(n.other.active), TRUE
# if and only if the vessel has ANY row in active_bh.rolling for that
# (Fishery, Batch.Year), regardless of whether the leave-one-out rate itself
# clears the floor. A present-but-thin cell now correctly resolves to NA
# (matching 12b_'s own established behavior at a thin target year) rather
# than the self-inclusive fallback.
candidate_grid.rolling <- candidate_grid.rolling %>%
  left_join(
    fleet_rate_loo.rolling %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, fleet.rate.excl.i, n.other.active),
    by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")
  ) %>%
  left_join(
    fleet_rate_full.rolling %>% select(Batch.Year, Fishery, fleet.rate.mean),
    by = c("Batch.Year", "Fishery")
  ) %>%
  mutate(
    # TRUE iff the vessel has ANY row in active_bh.rolling for this
    # (Fishery, Batch.Year), i.e. it was itself active there at all
    # (Section 2's "active" definition), independent of whether its OWN
    # leave-one-out rate clears BH_MIN_OTHER_ACTIVE_VESSELS.
    vessel.present.at.cell = !is.na(n.other.active),
    fleet.rate.applied      = if_else(vessel.present.at.cell, fleet.rate.excl.i, fleet.rate.mean),
    # Three-way diagnostic label, mutually exclusive, used only for the
    # cat() count below, not for any downstream construction (predicted.ijt
    # is built from fleet.rate.applied directly, which already encodes the
    # same logic).
    rate.branch = case_when(
      vessel.present.at.cell  & is.finite(fleet.rate.excl.i) ~ "leave-one-out (vessel present, fleet thick enough)",
      !vessel.present.at.cell & is.finite(fleet.rate.mean)   ~ "fallback, plain fleet mean (vessel absent that fishery-year)",
      vessel.present.at.cell                                  ~ "undefined, vessel present but fleet too thin (NO fallback, matches 12b_)",
      TRUE                                                    ~ "undefined, vessel absent AND fleet too thin"
    ),
    predicted.ijt = if_else(is.finite(fleet.rate.applied),
                             avg.days * fleet.rate.applied * vessel.ratio, NA_real_)
  )

cat("Rate-branch counts among candidate prediction cells (leave-one-out vs fallback vs undefined) -\n")
print(candidate_grid.rolling %>% count(rate.branch, name = "n") %>% mutate(share = round(n / sum(n), 4)))
cat("Share of candidate cells with a defined predicted.ijt overall -",
    round(mean(is.finite(candidate_grid.rolling$predicted.ijt)), 4), "\n")

# ============================================================================
# 5. Within-year normalization and the year-coverage floor
# ============================================================================
#
# fishery_weight.rolling, J*_{i,s}'s own per-fishery TYPICAL predicted
# dollar weight (mean predicted.ijt over the years it is itself defined),
# fixed per (vessel, window, fishery), used ONLY to judge how much of a
# GIVEN year's coverage is missing, never as part of Hbar^BH/H_LR^BH
# themselves (those use the year-specific predicted.ijt directly, Section 6).
fishery_weight.rolling <- candidate_grid.rolling %>%
  filter(is.finite(predicted.ijt)) %>%
  group_by(Vessel.ADFG.Number, window.start, Fishery) %>%
  summarise(weight.j = mean(predicted.ijt), .groups = "drop")

vessel_window_total_weight.rolling <- fishery_weight.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(total.weight = sum(weight.j), .groups = "drop")

year_coverage.rolling <- candidate_grid.rolling %>%
  filter(is.finite(predicted.ijt)) %>%
  left_join(fishery_weight.rolling, by = c("Vessel.ADFG.Number", "window.start", "Fishery")) %>%
  group_by(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  summarise(defined.weight = sum(weight.j), .groups = "drop") %>%
  left_join(vessel_window_total_weight.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(
    year.coverage.share = defined.weight / total.weight,
    year.survives        = year.coverage.share >= BH_MIN_YEAR_COVERAGE_SHARE
  )

cat("Candidate (vessel, window, year) cells with at least one defined prediction -", nrow(year_coverage.rolling),
    ", of which clear the", BH_MIN_YEAR_COVERAGE_SHARE, "year-coverage floor -",
    sum(year_coverage.rolling$year.survives), "\n")

# NEW, per a methodological review (this script's header note on the
# HHI-inflation-from-renormalization concern at the 0.5 boundary). Printed
# so a reader can see how close the surviving-year sample sits to the
# boundary, not just the single pass/fail count above.
cat("\n===== Year-coverage distribution among SURVIVING years (year.coverage.share) =====\n")
print(round(quantile(
  year_coverage.rolling$year.coverage.share[year_coverage.rolling$year.survives],
  probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE
), 4))
cat("Sensitivity (informational only, does NOT change the construction below), of the",
    sum(year_coverage.rolling$year.survives), "years surviving the", BH_MIN_YEAR_COVERAGE_SHARE, "floor,",
    sum(year_coverage.rolling$year.survives &
          year_coverage.rolling$year.coverage.share >= BH_MIN_YEAR_COVERAGE_SHARE_STRICT),
    "would ALSO survive a stricter", BH_MIN_YEAR_COVERAGE_SHARE_STRICT, "floor\n")

surviving_years.rolling <- year_coverage.rolling %>%
  filter(year.survives) %>%
  select(Vessel.ADFG.Number, window.start, Batch.Year)

n_surviving_years.rolling <- surviving_years.rolling %>%
  count(Vessel.ADFG.Number, window.start, name = "n.surviving.years")

# s^pred_ijt, normalized ONLY over the J*_{i,s} fisheries with a defined
# predicted.ijt in year t (per this script's spec, undefined cells are
# absent from a year's own normalization, not zero-filled into it), and
# ONLY within years that cleared the coverage floor above.
predicted_share.rolling <- candidate_grid.rolling %>%
  filter(is.finite(predicted.ijt)) %>%
  semi_join(surviving_years.rolling, by = c("Vessel.ADFG.Number", "window.start", "Batch.Year")) %>%
  group_by(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  mutate(year.total.predicted = sum(predicted.ijt), s.pred = predicted.ijt / year.total.predicted) %>%
  ungroup()

cat("Predicted-share rows entering Hbar^BH/H_LR^BH (surviving years only) -", nrow(predicted_share.rolling), "\n")

# ============================================================================
# 6. Aggregation, Hbar^BH, H_LR^BH, Phi^BH
# ============================================================================
#
# Hbar^BH, a plain mean of sum_j (s^pred_ijt)^2 over surviving years, the
# same "zero contributes zero to a sum of squares" shortcut 01b_'s own
# H_bar uses, an undefined cell that year is simply absent from that year's
# sum rather than needing an explicit zero-fill.
H_bar_bh.rolling <- predicted_share.rolling %>%
  group_by(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  summarise(hhi.year.bh = sum(s.pred^2), .groups = "drop") %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(H_bar.BH = mean(hhi.year.bh), .groups = "drop")

# H_LR^BH, the DIRECT analog of 01b_'s S_ijw / n.years.window shortcut
# (01b_ Section 4/5's own comment). S.pred.sum here plays S_ijw's role (the
# SUM of a fishery's defined predicted shares over the surviving years), and
# n.surviving.years plays n.years.window's role, the SAME shared denominator
# for every J*_{i,s} fishery in this vessel-window, not a per-fishery count
# of the years it individually cleared. A fishery with an undefined
# predicted.ijt in an otherwise-surviving year therefore contributes exactly
# 0 to its own numerator sum, identical in spirit to 01b_'s own treatment of
# an unfished (zero-share) year.
S_ijw_bh.rolling <- predicted_share.rolling %>%
  group_by(Vessel.ADFG.Number, window.start, Fishery) %>%
  summarise(S.pred.sum = sum(s.pred), .groups = "drop") %>%
  left_join(n_surviving_years.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(mean.s.pred = S.pred.sum / n.surviving.years)

H_LR_bh.rolling <- S_ijw_bh.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(H_LR.BH = sum(mean.s.pred^2), .groups = "drop")

# mean.lookback.BH / n.fisheries.J.star, the lookback-depth diagnostic,
# mirroring 12b_'s own mean.n.ratio.years.predicted philosophy exactly (see
# that script's Section 6 comment for the full reasoning, identical here).
# Every member of J*_{i,s} already individually clears
# BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION by construction, so mean.lookback.BH
# is trivially >= that floor too, it is still carried forward so a later
# figure can distinguish a J*_{i,s} barely clearing the floor (mean close to
# BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION) from one resting on much deeper
# history, exactly the question 12b_'s own diagnostic was added to answer.
mean_lookback.rolling <- J_star.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(mean.lookback.BH = mean(n.ratio.years), n.fisheries.J.star = n(), .groups = "drop")

# coverage.BH, mirroring 12b_'s own coverage field philosophy exactly, share
# of the vessel-window's REALIZED revenue (vessel_fishery_year's own
# already-deflated revenue, reused as-is, same source 12b_'s actual.revenue
# uses) earned in J*_{i,s} fisheries specifically, summed across the
# window's own active years (any fishery, vessel_year_window_eligible.rolling).
realized_revenue_window.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, revenue) %>%
  inner_join(
    vessel_year_window_eligible.rolling %>% select(Vessel.ADFG.Number, Batch.Year, window.start),
    by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many"
  )

realized_total.rolling <- realized_revenue_window.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(realized.revenue.total = sum(revenue), .groups = "drop")

realized_jstar.rolling <- realized_revenue_window.rolling %>%
  semi_join(J_star.rolling, by = c("Vessel.ADFG.Number", "window.start", "Fishery")) %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(realized.revenue.Jstar = sum(revenue), .groups = "drop")

coverage_bh.rolling <- realized_total.rolling %>%
  left_join(realized_jstar.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(
    realized.revenue.Jstar = replace_na(realized.revenue.Jstar, 0),
    coverage.BH = if_else(realized.revenue.total > 0,
                           realized.revenue.Jstar / realized.revenue.total, NA_real_)
  )

# ============================================================================
# 7. Support-matched realized Phi, the PRIMARY comparator for Phi.BH
# ============================================================================
#
# NEW, per a methodological review. See this script's header note for the
# full motivation, raw realized Phi and Phi.BH do not share a support
# whenever J*_{i,s} excludes a realized fishery, a year fails the coverage
# floor, OR (a SECOND, narrower round of review caught this) a SURVIVING
# year still has one of J*_{i,s}'s own fisheries undefined that particular
# year (Section 5's coverage floor allows up to half of J*_{i,s}'s typical
# weight to be missing and the year still survives). Phi.matched fixes this
# by recomputing a REALIZED analog of Phi.BH restricted to the IDENTICAL
# support Phi.BH itself was built on at THREE levels, not two,
# J*_{i,s} fisheries, surviving years, AND (this is what
# realized_matched_input.rolling's own semi_join against
# predicted_share.rolling, immediately below, enforces) the EXACT set of
# (fishery, year) CELLS that actually have a defined predicted.ijt that
# year, not merely "some J* fishery, some surviving year" independently. A
# realized cell for a fishery undefined in a given surviving year (present
# in J*_{i,s} overall, just not that particular year, e.g. a thin fleet
# that one year) is therefore EXCLUDED from that year's matched
# normalization, exactly mirroring which cells predicted_share.rolling
# itself normalizes over that same year (Section 5), using the exact same
# S_ijw/n shortcut Section 6 (and 01b_'s own H_LR) uses for the aggregation
# itself. Phi.gap.matched = Phi.matched - Phi.BH (built in Section 8) is the
# PRIMARY behavioral-gap comparator this whole script exists to produce, raw
# Phi/Phi.gap are kept as a SECONDARY pair in Section 8.
#
# THE BUG THIS FIXES (found by a second round of methodological review,
# fixed here, previously this semi_join was only against surviving_years.rolling,
# i.e. the YEAR axis, leaving the FISHERY axis unmatched within an otherwise-
# surviving year). Concretely, a vessel with literally ZERO reallocation
# (constant days, constant fleet rates, vessel.ratio == 1 in every J*
# fishery, fished every fishery every year) can still show a NONZERO
# Phi.gap.matched under the old restriction, because a fishery whose
# predicted.ijt happens to be undefined in one surviving year (a thin fleet
# that one year, nothing to do with the vessel's own behavior) drops out of
# the PREDICTED normalization that year but, under the old code, NOT out of
# the REALIZED-matched normalization, which stayed on the full J* set for
# every surviving year regardless. The two normalizations then disagree on
# which fisheries are even IN the denominator that year, manufacturing a
# gap from a construction artifact, not from anything the vessel did. This
# is the identical FAILURE MODE round 1 already fixed on the ACROSS-YEAR
# axis (a realized fishery Phi.BH never saw at all), just recurring one
# level down, on the WITHIN-YEAR axis (a J* fishery Phi.BH's own year-by-
# year normalization sometimes drops). The fix, and the general
# PRINCIPLE it establishes, is the same at both levels, Phi.matched's
# support must match Phi.BH's own support EXACTLY, cell for cell, not just
# in aggregate (same fisheries on average, same years on average).
#
# WHAT'S STILL NOT MATCHED (see this script's own header note's new
# "BLIND SPOT" paragraph, and coverage.BH). Phi.matched is computed ENTIRELY
# INSIDE J*_{i,s}, by construction, exactly like Phi.BH is, so REALIZED
# reallocation TOWARD a fishery outside J*_{i,s} entirely (the vessel starts
# fishing something new, never seen enough to enter the fixed portfolio) is
# invisible to Phi.matched, same as it is to Phi.BH, this is not a bug, it
# is what "support-matched" means, but a reader comparing Phi.matched to
# Phi.BH should not mistake agreement between the two for "this vessel never
# reallocated," only for "this vessel never reallocated WITHIN J*_{i,s}."
# coverage.BH (Section 6) already quantifies how much of the vessel-window's
# REALIZED revenue sits outside J*_{i,s} altogether, a low coverage.BH is
# the signal that this blind spot could be masking real reallocation, read
# the two together.
#
# EDGE CASE, unchanged by the fix above. A (fishery, surviving-year) cell
# that DOES have a defined predicted.ijt can still have ZERO realized
# revenue there (the vessel's REALIZED behavior skipped it that year, while
# the fixed-effort counterfactual did not), in which case that YEAR's
# overall realized-matched total can be zero if every one of THAT year's
# defined-cell fisheries were skipped, mathematically undefined (0/0), not
# merely thin. Such years are excluded from the matched construction
# specifically, via matched_years.rolling below, so
# n.realized.matched.years CAN be smaller than n.surviving.years (this is
# itself informative, a large gap between the two counts means the
# vessel's REALIZED activity diverged from its J*-implied activity even
# within years Phi.BH itself treats as covered), mirroring this script's own
# "NA not 0" convention for a genuinely undefined cell rather than
# fabricating a degenerate share vector for it.

realized_matched_input.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, revenue) %>%
  inner_join(J_star.rolling %>% select(Vessel.ADFG.Number, window.start, Fishery),
             by = c("Vessel.ADFG.Number", "Fishery"), relationship = "many-to-many") %>%
  # FIX (second round of methodological review), matches on the EXACT
  # (vessel, window, year, fishery) cell, not just "some J* fishery in a
  # surviving year" independently, see this section's own header note above
  # for the full bug description. predicted_share.rolling (Section 5) is
  # already restricted to is.finite(predicted.ijt) cells within surviving
  # years, so this single semi_join enforces both the year-level AND the
  # fishery-level match simultaneously, surviving_years.rolling alone (the
  # OLD restriction) is no longer referenced anywhere in this section.
  semi_join(predicted_share.rolling,
            by = c("Vessel.ADFG.Number", "window.start", "Batch.Year", "Fishery"))

realized_matched_year_total.rolling <- realized_matched_input.rolling %>%
  group_by(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  summarise(year.total.realized.matched = sum(revenue), .groups = "drop")

matched_years.rolling <- realized_matched_year_total.rolling %>%
  filter(year.total.realized.matched > 0) %>%
  select(Vessel.ADFG.Number, window.start, Batch.Year)

n_matched_years.rolling <- matched_years.rolling %>%
  count(Vessel.ADFG.Number, window.start, name = "n.realized.matched.years")

cat("\n===== Support-matched realized Phi construction =====\n")
cat("Vessel-window x surviving-year cells with SOME realized revenue in a J* fishery -",
    nrow(realized_matched_year_total.rolling), ", of which with POSITIVE J*-restricted realized revenue",
    "(well-defined matched share that year) -", nrow(matched_years.rolling), "\n")

realized_matched_share.rolling <- realized_matched_input.rolling %>%
  semi_join(matched_years.rolling, by = c("Vessel.ADFG.Number", "window.start", "Batch.Year")) %>%
  left_join(realized_matched_year_total.rolling, by = c("Vessel.ADFG.Number", "window.start", "Batch.Year")) %>%
  mutate(share.matched = revenue / year.total.realized.matched)

H_bar_matched.rolling <- realized_matched_share.rolling %>%
  group_by(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  summarise(hhi.year.matched = sum(share.matched^2), .groups = "drop") %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(H_bar.matched = mean(hhi.year.matched), .groups = "drop")

S_ijw_matched.rolling <- realized_matched_share.rolling %>%
  group_by(Vessel.ADFG.Number, window.start, Fishery) %>%
  summarise(S.matched.sum = sum(share.matched), .groups = "drop") %>%
  left_join(n_matched_years.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(mean.share.matched = S.matched.sum / n.realized.matched.years)

H_LR_matched.rolling <- S_ijw_matched.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(H_LR.matched = sum(mean.share.matched^2), .groups = "drop")

cat("Vessel-windows with a computable Phi.matched (n.realized.matched.years >= 1) -",
    nrow(n_matched_years.rolling), "\n")

# ============================================================================
# 8. Funnel and vessel-window assembly
# ============================================================================
#
# "Attempted" is every eligible vessel-window (vessel_window_eligibility.rolling,
# the pipeline's one eligibility rule, already semi_joined into every object
# above), not restricted further, unlike 12b_'s own "attempted" universe
# (which additionally requires the vessel to have fished SOMETHING at
# window.end), Phi^BH has no held-out target year to require activity in.
funnel_base.rolling <- vessel_window_eligibility.rolling %>%
  select(Vessel.ADFG.Number, window.start) %>%
  left_join(mean_lookback.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(n.fisheries.J.star = replace_na(n.fisheries.J.star, 0L)) %>%
  left_join(n_surviving_years.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(n.surviving.years = replace_na(n.surviving.years, 0L))

phi_bh_funnel <- tibble(step = character(), n.obs = integer(), n.vessels = integer())
phi_bh_funnel <- roll_attrition_row(phi_bh_funnel, "Attempted (eligible vessel-windows)", funnel_base.rolling)
phi_bh_funnel <- roll_attrition_row(
  phi_bh_funnel, paste0("At least one J* fishery (n.fisheries.J.star >= 1, clears BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION = ",
                         BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION, ")"),
  funnel_base.rolling %>% filter(n.fisheries.J.star > 0)
)
phi_bh_funnel <- roll_attrition_row(
  phi_bh_funnel, paste0("At least one surviving year (n.surviving.years >= 1, clears the ",
                         BH_MIN_YEAR_COVERAGE_SHARE, " year-coverage floor), Phi.BH computed"),
  funnel_base.rolling %>% filter(n.fisheries.J.star > 0, n.surviving.years > 0)
)
# NEW, per a methodological review. NOT a further cumulative exclusion
# stage (it is a SUBSET of the row directly above it, reported for
# information, so this row's own n.obs is not meant to be read as "what
# survives after this filter" the way the three rows above it are).
# n.fisheries.J.star == 1 forces Phi.BH == 0 MECHANICALLY (a single-fishery
# portfolio has a trivial share of 1.0 in every surviving year, zero
# variance by construction, this script's own synthetic test confirms it),
# which is not itself a finding about reallocation, just an artifact of a
# thin J*. 13c_'s own sample filter (n.fisheries.J.star >= 2) is where this
# actually gets excluded from the behavioral-gap analysis, this row exists
# so the SIZE of that exclusion is visible here too, at the source.
phi_bh_funnel <- roll_attrition_row(
  phi_bh_funnel,
  "Of the row above, single-fishery J* only (n.fisheries.J.star == 1, Phi.BH mechanically 0, INFORMATIONAL, not a further cumulative exclusion, see 13c_'s own n.fisheries.J.star >= 2 sample filter)",
  funnel_base.rolling %>% filter(n.fisheries.J.star == 1, n.surviving.years > 0)
)

cat("\n===== Predicted BH-effort Phi funnel =====\n")
print(phi_bh_funnel)

# H_bar.matched/H_LR.matched/n.realized.matched.years are LEFT-joined (a
# vessel-window can have J* non-empty and n.surviving.years > 0 while still
# having n.realized.matched.years == 0, the edge case Section 7's own
# comment describes), replace_na(0L) on the count column only, Phi.matched
# itself is correctly left NA via H_bar.matched/H_LR.matched both being NA
# when there is no join match, is.finite() propagates through Phi.gap.matched
# automatically, no separate guard needed.
predicted_bh_phi_vessel_window.rolling <- funnel_base.rolling %>%
  left_join(H_bar_bh.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  left_join(H_LR_bh.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(Phi.BH = H_bar.BH - H_LR.BH) %>%
  left_join(coverage_bh.rolling %>% select(Vessel.ADFG.Number, window.start, coverage.BH),
            by = c("Vessel.ADFG.Number", "window.start")) %>%
  left_join(H_bar_matched.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  left_join(H_LR_matched.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  left_join(n_matched_years.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(
    n.realized.matched.years = replace_na(n.realized.matched.years, 0L),
    Phi.matched              = H_bar.matched - H_LR.matched,
    # PRIMARY behavioral gap, per a methodological review, see Section 7's
    # own header note.
    Phi.gap.matched           = Phi.matched - Phi.BH
  ) %>%
  left_join(
    vessel_window_summary.rolling %>%
      select(Vessel.ADFG.Number, window.start, H_bar, H_LR, Phi, is.specialist.window,
             n.years.window, n.fisheries.fished.window),
    by = c("Vessel.ADFG.Number", "window.start")
  ) %>%
  # SECONDARY behavioral gap, the raw/un-matched version, kept for
  # comparison, see this script's header note on why it is no longer the
  # primary comparator.
  mutate(Phi.gap = Phi - Phi.BH) %>%
  select(
    Vessel.ADFG.Number, window.start, n.fisheries.J.star, mean.lookback.BH, n.surviving.years,
    H_bar.BH, H_LR.BH, Phi.BH, coverage.BH,
    H_bar.matched, H_LR.matched, Phi.matched, n.realized.matched.years, Phi.gap.matched,
    H_bar, H_LR, Phi, Phi.gap, n.years.window, n.fisheries.fished.window, is.specialist.window
  )

cat("\n===== predicted_bh_phi_vessel_window.rolling summary =====\n")
cat("Vessel-window rows -", nrow(predicted_bh_phi_vessel_window.rolling), ", distinct vessels -",
    n_distinct(predicted_bh_phi_vessel_window.rolling$Vessel.ADFG.Number), "\n")
cat("Vessel-windows with a defined Phi.BH -",
    sum(is.finite(predicted_bh_phi_vessel_window.rolling$Phi.BH)), "of",
    nrow(predicted_bh_phi_vessel_window.rolling), ", of which with a defined Phi.matched too -",
    sum(is.finite(predicted_bh_phi_vessel_window.rolling$Phi.matched)), "\n")
cat("Mean coverage.BH among vessel-windows with a defined value -",
    round(mean(predicted_bh_phi_vessel_window.rolling$coverage.BH, na.rm = TRUE), 4), ", median -",
    round(median(predicted_bh_phi_vessel_window.rolling$coverage.BH, na.rm = TRUE), 4), "\n")
cat("Mean lookback depth across J*, mean.lookback.BH -",
    round(mean(predicted_bh_phi_vessel_window.rolling$mean.lookback.BH, na.rm = TRUE), 3), "\n")
cat("PRIMARY behavioral gap (support-matched), Phi.gap.matched = Phi.matched - Phi.BH, mean -",
    round(mean(predicted_bh_phi_vessel_window.rolling$Phi.gap.matched, na.rm = TRUE), 4), ", median -",
    round(median(predicted_bh_phi_vessel_window.rolling$Phi.gap.matched, na.rm = TRUE), 4), "\n")
cat("SECONDARY behavioral gap (raw, mismatched support), Phi.gap = Phi - Phi.BH, mean -",
    round(mean(predicted_bh_phi_vessel_window.rolling$Phi.gap, na.rm = TRUE), 4), ", median -",
    round(median(predicted_bh_phi_vessel_window.rolling$Phi.gap, na.rm = TRUE), 4),
    " (compare to the PRIMARY figure directly above, a large gap between the two is itself evidence of how much",
    "the support mismatch alone was inflating the naive comparison)\n")
cat("Support divergence, among vessel-windows with a defined Phi.BH, share where n.years.window !=",
    "n.surviving.years -",
    round(mean(predicted_bh_phi_vessel_window.rolling$n.years.window[is.finite(predicted_bh_phi_vessel_window.rolling$Phi.BH)] !=
                 predicted_bh_phi_vessel_window.rolling$n.surviving.years[is.finite(predicted_bh_phi_vessel_window.rolling$Phi.BH)]), 4),
    ", share where n.fisheries.fished.window != n.fisheries.J.star -",
    round(mean(predicted_bh_phi_vessel_window.rolling$n.fisheries.fished.window[is.finite(predicted_bh_phi_vessel_window.rolling$Phi.BH)] !=
                 predicted_bh_phi_vessel_window.rolling$n.fisheries.J.star[is.finite(predicted_bh_phi_vessel_window.rolling$Phi.BH)]), 4), "\n")
cat("Phi.BH never negative (should be TRUE, it is a sum of variances exactly as Phi itself is, see 01b_) -",
    all(predicted_bh_phi_vessel_window.rolling$Phi.BH >= -1e-8, na.rm = TRUE),
    ", Phi.matched never negative -",
    all(predicted_bh_phi_vessel_window.rolling$Phi.matched >= -1e-8, na.rm = TRUE), "\n")

# ============================================================================
# 9. Save
# ============================================================================

predicted_bh_phi_path <- file.path(intermediate_dir, "ch3_predicted_bh_phi.rdata")
save(predicted_bh_phi_vessel_window.rolling, file = predicted_bh_phi_path)
cat("Saved predicted BH-effort Phi objects to", predicted_bh_phi_path, "\n")
