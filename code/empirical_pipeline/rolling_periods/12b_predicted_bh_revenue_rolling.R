# Chapter 3 empirical pipeline, rolling-window "predicted buy-and-hold
# revenue" benchmark, EFFORT (days fished) version
#
# No baseline (lifetime) counterpart, no "12_" script exists or is planned.
# The whole construction is defined in terms of a rolling window's own
# 5-lookback-years-plus-1-target-year split, there is no lifetime analogue
# of "the 6th year" to hold out, so this lives only under rolling_periods/.
#
# CONCEPT. The existing passive/buy-and-hold benchmark elsewhere in this
# pipeline (05_/05b_'s passive_series_window.rolling) fixes a vessel's
# REVENUE SHARES and asks what revenue the fleet's own average would have
# handed that fixed portfolio. This script asks a different question, fixing
# a vessel's EFFORT (days fished per fishery) instead of its revenue shares,
# and asking what revenue that fixed effort would have earned at OTHER
# vessels' going per-day rate in the fishery it was actually fished in. A
# vessel-fishery-window's PREDICTED revenue for the window's 6th (held-out)
# year is
#
#   predicted_ijw = avg.days_ij * fleet_rate_excl_i(Fishery=j, Batch.Year=window.end) * vessel.ratio_ij
#
# where avg.days_ij is vessel i's own mean days fished in fishery j across
# the window's first 5 years (the lookback years), fleet_rate_excl_i is the
# leave-one-out fleet mean revenue-per-day in (j, window.end), and
# vessel.ratio_ij is vessel i's own mean (across the same 5 lookback years)
# ratio of its per-day rate to the fleet's leave-one-out per-day rate that
# year. See Section 3 below for exactly which years qualify and why avg.days
# and vessel.ratio can be built over different subsets of those years.
#
# There is exactly ONE lookback/target split used throughout, a window
# w = [window.start, window.end] with window.end = window.start + 5. Years
# window.start .. window.start + 4 (5 years) are the lookback history,
# window.end (the 6th year) is the held-out target being predicted. This is
# NOT the same thing as 07b_/08b_'s ADJACENT non-overlapping classifier
# window (their preceding window c(s) = [s - 6, s - 1]), the lookback here
# is the window's own first 5 years, nothing outside the window itself is
# ever read as history.
#
# ELIGIBILITY. Restricted to (Vessel.ADFG.Number, window.start) pairs
# already in vessel_window_eligibility.rolling (the ROLL_MIN_ACTIVE_YEARS =
# 4-of-6 floor, built once in 01b_build_rolling_panel.R), via a single
# semi_join at the target_fisheries.rolling step below. No competing
# eligibility rule is built here, and ROLL_MIN_LOOKBACK_YEARS (3, the
# 07b_/08b_ classifier floor) is deliberately NOT applied either, this
# script's only lookback floor is n.active.years > 0 (any positive activity
# at all counts, see Section 3).
#
# TWO DIFFERENT REVENUE SOURCES, used for two different purposes, on
# purpose, not an inconsistency.
#   (1) The per-day rate machinery (Sections 1-4, rev.per.day and every
#       fleet/vessel rate and ratio built on it) uses this script's own
#       cleaned ticket-level revenue (Section 1), because a rate needs its
#       own numerator and denominator (days) built from the identical set of
#       surviving trips, see Section 1's own comment on why. That cleaned
#       revenue is built fresh from a raw catch_data_temp reload, so it
#       starts out NOMINAL, and is deflated in Section 1 via the same
#       load_deflator()/deflate() machinery 01_build_panel.R itself uses,
#       rebuilt here because the deflator object is not saved into
#       ch3_panel.rdata, only the already-deflated vessel_fishery_year$revenue
#       column is. Without this step predicted.total (avg.days x
#       fleet_rate(window.end) x vessel.ratio) would inherit window.end's
#       own nominal price level while actual.matching.total/actual.full.total
#       (below) sit on vessel_fishery_year's real, MAX_YEAR basis, a
#       systematic wedge growing with the distance between window.start and
#       MAX_YEAR that would look like a secular trend in the metric having
#       nothing to do with the mechanism being tested. vessel.ratio itself
#       was always deflator-invariant regardless (a same-year, same-fishery
#       ratio of vessel to fleet rate, any single-year deflator cancels out
#       of it exactly), the wedge lived entirely in fleet.rate.excl.i.target's
#       absolute dollar level, which is why deflating revenue.clean once,
#       upstream of everything, is enough to fix it everywhere it is used.
#   (2) Whether a lookback year "qualifies" at all (n.active.years, Section
#       3) and the final predicted-vs-actual comparison (Section 6) both
#       reuse vessel_fishery_year's own held/fished/revenue columns AS-IS
#       (already deflated there by 01_build_panel.R, never recomputed here),
#       the pipeline's existing established definition of "this vessel had
#       positive revenue in this fishery-year" and of "actual revenue,"
#       rather than a third, competing definition invented just for this
#       script. With (1) now deflated too, predicted.total and
#       actual.matching.total/actual.full.total sit on the SAME real,
#       MAX_YEAR dollar basis (both trace back to the same load_deflator()
#       CSV), a like-for-like comparison rather than an accepted mismatch.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year, read-only)
# and intermediate data/ch3_rolling.rdata (window_grid.rolling,
# vessel_window_eligibility.rolling, read-only, both built by
# 01b_build_rolling_panel.R), plus a fresh reload of
# intermediate data/catch_data_temp.rdata for trip/day grain, duplicating
# rather than sharing the cleaning steps, this pipeline's own established
# convention (06_, 08b_, 09b_ all reload and re-clean catch_data_temp
# independently rather than share one cleaned copy).
#
# Saves predicted_bh_vessel_window.rolling (one row per Vessel.ADFG.Number x
# window.start, the main deliverable, includes predicted.total,
# actual.matching.total, actual.full.total, coverage, and the
# mean.n.active.years.predicted/mean.n.ratio.years.predicted lookback-depth
# diagnostics added per a methodological review of
# 13b_predicted_bh_revenue_figures_rolling.R, see Section 6 below) and
# predicted_bh_detail.rolling (one row per Vessel.ADFG.Number x Fishery x
# window.start attempted, the diagnostic detail table
# predicted_bh_vessel_window.rolling is aggregated from) to
# intermediate data/ch3_predicted_bh.rdata.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

if (!exists("vessel_fishery_year") || !exists("MAX_YEAR")) load(panel_path)

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
if (!exists("window_grid.rolling") || !exists("vessel_window_eligibility.rolling")) load(rolling_panel_path)

# vessel_fishery_year$revenue was already deflated once, by 01_build_panel.R's
# own deflate(vessel_fishery_year, "revenue", deflator) call, but the
# deflator object itself is not saved into ch3_panel.rdata, only the
# already-deflated revenue column is. Rebuilt here (load_deflator(), defined
# in 00_setup.R) because Section 1 needs to apply the identical deflator to
# this script's OWN revenue.clean, which is built fresh from a raw
# catch_data_temp reload and so starts out nominal. A no-op (returns its
# input unchanged) if Chpt3/data/cpi_deflator.csv is absent, in which case
# vessel_fishery_year$revenue is nominal too, so the two stay consistent
# with each other either way.
deflator <- load_deflator()

# ============================================================================
# Local constants
# ============================================================================
#
# BH_ prefix (this script's own local scope, not a shared 00b_ constant),
# matching this pipeline's established pattern of locally-scoped thresholds
# (MIN_LANDINGS in 06_, MIN_FISHERY_WEEKS_ROLLING in 09b_).

# A trip lasting zero or negative days is a Date.Fishing.Began/Date.Landed
# data error, and a trip longer than a year is the same still-unresolved
# data issue prod_reg.R's own comment flags directly ("There are still some
# trips with trip lengths that are too long, e.g., trips ~= 50 years, They
# are left in for now"), left in there but excluded here since a single
# such trip would otherwise dominate a vessel-fishery-year's whole days
# total and badly distort rev.per.day.
BH_MAX_TRIP_DURATION_DAYS <- 365

# Leave-one-out floor, checked AFTER excluding the focal vessel, not before,
# a fishery-year with exactly 5 total active vessels gives every vessel in
# it only 4 "others," which is why this is a per-vessel, per-fishery-year
# check rather than a flat "fishery-year needs >= 6 active vessels" filter.
BH_MIN_OTHER_ACTIVE_VESSELS <- 5

cat("12b_predicted_bh_revenue_rolling.R loaded, BH_MAX_TRIP_DURATION_DAYS =",
    BH_MAX_TRIP_DURATION_DAYS, ", BH_MIN_OTHER_ACTIVE_VESSELS =", BH_MIN_OTHER_ACTIVE_VESSELS, "\n")

# ============================================================================
# 1. Day construction, per vessel x Batch.Year x Fishery
# ============================================================================
#
# Same vessel-ID fix / BAD_VESSEL_IDS drop / MIN_YEAR-MAX_YEAR window /
# Fishery derivation as every other trip-level reload in this pipeline
# (06_, 08b_, 09b_), duplicated here rather than shared for the same reason
# given there. get.trip() (code/myfunctions.R, already sourced via
# 00_setup.R) is what actually assigns trip.id, grouping on
# Vessel.ADFG.Number x Date.Landed ACROSS every Fishery a vessel touched
# that day, which is exactly what makes the cross-fishery day double-count
# below possible, and it zero-fills CFEC.Value..Detail. internally before
# returning, no separate NA-fill step needed here.

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
  filter(Fishery != "")

cat("Ticket rows entering the BH-effort day/revenue construction -", nrow(catch_data_temp), "\n")

catch_data_temp <- get.trip(catch_data_temp)
catch_data_temp <- catch_data_temp %>% mutate(trip.duration = as.numeric(trip.length) + 1)

# Canonicalize to at most ONE trip.duration per (trip.id, Fishery) BEFORE any
# exclusion filter or day summation. get.trip() (myfunctions.R) assigns
# trip.id from (Vessel.ADFG.Number, Date.Landed) alone, but trip.length is
# computed ROW-WISE from each ticket line's own Date.Fishing.Began, so two
# ticket lines landed the same day in the same fishery can carry two
# DIFFERENT trip.duration values, both of which would survive a plain
# distinct(trip.id, trip.duration) and both get summed, inflating
# fishing.days for that one trip. Confirmed concretely, two lines of one
# trip with Date.Fishing.Began of 07-05 and 07-08 respectively produced
# fishing.days = 9 instead of the correct 6 before this fix. Canonicalizing
# PER (trip.id, Fishery), not per trip.id alone, keeps the intended
# cross-fishery day double-count intact, a trip spanning two fisheries still
# gets one canonical duration PER fishery, each independently taking the max
# of that fishery's own ticket-line durations. The bias this fixes was
# one-directional (days inflated, rate deflated) and scaled with tickets
# filed per landing day, which correlates with vessel size and portfolio
# breadth, i.e. with exactly the variation this benchmark exists to
# characterize, not something safe to leave as noise.
trip_duration_raw.rolling <- catch_data_temp %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery, trip.id, trip.duration)

# Diagnostic in the style of 11_switch_event_weights.R's own duplicate-key
# check, so this is visible on real data rather than only asserted above.
n_multi_duration_cells.rolling <- trip_duration_raw.rolling %>%
  count(Vessel.ADFG.Number, Batch.Year, Fishery, trip.id) %>%
  filter(n > 1) %>%
  nrow()
n_trip_fishery_cells.rolling <- trip_duration_raw.rolling %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery, trip.id) %>%
  nrow()
cat("Trip x fishery cells carrying more than one distinct raw trip.duration value",
    "(the inconsistent-Date.Fishing.Began issue the canonicalization step below corrects) -",
    n_multi_duration_cells.rolling, "of", n_trip_fishery_cells.rolling, "trip x fishery cells\n")

# max(), not mean() or first(), a trip's true duration is at least as long
# as its longest recorded leg, so the max of several inconsistent
# Date.Fishing.Began-implied durations is the closest available proxy to
# the real trip length. max_or_na.rolling avoids max()'s own -Inf-with-a-
# warning behavior on an all-NA group, an all-NA canonical duration is
# still correctly treated as unusable by the is.na() branch of the
# exclusion filter just below, without a warning fired per group.
max_or_na.rolling <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

trip_duration_canonical.rolling <- trip_duration_raw.rolling %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery, trip.id) %>%
  summarise(trip.duration = max_or_na.rolling(trip.duration), .groups = "drop")

# Exclusion applied to the CANONICAL duration (post-collapse), not the raw
# per-row values, so the exclusion rule and the summation rule below operate
# on the identical object. Exclusion is at the whole-TRIP level (trip.id),
# not trip x fishery, if any one of a trip's fishery legs has an unusable
# canonical duration the ENTIRE trip (every fishery it touched) is dropped,
# matching the original spec's framing of a trip-level data error, not a
# fishery-line-level one.
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

cat("Trips excluded for an unusable canonical trip.duration (<= 0, NA, or >", BH_MAX_TRIP_DURATION_DAYS,
    "days, the same still-unresolved ~50-year-trip issue prod_reg.R flags) -",
    length(bad_trip_ids.rolling), "of", n_trips_total.rolling, "trips (",
    round(100 * length(bad_trip_ids.rolling) / n_trips_total.rolling, 4), "% ), representing",
    round(100 * revenue_excluded_tickets.rolling / revenue_all_tickets.rolling, 4),
    "% of ticket-line revenue in this universe. A whole trip.id is dropped if ANY of its",
    "lines (in any fishery it touched) is unusable, not just the specific fishery leg that failed\n")

# Excluded entirely, both from the day count AND from the revenue this
# script computes for itself below, so rev.per.day never mixes a
# denominator built from the clean trip set with a numerator that still
# carries an excluded trip's dollars.
catch_data_clean.rolling <- catch_data_temp %>% filter(!(trip.id %in% bad_trip_ids.rolling))

# fishing.days, summed from the CANONICAL (not raw) per-(trip.id, Fishery)
# duration, restricted to surviving (non-excluded) trip.ids. Summing
# trip_duration_raw directly, even after a plain distinct(), would still
# double-sum any trip x fishery cell carrying more than one raw duration
# value, which is exactly the overcount this canonicalization step exists
# to prevent.
vessel_fishery_year_days.rolling <- trip_duration_canonical.rolling %>%
  filter(!(trip.id %in% bad_trip_ids.rolling)) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  summarise(fishing.days = sum(trip.duration, na.rm = TRUE), .groups = "drop")

# revenue.clean, plain sum over every surviving ticket-line row, no trip.id
# deduplication needed or wanted here, CFEC.Value..Detail. is already
# fishery-specific per row (unlike trip.duration, which get.trip()/this
# script's own canonicalization above both treat as a per-trip constant),
# so summing it directly does not overcount the way summing trip.duration
# directly would.
vessel_fishery_year_revenue_clean.rolling <- catch_data_clean.rolling %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  summarise(revenue.clean = sum(CFEC.Value..Detail., na.rm = TRUE), .groups = "drop")

# full_join (not inner_join) purely as a defensive guard, every group with
# clean revenue should already have a matching clean days row by
# construction (both are built from the same surviving trip.ids), the
# replace_na(0) below only ever fires if that invariant is somehow broken.
#
# deflate("revenue.clean", deflator) applied BEFORE rev.per.day is computed,
# putting this script's own revenue figure onto the identical real,
# MAX_YEAR basis vessel_fishery_year$revenue already sits on (see this
# script's header note), a no-op if deflator is NULL (no cpi_deflator.csv).
vessel_fishery_year_bh.rolling <- vessel_fishery_year_days.rolling %>%
  full_join(vessel_fishery_year_revenue_clean.rolling,
            by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  mutate(
    fishing.days  = replace_na(fishing.days, 0),
    revenue.clean = replace_na(revenue.clean, 0)
  ) %>%
  deflate("revenue.clean", deflator) %>%
  mutate(
    # Guarded even though fishing.days == 0 should be structurally
    # impossible here (any surviving trip has trip.duration >= 1 by
    # construction), a single bad join should produce a dropped rate, not a
    # divide-by-zero.
    rev.per.day = if_else(fishing.days > 0, revenue.clean / fishing.days, NA_real_)
  )

# Same defensive check as 01_build_panel.R's own missing-CPI-year warning
# (Section 4 there), run here against THIS script's own Batch.Year
# coverage rather than re-trusted from vessel_fishery_year's version of it.
# If cpi_deflator.csv exists but is missing a row for a year this script's
# own cleaned universe covers, deflate()'s join leaves revenue.clean NA for
# every row in that year, which na.rm = TRUE then silently turns into 0
# downstream, here that would thin the active/fleet-rate universe for that
# year rather than read as an obvious fleet-wide collapse the way 01_'s own
# version of this check is framed around.
if (!is.null(deflator)) {
  years_missing_deflator.rolling <- setdiff(unique(vessel_fishery_year_bh.rolling$Batch.Year), deflator$Year)
  if (length(years_missing_deflator.rolling) > 0) {
    warning(
      "cpi_deflator.csv has no row for Batch.Year ",
      paste(sort(years_missing_deflator.rolling), collapse = ", "),
      ", revenue.clean silently drops to NA/0 for that year via na.rm downstream in this script's own ",
      "rev.per.day/fleet-rate machinery, thinning the active-vessel universe for that year rather than ",
      "reading as an obvious fleet-wide collapse"
    )
  }
}

cat("Vessel x fishery x year cells with cleaned days/revenue -", nrow(vessel_fishery_year_bh.rolling),
    ", with fishing.days == 0 despite a surviving revenue row (should be ~0) -",
    sum(vessel_fishery_year_bh.rolling$fishing.days == 0 & vessel_fishery_year_bh.rolling$revenue.clean > 0), "\n")

# ============================================================================
# 2. Fleet-side leave-one-out per-day rate
# ============================================================================
#
# Built ONCE here and reused for BOTH the lookback-year vessel.ratio
# (Section 3) and the window.end target rate (Section 4), so "leave-one-out"
# means the same thing everywhere it is used in this script.
#
# "Active" here is operationalized entirely within THIS script's own cleaned
# universe (revenue.clean > 0 and fishing.days > 0, equivalently rev.per.day
# is defined and positive), NOT vessel_fishery_year$fished. A vessel whose
# only ticket dollars in a fishery-year came from an excluded anomalous trip
# has no computable rev.per.day and cannot sensibly be averaged into a
# per-day rate, no matter what the panel's own (differently-cleaned) fished
# flag says for that vessel-fishery-year. This is the same numerator/
# denominator consistency Section 1 already enforces for rev.per.day itself,
# extended to who counts as "other active vessels" in the average of it.
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
    "active cells (the rest fall below", BH_MIN_OTHER_ACTIVE_VESSELS, "OTHER active vessels that fishery-year)\n")

# ============================================================================
# 3. Vessel's own lookback inputs, per (Vessel.ADFG.Number, Fishery,
#    window.start), built ONLY from the window's own first 5 years
# ============================================================================
#
# roll_expand_to_windows() (00b_rolling_periods.R, generic in year_col) maps
# every qualifying vessel-fishery-year onto every window that could contain
# it as one of its 6 calendar years, offsets 0..(ROLL_WINDOW_WIDTH - 1). The
# filter(Batch.Year < window.end) immediately below is what turns that
# generic 6-year mapping into a LOOKBACK-only mapping, dropping exactly the
# 6th year (window.end) from every window, window.end is never allowed to
# feed a historical average, only to be the target in Sections 4 and 6.
#
# "Qualifies" reuses vessel_fishery_year's own fished flag (positive revenue
# there, already deflated, reused as-is per the header note), not this
# script's own revenue.clean, any positive amount counts, no minimum-
# activity floor, by design (a single fishing day in some lookback year
# should count, not be smoothed away).
lookback_windows.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  roll_expand_to_windows("Batch.Year", window_grid.rolling) %>%
  filter(Batch.Year < window.end)

cat("Vessel x fishery x year x window lookback rows (year in the window's own first",
    ROLL_WINDOW_WIDTH - 1, "years) -", nrow(lookback_windows.rolling), "\n")

# n.active.years, stored as its own diagnostic column, NOT used as a filter
# here (kept for possible filtering later per the spec), a vessel-fishery-
# window proceeds through avg.days/vessel.ratio below regardless of how low
# n.active.years is, as long as it is at least 1.
n_active_years_bh.rolling <- lookback_windows.rolling %>%
  count(Vessel.ADFG.Number, Fishery, window.start, name = "n.active.years")

# avg.days, mean fishing.days across ALL qualifying lookback years,
# regardless of whether that year's fleet rate (or even its own rev.per.day)
# was definable, this average only ever needs the vessel's own days. LEFT
# JOIN + replace_na(0), not inner_join, a qualifying year (positive
# vessel_fishery_year revenue) can still have zero surviving CLEAN days if
# every one of its tickets in that fishery came from an excluded anomalous
# trip, that year must count as 0 days in this average, not drop out of it.
avg_days_bh.rolling <- lookback_windows.rolling %>%
  select(Vessel.ADFG.Number, Fishery, window.start, Batch.Year) %>%
  left_join(vessel_fishery_year_bh.rolling %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, fishing.days),
            by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  mutate(fishing.days = replace_na(fishing.days, 0)) %>%
  group_by(Vessel.ADFG.Number, Fishery, window.start) %>%
  summarise(avg.days = mean(fishing.days), .groups = "drop")

# vessel.ratio, mean of (rev.per.day / fleet.rate.excl.i) ONLY across
# qualifying lookback years where BOTH this script's own rev.per.day AND the
# leave-one-out fleet rate are defined that year, the double inner_join +
# filter below is what drops a qualifying year for either reason, exactly
# the "lookback history but no valid ratio" case Section 5's funnel reports
# on. This can and often will be a smaller set of years than avg.days above
# draws on, the two are deliberately not forced onto the same subset.
lookback_ratio_input.rolling <- lookback_windows.rolling %>%
  select(Vessel.ADFG.Number, Fishery, window.start, Batch.Year) %>%
  inner_join(vessel_fishery_year_bh.rolling %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, rev.per.day),
             by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  filter(is.finite(rev.per.day)) %>%
  inner_join(fleet_rate_loo.rolling %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, fleet.rate.excl.i),
             by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  filter(is.finite(fleet.rate.excl.i)) %>%
  mutate(ratio.year = rev.per.day / fleet.rate.excl.i)

vessel_ratio_bh.rolling <- lookback_ratio_input.rolling %>%
  group_by(Vessel.ADFG.Number, Fishery, window.start) %>%
  summarise(vessel.ratio = mean(ratio.year), n.ratio.years = n(), .groups = "drop")

vessel_lookback_bh.rolling <- n_active_years_bh.rolling %>%
  full_join(avg_days_bh.rolling, by = c("Vessel.ADFG.Number", "Fishery", "window.start")) %>%
  full_join(vessel_ratio_bh.rolling, by = c("Vessel.ADFG.Number", "Fishery", "window.start"))

cat("Vessel x fishery x window lookback cells -", nrow(vessel_lookback_bh.rolling),
    ", with a defined vessel.ratio -", sum(is.finite(vessel_lookback_bh.rolling$vessel.ratio)), "\n")

# ============================================================================
# 4. Target-year (window.end, the 6th year) leave-one-out fleet rate
# ============================================================================
#
# Reuses fleet_rate_loo.rolling from Section 2 unchanged, joined here purely
# to relabel window.end (Batch.Year) back onto its own window.start via
# window_grid.rolling, no separate leave-one-out formula is recomputed. A
# vessel only gets a row here for a (Fishery, window.start) if IT ITSELF was
# "active" (Section 2's definition) in that fishery at window.end, which is
# exactly what makes the join in Section 5 correctly produce an absent row
# (not just an NA rate) for a vessel that fished a fishery at window.end per
# vessel_fishery_year but had no surviving clean days/revenue there.
#
# NOT pre-filtered to is.finite(fleet.rate.excl.i), unlike the previous
# version of this section, n.other.active.target is carried through even
# when the rate itself is undefined. This is what lets Section 5's funnel
# distinguish "vessel itself had no clean window-end days/revenue" (absent
# from this table entirely) from "vessel was active but the fishery-year's
# OTHER vessels were too thin" (present here, finite n.other.active.target,
# NA fleet.rate.excl.i.target), a methodological review flagged these as two
# causes a reader would act on differently.
fleet_rate_at_target.rolling <- window_grid.rolling %>%
  select(window.start, window.end) %>%
  inner_join(fleet_rate_loo.rolling %>% rename(window.end = Batch.Year), by = "window.end") %>%
  select(Vessel.ADFG.Number, Fishery, window.start,
         fleet.rate.excl.i.target = fleet.rate.excl.i,
         n.other.active.target    = n.other.active)

cat("Vessel x fishery x window cells where the vessel itself was active at window.end -",
    nrow(fleet_rate_at_target.rolling), ", of those with a DEFINED (>=", BH_MIN_OTHER_ACTIVE_VESSELS,
    "other active vessels) leave-one-out fleet rate -",
    sum(is.finite(fleet_rate_at_target.rolling$fleet.rate.excl.i.target)), "\n")

# ============================================================================
# 5. Candidate universe and predicted_ijw
# ============================================================================
#
# target_fisheries.rolling is the FULL set of (vessel, fishery, window)
# triples this script attempts a prediction for, one row per fishery the
# vessel actually fished in that window's window.end, restricted to
# eligible vessel-windows via the ONE semi_join against
# vessel_window_eligibility.rolling called for anywhere in this script, no
# competing eligibility rule is built. actual.revenue is carried along here
# directly from vessel_fishery_year (already deflated, reused as-is), this
# is also what Section 6's actual.matching.total/actual.full.total are built
# from.
target_fisheries.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, revenue) %>%
  rename(window.end = Batch.Year, actual.revenue = revenue) %>%
  inner_join(window_grid.rolling %>% select(window.start, window.end), by = "window.end") %>%
  semi_join(vessel_window_eligibility.rolling, by = c("Vessel.ADFG.Number", "window.start"))

cat("Vessel x fishery x window candidates attempted (fished at window.end, eligible vessel-window) -",
    nrow(target_fisheries.rolling), ", covering",
    n_distinct(target_fisheries.rolling$Vessel.ADFG.Number), "distinct vessels and",
    n_distinct(paste(target_fisheries.rolling$Vessel.ADFG.Number, target_fisheries.rolling$window.start)),
    "distinct eligible vessel-windows of", nrow(vessel_window_eligibility.rolling),
    "total eligible vessel-windows (the gap is eligible vessel-windows with zero fisheries fished",
    "specifically in window.end, the 4-of-6 floor does not require the 6th year itself to be active)\n")

# predicted.revenue absent (not zero) whenever any of the three GATING
# conditions (has.lookback.history, has.valid.ratio, has.target.fleet.rate)
# fail, matching the spec's three-way "no prediction" logic exactly. The
# funnel below splits into FOUR reason labels, not three, has.target.fleet.rate
# failing is further broken out by vessel.active.at.target (see that
# mutate() below), so a reader can tell a vessel-side cause from a fleet-side
# one. A candidate can fail more than one condition at once, it is
# attributed to whichever comes first in the case_when() priority order.
predicted_bh_detail.rolling <- target_fisheries.rolling %>%
  left_join(vessel_lookback_bh.rolling, by = c("Vessel.ADFG.Number", "Fishery", "window.start")) %>%
  mutate(n.active.years = replace_na(n.active.years, 0L)) %>%
  left_join(fleet_rate_at_target.rolling, by = c("Vessel.ADFG.Number", "Fishery", "window.start")) %>%
  mutate(
    has.lookback.history    = n.active.years > 0,
    has.valid.ratio         = is.finite(vessel.ratio),
    # TRUE only if the vessel has a row in fleet_rate_at_target.rolling at
    # all, i.e. it was itself "active" (Section 2's definition) at
    # window.end in this fishery, distinct from has.target.fleet.rate below,
    # which additionally requires >= BH_MIN_OTHER_ACTIVE_VESSELS others.
    vessel.active.at.target = !is.na(n.other.active.target),
    has.target.fleet.rate   = is.finite(fleet.rate.excl.i.target),
    predicted.revenue = if_else(
      has.lookback.history & has.valid.ratio & has.target.fleet.rate,
      avg.days * fleet.rate.excl.i.target * vessel.ratio,
      NA_real_
    ),
    # Four mutually exclusive reasons now, not three. "No year-6 fleet rate"
    # used to conflate two different causes a reader would act on
    # differently, the vessel's OWN inactivity at window.end versus the
    # FLEET being too thin around an otherwise-active vessel, split apart
    # per the methodological review.
    reason.no.prediction = case_when(
      !is.na(predicted.revenue) ~ NA_character_,
      !has.lookback.history     ~ "no lookback history",
      !has.valid.ratio          ~ "lookback history but no valid ratio",
      !vessel.active.at.target  ~ "valid ratio but vessel has no clean window-end days/revenue in this fishery",
      !has.target.fleet.rate    ~ "valid ratio but fewer than 5 other active vessels at window-end",
      TRUE ~ NA_character_
    )
  )

cat("\n===== Predicted BH-effort revenue funnel, across all vessel-fishery-windows attempted =====\n")
funnel_bh.rolling <- predicted_bh_detail.rolling %>%
  mutate(outcome = if_else(!is.na(predicted.revenue), "predicted", reason.no.prediction)) %>%
  count(outcome, name = "n") %>%
  mutate(share = round(n / sum(n), 4))
print(funnel_bh.rolling)

# ============================================================================
# 6. Vessel-window aggregation
# ============================================================================
#
# J.predicted (per vessel-window) is implicit here, it is exactly the set of
# rows within a (Vessel.ADFG.Number, window.start) group that have a
# non-missing predicted.revenue, both predicted.total and
# actual.matching.total are sums restricted to that same set by
# construction (the if_else(!is.na(predicted.revenue), actual.revenue, NA)
# term below), never the vessel's full window.end fishery set.
#
# predicted.total/actual.matching.total are forced to NA rather than left at
# sum()'s all-NA-input default of 0 whenever n.fisheries.predicted == 0, a
# vessel-window with zero predicted fisheries has an UNDEFINED comparison,
# not a $0 one.
#
# mean.n.active.years.predicted / mean.n.ratio.years.predicted, added per a
# methodological review of 13b_predicted_bh_revenue_figures_rolling.R. A
# simulation there held true predictability CONSTANT across Phi and varied
# only lookback depth, and that alone produced a spurious Q8/Q1 gradient in
# 13b_'s gap measure, because a high-reallocation (high-Phi) vessel
# mechanically fishes any given fishery in fewer of the window's 5 lookback
# years, so avg.days/vessel.ratio for that fishery rest on less data. Both
# measures were already sitting in predicted_bh_detail.rolling
# (n.active.years, n.ratio.years) and simply dropped at this aggregation
# step, this carries their mean across J.predicted (the SAME set of
# fisheries predicted.total/actual.matching.total are summed over, via the
# identical if_else(!is.na(predicted.revenue), ..., NA) restriction) forward
# so 13b_ can check directly whether its Phi gradient tracks reallocation or
# tracks thinning history. mean, not min, chosen here, a mean reflects a
# multi-fishery vessel-window's overall data richness across its predicted
# portfolio, where a min would reduce to a single weakest-link fishery and
# be noisier for exactly the wide-portfolio (high-Phi) vessel-windows this
# diagnostic most needs to speak to.
predicted_bh_vessel_window.rolling <- predicted_bh_detail.rolling %>%
  group_by(Vessel.ADFG.Number, window.start, window.end) %>%
  summarise(
    n.fisheries.window.end    = n(),
    n.fisheries.predicted     = sum(!is.na(predicted.revenue)),
    predicted.total.raw       = sum(predicted.revenue, na.rm = TRUE),
    actual.matching.total.raw = sum(if_else(!is.na(predicted.revenue), actual.revenue, NA_real_), na.rm = TRUE),
    actual.full.total         = sum(actual.revenue, na.rm = TRUE),
    mean.n.active.years.predicted.raw = mean(
      if_else(!is.na(predicted.revenue), as.numeric(n.active.years), NA_real_), na.rm = TRUE
    ),
    mean.n.ratio.years.predicted.raw = mean(
      if_else(!is.na(predicted.revenue), as.numeric(n.ratio.years), NA_real_), na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    predicted.total       = if_else(n.fisheries.predicted > 0, predicted.total.raw, NA_real_),
    actual.matching.total = if_else(n.fisheries.predicted > 0, actual.matching.total.raw, NA_real_),
    # actual.full.total > 0 always holds in practice (every row summed into
    # it is a "fished" row with revenue > 0 by vessel_fishery_year's own
    # definition), guarded anyway rather than assumed.
    coverage              = if_else(actual.full.total > 0, actual.matching.total / actual.full.total, NA_real_),
    mean.n.active.years.predicted = if_else(n.fisheries.predicted > 0, mean.n.active.years.predicted.raw, NA_real_),
    mean.n.ratio.years.predicted  = if_else(n.fisheries.predicted > 0, mean.n.ratio.years.predicted.raw, NA_real_)
  ) %>%
  select(-predicted.total.raw, -actual.matching.total.raw,
         -mean.n.active.years.predicted.raw, -mean.n.ratio.years.predicted.raw)

cat("\n===== predicted_bh_vessel_window.rolling summary =====\n")
cat("Vessel-window rows -", nrow(predicted_bh_vessel_window.rolling), ", distinct vessels -",
    n_distinct(predicted_bh_vessel_window.rolling$Vessel.ADFG.Number), "\n")
cat("Vessel-windows with at least one predicted fishery -",
    sum(predicted_bh_vessel_window.rolling$n.fisheries.predicted > 0), "of",
    nrow(predicted_bh_vessel_window.rolling), "\n")
cat("Mean coverage (actual.matching.total / actual.full.total) among vessel-windows with a defined value -",
    round(mean(predicted_bh_vessel_window.rolling$coverage, na.rm = TRUE), 4), ", median -",
    round(median(predicted_bh_vessel_window.rolling$coverage, na.rm = TRUE), 4), "\n")
cat("Mean lookback depth across J.predicted, mean.n.active.years.predicted -",
    round(mean(predicted_bh_vessel_window.rolling$mean.n.active.years.predicted, na.rm = TRUE), 3),
    ", mean.n.ratio.years.predicted -",
    round(mean(predicted_bh_vessel_window.rolling$mean.n.ratio.years.predicted, na.rm = TRUE), 3), "\n")
cat("predicted.total and actual.matching.total/actual.full.total are on the SAME real, CPI-deflated",
    "dollar basis (base year MAX_YEAR), revenue.clean was deflated in Section 1 via the same",
    "load_deflator()/deflate() machinery vessel_fishery_year$revenue itself was deflated with in",
    "01_build_panel.R, see this script's header note\n")

# ============================================================================
# 7. Save
# ============================================================================

predicted_bh_path <- file.path(intermediate_dir, "ch3_predicted_bh.rdata")
save(predicted_bh_vessel_window.rolling, predicted_bh_detail.rolling, file = predicted_bh_path)
cat("Saved predicted BH-effort revenue objects to", predicted_bh_path, "\n")
