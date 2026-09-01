# Synthetic-data correctness test for
#   12c_predicted_bh_phi_rolling.R (Task A, the predicted BH-effort Phi
#   benchmark, including the support-matched realized Phi added per a
#   methodological review), and
#   12b_predicted_bh_revenue_rolling.R's Section 4-6 days/rate log
#   decomposition (Task B, added to that existing script).
#
# NEITHER SCRIPT CAN BE RUN AGAINST REAL DATA HERE, same reason every other
# script in code/empirical_pipeline/ cannot, confidential CFEC/AKFIN data
# lives only on the remote server (00_setup.R's own header note). This test
# instead checks the ARITHMETIC on small, hand-built data frames with known,
# independently-derived correct answers, the same purpose
# Chpt3/fig1_table4/smoke_test.R serves for that (separate, function-based)
# mini-pipeline. There is no other synthetic-data test anywhere in
# code/empirical_pipeline/ for the identical reason (every script there is a
# monolithic top-level script operating on globals, not a set of pure
# functions a smoke test could import and call directly).
#
# WHY THIS TEST RE-EXPRESSES 12c_'S SECTIONS 2-8 CODE RATHER THAN SOURCING
# 12c_predicted_bh_phi_rolling.R DIRECTLY. Three independent obstacles rule
# that out in this local environment, none of them about the correctness of
# the arithmetic itself, (1) 00_setup.R requires the "tidyverse", "xtable",
# and "data.table" packages, none of which are installed in this local R
# environment (only their dplyr/tidyr/tibble/purrr/stringr/ggplot2/fixest
# components are), (2) 00_setup.R also does setwd() to a path on the remote
# server and rm(list = ls()), neither of which is safe or meaningful to run
# locally, and (3) 12c_'s own Section 1 reads
# intermediate data/catch_data_temp.rdata, which does not exist locally.
# Given that, this test instead copies 12c_'s Section 2 through Section 8
# code VERBATIM (the parts of that script that are genuinely NEW logic,
# Section 1's day/trip construction is an unmodified duplicate of 12b_'s own
# already-established Section 1 and is not re-tested here), fed by small
# hand-built tables shaped exactly like Section 1's own OUTPUT
# (vessel_fishery_year_bh.rolling) and like ch3_rolling.rdata's own saved
# objects (vessel_fishery_year, vessel_window_eligibility.rolling,
# vessel_year_window_eligible.rolling, vessel_window_summary.rolling). The
# ONE exception is 12c_'s own call to roll_expand_to_windows()
# (00b_rolling_periods.R, itself data.table-based), replaced below by
# hand_expand_single_window(), a small local shim that reproduces that
# function's OUTPUT SHAPE for the special case of exactly one fixed window,
# which is all every scenario below needs. roll_expand_to_windows() itself
# is exercised identically by every other rolling script's own use of it and
# is not the logic this test exists to check.
#
# If 12c_predicted_bh_phi_rolling.R or 12b_predicted_bh_revenue_rolling.R is
# ever edited, the corresponding block below (clearly marked "COPIED FROM
# 12c_ Section N" / "COPIED FROM 12b_ Section N") must be updated to match,
# or this test silently stops verifying the code that actually ships.
#
# Run locally with: Rscript synthetic_test_predicted_bh.R

library(dplyr)
library(tidyr)
library(tibble)

failures <- character(0)
check <- function(desc, ok) {
  status <- if (isTRUE(ok)) "OK  " else "FAIL"
  message(status, " ", desc)
  if (!isTRUE(ok)) failures <<- c(failures, desc)
}
near <- function(a, b, tol = 1e-8) is.finite(a) && is.finite(b) && abs(a - b) < tol

# ============================================================================
# Shared local constants, matching 12c_'s own values exactly (see that
# script's own "Local constants" section for the full justification of each).
# ============================================================================

BH_MIN_OTHER_ACTIVE_VESSELS          <- 5
BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION <- 3
BH_MIN_YEAR_COVERAGE_SHARE           <- 0.5
BH_MIN_YEAR_COVERAGE_SHARE_STRICT    <- 0.75

# roll_attrition_row(), copied verbatim from 00b_rolling_periods.R (no
# data.table dependency in this particular helper, safe to copy as-is).
roll_attrition_row <- function(ledger, label, df, vessel_col = "Vessel.ADFG.Number") {
  bind_rows(
    ledger,
    tibble(step = label, n.obs = nrow(df), n.vessels = n_distinct(df[[vessel_col]]))
  )
}

# hand_expand_single_window(), the roll_expand_to_windows() shim described
# in this file's header note, for exactly the one fixed window every
# scenario below uses.
hand_expand_single_window <- function(df, window_start, window_end) {
  df %>%
    filter(Batch.Year >= window_start, Batch.Year <= window_end) %>%
    mutate(window.start = window_start, window.end = window_end)
}

WS <- 2000L  # window.start
WE <- 2005L  # window.end, ROLL_WINDOW_WIDTH = 6 (2000..2005 inclusive)

# ============================================================================
# TEST SUITE A. 12c_predicted_bh_phi_rolling.R's Phi^BH arithmetic
# ============================================================================
#
# Eight vessels, one shared window [2000, 2005], each isolated on its own
# fishery codes (no two vessels ever share a fishery or its filler-vessel
# pool), so nothing about one vessel's scenario can leak into another's
# through a shared fleet-rate computation.
#
#   V1 ("clean pass-through BH vessel"). Fishes FA1 and FB1 every year with
#       CONSTANT effort, CONSTANT fleet rates, and a vessel.ratio of exactly
#       1 (its own rate always equals the fleet's). Expected result,
#       Phi.BH == 0 EXACTLY, a constant share vector has zero variance by
#       construction, the direct analog of fig1_table4/smoke_test.R's own
#       "vessel 101 Phi == 0" check.
#   V2 ("realized reallocator, PREDICTED counterfactual still flat, MATCHED
#       comparator correctly shows a real positive gap"). Fishes FA2 every
#       year, but fishes FB2 only in 2002-2005, REALLY skipping it in
#       2000-2001 (fished = FALSE those two years in vessel_fishery_year,
#       zero realized dollars there, not merely a thin fleet or a floor
#       failure). Both fisheries have constant effort/ratio/fleet-rate, so
#       avg.days_FB2 and vessel.ratio_FB2 are unaffected by which years are
#       averaged over, predicted_FB2,t is IDENTICAL in every one of the 6
#       years including the 2 it skipped (2000-2001 hit the plain-fleet-mean
#       fallback branch, 2002-2005 hit the leave-one-out branch, both equal
#       the same constant), so Phi.BH == 0 EXACTLY (the counterfactual never
#       reallocates). Phi.matched, unlike Phi.BH, is built from REALIZED
#       activity within J*, so it correctly reflects that V2 genuinely
#       dropped FB2 for 2 years (share.matched is 1.0 on FA2 alone those 2
#       years, not the counterfactual 0.816/0.184 split), giving an EXACT
#       hand-derived Phi.matched == 36/2401 (derivation in this file's own
#       comment at the check() calls below) and Phi.gap.matched == 36/2401,
#       a genuinely POSITIVE primary behavioral gap, correctly capturing
#       real reallocation, in contrast to V1's and V7's exactly-0 matched
#       gap (see those two vessels' own comments, both of which never
#       actually drop a J* fishery in any surviving year). The SECONDARY raw
#       gap (hand-set realized Phi below, standing in for what
#       vessel_window_summary.rolling would show) is a separate, larger,
#       arbitrary positive number (0.05), kept only to test the join/
#       arithmetic for the SECONDARY column pair, not derived from these
#       synthetic tickets the way the PRIMARY (matched) comparator is.
#   V3 ("zero J* fisheries"). Active (positive revenue) in 4 of the 6 years
#       (clears vessel-window eligibility) but every fishery it touches
#       fails the J* floor, FC3 (fished only 2000-2001, so at most 2
#       ratio-years even with an ample fleet) and FZ3 (always thin-fleet,
#       never has a defined rate at all). Expected result, n.fisheries.J.star
#       == 0, Phi.BH is NA, and this row lands in the funnel's FIRST
#       exclusion stage.
#   V4 ("H_LR zero-fill, one NA cell within an otherwise-surviving year,
#       AND THE ROUND-2 REGRESSION TEST for the within-year fishery-cell
#       support-match fix"). FD4 (heavy weight, ~83% of J*'s total, defined
#       all 6 years) and FE4 (light weight, ~17%, defined only in 2000-2004,
#       thin-fleet-undefined in 2005 specifically). Year 2005 still clears
#       the 50% coverage floor on FD4's weight alone, so all 6 years survive,
#       but FE4 contributes an explicit ZERO to its own H_LR numerator sum in
#       2005 (the row is simply absent from predicted_share.rolling that
#       year), diluting mean.s.pred_FE4 by dividing by n.surviving.years = 6,
#       not 5. Expected result, Phi.BH == 10/1296 exactly (hand-derived
#       below). CRITICALLY, V4 has vessel.ratio == 1 in BOTH fisheries and
#       fishes BOTH every year (LITERALLY ZERO true reallocation), so
#       Phi.matched must ALSO equal 10/1296 exactly (Phi.gap.matched == 0),
#       this is exactly the case a second round of methodological review
#       found broken under round 1's own fix, which matched Phi.matched's
#       support to Phi.BH's only at the (J*, surviving-year) level, not at
#       the exact (fishery, year) CELL level, so FE4's real 2005 dollars
#       (present, since V4 really did fish FE4 that year) were incorrectly
#       included in the OLD Phi.matched's 2005 renormalization even though
#       Phi.BH's own 2005 renormalization had already dropped FE4 (thin
#       fleet), manufacturing a spurious NEGATIVE Phi.gap.matched of
#       -10/1296 for a vessel with zero true reallocation. See the check()
#       calls below for the exact assertions this regression test relies on.
#   V5 ("partial year survival"). J* = {FG5} only (a single fishery, 3
#       ratio-years, 2000-2002, constant effort/ratio/rate), but V5 is ALSO
#       active (vessel.year.rev > 0) via an unrelated always-thin fishery
#       FZ5 in 2003-2005, years with ZERO defined J* predictions at all.
#       Expected result, n.surviving.years == 3 (2000-2002 only), and since
#       FG5's own share is trivially 1.0 in every one of those 3 years (the
#       only J* member), Phi.BH == 0 exactly over the surviving years alone.
#   V6 ("thin-fleet boundary, vessel PRESENT at n.active == BH_MIN_OTHER_ACTIVE_VESSELS").
#       NEW, per a methodological review (item 2). Fishes FH6 every year,
#       constant days/rate. FH6's fleet has 5 OTHER active vessels (n.active
#       == 6, n.other.active == 5, clears the leave-one-out floor) in
#       2000-2002, but only 4 OTHER active vessels (n.active == 5, exactly
#       BH_MIN_OTHER_ACTIVE_VESSELS, n.other.active == 4, one short) in
#       2003-2005, with V6 ITSELF counted among that n.active == 5. This is
#       the EXACT scenario the review's own demonstration used (a
#       self-inclusive fleet mean of 208 for a vessel truly earning
#       1000/day among 10/day others). Expected result, every candidate cell
#       for V6/FH6/2003-2005 has fleet.rate.applied == NA (vessel present but
#       fleet too thin, NO fallback), not the self-inclusive 208.
#   V7 ("support-mismatch spurious gap, the review's own Phi example").
#       NEW, per a methodological review (item 1). Fishes FI7 and FJ7 every
#       year with CONSTANT effort, vessel.ratio == 1 exactly, but VARYING
#       (not constant) fleet rates year to year, so Phi.BH is genuinely
#       POSITIVE (real pass-through instability), not the degenerate 0 case
#       V1/V2 above exercise. ALSO fishes FK7 every year at a CONSTANT rate
#       with NO filler vessels at all (always thin, 0 ratio-years, never
#       enters J*), the "thin third fishery" the review's own example
#       describes. Expected result, Phi.gap.matched (PRIMARY, support-matched)
#       == 0 EXACTLY (ratio == 1 in both J* fisheries makes realized-matched
#       shares equal predicted shares exactly, the same algebraic argument as
#       V1/V2), while the SECONDARY raw gap (Phi - Phi.BH, using a REALIZED
#       Phi computed via 01b_'s own H_bar/H_LR shortcut on the FULL 3-fishery
#       realized portfolio, not an arbitrary hand-picked number) is
#       genuinely, materially NONZERO, demonstrating the exact
#       portfolio-breadth confound item 1 exists to fix.
#   V8 ("surviving-year restriction, exercised on its own"). NEW, per a
#       methodological review (item 3), which found that deleting the
#       year-level restriction from Section 7 entirely broke ZERO of the
#       (at the time) 53 checks, meaning only the WITHIN-year fishery-cell
#       restriction (V4, above) had test coverage, not the ACROSS-year
#       restriction. J* = {FL8, FM8} (2 fisheries, both ratio == 1, both
#       fished and defined 2000-2004), but BOTH fisheries' fleets go thin in
#       2005 (n.active drops to 3, well below the floor, for both), so 2005
#       has ZERO defined predictions at all and never enters
#       surviving_years.rolling, n.surviving.years == 5. V8 nonetheless has
#       GENUINE, POSITIVE realized revenue in BOTH fisheries in 2005 (it
#       really did fish them, the fleet being thin is a fact about OTHER
#       vessels, not V8), and that 2005 revenue is DELIBERATELY split
#       differently between FL8/FM8 (0.667/0.333) than every other year's
#       constant 0.833/0.167 split, so if 2005 were ever incorrectly
#       included in the matched construction (i.e. if the year-level half of
#       the Section 7 restriction were broken or removed), Phi.matched would
#       come out measurably NONZERO (hand-verified below to be ~0.0078)
#       instead of the correct, exact 0. This is precisely the assertion
#       the review asked for, one that FAILS if the surviving-year
#       semi_join is removed, not just one that happens to pass either way.

# ---- Section 1 output equivalent, vessel_fishery_year_bh.rolling ----------
#
# fishing.days/revenue.clean/rev.per.day, for every (vessel, fishery, year)
# cell that is itself active (Section 1's own cleaned universe) OR that a
# filler vessel occupies. Filler vessels are named f001, f002, ... per
# fishery pool, all with IDENTICAL rev.per.day within a given (fishery,
# year), which is what makes fleet.rate.excl.i(focal vessel) exactly equal
# to that constant regardless of how many filler vessels are used (their own
# mean is that constant, and excluding the FOCAL vessel never touches the
# filler pool at all).

make_filler <- function(prefix, fishery, years, rate, days = 10, n = 5) {
  expand.grid(Vessel.ADFG.Number = paste0(prefix, sprintf("%02d", seq_len(n))),
              Batch.Year = years, stringsAsFactors = FALSE) %>%
    mutate(Fishery = fishery, fishing.days = days, rev.per.day = rate,
           revenue.clean = fishing.days * rev.per.day)
}

# make_filler_varying(), a sibling of make_filler() for V7's two fisheries,
# whose fleet rate MOVES year to year (a named vector of Batch.Year ->
# rate), rather than make_filler()'s single constant rate. Every filler
# vessel in the pool gets the SAME rate in a given year (so, exactly as in
# make_filler(), the leave-one-out mean excluding the focal vessel equals
# that year's rate exactly, regardless of pool size).
make_filler_varying <- function(prefix, fishery, rates_by_year, days = 10, n = 5) {
  expand.grid(Vessel.ADFG.Number = paste0(prefix, sprintf("%02d", seq_len(n))),
              Batch.Year = as.integer(names(rates_by_year)), stringsAsFactors = FALSE) %>%
    mutate(Fishery = fishery, fishing.days = days,
           rev.per.day = unname(rates_by_year[as.character(Batch.Year)]),
           revenue.clean = fishing.days * rev.per.day)
}

# V7's two J*-eligible fisheries, YEAR-VARYING fleet rates (unlike every
# other scenario above, which uses a flat rate throughout), so Phi.BH here
# is genuinely positive rather than the degenerate exact-0 case.
fi7_rates <- c(`2000` = 100, `2001` = 120, `2002` = 90, `2003` = 110, `2004` = 95, `2005` = 105)
fj7_rates <- c(`2000` = 50, `2001` = 40, `2002` = 60, `2003` = 45, `2004` = 55, `2005` = 50)
fk7_rate  <- 30  # constant, and FK7 has NO filler vessels at all (always thin, see below)

vfy_bh_rows <- list(
  # V1, FA1 (rate 100, days 8) and FB1 (rate 40, days 4), every year, ratio 1.
  tibble(Vessel.ADFG.Number = "V1", Batch.Year = WS:WE, Fishery = "FA1",
         fishing.days = 8, rev.per.day = 100, revenue.clean = 8 * 100),
  tibble(Vessel.ADFG.Number = "V1", Batch.Year = WS:WE, Fishery = "FB1",
         fishing.days = 4, rev.per.day = 40, revenue.clean = 4 * 40),
  make_filler("v1a", "FA1", WS:WE, rate = 100),
  make_filler("v1b", "FB1", WS:WE, rate = 40),

  # V2, FA2 every year (rate 100, days 8). FB2 only 2002-2005 (rate 30, days 6).
  tibble(Vessel.ADFG.Number = "V2", Batch.Year = WS:WE, Fishery = "FA2",
         fishing.days = 8, rev.per.day = 100, revenue.clean = 8 * 100),
  tibble(Vessel.ADFG.Number = "V2", Batch.Year = 2002:2005, Fishery = "FB2",
         fishing.days = 6, rev.per.day = 30, revenue.clean = 6 * 30),
  make_filler("v2a", "FA2", WS:WE, rate = 100),
  make_filler("v2b", "FB2", WS:WE, rate = 30),  # present ALL 6 years, so the
  # fallback (plain fleet mean) branch is well-defined in 2000-2001 too.

  # V3, FC3 in 2000-2001 only (rate 70, days 5, ample filler, but only 2
  # ratio-years so it will not clear the J* floor). FZ3 in 2002-2003 (rate
  # 70, days 5, but ONLY 1 filler vessel, always below the thin-fleet floor).
  tibble(Vessel.ADFG.Number = "V3", Batch.Year = 2000:2001, Fishery = "FC3",
         fishing.days = 5, rev.per.day = 70, revenue.clean = 5 * 70),
  make_filler("v3a", "FC3", 2000:2001, rate = 70),
  tibble(Vessel.ADFG.Number = "V3", Batch.Year = 2002:2003, Fishery = "FZ3",
         fishing.days = 5, rev.per.day = 70, revenue.clean = 5 * 70),
  make_filler("v3z", "FZ3", 2002:2003, rate = 70, n = 1),

  # V4, FD4 every year (rate 100, days 10, ample filler all 6 years). FE4
  # every year (rate 20, days 10), ample filler 2000-2004 but only 2 filler
  # vessels (below BH_MIN_OTHER_ACTIVE_VESSELS) in 2005.
  tibble(Vessel.ADFG.Number = "V4", Batch.Year = WS:WE, Fishery = "FD4",
         fishing.days = 10, rev.per.day = 100, revenue.clean = 10 * 100),
  make_filler("v4d", "FD4", WS:WE, rate = 100),
  tibble(Vessel.ADFG.Number = "V4", Batch.Year = WS:WE, Fishery = "FE4",
         fishing.days = 10, rev.per.day = 20, revenue.clean = 10 * 20),
  make_filler("v4e", "FE4", 2000:2004, rate = 20),
  make_filler("v4e", "FE4", 2005, rate = 20, n = 2),

  # V5, FG5 in 2000-2002 only (rate 50, days 7, ample filler). FZ5 in
  # 2003-2005 (rate 50, days 7, but only 1 filler vessel, always thin).
  tibble(Vessel.ADFG.Number = "V5", Batch.Year = 2000:2002, Fishery = "FG5",
         fishing.days = 7, rev.per.day = 50, revenue.clean = 7 * 50),
  make_filler("v5g", "FG5", 2000:2002, rate = 50),
  tibble(Vessel.ADFG.Number = "V5", Batch.Year = 2003:2005, Fishery = "FZ5",
         fishing.days = 7, rev.per.day = 50, revenue.clean = 7 * 50),
  make_filler("v5z", "FZ5", 2003:2005, rate = 50, n = 1),

  # V6, FH6 every year (rate 1000, days 10). Filler pool v6f01-05 present
  # 2000-2002 (n.active == 6, n.other.active == 5, clears the LOO floor),
  # only v6f01-04 present 2003-2005 (n.active == 5 INCLUDING V6 itself,
  # n.other.active == 4, one short), the exact boundary item 2 exists to fix.
  tibble(Vessel.ADFG.Number = "V6", Batch.Year = WS:WE, Fishery = "FH6",
         fishing.days = 10, rev.per.day = 1000, revenue.clean = 10 * 1000),
  make_filler("v6f", "FH6", 2000:2002, rate = 10, n = 5),
  make_filler("v6f", "FH6", 2003:2005, rate = 10, n = 4),

  # V7, see the scenario list above. FI7/FJ7 ratio == 1 every year against a
  # YEAR-VARYING fleet rate, FK7 always thin (no filler at all).
  tibble(Vessel.ADFG.Number = "V7", Batch.Year = WS:WE, Fishery = "FI7",
         fishing.days = 8, rev.per.day = unname(fi7_rates[as.character(WS:WE)]),
         revenue.clean = 8 * unname(fi7_rates[as.character(WS:WE)])),
  make_filler_varying("v7i", "FI7", fi7_rates),
  tibble(Vessel.ADFG.Number = "V7", Batch.Year = WS:WE, Fishery = "FJ7",
         fishing.days = 5, rev.per.day = unname(fj7_rates[as.character(WS:WE)]),
         revenue.clean = 5 * unname(fj7_rates[as.character(WS:WE)])),
  make_filler_varying("v7j", "FJ7", fj7_rates),
  tibble(Vessel.ADFG.Number = "V7", Batch.Year = WS:WE, Fishery = "FK7",
         fishing.days = 6, rev.per.day = fk7_rate, revenue.clean = 6 * fk7_rate),
  # NO filler for FK7, n.active == 1 (V7 alone) every year, always below
  # BH_MIN_OTHER_ACTIVE_VESSELS, FK7 never gets a defined rate and so never
  # clears the J* floor, exactly "a thin third fishery drops out of J*".

  # V8, FL8 (heavy, rate 100, days 10) and FM8 (light, rate 20, days 10),
  # ratio == 1 in both, ample filler (5 vessels each) 2000-2004, only 2
  # filler vessels each in 2005 (thin, n.active == 3 with V8, well below the
  # floor), so BOTH fisheries are undefined in 2005, that whole year has
  # zero defined predictions and never survives.
  tibble(Vessel.ADFG.Number = "V8", Batch.Year = WS:WE, Fishery = "FL8",
         fishing.days = 10, rev.per.day = 100, revenue.clean = 10 * 100),
  make_filler("v8l", "FL8", 2000:2004, rate = 100, n = 5),
  make_filler("v8l", "FL8", 2005, rate = 100, n = 2),
  tibble(Vessel.ADFG.Number = "V8", Batch.Year = WS:WE, Fishery = "FM8",
         fishing.days = 10, rev.per.day = 20, revenue.clean = 10 * 20),
  make_filler("v8m", "FM8", 2000:2004, rate = 20, n = 5),
  make_filler("v8m", "FM8", 2005, rate = 20, n = 2)
)

vessel_fishery_year_bh.rolling <- bind_rows(vfy_bh_rows)

# ---- vessel_fishery_year, the "fished"/"revenue" table (already-deflated
#      basis, reused as-is, matching 12c_'s own header note) --------------
#
# fished == TRUE, revenue arbitrarily set equal to revenue.clean here for
# V1-V6 (a fine simplification, this test does not exercise the "two
# different revenue sources can diverge" scenario for 12c_, only that each
# source is read from the RIGHT place, which coverage.BH's construction
# below checks). V7's revenue is likewise set equal to its own
# revenue.clean (fishing.days * rate, matching the rows above exactly),
# used both here AND for the raw-Phi computation just below, so raw Phi is
# genuinely, internally consistent with this test's own synthetic tickets,
# not an arbitrary hand-picked number. Filler vessels are omitted here on
# purpose, vessel_fishery_year only needs rows for the FOCAL vessels this
# test actually checks fished-year/coverage/realized-Phi logic for.

vessel_fishery_year <- bind_rows(
  tibble(Vessel.ADFG.Number = "V1", Batch.Year = WS:WE, Fishery = "FA1", fished = TRUE, revenue = 8 * 100),
  tibble(Vessel.ADFG.Number = "V1", Batch.Year = WS:WE, Fishery = "FB1", fished = TRUE, revenue = 4 * 40),

  tibble(Vessel.ADFG.Number = "V2", Batch.Year = WS:WE, Fishery = "FA2", fished = TRUE, revenue = 8 * 100),
  tibble(Vessel.ADFG.Number = "V2", Batch.Year = 2002:2005, Fishery = "FB2", fished = TRUE, revenue = 6 * 30),

  tibble(Vessel.ADFG.Number = "V3", Batch.Year = 2000:2001, Fishery = "FC3", fished = TRUE, revenue = 5 * 70),
  tibble(Vessel.ADFG.Number = "V3", Batch.Year = 2002:2003, Fishery = "FZ3", fished = TRUE, revenue = 5 * 70),

  tibble(Vessel.ADFG.Number = "V4", Batch.Year = WS:WE, Fishery = "FD4", fished = TRUE, revenue = 10 * 100),
  tibble(Vessel.ADFG.Number = "V4", Batch.Year = WS:WE, Fishery = "FE4", fished = TRUE, revenue = 10 * 20),

  tibble(Vessel.ADFG.Number = "V5", Batch.Year = 2000:2002, Fishery = "FG5", fished = TRUE, revenue = 7 * 50),
  tibble(Vessel.ADFG.Number = "V5", Batch.Year = 2003:2005, Fishery = "FZ5", fished = TRUE, revenue = 7 * 50),

  tibble(Vessel.ADFG.Number = "V6", Batch.Year = WS:WE, Fishery = "FH6", fished = TRUE, revenue = 10 * 1000),

  tibble(Vessel.ADFG.Number = "V7", Batch.Year = WS:WE, Fishery = "FI7", fished = TRUE,
         revenue = 8 * unname(fi7_rates[as.character(WS:WE)])),
  tibble(Vessel.ADFG.Number = "V7", Batch.Year = WS:WE, Fishery = "FJ7", fished = TRUE,
         revenue = 5 * unname(fj7_rates[as.character(WS:WE)])),
  tibble(Vessel.ADFG.Number = "V7", Batch.Year = WS:WE, Fishery = "FK7", fished = TRUE, revenue = 6 * fk7_rate),

  # V8, FL8 realized revenue is CONSTANT (1000) every year including 2005
  # (matching its own days*rate combo exactly, ratio == 1). FM8 realized
  # revenue is 200 (matching its own days*rate combo) in 2000-2004 but
  # DELIBERATELY 500 (a genuine, unrelated real-world difference that one
  # year, NOT derived from days*rate) in 2005, specifically so that
  # INCLUDING 2005 in the matched construction (the bug this scenario
  # exists to catch) would be numerically DETECTABLE, not silently
  # identical to correctly excluding it, see this file's own V8 scenario
  # comment above for the full reasoning and the hand-derived numbers.
  tibble(Vessel.ADFG.Number = "V8", Batch.Year = WS:WE, Fishery = "FL8", fished = TRUE, revenue = 10 * 100),
  tibble(Vessel.ADFG.Number = "V8", Batch.Year = 2000:2004, Fishery = "FM8", fished = TRUE, revenue = 10 * 20),
  tibble(Vessel.ADFG.Number = "V8", Batch.Year = 2005, Fishery = "FM8", fished = TRUE, revenue = 500)
)

# ---- V7's RAW (full 3-fishery) realized Phi, computed via 01b_'s own
#      H_bar/H_LR shortcut, independently of 12c_'s own machinery ----------
#
# NOT part of the "COPIED FROM 12c_" blocks below, this reproduces 01b_'s
# own construction (Section 4/5 of 01b_build_rolling_panel.R, H_bar = mean
# of yearly HHI, H_LR = sum of (S_ij / n.years.window)^2), applied to V7's
# full realized (all 3 fisheries, all 6 years) revenue above, so the
# "realized Phi" fed into vessel_window_summary.rolling for V7 below is a
# GENUINE, internally-consistent value, not an arbitrary hand-picked one
# the way V1/V2's realized Phi values are (see those two vessels' own
# comment for why an arbitrary value was fine there, testing only the join/
# arithmetic, not the confound itself).
v7_realized <- vessel_fishery_year %>% filter(Vessel.ADFG.Number == "V7") %>%
  group_by(Batch.Year) %>%
  mutate(share = revenue / sum(revenue)) %>%
  ungroup()

v7_H_bar_raw <- v7_realized %>%
  group_by(Batch.Year) %>% summarise(hhi = sum(share^2), .groups = "drop") %>%
  summarise(H_bar = mean(hhi)) %>% pull(H_bar)
v7_H_LR_raw <- v7_realized %>%
  group_by(Fishery) %>% summarise(mean.share = sum(share) / 6, .groups = "drop") %>%
  summarise(H_LR = sum(mean.share^2)) %>% pull(H_LR)
v7_Phi_raw <- v7_H_bar_raw - v7_H_LR_raw

# ---- vessel_window_eligibility.rolling / vessel_year_window_eligible.rolling
#      (ch3_rolling.rdata equivalents) ---------------------------------------
#
# Every focal vessel is "active" (vessel.year.rev > 0, i.e. positive revenue
# in ANY fishery) in every calendar year it appears in vessel_fishery_year
# above, EXCEPT V3 and V5, whose active-year sets are deliberately built to
# differ from any single fishery's own fished-year set (V3, active
# 2000-2003 only, via FC3 then FZ3, clearing the 4-of-6 floor with room to
# spare, V5, active every year 2000-2005, via FG5 then FZ5). V6, V7, and V8
# are all active every year (V6 via FH6 alone, V7 via all three fisheries,
# V8 via both FL8 and FM8, including 2005 despite the fleet being thin
# that year for OTHER vessels, that thinness says nothing about whether V8
# itself is active).

vessel_window_eligibility.rolling <- tibble(
  Vessel.ADFG.Number = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"), window.start = WS
)

vessel_year_window_eligible.rolling <- bind_rows(
  tibble(Vessel.ADFG.Number = "V1", window.start = WS, Batch.Year = WS:WE),
  tibble(Vessel.ADFG.Number = "V2", window.start = WS, Batch.Year = WS:WE),
  tibble(Vessel.ADFG.Number = "V3", window.start = WS, Batch.Year = 2000:2003),
  tibble(Vessel.ADFG.Number = "V4", window.start = WS, Batch.Year = WS:WE),
  tibble(Vessel.ADFG.Number = "V5", window.start = WS, Batch.Year = WS:WE),
  tibble(Vessel.ADFG.Number = "V6", window.start = WS, Batch.Year = WS:WE),
  tibble(Vessel.ADFG.Number = "V7", window.start = WS, Batch.Year = WS:WE),
  tibble(Vessel.ADFG.Number = "V8", window.start = WS, Batch.Year = WS:WE)
)

# ---- vessel_window_summary.rolling (realized Phi etc., hand-set for V1/V2,
#      GENUINELY COMPUTED for V7, see above) --------------------------------
#
# Only V1, V2, and V7 are given a realized row here, on purpose, this table
# exercises the Section 8 JOIN and the Phi.gap/Phi.gap.matched arithmetic,
# not a claim that V1/V2's own numbers were derived from the synthetic
# tickets above (they were not, vessel_window_summary.rolling is built by
# an entirely separate construction in 01b_build_rolling_panel.R that this
# test does not reproduce in general, V7 is the one exception, built
# specifically to test item 1's fix against a REAL, internally-consistent
# mismatch rather than an arbitrary one). V2's hand-set Phi (0.05) is
# deliberately POSITIVE and does not need to match any particular value,
# only that Phi.gap = Phi - Phi.BH comes out to exactly Phi - 0 = Phi below,
# given this test's own V2 has Phi.BH == 0 exactly.

vessel_window_summary.rolling <- tibble(
  Vessel.ADFG.Number = c("V1", "V2", "V7"), window.start = WS,
  H_bar = c(0.73, 0.75, v7_H_bar_raw), H_LR = c(0.728, 0.70, v7_H_LR_raw),
  Phi = c(0.002, 0.05, v7_Phi_raw),
  is.specialist.window = FALSE,
  n.years.window = c(6L, 6L, 6L),
  n.fisheries.fished.window = c(2L, 2L, 3L)
)

# ============================================================================
# COPIED FROM 12c_ Section 2 (fleet-side per-day rate, leave-one-out and
# plain fleet mean), UNCHANGED except that Section 1's own
# vessel_fishery_year_bh.rolling is fed in directly above rather than built
# from a catch_data_temp reload.
# ============================================================================

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

fleet_rate_full.rolling <- fleet_fishery_year_bh.rolling %>%
  mutate(fleet.rate.mean = if_else(n.active >= BH_MIN_OTHER_ACTIVE_VESSELS,
                                    sum.rev.per.day / n.active, NA_real_)) %>%
  select(Fishery, Batch.Year, fleet.rate.mean, n.active)

# ============================================================================
# COPIED FROM 12c_ Section 3 (fixed portfolio J*), UNCHANGED except
# roll_expand_to_windows() is replaced by hand_expand_single_window(), see
# this file's header note.
# ============================================================================

lookback_windows_full.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  hand_expand_single_window(WS, WE) %>%
  semi_join(vessel_window_eligibility.rolling, by = c("Vessel.ADFG.Number", "window.start"))

avg_days_full.rolling <- lookback_windows_full.rolling %>%
  select(Vessel.ADFG.Number, Fishery, window.start, Batch.Year) %>%
  left_join(vessel_fishery_year_bh.rolling %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, fishing.days),
            by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  mutate(fishing.days = replace_na(fishing.days, 0)) %>%
  group_by(Vessel.ADFG.Number, Fishery, window.start) %>%
  summarise(avg.days = mean(fishing.days), .groups = "drop")

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

J_star.rolling <- vessel_ratio_full.rolling %>%
  filter(n.ratio.years >= BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION) %>%
  left_join(avg_days_full.rolling, by = c("Vessel.ADFG.Number", "Fishery", "window.start")) %>%
  select(Vessel.ADFG.Number, window.start, Fishery, avg.days, vessel.ratio, n.ratio.years)

# ============================================================================
# COPIED FROM 12c_ Section 4 (prediction grid), UPDATED to match the FIX for
# item 2 (the self-inclusive fleet-mean-fallback bug at the
# n.active == BH_MIN_OTHER_ACTIVE_VESSELS boundary).
# ============================================================================

candidate_grid.rolling <- J_star.rolling %>%
  inner_join(
    vessel_year_window_eligible.rolling %>% select(Vessel.ADFG.Number, window.start, Batch.Year),
    by = c("Vessel.ADFG.Number", "window.start"), relationship = "many-to-many"
  )

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
    vessel.present.at.cell = !is.na(n.other.active),
    fleet.rate.applied      = if_else(vessel.present.at.cell, fleet.rate.excl.i, fleet.rate.mean),
    rate.branch = case_when(
      vessel.present.at.cell  & is.finite(fleet.rate.excl.i) ~ "leave-one-out (vessel present, fleet thick enough)",
      !vessel.present.at.cell & is.finite(fleet.rate.mean)   ~ "fallback, plain fleet mean (vessel absent that fishery-year)",
      vessel.present.at.cell                                  ~ "undefined, vessel present but fleet too thin (NO fallback, matches 12b_)",
      TRUE                                                    ~ "undefined, vessel absent AND fleet too thin"
    ),
    predicted.ijt = if_else(is.finite(fleet.rate.applied),
                             avg.days * fleet.rate.applied * vessel.ratio, NA_real_)
  )

# ============================================================================
# COPIED FROM 12c_ Section 5 (normalization and year-coverage floor),
# UPDATED to match the NEW coverage-distribution/sensitivity diagnostics
# (item 8), which are pure prints, no new arithmetic beyond what was already
# tested above.
# ============================================================================

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

surviving_years.rolling <- year_coverage.rolling %>%
  filter(year.survives) %>%
  select(Vessel.ADFG.Number, window.start, Batch.Year)

n_surviving_years.rolling <- surviving_years.rolling %>%
  count(Vessel.ADFG.Number, window.start, name = "n.surviving.years")

predicted_share.rolling <- candidate_grid.rolling %>%
  filter(is.finite(predicted.ijt)) %>%
  semi_join(surviving_years.rolling, by = c("Vessel.ADFG.Number", "window.start", "Batch.Year")) %>%
  group_by(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  mutate(year.total.predicted = sum(predicted.ijt), s.pred = predicted.ijt / year.total.predicted) %>%
  ungroup()

# ============================================================================
# COPIED FROM 12c_ Section 6 (aggregation), UNCHANGED.
# ============================================================================

H_bar_bh.rolling <- predicted_share.rolling %>%
  group_by(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  summarise(hhi.year.bh = sum(s.pred^2), .groups = "drop") %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(H_bar.BH = mean(hhi.year.bh), .groups = "drop")

S_ijw_bh.rolling <- predicted_share.rolling %>%
  group_by(Vessel.ADFG.Number, window.start, Fishery) %>%
  summarise(S.pred.sum = sum(s.pred), .groups = "drop") %>%
  left_join(n_surviving_years.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(mean.s.pred = S.pred.sum / n.surviving.years)

H_LR_bh.rolling <- S_ijw_bh.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(H_LR.BH = sum(mean.s.pred^2), .groups = "drop")

mean_lookback.rolling <- J_star.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(mean.lookback.BH = mean(n.ratio.years), n.fisheries.J.star = n(), .groups = "drop")

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
# COPIED FROM 12c_ Section 7 (support-matched realized Phi), UPDATED per a
# SECOND round of methodological review (round-1 item 1's fix only matched
# on J*/surviving-years, still leaving the WITHIN-year fishery-cell axis
# unmatched, this fixes that, see 12c_'s own Section 7 header note).
# ============================================================================

realized_matched_input.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, revenue) %>%
  inner_join(J_star.rolling %>% select(Vessel.ADFG.Number, window.start, Fishery),
             by = c("Vessel.ADFG.Number", "Fishery"), relationship = "many-to-many") %>%
  # FIX (round 2), matches on the EXACT (vessel, window, year, fishery)
  # cell present in predicted_share.rolling, not just "some J* fishery in a
  # surviving year" independently (which is what semi_join(surviving_years.rolling,
  # by = c("Vessel.ADFG.Number", "window.start", "Batch.Year")) alone gave).
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

# ============================================================================
# COPIED FROM 12c_ Section 8 (funnel and vessel-window assembly), UPDATED
# for the PRIMARY/SECONDARY comparator split (item 1) and the single-fishery
# J* funnel diagnostic (item 3).
# ============================================================================

funnel_base.rolling <- vessel_window_eligibility.rolling %>%
  select(Vessel.ADFG.Number, window.start) %>%
  left_join(mean_lookback.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(n.fisheries.J.star = replace_na(n.fisheries.J.star, 0L)) %>%
  left_join(n_surviving_years.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(n.surviving.years = replace_na(n.surviving.years, 0L))

phi_bh_funnel <- tibble(step = character(), n.obs = integer(), n.vessels = integer())
phi_bh_funnel <- roll_attrition_row(phi_bh_funnel, "Attempted", funnel_base.rolling)
phi_bh_funnel <- roll_attrition_row(phi_bh_funnel, "At least one J* fishery",
                                     funnel_base.rolling %>% filter(n.fisheries.J.star > 0))
phi_bh_funnel <- roll_attrition_row(phi_bh_funnel, "At least one surviving year",
                                     funnel_base.rolling %>% filter(n.fisheries.J.star > 0, n.surviving.years > 0))
phi_bh_funnel <- roll_attrition_row(
  phi_bh_funnel, "Of which, single-fishery J* only (informational)",
  funnel_base.rolling %>% filter(n.fisheries.J.star == 1, n.surviving.years > 0)
)

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
    Phi.gap.matched           = Phi.matched - Phi.BH
  ) %>%
  left_join(
    vessel_window_summary.rolling %>%
      select(Vessel.ADFG.Number, window.start, H_bar, H_LR, Phi, is.specialist.window,
             n.years.window, n.fisheries.fished.window),
    by = c("Vessel.ADFG.Number", "window.start")
  ) %>%
  mutate(Phi.gap = Phi - Phi.BH) %>%
  select(
    Vessel.ADFG.Number, window.start, n.fisheries.J.star, mean.lookback.BH, n.surviving.years,
    H_bar.BH, H_LR.BH, Phi.BH, coverage.BH,
    H_bar.matched, H_LR.matched, Phi.matched, n.realized.matched.years, Phi.gap.matched,
    H_bar, H_LR, Phi, Phi.gap, n.years.window, n.fisheries.fished.window, is.specialist.window
  )

# ---- Checks -----------------------------------------------------------

row_of <- function(v) predicted_bh_phi_vessel_window.rolling %>% filter(Vessel.ADFG.Number == v)

r1 <- row_of("V1"); r2 <- row_of("V2"); r3 <- row_of("V3"); r4 <- row_of("V4")
r5 <- row_of("V5"); r6 <- row_of("V6"); r7 <- row_of("V7"); r8 <- row_of("V8")

check("V1 has both FA1 and FB1 in J* (n.fisheries.J.star == 2)", r1$n.fisheries.J.star == 2)
check("V1 all 6 years survive", r1$n.surviving.years == 6)
check("V1 Phi.BH == 0 exactly (constant shares, pure pass-through)", near(r1$Phi.BH, 0))
check("V1 H_bar.BH == H_LR.BH", near(r1$H_bar.BH, r1$H_LR.BH))
check("V1 coverage.BH == 1 (both its fisheries are in J*)", near(r1$coverage.BH, 1))
check("V1 Phi.matched == 0 exactly too (ratio == 1, realized matches predicted exactly)", near(r1$Phi.matched, 0))
check("V1 Phi.gap.matched == 0", near(r1$Phi.gap.matched, 0))

check("V2 has both FA2 and FB2 in J* (n.fisheries.J.star == 2)", r2$n.fisheries.J.star == 2)
check("V2 n.ratio.years for FB2 == 4 (2002-2005 only) but predicted into all 6 years",
      J_star.rolling %>% filter(Vessel.ADFG.Number == "V2", Fishery == "FB2") %>% pull(n.ratio.years) == 4)
check("V2 candidate grid has a FB2 prediction for 2000 and 2001 (skipped years, fallback branch)",
      candidate_grid.rolling %>%
        filter(Vessel.ADFG.Number == "V2", Fishery == "FB2", Batch.Year %in% c(2000, 2001)) %>%
        summarise(all_defined = all(is.finite(predicted.ijt)),
                  all_fallback = all(rate.branch == "fallback, plain fleet mean (vessel absent that fishery-year)")) %>%
        { .$all_defined && .$all_fallback })
check("V2 Phi.BH == 0 exactly (fixed-effort counterfactual never actually reallocates)", near(r2$Phi.BH, 0))
# Hand derivation, share.matched is 1.0 (FA2 alone) in 2000-2001 (V2 really
# had zero FB2 dollars those years), and 40/49 (FA2) / 9/49 (FB2) in
# 2002-2005 (800/980 and 180/980 respectively). H_bar.matched =
# (2*1^2 + 4*((40/49)^2 + (9/49)^2)) / 6 = 1921/2401. S.matched.sum_FA2 =
# 2*1 + 4*(40/49) = 258/49, mean.share.matched_FA2 = 43/49.
# S.matched.sum_FB2 = 4*(9/49) = 36/49 (2000-2001 contribute 0, absent from
# the sum, matching the same zero-fill H_LR shortcut used throughout),
# mean.share.matched_FB2 = 6/49. H_LR.matched = (43/49)^2 + (6/49)^2 =
# 1885/2401. Phi.matched = 1921/2401 - 1885/2401 = 36/2401 exactly.
check("V2 Phi.matched == 36/2401 exactly (genuine reallocation within J*, hand-derived, see comment above)",
      near(r2$Phi.matched, 36 / 2401))
check("V2 PRIMARY behavioral gap Phi.gap.matched == 36/2401 (support-matched, correctly POSITIVE, real reallocation)",
      near(r2$Phi.gap.matched, 36 / 2401))
check("V2 SECONDARY raw behavioral gap Phi.gap == 0.05 (Phi - Phi.BH, unmatched, still carried for comparison)",
      near(r2$Phi.gap, 0.05))

check("V3 has zero J* fisheries (both FC3 and FZ3 fail their own floors)", r3$n.fisheries.J.star == 0)
check("V3 Phi.BH is NA", is.na(r3$Phi.BH))
check("V3 Phi.matched is NA too", is.na(r3$Phi.matched))

# V4, hand-derived exact fractions, see this file's header note for the
# derivation, Hbar.BH = 83/108, H_LR.BH = 986/1296, Phi.BH = 10/1296.
check("V4 all 6 years survive (FD4 alone clears the coverage floor in 2005)", r4$n.surviving.years == 6)
check("V4 H_bar.BH == 83/108", near(r4$H_bar.BH, 83 / 108))
check("V4 H_LR.BH == 986/1296", near(r4$H_LR.BH, 986 / 1296))
check("V4 Phi.BH == 10/1296", near(r4$Phi.BH, 10 / 1296))
check("V4 FE4 has no predicted_share row in 2005 (thin fleet, zero-filled in the H_LR mean)",
      predicted_share.rolling %>% filter(Vessel.ADFG.Number == "V4", Fishery == "FE4", Batch.Year == 2005) %>%
        nrow() == 0)
# ITEM 2's OWN REGRESSION TEST (round 2 of methodological review). V4 has
# vessel.ratio == 1 in BOTH fisheries and fishes BOTH every year, LITERALLY
# ZERO true reallocation, so Phi.matched must equal Phi.BH EXACTLY, and the
# PRIMARY behavioral gap must be EXACTLY 0. Under the round-1-only fix
# (support matched at the J*/surviving-year level but not at the exact
# fishery-year CELL level), FE4's real 2005 dollars leaked into
# Phi.matched's 2005 renormalization even though Phi.BH's own 2005
# renormalization had already dropped FE4, giving Phi.matched == 0 and a
# spurious Phi.gap.matched == -10/1296 (this file's own git history, and
# the round-2 review report, has the exact numbers). This check is what
# would have caught that bug.
check("V4 Phi.matched == 10/1296 exactly (== Phi.BH, zero true reallocation, the round-2 regression test)",
      near(r4$Phi.matched, 10 / 1296))
check("V4 PRIMARY behavioral gap Phi.gap.matched == 0 exactly (the round-2 regression test)",
      near(r4$Phi.gap.matched, 0))
check("V4 realized_matched_input.rolling excludes FE4/2005 (real revenue exists, but no matching predicted cell)",
      realized_matched_input.rolling %>%
        filter(Vessel.ADFG.Number == "V4", Fishery == "FE4", Batch.Year == 2005) %>%
        nrow() == 0)

check("V5 J* has exactly one fishery (FG5)", r5$n.fisheries.J.star == 1)
check("V5 only 3 years survive (2000-2002, 2003-2005 have zero J* coverage)", r5$n.surviving.years == 3)
check("V5 Phi.BH == 0 exactly (single-fishery J*, trivial share of 1.0 every surviving year)",
      near(r5$Phi.BH, 0))

# V6, item 2's own boundary demonstration. In 2003-2005, n.active == 5
# (4 filler + V6 itself), the EXACT n.active == BH_MIN_OTHER_ACTIVE_VESSELS
# boundary the review's own bug report describes. Under the OLD (buggy)
# code this would have resolved to a SELF-INCLUSIVE fleet mean of
# (1000 + 10*4) / 5 == 208 (the review's own number). The FIXED code below
# must resolve every one of these cells to NA instead.
check("V6 FH6 clears the J* floor (n.ratio.years == 3, 2000-2002 only)",
      J_star.rolling %>% filter(Vessel.ADFG.Number == "V6", Fishery == "FH6") %>% pull(n.ratio.years) == 3)
v6_2003_2005 <- candidate_grid.rolling %>%
  filter(Vessel.ADFG.Number == "V6", Fishery == "FH6", Batch.Year %in% 2003:2005)
check("V6 2003-2005 candidate rows all have vessel.present.at.cell == TRUE (V6 itself fished FH6 those years)",
      nrow(v6_2003_2005) == 3 && all(v6_2003_2005$vessel.present.at.cell))
check("V6 2003-2005 fleet.rate.applied is NA (present-but-thin, NOT the self-inclusive 208)",
      all(is.na(v6_2003_2005$fleet.rate.applied)))
check("V6 2003-2005 fleet.rate.applied is NOT the self-inclusive value 208 (the exact bug value, sanity check)",
      all(is.na(v6_2003_2005$fleet.rate.applied) | abs(v6_2003_2005$fleet.rate.applied - 208) > 1e-8))
check("V6 2003-2005 predicted.ijt is NA (no prediction from a contaminated rate)",
      all(is.na(v6_2003_2005$predicted.ijt)))
check("V6 2003-2005 rate.branch correctly labeled 'present but fleet too thin' (no fallback)",
      all(v6_2003_2005$rate.branch == "undefined, vessel present but fleet too thin (NO fallback, matches 12b_)"))
check("V6 2000-2002 candidate rows DO get a defined leave-one-out prediction (fleet thick enough there)",
      candidate_grid.rolling %>% filter(Vessel.ADFG.Number == "V6", Fishery == "FH6", Batch.Year %in% 2000:2002) %>%
        summarise(all(is.finite(predicted.ijt))) %>% pull(1))

# V7, item 1's own support-mismatch demonstration.
check("V7 J* == {FI7, FJ7} only (2 fisheries, FK7 always thin, excluded)", r7$n.fisheries.J.star == 2)
check("V7 Phi.BH is genuinely positive (real pass-through instability, NOT the degenerate exact-0 case)",
      is.finite(r7$Phi.BH) && r7$Phi.BH > 1e-6)
check("V7 PRIMARY gap Phi.gap.matched == 0 EXACTLY (ratio == 1 in both J* fisheries)",
      near(r7$Phi.gap.matched, 0, tol = 1e-8))
check("V7 SECONDARY raw gap Phi.gap is materially NONZERO (the spurious support-mismatch artifact item 1 fixes)",
      is.finite(r7$Phi.gap) && abs(r7$Phi.gap) > 1e-4)
check("V7 |Phi.gap.matched| << |Phi.gap| (the fix materially shrinks the behavioral gap toward the true value of 0)",
      abs(r7$Phi.gap.matched) < abs(r7$Phi.gap))
check("V7 n.years.window == n.surviving.years == 6 (support divergence is on the FISHERY axis here, not years)",
      r7$n.years.window == 6 && r7$n.surviving.years == 6)
check("V7 n.fisheries.fished.window (3) != n.fisheries.J.star (2), the exact divergence item 1's diagnostic flags",
      r7$n.fisheries.fished.window == 3 && r7$n.fisheries.J.star == 2)

# V8, item 3's own regression test for the SURVIVING-YEAR half of the
# Section 7 support match (as opposed to V4's WITHIN-year fishery-cell
# half). 2005 has zero defined predictions for EITHER fishery (both
# fleets thin), so it must never enter surviving_years.rolling, and,
# CRITICALLY, must never enter the matched construction either, despite V8
# having genuine, positive, and (deliberately) DIFFERENTLY-SPLIT real
# revenue in both fisheries that year. If the year-level restriction were
# ever broken (e.g. a future edit collapses the semi_join key list down to
# just Fishery, or drops Batch.Year from it), 2005's real dollars would
# leak in and change Phi.matched from the correct EXACT 0 to a measurably
# different (hand-verified ~0.0078) nonzero value, see this file's own V8
# scenario comment above for the full derivation.
check("V8 J* == {FL8, FM8} (both clear the floor on 2000-2004 alone)", r8$n.fisheries.J.star == 2)
check("V8 n.surviving.years == 5 (2005 excluded, both fisheries thin that year)", r8$n.surviving.years == 5)
check("V8 candidate grid has zero defined predictions for BOTH fisheries in 2005",
      candidate_grid.rolling %>% filter(Vessel.ADFG.Number == "V8", Batch.Year == 2005) %>%
        summarise(all(!is.finite(predicted.ijt))) %>% pull(1))
check("V8 realized_matched_input.rolling has ZERO rows for 2005 (the direct, mechanistic check of the fix)",
      realized_matched_input.rolling %>% filter(Vessel.ADFG.Number == "V8", Batch.Year == 2005) %>% nrow() == 0)
check("V8 n.realized.matched.years == 5 (2005 correctly excluded from the matched year count too)",
      r8$n.realized.matched.years == 5)
check("V8 Phi.BH == 0 exactly (ratio == 1 in both fisheries, constant shares over the 5 surviving years)",
      near(r8$Phi.BH, 0))
check(paste("V8 Phi.matched == 0 exactly (NOT the ~0.0078 value a broken year-level restriction would produce,",
            "this is the assertion item 3 asked for, one that fails if the surviving-year semi_join is removed)"),
      near(r8$Phi.matched, 0))
check("V8 PRIMARY behavioral gap Phi.gap.matched == 0 exactly", near(r8$Phi.gap.matched, 0))

# Independent substantiation (NOT part of 12c_'s own code, a from-scratch
# parallel calculation in base R) that the check above actually has teeth,
# i.e. that a year-restriction failure would be CAUGHT, not just that the
# fixed code happens to give 0. Recomputes what Phi.matched WOULD have been
# had 2005 been incorrectly included (the exact "semi_join(surviving_years.rolling,
# ...) deleted" failure mode item 3 describes), using V8's own real
# vessel_fishery_year revenue for all 6 years directly, independent of
# candidate_grid.rolling/predicted_share.rolling entirely.
v8_all_years <- vessel_fishery_year %>% filter(Vessel.ADFG.Number == "V8") %>%
  group_by(Batch.Year) %>% mutate(share.buggy = revenue / sum(revenue)) %>% ungroup()
v8_H_bar_buggy <- v8_all_years %>% group_by(Batch.Year) %>% summarise(hhi = sum(share.buggy^2), .groups = "drop") %>%
  summarise(m = mean(hhi)) %>% pull(m)
v8_H_LR_buggy <- v8_all_years %>% group_by(Fishery) %>% summarise(s = sum(share.buggy) / 6, .groups = "drop") %>%
  summarise(h = sum(s^2)) %>% pull(h)
v8_Phi_buggy <- v8_H_bar_buggy - v8_H_LR_buggy
check(paste("V8 sanity, the value Phi.matched WOULD take if 2005 leaked in (~0.0078, computed independently",
            "above) is clearly nonzero and clearly different from the correct 0, confirming the V8 checks above",
            "actually discriminate rather than passing either way"),
      is.finite(v8_Phi_buggy) && abs(v8_Phi_buggy) > 0.005 && !near(v8_Phi_buggy, 0, tol = 1e-4))

# ============================================================================
# TEST SUITE A2. Funnel-counting logic, isolated hand-built rows, including
# the "J* non-empty but zero surviving years" combination the organic
# scenarios above cannot realistically produce (see 12c_'s own Section 8
# comment), verified directly here instead, PLUS the item 3 single-fishery-
# J* informational row.
# ============================================================================

funnel_test_base <- tibble(
  Vessel.ADFG.Number = c("A", "B", "C", "D", "E"),
  window.start        = WS,
  n.fisheries.J.star   = c(0L, 2L, 1L, 3L, 0L),
  n.surviving.years    = c(0L, 0L, 4L, 2L, 0L)
)

funnel_test <- tibble(step = character(), n.obs = integer(), n.vessels = integer())
funnel_test <- roll_attrition_row(funnel_test, "Attempted", funnel_test_base)
funnel_test <- roll_attrition_row(funnel_test, "At least one J* fishery",
                                   funnel_test_base %>% filter(n.fisheries.J.star > 0))
funnel_test <- roll_attrition_row(funnel_test, "At least one surviving year",
                                   funnel_test_base %>% filter(n.fisheries.J.star > 0, n.surviving.years > 0))
funnel_test <- roll_attrition_row(funnel_test, "Of which, single-fishery J* only (informational)",
                                   funnel_test_base %>% filter(n.fisheries.J.star == 1, n.surviving.years > 0))

check("Funnel stage 1 (attempted) == 5", funnel_test$n.obs[1] == 5)
check("Funnel stage 2 (>= 1 J* fishery) == 3 (A and E excluded, both n.fisheries.J.star == 0)",
      funnel_test$n.obs[2] == 3)
check("Funnel stage 3 (>= 1 surviving year) == 2 (B excluded, J* > 0 but n.surviving.years == 0)",
      funnel_test$n.obs[3] == 2)
check("Funnel stage 4 (single-fishery J*, informational) == 1 (C, n.fisheries.J.star == 1 AND surviving)",
      funnel_test$n.obs[4] == 1)

# Organic cross-check, the SAME informational row computed on this test's
# own 7-vessel universe above, V5 and V6 are the only ones with a
# single-fishery J* AND at least one surviving year (V3 has zero surviving
# years to begin with, so it is correctly excluded from this count too).
check("Organic funnel's own single-fishery-J* informational row == 2 (V5 and V6)",
      phi_bh_funnel$n.obs[4] == 2)

# ============================================================================
# TEST SUITE B. 12b_predicted_bh_revenue_rolling.R's days/rate log
# decomposition (Task B), COPIED FROM 12b_ Section 5/6 (the mutate() block
# that builds actual.rate/predicted.rate/days.component/rate.component/
# total.log.deviation/revenue.basis.ratio), fed a handful of fabricated
# detail-level rows instead of the real predicted_bh_detail.rolling.
# ============================================================================

decomposition_rows <- bind_rows(
  # Row 1, effort UP, rate UP (both departures the same direction), NO
  # revenue-basis wedge (revenue.clean.target == actual.revenue).
  # predicted.revenue = 10 * 11 * 1.2 = 132.
  tibble(actual.revenue = 1200, actual.days = 12, avg.days = 10,
         fleet.rate.excl.i.target = 11, vessel.ratio = 1.2, revenue.clean.target = 1200),
  # Row 2, effort DOWN, rate UP (offsetting departures, same predicted base
  # as row 1), NO revenue-basis wedge. predicted.revenue = 10 * 11 * 1.2 = 132.
  tibble(actual.revenue = 900, actual.days = 6, avg.days = 10,
         fleet.rate.excl.i.target = 11, vessel.ratio = 1.2, revenue.clean.target = 900),
  # Row 3, a specialist-like case, effort and rate both EXACTLY match their
  # predicted counterparts (actual.days == avg.days, actual.revenue ==
  # predicted.revenue == 5 * 10 * 1 = 50), total.log.deviation should be
  # exactly 0, and so should each individual component. NO revenue-basis
  # wedge.
  tibble(actual.revenue = 50, actual.days = 5, avg.days = 5,
         fleet.rate.excl.i.target = 10, vessel.ratio = 1.0, revenue.clean.target = 50),
  # Row 4, NEW per a methodological review (item 5), a DELIBERATE
  # revenue-basis wedge, actual.revenue (1320) and revenue.clean.target
  # (1000) diverge, exercising the revenue.basis.ratio diagnostic. UPDATED
  # per a non-blocking follow-up from a second review round, actual.days
  # (12) also differs from avg.days (10) here, unlike the row this replaced,
  # so this row exercises a NONZERO days.component simultaneously with the
  # revenue-basis wedge (rows 1-2 already covered nonzero days.component
  # without a wedge, this is the only row that combines both). The additive
  # identity (days.component + rate.component == total.log.deviation) must
  # STILL hold exactly here, it depends only on actual.revenue, actual.days,
  # avg.days, and predicted.revenue, revenue.clean.target never enters the
  # decomposition itself, only the separate diagnostic. predicted.revenue =
  # 10 * 10 * 1 = 100.
  tibble(actual.revenue = 1320, actual.days = 12, avg.days = 10,
         fleet.rate.excl.i.target = 10, vessel.ratio = 1.0, revenue.clean.target = 1000)
) %>%
  mutate(predicted.revenue = avg.days * fleet.rate.excl.i.target * vessel.ratio)

# ---- COPIED FROM 12b_ Section 5/6 (the decomposition mutate() block),
#      word for word ----------------------------------------------------
decomposition_rows <- decomposition_rows %>%
  mutate(
    actual.rate = if_else(is.finite(actual.days) & actual.days > 0,
                           actual.revenue / actual.days, NA_real_),
    predicted.rate = fleet.rate.excl.i.target * vessel.ratio,
    days.component = if_else(
      is.finite(actual.days) & actual.days > 0 & is.finite(avg.days) & avg.days > 0,
      log(actual.days) - log(avg.days), NA_real_
    ),
    rate.component = if_else(
      is.finite(actual.rate) & actual.rate > 0 & is.finite(predicted.rate) & predicted.rate > 0,
      log(actual.rate) - log(predicted.rate), NA_real_
    ),
    total.log.deviation = if_else(
      is.finite(actual.revenue) & actual.revenue > 0 & is.finite(predicted.revenue) & predicted.revenue > 0,
      log(actual.revenue) - log(predicted.revenue), NA_real_
    ),
    revenue.basis.ratio = if_else(
      is.finite(revenue.clean.target) & revenue.clean.target > 0 & is.finite(predicted.revenue),
      actual.revenue / revenue.clean.target, NA_real_
    )
  )

check("Decomposition identity holds exactly for every fabricated row",
      all(abs(decomposition_rows$total.log.deviation -
                (decomposition_rows$days.component + decomposition_rows$rate.component)) < 1e-10))
check("Row 3 (specialist-like, actual == predicted exactly) has total.log.deviation == 0",
      near(decomposition_rows$total.log.deviation[3], 0))
check("Row 3 days.component == 0 and rate.component == 0 individually",
      near(decomposition_rows$days.component[3], 0) && near(decomposition_rows$rate.component[3], 0))
check("Row 1 days.component == log(12/10)", near(decomposition_rows$days.component[1], log(12 / 10)))
check("Row 1 rate.component == log((1200/12) / (11 * 1.2))",
      near(decomposition_rows$rate.component[1], log((1200 / 12) / (11 * 1.2))))
check("exp(total.log.deviation) recovers actual.revenue / predicted.revenue exactly (row 2, offsetting case)",
      near(exp(decomposition_rows$total.log.deviation[2]),
           decomposition_rows$actual.revenue[2] / decomposition_rows$predicted.revenue[2]))
check("Row 4 revenue.basis.ratio == 1320/1000 == 1.32 (the deliberate wedge)",
      near(decomposition_rows$revenue.basis.ratio[4], 1320 / 1000))
check("Row 4 days.component == log(12/10) (nonzero, combined with the wedge, unlike the row this replaced)",
      near(decomposition_rows$days.component[4], log(12 / 10)))
check(paste("Row 4 decomposition identity STILL holds exactly despite the revenue-basis wedge (item 5's own caveat,",
            "the identity is pure algebra, independent of revenue.clean.target)"),
      near(decomposition_rows$total.log.deviation[4],
           decomposition_rows$days.component[4] + decomposition_rows$rate.component[4]))
check("Rows 1-3 revenue.basis.ratio == 1 exactly (no wedge, revenue.clean.target == actual.revenue by construction)",
      all(sapply(1:3, function(i) near(decomposition_rows$revenue.basis.ratio[i], 1))))

# ============================================================================
# Verdict
# ============================================================================

if (length(failures) > 0) {
  stop("Synthetic-data test failed:\n", paste(" -", failures, collapse = "\n"))
}
message("\nAll synthetic-data correctness checks passed.")
