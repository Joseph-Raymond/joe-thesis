# Chapter 3 empirical pipeline, DRAFT extension to Section 7
#
# NOT wired into run_all.R yet and not assigned a chapter table number.
# This is a first pass at a question raised in review, not yet approved,
# kept in its own file rather than touching 08_state_contingent_activation.R
# until the design below is checked. Reviewed once by a second pass focused
# on logic only (no access to real data), see the revision notes marked
# REVIEW below for what that pass changed and why.
#
# Table 10 (08_state_contingent_activation.R) asks an EXTENSIVE-margin
# question, does a held-but-currently-unfished permit turn on (0 to
# positive landings) when the vessel's primary fishery has a bad year. Its
# outcome is binary and its candidate sample is every held, non-primary
# fishery regardless of whether the vessel has ever actually fished it.
#
# This script asks a different, INTENSIVE-margin question about a
# different, narrower population, fisheries the vessel is already in the
# recurring habit of fishing a little (not fully dormant, not the primary
# fishery either), does the SIZE of that use move with the same shock. A
# fishery already fished in most years is already coded 1 in Table 10's
# binary sense regardless of how much more or less it gets used, so Table
# 10's design cannot see this margin at all. The two designs overlap in
# population (this script's sample is a subset of Table 10's candidates,
# restricted further to ones with an established first-half fishing
# history) and share the same shock and vessel/fishery-year fixed-effect
# structure, so the two results are meant to be read side by side, not as
# substitutes.
#
# REVIEW, outcome variable. The first draft of this script used realized
# revenue SHARE as the outcome. That is wrong, not just imprecise, because
# share = revenue_j / vessel.year.rev has the vessel's OWN primary-fishery
# revenue sitting in the denominator (01_build_panel.R's vessel_share_panel
# construction). A bad primary-fishery year shrinks that denominator, which
# arithmetically raises every other fishery's share even if the vessel
# lands the exact same pounds in it that year, d(share_j)/d(y_primary) =
# -y_j / R^2 < 0 always, with no behavioral content at all. Neither the
# vessel nor the fishery-year fixed effect absorbs this, since the
# contamination varies at the vessel-year level, exactly the level the
# shock itself is identified from. A negative coefficient on share would
# therefore be close to guaranteed by construction, not evidence of
# anything. The fix below switches the outcome to a LEVEL (the secondary
# fishery's own landed pounds, estimated with a Poisson pseudo-likelihood
# fit rather than a linear one, since pounds are non-negative and zero-
# heavy), which has no such mechanical channel back to the primary
# fishery's own outcome. Quantity rather than revenue also strips the
# common price channel, the same reason 08's own header gives for building
# the shock itself off pounds.
#
# Reuses:
#   - vessel_share_panel (01_build_panel.R) only to define which fisheries
#     count as "established" for a vessel (see Section 2), not as the
#     regression outcome itself (see REVIEW note above).
#   - vessel_year_shock (saved by 08_state_contingent_activation.R to
#     ch3_activation.rdata), the leave-one-out standardized quantity shock
#     to each vessel's predetermined primary fishery, already restricted to
#     each vessel's own second-half years. Reused rather than rebuilt, same
#     reasoning 06c_tau_shock_validation.R gives for doing the same thing.
#
# Rebuilds (rather than reuses) the first-half/second-half year split, the
# held-prior-year flag, and the ticket-level quantity aggregation, all
# needed below and all cheap to rebuild, following the same "runnable on
# its own" convention 08_state_contingent_activation.R follows relative to
# 07_behavioral_heterogeneity.R.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_fishery_year") || !exists("vessel_share_panel")) load(panel_path)
if (!exists("vessel_year_shock")) load(file.path(intermediate_dir, "ch3_activation.rdata"))

# ============================================================================
# 1. Rebuild the first-half / second-half split (matches 08's Section 1)
# ============================================================================

vessel_year_ordinal <- vessel_share_panel %>%
  distinct(Vessel.ADFG.Number, Batch.Year) %>%
  arrange(Vessel.ADFG.Number, Batch.Year) %>%
  group_by(Vessel.ADFG.Number) %>%
  mutate(
    year.rank = row_number(),
    n.years   = n(),
    half      = if_else(year.rank <= ceiling(n.years / 2), "first", "second")
  ) %>%
  ungroup()

first_half_years <- vessel_year_ordinal %>% filter(half == "first") %>%
  select(Vessel.ADFG.Number, Batch.Year)

n_first_half_years <- first_half_years %>% count(Vessel.ADFG.Number, name = "n.first.half.years")

# ============================================================================
# 2. Established secondary fisheries
# ============================================================================
#
# A non-primary fishery counts as "established" for a vessel if, within
# that vessel's own first-half years (predetermined, so a fishery cannot
# earn this status off the very years whose shock response is later being
# tested),
#   (a) it was actually fished (share > 0) in at least MIN_SECONDARY_YEARS
#       distinct years, AND at least MIN_SECONDARY_FRACTION of all its
#       first-half years, and
#   (b) its mean share across ALL first-half years (zero-filled, i.e.
#       counting years it was not fished as a zero, not just the years it
#       was) stays below MAX_SECONDARY_SHARE.
#
# REVIEW, both (a)'s fraction and (b) were added. The original draft used
# only a flat count (>= 2 first-half years fished), which is not scale-
# free, 2 of 3 first-half years is a genuine habit, 2 of 15 is closer to
# incidental, the opposite of what "established" is meant to mean here.
# The mean-share cap in (b) is what actually enforces "fished a LITTLE",
# without it a fishery a vessel fishes almost as hard as its primary one
# (a near-co-primary rather than a true secondary option) could still pass
# a purely frequency-based test.
#
# None of these three thresholds are derived from the data, they are
# judgment calls flagged here for sensitivity testing (try
# MIN_SECONDARY_YEARS in 1:3 and MAX_SECONDARY_SHARE in c(0.2, 0.3, 0.4))
# once this runs against the real panel, not settled facts.
MIN_SECONDARY_YEARS    <- 2
MIN_SECONDARY_FRACTION <- 1/3
MAX_SECONDARY_SHARE    <- 0.3

first_half_fishery_stats <- vessel_share_panel %>%
  semi_join(first_half_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  group_by(Vessel.ADFG.Number, Fishery) %>%
  summarise(
    n.first.half.fished.years = sum(share > 0),
    first.half.mean.share     = mean(share),
    .groups = "drop"
  ) %>%
  inner_join(n_first_half_years, by = "Vessel.ADFG.Number")

vessel_primary <- vessel_year_shock %>% distinct(Vessel.ADFG.Number, primary.fishery)

secondary_fisheries <- first_half_fishery_stats %>%
  inner_join(vessel_primary, by = "Vessel.ADFG.Number") %>%
  filter(
    Fishery != primary.fishery,
    n.first.half.fished.years >= MIN_SECONDARY_YEARS,
    n.first.half.fished.years >= MIN_SECONDARY_FRACTION * n.first.half.years,
    first.half.mean.share < MAX_SECONDARY_SHARE
  ) %>%
  select(Vessel.ADFG.Number, Fishery)

cat("Vessel x established-secondary-fishery pairs:", nrow(secondary_fisheries), "\n")
cat("Distinct vessels with at least one established secondary fishery:",
    n_distinct(secondary_fisheries$Vessel.ADFG.Number), "\n")

# ============================================================================
# 3. Held-in-t-1 flag, same construction as 08's Section 1
# ============================================================================
#
# Requires the fishery to have been HELD the prior year, i.e. that the
# option was predetermined as of the start of year t, the same
# predetermination logic Table 10's own held.lag applies. This is a
# predetermination check, not a guarantee that the permit is still held
# all the way through year t, a permit that lapses mid-way still enters
# this sample and still shows share/pounds falling to whatever it falls to
# in year t, which is the correct behavior (Table 10 treats a lapsed
# permit the same way). REVIEW, an earlier comment here claimed this
# restriction distinguishes "permit lapsed" from "held but unfished"
# outcomes, it does not, both land at the same zero, and this comment is
# corrected to say only what the restriction actually does.
held_prior_year <- vessel_fishery_year %>%
  filter(held) %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  mutate(Batch.Year = Batch.Year + 1, held.lag = TRUE)

# ============================================================================
# 4. Ticket-level quantity, same construction as 08's Section 2
# ============================================================================
#
# 08 only carries vessel_fishery_year_quantity forward as an input to the
# PRIMARY fishery's shock. The same object is indexed by (vessel, Fishery,
# Batch.Year) for every fishery, not only primary ones, so it is reused
# here unchanged for the secondary fishery's own landed pounds, the new
# outcome variable (see REVIEW note at the top of this file). Rebuilt from
# the raw ticket file rather than reloaded from 08's saved objects, since
# 08 does not currently save vessel_fishery_year_quantity, only
# activation_data and vessel_year_shock.

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp[["Pounds..Detail."]] <- as.numeric(catch_data_temp[["Pounds..Detail."]])

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
  filter(Fishery != "")

vessel_fishery_year_quantity <- catch_data_temp %>%
  group_by(Vessel.ADFG.Number, Fishery, Batch.Year) %>%
  summarise(own.pounds = sum(Pounds..Detail., na.rm = TRUE), .groups = "drop")

# ============================================================================
# 5. Assemble the regression panel
# ============================================================================
#
# One row per (vessel, established secondary fishery, second-half year)
# with that fishery held the prior year and a computable primary-fishery
# shock. own.pounds is 0, not missing, whenever no ticket row exists for
# that (vessel, fishery, year) cell, the expected, common case of "did not
# fish it that year" rather than an edge case, exactly how 08 itself
# zero-fills own.pounds.primary in its own Section 2.

intensity_data <- secondary_fisheries %>%
  inner_join(held_prior_year, by = c("Vessel.ADFG.Number", "Fishery")) %>%
  inner_join(vessel_year_shock, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  filter(Fishery != primary.fishery, is.finite(shock)) %>%
  left_join(vessel_fishery_year_quantity, by = c("Vessel.ADFG.Number", "Fishery", "Batch.Year")) %>%
  mutate(
    own.pounds   = replace_na(own.pounds, 0),
    fishery.year = paste(Fishery, Batch.Year, sep = "_")
  )

cat("Secondary-fishery intensity regression sample:", nrow(intensity_data), "\n")
cat("Mean own.pounds among established-secondary-fishery vessel-years:",
    round(mean(intensity_data$own.pounds), 1), "\n")
cat("Share of these vessel-years with own.pounds == 0 (established fishery went unfished that year):",
    round(mean(intensity_data$own.pounds == 0), 4), "\n")

# ============================================================================
# 6. Primary spec. own.pounds ~ shock, vessel + fishery-year FE, Poisson
# ============================================================================
#
# fepois rather than a linear fit, own.pounds is a non-negative, zero-heavy
# count-like quantity, and a Poisson pseudo-likelihood fit handles the
# zeros directly rather than needing a log(x + 1) transform or truncating
# them away, and estimates a proportional (semi-elasticity) effect of the
# shock the same way 08's own linear probability model reads as an average
# marginal effect on a probability.
#
# Same fixed-effect structure and clustering as Table 10, for direct
# comparability, and cluster = ~primary.fishery for the identical reason
# given there, shock varies only at (vessel, year) via the vessel's fixed
# primary fishery, so a vessel-clustered SE would treat vessels sharing a
# primary fishery as independent draws of what is one common shock. Vessel
# is nested inside primary.fishery (each vessel has exactly one), so
# clustering on primary.fishery alone already contains whatever within-
# vessel dependence there is, including the compositional link across a
# vessel's own several secondary fisheries in the same year flagged in an
# earlier version of this comment, that link is a reason to be careful
# reading the coefficient as an independent per-fishery effect, but it is
# not a reason to add a second cluster dimension.
#
# Predicted sign, negative. A bad year in the primary fishery (shock below
# its own mean) should raise landed pounds in an already-established
# secondary fishery. Unlike the share outcome the original draft used, this
# outcome has no mechanical channel back to the primary fishery's own
# revenue, so a negative estimate here would actually be evidence of
# reallocation rather than an artifact of a shared denominator.
model_intensity <- fepois(own.pounds ~ shock | Vessel.ADFG.Number + fishery.year,
                           data = intensity_data, cluster = ~primary.fishery)

print(etable(model_intensity, headers = "Secondary-fishery landed pounds (Poisson)"))

etable(
  model_intensity,
  headers = c("Secondary-fishery landed pounds"),
  tex = TRUE,
  file = file.path(table_dir, "draft_table_secondary_fishery_intensity.tex"),
  replace = TRUE
)

cat("Wrote draft_table_secondary_fishery_intensity.tex\n")

# ============================================================================
# 7. Placebo, future shock, same logic as Table 11
# ============================================================================
#
# Same conditional-placebo logic as 08's Table 11, both the current and
# next year's shock enter one regression on one sample, so the test is
# whether next year's shock adds anything once this year's is already in
# the model, which a merely-autocorrelated shock series cannot do on its
# own. REVIEW, the original draft skipped refitting the current-shock-only
# model on the placebo sample, added back here so the two columns are
# comparable on identical rows the way Table 11 itself requires (only the
# regressor set changes across columns, never the row set).
#
# Also note this placebo cannot detect the denominator artifact the REVIEW
# note at the top of this file describes, that problem was specific to a
# share outcome and operated only contemporaneously, so it would have
# passed this exact placebo cleanly while still being spurious. The
# placebo checks timing, not whether the outcome itself is mechanically
# tied to the shock, that has to be reasoned through separately (which is
# why the outcome was changed rather than left for this test to catch).

vessel_year_shock_future <- vessel_year_shock %>%
  transmute(Vessel.ADFG.Number, Batch.Year = Batch.Year - 1, shock.future = shock)

intensity_data_placebo <- intensity_data %>%
  left_join(vessel_year_shock_future, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  filter(is.finite(shock.future))

cat("Placebo sample (current and future shock both available):", nrow(intensity_data_placebo), "\n")

model_intensity_placebo_sample <- fepois(own.pounds ~ shock | Vessel.ADFG.Number + fishery.year,
                                          data = intensity_data_placebo, cluster = ~primary.fishery)
model_intensity_placebo_joint  <- fepois(own.pounds ~ shock + shock.future | Vessel.ADFG.Number + fishery.year,
                                          data = intensity_data_placebo, cluster = ~primary.fishery)

print(etable(model_intensity_placebo_sample, model_intensity_placebo_joint,
             headers = c("Current shock only", "Current + future shock")))

# ============================================================================
# 8. Identification check, same form as Table 10's
# ============================================================================

identification_check <- intensity_data %>%
  group_by(fishery.year) %>%
  summarise(n.distinct.primary = n_distinct(primary.fishery), n.obs = n(), .groups = "drop")

identifying_cells <- identification_check %>% filter(n.distinct.primary > 1)

cat("Identification, fishery-year cells with more than one distinct primary fishery:",
    nrow(identifying_cells), "of", nrow(identification_check),
    ", covering", sum(identifying_cells$n.obs), "of", nrow(intensity_data), "observations\n")

# ============================================================================
# Suggested next steps, not implemented here
# ============================================================================
#
# A second review pass on this design (logic only, no access to real data)
# suggested two cleaner complementary cuts worth adding once this core
# spec is agreed on, kept as a note rather than code so this file stays
# focused on the one design it was built to test.
#
#   1. Vessel-year collapse. Total non-primary revenue or pounds, one row
#      per vessel-year, vessel + year FE. Removes the adding-up constraint
#      across a vessel's several secondary fisheries entirely rather than
#      just noting it as a caveat, at the cost of no longer identifying
#      which specific fishery absorbs the reallocation.
#   2. Predetermined-capacity interaction. Run on the full activation_data
#      population (not just the established-secondary subset here) and
#      interact shock with each vessel's first-half COUNT of established
#      secondary fisheries, asking directly whether having more habitual
#      backup options buffers the primary-fishery shock more, immune to
#      the denominator artifact this file's outcome variable was changed
#      to avoid.
