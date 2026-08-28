# Chapter 3 empirical pipeline, Chapter3_outline.md Section 5, Figure 7
#
# Figure 7 [maybe]. Switch events between consecutive trips on different
#          permits, weighted by the cross-fishery co-participation distance
#          measure Section 5's own intro paragraph describes, net.distance =
#          1 - net.similarity, the same held-based, all-years-pooled Jaccard
#          measure 10_network_similarity.R builds for Table 13.
# Figure 7b. Same comparison as Figure 7 (observed switch events vs. a
#          within-vessel-year null, see Section 6), rescaled to a pooled
#          distance percentile instead of raw distance, in the same spirit
#          as the "one spike, thin tail" fix Figure 5b applies to
#          weekly.switching, see Section 6 below for why the raw scale is
#          expected to be hard to read here too and why this is a rescaling,
#          not a uniform-benchmark test.
#
# Descriptive only, matching the outline's own scope for this item, no table,
# no regression.
#
# NUMBERED 11 for run_all.R ordering convenience only (it sources after
# 10_network_similarity.R there). Its REAL dependencies are 01_build_panel.R
# (vessel_fishery_year, for the network) and 06_within_season_reallocation.R
# (switching_by_vessel_year, for the face-validity cross-check in Section 8
# below), not 07-10. The network is rebuilt LOCALLY in Section 1 below, a
# condensed copy of 10_network_similarity.R's own Section 1, rather than
# loaded from a saved object of 10_'s, on purpose, so a plot that has nothing
# to do with activation or seasonal overlap does not transitively depend on
# 08_state_contingent_activation.R / 09_seasonal_overlap.R having run
# cleanly. This is not a new pattern, rolling_periods/10b_network_similarity_rolling.R
# and rolling_periods/diagnostic_rolling_primary_fishery_clustering.R already
# each independently rebuild the identical network rather than sharing a
# saved copy, this is a third instance of that same convention, and
# 10_network_similarity.R's own "writes no new intermediate data" claim
# stays true.
#
# NO ROLLING TWIN. The network is all-years pooled and period-invariant by
# explicit design in both 10_network_similarity.R and
# rolling_periods/10b_network_similarity_rolling.R, and nothing below is
# grouped by calendar period or rolling window, no fixed effect, no
# regression, just a pooled distribution over switch events. This is the
# same reasoning rolling_periods/06b_within_season_reallocation_rolling.R
# already uses to skip Figure 5/6 (vessel-year / fishery-year grain objects
# that do not depend on how years get grouped into periods downstream), see
# its header. README_rolling.md notes Figure 7 lives here, not as an 11b_
# file, so it is not missed while the rolling pipeline is primary.
#
# TRIP DEFINITION. A trip is one vessel's one landing date
# (Vessel.ADFG.Number x Date.Landed), matching get.trip() in myfunctions.R
# (already sourced by 00_setup.R). get.trip() itself is not called, it tags
# a trip id but does not resolve which fishery a multi-permit day should
# count as for sequencing purposes, a real modeling choice this script has
# to make explicitly, see Section 4.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year, MAX_YEAR) and
# intermediate data/ch3_within_season.rdata (switching_by_vessel_year, saved
# by 06_). Reloads raw intermediate data/catch_data_temp.rdata directly, the
# identical cleaning block 06_within_season_reallocation.R Section 1 uses,
# verbatim, duplicated on purpose per this pipeline's established convention
# of not factoring ticket-reload cleaning out across scripts. Writes
# figure7_switch_events_by_network_distance.png and
# figure7b_switch_events_similarity_percentile.png to Chpt3/output/figures/.
# Writes no new intermediate data, no other script reads from this one.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_fishery_year") || !exists("MAX_YEAR")) load(panel_path)

within_season_path <- file.path(intermediate_dir, "ch3_within_season.rdata")
if (!exists("switching_by_vessel_year")) load(within_season_path)

# ============================================================================
# 1. Cross-fishery co-participation network (held-based, all years pooled)
# ============================================================================
#
# Condensed copy of 10_network_similarity.R Section 1, rebuilt locally here
# rather than shared, see header. Full rationale for HELD not FISHED, VESSELS
# not permit holders, and ALL YEARS POOLED lives there, not repeated here.
# MIN_NETWORK_VESSELS matches 10_'s own floor exactly, a judgment call, CHECK
# it there if it ever changes, it needs to change in both places.

held_pairs <- vessel_fishery_year %>%
  filter(held) %>%
  distinct(Vessel.ADFG.Number, Fishery)

fishery_size <- held_pairs %>% count(Fishery, name = "N_f")

MIN_NETWORK_VESSELS <- 10
fishery_size_filtered <- fishery_size %>% filter(N_f >= MIN_NETWORK_VESSELS)
surviving_fisheries <- fishery_size_filtered$Fishery
held_pairs_filtered <- held_pairs %>% filter(Fishery %in% surviving_fisheries)

cat("Fisheries surviving MIN_NETWORK_VESSELS >=", MIN_NETWORK_VESSELS, ":",
    length(surviving_fisheries), "of", nrow(fishery_size), "\n")

held_pairs_self <- held_pairs_filtered %>%
  inner_join(held_pairs_filtered, by = "Vessel.ADFG.Number",
             suffix = c(".A", ".B"), relationship = "many-to-many")

co_vessels_long <- held_pairs_self %>% count(Fishery.A, Fishery.B, name = "co.vessels")

network_long <- co_vessels_long %>%
  complete(Fishery.A = surviving_fisheries, Fishery.B = surviving_fisheries,
           fill = list(co.vessels = 0)) %>%
  left_join(fishery_size_filtered %>% rename(Fishery.A = Fishery, n.vessels.A = N_f), by = "Fishery.A") %>%
  left_join(fishery_size_filtered %>% rename(Fishery.B = Fishery, n.vessels.B = N_f), by = "Fishery.B") %>%
  mutate(
    net.similarity      = co.vessels / (n.vessels.A + n.vessels.B - co.vessels),
    net.ochiai          = co.vessels / sqrt(n.vessels.A * n.vessels.B),
    net.distance        = 1 - net.similarity,
    net.distance.ochiai = 1 - net.ochiai
  )

cat("network_long, fishery pairs with a computable similarity:", nrow(network_long), "\n")

# ============================================================================
# 2. Reload raw fish tickets, trip grain this time
# ============================================================================
#
# Identical cleaning steps to 06_within_season_reallocation.R Section 1,
# duplicated on purpose (see header). Statistical.Week itself is never
# referenced again below, kept only because its derivation's
# !is.na(Statistical.Week) filter is what guards against an unparseable
# Date.Landed reaching Section 4's trip collapse, dropping that line would
# silently drop the NA-date guard along with it.

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp[["CFEC.Value..Detail."]][is.na(catch_data_temp[["CFEC.Value..Detail."]])] <- 0
catch_data_temp[["Pounds..Detail."]] <- as.numeric(catch_data_temp[["Pounds..Detail."]])

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(
    Fishery = strip_fishery_space(CFEC.Permit.Fishery),
    Statistical.Week = derive_statistical_week(Date.Landed)
  ) %>%
  filter(Fishery != "", !is.na(Statistical.Week))

cat("Ticket rows entering the trip-level switch-event panel:", nrow(catch_data_temp), "\n")

# ============================================================================
# 3. Join-safety check, ticket-side (stripped) vs register-side (unstripped)
#    Fishery codes
# ============================================================================
#
# Same failure mode 10_network_similarity.R Section 3.4 guards against
# (01_build_panel.R applies strip_fishery_space() to the ticket side only,
# never to the register-side Fishery column the network above is built
# from), checked independently here rather than assumed, since this script's
# switch events are ticket-derived and the network is register-derived, a
# different join than 10_'s own check covers.

MIN_CODE_MATCH_RATE <- 0.05 # "near zero" tripwire, not a judgment about coverage quality, see Section 5 for the real coverage read

ticket_fisheries <- unique(catch_data_temp$Fishery)
code_match_rate <- mean(ticket_fisheries %in% surviving_fisheries)
cat("Share of distinct ticket-side Fishery codes present in the held-based network:",
    round(code_match_rate, 4), "\n")

if (code_match_rate < MIN_CODE_MATCH_RATE) {
  stop(
    "Section 3 join-safety check failed, only ", round(100 * code_match_rate, 2),
    "% of ticket-side Fishery codes are present in the held-based network. ",
    "Register-side and ticket-side Fishery codes likely disagree on whitespace/",
    "formatting, see 10_network_similarity.R Section 3.4 for the same check ",
    "on a different join and its fix (strip_fishery_space() on the ",
    "register-side Fishery column in 01_build_panel.R)."
  )
}

# ============================================================================
# 4. Collapse to one row per trip (Vessel.ADFG.Number x Batch.Year x
#    Date.Landed), dominant fishery by revenue
# ============================================================================
#
# A trip can span more than one Fishery (a vessel landing two permits the
# same day). The PRIMARY sequence below assigns each such trip-day to its
# higher-revenue fishery, tie-broken on Pounds..Detail. (revenue can be
# exactly 0 or negative for a zero-filled or correction/refund ticket, see
# 06_ Section 5's comment on that), then on Fishery code alphabetically as a
# final deterministic tiebreak, so no row-order dependency remains. A
# SENSITIVITY sequence built alongside it instead drops multi-fishery
# trip-days outright, see Section 5, since silently picking a winner asserts
# something (this was the day's "real" activity) that a same-day two-permit
# landing does not obviously support, arguably it is the cheapest possible
# switch in the whole dataset rather than not a switch at all.
#
# Batch.Year is added to the trip key on top of get.trip()'s own
# (Vessel.ADFG.Number, Date.Landed) definition, needed so Section 5's
# within-year lag never crosses a Batch.Year boundary. CHECK, this assumes a
# single Date.Landed never spans two Batch.Year values for the same vessel,
# printed below rather than assumed, if this ever prints a nonzero count the
# trip key needs Batch.Year resolved some other way before trusting Section 5.

trip_year_check <- catch_data_temp %>%
  distinct(Vessel.ADFG.Number, Date.Landed, Batch.Year) %>%
  count(Vessel.ADFG.Number, Date.Landed, name = "n.batch.years") %>%
  filter(n.batch.years > 1)

cat("Vessel-dates spanning more than one Batch.Year (should be ~0):", nrow(trip_year_check), "\n")

vessel_day_fishery <- catch_data_temp %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Date.Landed, Fishery) %>%
  summarise(revenue = sum(CFEC.Value..Detail., na.rm = TRUE),
            pounds  = sum(Pounds..Detail., na.rm = TRUE), .groups = "drop")

n_trip_days <- vessel_day_fishery %>% distinct(Vessel.ADFG.Number, Batch.Year, Date.Landed) %>% nrow()

multi_fishery_days <- vessel_day_fishery %>%
  count(Vessel.ADFG.Number, Batch.Year, Date.Landed, name = "n.fisheries") %>%
  filter(n.fisheries > 1)

cat("Trip-days spanning more than one Fishery code:", nrow(multi_fishery_days), "of", n_trip_days,
    "(", scales::percent(nrow(multi_fishery_days) / n_trip_days, accuracy = 0.1), ")\n")

# A trip-day only actually falls to the pounds/Fishery tiebreak when two or
# more of its fisheries are TIED at the maximum revenue that day (not merely
# when the maximum happens to be <= 0, a unique negative-revenue max, e.g.
# one correction ticket and nothing else, is decided by revenue alone, no
# tiebreak involved).
n_revenue_ties <- vessel_day_fishery %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Date.Landed) %>%
  filter(n() > 1, sum(revenue == max(revenue)) > 1) %>%
  ungroup() %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Date.Landed) %>%
  nrow()

cat("Multi-fishery trip-days with a revenue tie for the top fishery (falls to the pounds/Fishery tiebreak):",
    n_revenue_ties, "\n")

# PRIMARY sequence, dominant fishery kept for every trip-day. arrange() then
# slice(1) within group, not slice_max(), so the full tiebreak chain
# (revenue, then pounds, then Fishery) is explicit and deterministic rather
# than relying on slice_max()'s own tie-handling.
vessel_day_dominant <- vessel_day_fishery %>%
  arrange(Vessel.ADFG.Number, Batch.Year, Date.Landed, desc(revenue), desc(pounds), Fishery) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Date.Landed) %>%
  slice(1) %>%
  ungroup()

# SENSITIVITY sequence, multi-fishery trip-days dropped rather than assigned
# a winner, so the lag chain in Section 5 skips straight to the next
# single-fishery trip.
vessel_day_single <- vessel_day_fishery %>%
  anti_join(multi_fishery_days, by = c("Vessel.ADFG.Number", "Batch.Year", "Date.Landed"))

cat("Trip-days entering the PRIMARY (dominant-fishery) sequence:", nrow(vessel_day_dominant), "\n")
cat("Trip-days entering the SENSITIVITY (multi-fishery days dropped) sequence:", nrow(vessel_day_single), "\n")

# ============================================================================
# 5. Switch events, consecutive trips within a vessel-year, network distance
# ============================================================================
#
# Lagged WITHIN (Vessel.ADFG.Number, Batch.Year) only, not across year
# boundaries, mirrors 06_ Section 3's reasoning for weekly turnover, the
# first trip of a season is never scored as a switch from the last trip of
# the prior season across the off-season gap.

detect_switch_events <- function(trip_data) {
  trip_data %>%
    arrange(Vessel.ADFG.Number, Batch.Year, Date.Landed) %>%
    group_by(Vessel.ADFG.Number, Batch.Year) %>%
    mutate(Fishery.prev = lag(Fishery, order_by = Date.Landed), n.trips.vessel.year = n()) %>%
    ungroup() %>%
    filter(!is.na(Fishery.prev), Fishery != Fishery.prev)
}

switch_events_primary    <- detect_switch_events(vessel_day_dominant)
switch_events_sensitivity <- detect_switch_events(vessel_day_single)

cat("Switch events, PRIMARY sequence:", nrow(switch_events_primary), "\n")
cat("Switch events, SENSITIVITY sequence:", nrow(switch_events_sensitivity), "\n")

# Network-distance join. Both directions of network_long are identical by
# construction, held_pairs_self in Section 1 is a symmetric self-join and
# complete() fills the full surviving_fisheries x surviving_fisheries grid,
# so joining Fishery.prev -> Fishery.A and Fishery -> Fishery.B is safe
# regardless of which fishery came first.
join_network_distance <- function(events) {
  events %>%
    left_join(
      network_long %>% select(Fishery.A, Fishery.B, net.similarity, net.ochiai, net.distance, net.distance.ochiai),
      by = c("Fishery.prev" = "Fishery.A", "Fishery" = "Fishery.B")
    )
}

switch_events_primary     <- join_network_distance(switch_events_primary)
switch_events_sensitivity <- join_network_distance(switch_events_sensitivity)

cat("\nSensitivity check, multi-fishery trip-days dropped instead of dominant-assigned:\n")
cat("  PRIMARY:     ", nrow(switch_events_primary), "switch events, mean net.distance",
    round(mean(switch_events_primary$net.distance, na.rm = TRUE), 4), "\n")
cat("  SENSITIVITY: ", nrow(switch_events_sensitivity), "switch events, mean net.distance",
    round(mean(switch_events_sensitivity$net.distance, na.rm = TRUE), 4), "\n")

cat("Correlation, net.distance (Jaccard) vs net.distance.ochiai across PRIMARY switch events:",
    round(cor(switch_events_primary$net.distance, switch_events_primary$net.distance.ochiai, use = "complete.obs"), 4), "\n")

# Three-bucket coverage diagnostic. A switch event's net.distance is NA only
# because the join found no row, which per Section 1's complete() grid means
# exactly one of two things, the fishery never appears in the held panel at
# all, or it does but fails MIN_NETWORK_VESSELS. bucket_of() returns the
# worse of the two fisheries' own status (pmin), since one uncovered side of
# a pair is enough to make the pair's distance undefined.
bucket_of <- function(code) {
  if_else(code %in% surviving_fisheries, 2L,
  if_else(code %in% fishery_size$Fishery, 1L, 0L))
}

switch_coverage <- switch_events_primary %>%
  mutate(
    bucket.code = pmin(bucket_of(Fishery.prev), bucket_of(Fishery)),
    bucket = case_when(
      bucket.code == 2L ~ "covered",
      bucket.code == 1L ~ "below MIN_NETWORK_VESSELS",
      TRUE              ~ "absent from held panel entirely"
    )
  ) %>%
  count(bucket) %>%
  mutate(share = round(n / sum(n), 4))

cat("\nSwitch-event network coverage (PRIMARY sequence):\n")
print(switch_coverage)

# ============================================================================
# 6. A valid null, WITHIN-VESSEL-YEAR fishery pairs, then Figure 7/7b
# ============================================================================
#
# An earlier version of this section compared switch events against ALL
# ordered fishery pairs in network_long. That comparison is close to
# tautological. A switch event requires the SAME vessel to have landed both
# fisheries, so an observed pair has co.vessels >= 1 essentially by
# construction, while the all-pairs universe is dominated by pairs no vessel
# has ever held both sides of (co.vessels == 0, net.distance exactly 1,
# 10_network_similarity.R's own Section 3 top-1%-mass check, run on the
# Table 13 estimation sample, already shows how concentrated net.similarity
# is even after that floor). Comparing observed switches against that
# universe would show a large gap regardless of how vessels actually choose
# what to switch into.
#
# The null below instead holds each vessel-year's OWN set of fished
# fisheries fixed (from vessel_day_dominant, the PRIMARY sequence) and asks
# what the distance distribution would look like if that vessel's switches
# were a random pairing among fisheries it already touched that year, rather
# than the order actually observed. This controls for which fisheries a
# vessel holds at all (the confound that made the all-pairs comparison close
# to circular).
#
# WEIGHTED per vessel-year, not left as one row per candidate pair. A
# vessel-year with K distinct fisheries contributes K*(K-1) candidate pairs
# to an unweighted null but only n.covered.switch.events rows to the
# observed side, two numbers with no reason to track each other, a
# high-switch-count vessel-year whose fisheries happen to sit close together
# would then dominate the null far out of proportion to how much it
# contributes to the observed side, manufacturing a gap between the two
# that has nothing to do with whether SEQUENCING leans toward closer pairs
# (verified by simulation, a world where the true sequencing null holds
# EXACTLY by construction still showed a spurious observed-below-null gap of
# -0.26 on the [0,1] distance scale, purely from that composition effect,
# before this weighting was added). Each vessel-year's candidate pairs are
# given a per-pair weight of n.covered.switch.events / (COVERED candidate
# pairs in that vessel-year), COVERED because every consumer below (both
# figures, both weighted.mean() gap prints) drops uncovered (NA-distance)
# pairs first, dividing by K*(K-1) instead (ALL candidate pairs, covered or
# not) would leave a vessel-year's surviving weight below
# n.covered.switch.events whenever some of its pairs are uncovered, a
# smaller, sign-flippable version of the same composition bias this
# weighting exists to remove (also caught by simulation). Normalizing by the
# covered count means the null and the observed side put exactly
# n.covered.switch.events worth of weight on every vessel-year that has any,
# among the rows that actually reach the figures. The covered-pair count for
# a vessel-year with n.covered.switch.events > 0 is never 0, an observed
# covered switch is itself one of that vessel-year's own covered candidate
# pairs, so the division below cannot be by zero. This still leaves the null
# uniform over DISTINCT covered fishery pairs within a vessel-year rather
# than over its trip multiset, a pair between two fisheries the vessel
# fished many times is not up-weighted the way an exact trip-permutation
# null would, a coarser but far cheaper stand-in.

vessel_year_fisheries <- vessel_day_dominant %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery)

vessel_year_switch_summary <- switch_events_primary %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  summarise(
    n.switch.events         = n(),
    n.covered.switch.events = sum(is.finite(net.distance)),
    total.switch.distance   = sum(net.distance, na.rm = TRUE),
    .groups = "drop"
  )

vessel_year_pairs_null <- vessel_year_fisheries %>%
  inner_join(vessel_year_fisheries, by = c("Vessel.ADFG.Number", "Batch.Year"),
             suffix = c(".from", ".to"), relationship = "many-to-many") %>%
  filter(Fishery.from != Fishery.to) %>%
  left_join(
    network_long %>% select(Fishery.A, Fishery.B, net.similarity, net.ochiai, net.distance, net.distance.ochiai),
    by = c("Fishery.from" = "Fishery.A", "Fishery.to" = "Fishery.B")
  ) %>%
  add_count(Vessel.ADFG.Number, Batch.Year, name = "n.pairs.in.vessel.year") %>%
  add_count(Vessel.ADFG.Number, Batch.Year, wt = as.integer(is.finite(net.distance)),
            name = "n.covered.pairs.in.vessel.year") %>%
  inner_join(
    vessel_year_switch_summary %>% select(Vessel.ADFG.Number, Batch.Year, n.covered.switch.events),
    by = c("Vessel.ADFG.Number", "Batch.Year")
  ) %>%
  filter(n.covered.switch.events > 0) %>%
  mutate(null.weight = n.covered.switch.events / n.covered.pairs.in.vessel.year)

cat("\nWithin-vessel-year candidate pairs (the null), vessel-years with >= 1 covered switch event only:",
    nrow(vessel_year_pairs_null),
    ", of which covered by the network:", sum(is.finite(vessel_year_pairs_null$net.distance)), "\n")

# The observed side of Figure 7/7b is restricted to COVERED switch events
# (Section 5's coverage table above), and coverage is not random, an
# uncovered fishery is one below MIN_NETWORK_VESSELS, which skews toward
# smaller, more specialized fisheries that a network built on 10+ ever-
# holders would generically place FAR from most others. Dropping those
# switches therefore skews the plotted observed distribution toward LOWER
# distance than the true full set of switches would show, printed here so
# the direction of that skew is on the record next to the figures it
# affects, not just in the Section 5 coverage table above.
cat("Observed switch events used in Figure 7/7b (covered only):",
    sum(is.finite(switch_events_primary$net.distance)), "of", nrow(switch_events_primary),
    "(", scales::percent(mean(is.finite(switch_events_primary$net.distance)), accuracy = 0.1), "), ",
    "excluded events skew toward smaller/more specialized fisheries, see Section 5's coverage table\n")

# Jaccard vs. Ochiai, both computed in Section 1, checked here rather than
# left unused. 10_'s own Table 12 refit treats this size-asymmetry choice as
# material enough to change a sign, so the observed-vs-null GAP is reported
# under both rather than trusting Jaccard alone. weighted.mean(w =
# null.weight) on the null side, not mean(), per the weighting note above,
# an unweighted mean() here would reintroduce the exact composition bias
# the weighting was added to remove.
cat("Mean net.distance (Jaccard), observed vs. WEIGHTED null:",
    round(mean(switch_events_primary$net.distance, na.rm = TRUE), 4), "vs.",
    round(weighted.mean(vessel_year_pairs_null$net.distance, vessel_year_pairs_null$null.weight, na.rm = TRUE), 4), "\n")
cat("Mean net.distance.ochiai, observed vs. WEIGHTED null:",
    round(mean(switch_events_primary$net.distance.ochiai, na.rm = TRUE), 4), "vs.",
    round(weighted.mean(vessel_year_pairs_null$net.distance.ochiai, vessel_year_pairs_null$null.weight, na.rm = TRUE), 4), "\n")

# Figure 7. Two histograms (not geom_density, which smears mass past the
# [0,1] bound at the point mass net.distance == 0 exactly, co.vessels ==
# n.vessels.A == n.vessels.B, and 1 exactly, co.vessels == 0), density-
# normalized (position = "identity", y = after_stat(density)) so the two
# groups are comparable despite very different N.
figure7_data <- bind_rows(
  switch_events_primary %>% filter(is.finite(net.distance)) %>%
    transmute(net.distance, weight = 1, source = "Observed switch events"),
  vessel_year_pairs_null %>% filter(is.finite(net.distance)) %>%
    transmute(net.distance, weight = null.weight, source = "Within-vessel-year candidate pairs (null)")
)

figure7 <- figure7_data %>%
  ggplot(aes(x = net.distance, weight = weight, fill = source, y = after_stat(density))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "white") +
  labs(
    title = "Observed fishery switches vs. a within-vessel-year null",
    subtitle = "Network distance = 1 - Jaccard co-participation similarity",
    x = "Network distance", y = "Density", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(figure_dir, "figure7_switch_events_by_network_distance.png"),
       figure7, width = 7, height = 5, dpi = 300)

cat("Wrote figure7_switch_events_by_network_distance.png\n")

# Figure 7b. Observed switches and the null pooled into ONE ranking (not
# ranked separately) before computing a percentile, rank() gives low rank to
# low distance, so 0 = most similar (cheapest) pair, 1 = least similar (most
# costly), the direction an earlier version of this section had backwards.
# The percentile SCALE itself is unweighted, rank() runs over the raw pooled
# rows, it is a fixed, monotonic rescaling of the x-axis, not a second
# statistical comparison. The null.weight reweighting that fixes the
# vessel-year composition bias (see the Section 6 comment above) still
# applies to bin HEIGHTS below, aes(weight = weight), exactly as it does in
# Figure 7.
#
# An earlier version of this figure compared the observed percentile against
# a flat dashed line asserted to be the "uniform, no-relationship" benchmark.
# That assertion is FALSE whenever the pooled distance distribution itself
# has a large point mass (checked directly, simulating a case where the
# observed group is drawn from the exact same distribution as the null with
# a 90%-at-zero point mass, over 90% of the "no relationship" observed
# percentiles land in a single 10%-wide bin, not spread flat at all,
# rank()'s mid-rank tie handling keeps the EXPECTED rank centered but says
# nothing about the shape of any one finite draw). So this figure instead
# plots the null's OWN percentile distribution as a second series, the same
# two-histogram comparison Figure 7 already makes, just on the rescaled
# percentile axis, which still does the useful part of a percentile
# transform (expanding the low-density tails, compressing the point masses)
# without asserting a uniformity property that does not actually hold.
pooled_distances <- figure7_data %>%
  mutate(distance.percentile = rank(net.distance) / n())

figure7b <- pooled_distances %>%
  ggplot(aes(x = distance.percentile, weight = weight, fill = source, y = after_stat(density))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "white") +
  scale_x_continuous(limits = c(0, 1)) +
  labs(
    title = "Same comparison, network-distance percentile",
    subtitle = "0 = most similar pair, 1 = least similar, pooled before ranking",
    x = "Network-distance percentile (observed + null pooled)", y = "Density", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(figure_dir, "figure7b_switch_events_similarity_percentile.png"),
       figure7b, width = 7, height = 5, dpi = 300)

cat("Wrote figure7b_switch_events_similarity_percentile.png\n")

# ============================================================================
# 7. Vessel-year distance-weighted switching intensity, descriptive summary
# ============================================================================
#
# Chapter3_outline.md Section 5's own language ("weighted by the cross-
# fishery distance measure") supports this per-vessel reading as well as the
# pooled distribution Figure 7 plots. Both are cheap to build, only the
# distribution feeds a figure, this stays a printed summary, not a saved
# object or a regression table, matching the outline's descriptive-only
# scope for this item.
#
# Base is every vessel-year with >= 2 trips (a genuine transition
# opportunity), zero-filled for a vessel-year with trips but no switch
# event, that is a real, measured zero (a vessel that fished more than once
# but never changed fishery), the same distinction 06_'s own
# weekly.switching draws between a vessel-year that is genuinely at zero
# and one with too few active weeks to measure anything at all. Only
# vessel-years with fewer than 2 trips (no possible transition) are excluded
# here, matching that same "unmeasurable is not zero" logic on the other
# side of it.
vessel_year_trip_base <- vessel_day_dominant %>%
  count(Vessel.ADFG.Number, Batch.Year, name = "n.trips") %>%
  filter(n.trips >= 2)

# vessel_year_switch_summary was already built in Section 6, for the null
# weighting, reused here rather than recomputed.

# total.switch.distance/switch.distance.per.transition below treat an
# uncovered switch (Section 5's coverage table) as contributing 0 distance,
# an explicit convention, not a silent one, "a switch happened but its cost
# is unmeasured" collapses into "cost 0" only for these SUM-based measures.
# mean.switch.distance is built separately and is NA, not 0, for a
# vessel-year whose switches are all uncovered.
vessel_year_switch_intensity <- vessel_year_trip_base %>%
  left_join(vessel_year_switch_summary, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  mutate(
    n.switch.events         = coalesce(n.switch.events, 0L),
    n.covered.switch.events = coalesce(n.covered.switch.events, 0L),
    total.switch.distance   = coalesce(total.switch.distance, 0),
    mean.switch.distance    = if_else(n.covered.switch.events > 0,
                                       total.switch.distance / n.covered.switch.events, NA_real_),
    switch.count.per.transition    = n.switch.events / pmax(n.trips - 1, 1),
    switch.distance.per.transition = total.switch.distance / pmax(n.trips - 1, 1)
  )

cat("\nVessel-years with >= 2 trips (the base):", nrow(vessel_year_switch_intensity),
    ", of which had at least one switch event:", sum(vessel_year_switch_intensity$n.switch.events > 0),
    ", of which had at least one COVERED switch event:",
    sum(vessel_year_switch_intensity$n.covered.switch.events > 0), "\n")
cat("Mean of vessel-year mean switch-event network distance (covered switches only):",
    round(mean(vessel_year_switch_intensity$mean.switch.distance, na.rm = TRUE), 4), "\n")
print(summary(vessel_year_switch_intensity$mean.switch.distance))

# ============================================================================
# 8. Face-validity cross-check against 06_'s weekly.switching
# ============================================================================
#
# Mirrors how 10_'s Table 14 cross-checks net.similarity against
# seasonal.overlap. weekly.switching.per.transition (06_ Section 3) and
# switch.count.per.transition above are both per-transition-normalized on
# purpose, the raw sums/counts are both mechanically larger for vessels that
# are simply more active, the exact confound 06_'s own comment on
# weekly.switching.per.transition already diagnosed and controlled for with
# mean.active.weeks in Table 6. vessel_year_switch_intensity is already
# zero-filled over every vessel-year with >= 2 trips (Section 7), so this
# inner_join's only loss is vessel-years switching_by_vessel_year itself
# excludes (fewer than 2 active WEEKS, a different grain than trips), not
# every vessel-year with zero switches, both denominators are printed so
# that loss is visible rather than assumed away.

cross_check_data <- vessel_year_switch_intensity %>%
  inner_join(
    switching_by_vessel_year %>%
      select(Vessel.ADFG.Number, Batch.Year, weekly.switching, weekly.switching.per.transition),
    by = c("Vessel.ADFG.Number", "Batch.Year")
  )

cat("\nVessel-years entering the weekly.switching cross-check:", nrow(cross_check_data),
    "of", nrow(vessel_year_switch_intensity), "trip-level vessel-years (>= 2 trips), and of",
    nrow(switching_by_vessel_year), "week-level vessel-years (>= 2 active weeks)\n")

cat("Correlation, raw trip switch count vs. raw weekly.switching:",
    round(cor(cross_check_data$n.switch.events, cross_check_data$weekly.switching, use = "complete.obs"), 4), "\n")
cat("Correlation, per-transition trip switch count vs. per-transition weekly.switching:",
    round(cor(cross_check_data$switch.count.per.transition,
              cross_check_data$weekly.switching.per.transition, use = "complete.obs"), 4), "\n")

cat("\n11_switch_event_weights.R done\n")
