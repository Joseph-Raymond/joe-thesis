# Chapter 3 empirical pipeline, rolling-window (owner-period) panel
#
# Owner-grain twin of 01b_build_rolling_panel.R, read that file directly for
# the full design reasoning, ported here rather than re-derived since it
# transfers directly at owner grain. File.Number here means the CFEC permit
# holder, NOT the vessel owner, see 05_table4_figure3_owner.R's own header
# for why that distinction matters and why "owner" is this pipeline's
# established shorthand for it. The unit here is an owner-window (an owner
# observed inside one particular 6-year rolling window), rather than an
# owner aggregated over its whole panel lifetime, the rolling-window
# analogue of what 05_table4_figure3_owner.R already does at owner grain for
# the LIFETIME panel.
#
# THE SINGLE MOST IMPORTANT RULE IN THIS FILE (mirrors 01b_'s own trap #1
# exactly, and matters just as much here). meets.min.years is NEVER applied
# anywhere below. Eligibility here is built from active_owner_years.rolling
# (owner.year.rev > 0, the same activity gate 01_build_panel.R itself uses
# for owners) and owner_share_panel directly, not from a filtered
# owner_summary. Applying meets.min.years would silently make the rolling
# sample a strict subset of the baseline (lifetime) owner sample and defeat
# the entire point of this exercise, exactly the reasoning 01b_'s own header
# gives for vessels.
#
# Same two exact algebraic shortcuts as 01b_ (design Section 3.0), unchanged
# at owner grain.
#   H_bar_{i,w} = mean over the window's active years of hhi_year_owner (a
#                 plain group mean, since a zero share contributes exactly
#                 zero to a sum of squares, no zero-fill needed).
#   H_LR_{i,w}  = sum_j (S_ijw / n.years.window)^2, where S_ijw is the SUM of
#                 raw positive shares over the window's active years.
#
# INFRASTRUCTURE LANDMINE FOUND WHILE BUILDING THIS, flagged rather than
# silently patched. 00b_rolling_periods.R's roll_eligibility() LOOKS generic
# (it calls the genuinely generic roll_expand_to_windows() internally), but
# its own data.table grouping is hardcoded, by = .(Vessel.ADFG.Number,
# window.start), confirmed by reading its source directly. Calling it on
# owner data (File.Number, not Vessel.ADFG.Number) would error at runtime.
# Section 3 below replicates its exact logic locally instead of calling it,
# keyed on File.Number, rather than editing 00b_rolling_periods.R itself,
# which is shared infrastructure every already-approved vessel-level rolling
# script (05b_ through 13b_) depends on, editing its hardcoded column name
# was out of scope for this task and risks an unintended side effect on
# scripts this task never touched.
#
# SCOPE, deliberately narrower than 01b_. Only Sections 1 through 11 of
# 01b_'s own structure are ported (through the passive buy-and-hold
# benchmark), matching exactly what 05b_table4_figure3_rolling_owner.R
# actually reads (owner_window_summary.rolling,
# passive_benchmark_window_owner.rolling, window_grid.rolling), the same
# three objects 05b_table4_figure3_rolling.R itself reads at vessel grain,
# nothing more.
#   - 01b_'s Section 12 (the mandatory attrition ladder, tau.window and
#     lookback availability) is SKIPPED ENTIRELY. That ladder exists
#     specifically to gate 07b_/08b_'s later within-season classifier work
#     (tau.window, predetermined-primary), which has no owner-level
#     analogue built anywhere in this pipeline yet, building it here would
#     be dead machinery with nothing downstream to feed.
#   - 01b_'s Section 13 (the standing eligible-vessels-by-window figure and
#     windows-per-vessel distribution) is ALSO skipped, for the same reason,
#     it exists to support the same "inspect before trusting 05b_ through
#     09b_" narrative Section 12 does (design Section 7.4), and nothing
#     downstream at owner grain reads either of its outputs. Both are easy
#     follow-up additions later (owner_window_summary.rolling already has
#     everything an owner-level version of either would need) if an
#     owner-level 07b_/08b_ or a dedicated owner attrition ladder is ever
#     built.
#
# Reads intermediate data/ch3_panel.rdata (built by 01_build_panel.R, run
# read-only here, needs owner_year, owner_share_panel, owner_mean_share,
# owner_summary, owner_fishery_year, fleet_mean_revenue_owner, and MAX_YEAR,
# the first two and fleet_mean_revenue_owner were added to 01_'s own save()
# call specifically to support this script and 05_table4_figure3_owner.R,
# owner_year's own hhi column likewise added specifically for this script's
# own Section 4 self-consistency check).
#
# Saves intermediate data/ch3_rolling_owner.rdata, a NEW file, ch3_rolling.rdata
# itself (the vessel-level rolling save) is untouched, with window_grid.rolling
# (identical object to the vessel-level version, recomputed here rather than
# shared, purely calendar-based so grain-independent, keeps this script
# runnable on its own without depending on ch3_rolling.rdata existing),
# active_owner_years.rolling, owner_year_window.rolling,
# owner_year_window_eligible.rolling, owner_window_all_counts.rolling,
# owner_window_eligibility.rolling, hhi_year_owner.rolling,
# owner_window_summary.rolling, and passive_benchmark_window_owner.rolling.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

if (!exists("owner_year") || !exists("owner_share_panel") || !exists("owner_mean_share") ||
    !exists("owner_summary") || !exists("owner_fishery_year") || !exists("fleet_mean_revenue_owner") ||
    !exists("MAX_YEAR")) {
  load(panel_path)
}

# ============================================================================
# 1. Window grid (mirrors 01b_ Section 1, generic, grain-independent)
# ============================================================================
#
# Purely calendar-based (MIN_YEAR, MAX_YEAR, ROLL_WINDOW_WIDTH), does not
# touch owner or vessel data at all, so this is the identical object the
# vessel-level ch3_rolling.rdata already carries, recomputed here (not
# loaded from that file) so this script has no dependency on 01b_ having
# already run.

window_grid.rolling <- roll_window_grid(MIN_YEAR, MAX_YEAR, ROLL_WINDOW_WIDTH)

cat("Rolling window grid (owner build) -", nrow(window_grid.rolling), "windows, starts",
    min(window_grid.rolling$window.start), "through", max(window_grid.rolling$window.start),
    "(MIN_YEAR =", MIN_YEAR, ", MAX_YEAR =", MAX_YEAR, ")\n")

# ============================================================================
# 2. Active owner-years and the candidate-window expansion (trap #1)
# ============================================================================
#
# Built directly from owner_year's own activity gate, exactly mirroring
# 01_build_panel.R Section 7's own active_owner_years (and
# 05_table4_figure3_owner.R's own re-derivation of it), NOT from any
# filtered version of owner_summary. This is the one object every rolling
# eligibility and every rolling quantity below derives from.

active_owner_years.rolling <- owner_year %>%
  filter(owner.year.rev > 0) %>%
  select(File.Number, Batch.Year, owner.year.rev)

cat("Active owner-years (rolling basis) -", nrow(active_owner_years.rolling),
    ", distinct owners -", n_distinct(active_owner_years.rolling$File.Number), "\n")

# roll_expand_to_windows() (00b_rolling_periods.R) IS genuinely generic in
# year_col, confirmed by reading its source, it only ever touches
# df[[year_col]] and repeats whole rows, never referencing
# Vessel.ADFG.Number by name. Safe to reuse directly, unlike
# roll_eligibility() below.
t_expand <- Sys.time()
owner_year_window.rolling <- roll_expand_to_windows(
  active_owner_years.rolling, "Batch.Year", window_grid.rolling
)
cat("Owner-year x candidate-window expansion -", nrow(owner_year_window.rolling), "rows in",
    round(as.numeric(Sys.time() - t_expand, units = "secs"), 2), "sec\n")

# Unfiltered (owner, window.start) active-year counts, n.years.window in
# 1..ROLL_WINDOW_WIDTH, the owner-level mirror of vessel_window_all_counts.rolling.
owner_window_all_counts.rolling <- as.data.table(owner_year_window.rolling)[
  , .(n.years.window = .N), by = .(File.Number, window.start)
] %>% as_tibble()

# ============================================================================
# 3. Eligibility (mirrors 01b_ Section 3), the ONE owner-level sample
#    definition
# ============================================================================
#
# NOT built via roll_eligibility() (00b_rolling_periods.R), see this
# script's header landmine note, that helper hardcodes
# by = .(Vessel.ADFG.Number, window.start) internally and would error on
# owner data. Replicated here instead with the identical logic
# (count active years per unit-window, keep those >= ROLL_MIN_ACTIVE_YEARS),
# keyed on File.Number, reusing owner_year_window.rolling (already expanded
# in Section 2) rather than re-expanding a second time the way
# roll_eligibility() itself would (it calls roll_expand_to_windows()
# internally rather than accepting a precomputed expansion).
owner_window_eligibility.rolling <- as.data.table(owner_year_window.rolling)[
  , .(n.years.window = .N), by = .(File.Number, window.start)
][n.years.window >= ROLL_MIN_ACTIVE_YEARS] %>% as_tibble()

cat("Eligible (owner, window) pairs (n.years.window >=", ROLL_MIN_ACTIVE_YEARS, ") -",
    nrow(owner_window_eligibility.rolling),
    ", distinct owners -", n_distinct(owner_window_eligibility.rolling$File.Number), "\n")

# The single semi-join point every quantity below is built on.
owner_year_window_eligible.rolling <- owner_year_window.rolling %>%
  semi_join(owner_window_eligibility.rolling, by = c("File.Number", "window.start"))

# ============================================================================
# 4. H_bar (design Section 3.0), the hhi_year shortcut
# ============================================================================

hhi_year_owner.rolling <- owner_share_panel %>%
  group_by(File.Number, Batch.Year) %>%
  summarise(hhi.year = sum(share^2), .groups = "drop")

# One-time agreement check against owner_year$hhi (added to 01_build_panel.R
# specifically for this check, mirroring vessel_year$hhi), doubles as a
# data-quality diagnostic for the same negative-revenue edge case 01b_'s own
# vessel-level version checks for. A disagreement here means
# owner_share_panel's share (revenue / owner.year.rev, i.e. normalized
# against the owner's TOTAL revenue across every held fishery that year) and
# owner_year$hhi (normalized against sum(revenue[fished]) only) used
# different denominators for that owner-year, which happens when a
# held-but-unfished fishery's revenue is negative (a correction/refund
# ticket) rather than exactly zero.
hhi_check_owner <- hhi_year_owner.rolling %>%
  inner_join(owner_year %>% select(File.Number, Batch.Year, hhi),
             by = c("File.Number", "Batch.Year")) %>%
  mutate(disagrees = abs(hhi.year - hhi) > 1e-8)
cat("hhi_year_owner.rolling vs owner_year$hhi agreement check -", sum(hhi_check_owner$disagrees),
    "disagreements out of", nrow(hhi_check_owner), "owner-years\n")

t_hbar <- Sys.time()
H_bar_owner.rolling <- hhi_year_owner.rolling %>%
  inner_join(
    owner_year_window_eligible.rolling %>% select(File.Number, Batch.Year, window.start),
    by = c("File.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(File.Number, window.start) %>%
  summarise(H_bar = mean(hhi.year), .groups = "drop")
cat("H_bar_owner.rolling -", nrow(H_bar_owner.rolling), "rows in",
    round(as.numeric(Sys.time() - t_hbar, units = "secs"), 2), "sec\n")

# ============================================================================
# 5. H_LR and the passive-benchmark weights (design Section 3.0 and 3.4)
# ============================================================================
#
# owner_share_raw is NOT saved into ch3_panel.rdata (matching 01b_'s own
# trap #2 for vessel_share_raw), use owner_share_panel filtered to
# share > 0 instead, algebraically identical for S_ijw (the zero-filled rows
# contribute nothing to a sum).
#
# S_ijw_owner.rolling doubles as the passive-benchmark weight table
# (mean.share.window = S_ijw / n.years.window is exactly the window's own
# mean-share weight vector, design Section 3.4), built once here rather than
# twice, mirroring 01b_'s own S_ijw.rolling exactly.

owner_share_positive.rolling <- owner_share_panel %>% filter(share > 0)

t_sijw <- Sys.time()
S_ijw_owner.rolling <- owner_share_positive.rolling %>%
  inner_join(
    owner_year_window_eligible.rolling %>% select(File.Number, Batch.Year, window.start),
    by = c("File.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(File.Number, window.start, Fishery) %>%
  summarise(S_ijw = sum(share), .groups = "drop") %>%
  left_join(
    owner_window_eligibility.rolling %>% select(File.Number, window.start, n.years.window),
    by = c("File.Number", "window.start")
  ) %>%
  mutate(mean.share.window = S_ijw / n.years.window)
cat("S_ijw_owner.rolling (owner x window x fishery long-run share) -", nrow(S_ijw_owner.rolling), "rows in",
    round(as.numeric(Sys.time() - t_sijw, units = "secs"), 2), "sec\n")

H_LR_owner.rolling <- S_ijw_owner.rolling %>%
  group_by(File.Number, window.start) %>%
  summarise(H_LR = sum(mean.share.window^2), .groups = "drop")

# n.fisheries.fished.window (design Section 3.3), reusing S_ijw_owner.rolling
# rather than a second join to owner_fishery_year, S_ijw_owner.rolling is
# already restricted to positive-share (i.e. fished) fisheries.
n_fisheries_owner.rolling <- S_ijw_owner.rolling %>%
  count(File.Number, window.start, name = "n.fisheries.fished.window")

# ============================================================================
# 6. rev.cv (design Section 3.1, must use the SAME year set as H_bar)
# ============================================================================

rev_cv_owner.rolling <- active_owner_years.rolling %>%
  inner_join(
    owner_year_window_eligible.rolling %>% select(File.Number, Batch.Year, window.start),
    by = c("File.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(File.Number, window.start) %>%
  summarise(rev.cv = sd(owner.year.rev) / mean(owner.year.rev), .groups = "drop")

# ============================================================================
# 7. prime.fishery.window (design Section 3.2)
# ============================================================================
#
# The fishery with the most SUMMED realized revenue among the window's own
# active years, the exact within-window analogue of 01_build_panel.R
# Section 7's own lifetime prime_fishery_owner, ranked on revenue (not
# shares, which do not sum meaningfully across years) for the same reason
# given there.

prime_fishery_owner.rolling <- owner_fishery_year %>%
  filter(fished) %>%
  inner_join(
    owner_year_window_eligible.rolling %>% select(File.Number, Batch.Year, window.start),
    by = c("File.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(File.Number, window.start, Fishery) %>%
  summarise(total.rev = sum(revenue), .groups = "drop") %>%
  group_by(File.Number, window.start) %>%
  slice_max(total.rev, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(File.Number, window.start, prime.fishery.window = Fishery)

# ============================================================================
# 8. Lifetime labels, carried alongside the within-window versions
# ============================================================================

n_fisheries_lifetime_owner <- owner_mean_share %>%
  count(File.Number, name = "n.fisheries.fished.lifetime")

owner_lifetime_labels.rolling <- owner_summary %>%
  select(File.Number, prime.fishery.lifetime = prime.fishery) %>%
  left_join(n_fisheries_lifetime_owner, by = "File.Number") %>%
  mutate(is.specialist.lifetime = replace_na(n.fisheries.fished.lifetime, 0) == 1) %>%
  select(File.Number, prime.fishery.lifetime, is.specialist.lifetime)

# ============================================================================
# 9. n.windows.owner, inv.window.count (design Section 2.4)
# ============================================================================
#
# n.windows.vessel (01b_'s own name) renamed n.windows.owner here, it
# describes how many windows THIS UNIT contributes, which is an owner, not a
# vessel, at this grain. REQUIRED by 05b_table4_figure3_rolling_owner.R,
# which reads it directly (its own eligible-vs-in-sample diagnostic compares
# owner_multi.rolling$n.windows.owner against a freshly-built
# n.windows.owner.insample column), do not drop this column from Section
# 10's select() list below, that diagnostic would break. What 05b_owner
# recomputes independently, rather than reusing from here, is only the
# IN-SAMPLE version (n.windows.owner.insample/inv.window.count.insample)
# used as the actual regression weight, not this column itself, for the
# identical reason 05b_table4_figure3_rolling.R's own comment gives for
# vessels, this object's inv.window.count is 1 / (ELIGIBLE windows), but the
# weighted regression runs on a sample that has already dropped
# within-window specialists and non-finite rev.cv, so it needs its own
# in-sample recomputation rather than reusing inv.window.count as the actual
# weight.

n_windows_per_owner.rolling <- owner_window_eligibility.rolling %>%
  count(File.Number, name = "n.windows.owner") %>%
  mutate(inv.window.count = 1 / n.windows.owner)

# ============================================================================
# 10. Assemble owner_window_summary.rolling
# ============================================================================

owner_window_summary.rolling <- owner_window_eligibility.rolling %>%
  left_join(window_grid.rolling, by = "window.start") %>%
  left_join(H_bar_owner.rolling, by = c("File.Number", "window.start")) %>%
  left_join(H_LR_owner.rolling, by = c("File.Number", "window.start")) %>%
  mutate(Phi = H_bar - H_LR) %>%
  left_join(rev_cv_owner.rolling, by = c("File.Number", "window.start")) %>%
  left_join(prime_fishery_owner.rolling, by = c("File.Number", "window.start")) %>%
  left_join(n_fisheries_owner.rolling, by = c("File.Number", "window.start")) %>%
  mutate(
    n.fisheries.fished.window = replace_na(n.fisheries.fished.window, 0),
    is.specialist.window      = n.fisheries.fished.window == 1
  ) %>%
  left_join(owner_lifetime_labels.rolling, by = "File.Number") %>%
  left_join(n_windows_per_owner.rolling, by = "File.Number") %>%
  select(
    File.Number, window.start, window.end, n.years.window,
    H_bar, H_LR, Phi, rev.cv,
    prime.fishery.window, prime.fishery.lifetime,
    n.fisheries.fished.window, is.specialist.window, is.specialist.lifetime,
    n.windows.owner, inv.window.count
  )

cat("owner_window_summary.rolling -", nrow(owner_window_summary.rolling), "rows, ",
    n_distinct(owner_window_summary.rolling$File.Number), "distinct owners\n")

# ---- Diagnostics called for by design Sections 3.2 and 3.3, owner mirror ----

prime_switch_check_owner <- owner_window_summary.rolling %>%
  filter(n.windows.owner > 1) %>%
  group_by(File.Number) %>%
  summarise(n.distinct.prime = n_distinct(prime.fishery.window), .groups = "drop")
share_prime_switched_owner <- mean(prime_switch_check_owner$n.distinct.prime > 1)
cat("Share of multi-window owners whose within-window prime.fishery is not constant across their windows -",
    round(share_prime_switched_owner, 4), "\n")

within_window_specialist_share_owner <- mean(owner_window_summary.rolling$is.specialist.window)
cat("Within-window specialist share (owner) -", round(within_window_specialist_share_owner, 4), "\n")

cat("Cross-tab, within-window specialist status vs lifetime specialist status (owner)\n")
print(table(
  window.specialist   = owner_window_summary.rolling$is.specialist.window,
  lifetime.specialist  = owner_window_summary.rolling$is.specialist.lifetime
))

# ============================================================================
# 11. Passive buy-and-hold benchmark, within window (design Section 3.4)
# ============================================================================
#
# Weights are the window's own mean shares (S_ijw_owner.rolling's own
# mean.share.window, already built in Section 5), NOT the owner's lifetime
# owner_mean_share. Weights sum to 1 within a window by construction (same
# reasoning as the baseline's passive benchmark in
# 05_table4_figure3_owner.R), no renormalization needed.
# fleet_mean_revenue_owner filled to 0 for a (fishery, year) with zero
# fleet-wide owner activity, matching 05_table4_figure3_owner.R.

t_passive <- Sys.time()
passive_series_window_owner.rolling <- S_ijw_owner.rolling %>%
  select(File.Number, window.start, Fishery, weight = mean.share.window) %>%
  inner_join(
    owner_year_window_eligible.rolling %>% select(File.Number, window.start, Batch.Year),
    by = c("File.Number", "window.start"), relationship = "many-to-many"
  ) %>%
  left_join(fleet_mean_revenue_owner %>% select(Batch.Year, Fishery, fleet_mean_revenue),
            by = c("Batch.Year", "Fishery")) %>%
  mutate(fleet_mean_revenue = replace_na(fleet_mean_revenue, 0)) %>%
  group_by(File.Number, window.start, Batch.Year) %>%
  summarise(passive_revenue = sum(weight * fleet_mean_revenue), .groups = "drop")

passive_benchmark_window_owner.rolling <- passive_series_window_owner.rolling %>%
  group_by(File.Number, window.start) %>%
  summarise(
    n.years.passive.window = n(),
    passive.cv             = sd(passive_revenue) / mean(passive_revenue),
    .groups = "drop"
  )
cat("passive_benchmark_window_owner.rolling -", nrow(passive_benchmark_window_owner.rolling), "rows in",
    round(as.numeric(Sys.time() - t_passive, units = "secs"), 2), "sec\n")

# ============================================================================
# 12. Save
# ============================================================================
#
# Sections 12 (attrition ladder) and 13 (standing eligible-owners figure) of
# 01b_'s own numbering are skipped entirely, see this script's header for
# why, so this file's own Section 12 is the Save step, matching 01b_'s
# Section 14 in role but not in number.

rolling_owner_panel_path <- file.path(intermediate_dir, "ch3_rolling_owner.rdata")
save(
  window_grid.rolling, active_owner_years.rolling,
  owner_year_window.rolling, owner_year_window_eligible.rolling,
  owner_window_all_counts.rolling, owner_window_eligibility.rolling,
  hhi_year_owner.rolling, owner_window_summary.rolling, passive_benchmark_window_owner.rolling,
  file = rolling_owner_panel_path
)
cat("Saved owner rolling panel objects to", rolling_owner_panel_path, "\n")
