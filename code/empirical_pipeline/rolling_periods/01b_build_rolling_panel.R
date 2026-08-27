# Chapter 3 empirical pipeline, rolling-window (vessel-period) panel
#
# Builds the rolling-window twin of 01_build_panel.R's vessel-level summary
# objects, see rolling_periods_design.md for the full design. The unit here
# is a vessel-window (a vessel observed inside one particular 6-year rolling
# window), rather than a vessel aggregated over its whole panel lifetime.
#
# THE SINGLE MOST IMPORTANT RULE IN THIS FILE (design Section 7.2, trap #1):
# meets.min.years is NEVER applied anywhere below. Eligibility here is built
# from active_vessel_years.rolling (vessel.year.rev > 0, the same activity
# gate 01_build_panel.R itself uses) and vessel_share_panel directly, not
# from a filtered vessel_summary. Applying meets.min.years would silently
# make the rolling sample a strict subset of the baseline sample and defeat
# the entire point of this exercise.
#
# Two exact algebraic shortcuts (design Section 3.0) make this tractable
# without ever materializing a zero-filled window share panel (which would
# be millions of rows for no reason):
#   H_bar_{i,w} = mean over the window's active years of hhi_year (a plain
#                 group mean, since a zero share contributes exactly zero to
#                 a sum of squares, no zero-fill needed).
#   H_LR_{i,w}  = sum_j (S_ijw / n.years.window)^2, where S_ijw is the SUM of
#                 raw positive shares over the window's active years.
#
# Reads intermediate data/ch3_panel.rdata (built by 01_build_panel.R, run
# read-only here) and, if present, intermediate data/ch3_within_season.rdata
# (built by 06_within_season_reallocation.R) for the attrition ladder's
# tau-availability step only, see Section 6 below.
#
# Writes Chpt3/output/tables/table_rolling_sample_attrition.tex and
# Chpt3/output/figures/figure_rolling_eligible_vessels_by_window.png, both
# BEFORE anything downstream (05b_ through 09b_) should be trusted, per
# design Section 7.4, inspect these first.
#
# Saves intermediate data/ch3_rolling.rdata with window_grid.rolling,
# active_vessel_years.rolling, vessel_year_window.rolling,
# vessel_year_window_eligible.rolling, vessel_window_all_counts.rolling,
# vessel_window_eligibility.rolling, hhi_year.rolling,
# vessel_window_summary.rolling, passive_benchmark_window.rolling, and
# attrition_ledger.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

if (!exists("vessel_year") || !exists("vessel_share_panel") || !exists("vessel_mean_share") ||
    !exists("vessel_summary") || !exists("vessel_fishery_year") || !exists("fleet_mean_revenue") ||
    !exists("MAX_YEAR")) {
  load(panel_path)
}

# ============================================================================
# 1. Window grid (design Section 1.2)
# ============================================================================
#
# MAX_YEAR is read from the saved panel (trimmed by 01_'s trailing-coverage
# check), never hardcoded, trap #4. MIN_YEAR comes from 00_setup.R.

window_grid.rolling <- roll_window_grid(MIN_YEAR, MAX_YEAR, ROLL_WINDOW_WIDTH)

cat("Rolling window grid:", nrow(window_grid.rolling), "windows, starts",
    min(window_grid.rolling$window.start), "through", max(window_grid.rolling$window.start),
    "(MIN_YEAR =", MIN_YEAR, ", MAX_YEAR =", MAX_YEAR, ")\n")

# ============================================================================
# 2. Active vessel-years and the candidate-window expansion (trap #1)
# ============================================================================
#
# Built directly from vessel_year's own activity gate, exactly mirroring
# 01_build_panel.R Section 6 and 05_/07_'s own active_vessel_years, NOT from
# any filtered version of vessel_summary. This is the one object every
# rolling eligibility and every rolling quantity below derives from.

active_vessel_years.rolling <- vessel_year %>%
  filter(vessel.year.rev > 0) %>%
  select(Vessel.ADFG.Number, Batch.Year, vessel.year.rev)

cat("Active vessel-years (rolling basis):", nrow(active_vessel_years.rolling),
    " distinct vessels:", n_distinct(active_vessel_years.rolling$Vessel.ADFG.Number), "\n")

t_expand <- Sys.time()
vessel_year_window.rolling <- roll_expand_to_windows(
  active_vessel_years.rolling, "Batch.Year", window_grid.rolling
)
cat("Vessel-year x candidate-window expansion:", nrow(vessel_year_window.rolling), "rows in",
    round(as.numeric(Sys.time() - t_expand, units = "secs"), 2), "sec",
    "(design's own order-of-magnitude estimate is roughly 1.3M)\n")

# Unfiltered (vessel, window.start) active-year counts, n.years.window in
# 1..ROLL_WINDOW_WIDTH. This is BOTH the attrition ladder's first row (every
# candidate with at least one active year, before the 4-of-6 floor) AND,
# shifted by ROLL_WINDOW_WIDTH years below (Section 6), the source for the
# lookback-window active-year counts Table 8-rolling and 08b_'s
# predetermined-primary construction both need.
vessel_window_all_counts.rolling <- as.data.table(vessel_year_window.rolling)[
  , .(n.years.window = .N), by = .(Vessel.ADFG.Number, window.start)
] %>% as_tibble()

# ============================================================================
# 3. Eligibility (design Section 1.3), the ONE sample definition
# ============================================================================
#
# Built once here via the shared roll_eligibility() helper and semi-joined
# by every downstream rolling object in THIS script and by every later
# rolling script that loads ch3_rolling.rdata, so eligibility is never
# recomputed with a slightly different filter somewhere else in the pipeline.

vessel_window_eligibility.rolling <- roll_eligibility(
  active_vessel_years.rolling, window_grid.rolling, ROLL_MIN_ACTIVE_YEARS
)

cat("Eligible (vessel, window) pairs (n.years.window >=", ROLL_MIN_ACTIVE_YEARS, "):",
    nrow(vessel_window_eligibility.rolling),
    " distinct vessels:", n_distinct(vessel_window_eligibility.rolling$Vessel.ADFG.Number), "\n")

# The single semi-join point every quantity below is built on.
vessel_year_window_eligible.rolling <- vessel_year_window.rolling %>%
  semi_join(vessel_window_eligibility.rolling, by = c("Vessel.ADFG.Number", "window.start"))

# ============================================================================
# 4. H_bar (design Section 3.0), the hhi_year shortcut
# ============================================================================

hhi_year.rolling <- vessel_share_panel %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  summarise(hhi.year = sum(share^2), .groups = "drop")

# One-time agreement check against vessel_year$hhi, doubles as a
# data-quality diagnostic for 01_'s negative-revenue edge case in the
# "fished" gate (design Section 3.0). A disagreement here means
# vessel_share_panel's share (revenue / vessel.year.rev, i.e. normalized
# against the vessel's TOTAL revenue across every held fishery that year)
# and vessel_year$hhi (normalized against sum(revenue[fished]) only) used
# different denominators for that vessel-year, which happens when a
# held-but-unfished fishery's revenue is negative (a correction/refund
# ticket) rather than exactly zero.
hhi_check <- hhi_year.rolling %>%
  inner_join(vessel_year %>% select(Vessel.ADFG.Number, Batch.Year, hhi),
             by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  mutate(disagrees = abs(hhi.year - hhi) > 1e-8)
cat("hhi_year.rolling vs vessel_year$hhi agreement check:", sum(hhi_check$disagrees),
    "disagreements out of", nrow(hhi_check), "vessel-years\n")

t_hbar <- Sys.time()
H_bar.rolling <- hhi_year.rolling %>%
  inner_join(
    vessel_year_window_eligible.rolling %>% select(Vessel.ADFG.Number, Batch.Year, window.start),
    by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(H_bar = mean(hhi.year), .groups = "drop")
cat("H_bar.rolling:", nrow(H_bar.rolling), "rows in",
    round(as.numeric(Sys.time() - t_hbar, units = "secs"), 2), "sec\n")

# ============================================================================
# 5. H_LR and the passive-benchmark weights (design Section 3.0 and 3.4)
# ============================================================================
#
# vessel_share_raw is NOT saved into ch3_panel.rdata (trap #2), use
# vessel_share_panel filtered to share > 0 instead, algebraically identical
# for S_ijw (the zero-filled rows contribute nothing to a sum).
#
# S_ijw.rolling doubles as the passive-benchmark weight table
# (mean.share.window = S_ijw / n.years.window is exactly the window's own
# mean-share weight vector, design Section 3.4), built once here rather than
# twice.

vessel_share_positive.rolling <- vessel_share_panel %>% filter(share > 0)

t_sijw <- Sys.time()
S_ijw.rolling <- vessel_share_positive.rolling %>%
  inner_join(
    vessel_year_window_eligible.rolling %>% select(Vessel.ADFG.Number, Batch.Year, window.start),
    by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(Vessel.ADFG.Number, window.start, Fishery) %>%
  summarise(S_ijw = sum(share), .groups = "drop") %>%
  left_join(
    vessel_window_eligibility.rolling %>% select(Vessel.ADFG.Number, window.start, n.years.window),
    by = c("Vessel.ADFG.Number", "window.start")
  ) %>%
  mutate(mean.share.window = S_ijw / n.years.window)
cat("S_ijw.rolling (vessel x window x fishery long-run share):", nrow(S_ijw.rolling), "rows in",
    round(as.numeric(Sys.time() - t_sijw, units = "secs"), 2), "sec",
    "(design's own order-of-magnitude estimate is roughly 2.7M)\n")

H_LR.rolling <- S_ijw.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(H_LR = sum(mean.share.window^2), .groups = "drop")

# n.fisheries.fished.window (design Section 3.3), reusing S_ijw.rolling
# rather than a second join to vessel_fishery_year, S_ijw.rolling is already
# restricted to positive-share (i.e. fished) fisheries.
n_fisheries.rolling <- S_ijw.rolling %>%
  count(Vessel.ADFG.Number, window.start, name = "n.fisheries.fished.window")

# ============================================================================
# 6. rev.cv (design Section 3.1, must use the SAME year set as H_bar)
# ============================================================================

rev_cv.rolling <- active_vessel_years.rolling %>%
  inner_join(
    vessel_year_window_eligible.rolling %>% select(Vessel.ADFG.Number, Batch.Year, window.start),
    by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(rev.cv = sd(vessel.year.rev) / mean(vessel.year.rev), .groups = "drop")

# ============================================================================
# 7. prime.fishery.window (design Section 3.2)
# ============================================================================
#
# The fishery with the most SUMMED realized revenue among the window's own
# active years, the exact within-window analogue of 01_build_panel.R
# Section 6's lifetime prime.fishery, ranked on revenue (not shares, which
# do not sum meaningfully across years) for the same reason given there.

prime_fishery.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  inner_join(
    vessel_year_window_eligible.rolling %>% select(Vessel.ADFG.Number, Batch.Year, window.start),
    by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(Vessel.ADFG.Number, window.start, Fishery) %>%
  summarise(total.rev = sum(revenue), .groups = "drop") %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  slice_max(total.rev, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Vessel.ADFG.Number, window.start, prime.fishery.window = Fishery)

# ============================================================================
# 8. Lifetime labels, carried alongside the within-window versions
# ============================================================================

n_fisheries_lifetime <- vessel_mean_share %>%
  count(Vessel.ADFG.Number, name = "n.fisheries.fished.lifetime")

vessel_lifetime_labels.rolling <- vessel_summary %>%
  select(Vessel.ADFG.Number, prime.fishery.lifetime = prime.fishery) %>%
  left_join(n_fisheries_lifetime, by = "Vessel.ADFG.Number") %>%
  mutate(is.specialist.lifetime = replace_na(n.fisheries.fished.lifetime, 0) == 1) %>%
  select(Vessel.ADFG.Number, prime.fishery.lifetime, is.specialist.lifetime)

# ============================================================================
# 9. n.windows.vessel, inv.window.count (design Section 2.4)
# ============================================================================

n_windows_per_vessel.rolling <- vessel_window_eligibility.rolling %>%
  count(Vessel.ADFG.Number, name = "n.windows.vessel") %>%
  mutate(inv.window.count = 1 / n.windows.vessel)

# ============================================================================
# 10. Assemble vessel_window_summary.rolling
# ============================================================================

vessel_window_summary.rolling <- vessel_window_eligibility.rolling %>%
  left_join(window_grid.rolling, by = "window.start") %>%
  left_join(H_bar.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  left_join(H_LR.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(Phi = H_bar - H_LR) %>%
  left_join(rev_cv.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  left_join(prime_fishery.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  left_join(n_fisheries.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  mutate(
    n.fisheries.fished.window = replace_na(n.fisheries.fished.window, 0),
    is.specialist.window      = n.fisheries.fished.window == 1
  ) %>%
  left_join(vessel_lifetime_labels.rolling, by = "Vessel.ADFG.Number") %>%
  left_join(n_windows_per_vessel.rolling, by = "Vessel.ADFG.Number") %>%
  select(
    Vessel.ADFG.Number, window.start, window.end, n.years.window,
    H_bar, H_LR, Phi, rev.cv,
    prime.fishery.window, prime.fishery.lifetime,
    n.fisheries.fished.window, is.specialist.window, is.specialist.lifetime,
    n.windows.vessel, inv.window.count
  )

cat("vessel_window_summary.rolling:", nrow(vessel_window_summary.rolling), "rows, ",
    n_distinct(vessel_window_summary.rolling$Vessel.ADFG.Number), "distinct vessels\n")

# ---- Diagnostics called for by design Sections 3.2 and 3.3 ----

# share.prime.fishery.switched, restricted to multi-window vessels (a
# single-window vessel cannot switch by construction).
prime_switch_check <- vessel_window_summary.rolling %>%
  filter(n.windows.vessel > 1) %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(n.distinct.prime = n_distinct(prime.fishery.window), .groups = "drop")
share_prime_switched <- mean(prime_switch_check$n.distinct.prime > 1)
cat("Share of multi-window vessels whose within-window prime.fishery is not constant across their windows:",
    round(share_prime_switched, 4), "\n")

within_window_specialist_share <- mean(vessel_window_summary.rolling$is.specialist.window)
cat("Within-window specialist share:", round(within_window_specialist_share, 4),
    " (expected higher than the lifetime ~23% figure the writeup quotes,",
    "a 6-year window gives fewer chances to have fished a second fishery)\n")

cat("Cross-tab, within-window specialist status vs lifetime specialist status:\n")
print(table(
  window.specialist   = vessel_window_summary.rolling$is.specialist.window,
  lifetime.specialist  = vessel_window_summary.rolling$is.specialist.lifetime
))

# ============================================================================
# 11. Passive buy-and-hold benchmark, within window (design Section 3.4)
# ============================================================================
#
# Weights are the window's own mean shares (S_ijw.rolling$mean.share.window,
# already built in Section 5), NOT the vessel's lifetime vessel_mean_share.
# Weights sum to 1 within a window by construction (same reasoning as the
# baseline's passive benchmark in 05_table4_figure3.R), no renormalization
# needed. fleet_mean_revenue filled to 0 for a (fishery, year) with zero
# fleet-wide activity, matching 05_.

t_passive <- Sys.time()
passive_series_window.rolling <- S_ijw.rolling %>%
  select(Vessel.ADFG.Number, window.start, Fishery, weight = mean.share.window) %>%
  inner_join(
    vessel_year_window_eligible.rolling %>% select(Vessel.ADFG.Number, window.start, Batch.Year),
    by = c("Vessel.ADFG.Number", "window.start"), relationship = "many-to-many"
  ) %>%
  left_join(fleet_mean_revenue %>% select(Batch.Year, Fishery, fleet_mean_revenue),
            by = c("Batch.Year", "Fishery")) %>%
  mutate(fleet_mean_revenue = replace_na(fleet_mean_revenue, 0)) %>%
  group_by(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  summarise(passive_revenue = sum(weight * fleet_mean_revenue), .groups = "drop")

passive_benchmark_window.rolling <- passive_series_window.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(
    n.years.passive.window = n(),
    passive.cv             = sd(passive_revenue) / mean(passive_revenue),
    .groups = "drop"
  )
cat("passive_benchmark_window.rolling:", nrow(passive_benchmark_window.rolling), "rows in",
    round(as.numeric(Sys.time() - t_passive, units = "secs"), 2), "sec\n")

# ============================================================================
# 12. Mandatory attrition ladder (design Section 7.4), built BEFORE anything
#     downstream (05b_ through 09b_) should be trusted
# ============================================================================

step_finite_cv      <- vessel_window_summary.rolling %>% filter(is.finite(rev.cv))
step_positive_cv    <- step_finite_cv %>% filter(rev.cv > 0)
step_not_specialist <- step_positive_cv %>% filter(!is.specialist.window)

# tau.window availability. Loaded here from ch3_within_season.rdata (already
# built by baseline 06_within_season_reallocation.R, which run_all.R runs
# before any rolling script per this pipeline's own dependency chain) purely
# for this ladder's diagnostic purposes. This duplicates, rather than
# shares, the canonical tau_by_vessel_window.rolling construction
# 06b_within_season_reallocation_rolling.R builds for the actual Table
# 7/8-rolling models, following this pipeline's own established convention
# of duplicating a reload/recompute block across scripts (06_, 08_, 09_ all
# already do this) rather than making 01b_ depend on 06b_ having run.
within_season_path <- file.path(intermediate_dir, "ch3_within_season.rdata")

if (file.exists(within_season_path)) {
  if (!exists("switching_by_vessel_year")) load(within_season_path)

  tau_years.rolling <- switching_by_vessel_year %>%
    filter(is.finite(weekly.switching.per.transition)) %>%
    select(Vessel.ADFG.Number, Batch.Year)

  n_tau_years_window.rolling <- vessel_year_window_eligible.rolling %>%
    semi_join(tau_years.rolling, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
    count(Vessel.ADFG.Number, window.start, name = "n.tau.years.window")

  step_tau_available <- step_not_specialist %>%
    left_join(n_tau_years_window.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
    filter(!is.na(n.tau.years.window), n.tau.years.window >= ROLL_MIN_TAU_YEARS)
} else {
  warning(
    "ch3_within_season.rdata not found at ", within_season_path, ", run_all.R (through at least ",
    "06_within_season_reallocation.R) must be run before the rolling pipeline. The attrition ladder's ",
    "tau-availability and lookback rows below are left empty rather than fabricated."
  )
  step_tau_available <- step_not_specialist %>% filter(FALSE)
}

# Lookback availability, >= ROLL_MIN_LOOKBACK_YEARS active years in the
# preceding window c(s) = [s - 6, s - 1], i.e. window.start = s - ROLL_WINDOW_WIDTH
# in vessel_window_all_counts.rolling's own (unfiltered) counts.
lookback_counts.rolling <- vessel_window_all_counts.rolling %>%
  transmute(Vessel.ADFG.Number, window.start = window.start + ROLL_WINDOW_WIDTH,
            n.years.lookback = n.years.window)

step_lookback_available <- step_tau_available %>%
  left_join(lookback_counts.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(!is.na(n.years.lookback), n.years.lookback >= ROLL_MIN_LOOKBACK_YEARS)

attrition_ledger <- tibble(step = character(), n.obs = integer(), n.vessels = integer())
attrition_ledger <- roll_attrition_row(
  attrition_ledger, "All (vessel, window) candidates, >= 1 active year", vessel_window_all_counts.rolling
)
attrition_ledger <- roll_attrition_row(
  attrition_ledger, paste0("Eligible (n.years.window >= ", ROLL_MIN_ACTIVE_YEARS, ")"),
  vessel_window_eligibility.rolling
)
attrition_ledger <- roll_attrition_row(attrition_ledger, "is.finite(rev.cv)", step_finite_cv)
attrition_ledger <- roll_attrition_row(attrition_ledger, "rev.cv > 0", step_positive_cv)
attrition_ledger <- roll_attrition_row(attrition_ledger, "not is.specialist.window", step_not_specialist)
attrition_ledger <- roll_attrition_row(
  attrition_ledger, paste0("tau.window available (n.tau.years.window >= ", ROLL_MIN_TAU_YEARS, ")"),
  step_tau_available
)
attrition_ledger <- roll_attrition_row(
  attrition_ledger,
  paste0("lookback available (>= ", ROLL_MIN_LOOKBACK_YEARS, " active years in preceding window)"),
  step_lookback_available
)

cat("\n===== Rolling sample attrition ladder =====\n")
print(attrition_ledger)

print(
  xtable(
    attrition_ledger,
    caption = "Rolling-panel sample attrition, vessel-window observation counts and distinct vessel counts at each filtering step",
    label = "tab:ch3-rolling-attrition", digits = 0
  ),
  file = file.path(table_dir, "table_rolling_sample_attrition.tex"),
  include.rownames = FALSE
)
cat("Wrote table_rolling_sample_attrition.tex\n")

# ---- The decision-point check (design Section 7.4) ----

n_eligible_vessels_total <- n_distinct(vessel_window_eligibility.rolling$Vessel.ADFG.Number)
n_eligible_windows_total <- nrow(vessel_window_eligibility.rolling)

cat("\n*** Eligible vessels (any window):", n_eligible_vessels_total,
    " Eligible vessel-windows:", n_eligible_windows_total, "***\n")

if (n_eligible_vessels_total < 8000 || n_eligible_windows_total < 40000) {
  cat(
    "*** WARNING: eligible vessels/windows are well below the design document's expected order of\n",
    "magnitude (roughly 11,000-14,000 vessels, 120,000-170,000 vessel-windows). This is a decision\n",
    "point for the author, not something to silently proceed past, inspect the eligibility\n",
    "construction above before trusting 05b_ through 09b_. ***\n"
  )
}

# ============================================================================
# 13. Standing diagnostics (design Section 7.4)
# ============================================================================

eligible_by_window <- vessel_window_eligibility.rolling %>%
  count(window.start, name = "n.eligible.vessels") %>%
  left_join(window_grid.rolling, by = "window.start")

figure_rolling_eligible <- eligible_by_window %>%
  ggplot(aes(x = window.start, y = n.eligible.vessels)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 1.5) +
  labs(
    title = "Eligible vessels by rolling window",
    subtitle = paste0(ROLL_MIN_ACTIVE_YEARS, "-of-", ROLL_WINDOW_WIDTH,
                       " active-year floor, edges are survivor-biased toward locally dense vessels"),
    x = "Window start year", y = "Eligible vessels"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure_rolling_eligible_vessels_by_window.png"),
       figure_rolling_eligible, width = 8, height = 5, dpi = 300)

cat("Wrote figure_rolling_eligible_vessels_by_window.png\n")

windows_per_vessel_dist <- n_windows_per_vessel.rolling %>%
  count(n.windows.vessel, name = "n.vessels") %>%
  mutate(share = n.vessels / sum(n.vessels))

cat("\nDistribution of n.windows.vessel (eligible vessels):\n")
print(windows_per_vessel_dist)

share_single_window <- windows_per_vessel_dist %>%
  filter(n.windows.vessel == 1) %>%
  summarise(share = sum(share)) %>%
  pull(share)
if (length(share_single_window) == 0) share_single_window <- 0

cat("Share of eligible vessels contributing exactly one window",
    "(contribute nothing to any vessel-fixed-effect model):", round(share_single_window, 4), "\n")

# ============================================================================
# 14. Save
# ============================================================================

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
save(
  window_grid.rolling, active_vessel_years.rolling,
  vessel_year_window.rolling, vessel_year_window_eligible.rolling,
  vessel_window_all_counts.rolling, vessel_window_eligibility.rolling,
  hhi_year.rolling, vessel_window_summary.rolling, passive_benchmark_window.rolling,
  attrition_ledger,
  file = rolling_panel_path
)
cat("Saved rolling panel objects to", rolling_panel_path, "\n")
