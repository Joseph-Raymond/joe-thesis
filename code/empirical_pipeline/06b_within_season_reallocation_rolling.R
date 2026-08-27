# Chapter 3 empirical pipeline, rolling-window twin of
# 06_within_season_reallocation.R's Table 6
#
# Table 6-rolling. Vessel x window Phi and rev.cv (from
#                  vessel_window_summary.rolling) on within-season target
#                  switching, with prime.fishery.window and window.start
#                  fixed effects. Identical to the baseline's four columns
#                  with `period` replaced by `window.start` and
#                  `vessel_period_summary` replaced by
#                  `vessel_window_summary.rolling` (design Section 4.3).
#
# This is design Section 4.2's highest-value single conversion in the whole
# rolling exercise, it is already at vessel-period grain (a pure swap of the
# period definition), and its vessel-FE columns give the pipeline's only
# existing vessel fixed effect (currently identified off vessels with two or
# more of only THREE periods) a far richer source of within-vessel contrast.
#
# NO TICKET RELOAD NEEDED. Reads intermediate data/ch3_within_season.rdata
# (switching_by_vessel_year, already built by baseline
# 06_within_season_reallocation.R, which run_all.R runs before any rolling
# script) and intermediate data/ch3_rolling.rdata (vessel_window_summary.rolling,
# vessel_year_window_eligible.rolling, built by 01b_build_rolling_panel.R).
# 06_within_season_reallocation.R itself is not edited at all.
#
# Preserves the raw-versus-normalized switching split exactly as the
# baseline has it (trap #6). Table 6-rolling uses the raw weekly.switching
# mean with mean.active.weeks.window as a control, NOT tau (the
# per-transition-normalized measure Tables 7-rolling/8-rolling use).
#
# Saves tau_by_vessel_window.rolling and switching_by_vessel_window.rolling
# to intermediate data/ch3_rolling_tau.rdata, a SEPARATE file from
# ch3_rolling.rdata so 01b_build_rolling_panel.R has no dependency on this
# script having run (design Section 8.4, item 4). tau_by_vessel_window.rolling
# is built here for 07b_behavioral_heterogeneity_rolling.R's Table 7/8-rolling
# even though Table 6-rolling itself only needs the raw (non-normalized)
# switching measure.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/00b_rolling_periods.R")

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
if (!exists("vessel_window_summary.rolling") || !exists("vessel_year_window_eligible.rolling")) {
  load(rolling_panel_path)
}

within_season_path <- file.path(intermediate_dir, "ch3_within_season.rdata")
if (!exists("switching_by_vessel_year")) load(within_season_path)

# ============================================================================
# 1. tau_by_vessel_window.rolling and switching_by_vessel_window.rolling
# ============================================================================
#
# Both built the same way, restrict switching_by_vessel_year (already
# restricted to vessel-years with a computable measure, i.e. >= 2 active
# weeks, see 06_within_season_reallocation.R's own construction) to the
# window's active years via vessel_year_window_eligible.rolling, then take
# the per-(vessel, window) mean. Using an inner_join means a window's own
# n.years.switching.window can be smaller than n.years.window whenever some
# of the window's active years had fewer than 2 active weeks and so never
# entered switching_by_vessel_year at all (not zero-filled there, "no
# turnover measurable" is not the same claim as "turnover was zero", per
# 06_'s own comment).
#
# tau.window additionally requires n.tau.years.window >= ROLL_MIN_TAU_YEARS
# (design Section 3.5), set to NA and dropped below that floor, this is
# what Table 7-rolling/8-rolling actually use, NOT switching_by_vessel_window.rolling's
# raw mean.

vessel_year_switching_window.rolling <- vessel_year_window_eligible.rolling %>%
  select(Vessel.ADFG.Number, Batch.Year, window.start) %>%
  inner_join(
    switching_by_vessel_year %>%
      select(Vessel.ADFG.Number, Batch.Year, weekly.switching, n.active.weeks,
             weekly.switching.per.transition),
    by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many"
  )

switching_by_vessel_window.rolling <- vessel_year_switching_window.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(
    n.years.switching.window       = n(),
    within.season.switching.window = mean(weekly.switching),
    mean.active.weeks.window       = mean(n.active.weeks),
    .groups = "drop"
  )

tau_by_vessel_window.rolling <- vessel_year_switching_window.rolling %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(
    n.tau.years.window = n(),
    tau.window          = mean(weekly.switching.per.transition),
    .groups = "drop"
  ) %>%
  mutate(tau.window = if_else(n.tau.years.window >= ROLL_MIN_TAU_YEARS, tau.window, NA_real_))

cat("switching_by_vessel_window.rolling:", nrow(switching_by_vessel_window.rolling), "vessel-windows\n")
cat("tau_by_vessel_window.rolling:", nrow(tau_by_vessel_window.rolling), "vessel-windows, of which",
    sum(!is.na(tau_by_vessel_window.rolling$tau.window)),
    "clear the n.tau.years.window >=", ROLL_MIN_TAU_YEARS, "floor\n")

rolling_tau_path <- file.path(intermediate_dir, "ch3_rolling_tau.rdata")
save(tau_by_vessel_window.rolling, switching_by_vessel_window.rolling, file = rolling_tau_path)
cat("Saved", rolling_tau_path, "\n")

# ============================================================================
# 2. Table 6-rolling
# ============================================================================
#
# vessel_window_summary.rolling already carries prime.fishery.window, so no
# extra join is needed for the fixed effect (the baseline needed a left_join
# to vessel_summary for prime.fishery, that lifetime object is not the right
# one here, see design Section 3.2).

table6_data.rolling <- vessel_window_summary.rolling %>%
  filter(is.finite(rev.cv)) %>%
  inner_join(switching_by_vessel_window.rolling, by = c("Vessel.ADFG.Number", "window.start"))

cat("Vessel x window observations entering Table 6-rolling:", nrow(table6_data.rolling),
    " distinct vessels:", n_distinct(table6_data.rolling$Vessel.ADFG.Number), "\n")

# Three-layer inference protocol (design Section 2.2), window.start in the
# FE slot and two-way clustering everywhere.
m_phi_switching_roll <- feols(
  Phi ~ within.season.switching.window + mean.active.weeks.window | prime.fishery.window + window.start,
  data = table6_data.rolling, cluster = ~Vessel.ADFG.Number + window.start
)
m_cv_switching_roll <- feols(
  rev.cv ~ within.season.switching.window + mean.active.weeks.window | prime.fishery.window + window.start,
  data = table6_data.rolling, cluster = ~Vessel.ADFG.Number + window.start
)

# Vessel-FE columns, the payoff of the whole conversion (design Section 4.2),
# identified off vessels with two or more eligible windows rather than the
# baseline's two-or-more-of-three periods.
m_phi_switching_vfe_roll <- feols(
  Phi ~ within.season.switching.window + mean.active.weeks.window | Vessel.ADFG.Number + window.start,
  data = table6_data.rolling, cluster = ~Vessel.ADFG.Number + window.start
)
m_cv_switching_vfe_roll <- feols(
  rev.cv ~ within.season.switching.window + mean.active.weeks.window | Vessel.ADFG.Number + window.start,
  data = table6_data.rolling, cluster = ~Vessel.ADFG.Number + window.start
)

table6_dict.rolling <- c(
  within.season.switching.window = "Target switching (window mean)",
  mean.active.weeks.window       = "Active weeks (window mean)"
)

etable(
  m_phi_switching_roll, m_cv_switching_roll, m_phi_switching_vfe_roll, m_cv_switching_vfe_roll,
  headers = c("Phi", "rev.cv", "Phi (vessel FE)", "rev.cv (vessel FE)"),
  dict = table6_dict.rolling,
  tex = TRUE,
  file = file.path(table_dir, "table6_annual_instability_on_within_season_switching_rolling.tex"),
  replace = TRUE
)

print(etable(
  m_phi_switching_roll, m_cv_switching_roll, m_phi_switching_vfe_roll, m_cv_switching_vfe_roll,
  dict = table6_dict.rolling
))

cat("Wrote table6_annual_instability_on_within_season_switching_rolling.tex\n")
cat("Table 6-rolling N (vessel-windows):", nrow(table6_data.rolling),
    " distinct vessels:", n_distinct(table6_data.rolling$Vessel.ADFG.Number), "\n")

# ============================================================================
# 3. Mandatory stride-6 phase check (design Section 2.2, Layer 3), on the
#    two vessel-FE columns, the headline gain of this conversion
# ============================================================================

pc_phi_vfe <- roll_phase_check(
  fml = Phi ~ within.season.switching.window + mean.active.weeks.window | Vessel.ADFG.Number + window.start,
  data = table6_data.rolling, coef_name = "within.season.switching.window",
  label = "Table 6-rolling: Phi (vessel FE)"
)
pc_cv_vfe <- roll_phase_check(
  fml = rev.cv ~ within.season.switching.window + mean.active.weeks.window | Vessel.ADFG.Number + window.start,
  data = table6_data.rolling, coef_name = "within.season.switching.window",
  label = "Table 6-rolling: rev.cv (vessel FE)"
)

if (file.exists(ROLL_PHASE_CHECK_PATH)) {
  load(ROLL_PHASE_CHECK_PATH)
} else {
  rolling_overlap_robustness <- tibble(
    model = character(), coefficient = character(), estimate.full = double(),
    se.full = double(), used.twoway.cluster = logical(),
    phase.min = double(), phase.median = double(), phase.max = double(),
    se.phase.median = double(), se.ratio = double(), flag.outside.phase.range = logical()
  )
}

new_rows <- bind_rows(pc_phi_vfe$summary, pc_cv_vfe$summary)
rolling_overlap_robustness <- rolling_overlap_robustness %>%
  filter(!(paste(model, coefficient) %in% paste(new_rows$model, new_rows$coefficient))) %>%
  bind_rows(new_rows)

save(rolling_overlap_robustness, file = ROLL_PHASE_CHECK_PATH)

print(
  xtable(
    rolling_overlap_robustness %>% select(-flag.outside.phase.range),
    caption = "Rolling overlap-robustness check, full-panel two-way-clustered estimate versus the stride-6 non-overlapping phase estimates, one row per headline model coefficient",
    label = "tab:ch3-rolling-overlap-robustness", digits = 4
  ),
  file = file.path(table_dir, "table_rolling_overlap_robustness.tex"),
  include.rownames = FALSE
)
cat("Wrote table_rolling_overlap_robustness.tex (", nrow(rolling_overlap_robustness), "headline model rows so far)\n")

if (any(rolling_overlap_robustness$flag.outside.phase.range)) {
  cat("*** WARNING: the following headline models have a full-panel estimate outside their own",
      "phase min-max range, inspect before trusting them: ***\n")
  print(rolling_overlap_robustness %>% filter(flag.outside.phase.range) %>% select(model, coefficient))
}
