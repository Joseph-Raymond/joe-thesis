# Chapter 3 empirical pipeline, rolling-window twin of
# 07_behavioral_heterogeneity.R
#
# Table 7-rolling. Contemporaneous-window CV-on-H_bar slope by turnover
#                 type (tau.window), six columns, column (6) is the
#                 within-vessel interaction, the single most valuable new
#                 number the rolling exercise produces (design Section 5.2).
# Table 8-rolling. Adjacent-window predetermination, classify on tau.pre
#                 from the PRECEDING (non-overlapping) 6-year window,
#                 estimate on the outcome window (design Section 5.3).
# figure8b_slope_by_type_path_rolling.png. Table 7's two type slopes,
#                 re-estimated separately within each window, plotted as a
#                 coefficient path (design Section 5.4). Each individual
#                 estimate is a clean cross-section using each vessel-year
#                 at most once, no overlap problem.
#
# TIER SCOPE. This file builds Table 7-rolling and figure8b (Tier 1) and
# Table 8-rolling (Tier 2) completely. It deliberately does NOT build
# Figure 8-rolling in the baseline's four-slope-vs-Chapter-2 layout, or
# Figure 9-rolling / Table 9-rolling (the Phi-sorted appendix check), both
# Tier 3 per design Section 9.2, left for a follow-up pass so Tier 1/2 here
# get full attention rather than being rushed to make room for them.
#
# Design Section 5.1, REJECTED: running the existing baseline half-split
# INSIDE each 6-year window. A window has 4-6 active years, halves would be
# 2-3 years each, far too noisy (the baseline already flags its own
# MIN_SECOND_HALF_YEARS = 3 as marginal). Table 8-rolling instead uses an
# ADJACENT, non-overlapping classifier window six years back.
#
# Reads intermediate data/ch3_rolling.rdata (vessel_window_summary.rolling,
# vessel_year_window.rolling, vessel_window_all_counts.rolling,
# window_grid.rolling, built by 01b_build_rolling_panel.R),
# intermediate data/ch3_rolling_tau.rdata (tau_by_vessel_window.rolling,
# built by 06b_within_season_reallocation_rolling.R, which must run before
# this script), and intermediate data/ch3_within_season.rdata
# (switching_by_vessel_year, built by baseline 06_within_season_reallocation.R).
# 07_behavioral_heterogeneity.R itself is not edited at all.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
if (!exists("vessel_window_summary.rolling") || !exists("vessel_year_window.rolling") ||
    !exists("vessel_window_all_counts.rolling") || !exists("window_grid.rolling")) {
  load(rolling_panel_path)
}

rolling_tau_path <- file.path(intermediate_dir, "ch3_rolling_tau.rdata")
if (!exists("tau_by_vessel_window.rolling")) load(rolling_tau_path)

within_season_path <- file.path(intermediate_dir, "ch3_within_season.rdata")
if (!exists("switching_by_vessel_year")) load(within_season_path)

# ============================================================================
# 1. Table 7-rolling. Contemporaneous-window sample and classifier
# ============================================================================
#
# Design Section 5.2's exact sample-construction order, mirroring the
# baseline's headline choices. Restrict to eligible vessel-windows with
# is.finite(rev.cv) and rev.cv > 0, exclude is.specialist.window, require
# tau.window non-missing, THEN take the median split within that restricted
# sample, pooled across all vessel-windows (NOT within each window,
# Chapter 2's prediction is about a level of behavioral responsiveness, not
# a rank within an era, and window.start already absorbs era-level shifts).

table7_data.rolling <- vessel_window_summary.rolling %>%
  filter(is.finite(rev.cv), rev.cv > 0, !is.specialist.window) %>%
  inner_join(
    tau_by_vessel_window.rolling %>% filter(!is.na(tau.window)) %>%
      select(Vessel.ADFG.Number, window.start, tau.window, n.tau.years.window),
    by = c("Vessel.ADFG.Number", "window.start")
  ) %>%
  mutate(vessel.type = if_else(tau.window > median(tau.window), "High turnover", "Low turnover"))

cat("Table 7-rolling sample:", nrow(table7_data.rolling),
    " distinct vessels:", n_distinct(table7_data.rolling$Vessel.ADFG.Number),
    ", pooled median tau.window used as the split:", round(median(table7_data.rolling$tau.window), 4),
    ", High turnover:", sum(table7_data.rolling$vessel.type == "High turnover"), "\n")

# Within-window-median robustness line (design Section 5.2's explicit ask).
table7_data.rolling <- table7_data.rolling %>%
  group_by(window.start) %>%
  mutate(vessel.type.within.window = if_else(
    tau.window > median(tau.window), "High turnover", "Low turnover"
  )) %>%
  ungroup()

cat("Robustness, pooled-median vs within-window-median classifier agreement:",
    round(mean(table7_data.rolling$vessel.type == table7_data.rolling$vessel.type.within.window), 4), "\n")

# ============================================================================
# 2. Table 7-rolling, six columns
# ============================================================================
#
# (1)-(2) cluster on vessel only, repeated vessels across windows mean
# vcov = "hetero" is no longer appropriate the way it was on the baseline's
# genuinely cross-sectional sample (design Section 5.2). (3)-(6) two-way
# cluster, window.start in every FE slot.

low7.rolling  <- table7_data.rolling %>% filter(vessel.type == "Low turnover")
high7.rolling <- table7_data.rolling %>% filter(vessel.type == "High turnover")

m_low_roll  <- feols(log(rev.cv) ~ H_bar, data = low7.rolling,  cluster = ~Vessel.ADFG.Number)
m_high_roll <- feols(log(rev.cv) ~ H_bar, data = high7.rolling, cluster = ~Vessel.ADFG.Number)

m_low_fe_roll  <- feols(log(rev.cv) ~ H_bar | prime.fishery.window + window.start,
                         data = low7.rolling,  cluster = ~Vessel.ADFG.Number + window.start)
m_high_fe_roll <- feols(log(rev.cv) ~ H_bar | prime.fishery.window + window.start,
                         data = high7.rolling, cluster = ~Vessel.ADFG.Number + window.start)

m_interaction_roll <- feols(log(rev.cv) ~ H_bar * tau.window | prime.fishery.window + window.start,
                             data = table7_data.rolling, cluster = ~Vessel.ADFG.Number + window.start)

# Column (6), THE PAYOFF COLUMN (design Section 5.2 and 9.1). Asks whether
# the SAME vessel's own CV-on-concentration slope steepens in the windows
# where IT ITSELF reallocated more, holding everything time-invariant about
# that vessel fixed. Two cautions the design asks to be stated up front,
# adjacent overlapping windows make within-vessel movement in tau.window
# small and partly mechanical (hence the phase check below matters more
# here than anywhere else in the set), and H_bar's within-vessel variation
# is compressed relative to its cross-vessel variation, so a SMALLER
# coefficient here than in column (5) is expected on its own and not by
# itself evidence against the composition story.
m_interaction_vfe_roll <- feols(log(rev.cv) ~ H_bar * tau.window | Vessel.ADFG.Number + window.start,
                                 data = table7_data.rolling, cluster = ~Vessel.ADFG.Number + window.start)

cat("Table 7-rolling interaction coefficient (H_bar x tau.window), prime.fishery.window FE:",
    round(coef(m_interaction_roll)["H_bar:tau.window"], 4), "\n")
cat("Table 7-rolling interaction coefficient (H_bar x tau.window), VESSEL FE, the payoff column:",
    round(coef(m_interaction_vfe_roll)["H_bar:tau.window"], 4), "\n")

table7_dict.rolling <- c(tau.window = "Within-season turnover (tau, window)")

etable(
  m_low_roll, m_high_roll, m_low_fe_roll, m_high_fe_roll, m_interaction_roll, m_interaction_vfe_roll,
  headers = c("Low turnover", "High turnover", "Low turnover (FE)", "High turnover (FE)",
              "Interaction", "Interaction (vessel FE)"),
  dict = table7_dict.rolling,
  tex = TRUE,
  file = file.path(table_dir, "table7_slope_by_turnover_type_rolling.tex"),
  replace = TRUE
)

print(etable(
  m_low_roll, m_high_roll, m_low_fe_roll, m_high_fe_roll, m_interaction_roll, m_interaction_vfe_roll,
  dict = table7_dict.rolling
))

cat("Wrote table7_slope_by_turnover_type_rolling.tex. N:", nrow(table7_data.rolling),
    " distinct vessels:", n_distinct(table7_data.rolling$Vessel.ADFG.Number), "\n")

# ============================================================================
# 3. figure8b_slope_by_type_path_rolling.png
# ============================================================================
#
# Design Section 5.4. log(rev.cv) ~ H_bar estimated separately WITHIN each
# window, by turnover type (the pooled-median classifier from Section 1
# above, held fixed across windows, only the fitting sample changes).

window_starts_t7 <- sort(unique(table7_data.rolling$window.start))

slope_path.rolling <- lapply(window_starts_t7, function(w) {
  dat_w <- table7_data.rolling %>% filter(window.start == w)
  bind_rows(lapply(c("Low turnover", "High turnover"), function(ty) {
    dat_ty <- dat_w %>% filter(vessel.type == ty)
    if (nrow(dat_ty) < 20) {
      return(tibble(window.start = w, vessel.type = ty, slope = NA_real_, se = NA_real_, n = nrow(dat_ty)))
    }
    m <- tryCatch(feols(log(rev.cv) ~ H_bar, data = dat_ty, vcov = "hetero"), error = function(e) NULL)
    if (is.null(m) || !("H_bar" %in% names(coef(m)))) {
      return(tibble(window.start = w, vessel.type = ty, slope = NA_real_, se = NA_real_, n = nrow(dat_ty)))
    }
    tibble(window.start = w, vessel.type = ty,
           slope = unname(coef(m)["H_bar"]), se = unname(se(m)["H_bar"]), n = nrow(dat_ty))
  }))
}) %>% bind_rows()

cat("Per-window/type slope fits, computable estimates:", sum(is.finite(slope_path.rolling$slope)),
    "of", nrow(slope_path.rolling), "\n")

figure8b.rolling <- slope_path.rolling %>%
  filter(is.finite(slope)) %>%
  ggplot(aes(x = window.start, y = slope, color = vessel.type, fill = vessel.type)) +
  geom_ribbon(aes(ymin = slope - 1.96 * se, ymax = slope + 1.96 * se), alpha = 0.15, color = NA) +
  geom_line() +
  geom_point(size = 1.2) +
  scale_color_manual(values = c("Low turnover" = "steelblue", "High turnover" = "firebrick")) +
  scale_fill_manual(values = c("Low turnover" = "steelblue", "High turnover" = "firebrick")) +
  labs(
    title = "CV-on-H_bar slope by turnover type, by rolling window",
    subtitle = "Separate cross-sectional fit per window and type, 95% CI ribbon. Chapter 2 predicts High above Low throughout",
    x = "Window start year", y = "Estimated slope, log(rev.cv) ~ H_bar", color = NULL, fill = NULL
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure8b_slope_by_type_path_rolling.png"),
       figure8b.rolling, width = 8, height = 5, dpi = 300)

cat("Wrote figure8b_slope_by_type_path_rolling.png\n")

ordering_check.rolling <- slope_path.rolling %>%
  filter(is.finite(slope)) %>%
  select(window.start, vessel.type, slope) %>%
  pivot_wider(names_from = vessel.type, values_from = slope) %>%
  filter(!is.na(`Low turnover`), !is.na(`High turnover`)) %>%
  mutate(high.above.low = `High turnover` > `Low turnover`)

cat("Figure 8b-rolling: windows where the High-turnover slope exceeds the Low-turnover slope:",
    sum(ordering_check.rolling$high.above.low), "of", nrow(ordering_check.rolling), "\n")

# ============================================================================
# 4. Table 8-rolling. Adjacent-window predetermination (design Section 5.3)
# ============================================================================
#
# For outcome window w(s) = [s, s+5], the classifier window is
# c(s) = [s-6, s-1], i.e. window.start = s - ROLL_WINDOW_WIDTH, adjacent and
# NON-OVERLAPPING by construction, a cleaner severance between classifier
# and outcome than the baseline's vessel-relative half-split.
#
# tau.pre needs to be computed over ALL (vessel, window.start) pairs, not
# just the ones clearing the standard ROLL_MIN_ACTIVE_YEARS (4) eligibility
# floor, the classifier window's own floor is ROLL_MIN_LOOKBACK_YEARS (3),
# deliberately lower (design Section 5.3, "the lookback floor is 3 rather
# than 4 on purpose"). tau_by_vessel_window.rolling (06b_'s output) only
# covers windows that clear the 4-year floor, so it is NOT reused here,
# this section recomputes tau on the full vessel_year_window.rolling
# expansion instead.

tau_by_vessel_window_all.rolling <- vessel_year_window.rolling %>%
  select(Vessel.ADFG.Number, Batch.Year, window.start) %>%
  inner_join(
    switching_by_vessel_year %>% select(Vessel.ADFG.Number, Batch.Year, weekly.switching.per.transition),
    by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many"
  ) %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  summarise(n.tau.years.window.raw = n(), tau.window.raw = mean(weekly.switching.per.transition), .groups = "drop")

# Shift both the active-year lookback counts and the raw tau table forward
# by ROLL_WINDOW_WIDTH years, so window.start on the SHIFTED table now
# indexes the OUTCOME window s that classifier window c(s) feeds into.
lookback_counts.rolling <- vessel_window_all_counts.rolling %>%
  transmute(Vessel.ADFG.Number, window.start = window.start + ROLL_WINDOW_WIDTH, n.years.lookback = n.years.window)

tau_pre_lookback.rolling <- tau_by_vessel_window_all.rolling %>%
  transmute(Vessel.ADFG.Number, window.start = window.start + ROLL_WINDOW_WIDTH,
            n.tau.years.pre = n.tau.years.window.raw, tau.pre.raw = tau.window.raw)

outcome_starts_table8 <- window_grid.rolling %>%
  filter(window.start >= MIN_YEAR + ROLL_WINDOW_WIDTH) %>%
  pull(window.start)

cat("Table 8-rolling outcome window grid:", length(outcome_starts_table8), "windows,",
    min(outcome_starts_table8), "through", max(outcome_starts_table8), "\n")

table8_base.rolling <- vessel_window_summary.rolling %>%
  filter(window.start %in% outcome_starts_table8, is.finite(rev.cv), rev.cv > 0, !is.specialist.window)

table8_data.rolling <- table8_base.rolling %>%
  inner_join(lookback_counts.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(n.years.lookback >= ROLL_MIN_LOOKBACK_YEARS) %>%
  inner_join(tau_pre_lookback.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(n.tau.years.pre >= ROLL_MIN_TAU_YEARS) %>%
  mutate(tau.pre = tau.pre.raw,
         vessel.type = if_else(tau.pre > median(tau.pre), "High turnover", "Low turnover"))

cat("Table 8-rolling sample:", nrow(table8_data.rolling),
    " distinct vessels:", n_distinct(table8_data.rolling$Vessel.ADFG.Number),
    ", pooled median tau.pre:", round(median(table8_data.rolling$tau.pre), 4),
    ", High turnover:", sum(table8_data.rolling$vessel.type == "High turnover"), "\n")

low8.rolling  <- table8_data.rolling %>% filter(vessel.type == "Low turnover")
high8.rolling <- table8_data.rolling %>% filter(vessel.type == "High turnover")

m_split_low_roll  <- feols(log(rev.cv) ~ H_bar, data = low8.rolling,  cluster = ~Vessel.ADFG.Number)
m_split_high_roll <- feols(log(rev.cv) ~ H_bar, data = high8.rolling, cluster = ~Vessel.ADFG.Number)

m_split_interaction_roll <- feols(log(rev.cv) ~ H_bar * tau.pre | prime.fishery.window + window.start,
                                   data = table8_data.rolling, cluster = ~Vessel.ADFG.Number + window.start)
m_split_interaction_vfe_roll <- feols(log(rev.cv) ~ H_bar * tau.pre | Vessel.ADFG.Number + window.start,
                                       data = table8_data.rolling, cluster = ~Vessel.ADFG.Number + window.start)

table8_dict.rolling <- c(tau.pre = "Within-season turnover, preceding window (tau.pre)")

etable(
  m_split_low_roll, m_split_high_roll, m_split_interaction_roll, m_split_interaction_vfe_roll,
  headers = c("Low tau.pre", "High tau.pre", "Interaction", "Interaction (vessel FE)"),
  dict = table8_dict.rolling,
  tex = TRUE,
  file = file.path(table_dir, "table8_split_sample_slope_by_type_rolling.tex"),
  replace = TRUE
)

print(etable(
  m_split_low_roll, m_split_high_roll, m_split_interaction_roll, m_split_interaction_vfe_roll,
  dict = table8_dict.rolling
))

cat("Wrote table8_split_sample_slope_by_type_rolling.tex. N:", nrow(table8_data.rolling),
    " distinct vessels:", n_distinct(table8_data.rolling$Vessel.ADFG.Number), "\n")

# ---- Robustness, strict lookback floor of ROLL_MIN_ACTIVE_YEARS (4) instead
#      of ROLL_MIN_LOOKBACK_YEARS (3), design Section 5.3's explicit ask ----

table8_data_strict.rolling <- table8_base.rolling %>%
  inner_join(lookback_counts.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(n.years.lookback >= ROLL_MIN_ACTIVE_YEARS) %>%
  inner_join(tau_pre_lookback.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(n.tau.years.pre >= ROLL_MIN_TAU_YEARS) %>%
  mutate(tau.pre = tau.pre.raw,
         vessel.type = if_else(tau.pre > median(tau.pre), "High turnover", "Low turnover"))

m_split_low_strict_roll  <- feols(log(rev.cv) ~ H_bar,
                                   data = filter(table8_data_strict.rolling, vessel.type == "Low turnover"),
                                   cluster = ~Vessel.ADFG.Number)
m_split_high_strict_roll <- feols(log(rev.cv) ~ H_bar,
                                   data = filter(table8_data_strict.rolling, vessel.type == "High turnover"),
                                   cluster = ~Vessel.ADFG.Number)

cat("Robustness, lookback floor =", ROLL_MIN_ACTIVE_YEARS, "instead of", ROLL_MIN_LOOKBACK_YEARS,
    ", N:", nrow(table8_data_strict.rolling),
    ", slope Low:", round(coef(m_split_low_strict_roll)["H_bar"], 4),
    " High:", round(coef(m_split_high_strict_roll)["H_bar"], 4),
    " (standard floor gave Low:", round(coef(m_split_low_roll)["H_bar"], 4),
    " High:", round(coef(m_split_high_roll)["H_bar"], 4), ")\n")

# ============================================================================
# 5. Mandatory stride-6 phase check (design Section 2.2, Layer 3)
# ============================================================================
#
# Column (6) of Table 7-rolling needs this more than any other model in the
# set (design Section 5.2), adjacent overlapping windows make within-vessel
# movement in tau.window small and partly mechanical, so its phase range is
# reported prominently here in addition to the standard printed output.

pc_t7_interaction <- roll_phase_check(
  fml = log(rev.cv) ~ H_bar * tau.window | prime.fishery.window + window.start,
  data = table7_data.rolling, coef_name = "H_bar:tau.window",
  label = "Table 7-rolling: interaction (prime FE)"
)
pc_t7_interaction_vfe <- roll_phase_check(
  fml = log(rev.cv) ~ H_bar * tau.window | Vessel.ADFG.Number + window.start,
  data = table7_data.rolling, coef_name = "H_bar:tau.window",
  label = "Table 7-rolling: interaction (vessel FE, column 6)"
)

cat("\n*** Table 7-rolling column (6) phase range (the model needing this check most):",
    round(pc_t7_interaction_vfe$summary$phase.min, 4), "to", round(pc_t7_interaction_vfe$summary$phase.max, 4),
    ", full-panel estimate:", round(pc_t7_interaction_vfe$summary$estimate.full, 4), "***\n")

pc_t8_interaction <- roll_phase_check(
  fml = log(rev.cv) ~ H_bar * tau.pre | prime.fishery.window + window.start,
  data = table8_data.rolling, coef_name = "H_bar:tau.pre",
  label = "Table 8-rolling: interaction (prime FE)"
)
pc_t8_interaction_vfe <- roll_phase_check(
  fml = log(rev.cv) ~ H_bar * tau.pre | Vessel.ADFG.Number + window.start,
  data = table8_data.rolling, coef_name = "H_bar:tau.pre",
  label = "Table 8-rolling: interaction (vessel FE)"
)

if (file.exists(ROLL_PHASE_CHECK_PATH)) {
  load(ROLL_PHASE_CHECK_PATH)
} else {
  rolling_overlap_robustness <- tibble(
    model = character(), coefficient = character(), estimate.full = double(),
    se.full = double(), used.twoway.cluster = logical(),
    phase.min = double(), phase.median = double(), phase.max = double(),
    se.phase.median = double(), se.ratio = double(),
    n.obs.full = double(), n.fit.full = double(), retention.full = double(),
    n.fit.phase.median = double(), retention.phase.median = double(),
    flag.outside.phase.range = logical()
  )
}

new_rows <- bind_rows(
  pc_t7_interaction$summary, pc_t7_interaction_vfe$summary,
  pc_t8_interaction$summary, pc_t8_interaction_vfe$summary
)
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

# ============================================================================
# 6. Save
# ============================================================================
#
# table7_data.rolling and table8_data.rolling saved so a follow-up pass
# building the Tier 3 Figure 8-rolling baseline layout / Figure 9-rolling /
# Table 9-rolling does not have to reconstruct the classifier and sample
# machinery above from scratch.

rolling_behavioral_path <- file.path(intermediate_dir, "ch3_rolling_behavioral.rdata")
save(table7_data.rolling, table8_data.rolling, slope_path.rolling, file = rolling_behavioral_path)
cat("Saved", rolling_behavioral_path, "\n")
