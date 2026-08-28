# Chapter 3 empirical pipeline, rolling-window twin of
# 05_table4_figure3.R
#
# Table 4-rolling.       Baseline versus decomposed CV-on-HHI regression at
#                        the vessel-window grain, standardized versions,
#                        plus a new vessel-FE decomposed column (the payoff
#                        column, design Section 9.1) and an inverse-window-
#                        count-weighted robustness column (design Section 2.4).
# Table 4-pooled-rolling. Same four/six models on the full pooled sample
#                        (specialists included), robustness.
# Figure 3-rolling.      Passive buy-and-hold benchmark scatter, rolling.
# Figure 3b-rolling.     Gap-by-Phi, two-stage vessel-clustered bin SEs
#                        (design Section 3.4, trap #9).
# figure4b_decomposition_path_rolling.png. g1 (H_LR) and g2 (Phi)
#                        re-estimated separately within each window,
#                        plotted as a coefficient path (design Section 5.4's
#                        generalized pattern, applied to Table 4).
#
# Reads intermediate data/ch3_panel.rdata (read-only, fleet_mean_revenue not
# actually needed here, passive_benchmark_window.rolling already carries the
# window-local passive series) and intermediate data/ch3_rolling.rdata
# (vessel_window_summary.rolling, passive_benchmark_window.rolling,
# window_grid.rolling), both built by 01b_build_rolling_panel.R.
# 05_table4_figure3.R itself is not edited at all.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
if (!exists("vessel_window_summary.rolling") || !exists("passive_benchmark_window.rolling") ||
    !exists("window_grid.rolling")) {
  load(rolling_panel_path)
}

# ============================================================================
# 1. Sample construction
# ============================================================================
#
# vessel_window_summary.rolling is already restricted to eligible windows
# (semi-joined against vessel_window_eligibility.rolling inside 01b_), so
# the only additional filter needed here mirrors 05_'s own
# is.finite(rev.cv) restriction, never meets.min.years (trap #1, not
# applicable here anyway since this object was never built from
# vessel_summary in the first place).

vessel_analysis.rolling <- vessel_window_summary.rolling %>%
  filter(is.finite(rev.cv))

cat("Vessel-windows entering Table 4-rolling:", nrow(vessel_analysis.rolling),
    " distinct vessels:", n_distinct(vessel_analysis.rolling$Vessel.ADFG.Number),
    ", of which single-fishery specialists (window):", sum(vessel_analysis.rolling$is.specialist.window), "\n")

vessel_multi.rolling <- vessel_analysis.rolling %>% filter(!is.specialist.window)

# ============================================================================
# 2. Table 4-rolling, main text (multi-fishery vessel-windows)
# ============================================================================

m_baseline_roll   <- feols(rev.cv ~ H_bar | prime.fishery.window + window.start,
                            data = vessel_multi.rolling, cluster = ~Vessel.ADFG.Number + window.start)
m_decomposed_roll <- feols(rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
                            data = vessel_multi.rolling, cluster = ~Vessel.ADFG.Number + window.start)

vessel_std.rolling <- vessel_multi.rolling %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

m_baseline_std_roll   <- feols(z.rev.cv ~ z.H_bar | prime.fishery.window + window.start,
                                data = vessel_std.rolling, cluster = ~Vessel.ADFG.Number + window.start)
m_decomposed_std_roll <- feols(z.rev.cv ~ z.H_LR + z.Phi | prime.fishery.window + window.start,
                                data = vessel_std.rolling, cluster = ~Vessel.ADFG.Number + window.start)

# NEW, the payoff column (design Section 9.1). A genuine vessel fixed effect
# in the decomposition, identified off within-vessel variation across
# windows rather than the baseline's cross-vessel-only comparison.
m_decomposed_vfe_roll <- feols(rev.cv ~ H_LR + Phi | Vessel.ADFG.Number + window.start,
                                data = vessel_multi.rolling, cluster = ~Vessel.ADFG.Number + window.start)

# Inverse-window-count-weighted robustness column (design Section 2.4).
# Unweighted vessel-window regressions give a vessel with many eligible
# windows many times the weight of a vessel with few, this restores the
# baseline's one-vessel-one-vote estimand as a check, not a replacement.
#
# The weight is recomputed HERE, inside vessel_multi.rolling, rather than
# reusing 01b_'s inv.window.count column directly. 01b_'s inv.window.count
# is 1 / (ELIGIBLE windows for that vessel, vessel_window_eligibility.rolling),
# but this regression runs on vessel_multi.rolling, which has already
# dropped within-window specialists and non-finite rev.cv, so a vessel's
# inv.window.count values no longer sum to 1 over the rows actually being
# fit, they sum to (surviving windows) / (eligible windows), a ratio that
# differs across vessels and is not independent of the regressors (a vessel
# that specializes more of the time loses more of its windows here, and
# specialization is exactly what H_LR/Phi measure). n.windows.vessel /
# inv.window.count from 01b_ are left alone, they still correctly describe
# eligibility, just should not be reused as a weight for a filtered sample.
vessel_multi.rolling <- vessel_multi.rolling %>%
  add_count(Vessel.ADFG.Number, name = "n.windows.vessel.insample") %>%
  mutate(inv.window.count.insample = 1 / n.windows.vessel.insample)

cat("Eligible vs. in-sample window count per vessel (Table 4-rolling weighted column), ",
    "share of vessel-windows where these differ:",
    round(mean(vessel_multi.rolling$n.windows.vessel.insample != vessel_multi.rolling$n.windows.vessel), 4), "\n")

m_decomposed_weighted_roll <- feols(rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
                                     data = vessel_multi.rolling, weights = ~inv.window.count.insample,
                                     cluster = ~Vessel.ADFG.Number + window.start)

etable(
  m_baseline_roll, m_decomposed_roll, m_baseline_std_roll, m_decomposed_std_roll,
  m_decomposed_vfe_roll, m_decomposed_weighted_roll,
  headers = c("Baseline", "Decomposed", "Baseline (z)", "Decomposed (z)",
              "Decomposed (vessel FE)", "Decomposed (inv. window wt.)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression_rolling.tex"),
  replace = TRUE
)

print(etable(
  m_baseline_roll, m_decomposed_roll, m_baseline_std_roll, m_decomposed_std_roll,
  m_decomposed_vfe_roll, m_decomposed_weighted_roll
))

g2_share.rolling <- coef(m_decomposed_std_roll)["z.Phi"] /
  (coef(m_decomposed_std_roll)["z.H_LR"] + coef(m_decomposed_std_roll)["z.Phi"])
cat("Rolling standardized share of the decomposed slope loading onto Phi (g2_share):",
    round(g2_share.rolling, 3), "\n")

cat("Wrote table4_decomposition_regression_rolling.tex. Table 4-rolling (multi-fishery) N:",
    nrow(vessel_multi.rolling), " distinct vessels:", n_distinct(vessel_multi.rolling$Vessel.ADFG.Number), "\n")

# ============================================================================
# 3. Table 4-pooled-rolling (robustness, specialists included)
# ============================================================================

m_baseline_pooled_roll   <- feols(rev.cv ~ H_bar | prime.fishery.window + window.start,
                                   data = vessel_analysis.rolling, cluster = ~Vessel.ADFG.Number + window.start)
m_decomposed_pooled_roll <- feols(rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
                                   data = vessel_analysis.rolling, cluster = ~Vessel.ADFG.Number + window.start)

vessel_std_pooled.rolling <- vessel_analysis.rolling %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

m_baseline_std_pooled_roll   <- feols(z.rev.cv ~ z.H_bar | prime.fishery.window + window.start,
                                       data = vessel_std_pooled.rolling, cluster = ~Vessel.ADFG.Number + window.start)
m_decomposed_std_pooled_roll <- feols(z.rev.cv ~ z.H_LR + z.Phi | prime.fishery.window + window.start,
                                       data = vessel_std_pooled.rolling, cluster = ~Vessel.ADFG.Number + window.start)

etable(
  m_baseline_pooled_roll, m_decomposed_pooled_roll,
  m_baseline_std_pooled_roll, m_decomposed_std_pooled_roll,
  headers = c("Baseline (pooled)", "Decomposed (pooled)",
              "Baseline (pooled, z)", "Decomposed (pooled, z)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression_pooled_rolling.tex"),
  replace = TRUE
)

cat("Wrote table4_decomposition_regression_pooled_rolling.tex. N:", nrow(vessel_analysis.rolling),
    " distinct vessels:", n_distinct(vessel_analysis.rolling$Vessel.ADFG.Number), "\n")

# ============================================================================
# 4. Figure 3-rolling. Passive buy-and-hold benchmark scatter
# ============================================================================

fig3_data.rolling <- vessel_analysis.rolling %>%
  select(Vessel.ADFG.Number, window.start, rev.cv, H_bar, H_LR, Phi, is.specialist.window) %>%
  inner_join(passive_benchmark_window.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(is.finite(passive.cv))

cat("Vessel-windows with a computable passive benchmark:", nrow(fig3_data.rolling),
    " of which single-fishery specialists (window):", sum(fig3_data.rolling$is.specialist.window), "\n")

figure3.rolling <- fig3_data.rolling %>%
  filter(!is.specialist.window) %>%
  ggplot(aes(x = passive.cv, y = rev.cv)) +
  geom_point(alpha = 0.08, size = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "firebrick") +
  labs(
    title = "Realized revenue CV versus a passive buy-and-hold benchmark (rolling)",
    subtitle = "Multi-fishery vessel-windows, window-local weights, one point per eligible (vessel, window)",
    x = "Passive benchmark CV (buy-and-hold, window-local weights)",
    y = "Realized revenue CV"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3_passive_benchmark_rolling.png"),
       figure3.rolling, width = 7, height = 6, dpi = 300)

cat("Wrote figure3_passive_benchmark_rolling.png\n")

# ============================================================================
# 5. Figure 3b-rolling. Gap between realized and passive CV, binned by Phi
# ============================================================================
#
# gap_iw = rev.cv_iw - passive.cv_iw. Trap #9, the bin standard error must
# NOT be sd(gap) / sqrt(n_obs), a bin can contain several overlapping-window
# observations from the SAME vessel, which would badly understate it.
# two_stage_bin_summary() collapses to one value per vessel WITHIN the bin
# first (mean gap across that vessel's own windows landing in this bin),
# then takes sd / sqrt(n_distinct_vessels) over those vessel means, treating
# the vessel (not the vessel-window) as the independent sampling unit.

fig3b_data.rolling <- fig3_data.rolling %>%
  mutate(gap = rev.cv - passive.cv)

two_stage_bin_summary <- function(df) {
  vessel_means <- df %>%
    group_by(Vessel.ADFG.Number) %>%
    summarise(vessel.gap = mean(gap), .groups = "drop")
  tibble(
    n         = nrow(df),
    n.vessels = nrow(vessel_means),
    mean.Phi  = mean(df$Phi),
    mean.gap  = mean(vessel_means$vessel.gap),
    se.gap    = sd(vessel_means$vessel.gap) / sqrt(nrow(vessel_means))
  )
}

specialist_summary.rolling <- fig3b_data.rolling %>%
  filter(is.specialist.window) %>%
  two_stage_bin_summary() %>%
  mutate(bin.label = "Specialists\n(Phi = 0)", bin.order = 0)

# N_GAP_BINS_ROLLING, not N_GAP_BINS, N_GAP_BINS is on the design's
# do-not-reassign list (Section 8.3), a new name avoids any risk of
# collision if a rolling script is ever sourced interactively alongside the
# baseline 05_table4_figure3.R in the same session.
N_GAP_BINS_ROLLING <- 8

multi_summary.rolling <- fig3b_data.rolling %>%
  filter(!is.specialist.window) %>%
  mutate(phi.bin = ntile(Phi, N_GAP_BINS_ROLLING)) %>%
  group_by(phi.bin) %>%
  group_modify(~ two_stage_bin_summary(.x)) %>%
  ungroup() %>%
  mutate(bin.label = paste0("Q", phi.bin), bin.order = phi.bin) %>%
  select(-phi.bin)

gap_by_phi.rolling <- bind_rows(specialist_summary.rolling, multi_summary.rolling) %>%
  mutate(bin.label = fct_reorder(bin.label, bin.order), is.specialist.bin = bin.order == 0)

print(gap_by_phi.rolling)

figure3b.rolling <- gap_by_phi.rolling %>%
  ggplot(aes(x = bin.label, y = mean.gap, color = is.specialist.bin)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean.gap - 1.96 * se.gap, ymax = mean.gap + 1.96 * se.gap), width = 0.2) +
  geom_line(
    data = gap_by_phi.rolling %>% filter(!is.specialist.bin),
    aes(x = bin.label, y = mean.gap, group = 1), color = "steelblue", inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "gray40", "FALSE" = "steelblue"), guide = "none") +
  labs(
    title = "Gap between realized and passive-benchmark CV (rolling)",
    subtitle = "By reallocation intensity (Phi), window-local, two-stage vessel-clustered bin SEs",
    x = "Reallocation intensity (Phi), specialists then increasing bins",
    y = "Mean gap (realized CV - passive CV)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3b_gap_by_phi_rolling.png"),
       figure3b.rolling, width = 7, height = 5, dpi = 300)

cat("Wrote figure3b_gap_by_phi_rolling.png\n")

# ============================================================================
# 6. figure4b_decomposition_path_rolling.png. g1/g2 re-estimated per window
# ============================================================================
#
# Design Section 5.4's "estimate the baseline cross-sectional specification
# separately within each window and plot the coefficient path" pattern,
# applied to Table 4's decomposed model. Each window's fit uses only that
# window's own vessel-windows (a genuine cross-section, one row per vessel),
# so vcov = "hetero" is appropriate, the same reasoning Table 7/8's
# cross-sectional models use, NOT vessel clustering, which would be
# degenerate within a single window slice.

window_starts_multi <- sort(unique(vessel_multi.rolling$window.start))

decomposition_path.rolling <- lapply(window_starts_multi, function(w) {
  dat_w <- vessel_multi.rolling %>% filter(window.start == w)
  if (nrow(dat_w) < 30 || n_distinct(dat_w$prime.fishery.window) < 2) {
    return(tibble(window.start = w, g1 = NA_real_, g1.se = NA_real_,
                   g2 = NA_real_, g2.se = NA_real_, n = nrow(dat_w)))
  }
  m_w <- tryCatch(
    feols(rev.cv ~ H_LR + Phi | prime.fishery.window, data = dat_w, vcov = "hetero"),
    error = function(e) NULL
  )
  if (is.null(m_w) || !all(c("H_LR", "Phi") %in% names(coef(m_w)))) {
    return(tibble(window.start = w, g1 = NA_real_, g1.se = NA_real_,
                   g2 = NA_real_, g2.se = NA_real_, n = nrow(dat_w)))
  }
  tibble(
    window.start = w,
    g1 = unname(coef(m_w)["H_LR"]), g1.se = unname(se(m_w)["H_LR"]),
    g2 = unname(coef(m_w)["Phi"]),  g2.se = unname(se(m_w)["Phi"]),
    n = nrow(dat_w)
  )
}) %>% bind_rows()

cat("Per-window decomposition fits, windows with a computable estimate:",
    sum(is.finite(decomposition_path.rolling$g1)), "of", nrow(decomposition_path.rolling), "\n")

decomposition_path_long.rolling <- bind_rows(
  decomposition_path.rolling %>% transmute(window.start, coefficient = "g1 (H_LR)", estimate = g1, se = g1.se),
  decomposition_path.rolling %>% transmute(window.start, coefficient = "g2 (Phi)",  estimate = g2, se = g2.se)
) %>%
  filter(is.finite(estimate))

figure4b.rolling <- decomposition_path_long.rolling %>%
  ggplot(aes(x = window.start, y = estimate, color = coefficient, fill = coefficient)) +
  geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), alpha = 0.15, color = NA) +
  geom_line() +
  geom_point(size = 1.2) +
  labs(
    title = "Table 4 decomposition coefficients, by rolling window",
    subtitle = "Separate cross-sectional fit per window, multi-fishery vessel-windows only, 95% CI ribbon",
    x = "Window start year", y = "Coefficient (levels)", color = NULL, fill = NULL
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure4b_decomposition_path_rolling.png"),
       figure4b.rolling, width = 8, height = 5, dpi = 300)

cat("Wrote figure4b_decomposition_path_rolling.png\n")

# ============================================================================
# 7. Mandatory stride-6 phase check (design Section 2.2, Layer 3), on the
#    decomposed model's two coefficients, prime-FE and vessel-FE versions
# ============================================================================

pc_dec_hlr <- roll_phase_check(
  fml = rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
  data = vessel_multi.rolling, coef_name = "H_LR", label = "Table 4-rolling: decomposed (prime FE)"
)
pc_dec_phi <- roll_phase_check(
  fml = rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
  data = vessel_multi.rolling, coef_name = "Phi", label = "Table 4-rolling: decomposed (prime FE)"
)
pc_dec_vfe_hlr <- roll_phase_check(
  fml = rev.cv ~ H_LR + Phi | Vessel.ADFG.Number + window.start,
  data = vessel_multi.rolling, coef_name = "H_LR", label = "Table 4-rolling: decomposed (vessel FE)"
)
pc_dec_vfe_phi <- roll_phase_check(
  fml = rev.cv ~ H_LR + Phi | Vessel.ADFG.Number + window.start,
  data = vessel_multi.rolling, coef_name = "Phi", label = "Table 4-rolling: decomposed (vessel FE)"
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

new_rows <- bind_rows(pc_dec_hlr$summary, pc_dec_phi$summary, pc_dec_vfe_hlr$summary, pc_dec_vfe_phi$summary)
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
