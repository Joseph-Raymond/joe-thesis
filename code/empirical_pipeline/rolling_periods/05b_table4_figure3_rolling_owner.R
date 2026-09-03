# Chapter 3 empirical pipeline, owner-grain twin of
# 05b_table4_figure3_rolling.R
#
# File.Number here means the CFEC permit holder, NOT the vessel owner, see
# 05_table4_figure3_owner.R's own header for why that distinction matters.
# This script is the rolling-window analogue of 05_table4_figure3_owner.R
# the same way 05b_table4_figure3_rolling.R is the rolling-window analogue
# of 05_table4_figure3.R, at owner instead of vessel grain.
#
# Table 4-rolling (owner).       Full parity with the vessel-level Table
#                        4-rolling section, baseline versus decomposed
#                        CV-on-HHI regression at the owner-window grain,
#                        standardized versions, the owner-FE decomposed
#                        column (mirrors the vessel-FE "payoff column"),
#                        and the inverse-window-count-weighted robustness
#                        column.
# Table 4-pooled-rolling (owner). Same four/six models on the full pooled
#                        sample (specialists included), robustness.
# Figure 3-rolling (owner).      Passive buy-and-hold benchmark scatter,
#                        rolling, owner grain.
# Figure 3b-rolling (owner).     Gap-by-Phi, two-stage owner-clustered bin
#                        SEs.
#
# EXPLICIT EXCEPTION, NOT PORTED. figure4b_decomposition_path_rolling.png
# (05b_'s Section 6, g1/g2 re-estimated separately within each window and
# plotted as a coefficient path) is OUT OF SCOPE here, a separate, more
# elaborate deliverable in its own right, left as a named follow-up rather
# than silently built or silently omitted without a trace. If ever wanted,
# it is a direct port of that section with Vessel.ADFG.Number -> File.Number
# and vessel_multi.rolling -> owner_multi.rolling, nothing else changes.
# 05b_ also has no figure3_appendix_specialists_rolling.png of its own
# (checked directly, that appendix figure exists only in the LIFETIME
# 05_table4_figure3.R/05_table4_figure3_owner.R pair, not in the rolling
# twin), so no such file is built here either.
#
# INFRASTRUCTURE LANDMINE FOUND WHILE BUILDING THIS, flagged rather than
# silently patched or silently skipped. 00b_rolling_periods.R's
# roll_phase_check() is used for 05b_'s own mandatory stride-6 phase check
# (its Section 7), but that function hardcodes cluster = ~Vessel.ADFG.Number
# in TWO internal spots that ignore whatever cluster= the caller passes, the
# two-way-clustering fallback (not wrapped in tryCatch, would throw an
# UNCAUGHT error on owner data, since there is no Vessel.ADFG.Number column
# there) and every phase-level sub-fit (wrapped in tryCatch, so it would not
# crash, but would SILENTLY return NA for every single phase, always,
# regardless of real data sufficiency, confirmed by reading its source
# directly rather than assumed). Not fixed in 00b_rolling_periods.R itself,
# shared infrastructure every already-approved vessel-level rolling script
# depends on, out of scope to edit here. roll_phase_check_owner() below is a
# LOCAL copy with ONLY those two hardcoded references changed to
# ~File.Number, everything else (the two-way/fallback logic, phase
# computation via the shared roll_phase(), the retention diagnostics, the
# out-of-range warning, the returned $summary/$phases structure) is
# unchanged, so it plugs into the SAME shared rolling_overlap_robustness
# ledger exactly the way the vessel-level version does. See
# 00b_rolling_periods.R's own roll_phase_check() header comment for the
# full se.ratio-calibration reasoning, not re-derived here, it does not
# change at owner grain.
#
# Reads intermediate data/ch3_panel.rdata (read-only, fleet_mean_revenue_owner
# not actually needed here, passive_benchmark_window_owner.rolling already
# carries the window-local passive series) and intermediate
# data/ch3_rolling_owner.rdata (owner_window_summary.rolling,
# passive_benchmark_window_owner.rolling, window_grid.rolling), built by
# 01b_build_rolling_panel_owner.R. Neither 05b_table4_figure3_rolling.R nor
# 01b_build_rolling_panel_owner.R is edited by this script.
#
# Writes table4_decomposition_regression_rolling_owner.tex,
# table4_decomposition_regression_pooled_rolling_owner.tex,
# figure3_passive_benchmark_rolling_owner.png, and
# figure3b_gap_by_phi_rolling_owner.png, all to the SAME table_dir/figure_dir
# the vessel-level rolling outputs already sit in, and appends owner rows to
# the SAME shared table_rolling_overlap_robustness.tex ledger 05b_/08b_/09b_/
# 10b_ already write to, distinctly labeled ("(owner)" in the model string)
# so they never collide with the vessel-level rows already there.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

rolling_owner_panel_path <- file.path(intermediate_dir, "ch3_rolling_owner.rdata")
if (!exists("owner_window_summary.rolling") || !exists("passive_benchmark_window_owner.rolling") ||
    !exists("window_grid.rolling")) {
  load(rolling_owner_panel_path)
}

# ============================================================================
# 1. Sample construction
# ============================================================================
#
# owner_window_summary.rolling is already restricted to eligible windows
# (built inside 01b_build_rolling_panel_owner.R), so the only additional
# filter needed here mirrors 05b_'s own is.finite(rev.cv) restriction, never
# meets.min.years (trap #1, not applicable here anyway since this object was
# never built from owner_summary in the first place).

owner_analysis.rolling <- owner_window_summary.rolling %>%
  filter(is.finite(rev.cv))

cat("Owner-windows entering Table 4-rolling -", nrow(owner_analysis.rolling),
    ", distinct owners -", n_distinct(owner_analysis.rolling$File.Number),
    ", of which single-fishery specialists (window) -", sum(owner_analysis.rolling$is.specialist.window), "\n")

owner_multi.rolling <- owner_analysis.rolling %>% filter(!is.specialist.window)

# ============================================================================
# 2. Table 4-rolling (owner), main text (multi-fishery owner-windows)
# ============================================================================

m_baseline_roll_owner   <- feols(rev.cv ~ H_bar | prime.fishery.window + window.start,
                                  data = owner_multi.rolling, cluster = ~File.Number + window.start)
m_decomposed_roll_owner <- feols(rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
                                  data = owner_multi.rolling, cluster = ~File.Number + window.start)

owner_std.rolling <- owner_multi.rolling %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

m_baseline_std_roll_owner   <- feols(z.rev.cv ~ z.H_bar | prime.fishery.window + window.start,
                                      data = owner_std.rolling, cluster = ~File.Number + window.start)
m_decomposed_std_roll_owner <- feols(z.rev.cv ~ z.H_LR + z.Phi | prime.fishery.window + window.start,
                                      data = owner_std.rolling, cluster = ~File.Number + window.start)

# The owner-FE decomposed column, mirroring 05b_'s own "payoff column"
# (design Section 9.1) exactly, a genuine owner fixed effect in the
# decomposition, identified off within-owner variation across windows
# rather than the baseline's cross-owner-only comparison, this is precisely
# the margin (an owner reallocating across ITS OWN multiple vessels over
# time) this whole owner-level cut exists to surface.
m_decomposed_ownerfe_roll_owner <- feols(rev.cv ~ H_LR + Phi | File.Number + window.start,
                                          data = owner_multi.rolling, cluster = ~File.Number + window.start)

# Inverse-window-count-weighted robustness column, mirroring 05b_'s own
# construction exactly. Recomputed HERE, inside owner_multi.rolling, rather
# than reusing 01b_build_rolling_panel_owner.R's own inv.window.count
# column directly, for the identical reason 05b_'s own comment gives, that
# column is 1 / (ELIGIBLE windows), but this regression runs on
# owner_multi.rolling, which has already dropped within-window specialists
# and non-finite rev.cv, so an owner's inv.window.count values no longer sum
# to 1 over the rows actually being fit.
owner_multi.rolling <- owner_multi.rolling %>%
  add_count(File.Number, name = "n.windows.owner.insample") %>%
  mutate(inv.window.count.insample = 1 / n.windows.owner.insample)

cat("Eligible vs in-sample window count per owner (Table 4-rolling weighted column) -",
    "share of owner-windows where these differ -",
    round(mean(owner_multi.rolling$n.windows.owner.insample != owner_multi.rolling$n.windows.owner), 4), "\n")

m_decomposed_weighted_roll_owner <- feols(rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
                                           data = owner_multi.rolling, weights = ~inv.window.count.insample,
                                           cluster = ~File.Number + window.start)

etable(
  m_baseline_roll_owner, m_decomposed_roll_owner, m_baseline_std_roll_owner, m_decomposed_std_roll_owner,
  m_decomposed_ownerfe_roll_owner, m_decomposed_weighted_roll_owner,
  headers = c("Baseline", "Decomposed", "Baseline (z)", "Decomposed (z)",
              "Decomposed (owner FE)", "Decomposed (inv. window wt.)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression_rolling_owner.tex"),
  replace = TRUE
)

print(etable(
  m_baseline_roll_owner, m_decomposed_roll_owner, m_baseline_std_roll_owner, m_decomposed_std_roll_owner,
  m_decomposed_ownerfe_roll_owner, m_decomposed_weighted_roll_owner
))

g2_share_roll_owner <- coef(m_decomposed_std_roll_owner)["z.Phi"] /
  (coef(m_decomposed_std_roll_owner)["z.H_LR"] + coef(m_decomposed_std_roll_owner)["z.Phi"])
cat("Rolling standardized share of the decomposed slope loading onto Phi, owner (g2_share) -",
    round(g2_share_roll_owner, 3), "\n")

cat("Wrote table4_decomposition_regression_rolling_owner.tex. Table 4-rolling owner (multi-fishery) N -",
    nrow(owner_multi.rolling), ", distinct owners -", n_distinct(owner_multi.rolling$File.Number), "\n")

# ============================================================================
# 3. Table 4-pooled-rolling (owner), robustness, specialists included
# ============================================================================

m_baseline_pooled_roll_owner   <- feols(rev.cv ~ H_bar | prime.fishery.window + window.start,
                                         data = owner_analysis.rolling, cluster = ~File.Number + window.start)
m_decomposed_pooled_roll_owner <- feols(rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
                                         data = owner_analysis.rolling, cluster = ~File.Number + window.start)

owner_std_pooled.rolling <- owner_analysis.rolling %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

m_baseline_std_pooled_roll_owner <- feols(
  z.rev.cv ~ z.H_bar | prime.fishery.window + window.start,
  data = owner_std_pooled.rolling, cluster = ~File.Number + window.start
)
m_decomposed_std_pooled_roll_owner <- feols(
  z.rev.cv ~ z.H_LR + z.Phi | prime.fishery.window + window.start,
  data = owner_std_pooled.rolling, cluster = ~File.Number + window.start
)

etable(
  m_baseline_pooled_roll_owner, m_decomposed_pooled_roll_owner,
  m_baseline_std_pooled_roll_owner, m_decomposed_std_pooled_roll_owner,
  headers = c("Baseline (pooled)", "Decomposed (pooled)",
              "Baseline (pooled, z)", "Decomposed (pooled, z)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression_pooled_rolling_owner.tex"),
  replace = TRUE
)

cat("Wrote table4_decomposition_regression_pooled_rolling_owner.tex. N -", nrow(owner_analysis.rolling),
    ", distinct owners -", n_distinct(owner_analysis.rolling$File.Number), "\n")

# ============================================================================
# 4. Figure 3-rolling (owner). Passive buy-and-hold benchmark scatter
# ============================================================================

fig3_data_owner.rolling <- owner_analysis.rolling %>%
  select(File.Number, window.start, rev.cv, H_bar, H_LR, Phi, is.specialist.window) %>%
  inner_join(passive_benchmark_window_owner.rolling, by = c("File.Number", "window.start")) %>%
  filter(is.finite(passive.cv))

cat("Owner-windows with a computable passive benchmark -", nrow(fig3_data_owner.rolling),
    ", of which single-fishery specialists (window) -", sum(fig3_data_owner.rolling$is.specialist.window), "\n")

figure3_owner.rolling <- fig3_data_owner.rolling %>%
  filter(!is.specialist.window) %>%
  ggplot(aes(x = passive.cv, y = rev.cv)) +
  geom_point(alpha = 0.08, size = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "firebrick") +
  labs(
    title = "Realized revenue CV versus a passive buy-and-hold benchmark, owner (rolling)",
    subtitle = "Multi-fishery owner-windows, window-local weights, one point per eligible (owner, window)",
    x = "Passive benchmark CV (buy-and-hold, window-local weights)",
    y = "Realized revenue CV"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3_passive_benchmark_rolling_owner.png"),
       figure3_owner.rolling, width = 7, height = 6, dpi = 300)

cat("Wrote figure3_passive_benchmark_rolling_owner.png\n")

# ============================================================================
# 5. Figure 3b-rolling (owner). Gap between realized and passive CV, binned
#    by Phi
# ============================================================================
#
# gap_iw = rev.cv_iw - passive.cv_iw. Trap #9 applies identically at owner
# grain, the bin standard error must NOT be sd(gap) / sqrt(n_obs), a bin can
# contain several overlapping-window observations from the SAME owner,
# which would badly understate it. two_stage_bin_summary() collapses to one
# value per owner WITHIN the bin first (mean gap across that owner's own
# windows landing in this bin), then takes sd / sqrt(n_distinct_owners) over
# those owner means, treating the owner (not the owner-window) as the
# independent sampling unit.

fig3b_data_owner.rolling <- fig3_data_owner.rolling %>%
  mutate(gap = rev.cv - passive.cv)

two_stage_bin_summary_owner <- function(df) {
  owner_means <- df %>%
    group_by(File.Number) %>%
    summarise(owner.gap = mean(gap), .groups = "drop")
  tibble(
    n         = nrow(df),
    n.owners  = nrow(owner_means),
    mean.Phi  = mean(df$Phi),
    mean.gap  = mean(owner_means$owner.gap),
    se.gap    = sd(owner_means$owner.gap) / sqrt(nrow(owner_means))
  )
}

specialist_summary_owner.rolling <- fig3b_data_owner.rolling %>%
  filter(is.specialist.window) %>%
  two_stage_bin_summary_owner() %>%
  mutate(bin.label = "Specialists\n(Phi = 0)", bin.order = 0)

# N_GAP_BINS_ROLLING_OWNER, not N_GAP_BINS_ROLLING (05b_'s own constant) and
# not N_GAP_BINS (the design's do-not-reassign baseline name), a distinctly
# named local constant so this never collides if this script is ever
# sourced alongside 05b_table4_figure3_rolling.R or
# 05_table4_figure3.R in the same interactive session.
N_GAP_BINS_ROLLING_OWNER <- 8

multi_summary_owner.rolling <- fig3b_data_owner.rolling %>%
  filter(!is.specialist.window) %>%
  mutate(phi.bin = ntile(Phi, N_GAP_BINS_ROLLING_OWNER)) %>%
  group_by(phi.bin) %>%
  group_modify(~ two_stage_bin_summary_owner(.x)) %>%
  ungroup() %>%
  mutate(bin.label = paste0("Q", phi.bin), bin.order = phi.bin) %>%
  select(-phi.bin)

gap_by_phi_owner.rolling <- bind_rows(specialist_summary_owner.rolling, multi_summary_owner.rolling) %>%
  mutate(bin.label = fct_reorder(bin.label, bin.order), is.specialist.bin = bin.order == 0)

print(gap_by_phi_owner.rolling)

figure3b_owner.rolling <- gap_by_phi_owner.rolling %>%
  ggplot(aes(x = bin.label, y = mean.gap, color = is.specialist.bin)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean.gap - 1.96 * se.gap, ymax = mean.gap + 1.96 * se.gap), width = 0.2) +
  geom_line(
    data = gap_by_phi_owner.rolling %>% filter(!is.specialist.bin),
    aes(x = bin.label, y = mean.gap, group = 1), color = "steelblue", inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "gray40", "FALSE" = "steelblue"), guide = "none") +
  labs(
    title = "Gap between realized and passive-benchmark CV, owner (rolling)",
    subtitle = "By reallocation intensity (Phi), window-local, two-stage owner-clustered bin SEs",
    x = "Reallocation intensity (Phi), specialists then increasing bins",
    y = "Mean gap (realized CV - passive CV)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3b_gap_by_phi_rolling_owner.png"),
       figure3b_owner.rolling, width = 7, height = 5, dpi = 300)

cat("Wrote figure3b_gap_by_phi_rolling_owner.png\n")

# ============================================================================
# 6. Mandatory stride-6 phase check (design Section 2.2, Layer 3), on the
#    decomposed model's two coefficients, prime-FE and owner-FE versions
# ============================================================================
#
# roll_phase_check_owner(), a LOCAL copy of 00b_rolling_periods.R's own
# roll_phase_check(), see this script's header for exactly why a local copy
# was needed rather than calling the shared version directly (its two
# internal cluster = ~Vessel.ADFG.Number references are hardcoded, not
# parameterized by the cluster= argument, and would either error or
# silently return all-NA phases on owner data). Everything else is an exact
# copy, unchanged.

roll_phase_check_owner <- function(fml, data, coef_name, label,
                                    cluster = ~File.Number + window.start,
                                    min_year = MIN_YEAR, n_phases = ROLL_N_PHASES,
                                    ...) {

  used_twoway <- TRUE
  m_full <- tryCatch(
    feols(fml, data = data, cluster = cluster, ...),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  needs_fallback <- is.null(m_full) ||
    !(coef_name %in% names(coef(m_full))) ||
    !is.finite(se(m_full)[coef_name])

  if (needs_fallback) {
    used_twoway <- FALSE
    m_full <- feols(fml, data = data, cluster = ~File.Number, ...)
  }

  est_full   <- unname(coef(m_full)[coef_name])
  se_full    <- unname(se(m_full)[coef_name])
  n_obs_full <- nrow(data)
  n_fit_full <- nobs(m_full)

  phase_list <- lapply(0:(n_phases - 1), function(p) {
    data_p <- data %>% filter(roll_phase(window.start, min_year, n_phases) == p)
    m_p <- tryCatch(
      feols(fml, data = data_p, cluster = ~File.Number, ...),
      error = function(e) NULL
    )
    if (is.null(m_p) || !(coef_name %in% names(coef(m_p)))) {
      return(tibble(phase = p, n.obs = nrow(data_p), n.fit = NA_integer_, estimate = NA_real_, se = NA_real_))
    }
    tibble(phase = p, n.obs = nrow(data_p), n.fit = nobs(m_p),
           estimate = unname(coef(m_p)[coef_name]), se = unname(se(m_p)[coef_name]))
  })
  phase_tbl <- bind_rows(phase_list)

  se_phase_median   <- median(phase_tbl$se, na.rm = TRUE)
  se_ratio          <- se_full / se_phase_median
  phase_min         <- min(phase_tbl$estimate, na.rm = TRUE)
  phase_median      <- median(phase_tbl$estimate, na.rm = TRUE)
  phase_max         <- max(phase_tbl$estimate, na.rm = TRUE)
  n_fit_phase_median <- median(phase_tbl$n.fit, na.rm = TRUE)
  # Retention = n.fit / n.obs, how much of the RAW candidate sample actually
  # entered the regression after FE-based (mostly File.Number) singleton
  # dropping, see 00b_rolling_periods.R's own roll_phase_check() header for
  # the full reasoning, unchanged at owner grain.
  retention_full         <- n_fit_full / n_obs_full
  retention_phase_median <- n_fit_phase_median / median(phase_tbl$n.obs, na.rm = TRUE)
  out_of_range    <- is.finite(est_full) && is.finite(phase_min) && is.finite(phase_max) &&
    (est_full < phase_min || est_full > phase_max)

  cat("\n--- roll_phase_check_owner", label, "( coefficient", coef_name, ") ---\n")
  cat("  full-panel estimate -", round(est_full, 4),
      ", SE (", if (used_twoway) "two-way owner+window" else "owner-only, two-way clustering failed/degenerate",
      ") -", round(se_full, 4), "\n")
  cat("  full-panel N, raw -", n_obs_full, ", fit (post FE-dropping) -", n_fit_full,
      ", retention -", round(retention_full, 3), "\n")
  cat("  phase estimates, min -", round(phase_min, 4), ", median -", round(phase_median, 4),
      ", max -", round(phase_max, 4), "\n")
  cat("  median phase N, fit (post FE-dropping) -", round(n_fit_phase_median),
      ", retention -", round(retention_phase_median, 3),
      if (is.finite(retention_phase_median) && is.finite(retention_full) &&
          retention_phase_median < 0.7 * retention_full)
        "  *** phase retention notably below full-panel retention, se.ratio below is likely biased low ***"
      else "", "\n")
  cat("  median phase SE -", round(se_phase_median, 4),
      ", SE_full / SE_phase =", round(se_ratio, 3),
      " (rough anchor ~0.7 for a healthy model, no single fixed benchmark applies)\n")
  if (out_of_range) {
    cat("  *** WARNING", label, "(", coef_name, ") full-panel point estimate",
        round(est_full, 4), "falls OUTSIDE the phase min-max range [", round(phase_min, 4), ",",
        round(phase_max, 4), "]. This is a signal something may be wrong with the rolling",
        "construction for this model, inspect before trusting it. ***\n")
  }

  list(
    summary = tibble(
      model = label, coefficient = coef_name,
      estimate.full = est_full, se.full = se_full, used.twoway.cluster = used_twoway,
      phase.min = phase_min, phase.median = phase_median, phase.max = phase_max,
      se.phase.median = se_phase_median, se.ratio = se_ratio,
      n.obs.full = n_obs_full, n.fit.full = n_fit_full, retention.full = retention_full,
      n.fit.phase.median = n_fit_phase_median, retention.phase.median = retention_phase_median,
      flag.outside.phase.range = out_of_range
    ),
    phases = phase_tbl
  )
}

# Label strings below deliberately use a hyphen as the separator, not the
# punctuation mark 05b_'s own vessel-level rows already use in this same
# shared ledger (its "decomposed (prime FE)" row labels), a narrow,
# deliberate exception noted here rather than silently made consistent
# either way, these are literal string VALUES that become content in a
# persisted, shared .tex artifact whose existing vessel-level rows this
# script does not rewrite, not a code comment or a console message, so the
# no-colon convention is applied to the NEW rows this script adds without
# touching the OLD rows already there.
pc_dec_hlr_owner <- roll_phase_check_owner(
  fml = rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
  data = owner_multi.rolling, coef_name = "H_LR", label = "Table 4-rolling (owner) - decomposed (prime FE)"
)
pc_dec_phi_owner <- roll_phase_check_owner(
  fml = rev.cv ~ H_LR + Phi | prime.fishery.window + window.start,
  data = owner_multi.rolling, coef_name = "Phi", label = "Table 4-rolling (owner) - decomposed (prime FE)"
)
pc_dec_ofe_hlr_owner <- roll_phase_check_owner(
  fml = rev.cv ~ H_LR + Phi | File.Number + window.start,
  data = owner_multi.rolling, coef_name = "H_LR", label = "Table 4-rolling (owner) - decomposed (owner FE)"
)
pc_dec_ofe_phi_owner <- roll_phase_check_owner(
  fml = rev.cv ~ H_LR + Phi | File.Number + window.start,
  data = owner_multi.rolling, coef_name = "Phi", label = "Table 4-rolling (owner) - decomposed (owner FE)"
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

new_rows_owner <- bind_rows(
  pc_dec_hlr_owner$summary, pc_dec_phi_owner$summary,
  pc_dec_ofe_hlr_owner$summary, pc_dec_ofe_phi_owner$summary
)
rolling_overlap_robustness <- rolling_overlap_robustness %>%
  filter(!(paste(model, coefficient) %in% paste(new_rows_owner$model, new_rows_owner$coefficient))) %>%
  bind_rows(new_rows_owner)

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
  cat("*** WARNING, the following headline models have a full-panel estimate outside their own",
      "phase min-max range, inspect before trusting them ***\n")
  print(rolling_overlap_robustness %>% filter(flag.outside.phase.range) %>% select(model, coefficient))
}
