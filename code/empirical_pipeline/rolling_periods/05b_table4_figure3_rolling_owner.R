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
  # Short headers on purpose, the last two used to be "Decomposed (owner FE)"
  # and "Decomposed (inv. window wt.)", noticeably longer than the other
  # four, which made etable size those two columns wider than the rest and
  # the printed table looked lopsided. The Model row ((1) through (6)), the
  # File.Number fixed-effect row, and the caption's own column-by-column
  # description already carry the full detail these headers used to spell
  # out, so shortening them costs nothing a reader needs.
  headers = c("Baseline", "Decomposed", "Baseline (z)", "Decomposed (z)",
              "Owner FE", "Weighted"),
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

# ----------------------------------------------------------------------
# Table 4-rolling (owner), residency-controlled companion. Rolling-window
# analogue of 05_table4_figure3_owner.R's own residency-controlled
# companion, whether the H_LR/Phi coefficients are robust to adding the
# owner's residency. residency is time-invariant per owner (lifetime-
# modal, joined in 01b_build_rolling_panel_owner.R Section 10), so it
# carries the same value into every window the same owner appears in,
# this is not tracking a within-panel residency change. A separate
# companion table, not a replacement, same reason as the lifetime
# version, an owner with no residency observation would otherwise be
# silently dropped from the main sample the moment residency entered the
# formula. Same reference-level note as the lifetime version, feols
# defaults to alphabetical ("N", nonresident).
# ----------------------------------------------------------------------

owner_multi_resid.rolling <- owner_multi.rolling %>% filter(!is.na(residency))

cat("Table 4-rolling (owner), residency-controlled sample -", nrow(owner_multi_resid.rolling),
    "of", nrow(owner_multi.rolling), "multi-fishery owner-windows (",
    round(100 * nrow(owner_multi_resid.rolling) / nrow(owner_multi.rolling), 1), "% )\n")
print(owner_multi_resid.rolling %>% count(residency, sort = TRUE))

m_baseline_resid_roll_owner   <- feols(rev.cv ~ H_bar + residency | prime.fishery.window + window.start,
                                        data = owner_multi_resid.rolling, cluster = ~File.Number + window.start)
m_decomposed_resid_roll_owner <- feols(rev.cv ~ H_LR + Phi + residency | prime.fishery.window + window.start,
                                        data = owner_multi_resid.rolling, cluster = ~File.Number + window.start)

owner_std_resid.rolling <- owner_multi_resid.rolling %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

m_baseline_std_resid_roll_owner   <- feols(z.rev.cv ~ z.H_bar + residency | prime.fishery.window + window.start,
                                            data = owner_std_resid.rolling, cluster = ~File.Number + window.start)
m_decomposed_std_resid_roll_owner <- feols(z.rev.cv ~ z.H_LR + z.Phi + residency | prime.fishery.window + window.start,
                                            data = owner_std_resid.rolling, cluster = ~File.Number + window.start)

etable(
  m_baseline_resid_roll_owner, m_decomposed_resid_roll_owner,
  m_baseline_std_resid_roll_owner, m_decomposed_std_resid_roll_owner,
  headers = c("Baseline (+residency)", "Decomposed (+residency)",
              "Baseline (z, +residency)", "Decomposed (z, +residency)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression_rolling_owner_residency.tex"),
  replace = TRUE
)

print(etable(
  m_baseline_resid_roll_owner, m_decomposed_resid_roll_owner,
  m_baseline_std_resid_roll_owner, m_decomposed_std_resid_roll_owner
))

cat("Wrote table4_decomposition_regression_rolling_owner_residency.tex\n")

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

# ============================================================================
# 7. Table 4b-rolling (owner). Ex-ante held-fishery count as an extensive-
#    margin regressor, alongside and jointly with H_LR/Phi
# ============================================================================
#
# n.held.avg/n.fished.avg/n.unfished.avg (01b_build_rolling_panel_owner.R
# Section 6b) are new, purely additive columns on owner_window_summary.rolling,
# nothing above this section reads them and nothing above is changed by
# adding them.
#
# Sample, owner_analysis.rolling (the POOLED sample, specialists included),
# not owner_multi.rolling. Single-fishery-window owners are exactly the
# clearest case of unconverted access (up to 6 held fisheries, 1 fished,
# Phi = 0, H_LR = 1), excluding them would drop the population this
# question is most about. This is a deliberate departure from Table
# 4-rolling's own main-text sample above, not an oversight.
#
# All models write to a NEW table file, table4b_extensive_margin_rolling_owner.tex,
# the existing table4_decomposition_regression_rolling_owner.tex and its
# companions above are untouched.

owner_extmargin.rolling <- owner_analysis.rolling %>%
  filter(!is.na(n.held.avg), !is.na(n.fished.avg), !is.na(n.unfished.avg))

cat("Table 4b-rolling (owner), sample -", nrow(owner_extmargin.rolling), "of",
    nrow(owner_analysis.rolling), "owner-windows in the pooled Table 4-rolling sample (",
    round(100 * nrow(owner_extmargin.rolling) / nrow(owner_analysis.rolling), 2),
    "% ), any row dropped here has a missing n.held.avg/n.fished.avg/n.unfished.avg,",
    "should not happen given how those columns are built, inspect 01b_'s Section 6b if this is not 100%\n")

# ----------------------------------------------------------------------
# 7a. Diagnostics, read BEFORE trusting anything fit below, this is a
#     once-only server run, not an interactive session.
# ----------------------------------------------------------------------

cat("\n--- Table 4b-rolling (owner), n.held.avg distribution ---\n")
held_avg_quantiles <- quantile(owner_extmargin.rolling$n.held.avg,
                                probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99))
cat("mean -", round(mean(owner_extmargin.rolling$n.held.avg), 4),
    ", sd -", round(sd(owner_extmargin.rolling$n.held.avg), 4), "\n")
print(round(held_avg_quantiles, 4))
cat("share at exactly 0 -", round(mean(owner_extmargin.rolling$n.held.avg == 0), 4),
    ", share at exactly 1 -", round(mean(owner_extmargin.rolling$n.held.avg == 1), 4), "\n")

cat("\n--- Table 4b-rolling (owner), n.held.avg correlation against H_bar/H_LR/Phi ---\n")
for (v in c("H_bar", "H_LR", "Phi")) {
  pear <- cor(owner_extmargin.rolling$n.held.avg, owner_extmargin.rolling[[v]], method = "pearson")
  spear <- cor(owner_extmargin.rolling$n.held.avg, owner_extmargin.rolling[[v]], method = "spearman")
  cat("n.held.avg vs", v, "- Pearson -", round(pear, 4), ", Spearman -", round(spear, 4), "\n")
}

cat("\n--- Table 4b-rolling (owner), n.held.avg vs the existing ex-post fished-count variable ---\n")
cat("n.held.avg vs n.fisheries.fished.window - Pearson -",
    round(cor(owner_extmargin.rolling$n.held.avg, owner_extmargin.rolling$n.fisheries.fished.window,
              method = "pearson"), 4),
    ", Spearman -",
    round(cor(owner_extmargin.rolling$n.held.avg, owner_extmargin.rolling$n.fisheries.fished.window,
              method = "spearman"), 4), "\n")

cat("\n--- Table 4b-rolling (owner), multicollinearity gut check ---\n")
m_collin_check <- feols(n.held.avg ~ H_LR + Phi | prime.fishery.window + window.start,
                         data = owner_extmargin.rolling)
cat("R-squared regressing n.held.avg on H_LR + Phi (+ FE) -", round(r2(m_collin_check, "r2"), 4),
    ", a value close to 1 would mean the joint model below cannot separate n.unfished.avg/n.fished.avg",
    "from H_LR/Phi cleanly\n")

# n.years.floored, 01b_build_rolling_panel_owner.R Section 6b, how many of
# a window's own eligible active years had the year-level unfished floor
# actually bind (a ticket-to-register linkage miss making that year's
# held count come in below its fished count). Reported here at the WINDOW
# grain (how much of the actual regression sample was touched), on top of
# 01b_'s own owner-YEAR-level rate, so both are visible together.
cat("\n--- Table 4b-rolling (owner), year-level unfished floor, how much of THIS sample it touched ---\n")
cat("Owner-windows with n.years.floored > 0 -",
    sum(owner_extmargin.rolling$n.years.floored > 0), "of", nrow(owner_extmargin.rolling), "(",
    round(100 * mean(owner_extmargin.rolling$n.years.floored > 0), 2), "% )\n")
print(table(n.years.floored = owner_extmargin.rolling$n.years.floored))

# ----------------------------------------------------------------------
# 7b. Regressions
# ----------------------------------------------------------------------

# Model 1, ex-ante held count alone.
m_extmargin_held_roll_owner <- feols(rev.cv ~ n.held.avg | prime.fishery.window + window.start,
                                      data = owner_extmargin.rolling, cluster = ~File.Number + window.start)

# Model 2, ex-post fished-fishery COUNT alone (n.fisheries.fished.window,
# already built by 01b_, a union count over the window, distinct from
# n.fished.avg's per-year average below), the alternative diversification
# metric some prior literature uses in place of HHI.
m_extmargin_fishedcount_roll_owner <- feols(rev.cv ~ n.fisheries.fished.window | prime.fishery.window + window.start,
                                             data = owner_extmargin.rolling, cluster = ~File.Number + window.start)

# Model 3, the joint/reparameterized model. n.unfished.avg is a year-level
# FLOORED version of held minus fished, averaged over the window (01b_'s
# Section 6b), not simply n.held.avg - n.fished.avg, so the three no
# longer sum exactly for a window touched by n.years.floored > 0 (see the
# diagnostic above), a deliberate tradeoff to stop a single bad
# ticket-to-register linkage year from dragging an otherwise-fine window's
# average below a logically impossible negative value. Running the fished
# and unfished components separately rather than n.held.avg itself makes the
# "does unconverted access matter net of realized diversification"
# question a direct coefficient (on n.unfished.avg) rather than something
# inferred from a model where n.held.avg and H_LR/Phi partially overlap.
# The prime.fishery.window fixed effect means this is a within-prime-
# fishery, within-window comparison across owners, not a claim about the
# effect of any one owner acquiring a permit, and a negative coefficient
# alone is not evidence of option value on its own, Chapter 2's own
# buy-and-hold model already predicts CV falls mechanically as the choice
# set grows, so that is the null this coefficient needs to be read against,
# not a finding by itself.
m_extmargin_joint_roll_owner <- feols(rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi |
                                         prime.fishery.window + window.start,
                                       data = owner_extmargin.rolling, cluster = ~File.Number + window.start)

owner_extmargin_std.rolling <- owner_extmargin.rolling %>%
  mutate(across(c(rev.cv, n.unfished.avg, n.fished.avg, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

m_extmargin_joint_std_roll_owner <- feols(
  z.rev.cv ~ z.n.unfished.avg + z.n.fished.avg + z.H_LR + z.Phi | prime.fishery.window + window.start,
  data = owner_extmargin_std.rolling, cluster = ~File.Number + window.start
)

# Owner fixed-effects version of the joint model, identified off
# within-owner variation across windows rather than the cross-owner
# comparison above. If n.held.avg is close to time-invariant within an
# owner (permit holdings are persistent), this column will be imprecise
# relative to the prime-FE version above, and the CONTRAST between the two
# is itself informative, a prime-FE estimate that does not survive an
# owner FE is a between-owner sorting correlation (who holds more
# permits), not a within-owner relationship (what happens when the SAME
# owner's held count changes).
m_extmargin_joint_ownerfe_roll_owner <- feols(rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi |
                                                 File.Number + window.start,
                                               data = owner_extmargin.rolling, cluster = ~File.Number + window.start)

# Curvature-robustness check, adding I(Phi^2) as a plain control, the same
# move Section 6's own Table 7 curvature check already makes on H_bar (see
# chapter3_writeup.tex Appendix, "Curvature robustness for the turnover
# interaction"). Motivation, n.held.avg's correlation with Phi is much
# stronger in rank (Spearman 0.67) than in levels (Pearson 0.29), a sign
# the two move together but not on a straight line, and Phi carries by far
# the largest coefficient in Model 3 above, so any real curvature in the
# Phi-risk relationship that a linear term cannot capture could be
# leaking into n.unfished.avg's coefficient instead. If n.unfished.avg
# survives close to its Model 3 value once I(Phi^2) is added, that is
# real evidence it is not just absorbing Phi's own curvature. If it
# shrinks toward zero here, Model 3's estimate was likely picking up
# curvature Model 3 itself had no way to represent. Prime-FE only, not
# also built for the owner-FE column, Model 5's owner-FE estimate is
# already only marginally significant on its own, adding a second
# nonlinear term to that thinner specification would not give a
# readable answer.
m_extmargin_joint_quad_roll_owner <- feols(rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi + I(Phi^2) |
                                              prime.fishery.window + window.start,
                                            data = owner_extmargin.rolling, cluster = ~File.Number + window.start)

etable(
  m_extmargin_held_roll_owner, m_extmargin_fishedcount_roll_owner,
  m_extmargin_joint_roll_owner, m_extmargin_joint_std_roll_owner, m_extmargin_joint_ownerfe_roll_owner,
  m_extmargin_joint_quad_roll_owner,
  headers = c("Held count", "Fished count", "Joint", "Joint (z)", "Joint (owner FE)", "Joint (Phi\\textsuperscript{2})"),
  tex = TRUE,
  file = file.path(table_dir, "table4b_extensive_margin_rolling_owner.tex"),
  replace = TRUE
)

print(etable(
  m_extmargin_held_roll_owner, m_extmargin_fishedcount_roll_owner,
  m_extmargin_joint_roll_owner, m_extmargin_joint_std_roll_owner, m_extmargin_joint_ownerfe_roll_owner,
  m_extmargin_joint_quad_roll_owner
))

cat("\n--- Table 4b-rolling (owner), curvature check, does n.unfished.avg survive I(Phi^2)? ---\n")
cat("n.unfished.avg, Model 3 (linear Phi) -", round(coef(m_extmargin_joint_roll_owner)["n.unfished.avg"], 4),
    ", Model 6 (+ I(Phi^2)) -", round(coef(m_extmargin_joint_quad_roll_owner)["n.unfished.avg"], 4), "\n")

cat("Wrote table4b_extensive_margin_rolling_owner.tex. Table 4b-rolling (owner) N -",
    nrow(owner_extmargin.rolling), ", distinct owners -", n_distinct(owner_extmargin.rolling$File.Number), "\n")

# ----------------------------------------------------------------------
# 7c. Mandatory stride-6 phase check on the joint model's four coefficients,
#     prime-FE and owner-FE versions, extending this pipeline's existing
#     inference protocol to the new headline models rather than skipping it.
#
#     Model labels below are deliberately distinct strings from Section 6's
#     own "Table 4-rolling (owner) - decomposed (...)" labels. The shared
#     ledger this appends to (rolling_overlap_robustness,
#     table_rolling_overlap_robustness.tex) dedups on paste(model,
#     coefficient), reusing an existing label here would silently
#     OVERWRITE those rows instead of adding new ones.
# ----------------------------------------------------------------------

pc_ext_unfished_prime <- roll_phase_check_owner(
  fml = rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi | prime.fishery.window + window.start,
  data = owner_extmargin.rolling, coef_name = "n.unfished.avg",
  label = "Table 4b-rolling (owner) - extensive margin joint (prime FE)"
)
pc_ext_fished_prime <- roll_phase_check_owner(
  fml = rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi | prime.fishery.window + window.start,
  data = owner_extmargin.rolling, coef_name = "n.fished.avg",
  label = "Table 4b-rolling (owner) - extensive margin joint (prime FE)"
)
pc_ext_unfished_ofe <- roll_phase_check_owner(
  fml = rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi | File.Number + window.start,
  data = owner_extmargin.rolling, coef_name = "n.unfished.avg",
  label = "Table 4b-rolling (owner) - extensive margin joint (owner FE)"
)
pc_ext_fished_ofe <- roll_phase_check_owner(
  fml = rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi | File.Number + window.start,
  data = owner_extmargin.rolling, coef_name = "n.fished.avg",
  label = "Table 4b-rolling (owner) - extensive margin joint (owner FE)"
)
pc_ext_unfished_quad <- roll_phase_check_owner(
  fml = rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi + I(Phi^2) | prime.fishery.window + window.start,
  data = owner_extmargin.rolling, coef_name = "n.unfished.avg",
  label = "Table 4b-rolling (owner) - extensive margin joint quad (prime FE)"
)
pc_ext_fished_quad <- roll_phase_check_owner(
  fml = rev.cv ~ n.unfished.avg + n.fished.avg + H_LR + Phi + I(Phi^2) | prime.fishery.window + window.start,
  data = owner_extmargin.rolling, coef_name = "n.fished.avg",
  label = "Table 4b-rolling (owner) - extensive margin joint quad (prime FE)"
)

load(ROLL_PHASE_CHECK_PATH)
new_rows_extmargin <- bind_rows(
  pc_ext_unfished_prime$summary, pc_ext_fished_prime$summary,
  pc_ext_unfished_ofe$summary, pc_ext_fished_ofe$summary,
  pc_ext_unfished_quad$summary, pc_ext_fished_quad$summary
)
rolling_overlap_robustness <- rolling_overlap_robustness %>%
  filter(!(paste(model, coefficient) %in% paste(new_rows_extmargin$model, new_rows_extmargin$coefficient))) %>%
  bind_rows(new_rows_extmargin)

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
cat("Wrote table_rolling_overlap_robustness.tex (", nrow(rolling_overlap_robustness),
    "headline model rows so far, including Table 4b-rolling owner extensive-margin rows)\n")

if (any(rolling_overlap_robustness$flag.outside.phase.range)) {
  cat("*** WARNING, the following headline models have a full-panel estimate outside their own",
      "phase min-max range, inspect before trusting them ***\n")
  print(rolling_overlap_robustness %>% filter(flag.outside.phase.range) %>% select(model, coefficient))
}
