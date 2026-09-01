# Chapter 3 empirical pipeline, rolling-window figures/tables for the
# predicted buy-and-hold Phi benchmark (12c_predicted_bh_phi_rolling.R)
#
# Figure 3c-rolling. Support-matched realized Phi (Phi.matched) versus
#                    predicted (buy-and-hold) Phi^BH scatter, with the
#                    45-degree line, the direct empirical analog of Chapter
#                    2's Phi^regime vs Phi^BH comparison (sim_core.py's
#                    Hbar = H_LR + Phi decomposition, see 12c_'s own header
#                    note). PRIMARY comparator, per a methodological review,
#                    see Section 1's own comment on why Phi.matched (not raw
#                    Phi) is plotted here. Points ABOVE the 45-degree line
#                    are vessel-windows whose realized instability, WITHIN
#                    the fixed J*/surviving-year support, exceeds what a
#                    purely passive, fixed-effort operator would have
#                    generated from exogenous pass-through alone.
# Table (behavioral share). Mean/median (and levels, not just the ratio) of
#                    the PRIMARY behavioral share
#                    (Phi.matched - Phi.BH) / Phi.matched, overall and by
#                    quartile of the tau turnover classifier (06b_'s
#                    tau_by_vessel_window.rolling, intermediate data/ch3_rolling_tau.rdata),
#                    an EXTERNAL validation check, tau is built entirely
#                    independently of Phi/Phi.BH/Phi.matched, from
#                    weekly-frequency within-season target-switching data
#                    (06_/06b_'s own machinery). If the behavioral share
#                    rises across tau quartiles, a vessel-window's OWN
#                    independently-measured turnover tendency tracks how
#                    much of its (support-matched) Phi is left unexplained
#                    by a fixed-effort counterfactual, corroborating
#                    evidence for the reading that Phi.gap.matched really is
#                    capturing behavioral reallocation rather than noise.
#
# PRIMARY VS SECONDARY COMPARATOR, per a methodological review. 12c_ now
# carries TWO realized-Phi constructions, raw Phi (vessel_window_summary.rolling's
# own Phi, built over the vessel's FULL realized portfolio across ALL
# n.years.window active years) and Phi.matched (a REALIZED analog of Phi.BH
# computed on the IDENTICAL support Phi.BH itself uses, J*_{i,s} fisheries,
# surviving years, AND, within a surviving year, the exact (fishery, year)
# cells with a defined prediction that year, see 12c_'s own Section 7,
# matched at the cell level after a second round of review found the
# year-only version of this match still left a gap). The review
# demonstrated concretely that raw Phi and Phi.BH not sharing a support can
# manufacture a spurious Phi - Phi.BH gap of EITHER SIGN even for a vessel
# with ZERO true reallocation, purely from portfolio-breadth/year-coverage/
# within-year-fishery-coverage differences between the two constructions,
# not from anything behavioral (one reproduction gave a spurious POSITIVE
# gap, a later one a spurious NEGATIVE gap, the sign is example-specific).
# Phi.matched nets that out. This script therefore treats Phi.matched (and
# Phi.gap.matched = Phi.matched - Phi.BH) as PRIMARY throughout, raw Phi and
# Phi.gap are kept as a SECONDARY diagnostic (Section 1's own sample
# summary and Section 3's own side-by-side print), a reader who wants the
# un-matched picture can still see it, just not as the headline.
#
# BLIND SPOT, per a methodological review. Phi.matched is computed ENTIRELY
# INSIDE J*_{i,s} (same as Phi.BH), so realized reallocation TOWARD a
# fishery outside J*_{i,s} entirely is invisible to Phi.matched just as it
# is to Phi.BH, agreement between the two is evidence the vessel did not
# reallocate WITHIN J*_{i,s}, not proof it never reallocated at all.
# coverage.BH (already in predicted_bh_phi_vessel_window.rolling, see 12c_'s
# own Section 6) quantifies how much of the vessel-window's realized revenue
# sits inside J*_{i,s} in the first place, a low coverage.BH is the signal
# that this blind spot could be masking real reallocation, this script does
# not currently plot coverage.BH itself but a reader should consult it
# alongside Phi.matched before concluding "no reallocation" from a small gap.
#
# NEITHER 12c_predicted_bh_phi_rolling.R NOR 06b_within_season_reallocation_rolling.R
# IS EDITED BY THIS SCRIPT. Structure (source() order, ggsave/figure_dir
# conventions, xtable/table_dir conventions) follows
# 13b_predicted_bh_revenue_figures_rolling.R closely, see that script's own
# header for the fuller set of conventions this one inherits.
#
# NOTHING HERE IS RUN AGAINST REAL DATA. This cannot be executed locally
# (00_setup.R's own header note, confidential CFEC/AKFIN data lives only on
# the remote server), and no numeric result from this script should be
# treated as reported until it has actually been run there.
#
# Reads intermediate data/ch3_predicted_bh_phi.rdata (predicted_bh_phi_vessel_window.rolling,
# built by 12c_predicted_bh_phi_rolling.R) and intermediate data/ch3_rolling_tau.rdata
# (tau_by_vessel_window.rolling, built by 06b_within_season_reallocation_rolling.R,
# which run_all_rolling.R runs well before this script), both read-only.
#
# Saves figure3c_phi_vs_phi_bh_rolling.png to figure_dir and
# table_behavioral_share_by_tau_quartile_rolling.tex to table_dir. Prints the
# overall and by-quartile behavioral-share summary to console as well, so it
# is visible without opening the .tex file.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

predicted_bh_phi_path <- file.path(intermediate_dir, "ch3_predicted_bh_phi.rdata")
if (!exists("predicted_bh_phi_vessel_window.rolling")) load(predicted_bh_phi_path)

rolling_tau_path <- file.path(intermediate_dir, "ch3_rolling_tau.rdata")
if (!exists("tau_by_vessel_window.rolling")) load(rolling_tau_path)

# ============================================================================
# Local constants
# ============================================================================

# NEW, per a methodological review (item 4). (Phi.gap.matched / Phi.matched)
# is an unbounded ratio whenever Phi.matched sits near zero, exactly the
# common case for a low-reallocation multi-fishery vessel-window, a handful
# of tiny-denominator rows can otherwise dominate a mean/median that is
# supposed to summarize the WHOLE sample. The ratio-based table (Section 3)
# is therefore restricted to Phi.matched > PHI_FLOOR_FOR_BEHAVIORAL_SHARE,
# a small but non-trivial floor in the same documented-but-arbitrary spirit
# as 12c_'s own BH_MIN_* constants, NOT applied to the LEVELS-based summary
# (mean Phi.gap.matched itself, unbounded-ratio-free by construction), which
# is reported for the FULL (Phi.matched > 0) sample alongside it.
PHI_FLOOR_FOR_BEHAVIORAL_SHARE <- 0.01

# ============================================================================
# 1. Sample construction
# ============================================================================
#
# Restricted to non-specialist (multi-fishery) vessel-windows with a defined
# Phi.BH and Phi.matched, matching 05b_/13b_'s own established exclusion of
# specialists from every reallocation-intensity diagnostic. NEW, per a
# methodological review (item 3), ALSO restricted to n.fisheries.J.star >= 2.
# is.specialist.window is about the REALIZED portfolio, it does NOT rule out
# a vessel-window whose REALIZED portfolio has 3 fisheries but whose J*
# (the fixed-effort counterfactual portfolio) only has 1 surviving fishery,
# a case 12c_'s own synthetic test confirms forces Phi.BH == 0 MECHANICALLY
# (a single-fishery share is trivially 1.0 every year, zero variance by
# construction), not a finding about reallocation. Filtering on
# n.fisheries.J.star (the PREDICTED-side portfolio breadth this script's
# whole comparison is actually built on) is the correct guard, is.specialist.window
# alone is not.

fig3c_data.rolling <- predicted_bh_phi_vessel_window.rolling %>%
  filter(!is.specialist.window, is.finite(Phi.BH), is.finite(Phi.matched))

n_before_jstar_filter <- nrow(fig3c_data.rolling)
fig3c_data.rolling <- fig3c_data.rolling %>% filter(n.fisheries.J.star >= 2)

cat("Vessel-windows entering Figure 3c-rolling (multi-fishery REALIZED, defined Phi.BH and Phi.matched) -",
    n_before_jstar_filter, ", of which excluded for a single-fishery J* (n.fisheries.J.star == 1,",
    "Phi.BH mechanically 0) -", n_before_jstar_filter - nrow(fig3c_data.rolling),
    ", final sample -", nrow(fig3c_data.rolling), ", of", nrow(predicted_bh_phi_vessel_window.rolling),
    "total rows in predicted_bh_phi_vessel_window.rolling, distinct vessels -",
    n_distinct(fig3c_data.rolling$Vessel.ADFG.Number), "\n")

# SECONDARY diagnostic, per a methodological review, raw Phi's own coverage
# over this same sample, printed for comparison only, fig3c_data.rolling's
# own N is never filtered on is.finite(Phi).
cat("For comparison (SECONDARY, raw/un-matched), of this sample, share with a defined raw Phi too -",
    round(mean(is.finite(fig3c_data.rolling$Phi)), 4), "\n")

# ============================================================================
# 2. Figure 3c-rolling, Phi.matched vs Phi.BH scatter with the 45-degree line
# ============================================================================
#
# Linear (not log) axes, unlike 13b_'s dollar-level Figure A, Phi.matched
# and Phi.BH are both bounded HHI-difference quantities on [0, ~1) rather
# than dollar levels spanning orders of magnitude, so a log scale would buy
# nothing and would break at any Phi.BH == 0. alpha = 0.08 matches 05b_'s
# own Figure 3 density convention (a comparably dense full-sample scatter).
figure3c_phi_vs_phi_bh.rolling <- fig3c_data.rolling %>%
  ggplot(aes(x = Phi.BH, y = Phi.matched)) +
  geom_point(alpha = 0.08, size = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "firebrick") +
  labs(
    title = "Support-matched realized Phi versus predicted buy-and-hold Phi^BH (rolling)",
    subtitle = paste0(
      "Multi-fishery vessel-windows (n.fisheries.J.star >= 2), realized Phi renormalized over J* within the ",
      "surviving years (PRIMARY comparator, see this script's header note), points above the 45-degree line ",
      "have more realized instability, WITHIN that shared support, than a fixed-effort counterfactual predicts"
    ),
    x = "Predicted (buy-and-hold, fixed effort) Phi^BH",
    y = "Support-matched realized Phi (Phi.matched)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3c_phi_vs_phi_bh_rolling.png"),
       figure3c_phi_vs_phi_bh.rolling, width = 7, height = 6, dpi = 300)

cat("Wrote figure3c_phi_vs_phi_bh_rolling.png\n")

cat("Share of Figure 3c-rolling vessel-windows with Phi.matched > Phi.BH (reallocation adds instability,",
    "within the shared support) -", round(mean(fig3c_data.rolling$Phi.matched > fig3c_data.rolling$Phi.BH), 4), "\n")

# ============================================================================
# 3. Behavioral share, PRIMARY (Phi.matched - Phi.BH) / Phi.matched, overall
#    and by tau quartile, plus levels and the SECONDARY raw-Phi comparison
# ============================================================================
#
# tau.window (06b_'s own within-season turnover measure, built at WEEKLY
# frequency from target-switching, see that script's header) is joined in
# purely as an external cross-check, never used to filter or reweight
# fig3c_data.rolling itself, this table's own N is untouched by whether a
# given vessel-window happens to have a defined tau.window.
behavioral_share_data.rolling <- fig3c_data.rolling %>%
  filter(Phi.matched > 0) %>%
  mutate(
    behavioral.gap.matched.level = Phi.gap.matched,
    behavioral.share             = Phi.gap.matched / Phi.matched,
    # SECONDARY, raw/un-matched, computed on the SAME row set so the two are
    # directly comparable, NA wherever raw Phi itself is undefined (this
    # sample was never filtered on is.finite(Phi), see Section 1).
    behavioral.gap.raw.level      = Phi - Phi.BH,
    behavioral.share.raw          = if_else(Phi > 0, (Phi - Phi.BH) / Phi, NA_real_)
  ) %>%
  left_join(
    tau_by_vessel_window.rolling %>% filter(!is.na(tau.window)) %>%
      select(Vessel.ADFG.Number, window.start, tau.window),
    by = c("Vessel.ADFG.Number", "window.start")
  )

cat("Vessel-windows entering the behavioral-share summary (Phi.matched > 0) -",
    nrow(behavioral_share_data.rolling), ", of which with a defined tau.window (external check) -",
    sum(!is.na(behavioral_share_data.rolling$tau.window)), "\n")

# NEW, per a methodological review (item 4), min/max printed so a blow-up in
# the unbounded ratio is visible directly, BEFORE any flooring is applied.
cat("Behavioral share (PRIMARY, unfloored, Phi.matched > 0 only), min -",
    round(min(behavioral_share_data.rolling$behavioral.share, na.rm = TRUE), 4), ", max -",
    round(max(behavioral_share_data.rolling$behavioral.share, na.rm = TRUE), 4), "\n")

# LEVELS summary (Phi.gap.matched itself, no floor needed, an unbounded-
# ratio-free quantity by construction), reported for the FULL
# (Phi.matched > 0) sample, per a methodological review. CAVEAT, this still
# excludes exact Phi.matched == 0 vessel-windows (genuine zero-instability
# ones, not just a division guard, that restriction lives one step
# upstream in behavioral_share_data.rolling itself, shared with the ratio
# table below), which biases mean.gap.level slightly upward relative to the
# true full-sample mean, a small, one-directional, and easily fixed-later
# caveat, flagged rather than silently left for a reader to discover.
levels_summary.rolling <- behavioral_share_data.rolling %>%
  summarise(
    n              = n(),
    mean.gap.level = mean(behavioral.gap.matched.level),
    median.gap.level = median(behavioral.gap.matched.level)
  )
cat("\n===== Behavioral gap in LEVELS (PRIMARY, Phi.gap.matched, full Phi.matched > 0 sample, no floor,",
    "excludes exact Phi.matched == 0 rows, see this section's own caveat comment) =====\n")
print(levels_summary.rolling)

# Ratio-based table, restricted to the PHI_FLOOR_FOR_BEHAVIORAL_SHARE floor
# (Section header note above), this floor is what the rest of this section
# (overall_summary.rolling, tau_quartile_summary.rolling) is built on.
behavioral_share_floored.rolling <- behavioral_share_data.rolling %>%
  filter(Phi.matched > PHI_FLOOR_FOR_BEHAVIORAL_SHARE)

cat("Of the", nrow(behavioral_share_data.rolling), "vessel-windows above, ",
    nrow(behavioral_share_floored.rolling), "clear the Phi.matched >", PHI_FLOOR_FOR_BEHAVIORAL_SHARE,
    "floor used for the ratio-based table below\n")

overall_summary.rolling <- behavioral_share_floored.rolling %>%
  summarise(
    group          = "Overall",
    n              = n(),
    n.vessels      = n_distinct(Vessel.ADFG.Number),
    mean.share     = mean(behavioral.share),
    median.share   = median(behavioral.share),
    mean.share.raw = mean(behavioral.share.raw, na.rm = TRUE)
  )

# N_TAU_QUARTILES_ROLLING, a distinctly-named local constant (matching
# 05b_'s/13b_'s own reasoning for why their own bin-count constants are not
# just reused verbatim across scripts), quartiles computed WITHIN the subset
# that has a defined tau.window, so a vessel-window without a computable
# tau.window (thin activity at weekly grain, see 06_'s own comment on why
# this can happen even when the annual 4-of-6 floor is cleared) is excluded
# from the by-quartile rows but still counted in "Overall" above.
N_TAU_QUARTILES_ROLLING <- 4

tau_quartile_summary.rolling <- behavioral_share_floored.rolling %>%
  filter(!is.na(tau.window)) %>%
  mutate(tau.quartile = ntile(tau.window, N_TAU_QUARTILES_ROLLING)) %>%
  group_by(tau.quartile) %>%
  summarise(
    n              = n(),
    n.vessels      = n_distinct(Vessel.ADFG.Number),
    mean.tau       = mean(tau.window),
    mean.share     = mean(behavioral.share),
    median.share   = median(behavioral.share),
    mean.share.raw = mean(behavioral.share.raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = paste0("tau Q", tau.quartile)) %>%
  select(group, n, n.vessels, mean.tau, mean.share, median.share, mean.share.raw)

behavioral_share_table.rolling <- bind_rows(
  overall_summary.rolling %>% mutate(mean.tau = NA_real_) %>%
    select(group, n, n.vessels, mean.tau, mean.share, median.share, mean.share.raw),
  tau_quartile_summary.rolling
)

cat("\n===== Behavioral share, PRIMARY (Phi.matched - Phi.BH) / Phi.matched, floored at Phi.matched >",
    PHI_FLOOR_FOR_BEHAVIORAL_SHARE, ", overall and by tau quartile, mean.share.raw = SECONDARY",
    "(un-matched) ratio on the SAME rows for comparison =====\n")
print(behavioral_share_table.rolling)

cat("Behavioral share (PRIMARY, FLOORED sample), min -",
    round(min(behavioral_share_floored.rolling$behavioral.share, na.rm = TRUE), 4), ", max -",
    round(max(behavioral_share_floored.rolling$behavioral.share, na.rm = TRUE), 4),
    "(compare to the unfloored min/max printed above, flooring should visibly shrink this range)\n")

cat(
  "\nReading rule, if mean.share/median.share rise monotonically from tau Q1 to tau Q4, a vessel-",
  "window's OWN independently-measured (weekly-frequency) turnover tendency tracks how much of its ",
  "support-matched Phi is left unexplained by a fixed-effort counterfactual, external corroboration ",
  "that Phi.gap.matched is capturing behavioral reallocation rather than noise. This is a DESCRIPTIVE ",
  "check, not a regression, no causal claim is made or implied here. mean.share.raw (SECONDARY) is ",
  "printed alongside for comparison, a large and systematic difference between mean.share and ",
  "mean.share.raw across every row is itself a measure of how much the support mismatch was distorting ",
  "the un-matched (raw Phi) version of this same check.\n"
)

print(
  xtable(
    behavioral_share_table.rolling,
    caption = paste0(
      "Behavioral share of realized instability, PRIMARY (Phi.matched - Phi.BH) / Phi.matched (support-matched, ",
      "floored at Phi.matched > ", PHI_FLOOR_FOR_BEHAVIORAL_SHARE, "), overall and by quartile of the ",
      "independently-built within-season turnover classifier tau.window (06b\\_within\\_season\\_reallocation\\_rolling.R), ",
      "mean.share.raw is the SECONDARY (un-matched, raw Phi) ratio on the identical rows, for comparison"
    ),
    label = "tab:ch3-behavioral-share-by-tau-quartile", digits = 4
  ),
  file = file.path(table_dir, "table_behavioral_share_by_tau_quartile_rolling.tex"),
  include.rownames = FALSE
)
cat("Wrote table_behavioral_share_by_tau_quartile_rolling.tex\n")
