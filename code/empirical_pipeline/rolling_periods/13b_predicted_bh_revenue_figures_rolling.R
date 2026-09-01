# Chapter 3 empirical pipeline, rolling-window twin of Figure 3/3b-rolling
# (05b_table4_figure3_rolling.R), built on the EFFORT-based predicted
# buy-and-hold benchmark (12b_predicted_bh_revenue_rolling.R) instead of the
# revenue-share-based passive benchmark.
#
# NOT a straight duplicate of 05b_'s Section 4/5, the underlying benchmark
# answers a genuinely different question. passive_benchmark_window.rolling
# (built in 01b_, used by 05b_) fixes a vessel's REVENUE SHARES across a
# whole window and produces a full-window passive.cv, directly comparable
# to rev.cv (both are CVs, both already >= 0), so 05b_'s
# gap = rev.cv - passive.cv is a signed difference of two volatility
# measures, testing whether reallocation adds volatility.
# predicted_bh_vessel_window.rolling (built by 12b_) instead fixes a
# vessel's EFFORT (days fished per fishery) across the window's first 5
# years and produces a SINGLE HELD-OUT YEAR'S REVENUE LEVEL prediction for
# the 6th year, predicted.total, compared against that same year's
# actual.matching.total. There is no CV here at all, so Figure A below is a
# LEVEL scatter (dollars vs dollars for one target year), not a CV-vs-CV
# scatter like figure3_passive_benchmark_rolling.png, and Figure B's gap is
# redefined accordingly, see Section 4 below for exactly why.
#
# A methodological review's simulation found Figure B's Phi gradient can be
# CONFOUNDED with prediction precision, not just reallocation, since a
# high-Phi vessel mechanically fishes any given fishery in fewer of the
# window's 5 lookback years, so its avg.days/vessel.ratio rest on less data.
# 12b_ was extended (its own Section 6) to carry mean.n.active.years.predicted
# and mean.n.ratio.years.predicted forward for exactly this reason, and
# Section 5 below adds a third bin diagnostic reporting them per Phi bin,
# alongside coverage, so this confound is checkable rather than hidden. 12b_
# has since also been floored on the numerator side, predicted.revenue's own
# construction (12b_'s Section 5) now requires n.ratio.years >=
# BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION (3), so Figure B's sample and all
# three Section 5 diagnostics below are already restricted throughout to
# predictions resting on at least 3 lookback years of valid vessel-to-fleet
# ratio, not just left for the reader to notice after the fact. This does
# not mean Figure B is wrong, it means the confound is now visible, and
# partly addressed at the source.
#
# Reads intermediate data/ch3_rolling.rdata (vessel_window_summary.rolling,
# for Phi and is.specialist.window, and passive_benchmark_window.rolling,
# for the sample-size comparison printed in Section 1, both built by
# 01b_build_rolling_panel.R) and intermediate data/ch3_predicted_bh.rdata
# (predicted_bh_vessel_window.rolling, built by
# 12b_predicted_bh_revenue_rolling.R), both read-only. Neither
# 05b_table4_figure3_rolling.R nor 12b_predicted_bh_revenue_rolling.R is
# edited by this script.
#
# Saves figure3_predicted_bh_effort_rolling.png,
# figure3b_gap_by_phi_predicted_bh_effort_rolling.png, and (Section 4b, ADDED
# per a methodological review's suggestion) figure3d_gap_components_by_phi_predicted_bh_effort_rolling.png
# to figure_dir. Prints three additional diagnostics (signed relative
# deviation by Phi bin, mean coverage by Phi bin, mean lookback depth by Phi
# bin) plus Section 4b's own specialist consistency check (both components'
# mean for the Phi = 0 bin vs. the multi-fishery bins' own average, printed
# explicitly) and an optional exact variance/covariance decomposition,
# console only, no figure files for any of these.
#
# FIGURE D IS NOT A DECOMPOSITION OF FIGURE B, a methodological review
# flagged that the previous version of this header (and this figure's own
# title) incorrectly implied it was. Figure B's own gap is
# abs(actual.matching.total - predicted.total) / predicted.total, computed
# ONCE per vessel-window on DOLLAR TOTALS summed across J.predicted
# fisheries. Figure D instead plots, per Phi bin, the mean of
# mean.abs.days.component and mean.abs.rate.component, themselves each an
# unweighted mean of a PER-FISHERY LOG-space absolute deviation
# (12b_'s Section 6). Different aggregation level (dollar total vs
# per-fishery), different transform (a relative dollar deviation vs a mean
# of absolute log deviations), and even restricted to a common level and
# transform, MEAN|days| + MEAN|rate| does not equal MEAN|days + rate|
# whenever the two partially offset within a fishery (Jensen's-inequality-
# style slack, this script's own synthetic-data test Row 2, an "offsetting
# departures" case, demonstrates this concretely). Figure D is a genuinely
# separate, per-fishery, log-space diagnostic MOTIVATED by the same
# underlying prediction-error mechanism Figure B summarizes at the dollar
# level, not an algebraic decomposition of Figure B's own number, and Figure
# D's own subtitle now says so explicitly.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
if (!exists("vessel_window_summary.rolling") || !exists("passive_benchmark_window.rolling")) {
  load(rolling_panel_path)
}

predicted_bh_path <- file.path(intermediate_dir, "ch3_predicted_bh.rdata")
if (!exists("predicted_bh_vessel_window.rolling")) load(predicted_bh_path)

# ============================================================================
# 1. Sample construction
# ============================================================================
#
# predicted_bh_vessel_window.rolling is already restricted to eligible
# vessel-windows (semi-joined against vessel_window_eligibility.rolling
# inside 12b_), so the only additional filter needed here is
# n.fisheries.predicted > 0. 12b_ already forces predicted.total,
# actual.matching.total, and coverage to NA (not 0) whenever
# n.fisheries.predicted == 0, so this filter is equivalent to, and clearer
# than, an is.finite(predicted.total) filter.

fig_bh_data.rolling <- vessel_window_summary.rolling %>%
  select(Vessel.ADFG.Number, window.start, Phi, is.specialist.window) %>%
  inner_join(predicted_bh_vessel_window.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(n.fisheries.predicted > 0)

cat("Vessel-windows with a defined predicted BH-effort revenue -", nrow(fig_bh_data.rolling),
    ", of which single-fishery specialists (window) -", sum(fig_bh_data.rolling$is.specialist.window), "\n")

# Sanity check the invariant Sections 4-5's binning below leans on, coverage
# should already be finite for every row here (12b_'s own coverage
# construction only produces NA when actual.full.total <= 0, which 12b_'s
# own header notes should not happen once n.fisheries.predicted > 0, since
# every summand is itself a strictly positive "fished" revenue). Checked
# rather than assumed, printed rather than silently filtered, so every bin
# diagnostic below can share the identical row set without a defensive
# filter that could otherwise make one diagnostic's bins subtly disagree
# with another's.
cat("Rows in this sample with non-finite coverage (should be 0) -",
    sum(!is.finite(fig_bh_data.rolling$coverage)), "of", nrow(fig_bh_data.rolling), "\n")

# Non-blocking reviewer note, printed for comparison only, not used to
# filter fig_bh_data.rolling (nothing here needs rev.cv or passive.cv). Lets
# a reader comparing this script's Figure A/B against 05b_'s own
# figure3_passive_benchmark_rolling.png / figure3b_gap_by_phi_rolling.png
# see both sample sizes side by side rather than having to reload 05b_'s
# own script to find out.
passive_benchmark_n.rolling <- vessel_window_summary.rolling %>%
  filter(is.finite(rev.cv)) %>%
  inner_join(passive_benchmark_window.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(is.finite(passive.cv)) %>%
  nrow()
cat("For comparison, 05b_'s passive-benchmark sample size (figure3_passive_benchmark_rolling.png) -",
    passive_benchmark_n.rolling, "vs this script's effort-based-benchmark sample size -",
    nrow(fig_bh_data.rolling), "\n")

# ============================================================================
# 2. Shared Phi bin assignment, computed ONCE, used by Figure B and all
#    three Section 5 diagnostics
# ============================================================================
#
# Previously ntile(Phi, N_GAP_BINS_BH_ROLLING) was called independently at
# three separate lines (Figure B, diagnostic 1, diagnostic 2), with a
# comment claiming this was safe because all three shared
# two_stage_bin_summary(). That comment was wrong, sharing the AGGREGATION
# function does not guarantee sharing the bin ASSIGNMENT, a methodological
# review confirmed a row-order-dependent step upstream (e.g. an arrange())
# can make ntile() disagree between otherwise-identical calls, and
# demonstrated it changing 2 of 200 test rows' bin assignment. Fixed here by
# computing bin.order/bin.label exactly once, on fig_bh_data.rolling, before
# any of Figure B or Section 5's diagnostics touch it, every group_by()
# below keys off this one column pair, not a fresh ntile() call.
#
# N_GAP_BINS_BH_ROLLING, not N_GAP_BINS_ROLLING (05b_'s own constant) and
# not N_GAP_BINS (the design's do-not-reassign baseline name), a distinctly
# named local constant so this never collides if this script is ever
# sourced alongside 05b_table4_figure3_rolling.R in the same interactive
# session, mirroring 05b_'s own stated reasoning for why IT is
# N_GAP_BINS_ROLLING rather than N_GAP_BINS.
N_GAP_BINS_BH_ROLLING <- 8

fig_bh_binned.rolling <- bind_rows(
  fig_bh_data.rolling %>%
    filter(is.specialist.window) %>%
    mutate(bin.order = 0L, bin.label = "Specialists\n(Phi = 0)"),
  fig_bh_data.rolling %>%
    filter(!is.specialist.window) %>%
    mutate(bin.order = ntile(Phi, N_GAP_BINS_BH_ROLLING), bin.label = paste0("Q", bin.order))
)

# Exact mirror of 05b_'s two_stage_bin_summary(), trap #9 there applies
# identically here, a bin can contain several overlapping-window
# observations from the SAME vessel, so the bin SE must collapse to one
# value per vessel WITHIN the bin first (mean gap across that vessel's own
# windows landing here), then take sd / sqrt(n_distinct_vessels) over those
# vessel means, treating the vessel (not the vessel-window) as the
# independent sampling unit. na.rm = TRUE added at both mean() calls per a
# non-blocking reviewer note, one NA "gap" value (none currently arise in
# any of the four uses below, but coverage in particular is only checked
# to be finite empirically, not guaranteed to be by construction the way
# n.fisheries.predicted > 0 is) would otherwise silently NA out an entire
# vessel mean or an entire bin rather than just dropping one observation.
# sqrt() denominator uses sum(!is.na(...)), not nrow(vessel_means), so the
# SE's effective sample size is consistent with the na.rm = TRUE numerator
# above, immaterial today but avoids a latent under-count if this is ever
# triggered.
two_stage_bin_summary <- function(df) {
  vessel_means <- df %>%
    group_by(Vessel.ADFG.Number) %>%
    summarise(vessel.gap = mean(gap, na.rm = TRUE), .groups = "drop")
  tibble(
    n         = nrow(df),
    n.vessels = nrow(vessel_means),
    mean.Phi  = mean(df$Phi),
    mean.gap  = mean(vessel_means$vessel.gap, na.rm = TRUE),
    se.gap    = sd(vessel_means$vessel.gap, na.rm = TRUE) / sqrt(sum(!is.na(vessel_means$vessel.gap)))
  )
}

# Runs two_stage_bin_summary() once per bin on whatever "gap" column the
# caller has already attached to a copy of fig_bh_binned.rolling, then
# restores the specialists-first display order and the is.specialist.bin
# flag Figure B's own plot styling needs. Every one of Figure B and Section
# 5's three diagnostics below calls this on the SAME bin.order/bin.label
# pair, only the "gap" column differs.
run_bh_bin_summary <- function(binned_df) {
  binned_df %>%
    group_by(bin.order, bin.label) %>%
    group_modify(~ two_stage_bin_summary(.x)) %>%
    ungroup() %>%
    arrange(bin.order) %>%
    mutate(bin.label = fct_reorder(bin.label, bin.order), is.specialist.bin = bin.order == 0)
}

# ============================================================================
# 3. Figure A, figure3_predicted_bh_effort_rolling.png
# ============================================================================
#
# A LEVEL scatter (predicted vs actual dollars for the window's 6th year),
# not a CV-vs-CV scatter like figure3_passive_benchmark_rolling.png, the
# title/subtitle/axis labels below say so explicitly so the two figures are
# never mistaken for directly comparable. Restricted to non-specialist
# (multi-fishery) vessel-windows as the main plot, mirroring 05b_'s own
# specialist exclusion from its Figure 3 (a specialist's predicted_ijw is
# trivially its only fishery's own prediction, not an interesting
# reallocation test). Points colored by Phi quartile (ntile(Phi, 4),
# computed WITHIN the non-specialist subset, its own binning, coarser and
# independent of Figure B's N_GAP_BINS_BH_ROLLING octiles, a scatter needs
# fewer color groups to stay readable than a binned-mean plot does).
#
# Log scale on BOTH axes (scale_x_log10/scale_y_log10), per a methodological
# review, revenue levels span orders of magnitude and both predicted.total
# and actual.matching.total are confirmed strictly positive (verified when
# 12b_ was reviewed), so this is safe, no zero/negative values silently
# dropped, and the 45-degree reference line stays a 45-degree line in log
# space. Faceted by phi.quartile (facet_wrap), not just colored, at
# alpha = 0.25 with four color layers stacked in one panel later quartiles
# would paint over earlier ones on a dense scatter (05b_'s own Figure 3 uses
# alpha = 0.08 for a SINGLE color at similar density), faceting separates
# the four groups into their own panels so none of them get overplotted,
# while viridis coloring is kept (not reverted to a two-color palette) so
# the color legend still reads directly as the reallocation-intensity
# gradient facet_wrap's panel strips alone would not visually convey.
fig_a_data.rolling <- fig_bh_data.rolling %>%
  filter(!is.specialist.window) %>%
  mutate(phi.quartile = factor(ntile(Phi, 4), levels = 1:4, labels = paste0("Q", 1:4)))

figure3_predicted_bh_effort.rolling <- fig_a_data.rolling %>%
  ggplot(aes(x = predicted.total, y = actual.matching.total, color = phi.quartile)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ phi.quartile) +
  scale_color_viridis_d(name = "Reallocation\nintensity\n(Phi quartile)") +
  labs(
    title = "Predicted vs actual revenue, single held-out year, effort-based buy-and-hold benchmark (rolling)",
    subtitle = paste0(
      "Multi-fishery vessel-windows, dollars for the window's 6th (held-out) year only, ",
      "predictions require at least 3 lookback years of valid vessel-to-fleet ratio, ",
      "NOT a coefficient of variation, log-log axes, faceted by Phi quartile"
    ),
    x = "Predicted revenue, window's 6th year (fixed effort x fleet leave-one-out rate x vessel ratio), log scale",
    y = "Actual revenue, window's 6th year (matching fisheries only), log scale"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3_predicted_bh_effort_rolling.png"),
       figure3_predicted_bh_effort.rolling, width = 8, height = 7, dpi = 300)

cat("Wrote figure3_predicted_bh_effort_rolling.png\n")

# ============================================================================
# 4. Figure B, figure3b_gap_by_phi_predicted_bh_effort_rolling.png
# ============================================================================
#
# gap here is NOT a straight port of 05b_'s gap = rev.cv - passive.cv. That
# original gap differences two coefficients of variation (both already >= 0
# over a WHOLE window), so its sign directly means "more or less volatile
# than the fixed-mix benchmark," testing whether reallocation adds
# volatility. This benchmark only produces a SINGLE held-out year's revenue
# LEVEL, not a full-window CV, so there is no CV to difference here. The
# analogous test of "does deviation from a fixed-EFFORT counterfactual grow
# with reallocation intensity" needs the ABSOLUTE relative deviation as the
# primary quantity, gap = abs(actual.matching.total - predicted.total) /
# predicted.total, since that plays the same "magnitude of surprise
# relative to the passive benchmark" role a CV difference plays in the
# original. The SIGNED version of this same quantity (is there systematic
# over- or under-prediction, not just how big the surprise is) is checked
# separately as diagnostic 1 below, deliberately not folded into this
# figure, since a figure whose y-axis can be positive or negative would no
# longer read as a clean "magnitude of surprise" plot. IMPORTANT, per a
# methodological review's simulation, the gradient this figure shows can be
# CONFOUNDED with prediction precision (thinner lookback history), not just
# reallocation intensity, read this figure alongside diagnostic 3 below,
# not on its own.
fig3b_bh_data.rolling <- fig_bh_binned.rolling %>%
  mutate(gap = abs(actual.matching.total - predicted.total) / predicted.total)

gap_by_phi_bh.rolling <- run_bh_bin_summary(fig3b_bh_data.rolling)

print(gap_by_phi_bh.rolling)

figure3b_predicted_bh_effort.rolling <- gap_by_phi_bh.rolling %>%
  ggplot(aes(x = bin.label, y = mean.gap, color = is.specialist.bin)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean.gap - 1.96 * se.gap, ymax = mean.gap + 1.96 * se.gap), width = 0.2) +
  geom_line(
    data = gap_by_phi_bh.rolling %>% filter(!is.specialist.bin),
    aes(x = bin.label, y = mean.gap, group = 1), color = "steelblue", inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "gray40", "FALSE" = "steelblue"), guide = "none") +
  labs(
    title = "Absolute relative deviation from the effort-based buy-and-hold benchmark (rolling)",
    subtitle = paste0(
      "By reallocation intensity (Phi), single held-out year per window, two-stage vessel-clustered bin SEs, ",
      "predictions require at least 3 lookback years of valid vessel-to-fleet ratio, NOT a CV difference, ",
      "see this script's header note, and see diagnostic 3 (console) for a lookback-",
      "depth confound check before reading this gradient as pure reallocation"
    ),
    x = "Reallocation intensity (Phi), specialists then increasing bins",
    y = "Mean |actual - predicted| / predicted"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3b_gap_by_phi_predicted_bh_effort_rolling.png"),
       figure3b_predicted_bh_effort.rolling, width = 7, height = 5, dpi = 300)

cat("Wrote figure3b_gap_by_phi_predicted_bh_effort_rolling.png\n")

# ============================================================================
# 4b. Figure D, figure3d_gap_components_by_phi_predicted_bh_effort_rolling.png
#     Per-fishery, log-space days/rate deviation, by Phi bin, ADDED per a
#     methodological review's suggestion
# ============================================================================
#
# NOT A DECOMPOSITION OF FIGURE B, see this script's own header note for the
# full explanation (different aggregation level, different transform,
# absolute values do not sum additively even at a common level whenever the
# two components partially offset). Figure D is a separate diagnostic,
# motivated by the same underlying prediction-error mechanism, built from
# 12b_'s own EXACT additive log identity
# log(actual.revenue) - log(predicted.revenue) = days.component + rate.component
# (that script's own header note has the full derivation and the exactness
# argument, that identity itself IS exact, only Figure B and Figure D's
# comparability is not), aggregated to the vessel-window level as
# mean.abs.days.component/mean.abs.rate.component using the IDENTICAL
# unweighted-mean-across-J.predicted pattern mean.n.active.years.predicted/
# mean.n.ratio.years.predicted already use (12b_'s own Section 6 comment).
# This section bins those two columns with the SAME run_bh_bin_summary()
# two-stage vessel-clustered helper Figure B itself uses (Section 2 above),
# fed a different "gap" column each time, exactly the established idiom
# Section 5's three diagnostics below already use, so the days/rate split
# can never disagree with Figure B, or with each other, about which
# vessel-window falls in which bin.
#
# Plotted TOGETHER (one figure, two colored series, mirroring 05b_'s own
# figure4b_decomposition_path_rolling.png two-coefficient-series layout)
# rather than as two separate PNGs, so a reader sees at a glance which
# component is larger at each Phi level, without having to flip between
# files. This is a comparison of the two SERIES to each other, not of
# either series to Figure B's own y-axis value.
gap_by_phi_bh_days.rolling <- fig_bh_binned.rolling %>%
  mutate(gap = mean.abs.days.component) %>%
  run_bh_bin_summary() %>%
  mutate(component = "Days component")

gap_by_phi_bh_rate.rolling <- fig_bh_binned.rolling %>%
  mutate(gap = mean.abs.rate.component) %>%
  run_bh_bin_summary() %>%
  mutate(component = "Rate component")

gap_components_by_phi.rolling <- bind_rows(gap_by_phi_bh_days.rolling, gap_by_phi_bh_rate.rolling)

cat("\n===== Days-vs-rate MAD split by Phi bin =====\n")
print(gap_components_by_phi.rolling %>% select(bin.label, component, n, n.vessels, mean.gap, se.gap))

figure3d_gap_components.rolling <- gap_components_by_phi.rolling %>%
  ggplot(aes(x = bin.label, y = mean.gap, color = component, group = component)) +
  geom_point(size = 2.2, position = position_dodge(width = 0.3)) +
  geom_errorbar(
    aes(ymin = mean.gap - 1.96 * se.gap, ymax = mean.gap + 1.96 * se.gap),
    width = 0.2, position = position_dodge(width = 0.3)
  ) +
  geom_line(
    data = gap_components_by_phi.rolling %>% filter(!is.specialist.bin),
    position = position_dodge(width = 0.3)
  ) +
  scale_color_manual(values = c("Days component" = "darkorange", "Rate component" = "steelblue")) +
  labs(
    title = "Per-fishery log-space days/rate deviation, by Phi bin (rolling)",
    subtitle = paste0(
      "NOT a decomposition of Figure B (different aggregation level and transform, see this script's ",
      "header note), by reallocation intensity (Phi), single held-out year per window, two-stage ",
      "vessel-clustered bin SEs, mean of |days.component| and |rate.component| across each vessel-",
      "window's own J.predicted fisheries"
    ),
    x = "Reallocation intensity (Phi), specialists then increasing bins",
    y = "Mean |component| (log points)", color = NULL
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3d_gap_components_by_phi_predicted_bh_effort_rolling.png"),
       figure3d_gap_components.rolling, width = 7.5, height = 5, dpi = 300)

cat("Wrote figure3d_gap_components_by_phi_predicted_bh_effort_rolling.png\n")

# Specialist RELATIVE consistency check (not a falsification test), printed
# as an explicit diagnostic (not just a figure), REWORDED per a
# methodological review. A single-fishery (Phi = 0) vessel-window has
# nothing to reallocate TOWARD, there is no second fishery whose share could
# rise or fall, but that does NOT mean its days.component should be near
# zero in absolute terms, days.component = log(actual.days) - log(avg.days)
# measures pure SCALE variation in total effort (fishing more or fewer days
# than its own historical average that year), which a specialist can do
# just as much as a multi-fishery vessel, there is no structural reason for
# it to vanish. The earlier version of this check claimed "expected near
# zero" and framed a large specialist days component as evidence AGAINST
# reading Figure B's gradient as reallocation-driven at all, both are
# overstated, on real data a specialist's days component is likely to be
# comparable to (not near zero relative to) its own rate component, and a
# review of this check should not be read as falsifying anything either way.
# The more defensible, RELATIVE comparison is instead specialist vs.
# multi-fishery-bin AVERAGE, printed below for both components, a days
# component that grows sharply from the specialist bin to the multi-fishery
# bins (while the rate component stays comparatively flat) is CONSISTENT
# WITH (not proof of) reallocation driving the multi-fishery gradient, and a
# days component that does NOT grow at all across that same comparison
# would undercut that reading, but neither direction is dispositive on its
# own, printed here so the comparison itself is checkable once this script
# is actually run against real data, not to assert a directional claim in
# advance of that.
specialist_days.rolling <- gap_by_phi_bh_days.rolling %>% filter(is.specialist.bin)
specialist_rate.rolling <- gap_by_phi_bh_rate.rolling %>% filter(is.specialist.bin)
multi_days_mean.rolling <- gap_by_phi_bh_days.rolling %>% filter(!is.specialist.bin) %>%
  summarise(m = mean(mean.gap, na.rm = TRUE)) %>% pull(m)
multi_rate_mean.rolling <- gap_by_phi_bh_rate.rolling %>% filter(!is.specialist.bin) %>%
  summarise(m = mean(mean.gap, na.rm = TRUE)) %>% pull(m)

cat("\n===== Specialist vs. multi-fishery-bin-average consistency check (Phi = 0 bin, RELATIVE comparison) =====\n")
cat("Specialist mean |days.component| -",
    if (nrow(specialist_days.rolling) > 0) round(specialist_days.rolling$mean.gap, 4) else NA_real_,
    ", multi-fishery-bin average -", round(multi_days_mean.rolling, 4), ", ratio (specialist / multi-average) -",
    round((if (nrow(specialist_days.rolling) > 0) specialist_days.rolling$mean.gap else NA_real_) /
            multi_days_mean.rolling, 4), "\n")
cat("Specialist mean |rate.component| -",
    if (nrow(specialist_rate.rolling) > 0) round(specialist_rate.rolling$mean.gap, 4) else NA_real_,
    ", multi-fishery-bin average -", round(multi_rate_mean.rolling, 4), ", ratio (specialist / multi-average) -",
    round((if (nrow(specialist_rate.rolling) > 0) specialist_rate.rolling$mean.gap else NA_real_) /
            multi_rate_mean.rolling, 4), "\n")
cat("Reading rule, a days ratio well below the rate ratio (specialist relatively closer to the multi-fishery",
    "average on rate than on days) is CONSISTENT WITH reallocation contributing to the multi-fishery",
    "gradient, not proof of it, see this diagnostic's own comment above\n")

# ============================================================================
# 4c. Optional exact variance/covariance decomposition, DETAIL level,
#     added per a methodological review (explicitly marked optional there,
#     included since it is cheap and, unlike Figure D itself, genuinely
#     exact)
# ============================================================================
#
# Var(total.log.deviation) = Cov(total.log.deviation, days.component) +
# Cov(total.log.deviation, rate.component) holds EXACTLY, row-wise, at the
# DETAIL (Vessel.ADFG.Number x Fishery x window.start) grain, because
# total.log.deviation = days.component + rate.component holds exactly at
# that same grain (12b_'s own Section 5 identity), so
# Cov(total, days) + Cov(total, rate) = Cov(days, days) + Cov(rate, days) +
# Cov(days, rate) + Cov(rate, rate) = Var(days) + Var(rate) + 2 Cov(days, rate) =
# Var(days + rate) = Var(total), pure algebra, no approximation. This is a
# variance decomposition, not a MAD/mean-absolute-value one, so it does not
# have Figure D's own additivity problem, printed here (console only, no
# figure) BY Phi bin as an additional, genuinely-exact cross-check.
if (!exists("predicted_bh_detail.rolling")) load(predicted_bh_path)

detail_binned.rolling <- predicted_bh_detail.rolling %>%
  filter(is.finite(days.component), is.finite(rate.component), is.finite(total.log.deviation)) %>%
  inner_join(fig_bh_binned.rolling %>% select(Vessel.ADFG.Number, window.start, bin.order, bin.label),
             by = c("Vessel.ADFG.Number", "window.start"))

cat("\nDetail-level rows entering the exact variance/covariance decomposition -", nrow(detail_binned.rolling),
    "(a subset of predicted_bh_detail.rolling restricted to rows with every log-decomposition component",
    "defined AND whose vessel-window survived Section 1's own sample filter)\n")

covariance_by_phi.rolling <- detail_binned.rolling %>%
  group_by(bin.order, bin.label) %>%
  summarise(
    n            = n(),
    var.total    = var(total.log.deviation),
    cov.total.days = cov(total.log.deviation, days.component),
    cov.total.rate = cov(total.log.deviation, rate.component),
    .groups = "drop"
  ) %>%
  mutate(
    cov.sum.check   = cov.total.days + cov.total.rate,
    identity.residual = var.total - cov.sum.check
  ) %>%
  arrange(bin.order)

cat("\n===== Exact variance/covariance decomposition by Phi bin, Var(total) = Cov(total,days) + Cov(total,rate) =====\n")
print(covariance_by_phi.rolling %>% select(bin.label, n, var.total, cov.total.days, cov.total.rate, identity.residual))
cat("Max absolute identity residual across bins (should be ~0, floating-point epsilon) -",
    round(max(abs(covariance_by_phi.rolling$identity.residual), na.rm = TRUE), 10),
    " (na.rm = TRUE, a bin with only n = 1 detail rows has var()/cov() return NA by construction,",
    "not a broken identity, that bin's own row is still printed above so it is not silently hidden)\n")

# ============================================================================
# 5. Three console-only diagnostics, not saved as figures
# ============================================================================
#
# All three reuse fig_bh_binned.rolling's SAME bin.order/bin.label pair
# (Section 2) and the SAME run_bh_bin_summary() helper, only the "gap"
# column fed in differs, so none of these can disagree with Figure B, or
# with each other, about which vessel-windows fall in which bin.
#
# (1) SIGNED relative deviation per bin, checking for a directional
# (systematic over- or under-prediction) pattern layered on top of the pure
# magnitude pattern Figure B shows.
gap_by_phi_bh_signed.rolling <- fig_bh_binned.rolling %>%
  mutate(gap = (actual.matching.total - predicted.total) / predicted.total) %>%
  run_bh_bin_summary() %>%
  rename(mean.signed.gap = mean.gap, se.signed.gap = se.gap)

cat("\n===== Diagnostic 1, SIGNED relative deviation per bin (over- vs under-prediction check) =====\n")
print(gap_by_phi_bh_signed.rolling)

# (2) Mean coverage per bin, checking coverage is not itself confounded with
# Phi, a high-Phi bin with systematically worse coverage (more of its
# window.end fisheries missing a valid prediction) would undermine reading
# Figure B as clean evidence about reallocation rather than about which
# fisheries happened to get dropped from the comparison.
gap_by_phi_bh_coverage.rolling <- fig_bh_binned.rolling %>%
  mutate(gap = coverage) %>%
  run_bh_bin_summary() %>%
  rename(mean.coverage = mean.gap, se.coverage = se.gap)

cat("\n===== Diagnostic 2, mean coverage per bin (checking coverage is not confounded with Phi) =====\n")
print(gap_by_phi_bh_coverage.rolling)

# NOTE, added once 12b_'s BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION floor was in
# place. The floor does not remove the Phi confound, it converts part of it
# into a SELECTION effect on coverage instead, thin-history fishery legs are
# mechanically more common for high-Phi vessels (the same mechanism behind
# Diagnostic 3), so the floor now drops those fishery predictions
# non-randomly across Phi bins. That has two consequences worth reading this
# diagnostic for, (a) it mechanically lowers coverage more in high-Phi bins
# than low-Phi bins, since a high-Phi vessel-window is more likely to lose
# one of its fisheries to the floor, and (b) it can drop a vessel-window out
# of the Figure B / diagnostic sample entirely if the floor removed its only
# predicted fishery, shrinking n.vessels in a bin rather than just lowering
# coverage within it. Diagnostic 2 here is the check for this residual,
# floor-induced selection effect on coverage, a distinct question from
# Diagnostic 3 just below, which checks whether the numerator-side
# (predicted.revenue precision) problem the floor was added to fix is
# actually addressed, not whether the floor introduced a new coverage-side
# selection effect of its own.

# (3) NEW, mean lookback depth per bin, the direct check for the confound a
# methodological review's simulation identified, true predictability held
# CONSTANT across Phi while only lookback depth varied still produced a
# spurious Q8/Q1 gradient of 1.57x in Figure B's own gap measure there,
# because a high-Phi vessel mechanically fishes any given fishery in fewer
# of the window's 5 lookback years. Both lookback-depth measures 12b_ now
# carries (its own Section 6) are reported side by side here rather than
# picking just one, mean.n.active.years.predicted (the broader "any positive
# year" floor avg.days is built from) and mean.n.ratio.years.predicted (the
# stricter floor vessel.ratio, the more precision-sensitive multiplicative
# term, is built from). 12b_ now floors predicted.revenue's own construction
# on n.ratio.years >= BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION, so the numbers
# printed below already reflect that fix, they are the POST-floor
# lookback-depth distribution among surviving predictions, not the
# pre-floor distribution that originally exposed the problem. The reading
# rule below survives the fix and still matters, if either measure STILL
# declines sharply across Phi bins here despite the floor, that remains a
# live warning sign, it would mean the floor's fixed lower bound is not
# enough to equalize lookback depth across bins, and Figure B's gradient
# should still be read as at least partly a precision artifact, not purely
# a reallocation effect.
gap_by_phi_bh_lookback_active.rolling <- fig_bh_binned.rolling %>%
  mutate(gap = mean.n.active.years.predicted) %>%
  run_bh_bin_summary() %>%
  transmute(bin.order, bin.label, is.specialist.bin, n, n.vessels, mean.Phi,
            mean.n.active.years.predicted = mean.gap, se.n.active.years.predicted = se.gap)

gap_by_phi_bh_lookback_ratio.rolling <- fig_bh_binned.rolling %>%
  mutate(gap = mean.n.ratio.years.predicted) %>%
  run_bh_bin_summary() %>%
  transmute(bin.order, mean.n.ratio.years.predicted = mean.gap, se.n.ratio.years.predicted = se.gap)

gap_by_phi_bh_lookback.rolling <- gap_by_phi_bh_lookback_active.rolling %>%
  left_join(gap_by_phi_bh_lookback_ratio.rolling, by = "bin.order")

cat("\n===== Diagnostic 3, mean lookback depth per bin (checking Figure B's gradient against the",
    "thinning-history confound, not just reallocation intensity) =====\n")
print(gap_by_phi_bh_lookback.rolling)

# NOTE, added per a methodological review of real-data output that used this
# very diagnostic to catch the thinning-history problem in the first place
# (a handful of vessel-fishery-window predictions built from only 1-2
# lookback years were numerically unstable and were dominating the high-Phi
# bin means in an earlier run of Figure B). 12b_ was fixed in response, it
# now floors predicted.revenue's construction on n.ratio.years >=
# BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION (see 12b_predicted_bh_revenue_rolling.R's
# own local constants), so every row this diagnostic now averages over
# already cleared that floor. The numbers printed just above are therefore
# the POST-floor lookback-depth distribution among surviving predictions,
# not the pre-floor distribution that originally exposed the problem, a
# reader should not mistake a now-healthier-looking Diagnostic 3 for evidence
# the underlying data were never thin, it is evidence the floor is doing its
# job of excluding the thin cases.
cat("\nNote, Diagnostic 3 above reflects predictions that already cleared 12b_'s",
    "BH_MIN_LOOKBACK_YEARS_FOR_PREDICTION floor on n.ratio.years, this is the POST-floor lookback-depth",
    "distribution among surviving predictions, not the pre-floor distribution that originally",
    "diagnosed the thinning-history problem this floor was added to fix\n")
