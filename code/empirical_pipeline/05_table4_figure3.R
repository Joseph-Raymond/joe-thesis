# Chapter 3 empirical pipeline, Chapter3_outline.md Section 4
#
# Table 4. Baseline versus decomposed CV-on-HHI regression, standardized
#          coefficients, run in levels (not logs, since H_bar = H_LR + Phi is
#          additive and log of a sum does not split into a sum of logs, per
#          chapter3_plan.md Section 9.2).
# Figure 3. A passive buy-and-hold benchmark CV for each vessel's portfolio
#          (same sd/mean-of-revenue-levels formula as realized CV, applied
#          to a counterfactual fixed-weights revenue series), plotted
#          against realized CV.
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_summary")) load(panel_path)
if (!exists("vessel_mean_share")) load(panel_path)

vessel_analysis <- vessel_summary %>%
  filter(meets.min.years, is.finite(rev.cv), !is.na(prime.fishery))

# A vessel that only ever fished one fishery across its whole panel has
# H_LR = 1 and Phi = 0 exactly, there is no within-vessel reallocation for
# Phi to measure. vessel_mean_share's Fishery dimension is already each
# vessel's own ever-fished set (built by completing Fishery x Batch.Year
# grouped by vessel in 01_build_panel.R Section 6), so counting distinct
# fisheries there is a direct count of fisheries ever fished, not an
# approximation off H_LR's floating-point value.
n_fisheries_fished <- vessel_mean_share %>%
  count(Vessel.ADFG.Number, name = "n.fisheries.fished")

vessel_analysis <- vessel_analysis %>%
  left_join(n_fisheries_fished, by = "Vessel.ADFG.Number") %>%
  mutate(is.specialist = n.fisheries.fished == 1)

cat("Vessels entering Table 4:", nrow(vessel_analysis),
    " of which single-fishery specialists:", sum(vessel_analysis$is.specialist), "\n")

# ============================================================================
# Table 4. Baseline versus decomposed regression
# ============================================================================
#
# CV_i = b0 + b1 * H_bar_i + FE(prime.fishery)                (baseline)
# CV_i = g0 + g1 * H_LR_i + g2 * Phi_i + FE(prime.fishery)     (decomposed)
#
# prime.fishery fixed effects absorb a lot of cross-vessel variation in
# H_LR, since specialists in one fishery share an H_LR (chapter3_plan.md
# Section 9.2), so g1 in particular is identified off thin within-fishery
# variation and should be read with that caveat, not as evidence against the
# decomposition if it comes out small or noisy.
#
# No "controls" are added beyond the fixed effect, Chapter3_outline.md
# Section 4 does not specify a control set for Table 4 itself (vessel-level
# controls like length show up later, Section 6). Add them here once decided.
#
# Main-text models are restricted to multi-fishery vessels (is.specialist ==
# FALSE), not the full pooled sample. Single-fishery specialists have Phi
# pinned at exactly 0, a mass point with no within-group spread, so they
# contribute nothing to identifying g2 and instead just sit in the FE group
# for their prime.fishery as an unmoving anchor at (H_LR = 1, Phi = 0). A
# Phi-binned check on the Figure 3 gap (rev.cv - passive.cv) found the gap
# is NOT smooth through Phi = 0, specialists sit above the gap of vessels
# with small positive Phi before it climbs again, evidence they are on a
# different footing (idiosyncratic noise around the fleet mean, not
# reallocation) rather than the low end of the same relationship. Excluding
# them keeps the decomposition's slope estimates from being anchored by a
# subgroup the decomposition was never meant to describe. The pooled sample
# is kept below as an explicit robustness comparison, not dropped.

vessel_multi <- vessel_analysis %>% filter(!is.specialist)

model_baseline   <- feols(rev.cv ~ H_bar | prime.fishery, data = vessel_multi)
model_decomposed <- feols(rev.cv ~ H_LR + Phi | prime.fishery, data = vessel_multi)

# Standardized versions, z-scoring the outcome and regressors before fitting
# so coefficients are comparable in size across models (chapter3_plan.md
# Section 9.3, "report g1 and g2 standardized"). Scaled within the
# multi-fishery sample, not the pooled one, so the z-scores describe the
# same population the models are fit on.
vessel_std <- vessel_multi %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

model_baseline_std   <- feols(z.rev.cv ~ z.H_bar | prime.fishery, data = vessel_std)
model_decomposed_std <- feols(z.rev.cv ~ z.H_LR + z.Phi | prime.fishery, data = vessel_std)

etable(
  model_baseline, model_decomposed, model_baseline_std, model_decomposed_std,
  headers = c("Baseline", "Decomposed", "Baseline (z)", "Decomposed (z)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression.tex"),
  replace = TRUE
)

print(etable(model_baseline, model_decomposed, model_baseline_std, model_decomposed_std))

# Share of b1 that loads onto Phi rather than H_LR, only meaningful for the
# standardized models since that is the scale on which the plan's "loads
# onto Phi" language is defined.
g2_share <- coef(model_decomposed_std)["z.Phi"] /
  (coef(model_decomposed_std)["z.H_LR"] + coef(model_decomposed_std)["z.Phi"])
cat("Standardized share of the decomposed slope loading onto Phi:", round(g2_share, 3), "\n")

# ----------------------------------------------------------------------
# Robustness. Same four models on the full pooled sample (specialists and
# multi-fishery vessels together), the version this replaced as the
# main-text spec. Kept as an explicit side-by-side comparison rather than
# silently dropped, a reviewer will ask what specialists do to the estimate.
# ----------------------------------------------------------------------

model_baseline_pooled   <- feols(rev.cv ~ H_bar | prime.fishery, data = vessel_analysis)
model_decomposed_pooled <- feols(rev.cv ~ H_LR + Phi | prime.fishery, data = vessel_analysis)

vessel_std_pooled <- vessel_analysis %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

model_baseline_std_pooled   <- feols(z.rev.cv ~ z.H_bar | prime.fishery, data = vessel_std_pooled)
model_decomposed_std_pooled <- feols(z.rev.cv ~ z.H_LR + z.Phi | prime.fishery, data = vessel_std_pooled)

etable(
  model_baseline_pooled, model_decomposed_pooled,
  model_baseline_std_pooled, model_decomposed_std_pooled,
  headers = c("Baseline (pooled)", "Decomposed (pooled)",
              "Baseline (pooled, z)", "Decomposed (pooled, z)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression_pooled.tex"),
  replace = TRUE
)

cat("Wrote table4_decomposition_regression.tex (multi-fishery vessels, main text)",
    "and table4_decomposition_regression_pooled.tex (all vessels, robustness)\n")

# ============================================================================
# Figure 3. Passive buy-and-hold benchmark vs realized CV
# ============================================================================
#
# passive.cv is built the same way rev.cv is, sd/mean of a REVENUE LEVEL
# series over a vessel's own active years, not sqrt(w' Sigma w) on log
# returns (an earlier version of this script did that). Log returns and
# revenue levels are different statistical objects, and comparing rev.cv
# (levels) against a return-based benchmark biases the comparison, a vessel
# with any smooth multi-year revenue trend, from inflation, growth, anything,
# inflates CV-of-levels a lot while barely moving SD-of-log-returns, so
# "realized CV exceeds passive CV" could partly just reflect that mismatch
# rather than real reallocation risk. This version compares like with like.
#
# For vessel i in year t, restricted to i's own active years (the same
# window rev.cv is computed over), passive_revenue_it = sum_j w_ij *
# fleet_mean_revenue_jt, what i would have earned that year by holding its
# own fixed long-run weights (vessel_mean_share, the same weights that
# define H_LR) against what the fleet as a whole actually earned in each of
# i's held fisheries that year. passive.cv_i = sd/mean of that series, same
# formula, same window as rev.cv, only the revenue-generating process
# (actual vs counterfactual-fixed-weights) differs. This also matches
# Chapter 2's own CV definition (levels-based), keeping Chapter 3
# comparable to Chapter 2 rather than quietly using a different metric.
#
# Reading the figure, same as before, a point above the 45-degree line
# experienced more revenue instability than holding its own long-run
# portfolio fixed would have, given what the fleet actually earned, i.e.
# real reallocation risk, not just an accounting artifact of the H_bar/Phi
# construction. A point below hedged risk below that passive benchmark.
#
# fleet_mean_revenue is missing for a (fishery, year) with zero fleet-wide
# activity that year, filled with 0 here, consistent with how
# forgone.value/fished.value already treat that case in 01_build_panel.R.
# Weights in vessel_mean_share already sum to 1 for every vessel by
# construction (shares sum to 1 within any year, so their vessel-level means
# do too), so no renormalization or fishery-eligibility filter is needed
# here, unlike the return-covariance version this replaced.
#
# Single-fishery specialists (is.specialist, built above for Table 4) are
# kept in this figure rather than dropped, but colored separately. A
# specialist's passive.cv is built off the fleet mean of the one fishery it
# holds, so any gap to its own rev.cv is pure idiosyncratic noise around
# that fleet mean, not reallocation, there is nothing to reallocate. Hiding
# those points would hide that floor and let the 45-degree-line pattern read
# as evidence of reallocation risk alone, when part of the pattern predates
# any behavior at all. Keeping them visible, distinguished by color, lets
# the figure show both facts at once.

if (!exists("fleet_mean_revenue") || !exists("vessel_mean_share") || !exists("vessel_year")) load(panel_path)

active_vessel_years <- vessel_year %>%
  filter(vessel.year.rev > 0) %>%
  select(Vessel.ADFG.Number, Batch.Year)

passive_series <- vessel_mean_share %>%
  semi_join(vessel_analysis, by = "Vessel.ADFG.Number") %>%
  inner_join(active_vessel_years, by = "Vessel.ADFG.Number", relationship = "many-to-many") %>%
  left_join(fleet_mean_revenue %>% select(Batch.Year, Fishery, fleet_mean_revenue),
            by = c("Batch.Year", "Fishery")) %>%
  mutate(fleet_mean_revenue = replace_na(fleet_mean_revenue, 0)) %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  summarise(passive_revenue = sum(mean.share.fishery * fleet_mean_revenue), .groups = "drop")

passive_benchmark <- passive_series %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(
    n.years.passive = n(),
    passive.cv = sd(passive_revenue) / mean(passive_revenue),
    .groups = "drop"
  )

fig3_data <- vessel_analysis %>%
  select(Vessel.ADFG.Number, rev.cv, H_bar, H_LR, Phi, is.specialist) %>%
  inner_join(passive_benchmark, by = "Vessel.ADFG.Number") %>%
  filter(is.finite(passive.cv))

cat("Vessels with a computable passive benchmark:", nrow(fig3_data),
    " of which single-fishery specialists:", sum(fig3_data$is.specialist), "\n")

figure3 <- fig3_data %>%
  mutate(vessel.type = if_else(is.specialist, "Single-fishery specialist", "Multi-fishery vessel")) %>%
  ggplot(aes(x = passive.cv, y = rev.cv, color = vessel.type)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "firebrick") +
  scale_color_manual(values = c("Single-fishery specialist" = "gray50",
                                 "Multi-fishery vessel" = "steelblue")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  labs(
    title = "Realized revenue CV versus a passive buy-and-hold benchmark",
    subtitle = "Benchmark holds each vessel's own long-run fishery weights fixed against actual fleet-wide revenue, same years and CV formula as realized. Dashed line is the 45-degree reference. Specialists (one fishery ever) have Phi = 0 by construction, any gap for them is idiosyncratic noise, not reallocation.",
    x = "Passive benchmark CV (buy-and-hold, vessel's own weights)",
    y = "Realized revenue CV",
    color = NULL
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3_passive_benchmark.png"),
       figure3, width = 7, height = 6, dpi = 300)

cat("Wrote figure3_passive_benchmark.png\n")

# ============================================================================
# Figure 3b. Gap between realized and passive CV, binned by Phi
# ============================================================================
#
# gap_i = rev.cv_i - passive.cv_i, the vertical distance a point in Figure 3
# sits above (or below) the 45-degree line. Binning by Phi and plotting the
# mean gap per bin is the cleanest single piece of evidence in this pipeline
# that reallocation itself is associated with added revenue instability,
# cleaner than Table 4's regression coefficient because it imposes no
# functional form and does not depend on how much of the FE structure is
# absorbing cross-vessel variation.
#
# Single-fishery specialists are shown as their own point, not folded into
# the lowest Phi bin. Phi is exactly 0 for every specialist, a mass point,
# so binning them together with vessels that have small positive Phi would
# either swallow the specialist point whole or, if the specialist mass is
# bigger than one bin's worth, spill zeros into the neighboring bin and
# quietly pull its mean down. Keeping specialists as their own category
# avoids both problems and matches the point this figure exists to make,
# specialists sit apart from the reallocation relationship, not at its low
# end, since part of the gap is idiosyncratic noise around the fleet mean
# that has nothing to do with reallocation.

fig3b_data <- fig3_data %>%
  mutate(gap = rev.cv - passive.cv)

specialist_summary <- fig3b_data %>%
  filter(is.specialist) %>%
  summarise(
    bin.label = "Specialists\n(Phi = 0)",
    bin.order = 0,
    n = n(),
    mean.Phi = mean(Phi),
    mean.gap = mean(gap),
    se.gap = sd(gap) / sqrt(n)
  )

N_GAP_BINS <- 8

multi_summary <- fig3b_data %>%
  filter(!is.specialist) %>%
  mutate(phi.bin = ntile(Phi, N_GAP_BINS)) %>%
  group_by(phi.bin) %>%
  summarise(
    bin.label = paste0("Q", phi.bin),
    bin.order = phi.bin,
    n = n(),
    mean.Phi = mean(Phi),
    mean.gap = mean(gap),
    se.gap = sd(gap) / sqrt(n),
    .groups = "drop"
  ) %>%
  select(-phi.bin)

gap_by_phi <- bind_rows(specialist_summary, multi_summary) %>%
  mutate(bin.label = fct_reorder(bin.label, bin.order),
         is.specialist.bin = bin.order == 0)

print(gap_by_phi)

figure3b <- gap_by_phi %>%
  ggplot(aes(x = bin.label, y = mean.gap, color = is.specialist.bin)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean.gap - 1.96 * se.gap, ymax = mean.gap + 1.96 * se.gap), width = 0.2) +
  geom_line(
    data = gap_by_phi %>% filter(!is.specialist.bin),
    aes(x = bin.label, y = mean.gap, group = 1),
    color = "steelblue", inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "gray40", "FALSE" = "steelblue"), guide = "none") +
  labs(
    title = "Gap between realized and passive benchmark CV, by reallocation intensity",
    subtitle = "Specialists (Phi = 0) shown separately from multi-fishery vessels, grouped into equal-sized bins by Phi (low to high). Error bars are 95% CI on the mean gap.",
    x = "Reallocation intensity (Phi), specialists then increasing bins",
    y = "Mean gap (realized CV − passive CV)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3b_gap_by_phi.png"),
       figure3b, width = 7, height = 5, dpi = 300)

cat("Wrote figure3b_gap_by_phi.png\n")
