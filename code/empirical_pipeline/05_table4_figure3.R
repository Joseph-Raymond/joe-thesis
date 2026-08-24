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

vessel_analysis <- vessel_summary %>%
  filter(meets.min.years, is.finite(rev.cv), !is.na(prime.fishery))

cat("Vessels entering Table 4:", nrow(vessel_analysis), "\n")

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

model_baseline   <- feols(rev.cv ~ H_bar | prime.fishery, data = vessel_analysis)
model_decomposed <- feols(rev.cv ~ H_LR + Phi | prime.fishery, data = vessel_analysis)

# Standardized versions, z-scoring the outcome and regressors before fitting
# so coefficients are comparable in size across models (chapter3_plan.md
# Section 9.3, "report g1 and g2 standardized").
vessel_std <- vessel_analysis %>%
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
  select(Vessel.ADFG.Number, rev.cv, H_bar, H_LR, Phi) %>%
  inner_join(passive_benchmark, by = "Vessel.ADFG.Number") %>%
  filter(is.finite(passive.cv))

cat("Vessels with a computable passive benchmark:", nrow(fig3_data), "\n")

figure3 <- fig3_data %>%
  ggplot(aes(x = passive.cv, y = rev.cv)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "firebrick") +
  labs(
    title = "Realized revenue CV versus a passive buy-and-hold benchmark",
    subtitle = "Benchmark holds each vessel's own long-run fishery weights fixed against actual fleet-wide revenue, same years and CV formula as realized. Dashed line is the 45-degree reference.",
    x = "Passive benchmark CV (buy-and-hold, vessel's own weights)",
    y = "Realized revenue CV"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3_passive_benchmark.png"),
       figure3, width = 7, height = 6, dpi = 300)

cat("Wrote table4_decomposition_regression.tex and figure3_passive_benchmark.png\n")
