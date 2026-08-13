# Chapter 3 empirical pipeline, Chapter3_outline.md Section 4
#
# Table 4. Baseline versus decomposed CV-on-HHI regression, standardized
#          coefficients, run in levels (not logs, since H_bar = H_LR + Phi is
#          additive and log of a sum does not split into a sum of logs, per
#          chapter3_plan.md Section 9.2).
# Figure 3. Fleet-level fishery return covariance structure, used to build a
#          passive buy-and-hold benchmark CV for each vessel's portfolio,
#          plotted against realized CV.
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
# 1. Build a fleet-level fishery "return" series, the year-over-year log
#    change in fleet-mean revenue per active vessel (fleet_mean_revenue was
#    saved by 01_build_panel.R). This is the empirical analogue of Chapter
#    2's rho sweep, a covariance structure across fisheries' realized
#    outcomes, not across individual vessels.
# 2. Restrict to fisheries with at least MIN_FISHERY_RETURN_YEARS of data and
#    build their covariance matrix Sigma.
# 3. For each vessel, apply its own long-run realized shares (vessel_mean_share,
#    the same weights that define H_LR) as passive buy-and-hold portfolio
#    weights over the fisheries it actually holds, renormalized to the
#    subset of fisheries present in Sigma. passive.cv = sqrt(w' Sigma w), the
#    volatility of the return that a buy-and-hold operator with vessel i's
#    own long-run allocation would have experienced.
# 4. Compare passive.cv to realized rev.cv. Points above the 45-degree line
#    experienced more revenue instability than their held portfolio's return
#    covariance alone implies, i.e. rotation into imperfectly correlated
#    fisheries raised risk. Points below show rotation that hedged risk
#    below the passive benchmark.
#
# This is one reasonable operationalization of an intentionally open design
# choice (chapter3_plan.md Section 5C, "passive equal or historical
# weights"). Historical weights are used here as the primary version. An
# equal-weight variant across the full held set (including fisheries never
# fished, which get zero weight under the historical version by
# construction) is a natural robustness cut to add once this runs.

if (!exists("fleet_mean_revenue") || !exists("vessel_mean_share")) load(panel_path)

fishery_returns <- fleet_mean_revenue %>%
  arrange(Fishery, Batch.Year) %>%
  group_by(Fishery) %>%
  mutate(log.return = log(fleet_mean_revenue) - log(lag(fleet_mean_revenue))) %>%
  ungroup() %>%
  filter(!is.na(log.return))

fishery_year_counts <- fishery_returns %>% count(Fishery, name = "n.years")
eligible_fisheries <- fishery_year_counts %>% filter(n.years >= MIN_FISHERY_RETURN_YEARS) %>% pull(Fishery)
cat("Fisheries entering the return covariance matrix:", length(eligible_fisheries),
    "of", n_distinct(fishery_returns$Fishery), "\n")

return_wide <- fishery_returns %>%
  filter(Fishery %in% eligible_fisheries) %>%
  select(Batch.Year, Fishery, log.return) %>%
  pivot_wider(names_from = Fishery, values_from = log.return) %>%
  arrange(Batch.Year)

Sigma <- cov(select(return_wide, -Batch.Year), use = "pairwise.complete.obs")
Sigma[is.na(Sigma)] <- 0 # a fishery-pair with no year overlap contributes no covariance

# Portfolio return volatility under vessel i's own long-run held-fishery
# weights, restricted and renormalized to eligible_fisheries. Vessels with
# most of their weight in ineligible (thin) fisheries get a passive.cv built
# on a small, unreliable renormalized weight vector, hence dropped.weight.
passive_benchmark <- vessel_mean_share %>%
  filter(Fishery %in% eligible_fisheries) %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(
    kept.weight = sum(mean.share.fishery),
    weights = list(setNames(mean.share.fishery, Fishery)),
    .groups = "drop"
  ) %>%
  filter(kept.weight > 0) %>%
  mutate(
    weights = map(weights, ~ .x / sum(.x)),
    passive.cv = map_dbl(weights, function(w) {
      idx <- names(w)
      sqrt(as.numeric(t(w) %*% Sigma[idx, idx, drop = FALSE] %*% w))
    })
  ) %>%
  select(Vessel.ADFG.Number, kept.weight, passive.cv)

fig3_data <- vessel_analysis %>%
  select(Vessel.ADFG.Number, rev.cv, H_bar, H_LR, Phi) %>%
  inner_join(passive_benchmark, by = "Vessel.ADFG.Number") %>%
  filter(is.finite(passive.cv))

cat("Vessels with a computable passive benchmark:", nrow(fig3_data), "\n")
cat("Median share of held-portfolio weight retained after the eligibility filter:",
    round(median(fig3_data$kept.weight), 3), "\n")

figure3 <- fig3_data %>%
  ggplot(aes(x = passive.cv, y = rev.cv)) +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "firebrick") +
  labs(
    title = "Realized revenue CV versus a passive buy-and-hold benchmark",
    subtitle = "Benchmark uses each vessel's own long-run fishery weights against the fleet return covariance matrix. Dashed line is the 45-degree reference.",
    x = "Passive benchmark CV (buy-and-hold, vessel's own weights)",
    y = "Realized revenue CV"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3_passive_benchmark.png"),
       figure3, width = 7, height = 6, dpi = 300)

cat("Wrote table4_decomposition_regression.tex and figure3_passive_benchmark.png\n")
