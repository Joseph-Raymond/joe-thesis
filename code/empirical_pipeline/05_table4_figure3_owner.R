# Chapter 3 empirical pipeline, owner-level twin of 05_table4_figure3.R
#
# File.Number here means CFEC.Permit.Holder.Filing.Number (the permit
# holder), NOT CFEC.Vessel.Owner.Filing.Number (a different field,
# 01_build_panel.R Section 4 has an explicit comment on why these two IDs
# can differ). This matches the convention 04_table3.R and
# 10_network_similarity.R already use for "owner," so "owner" means "permit
# holder" everywhere in this pipeline, not literally whoever owns the
# physical vessel, do not assume otherwise reading this script.
#
# Motivation, an owner running multiple vessels can reallocate effort ACROSS
# VESSELS, not just across fisheries within one vessel, a margin invisible
# in every vessel-level number this pipeline computes elsewhere. This script
# reruns Table 4 and Figure 3/3b at the owner grain to look for that margin.
#
# Table 4 (owner).  Baseline versus decomposed CV-on-HHI regression, owner
#          grain, standardized coefficients, run in levels (not logs), same
#          reasoning as the vessel-level Table 4 (H_bar = H_LR + Phi is
#          additive, log of a sum does not split into a sum of logs).
# Figure 3 (owner). A passive buy-and-hold benchmark CV for each owner's
#          portfolio (same sd/mean-of-revenue-levels formula as realized
#          CV, applied to a counterfactual fixed-weights revenue series),
#          plotted against realized CV.
#
# Same methodology throughout as 05_table4_figure3.R, same regression spec,
# same specialist-exclusion logic for the main-text sample, same passive-
# benchmark construction, same Figure 3b gap-by-Phi binning, only the grain
# changes (File.Number instead of Vessel.ADFG.Number) and the underlying
# objects are their owner-level counterparts (owner_summary instead of
# vessel_summary, owner_mean_share instead of vessel_mean_share,
# fleet_mean_revenue_owner instead of fleet_mean_revenue). Every comment
# below explaining WHY a modeling choice was made is ported directly from
# 05_table4_figure3.R rather than re-derived, the reasoning transfers as-is,
# see that script if a comment here seems to assume context not repeated.
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R
# (owner_summary, owner_mean_share, owner_year, fleet_mean_revenue_owner,
# all built in that script's Section 7, owner_mean_share and owner_summary's
# own prime.fishery column both added there specifically to support this
# script, mirroring vessel_mean_share/vessel_summary's prime.fishery).
#
# Writes to the SAME table_dir/figure_dir as 05_table4_figure3.R, with an
# "_owner" suffix on every filename so nothing overwrites the vessel-level
# outputs and the two sit side by side.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_summary")) load(panel_path)
if (!exists("owner_mean_share")) load(panel_path)

owner_analysis <- owner_summary %>%
  filter(meets.min.years, is.finite(rev.cv), !is.na(prime.fishery))

# An owner that only ever fished one fishery across its whole panel has
# H_LR = 1 and Phi = 0 exactly, there is no within-owner reallocation for
# Phi to measure. owner_mean_share's Fishery dimension is already each
# owner's own ever-fished set (built by completing Fishery x Batch.Year
# grouped by owner in 01_build_panel.R Section 7), so counting distinct
# fisheries there is a direct count of fisheries ever fished, not an
# approximation off H_LR's floating-point value.
n_fisheries_fished_owner <- owner_mean_share %>%
  count(File.Number, name = "n.fisheries.fished")

owner_analysis <- owner_analysis %>%
  left_join(n_fisheries_fished_owner, by = "File.Number") %>%
  mutate(is.specialist = n.fisheries.fished == 1)

cat("Owners entering Table 4 -", nrow(owner_analysis),
    ", of which single-fishery specialists -", sum(owner_analysis$is.specialist), "\n")

# ============================================================================
# Table 4 (owner). Baseline versus decomposed regression
# ============================================================================
#
# CV_i = b0 + b1 * H_bar_i + FE(prime.fishery)                (baseline)
# CV_i = g0 + g1 * H_LR_i + g2 * Phi_i + FE(prime.fishery)     (decomposed)
#
# prime.fishery fixed effects absorb a lot of cross-owner variation in
# H_LR, since specialists in one fishery share an H_LR, so g1 in particular
# is identified off thin within-fishery variation and should be read with
# that caveat, not as evidence against the decomposition if it comes out
# small or noisy.
#
# No "controls" are added beyond the fixed effect, matching
# 05_table4_figure3.R's own choice not to specify a control set here.
#
# Main-text models are restricted to multi-fishery owners (is.specialist ==
# FALSE), not the full pooled sample. Single-fishery specialists have Phi
# pinned at exactly 0, a mass point with no within-group spread, so they
# contribute nothing to identifying g2 and instead just sit in the FE group
# for their prime.fishery as an unmoving anchor at (H_LR = 1, Phi = 0),
# exactly the reasoning 05_table4_figure3.R gives for the vessel-level
# exclusion. The pooled sample is kept below as an explicit robustness
# comparison, not dropped.

owner_multi <- owner_analysis %>% filter(!is.specialist)

# vcov = "hetero", not left to fixest's default. owner_multi is
# cross-sectional (one row per owner), so an owner cluster would be
# degenerate, and leaving vcov unset resolves to IID rather than "cluster on
# the first fixed effect," the same codebase-wide fixest-default point
# 05_table4_figure3.R's own comment makes. IID understates the standard
# error whenever the true errors are heteroskedastic, which the
# fixed-effect residuals here have no reason not to be.
model_baseline_owner   <- feols(rev.cv ~ H_bar | prime.fishery, data = owner_multi, vcov = "hetero")
model_decomposed_owner <- feols(rev.cv ~ H_LR + Phi | prime.fishery, data = owner_multi, vcov = "hetero")

# Standardized versions, z-scoring the outcome and regressors before fitting
# so coefficients are comparable in size across models. Scaled within the
# multi-fishery sample, not the pooled one, so the z-scores describe the
# same population the models are fit on.
owner_std <- owner_multi %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

model_baseline_std_owner   <- feols(z.rev.cv ~ z.H_bar | prime.fishery, data = owner_std, vcov = "hetero")
model_decomposed_std_owner <- feols(z.rev.cv ~ z.H_LR + z.Phi | prime.fishery, data = owner_std, vcov = "hetero")

etable(
  model_baseline_owner, model_decomposed_owner, model_baseline_std_owner, model_decomposed_std_owner,
  headers = c("Baseline", "Decomposed", "Baseline (z)", "Decomposed (z)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression_owner.tex"),
  replace = TRUE
)

print(etable(model_baseline_owner, model_decomposed_owner, model_baseline_std_owner, model_decomposed_std_owner))

# Share of b1 that loads onto Phi rather than H_LR, only meaningful for the
# standardized models since that is the scale on which "loads onto Phi" is
# defined.
g2_share_owner <- coef(model_decomposed_std_owner)["z.Phi"] /
  (coef(model_decomposed_std_owner)["z.H_LR"] + coef(model_decomposed_std_owner)["z.Phi"])
cat("Standardized share of the decomposed slope loading onto Phi (owner) -", round(g2_share_owner, 3), "\n")

# ----------------------------------------------------------------------
# Robustness. Same four models on the full pooled sample (specialists and
# multi-fishery owners together), kept as an explicit side-by-side
# comparison rather than silently dropped, matching 05_table4_figure3.R's
# own robustness table, a reviewer will ask what specialists do to the
# estimate here just as much as at the vessel level.
# ----------------------------------------------------------------------

model_baseline_pooled_owner   <- feols(rev.cv ~ H_bar | prime.fishery, data = owner_analysis, vcov = "hetero")
model_decomposed_pooled_owner <- feols(rev.cv ~ H_LR + Phi | prime.fishery, data = owner_analysis, vcov = "hetero")

owner_std_pooled <- owner_analysis %>%
  mutate(across(c(rev.cv, H_bar, H_LR, Phi), ~ as.numeric(scale(.x)), .names = "z.{.col}"))

model_baseline_std_pooled_owner <- feols(
  z.rev.cv ~ z.H_bar | prime.fishery, data = owner_std_pooled, vcov = "hetero"
)
model_decomposed_std_pooled_owner <- feols(
  z.rev.cv ~ z.H_LR + z.Phi | prime.fishery, data = owner_std_pooled, vcov = "hetero"
)

etable(
  model_baseline_pooled_owner, model_decomposed_pooled_owner,
  model_baseline_std_pooled_owner, model_decomposed_std_pooled_owner,
  headers = c("Baseline (pooled)", "Decomposed (pooled)",
              "Baseline (pooled, z)", "Decomposed (pooled, z)"),
  tex = TRUE,
  file = file.path(table_dir, "table4_decomposition_regression_pooled_owner.tex"),
  replace = TRUE
)

cat("Wrote table4_decomposition_regression_owner.tex (multi-fishery owners, main text)",
    "and table4_decomposition_regression_pooled_owner.tex (all owners, robustness)\n")

# ============================================================================
# Figure 3 (owner). Passive buy-and-hold benchmark vs realized CV
# ============================================================================
#
# passive.cv is built the same way rev.cv is, sd/mean of a REVENUE LEVEL
# series over an owner's own active years, not sqrt(w' Sigma w) on log
# returns. Log returns and revenue levels are different statistical
# objects, and comparing rev.cv (levels) against a return-based benchmark
# biases the comparison, an owner with any smooth multi-year revenue trend,
# from inflation, growth, anything, inflates CV-of-levels a lot while
# barely moving SD-of-log-returns, so "realized CV exceeds passive CV"
# could partly just reflect that mismatch rather than real reallocation
# risk. This version compares like with like, the same point
# 05_table4_figure3.R's own comment makes for vessels.
#
# For owner i in year t, restricted to i's own active years (the same
# window rev.cv is computed over), passive_revenue_it = sum_j w_ij *
# fleet_mean_revenue_owner_jt, what i would have earned that year by
# holding its own fixed long-run weights (owner_mean_share, the same
# weights that define H_LR) against what the fleet of owners as a whole
# actually earned in each of i's held fisheries that year. passive.cv_i =
# sd/mean of that series, same formula, same window as rev.cv, only the
# revenue-generating process (actual vs counterfactual-fixed-weights)
# differs. This also matches Chapter 2's own CV definition (levels-based),
# and keeps this owner-level cut comparable to the vessel-level one rather
# than quietly using a different metric.
#
# Reading the figure, same as the vessel-level version, a point above the
# 45-degree line experienced more revenue instability than holding its own
# long-run portfolio fixed would have, given what the fleet of owners
# actually earned, i.e. real reallocation risk (here, potentially INCLUDING
# reallocation across the owner's own vessels, not just within one vessel's
# own fisheries), not just an accounting artifact of the H_bar/Phi
# construction. A point below hedged risk below that passive benchmark.
#
# fleet_mean_revenue_owner is missing for a (fishery, year) with zero
# fleet-wide owner activity that year, filled with 0 here, consistent with
# how forgone.value/fished.value already treat that case in
# 01_build_panel.R. Weights in owner_mean_share already sum to 1 for every
# owner by construction (shares sum to 1 within any year, so their
# owner-level means do too), so no renormalization or fishery-eligibility
# filter is needed here.
#
# Single-fishery specialists (is.specialist, built above for Table 4) are
# excluded from the main scatter and shown in their own appendix figure
# instead. A specialist's passive.cv is built off the fleet mean of the one
# fishery it holds, so any gap to its own rev.cv is pure idiosyncratic
# noise around that fleet mean, not reallocation, there is nothing to
# reallocate. Mixing that noise floor into the main-text figure risked
# reading the 45-degree-line pattern as evidence of reallocation risk
# alone, when part of it predates any behavior at all, so the two
# populations get their own plots instead of one plot with two colors,
# exactly as 05_table4_figure3.R does at the vessel level.

if (!exists("fleet_mean_revenue_owner") || !exists("owner_mean_share") || !exists("owner_year")) load(panel_path)

active_owner_years <- owner_year %>%
  filter(owner.year.rev > 0) %>%
  select(File.Number, Batch.Year)

passive_series_owner <- owner_mean_share %>%
  semi_join(owner_analysis, by = "File.Number") %>%
  inner_join(active_owner_years, by = "File.Number", relationship = "many-to-many") %>%
  left_join(fleet_mean_revenue_owner %>% select(Batch.Year, Fishery, fleet_mean_revenue),
            by = c("Batch.Year", "Fishery")) %>%
  mutate(fleet_mean_revenue = replace_na(fleet_mean_revenue, 0)) %>%
  group_by(File.Number, Batch.Year) %>%
  summarise(passive_revenue = sum(mean.share.fishery * fleet_mean_revenue), .groups = "drop")

passive_benchmark_owner <- passive_series_owner %>%
  group_by(File.Number) %>%
  summarise(
    n.years.passive = n(),
    passive.cv = sd(passive_revenue) / mean(passive_revenue),
    .groups = "drop"
  )

fig3_data_owner <- owner_analysis %>%
  select(File.Number, rev.cv, H_bar, H_LR, Phi, is.specialist) %>%
  inner_join(passive_benchmark_owner, by = "File.Number") %>%
  filter(is.finite(passive.cv))

cat("Owners with a computable passive benchmark -", nrow(fig3_data_owner),
    ", of which single-fishery specialists -", sum(fig3_data_owner$is.specialist), "\n")

passive_benchmark_scatter_owner <- function(data, subtitle) {
  data %>%
    ggplot(aes(x = passive.cv, y = rev.cv)) +
    geom_point(alpha = 0.15, size = 0.8, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "firebrick") +
    labs(
      title = "Realized revenue CV versus a passive buy-and-hold benchmark (owner)",
      subtitle = subtitle,
      x = "Passive benchmark CV (buy-and-hold, owner's own weights)",
      y = "Realized revenue CV"
    ) +
    theme_minimal()
}

figure3_owner <- fig3_data_owner %>%
  filter(!is.specialist) %>%
  # Full benchmark construction and the 45-degree reference belong in the
  # caption, this figure's width can't hold the multi-sentence version.
  passive_benchmark_scatter_owner("Multi-fishery owners only")

ggsave(file.path(figure_dir, "figure3_passive_benchmark_owner.png"),
       figure3_owner, width = 7, height = 6, dpi = 300)

cat("Wrote figure3_passive_benchmark_owner.png (multi-fishery owners)\n")

# ----------------------------------------------------------------------
# Appendix. Same benchmark, single-fishery specialists only
# ----------------------------------------------------------------------
#
# Phi = 0 for every point here by construction, so this is not a
# reallocation-risk figure the way the main-text one is, it is a check on
# how noisy the passive benchmark itself is when an owner's whole portfolio
# is one fishery. That is why it belongs in the appendix rather than the
# main text, exactly as at the vessel level.

figure3_appendix_owner <- fig3_data_owner %>%
  filter(is.specialist) %>%
  # Why a gap here is idiosyncratic noise rather than reallocation risk
  # (Phi = 0 by construction) belongs in the caption, not this subtitle.
  passive_benchmark_scatter_owner("Single-fishery specialists only")

ggsave(file.path(figure_dir, "figure3_appendix_specialists_owner.png"),
       figure3_appendix_owner, width = 7, height = 6, dpi = 300)

cat("Wrote figure3_appendix_specialists_owner.png (single-fishery specialists, appendix)\n")

# ============================================================================
# Figure 3b (owner). Gap between realized and passive CV, binned by Phi
# ============================================================================
#
# gap_i = rev.cv_i - passive.cv_i, the vertical distance a point in Figure 3
# sits above (or below) the 45-degree line. Binning by Phi and plotting the
# mean gap per bin is the cleanest single piece of evidence in this pipeline
# that reallocation itself is associated with added revenue instability,
# cleaner than Table 4's regression coefficient because it imposes no
# functional form and does not depend on how much of the FE structure is
# absorbing cross-owner variation.
#
# Single-fishery specialists are shown as their own point, not folded into
# the lowest Phi bin. Phi is exactly 0 for every specialist, a mass point,
# so binning them together with owners that have small positive Phi would
# either swallow the specialist point whole or, if the specialist mass is
# bigger than one bin's worth, spill zeros into the neighboring bin and
# quietly pull its mean down. Keeping specialists as their own category
# avoids both problems and matches the point this figure exists to make,
# specialists sit apart from the reallocation relationship, not at its low
# end, since part of the gap is idiosyncratic noise around the fleet mean
# that has nothing to do with reallocation.

fig3b_data_owner <- fig3_data_owner %>%
  mutate(gap = rev.cv - passive.cv)

specialist_summary_owner <- fig3b_data_owner %>%
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

multi_summary_owner <- fig3b_data_owner %>%
  filter(!is.specialist) %>%
  mutate(phi.bin = ntile(Phi, N_GAP_BINS)) %>%
  group_by(phi.bin) %>%
  summarise(
    n = n(),
    mean.Phi = mean(Phi),
    mean.gap = mean(gap),
    se.gap = sd(gap) / sqrt(n),
    .groups = "drop"
  ) %>%
  # bin.label/bin.order built after summarise, once phi.bin is one row per
  # group, referencing it inside summarise() itself returns the full
  # per-row vector for that group (not a scalar), which is what raised the
  # "must be size 1" error in the vessel-level version of this script,
  # group_by does not collapse its own key inside summarise unless you
  # explicitly reduce it.
  mutate(bin.label = paste0("Q", phi.bin), bin.order = phi.bin) %>%
  select(-phi.bin)

gap_by_phi_owner <- bind_rows(specialist_summary_owner, multi_summary_owner) %>%
  mutate(bin.label = fct_reorder(bin.label, bin.order),
         is.specialist.bin = bin.order == 0)

print(gap_by_phi_owner)

figure3b_owner <- gap_by_phi_owner %>%
  ggplot(aes(x = bin.label, y = mean.gap, color = is.specialist.bin)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = mean.gap - 1.96 * se.gap, ymax = mean.gap + 1.96 * se.gap), width = 0.2) +
  geom_line(
    data = gap_by_phi_owner %>% filter(!is.specialist.bin),
    aes(x = bin.label, y = mean.gap, group = 1),
    color = "steelblue", inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "gray40", "FALSE" = "steelblue"), guide = "none") +
  labs(
    # Bin construction and the 95% CI error bars belong in the caption.
    title = "Gap between realized and passive-benchmark CV (owner)",
    subtitle = "By reallocation intensity (Phi)",
    x = "Reallocation intensity (Phi), specialists then increasing bins",
    y = "Mean gap (realized CV − passive CV)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure3b_gap_by_phi_owner.png"),
       figure3b_owner, width = 7, height = 5, dpi = 300)

cat("Wrote figure3b_gap_by_phi_owner.png\n")
