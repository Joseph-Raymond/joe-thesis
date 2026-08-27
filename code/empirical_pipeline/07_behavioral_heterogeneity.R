# Chapter 3 empirical pipeline, Chapter3_outline.md Section 6
#
# Table 7.  Baseline CV-on-H_bar regression estimated separately by
#           within-season-turnover type, compared with Chapter 2's simulated
#           regime slopes.
# Table 8.  Same split, but type is classified on each vessel's first half
#           of active years and the slope is estimated on its second half,
#           which is what makes this a test rather than a sort.
# Figure 8. Estimated slope by type, next to Chapter 2's simulated slopes.
# Figure 9 [appendix]. The same exercise sorted on Phi instead, shown once
#           to check whether the Table 7/Figure 8 gap is just the generic
#           mechanical artifact any HHI-component sort would produce.
#           Checked against a real run, it is not, the Phi sort produces
#           only a small, overlapping-interval gap versus the large,
#           robust gap from the within-season classifier, so this reads as
#           corroboration for Table 7 rather than a caution about it.
#
# Skips Table 9 [maybe] (within-gear-class matched estimation) and the "if I
# have time" interactions/quantile-regression/causal-forest block, both
# explicitly optional in Chapter3_outline.md.
#
# Per chapter3_plan.md Section 9.2 ("R3's behavioral typing is... circular
# for two of its three classifiers"), Phi and annual share switching are both
# mechanical components of the same H_bar/CV objects being regressed, so
# sorting on either and finding a steeper slope in the high group is close
# to guaranteed by construction, independent of whether Chapter 2's
# behavioral story is true. Only the within-season classifier from Section 5
# is measured at a different frequency than the annual outcome and actually
# escapes that circularity, which is why it is the ONLY classifier used for
# Table 7/8/Figure 8, with the Phi-sorted version demoted to Figure 9's
# one-time illustration of what the circular version would have shown.
#
# The regression here is log(rev.cv) ~ H_bar, not the levels specification
# Table 4 uses. Table 4 runs in levels because H_bar = H_LR + Phi is
# additive and only decomposes in levels, chapter3_plan.md Section 9.2. That
# reason does not apply here, nothing is being decomposed, and Chapter 2's
# own regime slopes (0.74/0.75/0.78/0.87 in writeup/simulation_results.tex)
# come from log(CV) ~ H_bar. Matching that specification is what makes
# Figure 8's comparison to Chapter 2 apples to apples.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_summary") || !exists("vessel_share_panel") || !exists("vessel_year")) load(panel_path)

within_season_path <- file.path(intermediate_dir, "ch3_within_season.rdata")
if (!exists("switching_by_vessel_year")) load(within_season_path)

active_vessel_years <- vessel_year %>%
  filter(vessel.year.rev > 0) %>%
  select(Vessel.ADFG.Number, Batch.Year, vessel.year.rev)

# ============================================================================
# 1. Type classifier, within-season target switching averaged over a
#    vessel's years
# ============================================================================
#
# Matches chapter3_plan.md Section 9.3 ("averaged over a vessel's years to
# form the R3 classifier") exactly. Median split into two types rather than
# three, since Chapter 2's comparison benchmark here is the two-way
# responsive-versus-passive contrast (0.87 versus 0.78), not the three
# separate BH/QAC/Flex regime slopes individually.

vessel_switching <- switching_by_vessel_year %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(n.years.switching = n(), within.season.switching = mean(weekly.switching), .groups = "drop")

vessel_type <- vessel_switching %>%
  mutate(vessel.type = if_else(within.season.switching > median(within.season.switching),
                                "High turnover", "Low turnover"))

cat("Vessels with a within-season target-switching classifier:", nrow(vessel_type),
    ", median switching used as the split:", round(median(vessel_switching$within.season.switching), 3), "\n")

# ============================================================================
# 2. Table 7. Full-panel slope by type
# ============================================================================

table7_data <- vessel_summary %>%
  filter(meets.min.years, is.finite(rev.cv), rev.cv > 0) %>%
  inner_join(vessel_type, by = "Vessel.ADFG.Number")

cat("Vessels entering Table 7:", nrow(table7_data),
    ", High turnover:", sum(table7_data$vessel.type == "High turnover"), "\n")

# No-FE versions are what Figure 8 plots against Chapter 2's unconditional
# simulated slopes, prime.fishery-FE versions are kept alongside for
# consistency with how the rest of the chapter (Table 4, Table 6) always
# controls for prime.fishery. vcov = "hetero" throughout, not left to
# fixest's default, table7_data is cross-sectional (one row per vessel), so
# a vessel cluster would be degenerate (one observation per cluster, no
# different from robust), and fixest's actual default with the FE models
# would cluster on prime.fishery instead, a small handful of fishery
# classes, too few clusters for reliable inference.
model_low_raw   <- feols(log(rev.cv) ~ H_bar, data = filter(table7_data, vessel.type == "Low turnover"), vcov = "hetero")
model_high_raw  <- feols(log(rev.cv) ~ H_bar, data = filter(table7_data, vessel.type == "High turnover"), vcov = "hetero")
model_low_fe    <- feols(log(rev.cv) ~ H_bar | prime.fishery, data = filter(table7_data, vessel.type == "Low turnover"), vcov = "hetero")
model_high_fe   <- feols(log(rev.cv) ~ H_bar | prime.fishery, data = filter(table7_data, vessel.type == "High turnover"), vcov = "hetero")

# Interaction model, the sharper version of the same test and the one
# reported as the headline Table 7 statistic. The median split above is
# useful for Figure 8's visual and for Table 8's split-sample design, but it
# discards information and imposes an arbitrary cutpoint. Interacting
# H_bar with the continuous switching measure directly tests whether the
# CV-on-H_bar slope RISES with within-season turnover, which is Chapter 2's
# actual prediction (the slope is a composition-weighted mixture that moves
# with how responsive the population is), without first collapsing
# switching into two bins.
model_interaction <- feols(log(rev.cv) ~ H_bar * within.season.switching | prime.fishery,
                            data = table7_data, vcov = "hetero")

cat("Interaction coefficient (H_bar x within.season.switching), the headline Table 7 statistic, ",
    "a positive value says the slope steepens with turnover intensity as Chapter 2 predicts, ",
    round(coef(model_interaction)["H_bar:within.season.switching"], 4), "\n")

# dict relabels within.season.switching for the printed/exported table only.
table7_dict <- c(within.season.switching = "Target switching")

etable(
  model_low_raw, model_high_raw, model_low_fe, model_high_fe, model_interaction,
  headers = c("Low turnover", "High turnover", "Low turnover (FE)", "High turnover (FE)", "Interaction"),
  dict = table7_dict,
  tex = TRUE,
  file = file.path(table_dir, "table7_slope_by_turnover_type.tex"),
  replace = TRUE
)

print(etable(model_low_raw, model_high_raw, model_low_fe, model_high_fe, model_interaction, dict = table7_dict))

cat("Wrote table7_slope_by_turnover_type.tex\n")

# ----------------------------------------------------------------------
# Robustness. Re-classify type on the per-transition-normalized switching
# measure instead of the raw sum, mirroring Table 7's full five-column
# structure (raw split, prime.fishery-FE split, continuous interaction)
# rather than just the two no-FE models, written out as its own table so
# the coefficients backing the "does not survive reclassifying type on a
# per-transition-normalized switching measure" claim in the writeup are
# quotable rather than console-only. This checks the median-split result is
# not an artifact of raw weekly.switching being mechanically larger for
# vessels that simply fish more weeks (see the comment above
# weekly.switching.per.transition in 06_within_season_reallocation.R).
# ----------------------------------------------------------------------

vessel_type_normalized <- switching_by_vessel_year %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(within.season.switching.norm = mean(weekly.switching.per.transition), .groups = "drop") %>%
  mutate(vessel.type.norm = if_else(within.season.switching.norm > median(within.season.switching.norm),
                                     "High turnover", "Low turnover"))

table7_data_norm <- vessel_summary %>%
  filter(meets.min.years, is.finite(rev.cv), rev.cv > 0) %>%
  inner_join(vessel_type_normalized, by = "Vessel.ADFG.Number")

model_low_raw_norm  <- feols(log(rev.cv) ~ H_bar, data = filter(table7_data_norm, vessel.type.norm == "Low turnover"), vcov = "hetero")
model_high_raw_norm <- feols(log(rev.cv) ~ H_bar, data = filter(table7_data_norm, vessel.type.norm == "High turnover"), vcov = "hetero")
model_low_fe_norm    <- feols(log(rev.cv) ~ H_bar | prime.fishery, data = filter(table7_data_norm, vessel.type.norm == "Low turnover"), vcov = "hetero")
model_high_fe_norm   <- feols(log(rev.cv) ~ H_bar | prime.fishery, data = filter(table7_data_norm, vessel.type.norm == "High turnover"), vcov = "hetero")

cat("Robustness, per-transition-normalized switching classifier, slope Low turnover:",
    round(coef(model_low_raw_norm)["H_bar"], 4), " High turnover:",
    round(coef(model_high_raw_norm)["H_bar"], 4),
    " (raw-switching classifier gave Low:", round(coef(model_low_raw)["H_bar"], 4),
    " High:", round(coef(model_high_raw)["H_bar"], 4), ")\n")

# Interaction model, normalized-measure analogue of model_interaction above,
# within.season.switching.norm (per-transition intensity) in place of the
# raw sum, same full table7_data_norm sample (not split by type). Checks
# whether the headline interaction result also holds up under the
# normalized measure, not just the median-split slopes.
model_interaction_norm <- feols(log(rev.cv) ~ H_bar * within.season.switching.norm | prime.fishery,
                                 data = table7_data_norm, vcov = "hetero")

cat("Interaction coefficient (H_bar x within.season.switching.norm), normalized-measure analogue of the ",
    "headline Table 7 statistic, ",
    round(coef(model_interaction_norm)["H_bar:within.season.switching.norm"], 4), "\n")

# dict relabels within.season.switching.norm for the printed/exported table
# only, the column itself keeps its name so nothing else in this script
# needs to change if the display label is tweaked again later.
table7_norm_dict <- c(within.season.switching.norm = "Target switching (per-transition)")

etable(
  model_low_raw_norm, model_high_raw_norm, model_low_fe_norm, model_high_fe_norm, model_interaction_norm,
  headers = c("Low turnover", "High turnover", "Low turnover (FE)", "High turnover (FE)", "Interaction"),
  dict = table7_norm_dict,
  tex = TRUE,
  file = file.path(table_dir, "table7_slope_by_turnover_type_normalized.tex"),
  replace = TRUE
)

print(etable(model_low_raw_norm, model_high_raw_norm, model_low_fe_norm, model_high_fe_norm, model_interaction_norm,
             dict = table7_norm_dict))

cat("Wrote table7_slope_by_turnover_type_normalized.tex\n")

# ----------------------------------------------------------------------
# Diagnostic. H_bar distribution by turnover type, both classifiers side
# by side. Not part of chapter3_outline.md, added to help explain why the
# Low turnover slope is so unstable above (raw R^2 = 0.001, sign flips
# with FE choice under the normalized classifier). A group whose H_bar
# barely varies gives a regression slope almost no leverage to estimate
# from, so a handful of vessels near the tails can swing the point
# estimate, which is consistent with the instability already seen in both
# Table 7 versions. Density rather than a histogram, the two groups have
# very different N (Low ~4,500 vs High ~6,300 in both classifiers), so a
# count-based histogram would visually exaggerate the size difference
# rather than the shape difference this is meant to show.
# ----------------------------------------------------------------------

hbar_by_type <- bind_rows(
  table7_data %>% transmute(H_bar, vessel.type, classifier = "Raw classifier"),
  table7_data_norm %>% transmute(H_bar, vessel.type = vessel.type.norm, classifier = "Normalized classifier")
)

figure_hbar_by_type <- hbar_by_type %>%
  ggplot(aes(x = H_bar, fill = vessel.type, color = vessel.type)) +
  geom_density(alpha = 0.35, linewidth = 0.6) +
  facet_wrap(~ classifier) +
  scale_fill_manual(values = c("Low turnover" = "steelblue", "High turnover" = "firebrick")) +
  scale_color_manual(values = c("Low turnover" = "steelblue", "High turnover" = "firebrick")) +
  labs(
    title = "H_bar distribution by turnover type",
    subtitle = "Diagnostic for Table 7's Low-turnover instability, not an outline figure",
    x = expression(bar(H)), y = "Density", fill = NULL, color = NULL
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "diagnostic_hbar_by_turnover_type.png"),
       figure_hbar_by_type, width = 9, height = 5, dpi = 300)

cat("Wrote diagnostic_hbar_by_turnover_type.png\n")

# ----------------------------------------------------------------------
# Diagnostic. A first pass at this (comparing n.fisheries.fished against a
# raw distinct-Fishery count off 06_'s own reload, vessel_fisheries_06)
# found 13 vessels where the counts disagreed, but every one turned out to
# be a $0-value ticket (revenue == 0, fished == FALSE in
# vessel_fishery_year for the "extra" fishery in every row), not a real
# second fishery. A $0-revenue fishery is share-INERT, share = revenue /
# week.revenue = 0 / anything = 0 in every week it appears, so it cannot
# move weekly.switching no matter how many weeks it shows up in. That
# raw-count comparison was answering an adjacent question (does
# n.fisheries.fished undercount vessels with $0-value/bycatch-style
# tickets, yes, confirms the concern raised earlier about season_windows)
# rather than the one that actually motivated it. The direct test is
# whether a TRUE specialist's own switching measure, already computed,
# already share-based, is ever actually positive.
# ----------------------------------------------------------------------

if (!exists("vessel_mean_share") || !exists("vessel_year")) load(panel_path)

n_fisheries_fished <- vessel_mean_share %>%
  count(Vessel.ADFG.Number, name = "n.fisheries.fished")

specialist_switching <- n_fisheries_fished %>%
  filter(n.fisheries.fished == 1) %>%
  inner_join(switching_by_vessel_year, by = "Vessel.ADFG.Number") %>%
  filter(weekly.switching > 0)

cat("Vessel-years where 01_build_panel.R counts the vessel as a lifetime single-fishery",
    "specialist but 06_'s own switching measure is positive that year:", nrow(specialist_switching),
    " across", n_distinct(specialist_switching$Vessel.ADFG.Number), "distinct vessels\n")

# For each flagged vessel-year, pull that SAME vessel-year's total revenue
# across every fishery from vessel_year (01_build_panel.R's object). If it
# nets to <= 0 that year, active_vessel_years (vessel_year.rev > 0) drops
# the WHOLE YEAR from vessel_share_panel before n.fisheries.fished is ever
# computed, so a fishery with genuine per-week positive revenue that year
# would never register with 01_ even though it is real activity, a
# different, whole-year version of the same annual-netting problem rather
# than a per-fishery one. If vessel.year.rev is clearly positive instead,
# this points at something else, worth chasing next (vessel ID cleaning
# divergence between the two scripts' independently duplicated Section 2
# cleaning steps would be the next thing to check).
if (nrow(specialist_switching) > 0) {
  specialist_switching_rev <- specialist_switching %>%
    left_join(vessel_year %>% select(Vessel.ADFG.Number, Batch.Year, vessel.year.rev),
              by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
    select(Vessel.ADFG.Number, Batch.Year, weekly.switching, n.active.weeks, vessel.year.rev)

  # Summary first, over all flagged vessel-years, not just the 10 printed
  # below, share with vessel.year.rev <= 0 is the direct test of the
  # whole-year-nets-to-zero hypothesis above.
  cat("Of those", nrow(specialist_switching_rev), "flagged vessel-years, share with",
      "vessel.year.rev <= 0 that year:",
      round(mean(specialist_switching_rev$vessel.year.rev <= 0), 4), "\n")

  # as.data.frame() so the console print does not truncate columns to fit
  # width, a tibble print silently dropped vessel.year.rev off the right
  # edge on the last run, the one column this diagnostic exists to show.
  print(as.data.frame(specialist_switching_rev %>% arrange(desc(weekly.switching)) %>% head(10)))
}

# ----------------------------------------------------------------------
# Diagnostic, refined. vessel.year.rev above is the vessel's TOTAL revenue
# across every fishery that year, and it came back clearly positive for
# every flagged vessel-year (checked directly against the printed sample,
# $2,008 to $77,884), ruling out the whole-year-nets-to-zero hypothesis.
# The right test needs the SPECIFIC extra fishery's OWN revenue in THAT
# flagged year, not the vessel-year total, the original per-fishery
# hypothesis (01_'s fished = revenue > 0 gate) but scoped down to the
# vessel-years that actually produced switching, rather than the
# fleet-wide, all-years-pooled scope the first diagnostic used, which is
# what surfaced 13 unrelated, switching-inert vessels instead of these.
# ----------------------------------------------------------------------

if (!exists("vessel_fisheries_06")) load(within_season_path)

extra_fishery_by_vessel <- specialist_switching %>%
  distinct(Vessel.ADFG.Number) %>%
  left_join(vessel_mean_share %>% group_by(Vessel.ADFG.Number) %>% summarise(fisheries.01 = list(Fishery), .groups = "drop"),
            by = "Vessel.ADFG.Number") %>%
  left_join(vessel_fisheries_06 %>% group_by(Vessel.ADFG.Number) %>% summarise(fisheries.06 = list(Fishery), .groups = "drop"),
            by = "Vessel.ADFG.Number") %>%
  mutate(extra.fishery = map2(fisheries.06, fisheries.01, setdiff)) %>%
  select(Vessel.ADFG.Number, extra.fishery) %>%
  unnest(extra.fishery)

# Left-joined per flagged (vessel, year), not per vessel, since a vessel
# can have more than one candidate extra fishery across its whole panel
# but only one is necessarily the one active in a given flagged year.
# !is.na(fished) after the join keeps only the extra fishery actually
# held/fished (per 01_'s reckoning) THAT specific year, dropping
# candidates irrelevant to that particular flagged vessel-year.
extra_fishery_year_revenue <- specialist_switching %>%
  distinct(Vessel.ADFG.Number, Batch.Year) %>%
  inner_join(extra_fishery_by_vessel, by = "Vessel.ADFG.Number") %>%
  left_join(
    vessel_fishery_year %>% select(Vessel.ADFG.Number, Batch.Year, Fishery, revenue, held, fished),
    by = c("Vessel.ADFG.Number", "Batch.Year", "extra.fishery" = "Fishery")
  ) %>%
  filter(!is.na(fished))

cat("Of", nrow(extra_fishery_year_revenue), "flagged (vessel, year, extra fishery) rows, share",
    "with that SPECIFIC fishery's revenue <= 0 in that SPECIFIC year:",
    round(mean(extra_fishery_year_revenue$revenue <= 0), 4), "\n")

print(as.data.frame(extra_fishery_year_revenue %>% arrange(Vessel.ADFG.Number, Batch.Year) %>% head(15)))

# ----------------------------------------------------------------------
# Robustness. Same normalized-classifier structure as the full-sample
# version above, but the median split itself is now computed WITHIN the
# eligible sample (meets.min.years, finite positive rev.cv, multi-fishery)
# rather than on the full switching-measure population and filtered down
# afterward. The full-sample version's median is taken over every vessel
# with a computable switching measure, including specialists and vessels
# that never clear meets.min.years and so never enter the regression
# either way, so its Low/High split does not describe the sample actually
# being tested. Restricting first and then splitting on THAT restricted
# sample's own median instead answers "how does log(rev.cv) ~ H_bar differ
# for the more versus less reallocating HALF of the vessels actually
# eligible for this regression," the comparison Table 7/Figure 8 are meant
# to make. is.specialist exclusion (n.fisheries.fished > 1) is applied
# before the median is taken, same as before, so a specialist can no
# longer end up on either side of this particular split by construction,
# though the diagnostic above still matters for anything elsewhere in the
# chapter that relies on n.fisheries.fished being accurate.
# ----------------------------------------------------------------------

eligible_vessels <- vessel_summary %>%
  filter(meets.min.years, is.finite(rev.cv), rev.cv > 0) %>%
  left_join(n_fisheries_fished, by = "Vessel.ADFG.Number") %>%
  filter(n.fisheries.fished > 1) %>%
  inner_join(
    switching_by_vessel_year %>%
      group_by(Vessel.ADFG.Number) %>%
      summarise(within.season.switching.norm = mean(weekly.switching.per.transition), .groups = "drop"),
    by = "Vessel.ADFG.Number"
  )

table7_data_norm_multi <- eligible_vessels %>%
  mutate(vessel.type.norm = if_else(within.season.switching.norm > median(within.season.switching.norm),
                                     "High turnover", "Low turnover"))

cat("Table 7 (normalized), median split on the eligible (multi-fishery, meets.min.years) sample:",
    nrow(table7_data_norm_multi), " eligible median switching:",
    round(median(table7_data_norm_multi$within.season.switching.norm), 4),
    " High turnover:", sum(table7_data_norm_multi$vessel.type.norm == "High turnover"), "\n")

model_low_raw_norm_multi  <- feols(log(rev.cv) ~ H_bar, data = filter(table7_data_norm_multi, vessel.type.norm == "Low turnover"), vcov = "hetero")
model_high_raw_norm_multi <- feols(log(rev.cv) ~ H_bar, data = filter(table7_data_norm_multi, vessel.type.norm == "High turnover"), vcov = "hetero")
model_low_fe_norm_multi   <- feols(log(rev.cv) ~ H_bar | prime.fishery, data = filter(table7_data_norm_multi, vessel.type.norm == "Low turnover"), vcov = "hetero")
model_high_fe_norm_multi  <- feols(log(rev.cv) ~ H_bar | prime.fishery, data = filter(table7_data_norm_multi, vessel.type.norm == "High turnover"), vcov = "hetero")

model_interaction_norm_multi <- feols(log(rev.cv) ~ H_bar * within.season.switching.norm | prime.fishery,
                                       data = table7_data_norm_multi, vcov = "hetero")

cat("Eligible-sample-median slope, Low turnover:", round(coef(model_low_raw_norm_multi)["H_bar"], 4),
    " High turnover:", round(coef(model_high_raw_norm_multi)["H_bar"], 4), "\n")

etable(
  model_low_raw_norm_multi, model_high_raw_norm_multi, model_low_fe_norm_multi, model_high_fe_norm_multi, model_interaction_norm_multi,
  headers = c("Low turnover", "High turnover", "Low turnover (FE)", "High turnover (FE)", "Interaction"),
  dict = table7_norm_dict,
  tex = TRUE,
  file = file.path(table_dir, "table7_slope_by_turnover_type_normalized_multifishery.tex"),
  replace = TRUE
)

print(etable(model_low_raw_norm_multi, model_high_raw_norm_multi, model_low_fe_norm_multi, model_high_fe_norm_multi, model_interaction_norm_multi,
             dict = table7_norm_dict))

cat("Wrote table7_slope_by_turnover_type_normalized_multifishery.tex\n")

# ============================================================================
# 3. Table 8. Split-sample robustness, classify on first half, test on
#    second half
# ============================================================================
#
# Severs the mechanical link between classifier and outcome, chapter3_plan.md
# Section 9.3's key robustness check for this section. Type comes from
# within-season target switching in each vessel's OWN first half of active years, the
# regression is estimated on H_bar/rev.cv built from ONLY that vessel's
# second half, computed here from scratch (not vessel_summary, which is
# whole-panel) the same way vessel_summary itself is built, sd/mean of
# vessel-year revenue for CV and mean of within-year sum-of-squared-shares
# for H_bar, just restricted to the second-half years.

vessel_year_ordinal <- vessel_share_panel %>%
  distinct(Vessel.ADFG.Number, Batch.Year) %>%
  arrange(Vessel.ADFG.Number, Batch.Year) %>%
  group_by(Vessel.ADFG.Number) %>%
  mutate(
    year.rank = row_number(),
    n.years   = n(),
    half      = if_else(year.rank <= ceiling(n.years / 2), "first", "second")
  ) %>%
  ungroup()

first_half_years  <- vessel_year_ordinal %>% filter(half == "first")  %>% select(Vessel.ADFG.Number, Batch.Year)
second_half_years <- vessel_year_ordinal %>% filter(half == "second") %>% select(Vessel.ADFG.Number, Batch.Year)

vessel_type_first_half <- switching_by_vessel_year %>%
  semi_join(first_half_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(within.season.switching.first.half = mean(weekly.switching), .groups = "drop") %>%
  mutate(vessel.type = if_else(
    within.season.switching.first.half > median(within.season.switching.first.half),
    "High turnover", "Low turnover"
  ))

# MIN_SECOND_HALF_YEARS is deliberately lower than MIN_ACTIVE_YEARS (5), a
# vessel's second half is roughly half its panel by construction, so a
# proportionally smaller minimum is the natural analogue, the same logic
# 00_setup.R already uses for MIN_ACTIVE_YEARS_PERIOD. CHECK this once run
# on real data.
MIN_SECOND_HALF_YEARS <- 3

vessel_summary_second_half <- vessel_share_panel %>%
  semi_join(second_half_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(
    n.years.second.half = n_distinct(Batch.Year),
    H_bar = mean(tapply(share, Batch.Year, function(s) sum(s^2))),
    .groups = "drop"
  ) %>%
  left_join(
    active_vessel_years %>%
      semi_join(second_half_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
      group_by(Vessel.ADFG.Number) %>%
      summarise(rev.cv = sd(vessel.year.rev) / mean(vessel.year.rev), .groups = "drop"),
    by = "Vessel.ADFG.Number"
  )

table8_data <- vessel_summary_second_half %>%
  filter(n.years.second.half >= MIN_SECOND_HALF_YEARS, is.finite(rev.cv), rev.cv > 0) %>%
  inner_join(vessel_type_first_half, by = "Vessel.ADFG.Number")

cat("Vessels entering Table 8:", nrow(table8_data),
    ", High turnover:", sum(table8_data$vessel.type == "High turnover"), "\n")

# vcov = "hetero", not the iid OLS default, for the same reason as Table 7,
# these feed Figure 8's confidence intervals and iid SEs on cross-sectional
# data understate them.
model_split_low  <- feols(log(rev.cv) ~ H_bar, data = filter(table8_data, vessel.type == "Low turnover"), vcov = "hetero")
model_split_high <- feols(log(rev.cv) ~ H_bar, data = filter(table8_data, vessel.type == "High turnover"), vcov = "hetero")

etable(
  model_split_low, model_split_high,
  headers = c("Low turnover (classified on 1st half)", "High turnover (classified on 1st half)"),
  tex = TRUE,
  file = file.path(table_dir, "table8_split_sample_slope_by_type.tex"),
  replace = TRUE
)

print(etable(model_split_low, model_split_high))

cat("Wrote table8_split_sample_slope_by_type.tex\n")

# Robustness, a higher second-half-years floor. chapter3_plan.md Section
# 9.2 flags CV as a noisy, small-sample-biased ratio, and that bites
# hardest here since table8_data's rev.cv can be built on as few as
# MIN_SECOND_HALF_YEARS = 3 years. Refit at a higher floor (5, matching
# MIN_ACTIVE_YEARS) and print alongside so the reader can see whether the
# type ordering survives a less noisy CV estimate, not written as its own
# table since it is a diagnostic on the floor choice, not a new result.
MIN_SECOND_HALF_YEARS_STRICT <- 5

table8_data_strict <- vessel_summary_second_half %>%
  filter(n.years.second.half >= MIN_SECOND_HALF_YEARS_STRICT, is.finite(rev.cv), rev.cv > 0) %>%
  inner_join(vessel_type_first_half, by = "Vessel.ADFG.Number")

model_split_low_strict  <- feols(log(rev.cv) ~ H_bar, data = filter(table8_data_strict, vessel.type == "Low turnover"), vcov = "hetero")
model_split_high_strict <- feols(log(rev.cv) ~ H_bar, data = filter(table8_data_strict, vessel.type == "High turnover"), vcov = "hetero")

cat("Robustness, MIN_SECOND_HALF_YEARS =", MIN_SECOND_HALF_YEARS_STRICT, "instead of", MIN_SECOND_HALF_YEARS,
    ", vessels:", nrow(table8_data_strict),
    ", slope Low:", round(coef(model_split_low_strict)["H_bar"], 4),
    " High:", round(coef(model_split_high_strict)["H_bar"], 4),
    " (floor =", MIN_SECOND_HALF_YEARS, "gave Low:", round(coef(model_split_low)["H_bar"], 4),
    " High:", round(coef(model_split_high)["H_bar"], 4), ")\n")

# ============================================================================
# 4. Figure 8. Empirical slopes next to Chapter 2's simulated slopes
# ============================================================================
#
# Chapter 2 reference slopes are read directly off writeup/simulation_results.tex
# (log(CV) on H_bar, per-regime OLS), not recomputed here, since this script
# has no access to Chapter 2's simulation code or its results/regression.pkl.
# CHECK these four numbers against results/regression.pkl if it has been
# regenerated since writeup/simulation_results.tex was last built, the
# CLAUDE.md note "QAC uses c in {1, 4}; regression slopes will update after
# next simulation run" means these are not guaranteed permanent.
chapter2_slopes <- tibble(
  regime = c("QAC (c=1)", "QAC (c=4)", "BH", "Flex"),
  slope  = c(0.74, 0.75, 0.78, 0.87)
)

empirical_slopes <- bind_rows(
  tibble(spec = "Table 7 (full panel)",   vessel.type = "Low turnover",
         slope = coef(model_low_raw)["H_bar"],   se = se(model_low_raw)["H_bar"]),
  tibble(spec = "Table 7 (full panel)",   vessel.type = "High turnover",
         slope = coef(model_high_raw)["H_bar"],  se = se(model_high_raw)["H_bar"]),
  tibble(spec = "Table 8 (split-sample)", vessel.type = "Low turnover",
         slope = coef(model_split_low)["H_bar"],  se = se(model_split_low)["H_bar"]),
  tibble(spec = "Table 8 (split-sample)", vessel.type = "High turnover",
         slope = coef(model_split_high)["H_bar"], se = se(model_split_high)["H_bar"])
)

# Chapter 2's dashed reference lines are shown for context, not as a level
# Figure 8's points are expected to land on. The empirical H_bar comes from
# a fleet with dozens of fisheries and a mass of near-specialist vessels at
# H_bar close to 1, nothing like the simulated 3-fishery grid those slopes
# were estimated on, so the raw slope magnitude is not on the same footing
# even though the log(CV) ~ H_bar functional form matches. What IS
# comparable, and what Chapter 2 actually predicts, is the ORDERING, High
# turnover should sit above Low turnover, the same direction Flex sits
# above BH. Title/subtitle foreground that rather than numeric proximity to
# 0.78/0.87 specifically.
figure8 <- empirical_slopes %>%
  ggplot(aes(x = spec, y = slope, color = vessel.type)) +
  geom_hline(data = chapter2_slopes, aes(yintercept = slope), linetype = "dashed", color = "gray60") +
  # x = -Inf (left panel edge) rather than a hardcoded numeric position like
  # 0.55, spec on the main aes mapping is a discrete/character scale, and a
  # literal number in the same plot's x aesthetic (even in a layer with
  # inherit.aes = FALSE, the x scale is still shared across the whole plot)
  # throws "Discrete value supplied to a continuous scale," confirmed
  # against a real run. -Inf works on a discrete scale exactly like it does
  # on a continuous one, no need to guess a position that happens to sit
  # just left of the first category.
  geom_text(data = chapter2_slopes, aes(x = -Inf, y = slope, label = regime),
            inherit.aes = FALSE, hjust = -0.1, size = 3, color = "gray40") +
  geom_pointrange(aes(ymin = slope - 1.96 * se, ymax = slope + 1.96 * se),
                   position = position_dodge(width = 0.3), size = 0.6) +
  scale_color_manual(values = c("Low turnover" = "steelblue", "High turnover" = "firebrick")) +
  labs(
    # Why the comparison is ORDERING rather than numeric level (see the
    # comment above this plot) belongs in the caption, not here.
    title = "CV-on-H_bar slope versus Chapter 2's prediction",
    subtitle = "Chapter 2's regime slopes shown dashed for context",
    x = NULL, y = "Estimated slope, log(rev.cv) ~ H_bar", color = NULL
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure8_slope_by_type_vs_chapter2.png"),
       figure8, width = 7, height = 5.5, dpi = 300)

cat("Wrote figure8_slope_by_type_vs_chapter2.png\n")

# ============================================================================
# 5. Figure 9 [appendix]. Same exercise sorted on Phi, a check against the
#    generic mechanical-sort artifact rather than a second piece of evidence
# ============================================================================
#
# Phi is a component of H_bar (H_bar = H_LR + Phi), so sorting vessels by
# Phi and then regressing log(rev.cv) on H_bar within each group is close to
# mechanically guaranteed to show SOME gap, independent of any real
# behavioral difference, which is why this is not offered as a second piece
# of evidence for Table 7's claim. Checked against a real run, though, the
# Phi-sorted gap is small with heavily overlapping confidence intervals,
# nothing like Table 7/Figure 8's large, robust gap from the genuinely
# different-frequency within-season classifier. That makes this figure
# reassuring rather than a caution, it shows the Table 7 result is not just
# the generic thing any HHI-component sort would produce. vcov = "hetero"
# for consistency with every other cross-sectional model in this script,
# an earlier version left this at fixest's iid default, which put its
# intervals on a different basis than Figure 8's.

vessel_type_phi <- vessel_summary %>%
  filter(meets.min.years, is.finite(rev.cv), rev.cv > 0) %>%
  mutate(vessel.type = if_else(Phi > median(Phi), "High Phi", "Low Phi"))

model_phi_low  <- feols(log(rev.cv) ~ H_bar, data = filter(vessel_type_phi, vessel.type == "Low Phi"), vcov = "hetero")
model_phi_high <- feols(log(rev.cv) ~ H_bar, data = filter(vessel_type_phi, vessel.type == "High Phi"), vcov = "hetero")

phi_sorted_slopes <- bind_rows(
  tibble(vessel.type = "Low Phi",  slope = coef(model_phi_low)["H_bar"],  se = se(model_phi_low)["H_bar"]),
  tibble(vessel.type = "High Phi", slope = coef(model_phi_high)["H_bar"], se = se(model_phi_high)["H_bar"])
)

cat("Figure 9, Phi-sorted slopes, Low Phi:", round(phi_sorted_slopes$slope[1], 4),
    "(se", round(phi_sorted_slopes$se[1], 4), ") High Phi:", round(phi_sorted_slopes$slope[2], 4),
    "(se", round(phi_sorted_slopes$se[2], 4), ")\n")

# Written out as its own small table, not just plotted, so the two slopes
# are directly quotable rather than only readable off Figure 9's points.
etable(
  model_phi_low, model_phi_high,
  headers = c("Low Phi", "High Phi"),
  tex = TRUE,
  file = file.path(table_dir, "table9_appendix_slope_by_phi.tex"),
  replace = TRUE
)

figure9_appendix <- phi_sorted_slopes %>%
  ggplot(aes(x = vessel.type, y = slope)) +
  geom_hline(data = chapter2_slopes, aes(yintercept = slope), linetype = "dashed", color = "gray60") +
  geom_pointrange(aes(ymin = slope - 1.96 * se, ymax = slope + 1.96 * se), color = "firebrick", size = 0.6) +
  labs(
    # Why this reads as corroboration rather than a caution (see the
    # comment above this section) belongs in the caption.
    title = "Same exercise, sorted on Phi instead of turnover",
    subtitle = "A check against the generic mechanical-sort artifact, not a second result",
    x = NULL, y = "Estimated slope, log(rev.cv) ~ H_bar"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure9_appendix_phi_sorted.png"),
       figure9_appendix, width = 6, height = 5, dpi = 300)

cat("Wrote figure9_appendix_phi_sorted.png and table9_appendix_slope_by_phi.tex\n")
