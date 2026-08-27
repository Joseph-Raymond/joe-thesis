# Chapter 3 empirical pipeline, rolling-window twin of
# 09_seasonal_overlap.R
#
# TIER 3 (design Section 9.2, low priority, build only once Tier 1/2 are
# solid). Seasonal overlap STAYS ALL-YEARS POOLED, not rolling (design
# Section 3.7). A fishery's season is a fixed calendar characteristic
# pooled fleet-wide across every year precisely so no single vessel's own
# timing drives the signature used to explain that vessel's own choices, a
# 6-year rolling version would reintroduce exactly the leave-one-out
# concern the pooling exists to avoid (a thin fishery's 6-year fleet-wide
# weekly distribution can be dominated by a handful of vessels) and would
# confound "the season moved" with "this window sampled the season
# differently."
#
# Table 12-rolling, pooled only. Same interaction as baseline Table 12
# (activated ~ shock * overlap.with.primary), with window.start added to
# the fixed effects and two-way clustering, on the SAME all-years-pooled
# overlap matrix, not a rolling one.
#
# The ONE seasonal diagnostic this file adds beyond a straight port of
# Table 12 (design Section 3.7's explicit alternative to building 26
# rolling overlap matrices), a pre-1995 versus post-1995 pooled overlap
# matrix, compared once. The documented halibut B06B season widening after
# the mid-1990s IFQ conversion is the leading candidate for where a real
# change would show up here, if anywhere.
#
# Reloads catch_data_temp.rdata directly (Statistical.Week is derived, not
# a real column, same as 06_/09_, see derive_statistical_week() in
# 00_setup.R) and intermediate data/ch3_rolling_activation.rdata
# (activation_data.rolling, built by 08b_state_contingent_activation_rolling.R,
# which must run before this script). 09_seasonal_overlap.R itself is not
# edited at all.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

# MAX_YEAR is computed by 01_build_panel.R and saved into panel_path, not a
# 00_setup.R constant, same reload baseline 09_ and rolling 08b_ both already
# do before touching catch_data_temp.
if (!exists("MAX_YEAR")) load(panel_path)

rolling_activation_path <- file.path(intermediate_dir, "ch3_rolling_activation.rdata")
if (!exists("activation_data.rolling")) load(rolling_activation_path)

# ============================================================================
# 1. Fishery seasonal signature and pairwise overlap (fleet-wide, all years)
# ============================================================================
#
# Same cleaning steps as 01_/06_/09_'s own reloads, duplicated for the same
# reason given there. MIN_FISHERY_WEEKS_ROLLING, not MIN_FISHERY_WEEKS,
# MIN_FISHERY_WEEKS is on the design's do-not-reassign list (Section 8.3).

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)

# 32-bit cumsum overflow defense, same as 06_/09_.
catch_data_temp[["Pounds..Detail."]] <- as.numeric(catch_data_temp[["Pounds..Detail."]])

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(
    Fishery = strip_fishery_space(CFEC.Permit.Fishery),
    Statistical.Week = derive_statistical_week(Date.Landed)
  ) %>%
  filter(Fishery != "", !is.na(Statistical.Week))

MIN_FISHERY_WEEKS_ROLLING <- 3

# Builds the same Bhattacharyya-coefficient pairwise overlap matrix
# 09_seasonal_overlap.R's own Section 1 builds, factored into a function
# here only because this script calls it three times (all years, pre-1995,
# post-1995), not because the construction itself has changed.
build_overlap_long <- function(ticket_data, min_fishery_weeks) {
  fishery_week_pounds <- ticket_data %>%
    group_by(Fishery, Statistical.Week) %>%
    summarise(pounds = sum(Pounds..Detail., na.rm = TRUE), .groups = "drop")

  fisheries_with_shape <- fishery_week_pounds %>%
    filter(pounds > 0) %>%
    count(Fishery, name = "n.weeks") %>%
    filter(n.weeks >= min_fishery_weeks) %>%
    pull(Fishery)

  fishery_week_wide <- fishery_week_pounds %>%
    filter(Fishery %in% fisheries_with_shape) %>%
    group_by(Fishery) %>%
    mutate(share = pounds / sum(pounds)) %>%
    ungroup() %>%
    select(Fishery, Statistical.Week, share) %>%
    pivot_wider(names_from = Statistical.Week, values_from = share, values_fill = 0)

  fishery_ids   <- fishery_week_wide$Fishery
  share_matrix  <- as.matrix(fishery_week_wide %>% select(-Fishery))
  rownames(share_matrix) <- fishery_ids

  sqrt_matrix    <- sqrt(share_matrix)
  overlap_matrix <- sqrt_matrix %*% t(sqrt_matrix)

  as_tibble(overlap_matrix, rownames = "Fishery.A") %>%
    pivot_longer(-Fishery.A, names_to = "Fishery.B", values_to = "seasonal.overlap")
}

overlap_long.rolling <- build_overlap_long(catch_data_temp, MIN_FISHERY_WEEKS_ROLLING)
cat("Fishery pairs with a computable seasonal overlap (all years pooled, unchanged construction):",
    nrow(overlap_long.rolling), "\n")

# ============================================================================
# 2. The ONE seasonal diagnostic (design Section 3.7), pre-1995 vs post-1995
# ============================================================================

overlap_pre1995.rolling  <- build_overlap_long(catch_data_temp %>% filter(Batch.Year < 1995), MIN_FISHERY_WEEKS_ROLLING)
overlap_post1995.rolling <- build_overlap_long(catch_data_temp %>% filter(Batch.Year >= 1995), MIN_FISHERY_WEEKS_ROLLING)

overlap_period_compare.rolling <- overlap_pre1995.rolling %>%
  rename(overlap.pre1995 = seasonal.overlap) %>%
  inner_join(overlap_post1995.rolling %>% rename(overlap.post1995 = seasonal.overlap),
             by = c("Fishery.A", "Fishery.B")) %>%
  filter(Fishery.A != Fishery.B) %>%
  mutate(overlap.change = overlap.post1995 - overlap.pre1995)

cat("Pre/post-1995 pooled seasonal-overlap diagnostic, fishery pairs compared (present in both periods):",
    nrow(overlap_period_compare.rolling), "\n")
cat("Mean absolute change in pairwise overlap, pre- vs post-1995:",
    round(mean(abs(overlap_period_compare.rolling$overlap.change)), 4), "\n")
cat("Correlation between pre- and post-1995 pairwise overlap (near 1 = the season signature is stable",
    "across the panel, which is the assumption the all-years-pooled matrix rests on):",
    round(cor(overlap_period_compare.rolling$overlap.pre1995, overlap_period_compare.rolling$overlap.post1995), 4), "\n")

cat("Largest pairwise overlap changes, pre- to post-1995 (the documented halibut B06B season widening",
    "after the mid-1990s IFQ conversion is the leading candidate for where a real change would show up):\n")
print(overlap_period_compare.rolling %>% arrange(desc(abs(overlap.change))) %>% head(10))

# ============================================================================
# 3. Table 12-rolling, pooled only, on the all-years-pooled overlap matrix
# ============================================================================

activation_data_overlap.rolling <- activation_data.rolling %>%
  left_join(overlap_long.rolling,
            by = c("Fishery" = "Fishery.A", "predetermined.primary.window" = "Fishery.B")) %>%
  rename(overlap.with.primary = seasonal.overlap) %>%
  filter(is.finite(overlap.with.primary))

cat("Table 12-rolling sample (activation candidates with a computable overlap to that window's",
    "predetermined primary):", nrow(activation_data_overlap.rolling), "of", nrow(activation_data.rolling), "\n")

m_table12_roll <- feols(
  activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_data_overlap.rolling, cluster = ~Vessel.ADFG.Number + window.start
)

etable(
  m_table12_roll,
  headers = c("Activated (pooled, stacked)"),
  tex = TRUE,
  file = file.path(table_dir, "table12_activation_by_seasonal_overlap_rolling.tex"),
  replace = TRUE
)

print(etable(m_table12_roll))

cat("Wrote table12_activation_by_seasonal_overlap_rolling.tex. N:", nrow(activation_data_overlap.rolling),
    " distinct vessels:", n_distinct(activation_data_overlap.rolling$Vessel.ADFG.Number), "\n")

# ============================================================================
# 4. Mandatory stride-6 phase check (design Section 2.2, Layer 3), added
#    retroactively. This model was originally shipped with only two-way
#    clustering and no phase check, the one headline rolling model in this
#    folder that skipped it. 10b_network_similarity_rolling.R's own Table
#    13-rolling check (which refits this same shock x overlap interaction
#    as part of its own column 1 and column 3) is what exposed the gap, so
#    it is closed here too rather than only in the newer script. Checks all
#    three coefficients, the two main effects and the interaction, same
#    reasoning 10b_ gives for checking more than just the headline
#    interaction, a collapse or reversal in either main effect under the
#    phase check would be just as informative as an interaction that fails
#    it.
# ============================================================================

pc_shock <- roll_phase_check(
  fml = activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_data_overlap.rolling, coef_name = "shock",
  label = "Table 12-rolling: pooled overlap interaction"
)
pc_overlap_main <- roll_phase_check(
  fml = activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_data_overlap.rolling, coef_name = "overlap.with.primary",
  label = "Table 12-rolling: pooled overlap interaction"
)
pc_overlap_interaction <- roll_phase_check(
  fml = activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_data_overlap.rolling, coef_name = "shock:overlap.with.primary",
  label = "Table 12-rolling: pooled overlap interaction"
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

new_rows_table12 <- bind_rows(pc_shock$summary, pc_overlap_main$summary, pc_overlap_interaction$summary)
rolling_overlap_robustness <- rolling_overlap_robustness %>%
  filter(!(paste(model, coefficient) %in% paste(new_rows_table12$model, new_rows_table12$coefficient))) %>%
  bind_rows(new_rows_table12)

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
