# Chapter 3 empirical pipeline, rolling-window twin of
# 08_state_contingent_activation.R
#
# The unit here STAYS vessel-fishery-year, it does NOT become a
# vessel-window (design Section 6.1). Activation is a discrete yearly event
# whose whole logic is a within-year contemporaneous response to a
# within-year shock, and Table 11's placebo is a lead-lag test on the year
# index, averaging into a 6-year rate would destroy exactly the timing this
# design exists to exploit. What becomes window-specific is (a) which
# vessel-fishery-years enter the estimation sample, (b) the predetermined-
# primary label (now fixed from the PRECEDING 6-year window instead of each
# vessel's own first half of active years), and (c) an extra fixed-effect
# and clustering dimension.
#
# Table 10-rolling, headline. activated ~ shock | Vessel.ADFG.Number +
#                  fishery.year fit SEPARATELY within each outcome window,
#                  twenty regressions, each using every vessel-year at most
#                  once (no duplication problem), plotted as
#                  figure10b_activation_path_rolling.png.
# Table 10-rolling, secondary. One pooled stacked regression with
#                  window.start added to the FE, two-way clustering. A
#                  given vessel-fishery-year can appear once per eligible
#                  outcome window containing it (up to 6 times), each time
#                  with a POTENTIALLY DIFFERENT predetermined-primary label,
#                  because the lookback window differs, this duplicates
#                  outcome rows and both N and the distinct
#                  (vessel, fishery, year) count are reported so a reader
#                  does not mistake the pooled N for that many independent
#                  observations.
# Table 11-rolling, pooled only. Current-plus-future joint placebo on the
#                  pooled stacked sample with window fixed effects, NOT
#                  twenty per-window placebos (design Section 6.3).
#
# Predetermination rule (design Section 6.2). For outcome window
# w(s) = [s, s+5], the predetermined primary fishery is fixed from the
# PRECEDING window c(s) = [s-6, s-1], ranked on summed real revenue among
# c(s)'s own active years (08_'s exact construction, restricted to those
# years), requiring at least ROLL_MIN_LOOKBACK_YEARS (3) FISHED years in
# c(s). This is the SAME lookback convention Table 8-rolling uses, so the
# chapter's two split-sample designs finally share one convention rather
# than two different ones.
#
# Held set is UNCHANGED (design Section 6.2), held in t-1, vessel-fishery-
# year grain, base population built from held_prior_year. Shock is
# UNCHANGED, full-series ref.mean/ref.sd (never recomputed within a window,
# trap #8), the leave-one-out numerator, the n.remaining.vessels >= 1
# guard, pounds rather than revenue basis.
#
# TICKET RELOAD (design Section 6.4). Pounds..Detail. is not carried into
# ch3_panel.rdata, catch_data_temp.rdata is reloaded and re-cleaned here
# using 08_'s own Section 2 block, duplicating rather than sharing it, this
# pipeline's own established convention (06_, 08_, 09_ all do this).
#
# NOTE ON A LIVE-FILE DISCREPANCY, flagged rather than silently resolved.
# The design document states this reload should include "the as.numeric
# coercion on Pounds..Detail. that 06_ and 09_ apply against 32-bit
# overflow" as if 08_'s own Section 2 already has it. Checked directly
# against the live 08_state_contingent_activation.R, it does NOT have that
# coercion (06_ and 09_ do, 08_ does not). This may be a real, small latent
# bug in the baseline script (08_'s own fishery_year_quantity sums
# Pounds..Detail. within a single Fishery-Batch.Year cell, a much smaller
# overflow risk than 06_'s within-fishery-year cumsum() or 09_'s
# all-years-pooled sum, but not zero). Not fixed in 08_ itself (baseline
# files are read-only here), but included below in 08b_'s own independent
# reload regardless, both because it is harmless and because the design
# explicitly asks for it.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year, read-only)
# and intermediate data/ch3_rolling.rdata (vessel_year_window.rolling,
# window_grid.rolling, built by 01b_build_rolling_panel.R).
# 08_state_contingent_activation.R itself is not edited at all.
#
# Saves activation_data.rolling to intermediate data/ch3_rolling_activation.rdata.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

if (!exists("vessel_fishery_year")) load(panel_path)

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
if (!exists("vessel_year_window.rolling") || !exists("window_grid.rolling")) load(rolling_panel_path)

# ============================================================================
# 1. Predetermined primary fishery, from the PRECEDING window c(s)
# ============================================================================
#
# classifier_years.rolling relabels vessel_year_window.rolling's own
# window.start (the CLASSIFIER window c(s)'s start) to s = window.start +
# ROLL_WINDOW_WIDTH (the OUTCOME window it feeds into), then caps s at the
# grid's own maximum valid window.start so every s below is a genuine
# outcome window, s in {MIN_YEAR + ROLL_WINDOW_WIDTH, ..., MAX_YEAR -
# ROLL_WINDOW_WIDTH + 1}, the SAME 20-window grid Table 8-rolling uses.

classifier_years.rolling <- vessel_year_window.rolling %>%
  select(Vessel.ADFG.Number, Batch.Year, window.start) %>%
  mutate(window.start = window.start + ROLL_WINDOW_WIDTH) %>%
  filter(window.start <= max(window_grid.rolling$window.start))

cat("Table 10-rolling outcome window grid:", n_distinct(classifier_years.rolling$window.start), "windows,",
    min(classifier_years.rolling$window.start), "through", max(classifier_years.rolling$window.start), "\n")

n_fished_years_classifier.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  distinct(Vessel.ADFG.Number, Batch.Year) %>%
  inner_join(classifier_years.rolling, by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many") %>%
  distinct(Vessel.ADFG.Number, window.start, Batch.Year) %>%
  count(Vessel.ADFG.Number, window.start, name = "n.fished.years.classifier")

eligible_classifier.rolling <- n_fished_years_classifier.rolling %>%
  filter(n.fished.years.classifier >= ROLL_MIN_LOOKBACK_YEARS) %>%
  select(Vessel.ADFG.Number, window.start)

cat("Vessel x outcome-window pairs clearing the classifier floor (>=", ROLL_MIN_LOOKBACK_YEARS,
    "fished years in the preceding window):", nrow(eligible_classifier.rolling), "\n")

# Ranked on summed revenue within the classifier window only, 08_'s exact
# construction restricted to c(s)'s own active years.
predetermined_primary.rolling <- vessel_fishery_year %>%
  filter(fished) %>%
  inner_join(classifier_years.rolling, by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many") %>%
  semi_join(eligible_classifier.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  group_by(Vessel.ADFG.Number, window.start, Fishery) %>%
  summarise(total.rev = sum(revenue), .groups = "drop") %>%
  group_by(Vessel.ADFG.Number, window.start) %>%
  slice_max(total.rev, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Vessel.ADFG.Number, window.start, predetermined.primary.window = Fishery)

cat("Vessel x outcome-window predetermined-primary labels:", nrow(predetermined_primary.rolling), "\n")

# ============================================================================
# 2. Held set (unchanged, design Section 6.2) and outcome-year scope
# ============================================================================

held_prior_year <- vessel_fishery_year %>%
  filter(held) %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  mutate(Batch.Year = Batch.Year + 1, held.lag = TRUE)

# Outcome-year scope, the window's own active years (design Section 6.1,
# "which vessel-years enter the estimation sample" is what becomes window-
# specific). Deliberately NOT restricted to the ROLL_MIN_ACTIVE_YEARS (4-of-6)
# eligibility floor, that floor defines a vessel-WINDOW observation, Table
# 10-rolling's unit stays vessel-fishery-year and the design's only stated
# floor on this side of the construction is the classifier-window one above.
outcome_years.rolling <- vessel_year_window.rolling %>%
  select(Vessel.ADFG.Number, Batch.Year, window.start)

activation_candidates.rolling <- held_prior_year %>%
  inner_join(outcome_years.rolling, by = c("Vessel.ADFG.Number", "Batch.Year"), relationship = "many-to-many") %>%
  inner_join(predetermined_primary.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  filter(Fishery != predetermined.primary.window) %>%
  left_join(
    vessel_fishery_year %>% filter(fished) %>% distinct(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
      mutate(activated = TRUE),
    by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")
  ) %>%
  mutate(activated = replace_na(activated, FALSE)) %>%
  select(Vessel.ADFG.Number, Batch.Year, window.start, Fishery, predetermined.primary.window, activated)

cat("Vessel x fishery x outcome-window activation candidates (held in t-1, not that window's",
    "predetermined primary):", nrow(activation_candidates.rolling),
    ", mean activation rate:", round(mean(activation_candidates.rolling$activated), 4), "\n")

# ============================================================================
# 3. Ticket reload, leave-one-out quantity shock (UNCHANGED formula, design
#    Section 6.2 and 6.4, trap #8, full-series ref.mean/ref.sd only)
# ============================================================================

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)

# See the header note above, 06_/09_'s int-overflow defense, included here
# regardless of whether baseline 08_'s own Section 2 currently has it.
catch_data_temp[["Pounds..Detail."]] <- as.numeric(catch_data_temp[["Pounds..Detail."]])

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
  filter(Fishery != "")

fishery_year_quantity <- catch_data_temp %>%
  group_by(Fishery, Batch.Year) %>%
  summarise(total.pounds = sum(Pounds..Detail., na.rm = TRUE), n.vessels = n_distinct(Vessel.ADFG.Number),
            .groups = "drop")

vessel_fishery_year_quantity <- catch_data_temp %>%
  group_by(Vessel.ADFG.Number, Fishery, Batch.Year) %>%
  summarise(own.pounds = sum(Pounds..Detail., na.rm = TRUE), .groups = "drop")

# Full multi-year fleet-mean-pounds reference, NEVER recomputed within a
# window (trap #8, design Section 3.6, the one deliberate exception to
# "recompute within window" in the whole document).
fishery_quantity_stats <- fishery_year_quantity %>%
  mutate(fleet.mean.pounds = total.pounds / pmax(n.vessels, 1)) %>%
  group_by(Fishery) %>%
  summarise(ref.mean = mean(fleet.mean.pounds), ref.sd = sd(fleet.mean.pounds), n.years.fishery = n(), .groups = "drop") %>%
  filter(n.years.fishery >= 5, ref.sd > 0)

# Shock computed once per (vessel, outcome window, active year), against
# THAT window's own predetermined.primary.window label.
vessel_window_year_shock.rolling <- outcome_years.rolling %>%
  inner_join(predetermined_primary.rolling, by = c("Vessel.ADFG.Number", "window.start")) %>%
  left_join(fishery_year_quantity, by = c("predetermined.primary.window" = "Fishery", "Batch.Year")) %>%
  left_join(
    vessel_fishery_year_quantity %>% rename(own.pounds.primary = own.pounds),
    by = c("Vessel.ADFG.Number", "predetermined.primary.window" = "Fishery", "Batch.Year")
  ) %>%
  mutate(
    own.pounds.primary  = replace_na(own.pounds.primary, 0),
    contributed         = own.pounds.primary > 0,
    n.remaining.vessels = replace_na(n.vessels, 0) - as.integer(contributed),
    loo.mean.pounds     = if_else(
      n.remaining.vessels >= 1,
      (replace_na(total.pounds, 0) - own.pounds.primary) / n.remaining.vessels,
      NA_real_
    )
  ) %>%
  left_join(fishery_quantity_stats, by = c("predetermined.primary.window" = "Fishery")) %>%
  mutate(shock = (loo.mean.pounds - ref.mean) / ref.sd) %>%
  select(Vessel.ADFG.Number, Batch.Year, window.start, predetermined.primary.window, shock, n.remaining.vessels)

cat("Vessel x outcome-window x year shocks dropped as an undefined leave-one-out:",
    sum(vessel_window_year_shock.rolling$n.remaining.vessels < 1, na.rm = TRUE), "\n")
cat("Vessel x outcome-window x year shock observations with a computable value:",
    sum(is.finite(vessel_window_year_shock.rolling$shock)), "of", nrow(vessel_window_year_shock.rolling), "\n")

# ============================================================================
# 4. activation_data.rolling
# ============================================================================

activation_data.rolling <- activation_candidates.rolling %>%
  left_join(vessel_window_year_shock.rolling %>% select(-predetermined.primary.window),
            by = c("Vessel.ADFG.Number", "Batch.Year", "window.start")) %>%
  filter(is.finite(shock)) %>%
  mutate(fishery.year = paste(Fishery, Batch.Year, sep = "_"))

n_distinct_vfy.rolling <- activation_data.rolling %>% distinct(Vessel.ADFG.Number, Fishery, Batch.Year) %>% nrow()

cat("Table 10-rolling pooled activation sample:", nrow(activation_data.rolling), "rows",
    "(a vessel-fishery-year can repeat across up to", ROLL_WINDOW_WIDTH,
    "outcome windows with a DIFFERENT predetermined-primary label each time, this is NOT a claim of",
    "that many independent observations), distinct (vessel, fishery, year) triples:", n_distinct_vfy.rolling,
    ", distinct vessels:", n_distinct(activation_data.rolling$Vessel.ADFG.Number), "\n")
cat("Mean activation rate:", round(mean(activation_data.rolling$activated), 3), "\n")

# ============================================================================
# 5. Table 10-rolling, HEADLINE. Per-window activation regressions
# ============================================================================
#
# Design Section 6.3. Each of these twenty fits uses every vessel-year in
# that window's sample at most once, no duplication problem whatsoever. No
# cluster argument given, matching baseline 08_'s own convention exactly
# (fixest's default clusters on the first fixed effect, Vessel.ADFG.Number,
# which is the appropriate repeated unit here since a vessel can hold
# several non-primary fisheries within the same outcome window).

outcome_starts_activation <- sort(unique(activation_data.rolling$window.start))

activation_path.rolling <- lapply(outcome_starts_activation, function(s) {
  dat_s <- activation_data.rolling %>% filter(window.start == s)
  if (nrow(dat_s) < 30 || n_distinct(dat_s$Vessel.ADFG.Number) < 5) {
    return(tibble(window.start = s, estimate = NA_real_, se = NA_real_, n = nrow(dat_s)))
  }
  m_s <- tryCatch(
    feols(activated ~ shock | Vessel.ADFG.Number + fishery.year, data = dat_s),
    error = function(e) NULL
  )
  if (is.null(m_s) || !("shock" %in% names(coef(m_s)))) {
    return(tibble(window.start = s, estimate = NA_real_, se = NA_real_, n = nrow(dat_s)))
  }
  tibble(window.start = s, estimate = unname(coef(m_s)["shock"]), se = unname(se(m_s)["shock"]), n = nrow(dat_s))
}) %>% bind_rows()

cat("Per-window activation fits, computable estimates:", sum(is.finite(activation_path.rolling$estimate)),
    "of", nrow(activation_path.rolling), "\n")

figure10b.rolling <- activation_path.rolling %>%
  filter(is.finite(estimate)) %>%
  ggplot(aes(x = window.start, y = estimate)) +
  geom_ribbon(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), alpha = 0.15, fill = "steelblue") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Activation-on-shock coefficient, by outcome window",
    subtitle = "Separate fit per window, each vessel-year used at most once, negative = state-contingent insurance response",
    x = "Outcome window start year", y = "Coefficient on shock (activated ~ shock | vessel + fishery-year)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure10b_activation_path_rolling.png"),
       figure10b.rolling, width = 8, height = 5, dpi = 300)

cat("Wrote figure10b_activation_path_rolling.png\n")

# ============================================================================
# 6. Table 10-rolling, SECONDARY. Pooled stacked regression
# ============================================================================

m_activation_roll <- feols(activated ~ shock | Vessel.ADFG.Number + fishery.year + window.start,
                            data = activation_data.rolling, cluster = ~Vessel.ADFG.Number + window.start)

etable(
  m_activation_roll,
  headers = c("Activated (pooled, stacked)"),
  tex = TRUE,
  file = file.path(table_dir, "table10_activation_on_primary_fishery_shock_rolling.tex"),
  replace = TRUE
)

print(etable(m_activation_roll))

cat("Wrote table10_activation_on_primary_fishery_shock_rolling.tex. N (rows, DUPLICATED across",
    "outcome windows):", nrow(activation_data.rolling), ", distinct (vessel, fishery, year):",
    n_distinct_vfy.rolling, "\n")

# ============================================================================
# 7. Table 11-rolling, pooled placebo only (design Section 6.3)
# ============================================================================

vessel_window_year_shock_future.rolling <- vessel_window_year_shock.rolling %>%
  transmute(Vessel.ADFG.Number, window.start, Batch.Year = Batch.Year - 1, shock.future = shock)

activation_data_placebo.rolling <- activation_data.rolling %>%
  left_join(vessel_window_year_shock_future.rolling,
            by = c("Vessel.ADFG.Number", "window.start", "Batch.Year")) %>%
  filter(is.finite(shock.future))

cat("Placebo regression sample (current and future shock both available, pooled):",
    nrow(activation_data_placebo.rolling), "\n")

m_activation_placebo_sample_roll <- feols(
  activated ~ shock | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_data_placebo.rolling, cluster = ~Vessel.ADFG.Number + window.start
)
m_placebo_joint_roll <- feols(
  activated ~ shock + shock.future | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_data_placebo.rolling, cluster = ~Vessel.ADFG.Number + window.start
)

etable(
  m_activation_placebo_sample_roll, m_placebo_joint_roll,
  headers = c("Current shock only", "Current + future shock"),
  tex = TRUE,
  file = file.path(table_dir, "table11_placebo_future_shock_rolling.tex"),
  replace = TRUE
)

print(etable(m_activation_placebo_sample_roll, m_placebo_joint_roll))

cat("Wrote table11_placebo_future_shock_rolling.tex\n")

# ============================================================================
# 8. Mandatory stride-6 phase check (design Section 2.2, Layer 3)
# ============================================================================

pc_activation <- roll_phase_check(
  fml = activated ~ shock | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_data.rolling, coef_name = "shock", label = "Table 10-rolling: pooled activation"
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

new_rows <- pc_activation$summary
rolling_overlap_robustness <- rolling_overlap_robustness %>%
  filter(!(paste(model, coefficient) %in% paste(new_rows$model, new_rows$coefficient))) %>%
  bind_rows(new_rows)

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

# ============================================================================
# 9. Save
# ============================================================================

rolling_activation_path <- file.path(intermediate_dir, "ch3_rolling_activation.rdata")
save(activation_data.rolling, file = rolling_activation_path)
cat("Saved activation panel to", rolling_activation_path, "\n")
