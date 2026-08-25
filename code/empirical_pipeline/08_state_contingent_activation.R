# Chapter 3 empirical pipeline, Chapter3_outline.md Section 7
#
# Table 10.  Activation probability on the leave-one-out quantity shock to
#            the predetermined primary fishery, vessel and fishery-year
#            fixed effects.
# Figure 10. Predicted activation probability against shock magnitude.
# Table 11.  Placebo on a future primary-fishery shock, required rather than
#            maybe since it is what distinguishes a state-contingent
#            response from a spurious correlation.
#
# Skips Table 12 [maybe] (interaction with the Figure 3 return correlation
# and the Section 5 cross-fishery distance measure), which this pipeline
# does not build (see 06_within_season_reallocation.R's header).
#
# Follows chapter3_plan.md Section 9.3's "R4, expanded and relabeled" spec,
# the resolved version that supersedes the earlier R4/R5(a) draft in
# Section 4. Three things that draft got wrong and this corrects, per
# Section 9.2's critique.
#
#   1. "Held set" and "primary fishery" must be PREDETERMINED relative to
#      the year being explained, not contemporaneous. A vessel that acquires
#      permit j only after a bad primary-fishery year, or whose "primary"
#      fishery is defined using data from the very year the shock hits,
#      would make the design partly circular. Held set here is LAGGED (held
#      in t-1), primary fishery is fixed once from each vessel's own FIRST
#      HALF of active years (reusing the exact split-sample idea Table 8
#      already uses), and the regression sample is restricted to each
#      vessel's SECOND HALF of years, so primary is genuinely predetermined
#      relative to every year it is applied to.
#   2. The shock is built from QUANTITY (Pounds..Detail.), not revenue, to
#      strip the common salmon price channel that a revenue-based shock
#      would share with every other fishery the vessel could activate into.
#   3. The finding is state-contingent EXERCISE of an already-held option,
#      not the value of access, since permits never held are excluded from
#      the sample by construction (chapter3_plan.md Section 9.2's
#      "risk-set problem"). Framed that way in every label and comment
#      below rather than as an access-value result.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year, vessel_year,
# vessel_share_panel) AND reloads the raw catch_data_temp.rdata ticket file
# directly for Pounds..Detail., which is not carried into the saved panel.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_fishery_year") || !exists("vessel_share_panel") || !exists("vessel_year")) load(panel_path)

# ============================================================================
# 1. Predetermined primary fishery and predetermined held set
# ============================================================================
#
# Reuses Table 8's exact first-half/second-half split (07_behavioral_heterogeneity.R),
# rebuilt here rather than shared, since this script is meant to be
# runnable on its own the same way every other script in this folder is.

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
second_half_years_raw <- vessel_year_ordinal %>% filter(half == "second") %>% select(Vessel.ADFG.Number, Batch.Year)

# MIN_PRIMARY_YEARS and MIN_SECOND_HALF_YEARS mirror
# 07_behavioral_heterogeneity.R's MIN_SECOND_HALF_YEARS = 3, a floor this
# script did not otherwise have. Without it a vessel with a single
# first-half fished year could define a "predetermined primary" off that
# one year, and a vessel with a single second-half year could still enter
# the activation sample. Neither is circular (the split-sample logic still
# holds), just noisy, so the fix is the same minimum-years floor the rest
# of the pipeline already applies rather than anything structural.
MIN_PRIMARY_YEARS <- 3
MIN_SECOND_HALF_YEARS <- 3

n_first_half_fished_years <- vessel_fishery_year %>%
  filter(fished) %>%
  semi_join(first_half_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  distinct(Vessel.ADFG.Number, Batch.Year) %>%
  count(Vessel.ADFG.Number, name = "n.first.half.fished.years")

n_second_half_years <- second_half_years_raw %>%
  count(Vessel.ADFG.Number, name = "n.second.half.years")

second_half_years <- second_half_years_raw %>%
  semi_join(n_second_half_years %>% filter(n.second.half.years >= MIN_SECOND_HALF_YEARS),
            by = "Vessel.ADFG.Number")

cat("Vessels meeting the second-half-years floor (>=", MIN_SECOND_HALF_YEARS, "):",
    n_distinct(second_half_years$Vessel.ADFG.Number), "of", n_distinct(second_half_years_raw$Vessel.ADFG.Number), "\n")

# Ranked on summed revenue within the first half only, same reasoning
# 01_build_panel.R Section 6 already uses for the whole-panel prime.fishery,
# revenue is comparable across years, shares are not.
predetermined_primary <- vessel_fishery_year %>%
  filter(fished) %>%
  semi_join(first_half_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  group_by(Vessel.ADFG.Number, Fishery) %>%
  summarise(total.rev = sum(revenue), .groups = "drop") %>%
  group_by(Vessel.ADFG.Number) %>%
  slice_max(total.rev, n = 1, with_ties = FALSE) %>%
  select(Vessel.ADFG.Number, primary.fishery = Fishery) %>%
  semi_join(n_first_half_fished_years %>% filter(n.first.half.fished.years >= MIN_PRIMARY_YEARS),
            by = "Vessel.ADFG.Number")

# held.lag = TRUE means fishery j was held in year (Batch.Year - 1), which
# is what a row's own Batch.Year now represents after the shift below. This
# operationalizes "held set as of the start of t" as "held in t-1," one of
# the two options chapter3_plan.md Section 9.3 names explicitly.
held_prior_year <- vessel_fishery_year %>%
  filter(held) %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  mutate(Batch.Year = Batch.Year + 1, held.lag = TRUE)

# Base population for the activation sample. Starting from held_prior_year
# rather than vessel_fishery_year's own rows for year t matters, a permit
# held in t-1 but let lapse by t (not held AND not fished in t) still has to
# enter as a fished = FALSE observation, and it would otherwise have no row
# at all in vessel_fishery_year for year t to inner_join against.
activation_candidates <- held_prior_year %>%
  inner_join(second_half_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  inner_join(predetermined_primary, by = "Vessel.ADFG.Number") %>%
  filter(Fishery != primary.fishery) %>%
  left_join(
    vessel_fishery_year %>% filter(fished) %>% distinct(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
      mutate(activated = TRUE),
    by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")
  ) %>%
  mutate(activated = replace_na(activated, FALSE)) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, primary.fishery, activated)

cat("Vessel x fishery x year activation candidates (held in t-1, not the predetermined primary fishery):",
    nrow(activation_candidates), ", mean activation rate:", round(mean(activation_candidates$activated), 4), "\n")

# ============================================================================
# 2. Leave-one-out quantity shock to the predetermined primary fishery
# ============================================================================
#
# Reloads catch_data_temp for Pounds..Detail., not carried into the saved
# panel. Same cleaning steps as 01_build_panel.R Section 2 and
# 06_within_season_reallocation.R Section 1, duplicated for the same reason
# given there, this is the only other script that needs ticket-level
# granularity. CHECK Pounds..Detail. is the right weight field once run
# against real headers, chapter3_plan.md Section 1 lists
# Whole.Pounds..Detail. and CFEC.Whole.Pounds..Detail. as alternatives.

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)

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

# Standardization reference is the FULL fleet-mean-pounds time series for
# that fishery (all years, not leave-one-out), a stable per-fishery
# baseline. The leave-one-out adjustment only ever changes the NUMERATOR
# below, subtracting one vessel's own landings barely moves a fishery's own
# multi-year mean/sd, using a per-vessel leave-one-out reference for the
# standardization itself would mean every vessel is on a very slightly
# different scale for no real gain. Fisheries with fewer than 5 years of
# data get no reference (sd is not meaningful on that little data) and drop
# out of the shock series entirely.
fishery_quantity_stats <- fishery_year_quantity %>%
  mutate(fleet.mean.pounds = total.pounds / pmax(n.vessels, 1)) %>%
  group_by(Fishery) %>%
  summarise(ref.mean = mean(fleet.mean.pounds), ref.sd = sd(fleet.mean.pounds), n.years.fishery = n(), .groups = "drop") %>%
  filter(n.years.fishery >= 5, ref.sd > 0)

vessel_year_shock <- second_half_years %>%
  inner_join(predetermined_primary, by = "Vessel.ADFG.Number") %>%
  left_join(fishery_year_quantity, by = c("primary.fishery" = "Fishery", "Batch.Year")) %>%
  left_join(
    vessel_fishery_year_quantity %>% rename(own.pounds.primary = own.pounds),
    by = c("Vessel.ADFG.Number", "primary.fishery" = "Fishery", "Batch.Year")
  ) %>%
  mutate(
    own.pounds.primary  = replace_na(own.pounds.primary, 0),
    contributed         = own.pounds.primary > 0,
    n.remaining.vessels = replace_na(n.vessels, 0) - as.integer(contributed),
    # A vessel that is the SOLE lander of its own primary fishery that year
    # has no "everyone else" to average, the leave-one-out mean is
    # undefined there, not zero. Flooring the denominator at 1 turned this
    # into loo.mean.pounds = 0 (since total.pounds - own.pounds.primary is
    # exactly 0 whenever the vessel is the only lander), which standardizes
    # to a fabricated large-negative shock, a spurious "bad year" that is
    # really just an undefined leave-one-out. NA here instead, dropped by
    # the is.finite(shock) filters already downstream.
    loo.mean.pounds     = if_else(
      n.remaining.vessels >= 1,
      (replace_na(total.pounds, 0) - own.pounds.primary) / n.remaining.vessels,
      NA_real_
    )
  ) %>%
  left_join(fishery_quantity_stats, by = c("primary.fishery" = "Fishery")) %>%
  mutate(shock = (loo.mean.pounds - ref.mean) / ref.sd) %>%
  select(Vessel.ADFG.Number, Batch.Year, primary.fishery, shock, n.remaining.vessels)

cat("Vessel x year shocks dropped as an undefined leave-one-out (sole lander of its own primary fishery):",
    sum(vessel_year_shock$n.remaining.vessels < 1, na.rm = TRUE), "\n")

cat("Vessel x year shock observations with a computable value:",
    sum(is.finite(vessel_year_shock$shock)), "of", nrow(vessel_year_shock), "\n")

# ============================================================================
# 3. Table 10. Activation on the current-year shock
# ============================================================================
#
# A NEGATIVE coefficient is the predicted sign, a bad year in the primary
# fishery (shock below its own long-run mean) raising the probability that
# an already-held, non-primary permit gets fished that year, exactly the
# state-contingent-insurance pattern Chapter 2 predicts an unused option
# could carry even though it changes nothing on average.

activation_data <- activation_candidates %>%
  left_join(vessel_year_shock %>% select(-primary.fishery), by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  filter(is.finite(shock)) %>%
  mutate(fishery.year = paste(Fishery, Batch.Year, sep = "_"))

cat("Activation regression sample:", nrow(activation_data), "\n")
cat("Mean activation rate:", round(mean(activation_data$activated), 3), "\n")

model_activation <- feols(activated ~ shock | Vessel.ADFG.Number + fishery.year, data = activation_data)

etable(
  model_activation,
  headers = c("Activated"),
  tex = TRUE,
  file = file.path(table_dir, "table10_activation_on_primary_fishery_shock.tex"),
  replace = TRUE
)

print(etable(model_activation))

cat("Wrote table10_activation_on_primary_fishery_shock.tex\n")

# The shock varies only at (vessel, year), via each vessel's fixed primary
# fishery, so it is identified off WITHIN-fishery.year variation in shock
# across vessels with DIFFERENT primary fisheries. A fishery.year cell
# where every candidate vessel shares the same primary (or where only one
# vessel appears) contributes nothing to that coefficient, fixest absorbs
# it silently. Reported here so a reader can see whether the estimate rests
# on a broad base or a thin sliver, not left implicit.
identification_check <- activation_data %>%
  group_by(fishery.year) %>%
  summarise(n.distinct.primary = n_distinct(primary.fishery), n.obs = n(), .groups = "drop")

identifying_cells <- identification_check %>% filter(n.distinct.primary > 1)

cat("Table 10 identification, fishery-year cells with more than one distinct primary fishery:",
    nrow(identifying_cells), "of", nrow(identification_check),
    ", covering", sum(identifying_cells$n.obs), "of", nrow(activation_data), "observations\n")

# ============================================================================
# 4. Figure 10. Activation probability against shock magnitude
# ============================================================================
#
# FE-residualized binned scatter, not a raw unconditional one. Table 10
# identifies the shock coefficient WITHIN vessel and WITHIN fishery-year, a
# raw bin of activated against shock ignores both fixed effects and can
# show a different pattern than the table, even a different sign, if
# vessels or fishery-years with systematically different activation rates
# also happen to have systematically different shocks (a Simpson's-paradox
# risk with two-way FE). Partialling both activated and shock out of the
# same Vessel.ADFG.Number + fishery.year structure first (via two auxiliary
# feols fits and their residuals, the Frisch-Waugh-Lovell decomposition)
# reproduces exactly the variation the Table 10 coefficient is estimated
# from, so this figure and that table cannot visually contradict each
# other.

resid_activated <- resid(feols(activated ~ 1 | Vessel.ADFG.Number + fishery.year, data = activation_data))
resid_shock     <- resid(feols(shock ~ 1 | Vessel.ADFG.Number + fishery.year, data = activation_data))

figure10_data <- tibble(resid.shock = resid_shock, resid.activated = resid_activated) %>%
  mutate(shock.bin = ntile(resid.shock, 10)) %>%
  group_by(shock.bin) %>%
  summarise(
    mean.resid.shock     = mean(resid.shock),
    mean.resid.activated = mean(resid.activated),
    se = sd(resid.activated) / sqrt(n()),
    n  = n(),
    .groups = "drop"
  )

figure10 <- figure10_data %>%
  ggplot(aes(x = mean.resid.shock, y = mean.resid.activated)) +
  geom_errorbar(aes(ymin = mean.resid.activated - 1.96 * se, ymax = mean.resid.activated + 1.96 * se),
                width = 0.02, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linetype = "dashed") +
  labs(
    # Why residualized (matches Table 10's fixed effects) rather than a raw
    # binscatter belongs in the caption, not this subtitle.
    title = "Activation versus the shock to the primary fishery",
    subtitle = "Binned scatter, residualized on vessel and fishery-year FE",
    x = "Standardized leave-one-out quantity shock (residualized)",
    y = "Activation of a held-in-t-1, non-primary permit (residualized)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure10_activation_vs_shock.png"),
       figure10, width = 7, height = 5, dpi = 300)

cat("Wrote figure10_activation_vs_shock.png\n")

# ============================================================================
# 5. Table 11. Placebo, a FUTURE primary-fishery shock should not add
#    anything BEYOND what the current shock already explains
# ============================================================================
#
# Required rather than maybe, chapter3_outline.md Section 7, "this is what
# distinguishes a state-contingent response from a spurious correlation."
#
# An earlier version of this placebo just replaced shock with shock.future
# and compared coefficients across two separately-fit models. Two problems
# with that, caught in review. First, the shock is a fish-stock quantity
# series and is autocorrelated (a good year tends to follow a good year),
# so shock at t+1 correlates with shock at t even under a perfectly valid
# design, a "significant" coefficient on shock.future alone is not evidence
# of contamination, it could just be persistence. Second, model_activation
# was fit on the full activation_data while the placebo model was fit on a
# smaller future-shock-available sample, so a coefficient difference could
# reflect the sample change rather than the timing change.
#
# Both are fixed by putting shock and shock.future in ONE regression on the
# SAME sample and reading the test off shock.future's coefficient
# CONDITIONAL ON shock already being in the model, which asks whether next
# year's shock adds anything beyond what this year's shock explains, a
# question simple persistence cannot answer by itself. The "current shock"
# column here is the exact same activated ~ shock model, refit on this
# placebo sample (not on the full activation_data), so only the regressor
# set changes across columns, never the row set.

vessel_year_shock_future <- vessel_year_shock %>%
  transmute(Vessel.ADFG.Number, Batch.Year = Batch.Year - 1, shock.future = shock)

activation_data_placebo <- activation_data %>%
  left_join(vessel_year_shock_future, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  filter(is.finite(shock.future))

cat("Placebo regression sample (current and future shock both available):", nrow(activation_data_placebo), "\n")

model_activation_placebo_sample <- feols(activated ~ shock | Vessel.ADFG.Number + fishery.year,
                                          data = activation_data_placebo)
model_placebo_joint <- feols(activated ~ shock + shock.future | Vessel.ADFG.Number + fishery.year,
                              data = activation_data_placebo)

etable(
  model_activation_placebo_sample, model_placebo_joint,
  headers = c("Current shock only", "Current + future shock"),
  tex = TRUE,
  file = file.path(table_dir, "table11_placebo_future_shock.tex"),
  replace = TRUE
)

print(etable(model_activation_placebo_sample, model_placebo_joint))

cat("Wrote table11_placebo_future_shock.tex\n")

# ----------------------------------------------------------------------
# Supplementary. A leads-and-lags event-study version (shock at t-1, t,
# and t+1 together), printed only, not written as its own table. If the
# design is sound the effect should load on shock (and possibly
# shock.lag, a delayed response is plausible), not on shock.future once
# both shock and shock.lag already sit in the same model.
# ----------------------------------------------------------------------

vessel_year_shock_lag <- vessel_year_shock %>%
  transmute(Vessel.ADFG.Number, Batch.Year = Batch.Year + 1, shock.lag = shock)

activation_data_event <- activation_data_placebo %>%
  left_join(vessel_year_shock_lag, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  filter(is.finite(shock.lag))

cat("Event-study sample (lagged, current, and future shock all available):", nrow(activation_data_event), "\n")

if (nrow(activation_data_event) > 0) {
  model_event_study <- feols(activated ~ shock.lag + shock + shock.future | Vessel.ADFG.Number + fishery.year,
                              data = activation_data_event)
  print(etable(model_event_study, headers = "Leads/lags (supplementary)"))
}

# ============================================================================
# 6. Save
# ============================================================================
#
# activation_data (Vessel.ADFG.Number, Batch.Year, Fishery, primary.fishery,
# activated, shock, fishery.year) is exactly what 09_seasonal_overlap.R
# needs to build Table 12, saved here rather than making that script
# re-derive the predetermined-primary/lagged-held-set/leave-one-out-shock
# machinery a second time.

activation_path <- file.path(intermediate_dir, "ch3_activation.rdata")
save(activation_data, file = activation_path)
cat("Saved activation panel to", activation_path, "\n")
