# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of the regular
# rolling build (not sourced by run_all_rolling.R, run this standalone).
#
# Checks the leading hypothesis offered for why Section 7's shock-based
# rolling results (Table 10/11/12-rolling, and the shock interactions in
# Table 13-rolling) come out substantially weaker than baseline while the
# network-similarity main effect in Table 13-rolling replicates cleanly.
# The hypothesis, predetermined.primary.window is re-derived independently
# inside EVERY rolling window (08b_state_contingent_activation_rolling.R
# Section 1), so the same vessel can carry a DIFFERENT primary-fishery
# label, and therefore a different shock value, across two overlapping
# windows that share five of their six calendar years. If that
# reclassification happens often, it injects classical-measurement-error
# noise into shock specifically, which attenuates shock's coefficient
# without similarly harming network similarity, a fixed, pair-level,
# all-years-pooled object that never depends on which window a vessel is
# viewed through. This script measures how often that reclassification
# actually happens, directly, rather than leaving the explanation as
# untested speculation.
#
# Reads intermediate data/ch3_rolling_activation.rdata
# (activation_data.rolling, built by
# 08b_state_contingent_activation_rolling.R). Writes nothing, prints only.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

rolling_activation_path <- file.path(intermediate_dir, "ch3_rolling_activation.rdata")
if (!exists("activation_data.rolling")) load(rolling_activation_path)

# One row per (vessel, window), the label actually used in that window's
# shock and in Table 10/11/12/13-rolling's regressions. Restricted to the
# ESTIMATION sample (activation_data.rolling already only contains
# vessel-windows with at least one held, non-primary candidate fishery),
# not every eligible vessel-window, since windows that never enter the
# regression at all cannot contribute noise to a coefficient in it.
primary_by_vessel_window <- activation_data.rolling %>%
  distinct(Vessel.ADFG.Number, window.start, predetermined.primary.window) %>%
  arrange(Vessel.ADFG.Number, window.start)

cat("Vessel-windows with a predetermined primary label in the estimation sample:",
    nrow(primary_by_vessel_window), ", distinct vessels:",
    n_distinct(primary_by_vessel_window$Vessel.ADFG.Number), "\n")

# Adjacent means stride 1, window.start and window.start + 1, the pair of
# overlapping windows that share ROLL_WINDOW_WIDTH - 1 = 5 of their 6
# calendar years, the single most likely place to see the SAME
# (vessel, fishery, year) triple carry two different primary labels since
# it is the closest possible pair of windows. A gap greater than 1 means
# the vessel was not in the estimation sample in the intervening window(s),
# not a genuine non-adjacent comparison, and is excluded rather than
# compared.
primary_churn <- primary_by_vessel_window %>%
  group_by(Vessel.ADFG.Number) %>%
  mutate(
    window.gap   = window.start - lag(window.start),
    prev.primary = lag(predetermined.primary.window),
    adjacent     = !is.na(window.gap) & window.gap == 1,
    same.primary = adjacent & (predetermined.primary.window == prev.primary)
  ) %>%
  ungroup()

adjacent_pairs <- primary_churn %>% filter(adjacent)

cat("\nAdjacent (stride-1) window-pairs with a computable primary comparison:", nrow(adjacent_pairs), "\n")
cat("Share of adjacent window-pairs where the primary fishery label STAYS THE SAME:",
    round(mean(adjacent_pairs$same.primary), 4), "\n")
cat("Share of adjacent window-pairs where the primary fishery label CHANGES:",
    round(mean(!adjacent_pairs$same.primary), 4), "\n")

# Per-vessel view, since a fleet-wide average could hide a fleet that is
# mostly stable with a churning minority (which would still be enough to
# attenuate shock if that minority is disproportionately represented in
# the stacked panel) or vice versa.
vessel_churn <- adjacent_pairs %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(n.adjacent.pairs = n(), n.same = sum(same.primary),
            share.same = mean(same.primary), .groups = "drop")

cat("\nDistribution of each vessel's own share-same-primary across its adjacent windows\n")
print(quantile(vessel_churn$share.same, probs = c(0, .1, .25, .5, .75, .9, 1)))

cat("\nVessels with ZERO churn (primary never changes across any adjacent window pair):",
    sum(vessel_churn$share.same == 1), "of", nrow(vessel_churn),
    " (", round(100 * mean(vessel_churn$share.same == 1), 1), "percent )\n")
cat("Vessels with ANY churn (primary changes at least once):",
    sum(vessel_churn$share.same < 1), "of", nrow(vessel_churn),
    " (", round(100 * mean(vessel_churn$share.same < 1), 1), "percent )\n")

# Reading rule, stated explicitly rather than left implicit. A high overall
# same-primary share (loosely, above ~90 percent) would say reclassification
# is rare and this is probably NOT the main driver of the shock attenuation,
# the explanation would need another look. A share meaningfully lower than
# that, especially concentrated among vessels with many adjacent-window
# pairs (which contribute more stacked rows to the regression), would
# directly support the measurement-error mechanism offered as the leading
# hypothesis.
cat("\nReading rule, if the overall same-primary share above is high (loosely, >90%),",
    "reclassification is rare and is probably NOT the main driver of the shock-based results",
    "coming out weaker under rolling, the explanation needs another look. If it is meaningfully",
    "lower than that, this directly supports the measurement-error mechanism offered as the",
    "leading hypothesis for why Table 10/11/12-rolling and Table 13-rolling's shock interactions",
    "come out weaker while the network-similarity main effect does not.\n")

cat("diagnostic_primary_churn.R done\n")
