# Chapter 3 empirical pipeline, rolling-window twin of
# 10_network_similarity.R
#
# Table 13-rolling. Extends Table 12-rolling (09b_seasonal_overlap_rolling.R)
# with the same held-based co-participation Jaccard network used in the
# baseline Table 13, refit on the rolling activation sample with
# window.start added to the fixed effects and to clustering, plus the
# mandatory stride-6 non-overlapping phase check (design Section 2.2, Layer
# 3) on every headline coefficient this table reports.
#
# That phase check is the direct answer to the reason this script exists.
# A naive vessel-clustered SE on a stacked panel where the same (vessel,
# fishery, year) triple can appear across up to ROLL_WINDOW_WIDTH
# overlapping outcome windows would risk overstating how much genuinely
# independent information the panel contains, a single vessel-year's
# realized activation outcome is not new information each time an
# overlapping window repeats it with a different predetermined-primary
# label. Two-way clustering (Vessel.ADFG.Number + window.start) is the
# primary correction, matching every other rolling activation model in this
# folder, and the phase check calibrates whether that correction is doing
# its job by comparing the full-panel SE against SEs from genuinely
# non-overlapping stride-6 phases. See 00b_rolling_periods.R's
# roll_phase_check() header for the exact reading rule, if SE_full sits
# near SE_phase the clustering is working, if it sits near
# SE_phase / sqrt(6) the panel is manufacturing precision and the phase SEs
# are the ones to trust instead.
#
# NETWORK STAYS ALL-YEARS POOLED, NOT ROLLING, same reasoning
# 09b_seasonal_overlap_rolling.R gives for keeping seasonal overlap pooled
# and baseline 10_network_similarity.R gives for itself. A fishery's
# co-participation adjacency to another fishery is treated as a structural
# characteristic of the pair, not something to re-estimate inside every
# thin 6-year window off a window-local ever-holder count, and pooling
# across the whole 31-year panel is what keeps any single vessel's own
# membership from meaningfully driving the network used to explain that
# same vessel's own choices. Rebuilt here rather than loaded from the
# baseline script's objects (00_setup.R's rm(list = ls()) wipes those
# between scripts regardless of what ran before), using the identical
# construction, held not fished, all years pooled, a
# MIN_NETWORK_VESSELS_ROLLING floor renamed from baseline's own
# MIN_NETWORK_VESSELS to avoid reassigning a name outside this file's own
# scope, the same reasoning 09b_ renamed MIN_FISHERY_WEEKS.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year, for the
# held-permit network) and intermediate data/ch3_rolling_activation.rdata
# (activation_data.rolling, built by
# 08b_state_contingent_activation_rolling.R, which must run before this
# script). Rebuilds the seasonal overlap matrix locally, identical
# construction to 09b_'s own Section 1, duplicated for the same reason
# every rolling script duplicates shared logic rather than depending on
# another script's in-memory state, since neither script persists
# overlap_long.rolling for reuse. Writes
# table13_activation_by_network_similarity_rolling.tex to
# Chpt3/output/tables/ and appends six rows (both main effects alone, both
# main effects with the other measure controlled for, and both shock
# interactions) to table_rolling_overlap_robustness.tex.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

if (!exists("vessel_fishery_year") || !exists("MAX_YEAR")) load(panel_path)

rolling_activation_path <- file.path(intermediate_dir, "ch3_rolling_activation.rdata")
if (!exists("activation_data.rolling")) load(rolling_activation_path)

# ============================================================================
# 1. Network construction (held-based, all years pooled), unchanged from
#    baseline 10_network_similarity.R Section 1, renamed objects only
# ============================================================================

held_pairs.rolling <- vessel_fishery_year %>%
  filter(held) %>%
  distinct(Vessel.ADFG.Number, Fishery)

cat("held_pairs.rolling, distinct vessels:", n_distinct(held_pairs.rolling$Vessel.ADFG.Number),
    " distinct fisheries:", n_distinct(held_pairs.rolling$Fishery), "\n")

fishery_size.rolling <- held_pairs.rolling %>% count(Fishery, name = "N_f")

MIN_NETWORK_VESSELS_ROLLING <- 10

fishery_size_filtered.rolling <- fishery_size.rolling %>% filter(N_f >= MIN_NETWORK_VESSELS_ROLLING)
surviving_fisheries.rolling <- fishery_size_filtered.rolling$Fishery

held_pairs_filtered.rolling <- held_pairs.rolling %>% filter(Fishery %in% surviving_fisheries.rolling)

cat("Fisheries surviving MIN_NETWORK_VESSELS_ROLLING >=", MIN_NETWORK_VESSELS_ROLLING, ":",
    length(surviving_fisheries.rolling), "of", nrow(fishery_size.rolling), "\n")

# Size guard, same reasoning as baseline 10_'s own Section 1, printed so a
# data pathology shows up before the self-join runs rather than after.
fisheries_per_vessel.rolling <- held_pairs_filtered.rolling %>% count(Vessel.ADFG.Number, name = "n.fisheries")
presplit_rows.rolling <- sum(fisheries_per_vessel.rolling$n.fisheries^2)
cat("Self-join row count before count() collapses it:", presplit_rows.rolling, "\n")
if (presplit_rows.rolling > 2e6) {
  cat("  NOTE, this exceeds 2 million rows, the self-join below may be slow.\n")
}

held_pairs_self.rolling <- held_pairs_filtered.rolling %>%
  inner_join(held_pairs_filtered.rolling, by = "Vessel.ADFG.Number",
             suffix = c(".A", ".B"), relationship = "many-to-many")

co_vessels_long.rolling <- held_pairs_self.rolling %>%
  count(Fishery.A, Fishery.B, name = "co.vessels")

network_long.rolling <- co_vessels_long.rolling %>%
  complete(Fishery.A = surviving_fisheries.rolling, Fishery.B = surviving_fisheries.rolling,
           fill = list(co.vessels = 0)) %>%
  left_join(fishery_size_filtered.rolling %>% rename(Fishery.A = Fishery, n.vessels.A = N_f), by = "Fishery.A") %>%
  left_join(fishery_size_filtered.rolling %>% rename(Fishery.B = Fishery, n.vessels.B = N_f), by = "Fishery.B") %>%
  mutate(net.similarity = co.vessels / (n.vessels.A + n.vessels.B - co.vessels))

cat("network_long.rolling, fishery pairs with a computable Jaccard similarity:", nrow(network_long.rolling), "\n")

# Join-safety check, same reasoning as baseline 10_'s Section 3.4. Register-
# side Fishery codes (this network) and the rolling activation sample's own
# predetermined.primary.window (ticket-derived, built the same way baseline's
# primary.fishery is) need to agree on formatting or the join in Section 3
# below silently returns NA for every row rather than erroring.
MIN_CODE_MATCH_RATE_ROLLING <- 0.05

code_match_rate.rolling <- mean(activation_data.rolling$predetermined.primary.window %in% surviving_fisheries.rolling)
cat("Share of activation_data.rolling rows whose predetermined.primary.window matches a fishery code",
    "present in the filtered held-based network:", round(code_match_rate.rolling, 4), "\n")

if (code_match_rate.rolling < MIN_CODE_MATCH_RATE_ROLLING) {
  stop(
    "Join-safety check failed, only ", round(100 * code_match_rate.rolling, 2),
    "% of activation_data.rolling rows have a predetermined.primary.window present in the ",
    "held-based network. Same failure mode baseline 10_network_similarity.R's Section 3.4 guards ",
    "against, register-side and ticket-side Fishery codes disagreeing on whitespace or formatting. ",
    "Fix the code-cleaning mismatch before proceeding, do not just raise the threshold."
  )
}

# ============================================================================
# 2. Seasonal overlap matrix, identical construction to
#    09b_seasonal_overlap_rolling.R Section 1, duplicated rather than
#    reloaded since that script does not persist overlap_long.rolling
# ============================================================================

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp[["Pounds..Detail."]] <- as.numeric(catch_data_temp[["Pounds..Detail."]])

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(
    Fishery = strip_fishery_space(CFEC.Permit.Fishery),
    Statistical.Week = derive_statistical_week(Date.Landed)
  ) %>%
  filter(Fishery != "", !is.na(Statistical.Week))

MIN_FISHERY_WEEKS_ROLLING <- 3

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
cat("overlap_long.rolling, fishery pairs with a computable seasonal overlap:", nrow(overlap_long.rolling), "\n")

# ============================================================================
# 3. Build activation_net.rolling
# ============================================================================

n_activation_data.rolling <- nrow(activation_data.rolling)

activation_net_step1.rolling <- activation_data.rolling %>%
  left_join(overlap_long.rolling,
            by = c("Fishery" = "Fishery.A", "predetermined.primary.window" = "Fishery.B")) %>%
  rename(overlap.with.primary = seasonal.overlap) %>%
  filter(is.finite(shock), is.finite(overlap.with.primary))

activation_net.rolling <- activation_net_step1.rolling %>%
  left_join(network_long.rolling,
            by = c("Fishery" = "Fishery.A", "predetermined.primary.window" = "Fishery.B")) %>%
  filter(is.finite(net.similarity))

n_step2.rolling <- nrow(activation_net.rolling)

cat("Row counts, activation_data.rolling to activation_net.rolling (rows, DUPLICATED across outcome",
    "windows, see 08b_'s own note, not a claim of that many independent observations)\n")
cat("  activation_data.rolling:", n_activation_data.rolling, "\n")
cat("  + finite shock and overlap.with.primary:", nrow(activation_net_step1.rolling), "\n")
cat("  + finite net.similarity:", n_step2.rolling, "\n")
cat("  distinct (vessel, fishery, year) triples:",
    activation_net.rolling %>% distinct(Vessel.ADFG.Number, Fishery, Batch.Year) %>% nrow(), "\n")

net_sim_mean.rolling <- mean(activation_net.rolling$net.similarity)
net_sim_sd.rolling   <- sd(activation_net.rolling$net.similarity)

activation_net.rolling <- activation_net.rolling %>%
  mutate(net.sim.z = (net.similarity - net_sim_mean.rolling) / net_sim_sd.rolling)

cat("Mean net.similarity:", round(net_sim_mean.rolling, 4), " sd:", round(net_sim_sd.rolling, 4), "\n")

# Collinearity check, net of the SAME fixed-effect structure the real model
# uses (vessel + fishery.year + window.start here, not just vessel +
# fishery.year), the lesson carried over from the baseline script's second
# deep review, the raw correlation is not the diagnostic that matters for
# whether column 3's partial coefficients below are safe to read as a
# decomposition.
resid_overlap.rolling <- resid(feols(overlap.with.primary ~ 1 | Vessel.ADFG.Number + fishery.year + window.start,
                                      data = activation_net.rolling))
resid_netsimz.rolling <- resid(feols(net.sim.z ~ 1 | Vessel.ADFG.Number + fishery.year + window.start,
                                      data = activation_net.rolling))
cor_resid.rolling <- cor(resid_overlap.rolling, resid_netsimz.rolling)
cat("FE-residualized correlation between overlap.with.primary and net.sim.z:", round(cor_resid.rolling, 4), "\n")
if (abs(cor_resid.rolling) > 0.7) {
  cat("  Collinearity flag TRIGGERED, column 3's partial coefficients are fragile, read columns 1 and 2",
      "side by side rather than column 3's partial effects\n")
}

# ============================================================================
# 4. Table 13-rolling. Three models, window.start added to the fixed
#    effects and to clustering, matching 09b_'s Table 12-rolling convention
# ============================================================================

m_table13_col1_roll <- feols(activated ~ shock * overlap.with.primary |
                                Vessel.ADFG.Number + fishery.year + window.start,
                              data = activation_net.rolling, cluster = ~Vessel.ADFG.Number + window.start)
m_table13_col2_roll <- feols(activated ~ shock * net.sim.z |
                                Vessel.ADFG.Number + fishery.year + window.start,
                              data = activation_net.rolling, cluster = ~Vessel.ADFG.Number + window.start)
m_table13_col3_roll <- feols(activated ~ (shock * overlap.with.primary) + (shock * net.sim.z) |
                                Vessel.ADFG.Number + fishery.year + window.start,
                              data = activation_net.rolling, cluster = ~Vessel.ADFG.Number + window.start)

table13_dict_roll <- c(net.sim.z = "Network similarity (z-scored)")

etable(
  m_table13_col1_roll, m_table13_col2_roll, m_table13_col3_roll,
  headers = c("Seasonal overlap", "Network similarity", "Both"),
  dict = table13_dict_roll,
  tex = TRUE,
  file = file.path(table_dir, "table13_activation_by_network_similarity_rolling.tex"),
  replace = TRUE
)

print(etable(m_table13_col1_roll, m_table13_col2_roll, m_table13_col3_roll,
             headers = c("Seasonal overlap", "Network similarity", "Both"), dict = table13_dict_roll))

cat("Wrote table13_activation_by_network_similarity_rolling.tex. N:", nrow(activation_net.rolling),
    " distinct vessels:", n_distinct(activation_net.rolling$Vessel.ADFG.Number), "\n")

# ============================================================================
# 5. Mandatory stride-6 phase check (design Section 2.2, Layer 3), on every
#    headline coefficient this table reports, both main effects alone
#    (columns 1 and 2), both main effects with the other measure controlled
#    for (column 3), and both shock interactions (column 3). This is the
#    direct answer to whether the two-way-clustered standard errors above
#    are trustworthy or whether the overlapping-window stacking is
#    manufacturing precision, see 00b_rolling_periods.R's roll_phase_check()
#    header for the exact reading rule.
# ============================================================================

pc_overlap_alone <- roll_phase_check(
  fml = activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_net.rolling, coef_name = "overlap.with.primary",
  label = "Table 13-rolling: seasonal overlap alone (col 1)"
)

pc_netsim_alone <- roll_phase_check(
  fml = activated ~ shock * net.sim.z | Vessel.ADFG.Number + fishery.year + window.start,
  data = activation_net.rolling, coef_name = "net.sim.z",
  label = "Table 13-rolling: network similarity alone (col 2)"
)

both_fml_roll <- activated ~ (shock * overlap.with.primary) + (shock * net.sim.z) |
  Vessel.ADFG.Number + fishery.year + window.start

pc_overlap_both <- roll_phase_check(
  fml = both_fml_roll, data = activation_net.rolling, coef_name = "overlap.with.primary",
  label = "Table 13-rolling: both (col 3)"
)
pc_netsim_both <- roll_phase_check(
  fml = both_fml_roll, data = activation_net.rolling, coef_name = "net.sim.z",
  label = "Table 13-rolling: both (col 3)"
)
pc_overlap_interaction <- roll_phase_check(
  fml = both_fml_roll, data = activation_net.rolling, coef_name = "shock:overlap.with.primary",
  label = "Table 13-rolling: both (col 3)"
)
pc_netsim_interaction <- roll_phase_check(
  fml = both_fml_roll, data = activation_net.rolling, coef_name = "shock:net.sim.z",
  label = "Table 13-rolling: both (col 3)"
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

new_rows.rolling <- bind_rows(
  pc_overlap_alone$summary, pc_netsim_alone$summary,
  pc_overlap_both$summary, pc_netsim_both$summary,
  pc_overlap_interaction$summary, pc_netsim_interaction$summary
)
rolling_overlap_robustness <- rolling_overlap_robustness %>%
  filter(!(paste(model, coefficient) %in% paste(new_rows.rolling$model, new_rows.rolling$coefficient))) %>%
  bind_rows(new_rows.rolling)

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

cat("10b_network_similarity_rolling.R done\n")
