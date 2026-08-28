# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of
# run_all_rolling.R, run this standalone.
#
# A quick look at whether the same Moulton-problem clustering concern just
# fixed for baseline Section 7 (diagnostic_primary_fishery_clustering.R,
# folded into 08_/09_/10_'s cluster = ~primary.fishery convention) is worth
# fixing in the rolling pipeline too, now that rolling is the candidate
# primary specification for the whole chapter rather than a secondary
# comparison. Rolling's shock is the same leave-one-out fleet-wide mean,
# defined at the (predetermined.primary.window, year) level, so the same
# logic applies, vessels sharing a primary-fishery-window label in the same
# year carry nearly the same shock value, and clustering on
# Vessel.ADFG.Number + window.start alone does not address that.
#
# This is deliberately a SCREENING check, not a rebuild of the rolling
# pipeline's own clustering convention. It refits every rolling Section 7
# headline coefficient under the published two-way convention and under two
# alternatives that add predetermined.primary.window, and reports whether
# any significance verdict changes. If nothing flips, the fuller three-way
# fix (extending roll_phase_check()'s own fallback pattern) is not worth
# building right now. If something does flip, that is the trigger to build
# it properly.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year, to rebuild
# the network for the Table 13-rolling models) and intermediate
# data/ch3_rolling_activation.rdata (activation_data.rolling, built by
# 08b_state_contingent_activation_rolling.R). Rebuilds the Table 12/13
# rolling joined samples locally, identical construction to
# 09b_seasonal_overlap_rolling.R and 10b_network_similarity_rolling.R,
# since neither script persists them. Writes nothing, prints only.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

if (!exists("vessel_fishery_year") || !exists("MAX_YEAR")) load(panel_path)

rolling_activation_path <- file.path(intermediate_dir, "ch3_rolling_activation.rdata")
if (!exists("activation_data.rolling")) load(rolling_activation_path)

cat("Distinct predetermined.primary.window clusters in activation_data.rolling:",
    n_distinct(activation_data.rolling$predetermined.primary.window), "\n")
cat("Distinct window.start clusters:", n_distinct(activation_data.rolling$window.start), "\n")

# ============================================================================
# 1. Rebuild the seasonal overlap matrix and network, identical construction
#    to 09b_/10b_, needed for the Table 12/13-rolling samples
# ============================================================================

load(file.path(intermediate_dir, "catch_data_temp.rdata"))
catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp[["Pounds..Detail."]] <- as.numeric(catch_data_temp[["Pounds..Detail."]])
catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery), Statistical.Week = derive_statistical_week(Date.Landed)) %>%
  filter(Fishery != "", !is.na(Statistical.Week))

MIN_FISHERY_WEEKS_ROLLING <- 3

build_overlap_long <- function(ticket_data, min_fishery_weeks) {
  fishery_week_pounds <- ticket_data %>%
    group_by(Fishery, Statistical.Week) %>%
    summarise(pounds = sum(Pounds..Detail., na.rm = TRUE), .groups = "drop")
  fisheries_with_shape <- fishery_week_pounds %>% filter(pounds > 0) %>%
    count(Fishery, name = "n.weeks") %>% filter(n.weeks >= min_fishery_weeks) %>% pull(Fishery)
  fishery_week_wide <- fishery_week_pounds %>% filter(Fishery %in% fisheries_with_shape) %>%
    group_by(Fishery) %>% mutate(share = pounds / sum(pounds)) %>% ungroup() %>%
    select(Fishery, Statistical.Week, share) %>%
    pivot_wider(names_from = Statistical.Week, values_from = share, values_fill = 0)
  fishery_ids  <- fishery_week_wide$Fishery
  share_matrix <- as.matrix(fishery_week_wide %>% select(-Fishery))
  rownames(share_matrix) <- fishery_ids
  overlap_matrix <- sqrt(share_matrix) %*% t(sqrt(share_matrix))
  as_tibble(overlap_matrix, rownames = "Fishery.A") %>% pivot_longer(-Fishery.A, names_to = "Fishery.B", values_to = "seasonal.overlap")
}

overlap_long.rolling <- build_overlap_long(catch_data_temp, MIN_FISHERY_WEEKS_ROLLING)

held_pairs.rolling <- vessel_fishery_year %>% filter(held) %>% distinct(Vessel.ADFG.Number, Fishery)
fishery_size.rolling <- held_pairs.rolling %>% count(Fishery, name = "N_f")
MIN_NETWORK_VESSELS_ROLLING <- 10
fishery_size_filtered.rolling <- fishery_size.rolling %>% filter(N_f >= MIN_NETWORK_VESSELS_ROLLING)
surviving_fisheries.rolling <- fishery_size_filtered.rolling$Fishery
held_pairs_filtered.rolling <- held_pairs.rolling %>% filter(Fishery %in% surviving_fisheries.rolling)
held_pairs_self.rolling <- held_pairs_filtered.rolling %>%
  inner_join(held_pairs_filtered.rolling, by = "Vessel.ADFG.Number", suffix = c(".A", ".B"), relationship = "many-to-many")
network_long.rolling <- held_pairs_self.rolling %>% count(Fishery.A, Fishery.B, name = "co.vessels") %>%
  complete(Fishery.A = surviving_fisheries.rolling, Fishery.B = surviving_fisheries.rolling, fill = list(co.vessels = 0)) %>%
  left_join(fishery_size_filtered.rolling %>% rename(Fishery.A = Fishery, n.vessels.A = N_f), by = "Fishery.A") %>%
  left_join(fishery_size_filtered.rolling %>% rename(Fishery.B = Fishery, n.vessels.B = N_f), by = "Fishery.B") %>%
  mutate(net.similarity = co.vessels / (n.vessels.A + n.vessels.B - co.vessels))

# ============================================================================
# 2. Table 12/13-rolling samples, identical construction to 09b_/10b_
# ============================================================================

activation_data_overlap.rolling <- activation_data.rolling %>%
  left_join(overlap_long.rolling, by = c("Fishery" = "Fishery.A", "predetermined.primary.window" = "Fishery.B")) %>%
  rename(overlap.with.primary = seasonal.overlap) %>%
  filter(is.finite(overlap.with.primary))

activation_net.rolling <- activation_data_overlap.rolling %>%
  filter(is.finite(shock)) %>%
  left_join(network_long.rolling, by = c("Fishery" = "Fishery.A", "predetermined.primary.window" = "Fishery.B")) %>%
  filter(is.finite(net.similarity))

net_sim_mean.rolling <- mean(activation_net.rolling$net.similarity)
net_sim_sd.rolling   <- sd(activation_net.rolling$net.similarity)
activation_net.rolling <- activation_net.rolling %>% mutate(net.sim.z = (net.similarity - net_sim_mean.rolling) / net_sim_sd.rolling)

cat("Table 12-rolling sample:", nrow(activation_data_overlap.rolling), "rows\n")
cat("Table 13-rolling sample:", nrow(activation_net.rolling), "rows\n")

# ============================================================================
# 3. Helper. Fits one formula three ways, published (vessel + window.start),
#    swap (predetermined.primary.window + window.start, dropping vessel,
#    the direct analogue of what worked for baseline), and a three-way
#    attempt, falling back to NA (not a crash) if the variance matrix
#    cannot be inverted, same defensive pattern roll_phase_check() uses.
# ============================================================================

fit_screen <- function(fml, data) {
  m_published <- feols(fml, data = data, cluster = ~Vessel.ADFG.Number + window.start)
  m_swap <- tryCatch(
    feols(fml, data = data, cluster = ~predetermined.primary.window + window.start),
    error = function(e) NULL, warning = function(w) NULL
  )
  m_threeway <- tryCatch(
    feols(fml, data = data, cluster = ~Vessel.ADFG.Number + window.start + predetermined.primary.window),
    error = function(e) NULL, warning = function(w) NULL
  )
  list(published = m_published, swap = m_swap, threeway = m_threeway)
}

stars <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

report_row <- function(model_list, coef_name, label) {
  get_one <- function(m) {
    if (is.null(m) || !(coef_name %in% names(coef(m)))) return(c(est = NA_real_, se = NA_real_, stars = "fail"))
    c(est = round(unname(coef(m)[coef_name]), 4), se = round(unname(se(m)[coef_name]), 4),
      stars = stars(unname(pvalue(m)[coef_name])))
  }
  pub <- get_one(model_list$published)
  swp <- get_one(model_list$swap)
  tw  <- get_one(model_list$threeway)
  cat(sprintf("  %-16s %-28s published: %8s (%8s) %-4s | swap: %8s (%8s) %-4s | 3-way: %8s (%8s) %-4s\n",
              label, coef_name, pub["est"], pub["se"], pub["stars"], swp["est"], swp["se"], swp["stars"],
              tw["est"], tw["se"], tw["stars"]))
}

# ============================================================================
# 4. Screen every rolling Section 7 headline coefficient
# ============================================================================

cat("\n=== Table 10-rolling ===\n")
t10r <- fit_screen(activated ~ shock | Vessel.ADFG.Number + fishery.year + window.start, activation_data.rolling)
report_row(t10r, "shock", "Table 10")

cat("\n=== Table 12-rolling ===\n")
t12r <- fit_screen(activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year + window.start,
                    activation_data_overlap.rolling)
report_row(t12r, "shock", "Table 12")
report_row(t12r, "overlap.with.primary", "Table 12")
report_row(t12r, "shock:overlap.with.primary", "Table 12")

cat("\n=== Table 13-rolling, col 1 ===\n")
t13r_1 <- fit_screen(activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year + window.start,
                      activation_net.rolling)
report_row(t13r_1, "overlap.with.primary", "Table 13 col1")

cat("\n=== Table 13-rolling, col 2 ===\n")
t13r_2 <- fit_screen(activated ~ shock * net.sim.z | Vessel.ADFG.Number + fishery.year + window.start,
                      activation_net.rolling)
report_row(t13r_2, "net.sim.z", "Table 13 col2")
report_row(t13r_2, "shock:net.sim.z", "Table 13 col2")

cat("\n=== Table 13-rolling, col 3 ===\n")
t13r_3 <- fit_screen(
  activated ~ (shock * overlap.with.primary) + (shock * net.sim.z) | Vessel.ADFG.Number + fishery.year + window.start,
  activation_net.rolling
)
report_row(t13r_3, "overlap.with.primary", "Table 13 col3")
report_row(t13r_3, "net.sim.z", "Table 13 col3")
report_row(t13r_3, "shock:overlap.with.primary", "Table 13 col3")
report_row(t13r_3, "shock:net.sim.z", "Table 13 col3")

cat("\nReading rule. 'fail' means the coefficient was not present in a degenerate refit, 'NA' means the",
    "clustered variance matrix could not be inverted for that choice. Compare the stars column across",
    "published / swap / 3-way for each row. If every row's stars match across all three, the fuller",
    "three-way fix is not worth building, the published rolling convention is already giving the same",
    "verdict this more conservative check would. If any row's stars differ, that is the coefficient",
    "(and the direction of the discrepancy) to build the proper fix around.\n")

cat("diagnostic_rolling_primary_fishery_clustering.R done\n")
