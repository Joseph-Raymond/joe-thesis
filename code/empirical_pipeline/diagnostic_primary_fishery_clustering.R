# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone.
#
# Checks the clustering-level concern flagged for baseline Section 7 (Table
# 10, 11, 12, 13), never addressed anywhere in this pipeline until now. The
# activation shock is a leave-one-out FLEET-WIDE mean for a given (primary
# fishery, year), so any two vessels that share the same primary fishery in
# the same year carry nearly the same shock value (they differ only by the
# leave-one-out adjustment, which is small whenever the fishery has more
# than a handful of vessels). Clustering standard errors on
# Vessel.ADFG.Number does not address that, a vessel-clustered SE treats
# each vessel as an independent draw even when dozens of vessels are really
# just repeating the same underlying (fishery, year) data point. This is a
# textbook Moulton (1990) problem, and the conservative fix is to cluster
# on the fishery the shock is actually defined over (primary.fishery here),
# or two-way on vessel and primary.fishery.
#
# Refits Table 10, 11, 12, and 13's exact baseline specifications three
# ways, cluster = ~Vessel.ADFG.Number (the published convention, reproduced
# here as the reference point), cluster = ~primary.fishery, and
# cluster = ~Vessel.ADFG.Number + primary.fishery. Ends with one compact
# summary table pulling every headline coefficient's estimate, SE, and
# significance stars across all three choices, so the reading is immediate
# without scrolling through a dozen separate etable blocks.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year, to rebuild
# the network for Table 13, identical construction to
# 10_network_similarity.R Section 1), intermediate data/ch3_activation.rdata
# (activation_data, built by 08_state_contingent_activation.R), and
# intermediate data/ch3_seasonal_overlap.rdata (overlap_long, built by
# 09_seasonal_overlap.R). Table 11's placebo sample and Table 12/13's joined
# samples are rebuilt here rather than reloaded, since neither
# 08_/09_/10_ persists them, only activation_data and overlap_long are
# ever saved. Writes nothing, prints only.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_fishery_year")) load(panel_path)

activation_path <- file.path(intermediate_dir, "ch3_activation.rdata")
if (!exists("activation_data")) load(activation_path)

seasonal_overlap_path <- file.path(intermediate_dir, "ch3_seasonal_overlap.rdata")
if (!exists("overlap_long")) load(seasonal_overlap_path)

# ============================================================================
# 1. Rebuild the three derived samples this diagnostic needs, identical
#    construction to the scripts that originally built them
# ============================================================================

# Table 11's placebo sample. vessel_year_shock itself is not saved anywhere,
# but activation_data already carries one shock value per (vessel, year),
# identical across every candidate Fishery row for that vessel-year since
# shock depends only on the vessel's own primary fishery's performance that
# year, not on which alternative fishery is being tested. Recovering it via
# distinct() avoids reloading and recomputing from raw tickets.
vessel_year_shock <- activation_data %>%
  distinct(Vessel.ADFG.Number, Batch.Year, shock) %>%
  filter(is.finite(shock))

vessel_year_shock_future <- vessel_year_shock %>%
  transmute(Vessel.ADFG.Number, Batch.Year = Batch.Year - 1, shock.future = shock)

activation_data_placebo <- activation_data %>%
  left_join(vessel_year_shock_future, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  filter(is.finite(shock.future))

cat("Table 11 placebo sample rebuilt:", nrow(activation_data_placebo), "rows\n")

# Table 12's sample, identical construction to 09_seasonal_overlap.R.
activation_data_overlap <- activation_data %>%
  left_join(overlap_long, by = c("Fishery" = "Fishery.A", "primary.fishery" = "Fishery.B")) %>%
  rename(overlap.with.primary = seasonal.overlap) %>%
  filter(is.finite(overlap.with.primary))

cat("Table 12 sample rebuilt:", nrow(activation_data_overlap), "rows\n")

# Network (held-based, all years pooled), identical construction to
# 10_network_similarity.R Section 1, needed for Table 13's sample.
held_pairs <- vessel_fishery_year %>%
  filter(held) %>%
  distinct(Vessel.ADFG.Number, Fishery)

fishery_size <- held_pairs %>% count(Fishery, name = "N_f")
MIN_NETWORK_VESSELS <- 10
fishery_size_filtered <- fishery_size %>% filter(N_f >= MIN_NETWORK_VESSELS)
surviving_fisheries <- fishery_size_filtered$Fishery
held_pairs_filtered <- held_pairs %>% filter(Fishery %in% surviving_fisheries)

held_pairs_self <- held_pairs_filtered %>%
  inner_join(held_pairs_filtered, by = "Vessel.ADFG.Number",
             suffix = c(".A", ".B"), relationship = "many-to-many")

co_vessels_long <- held_pairs_self %>% count(Fishery.A, Fishery.B, name = "co.vessels")

network_long <- co_vessels_long %>%
  complete(Fishery.A = surviving_fisheries, Fishery.B = surviving_fisheries,
           fill = list(co.vessels = 0)) %>%
  left_join(fishery_size_filtered %>% rename(Fishery.A = Fishery, n.vessels.A = N_f), by = "Fishery.A") %>%
  left_join(fishery_size_filtered %>% rename(Fishery.B = Fishery, n.vessels.B = N_f), by = "Fishery.B") %>%
  mutate(net.similarity = co.vessels / (n.vessels.A + n.vessels.B - co.vessels))

# Table 13's sample, identical construction to 10_network_similarity.R
# Section 3.
activation_net <- activation_data_overlap %>%
  filter(is.finite(shock)) %>%
  left_join(network_long, by = c("Fishery" = "Fishery.A", "primary.fishery" = "Fishery.B")) %>%
  filter(is.finite(net.similarity))

net_sim_mean <- mean(activation_net$net.similarity)
net_sim_sd   <- sd(activation_net$net.similarity)
activation_net <- activation_net %>% mutate(net.sim.z = (net.similarity - net_sim_mean) / net_sim_sd)

cat("Table 13 sample rebuilt:", nrow(activation_net), "rows\n")

# ============================================================================
# 2. Cluster-count diagnostics, printed before any regression. Cluster-
#    robust inference needs enough distinct clusters to be trustworthy, a
#    primary.fishery cluster with only a handful of distinct values would
#    make this check uninformative on its own terms.
# ============================================================================

cat("\nDistinct primary.fishery clusters, by sample\n")
cat("  Table 10 (activation_data):", n_distinct(activation_data$primary.fishery), "\n")
cat("  Table 11 (activation_data_placebo):", n_distinct(activation_data_placebo$primary.fishery), "\n")
cat("  Table 12 (activation_data_overlap):", n_distinct(activation_data_overlap$primary.fishery), "\n")
cat("  Table 13 (activation_net):", n_distinct(activation_net$primary.fishery), "\n")

# ============================================================================
# 3. Helper. Fits one formula on one dataset under all three clustering
#    choices, two-way falls back to a printed warning (not a crash) if the
#    variance matrix cannot be inverted, same defensive pattern
#    00b_rolling_periods.R's roll_phase_check() already uses for exactly
#    this failure mode.
# ============================================================================

fit_three_ways <- function(fml, data) {
  m_vessel  <- feols(fml, data = data, cluster = ~Vessel.ADFG.Number)
  m_primary <- feols(fml, data = data, cluster = ~primary.fishery)
  m_twoway  <- tryCatch(
    feols(fml, data = data, cluster = ~Vessel.ADFG.Number + primary.fishery),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(m_twoway)) {
    cat("  NOTE, two-way (vessel + primary.fishery) clustering failed or was degenerate for this model,",
        "shown as NA below.\n")
  }
  list(vessel = m_vessel, primary = m_primary, twoway = m_twoway)
}

# ============================================================================
# 4. Refit every headline model three ways
# ============================================================================

cat("\n=== Table 10 ===\n")
t10 <- fit_three_ways(activated ~ shock | Vessel.ADFG.Number + fishery.year, activation_data)
print(etable(t10$vessel, t10$primary, t10$twoway,
             headers = c("Cluster: vessel", "Cluster: primary.fishery", "Cluster: vessel + primary.fishery")))

cat("\n=== Table 11 (current shock only) ===\n")
t11a <- fit_three_ways(activated ~ shock | Vessel.ADFG.Number + fishery.year, activation_data_placebo)
print(etable(t11a$vessel, t11a$primary, t11a$twoway,
             headers = c("Cluster: vessel", "Cluster: primary.fishery", "Cluster: vessel + primary.fishery")))

cat("\n=== Table 11 (current + future shock) ===\n")
t11b <- fit_three_ways(activated ~ shock + shock.future | Vessel.ADFG.Number + fishery.year, activation_data_placebo)
print(etable(t11b$vessel, t11b$primary, t11b$twoway,
             headers = c("Cluster: vessel", "Cluster: primary.fishery", "Cluster: vessel + primary.fishery")))

cat("\n=== Table 12 ===\n")
t12 <- fit_three_ways(activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year, activation_data_overlap)
print(etable(t12$vessel, t12$primary, t12$twoway,
             headers = c("Cluster: vessel", "Cluster: primary.fishery", "Cluster: vessel + primary.fishery")))

cat("\n=== Table 13, column 1 (seasonal overlap alone) ===\n")
t13_1 <- fit_three_ways(activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year, activation_net)
print(etable(t13_1$vessel, t13_1$primary, t13_1$twoway,
             headers = c("Cluster: vessel", "Cluster: primary.fishery", "Cluster: vessel + primary.fishery")))

cat("\n=== Table 13, column 2 (network similarity alone) ===\n")
t13_2 <- fit_three_ways(activated ~ shock * net.sim.z | Vessel.ADFG.Number + fishery.year, activation_net)
print(etable(t13_2$vessel, t13_2$primary, t13_2$twoway,
             headers = c("Cluster: vessel", "Cluster: primary.fishery", "Cluster: vessel + primary.fishery")))

cat("\n=== Table 13, column 3 (both) ===\n")
t13_3 <- fit_three_ways(
  activated ~ (shock * overlap.with.primary) + (shock * net.sim.z) | Vessel.ADFG.Number + fishery.year,
  activation_net
)
print(etable(t13_3$vessel, t13_3$primary, t13_3$twoway,
             headers = c("Cluster: vessel", "Cluster: primary.fishery", "Cluster: vessel + primary.fishery")))

# ============================================================================
# 5. Consolidated summary, one row per headline coefficient, estimate / SE /
#    stars under each of the three clustering choices side by side
# ============================================================================

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

extract_row <- function(model_list, coef_name, label) {
  get_one <- function(m) {
    if (is.null(m) || !(coef_name %in% names(coef(m)))) {
      return(tibble(estimate = NA_real_, se = NA_real_, p = NA_real_))
    }
    tibble(estimate = unname(coef(m)[coef_name]), se = unname(se(m)[coef_name]), p = unname(pvalue(m)[coef_name]))
  }
  v <- get_one(model_list$vessel)
  p <- get_one(model_list$primary)
  w <- get_one(model_list$twoway)
  tibble(
    model = label, coefficient = coef_name,
    vessel.est = v$estimate, vessel.se = v$se, vessel.stars = stars(v$p),
    primary.est = p$estimate, primary.se = p$se, primary.stars = stars(p$p),
    twoway.est = w$estimate, twoway.se = w$se, twoway.stars = stars(w$p)
  )
}

summary_tbl <- bind_rows(
  extract_row(t10, "shock", "Table 10"),
  extract_row(t11a, "shock", "Table 11 (current only)"),
  extract_row(t11b, "shock", "Table 11 (current, joint)"),
  extract_row(t11b, "shock.future", "Table 11 (future, joint)"),
  extract_row(t12, "shock", "Table 12"),
  extract_row(t12, "overlap.with.primary", "Table 12"),
  extract_row(t12, "shock:overlap.with.primary", "Table 12"),
  extract_row(t13_1, "overlap.with.primary", "Table 13 col 1"),
  extract_row(t13_2, "net.sim.z", "Table 13 col 2"),
  extract_row(t13_3, "overlap.with.primary", "Table 13 col 3"),
  extract_row(t13_3, "net.sim.z", "Table 13 col 3"),
  extract_row(t13_3, "shock:overlap.with.primary", "Table 13 col 3"),
  extract_row(t13_3, "shock:net.sim.z", "Table 13 col 3")
) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

cat("\n=== Consolidated summary, every headline coefficient under all three clustering choices ===\n")
print(summary_tbl, n = Inf, width = Inf)

cat("\nReading rule. If a coefficient keeps its stars under vessel clustering (the published convention)",
    "but loses them under primary.fishery or two-way clustering, that coefficient's published",
    "significance was resting on an inference assumption the Moulton problem invalidates, and the",
    "writeup needs to either report the more conservative clustering or explicitly justify vessel",
    "clustering as sufficient (e.g. if the primary.fishery cluster count is themselves too thin to",
    "trust, printed in Section 2 above).\n")

cat("diagnostic_primary_fishery_clustering.R done\n")
