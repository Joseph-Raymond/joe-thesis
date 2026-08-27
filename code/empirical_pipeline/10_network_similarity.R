# Chapter 3 empirical pipeline, cross-fishery co-participation network
#
# Table 13.  Extends Table 12 (09_seasonal_overlap.R) with a second,
#            independent similarity measure between a held-but-non-primary
#            fishery and the vessel's predetermined primary fishery, this
#            time built from revealed co-participation (gear/region/species,
#            "already rigged and permitted for") rather than the calendar.
#            Three columns on one shared sample, seasonal overlap alone,
#            network similarity alone, and both together.
# Table 14.  Optional. Top-10 and bottom-5 fishery pairs by network
#            similarity, with seasonal overlap alongside them, a printed
#            face-validity check on what the new measure is actually picking
#            up.
#
# Why this exists. writeup/chapter3_writeup.tex's Table 12 discussion (the
# overlap.with.primary main effect, +0.0683) can only speculate that a
# fleet-wide pooled weekly calendar signature is proxying for gear/region/
# species similarity rather than literal scheduling conflict. This script
# tests that speculation directly by building the thing it is speculating
# about, a co-participation network in the style of Kroetz et al. (2019)
# and Addicott et al. (2018), who build weighted adjacency matrices from
# Alaska cross-fishery permitting patterns, edge weight equal to the number
# of individuals permitted in both connected fisheries. This script follows
# their CONSTRUCTION, not their paper, no closeness centrality, no
# shortest-path distance, no community detection, none of that graph
# machinery is needed to answer a pairwise similarity question, and none of
# it is built here.
#
# HELD, not FISHED. The network comes from the permit-register side of
# vessel_fishery_year (held == TRUE), not the landings side, for three
# reasons. It matches chapter3_plan.md Section 0.5 and Chapter3_outline.md
# Section 5's own language ("permitted in both connected fisheries"). It
# matches Addicott et al., who build off permit holders, not landings. And
# most importantly, Table 13's outcome (activated) is a FISHED indicator,
# so building the regressor off the HELD margin keeps it structurally
# separate from the outcome, a landings-based network would not have that
# separation.
#
# VESSELS, not permit holders. Addicott et al. use individuals. This script
# uses Vessel.ADFG.Number, matching every other vessel-level object in this
# chapter (Table 10, 11, 12 are all vessel-level, the fixed effect is
# Vessel.ADFG.Number). 01_build_panel.R does build an owner-level analogue
# (owner_fishery_year, keyed on File.Number, the closer match to Addicott's
# permit holders), it is not used here, matching this regression's own unit
# of observation matters more than replicating Addicott exactly.
#
# ALL YEARS POOLED, one fixed network, not a per-year object, same reasoning
# 09_seasonal_overlap.R gives for its seasonal signature. A fishery's
# adjacency to other fisheries is treated as a structural characteristic,
# not something that should vary year to year, and pooling across the whole
# panel means no single vessel's own switching meaningfully drives the
# network used to explain that same vessel's own switching (a single vessel
# is a negligible share of a fishery's ever-holder set pooled across the
# ENTIRE panel, so unlike the Table 10 shock, no leave-one-out adjustment is
# needed here, see Section 1 below for the size floor that keeps this true).
#
# The sample-selection point, stated plainly so it is not mistaken for
# circularity. Table 13's sample (inherited from activation_data) conditions
# on vessel i having held fishery j in t-1 and on j not being i's primary,
# so every observation is, by construction, a vessel holding both j and its
# primary. The network regressor is therefore correlated with SAMPLE
# MEMBERSHIP. It is not correlated with the OUTCOME by construction, the
# outcome is whether the vessel FISHED j, which the held-based network never
# touches. What varies across observations is how common the (j, primary)
# combination is FLEET-WIDE, not whether this particular vessel has it.
#
# Reads intermediate data/ch3_panel.rdata (vessel_fishery_year), intermediate
# data/ch3_activation.rdata (activation_data, saved by
# 08_state_contingent_activation.R Section 6), and intermediate
# data/ch3_seasonal_overlap.rdata (overlap_long, saved by
# 09_seasonal_overlap.R Section 4). Needs no raw ticket reload at all, unlike
# 08 and 09, everything here comes off the saved panel, so this script runs
# in seconds. Writes table13_activation_by_network_similarity.tex and
# table14_top_co_permitted_pairs.tex to Chpt3/output/tables/. Writes no new
# intermediate data, no other script reads from this one.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_fishery_year")) load(panel_path)

activation_path <- file.path(intermediate_dir, "ch3_activation.rdata")
if (!exists("activation_data")) load(activation_path)

seasonal_overlap_path <- file.path(intermediate_dir, "ch3_seasonal_overlap.rdata")
if (!exists("overlap_long")) load(seasonal_overlap_path)

# ============================================================================
# 1. Network construction (held-based, all years pooled)
# ============================================================================

held_pairs <- vessel_fishery_year %>%
  filter(held) %>%
  distinct(Vessel.ADFG.Number, Fishery)

cat("held_pairs, distinct vessels:", n_distinct(held_pairs$Vessel.ADFG.Number),
    " distinct fisheries:", n_distinct(held_pairs$Fishery), "\n")

fishery_size <- held_pairs %>%
  count(Fishery, name = "N_f")

# A fishery must have at least this many distinct ever-holding vessels to
# enter the network. Mirrors MIN_FISHERY_WEEKS in 09_seasonal_overlap.R, a
# Jaccard built on a fishery with two ever-holders is degenerate, and this is
# also where the own-vessel contribution the no-leave-one-out argument above
# relies on stops being negligible. CHECK on real data, a judgment call.
MIN_NETWORK_VESSELS <- 10

fishery_size_filtered <- fishery_size %>% filter(N_f >= MIN_NETWORK_VESSELS)
surviving_fisheries <- fishery_size_filtered$Fishery

held_pairs_filtered <- held_pairs %>% filter(Fishery %in% surviving_fisheries)

cat("Fisheries surviving MIN_NETWORK_VESSELS >=", MIN_NETWORK_VESSELS, ":",
    length(surviving_fisheries), "of", nrow(fishery_size),
    " (", nrow(fishery_size) - length(surviving_fisheries), "removed)\n")
cat("held_pairs after the floor, distinct vessels:", n_distinct(held_pairs_filtered$Vessel.ADFG.Number),
    " distinct fisheries:", n_distinct(held_pairs_filtered$Fishery), "\n")

# Size guard. The self-join below produces sum_v(number of fisheries v ever
# held)^2 rows before count() collapses it, well under a million normally
# given a typical vessel holds a handful of distinct fisheries. Printed so a
# data pathology shows up before the join runs, not after. The documented
# fallback if this number is large is a dense crossprod() of a
# fishery-by-vessel 0/1 matrix (same C_jk in one line), not implemented here,
# see the design doc Section 3.2, do not reach for it unless this diagnostic
# actually says to.
fisheries_per_vessel <- held_pairs_filtered %>% count(Vessel.ADFG.Number, name = "n.fisheries")
max_fisheries_per_vessel <- max(fisheries_per_vessel$n.fisheries)
presplit_rows <- sum(fisheries_per_vessel$n.fisheries^2)

cat("Max distinct (surviving) fisheries ever held by a single vessel:", max_fisheries_per_vessel, "\n")
cat("Self-join row count before count() collapses it:", presplit_rows, "\n")
if (presplit_rows > 2e6) {
  cat("  NOTE, this exceeds 2 million rows, the self-join below may be slow.",
      "Consider the crossprod() fallback described in the header before rerunning.\n")
}

# Every vessel's own fisheries paired against themselves, ordered pairs plus
# the diagonal. suffix = c(".A", ".B") gives Fishery.A/Fishery.B directly,
# matching overlap_long's own column names so this joins into activation_data
# with the identical key pattern 09_seasonal_overlap.R already uses.
held_pairs_self <- held_pairs_filtered %>%
  inner_join(held_pairs_filtered, by = "Vessel.ADFG.Number",
             suffix = c(".A", ".B"), relationship = "many-to-many")

co_vessels_long <- held_pairs_self %>%
  count(Fishery.A, Fishery.B, name = "co.vessels")

# complete() over the full surviving-fishery grid before computing Jaccard,
# so a genuine zero (two fisheries with disjoint ever-holder sets) is
# distinguishable from a genuine absence (a fishery that failed the
# MIN_NETWORK_VESSELS floor, or never appears in the held panel at all).
# Only surviving_fisheries enters the grid, so a pair involving an excluded
# fishery correctly gets no row here at all rather than a fabricated zero.
network_long <- co_vessels_long %>%
  complete(Fishery.A = surviving_fisheries, Fishery.B = surviving_fisheries,
           fill = list(co.vessels = 0)) %>%
  left_join(fishery_size_filtered %>% rename(Fishery.A = Fishery, n.vessels.A = N_f), by = "Fishery.A") %>%
  left_join(fishery_size_filtered %>% rename(Fishery.B = Fishery, n.vessels.B = N_f), by = "Fishery.B") %>%
  mutate(
    net.similarity = co.vessels / (n.vessels.A + n.vessels.B - co.vessels),
    # Ochiai (cosine) variant, printed-only robustness in Section 5 below,
    # design doc Section 2.5. Gentler than Jaccard on size-asymmetric pairs,
    # a small specialty fishery every one of whose holders also holds
    # Bristol Bay salmon scores near zero on Jaccard but not on Ochiai.
    net.ochiai = co.vessels / sqrt(n.vessels.A * n.vessels.B)
  )

cat("network_long, fishery pairs with a computable Jaccard similarity:", nrow(network_long), "\n")

# Section 3.4 join-safety check. 01_build_panel.R applies strip_fishery_space()
# to the TICKET-side CFEC.Permit.Fishery but never to the PERMIT-REGISTER-side
# Fishery column, this network is built from register-derived (held) codes and
# joined against primary.fishery, which is ticket-derived. If the two ever
# disagree on whitespace or formatting the join below returns NA for every
# primary fishery and the regression would silently lose its entire sample.
# The pipeline evidently already works (Table 10/11/12 exist), so the codes
# must in fact agree, checked directly rather than assumed.
MIN_CODE_MATCH_RATE <- 0.05   # "near zero" threshold for the stop() below, not a judgment call about the network itself

code_match_rate <- mean(activation_data$primary.fishery %in% surviving_fisheries)
cat("Share of activation_data rows whose primary.fishery matches a fishery code present",
    "in the filtered held-based network:", round(code_match_rate, 4), "\n")

if (code_match_rate < MIN_CODE_MATCH_RATE) {
  stop(
    "Section 3.4 join-safety check failed, only ", round(100 * code_match_rate, 2),
    "% of activation_data rows have a primary.fishery present in the held-based network. ",
    "This is the exact failure mode the design doc warns about, register-side Fishery codes ",
    "and ticket-side Fishery codes disagreeing on whitespace/formatting, which would otherwise ",
    "silently produce an empty regression rather than an error. Fix the code-cleaning mismatch ",
    "(e.g. apply strip_fishery_space() to the register-side Fishery column in 01_build_panel.R) ",
    "before proceeding, do not just raise MIN_CODE_MATCH_RATE."
  )
}

# ============================================================================
# 2. Descriptive diagnostics and Table 14 (optional, built per the design
#    doc's default)
# ============================================================================

cat("Quantiles of N_f (ever-holding vessels) across the", nrow(fishery_size_filtered), "surviving fisheries\n")
print(quantile(fishery_size_filtered$N_f, probs = c(0, .1, .25, .5, .75, .9, 1)))

# Section 5.1's one-number test, computed across ALL fishery pairs in
# network_long, printed before any regression. The estimation-sample version
# is computed and printed alongside it in Section 3 below, once
# activation_net exists.
all_pairs <- network_long %>%
  left_join(overlap_long, by = c("Fishery.A", "Fishery.B")) %>%
  filter(is.finite(net.similarity), is.finite(seasonal.overlap))

cor_all_pairs <- cor(all_pairs$net.similarity, all_pairs$seasonal.overlap)
cat("cor(net.similarity, seasonal.overlap), ALL", nrow(all_pairs),
    "fishery pairs in network_long (printed before any regression):", round(cor_all_pairs, 4), "\n")

# Preview of the Table 13 estimation sample's DISTINCT (Fishery,
# primary.fishery) pairs, built here rather than waiting for activation_net
# in Section 3. This is exact, not approximate, because activation_data was
# already filtered to is.finite(shock) when 08_state_contingent_activation.R
# built it, so the only two additional filters Section 3 applies below,
# is.finite(overlap.with.primary) and is.finite(net.similarity), are both
# PAIR-level properties, fixed for a given (Fishery, primary.fishery)
# combination regardless of which vessel-year row carries it. The distinct
# pairs here are therefore identical to the distinct pairs activation_net
# will carry, checked explicitly in Section 3 below rather than just assumed.
estimation_sample_pairs <- activation_data %>%
  distinct(Fishery, primary.fishery) %>%
  left_join(overlap_long, by = c("Fishery" = "Fishery.A", "primary.fishery" = "Fishery.B")) %>%
  left_join(network_long, by = c("Fishery" = "Fishery.A", "primary.fishery" = "Fishery.B")) %>%
  filter(is.finite(seasonal.overlap), is.finite(net.similarity)) %>%
  rename(Fishery.A = Fishery, Fishery.B = primary.fishery)

cat("Distinct (Fishery, primary.fishery) pairs entering the Table 13 estimation sample:",
    nrow(estimation_sample_pairs), "\n")

# Section 6 face validity, top and bottom pairs. Because CFEC codes encode
# species-gear-area, a reader can check by inspection whether the top pairs
# are obviously same-species or same-region combinations.
top_pairs <- estimation_sample_pairs %>% arrange(desc(net.similarity)) %>% slice_head(n = 10)
bottom_pairs <- estimation_sample_pairs %>% arrange(net.similarity) %>% slice_head(n = 5)

cat("Top 10 fishery pairs by net.similarity (Section 6 face-validity check)\n")
print(top_pairs %>% select(Fishery.A, Fishery.B, co.vessels, n.vessels.A, n.vessels.B, net.similarity, seasonal.overlap))

cat("Bottom 5 fishery pairs by net.similarity\n")
print(bottom_pairs %>% select(Fishery.A, Fishery.B, co.vessels, n.vessels.A, n.vessels.B, net.similarity, seasonal.overlap))

table14_data <- bind_rows(
  top_pairs %>% mutate(group = "Top 10"),
  bottom_pairs %>% mutate(group = "Bottom 5")
) %>%
  select(group, Fishery.A, Fishery.B, co.vessels, n.vessels.A, n.vessels.B, net.similarity, seasonal.overlap)

# digits, first slot is xtable's implicit row-name column (ignored), then one
# slot per table14_data column, integer columns at 0 decimals, the two
# similarity measures at 4 (matching table2_data_quality.tex's reasoning,
# without explicit digits xtable defaults to 2, which would flatten most of
# this distribution to 0.00 given how right-skewed Jaccard is, Section 2.4).
print(xtable(table14_data, caption = "Top and bottom fishery pairs by co-participation network similarity",
             label = "tab:ch3-table14", digits = c(0, 0, 0, 0, 0, 0, 0, 4, 4)),
      file = file.path(table_dir, "table14_top_co_permitted_pairs.tex"),
      include.rownames = FALSE)

cat("Wrote table14_top_co_permitted_pairs.tex\n")

# Section 6 shared-attribute means. split_fishery_code() (00_setup.R) splits
# a code into species letter, two-digit gear code, and region letter. A
# simplified, tabulated version of Addicott et al.'s equation (2) logit of
# cluster membership on shared gear/area/species, no new data, existing
# helper, tests the writeup's "calendar similarity proxies for gear/region/
# species similarity" conjecture in its own words without needing a
# regression at all.
attrs_A <- split_fishery_code(estimation_sample_pairs$Fishery.A) %>%
  transmute(species.A = species, gear.A = gear, region.A = region)
attrs_B <- split_fishery_code(estimation_sample_pairs$Fishery.B) %>%
  transmute(species.B = species, gear.B = gear, region.B = region)

pair_attributes <- bind_cols(estimation_sample_pairs, attrs_A, attrs_B) %>%
  mutate(
    shared.species = species.A == species.B,
    shared.gear    = gear.A == gear.B,
    shared.region  = region.A == region.B
  )

cat("Shared-attribute means (Section 6), mean net.similarity / mean seasonal.overlap by shared attribute\n")
for (attr_name in c("shared.species", "shared.gear", "shared.region")) {
  attr_summary <- pair_attributes %>%
    group_by(.data[[attr_name]]) %>%
    summarise(mean.net.similarity = mean(net.similarity), mean.seasonal.overlap = mean(seasonal.overlap),
              n = n(), .groups = "drop")
  cat(" ", attr_name, "\n")
  print(attr_summary)
}

# ============================================================================
# 3. Build activation_net
# ============================================================================
#
# All three Table 13 columns are fit on activation_net, the SAME sample.
# 08_state_contingent_activation.R Section 5 documents at length why a
# coefficient difference across columns must come from the regressor set and
# never from the row set, the same discipline applies here.

n_activation_data <- nrow(activation_data)

activation_net_step1 <- activation_data %>%
  left_join(overlap_long, by = c("Fishery" = "Fishery.A", "primary.fishery" = "Fishery.B")) %>%
  rename(overlap.with.primary = seasonal.overlap) %>%
  filter(is.finite(shock), is.finite(overlap.with.primary))

n_step1 <- nrow(activation_net_step1)

activation_net <- activation_net_step1 %>%
  left_join(network_long, by = c("Fishery" = "Fishery.A", "primary.fishery" = "Fishery.B")) %>%
  filter(is.finite(net.similarity))

n_step2 <- nrow(activation_net)

cat("Row counts, activation_data to activation_net\n")
cat("  activation_data:", n_activation_data, "\n")
cat("  + finite shock and overlap.with.primary:", n_step1, "\n")
cat("  + finite net.similarity:", n_step2, "\n")
cat("  retention rate (activation_net / activation_data):", round(n_step2 / n_activation_data, 4), "\n")

# Confirms the Section 2 preview's distinct-pairs claim held exactly, not
# just approximately.
n_pairs_in_activation_net <- activation_net %>% distinct(Fishery, primary.fishery) %>% nrow()
cat("Distinct pairs in activation_net:", n_pairs_in_activation_net,
    " (Section 2 preview said", nrow(estimation_sample_pairs), ")\n")

# Section 2.4. net.sim.z is z-scored on this final estimation sample, mean
# and sd computed once so all three Table 13 columns use the identical
# variable. overlap.with.primary stays RAW, so its column-1 coefficient
# remains directly comparable to the published Table 12 value of +0.0683,
# which is the entire point of the exercise.
net_sim_mean <- mean(activation_net$net.similarity)
net_sim_sd   <- sd(activation_net$net.similarity)

activation_net <- activation_net %>%
  mutate(
    net.sim.z = (net.similarity - net_sim_mean) / net_sim_sd,
    # z-scored the same way, used only by the Section 5 printed-only Ochiai
    # robustness refit below, never tabled.
    net.ochiai.z = (net.ochiai - mean(net.ochiai)) / sd(net.ochiai)
  )

cat("Quantiles of net.similarity on the Table 13 estimation sample\n")
print(quantile(activation_net$net.similarity, probs = c(0, .1, .25, .5, .75, .9, .99, 1)))
cat("Mean net.similarity:", round(net_sim_mean, 4), " sd:", round(net_sim_sd, 4), "\n")
cat("Mean overlap.with.primary (raw, not standardized):", round(mean(activation_net$overlap.with.primary), 4), "\n")

net_sim_p99 <- quantile(activation_net$net.similarity, 0.99)
top1pct_share <- sum(activation_net$net.similarity[activation_net$net.similarity >= net_sim_p99]) /
  sum(activation_net$net.similarity)
cat("Share of total net.similarity mass held by observations at or above the 99th percentile:",
    round(top1pct_share, 4), "\n")
if (top1pct_share > 0.10) {
  cat("  NOTE, exceeds 10 percent, a handful of high-similarity pairs dominate the distribution.",
      "A rank or log transform of net.similarity is worth trying next, not implemented in this version.\n")
}

# Section 5.1/5.6. The one-number test that comes before the regression, plus
# the collinearity contingency that determines how column 3 can be read.
COLLINEARITY_FLAG_THRESHOLD <- 0.7   # design doc Section 5.6, not a judgment call, this exact value is specified there

cor_estimation_sample <- cor(activation_net$net.similarity, activation_net$overlap.with.primary)
cat("cor(net.similarity, overlap.with.primary), Table 13 estimation sample:", round(cor_estimation_sample, 4), "\n")
cat("cor(net.similarity, seasonal.overlap), all pairs in network_long (from Section 2):", round(cor_all_pairs, 4), "\n")
if (abs(cor_estimation_sample) > COLLINEARITY_FLAG_THRESHOLD) {
  cat("  Section 5.6 contingency TRIGGERED, |cor| exceeds", COLLINEARITY_FLAG_THRESHOLD,
      ", column 3's separately-identified coefficients are fragile, read columns 1 and 2 side by",
      "side (standalone coefficient and within-R2 each) rather than column 3's partial effects.\n")
} else {
  cat("  Below the Section 5.6 collinearity flag of", COLLINEARITY_FLAG_THRESHOLD,
      ", column 3's partial coefficients are safe to read as a decomposition.\n")
}

# 08_state_contingent_activation.R lines 260-268's identification check,
# rerun here on activation_net. Binds hardest on the shock INTERACTIONS,
# shock varies only at (vessel, year) via each vessel's fixed primary
# fishery, while the pair-level main effects (overlap.with.primary,
# net.sim.z) have richer variation than that shock does.
identification_check_net <- activation_net %>%
  group_by(fishery.year) %>%
  summarise(n.distinct.primary = n_distinct(primary.fishery), n.obs = n(), .groups = "drop")

identifying_cells_net <- identification_check_net %>% filter(n.distinct.primary > 1)

cat("Table 13 identification, fishery-year cells with more than one distinct primary fishery:",
    nrow(identifying_cells_net), "of", nrow(identification_check_net),
    ", covering", sum(identifying_cells_net$n.obs), "of", nrow(activation_net), "observations\n")
cat("  This binds hardest on the shock interactions, see the comment above this block\n")

# Section 8.4. Table 13 is its own exhibit rather than overwriting Table 12
# because its sample is narrower (the intersection where seasonal overlap AND
# network similarity are both computable). Column 1 is refit on this shared
# sample, not copied from the published table, this rebuilds the ORIGINAL
# (wider) Table 12 sample only for the comparison message below and for the
# Section 5 reference refit, table12_activation_by_seasonal_overlap.tex
# itself is never touched.
activation_data_overlap <- activation_data %>%
  left_join(overlap_long, by = c("Fishery" = "Fishery.A", "primary.fishery" = "Fishery.B")) %>%
  rename(overlap.with.primary = seasonal.overlap) %>%
  filter(is.finite(overlap.with.primary))

cat("Table 13 column 1 sample:", n_step2, " published Table 12 sample:", nrow(activation_data_overlap),
    " retention:", round(100 * n_step2 / nrow(activation_data_overlap), 2), "percent\n")

# ============================================================================
# 4. Table 13. Three models, seasonal overlap / network similarity / both
# ============================================================================
#
# Fixed effects Vessel.ADFG.Number + fishery.year in all three, identical to
# Table 10 and Table 12. cluster = ~Vessel.ADFG.Number on all three, matching
# 09_seasonal_overlap.R's model_table12 call, which now passes this
# explicitly rather than relying on fixest's default (checked directly
# against the real generated table12_activation_by_seasonal_overlap.tex, the
# unset default resolved to IID, not vessel-clustered, a mistake fixed there
# and mirrored here so column 1 still reproduces Table 12 and so this table's
# own dyadic, repeated-vessel structure gets the same correction).

model_table13_col1 <- feols(activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year,
                             data = activation_net, cluster = ~Vessel.ADFG.Number)
model_table13_col2 <- feols(activated ~ shock * net.sim.z | Vessel.ADFG.Number + fishery.year,
                             data = activation_net, cluster = ~Vessel.ADFG.Number)
model_table13_col3 <- feols(activated ~ (shock * overlap.with.primary) + (shock * net.sim.z) |
                               Vessel.ADFG.Number + fishery.year,
                             data = activation_net, cluster = ~Vessel.ADFG.Number)

# dict relabels net.sim.z for the printed/exported table only, the column
# itself stays net.sim.z so nothing else in this script needs to change if
# the display label is tweaked later. overlap.with.primary and shock are
# left undicted on purpose, matching how they render, undicted, in the
# published table12_activation_by_seasonal_overlap.tex.
table13_dict <- c(net.sim.z = "Network similarity (z-scored)")

etable(
  model_table13_col1, model_table13_col2, model_table13_col3,
  headers = c("Seasonal overlap", "Network similarity", "Both"),
  dict = table13_dict,
  tex = TRUE,
  file = file.path(table_dir, "table13_activation_by_network_similarity.tex"),
  replace = TRUE
)

print(etable(model_table13_col1, model_table13_col2, model_table13_col3,
             headers = c("Seasonal overlap", "Network similarity", "Both"), dict = table13_dict))

cat("Wrote table13_activation_by_network_similarity.tex\n")

# Table note, stated here rather than relying on an unverified etable notes=
# argument. overlap.with.primary is RAW (bounded [0,1], directly comparable
# to the published Table 12 coefficient of +0.0683), net.sim.z is z-scored on
# this activation_net sample (mean/sd of the raw net.similarity printed
# above), so its coefficient reads per standard deviation of network
# similarity, not per unit, and the shock interaction reads as "per sd of
# shock per sd of similarity."
cat("Table 13 note, overlap.with.primary is raw, net.sim.z is z-scored (see mean/sd printed above),",
    "these two regressors are NOT on the same scale, do not compare their coefficients directly\n")

# Section 4.4 sign predictions, checked against the fitted coefficients.
coef1 <- coef(model_table13_col1)
coef3 <- coef(model_table13_col3)

cat("Section 4.4 sign check\n")
cat("  net.sim.z main effect (col 3), predicted LARGE POSITIVE, got:", round(coef3[["net.sim.z"]], 4), "\n")
cat("  shock:net.sim.z (col 3), predicted NEGATIVE, got:", round(coef3[["shock:net.sim.z"]], 4), "\n")
cat("  overlap.with.primary, col 1:", round(coef1[["overlap.with.primary"]], 4),
    " -> col 3:", round(coef3[["overlap.with.primary"]], 4),
    " (predicted to shrink substantially, this is the puzzle test)\n")
cat("  shock:overlap.with.primary, col 1:", round(coef1[["shock:overlap.with.primary"]], 4),
    " -> col 3:", round(coef3[["shock:overlap.with.primary"]], 4),
    " (predicted to survive near +0.0125, this is the coefficient with the most at stake, see design doc Section 5.4)\n")

# Section 4.4's "trap for the writer." Column 1's shock coefficient is the
# shock slope at overlap = 0. Column 3's own shock coefficient is the slope
# at overlap = 0 AND net.sim.z = 0 (mean similarity, since z-scored). These
# are not the same object and must not be compared naively, the general
# slope needs both interaction terms evaluated at the moderators' sample
# means.
overlap_mean_net <- mean(activation_net$overlap.with.primary)
overlap_sd_net   <- sd(activation_net$overlap.with.primary)
netsimz_mean_net <- mean(activation_net$net.sim.z)   # ~0 by construction, printed as a check on the z-scoring itself
netsimz_sd_net   <- sd(activation_net$net.sim.z)     # ~1 by construction

slope_shock_col3_at_means <- coef3[["shock"]] +
  coef3[["shock:overlap.with.primary"]] * overlap_mean_net +
  coef3[["shock:net.sim.z"]] * netsimz_mean_net

cat("Moderator sample moments on activation_net\n")
cat("  overlap.with.primary, mean:", round(overlap_mean_net, 4), " sd:", round(overlap_sd_net, 4), "\n")
cat("  net.sim.z, mean:", round(netsimz_mean_net, 4), "(should be ~0 by construction) sd:",
    round(netsimz_sd_net, 4), "(should be ~1)\n")
cat("Column 1 shock coefficient (slope at overlap = 0):", round(coef1[["shock"]], 4), "\n")
cat("Column 3 shock coefficient (slope at overlap = 0, net.sim.z = 0):", round(coef3[["shock"]], 4), "\n")
cat("Column 3 shock slope evaluated at BOTH sample-mean moderators (Section 4.4 formula):",
    round(slope_shock_col3_at_means, 4), "\n")
cat("  These three numbers are different objects, do not compare column 1's and column 3's own",
    "shock coefficients directly, use the sample-mean-evaluated slope for that comparison\n")

# ============================================================================
# 5. Printed-only robustness, not written as tables
# ============================================================================

# 5.1. Ochiai (cosine) normalization in place of Jaccard, design doc Section
# 2.5. If the coefficient signs flip between Jaccard and Ochiai the author
# needs to know before the writeup is drafted.
model_ochiai <- feols(activated ~ (shock * overlap.with.primary) + (shock * net.ochiai.z) |
                         Vessel.ADFG.Number + fishery.year,
                       data = activation_net, cluster = ~Vessel.ADFG.Number)

cat("Section 5.1, printed-only Ochiai-normalization robustness (not written as a table)\n")
print(etable(model_ochiai, headers = "Both, Ochiai instead of Jaccard (printed only)"))

coef_ochiai <- coef(model_ochiai)
ochiai_main_sign_match <- sign(coef_ochiai[["net.ochiai.z"]]) == sign(coef3[["net.sim.z"]])
ochiai_int_sign_match  <- sign(coef_ochiai[["shock:net.ochiai.z"]]) == sign(coef3[["shock:net.sim.z"]])
cat("  Jaccard vs Ochiai, main-effect sign match:", ochiai_main_sign_match,
    " interaction sign match:", ochiai_int_sign_match, "\n")
if (!ochiai_main_sign_match || !ochiai_int_sign_match) {
  cat("  WARNING, at least one sign flips between the Jaccard and Ochiai normalizations,",
      "read the network-similarity result as normalization-sensitive before drafting the writeup\n")
}

# 5.2. Two-way clustered sensitivity for column 3, design doc Section 4.3.
# Regressors are pair-level, dyadic dependence (the same fishery pair
# appearing for many vessels) is a reasonable thing to be asked about.
model_table13_col3_cluster <- feols(activated ~ (shock * overlap.with.primary) + (shock * net.sim.z) |
                                       Vessel.ADFG.Number + fishery.year,
                                     data = activation_net, cluster = ~ Vessel.ADFG.Number + Fishery)

cat("Section 5.2, printed-only two-way clustered sensitivity for column 3\n")
print(etable(model_table13_col3_cluster, headers = "Both, clustered on vessel and fishery (printed only)"))

# 5.3. The published Table 12 model, refit on its own ORIGINAL (wider)
# sample, design doc Section 8.4, so the author can confirm the coefficients
# barely move between this reference refit and the real column 1
# (fit on the narrower activation_net above).
model_table12_refit <- feols(activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year,
                              data = activation_data_overlap, cluster = ~Vessel.ADFG.Number)

cat("Section 5.3, printed-only Table 12 spec refit on its own original sample, for reference\n")
print(etable(model_table12_refit, headers = "Table 12 spec, original sample (printed only)"))

# ============================================================================
# 5.4-5.7. Diagnostics for the shock x net.sim.z sign miss and the column 3
# collinearity claim, added after a deep review of the first real run of
# this script. Section 4.4 predicted shock x net.sim.z NEGATIVE, on the
# theory that a vessel already rigged and permitted for a similar
# alternative should find it CHEAPER to switch into on a bad primary year.
# The real coefficient came back POSITIVE (0.0091* alone, col 2 above), the
# opposite sign. None of what follows is written as a table, this is
# entirely about deciding whether that miss needs an economic story or not
# before any of it goes in the writeup.
# ============================================================================

# 5.4. Decisive test. net.sim.z is co.vessels / (n.vessels.A + n.vessels.B -
# co.vessels), so holding co.vessels fixed, similarity is mechanically
# LARGER when the primary fishery (n.vessels.B here, since primary.fishery
# is joined as Fishery.B throughout this script) has fewer ever-holders.
# The activation shock is a leave-one-out mean standardized against that
# SAME primary fishery's own multi-year series
# (08_state_contingent_activation.R, vessel_year_shock), and its sampling
# noise scales roughly as 1/n.remaining.vessels, so the shock is a noisier
# measure of the true state exactly where similarity is high. Classical
# measurement error attenuates a slope toward zero, and attenuation
# concentrated at high similarity is arithmetically indistinguishable from a
# positive shock x similarity interaction with no behavioral content at
# all. n.remaining.vessels survives 08_state_contingent_activation.R's own
# joins into activation_data and from there into activation_net, checked
# directly below rather than assumed.
if (!("n.remaining.vessels" %in% names(activation_net))) {
  stop("n.remaining.vessels is not present on activation_net, the Section 5.4 size-control ",
       "diagnostic cannot run. Check whether 08_state_contingent_activation.R's join at its ",
       "line ~241 (left_join(vessel_year_shock %>% select(-primary.fishery), ...)) still carries ",
       "this column through to the saved activation_data.")
}

cat("Vessel-fishery-years with a usable n.remaining.vessels on activation_net:",
    sum(is.finite(activation_net$n.remaining.vessels) & activation_net$n.remaining.vessels > 0),
    "of", nrow(activation_net), "\n")

# Second deep review caught two problems with the first cut of this model.
# (1) It was labeled as column 3 plus a control but the formula never
# actually included the overlap.with.primary terms, so the printed
# comparison against coef3 was silently benchmarked against the wrong
# baseline (col 2's shock:net.sim.z, not col 3's). Fixed by actually
# including the overlap terms below, so this now really is col 3 plus a
# control and the coef3 comparison is apples to apples.
# (2) n.remaining.vessels (08_state_contingent_activation.R, a LANDINGS-side
# annual count) is a proxy one step removed from the object that actually
# drives the mechanism under test. The Jaccard denominator that would create
# the artifact runs through n.vessels.B, the REGISTER-side, all-years-pooled
# ever-holder count of the PRIMARY fishery (Section 1 above, joined onto
# activation_net as part of network_long). Its main effect is fully absorbed
# by Vessel.ADFG.Number (primary.fishery, and so n.vessels.B, is fixed per
# vessel), but the shock interaction is still identified and is the sharper
# version of this test, added alongside the original proxy rather than
# replacing it.
model_size_control <- feols(
  activated ~ (shock * overlap.with.primary) + (shock * net.sim.z) +
    shock:log(n.remaining.vessels) + log(n.remaining.vessels) +
    shock:log(n.vessels.B) |
    Vessel.ADFG.Number + fishery.year,
  data = activation_net, cluster = ~Vessel.ADFG.Number
)

cat("Section 5.4, printed-only decisive test, shock x net.sim.z with two shock-precision controls\n")
print(etable(model_size_control, headers = "Both plus shock-precision controls (printed only)"))

coef_size_control <- coef(model_size_control)
cat("  shock x net.sim.z, WITHOUT size control (col 3 above):", round(coef3[["shock:net.sim.z"]], 4), "\n")
cat("  shock x net.sim.z, WITH size controls:", round(coef_size_control[["shock:net.sim.z"]], 4), "\n")
cat("  shock x log(n.vessels.B), the sharper mechanism-relevant control, coefficient:",
    round(coef_size_control[["shock:log(n.vessels.B)"]], 4), "\n")
if (abs(coef_size_control[["shock:net.sim.z"]]) < abs(coef3[["shock:net.sim.z"]]) * 0.5) {
  cat("  Interaction shrinks by more than half once shock precision is controlled for,",
      "consistent with the sign miss being a measurement artifact rather than a real effect,",
      "no economic story needed for this coefficient\n")
} else {
  cat("  Interaction does NOT shrink materially once shock precision is controlled for,",
      "the measurement-artifact explanation is not supported on its own and the sign miss",
      "needs an actual economic account before it goes in the writeup\n")
}

# 5.5. The collinearity check COLLINEARITY_FLAG_THRESHOLD was actually meant
# to guard. Section 3 above computed cor() on the RAW regressors, but
# column 3's partial coefficients are only threatened by whatever
# collinearity is LEFT after the two-way fixed effects absorb their share,
# not by the raw correlation. Residualizing both regressors on the same
# Vessel.ADFG.Number + fishery.year structure the real model uses, then
# correlating the residuals, is the actual test the threshold was written
# for.
resid_overlap_net <- resid(feols(overlap.with.primary ~ 1 | Vessel.ADFG.Number + fishery.year,
                                  data = activation_net))
resid_netsimz_net <- resid(feols(net.sim.z ~ 1 | Vessel.ADFG.Number + fishery.year,
                                  data = activation_net))
cor_resid_net <- cor(resid_overlap_net, resid_netsimz_net)

cat("Section 5.5, FE-residualized correlation between overlap.with.primary and net.sim.z\n")
cat("  Raw correlation on activation_net (Section 3 above):", round(cor_estimation_sample, 4), "\n")
cat("  FE-residualized correlation (what the collinearity flag is actually meant to guard):",
    round(cor_resid_net, 4), "\n")
if (abs(cor_resid_net) > COLLINEARITY_FLAG_THRESHOLD) {
  cat("  Collinearity flag TRIGGERED on the RESIDUALIZED correlation even though the raw",
      "correlation looked safe, column 3's partial coefficients are fragile, read columns 1",
      "and 2 side by side instead of column 3's partial effects\n")
} else {
  cat("  Below the collinearity flag even net of the fixed effects, column 3's partial",
      "coefficients are safe to read as a real decomposition\n")
}

# 5.6. Quartile-binned version of shock x net.similarity, printed only. The
# linear interaction is fit on a regressor whose top 1 percent holds over
# 15 percent of its own total mass (the NOTE printed in Section 3 above), so
# a single linear coefficient there is closer to a leverage estimate off a
# handful of pairs than an average effect across the distribution unless the
# per-bin pattern is graded rather than concentrated in one bin.
#
# The first cut of this diagnostic used i(net.sim.quartile, shock, ref = 1)
# with no separate shock term, on the mistaken assumption that ref = 1 would
# make quartile 1 an omitted reference the way a factor dummy works. It does
# not, fixest's i() deletes that level's interaction column outright, which
# silently constrains quartile 1's own slope to zero rather than reporting
# a contrast against it, so the three printed coefficients were close to
# absolute per-quartile slopes with quartile 1 forced to zero, not
# differences from quartile 1 as the old comment here claimed (caught in a
# second deep review, confirmed against a calibrated simulation). Fixed
# below by reporting ALL FOUR quartiles' own absolute slopes directly
# (i(net.sim.quartile, shock), no ref, so nothing is constrained), with
# i(net.sim.quartile, ref = 1) added separately as the quartile LEVEL
# effect, since net.similarity is pair-level and only partly absorbed by
# the vessel/fishery-year fixed effects.
#
# ntile() also splits any pair whose value sits exactly on a bin boundary
# by row order rather than by value, which after the joins tracks vessel
# and year ordering, an arbitrary tie-break. cut() on explicit quantile
# breaks assigns by value instead. net.similarity is pair-level, so this
# also switches to two-way clustering (vessel + Fishery), matching Section
# 5.2's reasoning for the pooled column 3 model, since a bin's effective
# sample size is closer to its count of distinct pairs than its count of
# vessel-fishery-year rows.
net_sim_quartile_breaks <- quantile(activation_net$net.similarity, probs = c(0, .25, .5, .75, 1))

activation_net <- activation_net %>%
  mutate(net.sim.quartile = cut(net.similarity, breaks = net_sim_quartile_breaks,
                                 include.lowest = TRUE, labels = FALSE))

model_net_sim_binned <- feols(
  activated ~ i(net.sim.quartile, shock) + i(net.sim.quartile, ref = 1) |
    Vessel.ADFG.Number + fishery.year,
  data = activation_net, cluster = ~ Vessel.ADFG.Number + Fishery
)

cat("Section 5.6, printed-only quartile-binned shock x net.similarity",
    "(coefficients are each quartile's OWN absolute shock slope, all four reported, none constrained)\n")
print(etable(model_net_sim_binned, headers = "Shock slope by net.similarity quartile (printed only)"))
cat("  net.similarity quartile boundaries\n")
print(net_sim_quartile_breaks)
cat("  Distinct (Fishery, primary.fishery) pairs per quartile, the effective sample behind each slope\n")
print(activation_net %>% distinct(Fishery, primary.fishery, net.sim.quartile) %>% count(net.sim.quartile))
cat("  A monotonically increasing (less negative, or more positive) slope from quartile 1 to 4",
    "confirms the pooled linear interaction, a non-monotonic or reversed pattern would contradict it\n")

# 5.7. Top-1-percent-trimmed refit, the direct complement to 5.6. Drops the
# same observations the Section 3 NOTE flagged as holding a
# disproportionate share of net.similarity's own mass (net_sim_p99,
# computed in Section 3), and refits column 3's exact spec on what remains.
activation_net_trimmed <- activation_net %>% filter(net.similarity < net_sim_p99)

cat("Section 5.7, top-1-percent-trimmed refit, dropped",
    nrow(activation_net) - nrow(activation_net_trimmed), "of", nrow(activation_net),
    "observations at or above the 99th percentile of net.similarity\n")

model_trimmed <- feols(
  activated ~ (shock * overlap.with.primary) + (shock * net.sim.z) | Vessel.ADFG.Number + fishery.year,
  data = activation_net_trimmed, cluster = ~Vessel.ADFG.Number
)

cat("Section 5.7, printed-only top-1-percent-trimmed refit of column 3\n")
print(etable(model_trimmed, headers = "Both, top 1 percent of net.similarity dropped (printed only)"))
coef_trimmed <- coef(model_trimmed)
cat("  shock x net.sim.z, full sample (col 3 above):", round(coef3[["shock:net.sim.z"]], 4), "\n")
cat("  shock x net.sim.z, top-1-percent trimmed:", round(coef_trimmed[["shock:net.sim.z"]], 4), "\n")
cat("  shock x overlap.with.primary, full sample (col 3 above):", round(coef3[["shock:overlap.with.primary"]], 4), "\n")
cat("  shock x overlap.with.primary, top-1-percent trimmed:",
    round(coef_trimmed[["shock:overlap.with.primary"]], 4), "\n")

cat("10_network_similarity.R done\n")
