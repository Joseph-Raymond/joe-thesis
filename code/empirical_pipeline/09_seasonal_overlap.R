# Chapter 3 empirical pipeline, seasonal overlap of the HELD portfolio
#
# Table 12.  Interaction of the Section 7 activation shock with the
#            seasonal overlap between a held-but-non-primary fishery and
#            the vessel's predetermined primary fishery.
# Figure 11. The held-versus-fished wedge (Section 3), re-cut by whether a
#            held-but-unfished permit was seasonally blocked by what the
#            vessel actually fished that year, or genuinely free.
#
# Why this exists. Abbott, Sakai & Holland (2023, "Species, space and time,"
# Context_papers/) measure a fisher's TEMPORAL diversification the same way
# they measure species diversification, a Shannon index over revenue shares
# across calendar weeks. A deep-reasoning review of whether that measure
# would fit THIS chapter concluded it should not be imported directly, this
# chapter's entire structural advantage is that CFEC lets it observe the
# HELD option set alongside the realized one, and there is no "held"
# analogue for a calendar week on the West Coast data Abbott et al. use, so
# adopting their measure would throw away the one thing this chapter can do
# that they cannot. What their paper does unlock is a different, Alaska-
# specific object, since a CFEC permit is a species-gear-area right with a
# biologically and regulatorily fixed calendar attached, the SEASON of a
# HELD permit is observable ex ante, in a way that has no equivalent in
# their setting. That is what this script builds and uses, not a copy of
# their index.
#
# Two consequences follow. First, Section 3's wedge (Table 3, Figure 1) can
# be re-cut. A permit held but not fished could be a seasonal DUPLICATE (its
# season sits on top of a fishery the vessel actually fished that year, so
# one boat physically could not have fished both regardless of intent) or a
# seasonal COMPLEMENT (a genuinely free window left on the table). Second,
# it fills Table 12, chapter3_plan.md Section 9.2's request for an
# interaction that breaks the ambiguity in a Section 7 null result between
# "no option value" and "the options were too correlated/blocked to be
# worth exercising." The plan's own text names return correlation (from a
# return-covariance Figure 3, since replaced by the levels-based passive
# benchmark in 05_table4_figure3.R, so that ingredient no longer exists)
# and the Section 5 co-participation network (skipped, see
# 06_within_season_reallocation.R's header) as candidate interactions.
# Seasonal overlap needs neither, and arguably fits a discrete activation
# outcome better than either would have, it operates on the margin that
# actually PHYSICALLY prevents fishing two permits at once, not just a
# statistical correlation between their returns.
#
# Reloads the raw catch_data_temp.rdata ticket file for weekly pounds by
# fishery (fleet-wide, ALL years pooled, this is a fixed characteristic of
# a fishery's calendar, not a per-year object the way season_windows in
# 06_within_season_reallocation.R is). Reads intermediate
# data/ch3_panel.rdata (vessel_fishery_year) and intermediate
# data/ch3_activation.rdata (activation_data, saved by
# 08_state_contingent_activation.R Section 6).

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_fishery_year")) load(panel_path)

activation_path <- file.path(intermediate_dir, "ch3_activation.rdata")
if (!exists("activation_data")) load(activation_path)

# ============================================================================
# 1. Fishery seasonal signature and pairwise overlap (fleet-wide, all years)
# ============================================================================
#
# Pooled across every year in the panel, not per fishery-year, a fishery's
# SEASON is treated as a fixed calendar characteristic (which biological
# run timing and area/gear regulation mostly determine), not something that
# should vary year to year the way season_windows' per-year start/end does.
# Pooling across all years and vessels also means no single vessel's own
# switching behavior meaningfully drives the signature used to explain that
# same vessel's own behavior, unlike the Section 7 shock, which needs an
# explicit leave-one-out adjustment because a single vessel can dominate a
# single fishery-YEAR, a single vessel's landings are a negligible share of
# a fishery's landings pooled across its ENTIRE panel history, so no
# leave-one-out adjustment is needed here.
#
# Same cleaning steps as 01_build_panel.R Section 2 and the other scripts
# that reload raw tickets, duplicated for the same reason given there. As in
# 06_within_season_reallocation.R, Statistical.Week is not an actual column
# in catch_data_temp (checked directly against the real object on the
# server), it is DERIVED from Date.Landed via derive_statistical_week() in
# 00_setup.R, see the comment there for the exact definition and why.

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(
    Fishery = strip_fishery_space(CFEC.Permit.Fishery),
    Statistical.Week = derive_statistical_week(Date.Landed)
  ) %>%
  filter(Fishery != "", !is.na(Statistical.Week))

# A fishery needs landings in at least MIN_FISHERY_WEEKS distinct weeks,
# pooled across the whole panel, to have a meaningful season SHAPE rather
# than one or two landings that happen to define a degenerate "distribution."
# CHECK this threshold once run on real data, it is a judgment call.
MIN_FISHERY_WEEKS <- 3

fishery_week_pounds <- catch_data_temp %>%
  group_by(Fishery, Statistical.Week) %>%
  summarise(pounds = sum(Pounds..Detail., na.rm = TRUE), .groups = "drop")

fisheries_with_shape <- fishery_week_pounds %>%
  filter(pounds > 0) %>%
  count(Fishery, name = "n.weeks") %>%
  filter(n.weeks >= MIN_FISHERY_WEEKS) %>%
  pull(Fishery)

cat("Fisheries with a usable seasonal signature:", length(fisheries_with_shape),
    "of", n_distinct(fishery_week_pounds$Fishery), "\n")

# Wide Fishery x Statistical.Week share matrix, missing weeks (no landings
# fleet-wide that week) filled with 0. sqrt() first, then a single matrix
# product, gives every pairwise Bhattacharyya coefficient at once,
# BC(i, j) = sum_w sqrt(p_iw * q_jw) = (sqrt(M) %*% t(sqrt(M)))[i, j], a
# bounded [0, 1] overlap measure (1 = identical week-by-week distributions,
# 0 = no weeks in common), standard for comparing two probability
# distributions and cheaper here than a per-pair loop over dozens of
# fisheries.
fishery_week_wide <- fishery_week_pounds %>%
  filter(Fishery %in% fisheries_with_shape) %>%
  group_by(Fishery) %>%
  mutate(share = pounds / sum(pounds)) %>%
  ungroup() %>%
  select(Fishery, Statistical.Week, share) %>%
  pivot_wider(names_from = Statistical.Week, values_from = share, values_fill = 0)

fishery_ids <- fishery_week_wide$Fishery
share_matrix <- as.matrix(fishery_week_wide %>% select(-Fishery))
rownames(share_matrix) <- fishery_ids

sqrt_matrix <- sqrt(share_matrix)
overlap_matrix <- sqrt_matrix %*% t(sqrt_matrix)

overlap_long <- as_tibble(overlap_matrix, rownames = "Fishery.A") %>%
  pivot_longer(-Fishery.A, names_to = "Fishery.B", values_to = "seasonal.overlap")

cat("Fishery pairs with a computable seasonal overlap:", nrow(overlap_long), "\n")

# ============================================================================
# 2. Figure 11. The Section 3 wedge, re-cut by seasonal overlap
# ============================================================================
#
# For each held-but-unfished (vessel, year, Fishery), overlap.with.fished is
# the revenue-share-weighted mean seasonal overlap between that idle
# fishery and whatever the vessel DID fish that year. High overlap says the
# idle permit's season sat on top of what the vessel was already doing, one
# boat cannot fish two openings in the same weeks, so it could not have
# been fished regardless of intent, a seasonal DUPLICATE. Low overlap says
# the idle permit's season was genuinely free, a seasonal COMPLEMENT left
# unused by choice rather than by calendar conflict. This does not change
# Table 3's headline wedge number, it asks what kind of idle time makes it
# up.

held_not_fished <- vessel_fishery_year %>%
  filter(held, !fished) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery)

fished_shares <- vessel_fishery_year %>%
  filter(fished) %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  mutate(fished.share = revenue / sum(revenue)) %>%
  ungroup() %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, fished.share)

idle_overlap <- held_not_fished %>%
  inner_join(fished_shares, by = c("Vessel.ADFG.Number", "Batch.Year"),
             suffix = c(".idle", ".fished"), relationship = "many-to-many") %>%
  left_join(overlap_long, by = c("Fishery.idle" = "Fishery.A", "Fishery.fished" = "Fishery.B")) %>%
  filter(is.finite(seasonal.overlap)) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery.idle) %>%
  summarise(overlap.with.fished = weighted.mean(seasonal.overlap, w = fished.share), .groups = "drop")

# A vessel-year that fished NOTHING at all (held permits but zero revenue
# that year) has no fished.share to weight against and is correctly absent
# from idle_overlap, its idle permits are not "blocked by what was fished"
# in any meaningful sense, they just were not an active vessel-year at all.
cat("Held-but-unfished vessel-fishery-years with a computable seasonal-overlap classification:",
    nrow(idle_overlap), "of", nrow(held_not_fished), "\n")

# SEASONAL_OVERLAP_CUTOFF splits idle permits into "seasonally blocked" and
# "seasonally free." 0.5 is a judgment call, not a fact estimated from the
# data, CHECK it once run for real, the qualitative point (some idle
# permits could not have been fished, others could have been) does not
# depend on the exact cutoff, only the reported share does.
SEASONAL_OVERLAP_CUTOFF <- 0.5

idle_overlap <- idle_overlap %>%
  mutate(seasonally.blocked = overlap.with.fished >= SEASONAL_OVERLAP_CUTOFF)

cat("Share of held-but-unfished vessel-fishery-years that were seasonally blocked (overlap >=",
    SEASONAL_OVERLAP_CUTOFF, "):", round(mean(idle_overlap$seasonally.blocked), 3), "\n")

figure11 <- idle_overlap %>%
  ggplot(aes(x = overlap.with.fished)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  geom_vline(xintercept = SEASONAL_OVERLAP_CUTOFF, linetype = "dashed", color = "firebrick") +
  labs(
    # The Bhattacharyya-coefficient definition and its interpretation belong
    # in the caption, not this subtitle.
    title = "Held-but-unfished wedge, by seasonal overlap",
    subtitle = "Overlap between the idle permit and the vessel's own season",
    x = "Seasonal overlap between the idle permit and the vessel's fished fisheries that year",
    y = "Held-but-unfished vessel-fishery-years"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure11_wedge_by_seasonal_overlap.png"),
       figure11, width = 7, height = 5, dpi = 300)

cat("Wrote figure11_wedge_by_seasonal_overlap.png\n")

# ============================================================================
# 3. Table 12. Activation on the shock, interacted with seasonal overlap
# ============================================================================
#
# The prediction is signed. A held-but-non-primary permit whose season sits
# on TOP of the primary fishery's season cannot be activated in response to
# a bad primary year, however bad, the vessel is out fishing its primary
# fishery in exactly the weeks the alternative would also be open. So the
# negative shock coefficient from Table 10 should attenuate toward zero as
# overlap.with.primary rises, and be at its most negative for low-overlap
# (seasonally free) alternatives. That is a positive coefficient on the
# shock x overlap.with.primary interaction (it pushes the negative main
# effect back toward zero as overlap increases).

activation_data_overlap <- activation_data %>%
  left_join(overlap_long, by = c("Fishery" = "Fishery.A", "primary.fishery" = "Fishery.B")) %>%
  rename(overlap.with.primary = seasonal.overlap) %>%
  filter(is.finite(overlap.with.primary))

cat("Table 12 sample (activation candidates with a computable overlap to the predetermined primary):",
    nrow(activation_data_overlap), "of", nrow(activation_data), "\n")

model_table12 <- feols(activated ~ shock * overlap.with.primary | Vessel.ADFG.Number + fishery.year,
                        data = activation_data_overlap)

etable(
  model_table12,
  headers = c("Activated"),
  tex = TRUE,
  file = file.path(table_dir, "table12_activation_by_seasonal_overlap.tex"),
  replace = TRUE
)

print(etable(model_table12))

cat("Wrote table12_activation_by_seasonal_overlap.tex\n")
