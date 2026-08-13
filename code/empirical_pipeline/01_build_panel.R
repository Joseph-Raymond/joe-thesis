# Chapter 3 empirical pipeline, panel construction
#
# Builds the one reproducible panel that Chapter3_outline.md Sections 2-4
# read from (chapter3_plan.md Section 7, "Phase 1"). Everything is built at
# the Fishery-class level (e.g. "S03T"), matching how permit_link.R already
# computes HHI and the fished/unfished counts, not at the individual permit
# serial level (a vessel can hold more than one serial of the same Fishery
# class, "permit stacking", and that is deliberately collapsed here).
#
# Reuses the cleaning steps in code/Permit_Linking/permit_link.R lines 17-21
# and code/data load module.R's catch_data_temp block, but keeps a copy of
# the permit register BEFORE the valid-vessel-ID filter, because Section 3's
# Table 3 needs to compare the wedge with and without permits that have no
# vessel attached.
#
# Saves intermediate data/ch3_panel.rdata with these objects.
#   vessel_fishery_year  long panel, one row per vessel x year x fishery
#   vessel_year           one row per vessel x year
#   vessel_summary         one row per vessel, collapsed over its active years
#   owner_fishery_year, owner_year, owner_summary   same three at the
#                                                     File.Number (owner) level
#   match_diag             data-quality diagnostics for Table 2
#
# Run 00_setup.R first (or just source it, which this script does).

source("code/empirical_pipeline/00_setup.R")
deflator <- load_deflator()

# ============================================================================
# 1. Permit register (held set)
# ============================================================================

load(file.path(intermediate_dir, "permit_clean.rdata"))

# permit_clean.rdata loads an object literally called permit_clean.
# Same rename/derive steps as permit_link.R lines 17-20. CHECK the "...1"
# column still exists on the server's copy, it is a leftover row-index column
# from how permit_clean.rdata was written and may not always be present.
if ("...1" %in% names(permit_clean)) permit_clean <- permit_clean %>% select(-"...1")

lookup <- c(Vessel.ADFG.Number = "Vessel.ADFG", Batch.Year = "Year")
permit_register_raw <- permit_clean %>%
  rename(all_of(lookup)) %>%
  mutate(
    Vessel.ADFG.Number       = as.integer(Vessel.ADFG.Number),
    CFEC.Permit.Serial.Number = as.integer(substr(Permit.Number, 1, 5)),
    CFEC.Permit.Check         = substr(Permit.Number, 6, 6)
  ) %>%
  filter(Batch.Year >= MIN_YEAR, Fishery != "")

# has.vessel.id is FALSE for a permit register row with no vessel attached
# (NA, 0, or 99999) or an owner-only holding. permit_link.R drops these
# outright. Kept here because Table 3 (04_table3.R) needs both versions and
# because chapter3_plan.md Section 6 treats this as central, not a
# robustness footnote, a permit held with no boat is the cleanest example of
# holding without fishing in the whole dataset.
permit_register_raw <- permit_register_raw %>%
  mutate(has.vessel.id = !is.na(Vessel.ADFG.Number) & !(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))

cat("Permit register rows:", nrow(permit_register_raw),
    " missing/sentinel vessel ID:", sum(!permit_register_raw$has.vessel.id), "\n")

# Vessel-matched register, what permit_link.R uses. This is the basis for
# every vessel-level object below. The owner-inclusive version (all rows,
# keyed on File.Number) is built separately in Section 5 of this script.
permit_register <- permit_register_raw %>% filter(has.vessel.id)

held_vessel_fishery <- permit_register %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  mutate(held = TRUE)

# ============================================================================
# 2. Fish tickets (fished set and realized revenue)
# ============================================================================

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

# Same fixes as data load module.R / permit_link.R.
catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)

# Captured before the zero-fill below overwrites every NA, otherwise this
# diagnostic would trivially read 0 no matter how much revenue was missing.
share_revenue_zero_filled <- mean(is.na(catch_data_temp[["CFEC.Value..Detail."]]))

catch_data_temp[["CFEC.Value..Detail."]][is.na(catch_data_temp[["CFEC.Value..Detail."]])] <- 0

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
  filter(Fishery != "")

fished_vessel_fishery_year <- catch_data_temp %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  summarise(
    revenue      = sum(CFEC.Value..Detail., na.rm = TRUE),
    File.Number  = first(CFEC.Vessel.Owner.Filing.Number),
    .groups = "drop"
  )

# ============================================================================
# 3. Match-rate diagnostics (feeds Table 2, 02_table1_table2.R)
# ============================================================================

# Ticket-to-register match rate, permit serials on fish tickets that fail to
# match the CFEC permit register. permit_link.R flags this same quantity via
# CFEC.Permit.Serial.Number being NA and notes it skews out-of-state.
ticket_serial_match_rate <- catch_data_temp %>%
  summarise(match_rate = mean(!is.na(CFEC.Permit.Serial.Number))) %>%
  pull(match_rate)

match_diag <- tibble(
  metric = c(
    "ticket_serial_match_rate",
    "share_permits_missing_vessel_id",
    "share_revenue_zero_filled"
  ),
  value = c(
    ticket_serial_match_rate,
    mean(!permit_register_raw$has.vessel.id),
    share_revenue_zero_filled
  )
)
print(match_diag)

# ============================================================================
# 4. Vessel x fishery x year panel (held, fished, revenue)
# ============================================================================

vessel_fishery_year <- held_vessel_fishery %>%
  full_join(fished_vessel_fishery_year, by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")) %>%
  mutate(
    held   = replace_na(held, FALSE),
    fished = !is.na(revenue) & revenue > 0,
    revenue = replace_na(revenue, 0)
  )

# fished.unmatched: fished a fishery with no corresponding held permit that
# year. Should be rare or zero if the permit-ticket join is clean, a sizeable
# nonzero share here means the register-to-ticket match is lossy, not that
# fishers are landing without a permit. Worth checking before trusting the
# wedge numbers downstream.
n_fished_unmatched <- vessel_fishery_year %>% filter(fished & !held) %>% nrow()
cat("Vessel-fishery-years fished without a matched held permit:", n_fished_unmatched,
    "(", round(100 * n_fished_unmatched / sum(vessel_fishery_year$fished), 2), "% of fished rows)\n")

vessel_fishery_year <- deflate(vessel_fishery_year, "revenue", deflator)

# Fleet-mean revenue per active (fishing) vessel, by fishery-year. Used as
# the stand-in for what an idle vessel forgoes (Figure 1's value-share line)
# and again in 05_table4_figure3.R for the passive benchmark. No leave-one-out
# adjustment is needed for the forgone-value use, a vessel that did not fish
# fishery j contributes nothing to fishery j's mean that year regardless.
fleet_mean_revenue <- vessel_fishery_year %>%
  filter(fished) %>%
  group_by(Batch.Year, Fishery) %>%
  summarise(fleet_mean_revenue = mean(revenue, na.rm = TRUE), n_active = n(), .groups = "drop")

vessel_fishery_year <- vessel_fishery_year %>%
  left_join(fleet_mean_revenue, by = c("Batch.Year", "Fishery"))

# ============================================================================
# 5. Vessel-year summary (Figure 1, Figure 2, Table 3 inputs)
# ============================================================================

vessel_year <- vessel_fishery_year %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  summarise(
    n.held.fishery     = sum(held),
    n.fished.fishery   = sum(held & fished),
    n.unfished.fishery = sum(held & !fished),
    fished.value       = sum(revenue[held & fished], na.rm = TRUE),
    # forgone.value falls back to 0 (not NA) for a held-not-fished fishery
    # that also had zero fleet-wide participation that year, an empty
    # fishery contributes no forgone option value by construction.
    forgone.value      = sum(replace_na(fleet_mean_revenue[held & !fished], 0)),
    vessel.year.rev    = sum(revenue, na.rm = TRUE),
    hhi                = sum((revenue[fished] / sum(revenue[fished]))^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    unused.count.share = if_else(n.held.fishery > 0, n.unfished.fishery / n.held.fishery, NA_real_),
    unused.value.share = if_else((forgone.value + fished.value) > 0,
                                  forgone.value / (forgone.value + fished.value), NA_real_)
  )

cat("vessel_year rows:", nrow(vessel_year), "\n")

# ============================================================================
# 6. Vessel summary (Table 4, Figure 3 inputs), Hbar/H_LR/Phi/CV
# ============================================================================
#
# H_bar_i = mean_t sum_j s_ijt^2
# H_LR_i  = sum_j (mean_t s_ijt)^2
# Phi_i   = H_bar_i - H_LR_i = sum_j Var_t(s_ijt)
# matching chapter3_plan.md Section R2 / Chapter3_outline.md Section 4 exactly.
# The mean and variance are taken over the vessel's own active years only
# (vessel.year.rev > 0), and shares are zero-filled for years the vessel was
# active but did not fish a fishery it fished in some other year of its own
# panel, so a fishery's long-run share reflects true within-vessel
# instability rather than just entry/exit from the panel.

active_vessel_years <- vessel_year %>% filter(vessel.year.rev > 0) %>%
  select(Vessel.ADFG.Number, Batch.Year, vessel.year.rev)

# group_by(Vessel.ADFG.Number) before complete() matters, it fills each
# vessel's own ever-fished fishery set across its own years, not the union of
# every fishery any vessel in the fleet ever fished.
vessel_share_panel <- vessel_fishery_year %>%
  filter(fished) %>%
  inner_join(active_vessel_years, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  mutate(share = revenue / vessel.year.rev) %>%
  select(Vessel.ADFG.Number, Batch.Year, Fishery, share) %>%
  group_by(Vessel.ADFG.Number) %>%
  complete(Fishery, Batch.Year, fill = list(share = 0)) %>%
  ungroup() %>%
  # complete() above also fills Batch.Year x Fishery combos for years the
  # vessel was not active at all (not in active_vessel_years). Drop those,
  # an inactive year is an entry/exit gap, not a zero-share portfolio choice.
  semi_join(active_vessel_years, by = c("Vessel.ADFG.Number", "Batch.Year"))

# Per vessel-fishery long-run mean share, s_bar_ij. This is the weight vector
# that defines H_LR_i = sum_j s_bar_ij^2, and it is saved on its own (not
# just folded into vessel_summary) because 05_table4_figure3.R reuses it as
# the passive buy-and-hold portfolio weights for Figure 3.
vessel_mean_share <- vessel_share_panel %>%
  group_by(Vessel.ADFG.Number, Fishery) %>%
  summarise(mean.share.fishery = mean(share), .groups = "drop")

vessel_summary <- vessel_share_panel %>%
  left_join(vessel_mean_share, by = c("Vessel.ADFG.Number", "Fishery")) %>%
  group_by(Vessel.ADFG.Number) %>%
  summarise(
    n.years  = n_distinct(Batch.Year),
    H_bar    = mean(tapply(share, Batch.Year, function(s) sum(s^2))),
    H_LR     = sum(unique(mean.share.fishery)^2),
    .groups = "drop"
  ) %>%
  mutate(Phi = H_bar - H_LR) %>%
  left_join(
    active_vessel_years %>%
      group_by(Vessel.ADFG.Number) %>%
      summarise(rev.cv = sd(vessel.year.rev) / mean(vessel.year.rev), .groups = "drop"),
    by = "Vessel.ADFG.Number"
  )

# prime.fishery = the fishery with the most total realized revenue across the
# vessel's whole panel, matching permit_link.R's prime.fishery construction
# (used there per year, here once per vessel since vessel_summary is a
# cross-section). Needed as the fixed effect in Table 4. Ranked on summed
# revenue directly (not shares, which are already normalized per year and so
# cannot be summed meaningfully across years for a magnitude ranking).
prime_fishery <- vessel_fishery_year %>%
  filter(fished) %>%
  group_by(Vessel.ADFG.Number, Fishery) %>%
  summarise(total.rev = sum(revenue), .groups = "drop") %>%
  group_by(Vessel.ADFG.Number) %>%
  slice_max(total.rev, n = 1, with_ties = FALSE) %>%
  select(Vessel.ADFG.Number, prime.fishery = Fishery)

vessel_summary <- vessel_summary %>%
  left_join(prime_fishery, by = "Vessel.ADFG.Number") %>%
  mutate(meets.min.years = n.years >= MIN_ACTIVE_YEARS)

cat("vessel_summary rows:", nrow(vessel_summary),
    " meeting MIN_ACTIVE_YEARS =", MIN_ACTIVE_YEARS, ":", sum(vessel_summary$meets.min.years), "\n")

# ============================================================================
# 7. Owner-level panel (File.Number), same three objects
# ============================================================================
#
# Built from permit_register_raw (not the vessel-ID-filtered permit_register)
# because a permit with no vessel attached still belongs to an owner. This is
# what makes the owner-level wedge in 04_table3.R different from, and larger
# than, the vessel-level one.

# held.vessel.matched marks whether at least one of the underlying permit
# rows for this owner-fishery-year had a real vessel attached. Carried
# through to owner_year below so 04_table3.R can compare the wedge with and
# without permits that have no vessel identifier, per Chapter3_outline.md
# Section 3 Table 3, without re-loading and re-cleaning the register.
held_owner_fishery <- permit_register_raw %>%
  filter(!is.na(File.Number)) %>%
  group_by(File.Number, Batch.Year, Fishery) %>%
  summarise(held = TRUE, held.vessel.matched = any(has.vessel.id), .groups = "drop")

fished_owner_fishery_year <- fished_vessel_fishery_year %>%
  filter(!is.na(File.Number)) %>%
  group_by(File.Number, Batch.Year, Fishery) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")

owner_fishery_year <- held_owner_fishery %>%
  full_join(fished_owner_fishery_year, by = c("File.Number", "Batch.Year", "Fishery")) %>%
  mutate(
    held   = replace_na(held, FALSE),
    held.vessel.matched = replace_na(held.vessel.matched, FALSE),
    fished = !is.na(revenue) & revenue > 0,
    revenue = replace_na(revenue, 0)
  ) %>%
  deflate("revenue", deflator)

fleet_mean_revenue_owner <- owner_fishery_year %>%
  filter(fished) %>%
  group_by(Batch.Year, Fishery) %>%
  summarise(fleet_mean_revenue = mean(revenue, na.rm = TRUE), .groups = "drop")

owner_fishery_year <- owner_fishery_year %>%
  left_join(fleet_mean_revenue_owner, by = c("Batch.Year", "Fishery"))

owner_year <- owner_fishery_year %>%
  group_by(File.Number, Batch.Year) %>%
  summarise(
    # "with" columns, includes permits with no vessel attached
    n.held.fishery     = sum(held),
    n.fished.fishery   = sum(held & fished),
    n.unfished.fishery = sum(held & !fished),
    fished.value       = sum(revenue[held & fished], na.rm = TRUE),
    forgone.value      = sum(replace_na(fleet_mean_revenue[held & !fished], 0)),
    # "without" columns, restricted to permits matched to a real vessel,
    # what permit_link.R and the vessel-level panel both already do
    n.held.fishery.matched     = sum(held & held.vessel.matched),
    n.unfished.fishery.matched = sum(held & held.vessel.matched & !fished),
    forgone.value.matched      = sum(replace_na(fleet_mean_revenue[held & held.vessel.matched & !fished], 0)),
    owner.year.rev     = sum(revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    unused.count.share = if_else(n.held.fishery > 0, n.unfished.fishery / n.held.fishery, NA_real_),
    unused.value.share = if_else((forgone.value + fished.value) > 0,
                                  forgone.value / (forgone.value + fished.value), NA_real_),
    unused.count.share.matched = if_else(n.held.fishery.matched > 0,
                                          n.unfished.fishery.matched / n.held.fishery.matched, NA_real_),
    unused.value.share.matched = if_else((forgone.value.matched + fished.value) > 0,
                                          forgone.value.matched / (forgone.value.matched + fished.value), NA_real_)
  )

active_owner_years <- owner_year %>% filter(owner.year.rev > 0) %>%
  select(File.Number, Batch.Year, owner.year.rev)

owner_share_panel <- owner_fishery_year %>%
  filter(fished) %>%
  inner_join(active_owner_years, by = c("File.Number", "Batch.Year")) %>%
  mutate(share = revenue / owner.year.rev) %>%
  select(File.Number, Batch.Year, Fishery, share) %>%
  group_by(File.Number) %>%
  complete(Fishery, Batch.Year, fill = list(share = 0)) %>%
  ungroup() %>%
  semi_join(active_owner_years, by = c("File.Number", "Batch.Year"))

owner_summary <- owner_share_panel %>%
  group_by(File.Number, Fishery) %>%
  mutate(mean.share.fishery = mean(share)) %>%
  group_by(File.Number) %>%
  summarise(
    n.years = n_distinct(Batch.Year),
    H_bar   = mean(tapply(share, Batch.Year, function(s) sum(s^2))),
    H_LR    = sum(unique(mean.share.fishery)^2),
    .groups = "drop"
  ) %>%
  mutate(Phi = H_bar - H_LR) %>%
  left_join(
    active_owner_years %>%
      group_by(File.Number) %>%
      summarise(rev.cv = sd(owner.year.rev) / mean(owner.year.rev), .groups = "drop"),
    by = "File.Number"
  ) %>%
  mutate(meets.min.years = n.years >= MIN_ACTIVE_YEARS)

cat("owner_summary rows:", nrow(owner_summary),
    " meeting MIN_ACTIVE_YEARS:", sum(owner_summary$meets.min.years), "\n")

# ============================================================================
# 8. Save
# ============================================================================

save(
  vessel_fishery_year, vessel_year, vessel_summary, vessel_mean_share,
  owner_fishery_year, owner_year, owner_summary,
  match_diag, fleet_mean_revenue,
  file = panel_path
)
cat("Saved panel to", panel_path, "\n")
