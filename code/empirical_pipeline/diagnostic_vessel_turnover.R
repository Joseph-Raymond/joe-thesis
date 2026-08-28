# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone.
#
# Checks a specific threat to baseline Table 10/11/12's own validity, not
# to the rolling design. That design fixes a vessel's primary fishery ONCE,
# from its first half of active years (as far back as 1991), then tests
# whether a SECOND-half shock to that fixed primary predicts activation of
# a held, non-primary permit, potentially decades later. If a vessel
# (Vessel.ADFG.Number) is sold, or the same hull is repurposed to an
# entirely different fishery partway through the panel, the "primary
# fishery" identified from the first half may describe a different
# economic operator than the one making the activation decisions the
# second half is testing. That is a real threat baseline is more exposed
# to than the rolling design, since rolling re-derives predetermined.primary
# fresh inside every 6-year window rather than committing once for the
# vessel's entire multi-decade panel life.
#
# No owner-ID field is currently loaded anywhere in this pipeline
# (CFEC.Vessel.Owner.Filing.Number is referenced only in a comment in
# 01_build_panel.R, never read). The CFEC vessel register
# (03_figure1_figure2.R's own data source) is a genuine per-vessel-per-year
# panel of gear and length, and vessel_clean.R already detects that both
# can change year to year for the same Vessel.ADFG.Number, per that
# script's own comment. A gear-class change is a common, softer signal (an
# owner can legitimately re-rig a boat without selling it). A LENGTH
# change is a much harder signal, since a hull's physical length cannot
# change without a full rebuild, so a length change is closer to direct
# evidence the ADFG number is not tracking one continuous physical vessel
# across its recorded life. Both are checked, split at the same
# first-half/second-half boundary 08_state_contingent_activation.R uses to
# fix the primary fishery, and the exact Table 10 model
# (activated ~ shock | Vessel.ADFG.Number + fishery.year) is refit with
# flagged vessels excluded, to see whether the headline coefficient moves.
#
# Reads intermediate data/ch3_panel.rdata (vessel_share_panel, for the
# first-half/second-half year split) and intermediate
# data/ch3_activation.rdata (activation_data, Table 10's own regression
# sample, built by 08_state_contingent_activation.R). Reads the CFEC
# vessel register directly, same path 03_figure1_figure2.R already uses.
# Writes nothing, prints only.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_share_panel")) load(panel_path)

activation_path <- file.path(intermediate_dir, "ch3_activation.rdata")
if (!exists("activation_data")) load(activation_path)

cat("Table 10's own regression sample, rows:", nrow(activation_data),
    " distinct vessels:", n_distinct(activation_data$Vessel.ADFG.Number), "\n")

# ============================================================================
# 1. First-half / second-half year split, IDENTICAL construction to
#    08_state_contingent_activation.R Section 1, copied rather than
#    depending on that script's in-memory state
# ============================================================================

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

# ============================================================================
# 2. CFEC vessel register, gear class and length, same path and
#    classify_gear() logic as 03_figure1_figure2.R, copied rather than
#    depending on that script's in-memory state
# ============================================================================

vessel_register_path <- "~/JoeData/clean_data/vessels_clean_1978_2022.csv"

GEAR_COLUMNS <- c(
  "Purse.Seine", "Beach.Seine", "Drift.Gill.Net", "Set.Gill.Net",
  "Hand.Troll", "Power.Troll", "Long.Line", "Otter.Trawl", "Pots",
  "Fish.Wheel", "Beam.Trawl", "Scallop.Dredge", "Mechanical.Jig",
  "Double.Otter.Trawl", "Herring.Gill.Net", "Pair.Trawl", "Diving.Hand.Picking"
)

classify_gear <- function(df) {
  gear_matrix <- df %>% select(any_of(GEAR_COLUMNS)) %>% mutate(across(everything(), ~ .x == "Yes"))
  present <- intersect(GEAR_COLUMNS, names(gear_matrix))
  gear_class <- rep(NA_character_, nrow(df))
  for (g in present) {
    gear_class[is.na(gear_class) & gear_matrix[[g]]] <- gsub("\\.", " ", g)
  }
  gear_class[is.na(gear_class)] <- "Unclassified"
  gear_class
}

if (!file.exists(vessel_register_path)) {
  stop(
    "Vessel register not found at ", vessel_register_path, ". This diagnostic needs it for ",
    "gear class and length, the same file 03_figure1_figure2.R already depends on. Check the ",
    "path once running on the server (chapter3_plan.md Section 0.2)."
  )
}

vessel_register <- read_csv(vessel_register_path, show_col_types = FALSE) %>%
  rename(Vessel.ADFG.Number = ADFG.Number, Batch.Year = Year) %>%
  mutate(Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number))

vessel_register <- vessel_register %>% mutate(gear_class = classify_gear(vessel_register))

# ============================================================================
# 3. Per-vessel modal gear and median length, within each half, restricted
#    to the vessels actually in Table 10's regression sample
# ============================================================================

sample_vessels <- activation_data %>% distinct(Vessel.ADFG.Number)

register_by_half <- vessel_register %>%
  semi_join(sample_vessels, by = "Vessel.ADFG.Number") %>%
  inner_join(vessel_year_ordinal %>% select(Vessel.ADFG.Number, Batch.Year, half),
             by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  filter(!is.na(gear_class))

modal_gear_by_half <- register_by_half %>%
  count(Vessel.ADFG.Number, half, gear_class) %>%
  group_by(Vessel.ADFG.Number, half) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Vessel.ADFG.Number, half, modal.gear = gear_class)

median_length_by_half <- register_by_half %>%
  filter(!is.na(Length)) %>%
  group_by(Vessel.ADFG.Number, half) %>%
  summarise(median.length = median(Length), .groups = "drop")

vessel_stability <- sample_vessels %>%
  left_join(modal_gear_by_half %>% filter(half == "first")  %>% select(Vessel.ADFG.Number, gear.first  = modal.gear), by = "Vessel.ADFG.Number") %>%
  left_join(modal_gear_by_half %>% filter(half == "second") %>% select(Vessel.ADFG.Number, gear.second = modal.gear), by = "Vessel.ADFG.Number") %>%
  left_join(median_length_by_half %>% filter(half == "first")  %>% select(Vessel.ADFG.Number, length.first  = median.length), by = "Vessel.ADFG.Number") %>%
  left_join(median_length_by_half %>% filter(half == "second") %>% select(Vessel.ADFG.Number, length.second = median.length), by = "Vessel.ADFG.Number") %>%
  mutate(
    has.both.gear.halves   = !is.na(gear.first) & !is.na(gear.second),
    has.both.length.halves = !is.na(length.first) & !is.na(length.second),
    gear.changed           = has.both.gear.halves & (gear.first != gear.second),
    # 15 percent, a judgment call, wide enough that ordinary register
    # measurement noise (a foot or two of rounding) should not trip it, tight
    # enough that a genuinely different hull should.
    length.pct.change      = if_else(has.both.length.halves, abs(length.second - length.first) / length.first, NA_real_),
    length.changed          = has.both.length.halves & (length.pct.change > 0.15)
  )

cat("\nVessels in Table 10's sample with register coverage in BOTH halves\n")
cat("  Gear class comparison possible:", sum(vessel_stability$has.both.gear.halves), "of", nrow(vessel_stability), "\n")
cat("  Length comparison possible:", sum(vessel_stability$has.both.length.halves), "of", nrow(vessel_stability), "\n")

cat("\nGear class changed between first and second half (softer signal, re-rigging without a sale",
    "is possible):", sum(vessel_stability$gear.changed, na.rm = TRUE), "of", sum(vessel_stability$has.both.gear.halves),
    " with a comparison (", round(100 * mean(vessel_stability$gear.changed[vessel_stability$has.both.gear.halves]), 1), "percent )\n")

cat("Length changed >15 percent between first and second half (harder signal, a hull cannot",
    "resize itself):", sum(vessel_stability$length.changed, na.rm = TRUE), "of", sum(vessel_stability$has.both.length.halves),
    " with a comparison (", round(100 * mean(vessel_stability$length.changed[vessel_stability$has.both.length.halves]), 1), "percent )\n")

flagged_either <- vessel_stability %>%
  filter((has.both.gear.halves & gear.changed) | (has.both.length.halves & length.changed)) %>%
  pull(Vessel.ADFG.Number)

cat("\nVessels flagged by EITHER signal:", length(flagged_either), "of", nrow(vessel_stability),
    " (", round(100 * length(flagged_either) / nrow(vessel_stability), 1), "percent )\n")

# ============================================================================
# 4. Refit Table 10's exact spec, full sample vs each exclusion
# ============================================================================

m_full <- feols(activated ~ shock | Vessel.ADFG.Number + fishery.year,
                 data = activation_data, cluster = ~Vessel.ADFG.Number)

data_no_gear_change <- activation_data %>%
  filter(!(Vessel.ADFG.Number %in% (vessel_stability %>% filter(gear.changed) %>% pull(Vessel.ADFG.Number))))
m_no_gear_change <- feols(activated ~ shock | Vessel.ADFG.Number + fishery.year,
                           data = data_no_gear_change, cluster = ~Vessel.ADFG.Number)

data_no_length_change <- activation_data %>%
  filter(!(Vessel.ADFG.Number %in% (vessel_stability %>% filter(length.changed) %>% pull(Vessel.ADFG.Number))))
m_no_length_change <- feols(activated ~ shock | Vessel.ADFG.Number + fishery.year,
                             data = data_no_length_change, cluster = ~Vessel.ADFG.Number)

data_no_either <- activation_data %>% filter(!(Vessel.ADFG.Number %in% flagged_either))
m_no_either <- feols(activated ~ shock | Vessel.ADFG.Number + fishery.year,
                      data = data_no_either, cluster = ~Vessel.ADFG.Number)

cat("\n")
print(etable(
  m_full, m_no_gear_change, m_no_length_change, m_no_either,
  headers = c("Full sample (reproduces Table 10)", "Excl. gear-changed", "Excl. length-changed", "Excl. either")
))

cat("\nReading rule, stated explicitly. If the shock coefficient is materially LARGER (more negative)",
    "once flagged vessels are excluded, that supports the concern, vessel turnover is diluting the",
    "activation coefficient toward zero and the 'stable-vessel' estimate is the more trustworthy one.",
    "If it barely moves, turnover contamination is probably not a major factor for this coefficient,",
    "whatever weakens Table 10-rolling relative to baseline is more likely explained elsewhere.\n")

cat("diagnostic_vessel_turnover.R done\n")
