# Chapter 3 empirical pipeline, Chapter3_outline.md Section 3, Table 3
#
# The wedge computed with and without permits missing a vessel identifier.
# permit_link.R drops these permits outright before any analysis. A permit
# held but attached to no boat is the cleanest example of holding without
# fishing in the whole dataset (chapter3_plan.md Section 6), so this
# comparison is treated as central, not a robustness footnote.
#
# Also computed against a "fishable fishery-years only" restriction, added
# 2026-09-17. The rows above already ask "was this permit fished", the
# fishable-only rows ask a prior question, "could it have been", counting a
# held permit toward portfolio breadth and the wedge only if fishery.year.
# active is TRUE, i.e. someone in the fleet actually landed something under
# that fishery code that year. Motivated by D91H (Cook Inlet Dungeness crab,
# a real "Limited" status fishery, not administrative, not junk, yet zero
# ticket-side rows across all 30 held years, matched by D10H and K09H in the
# same region). Counting a permit for an effectively defunct fishery the
# same as a genuinely fishable-but-idle one overstates ex-ante
# diversification, the fisher never really had that option to begin with.
# See 01_build_panel.R Section 4 (fishery_year_active_lookup) and Section 7
# for how the flag is built and joined onto owner_fishery_year/owner_year.
#
# Built at the OWNER (File.Number) level, not the vessel level. A permit with
# no vessel attached has, by definition, no vessel to be a "vessel-year" row
# for, so the only unit that can hold it at all is the owner. "With" below
# means every permit register row for that owner-year-fishery, including
# those with no vessel ID. "Without" restricts to rows matched to a real
# vessel, i.e., what the vessel-level panel already reports. See
# 01_build_panel.R Section 7 for how held.vessel.matched is constructed.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_year")) load(panel_path)

owner_year_valid <- owner_year %>% filter(n.held.fishery > 0)

table3 <- owner_year_valid %>%
  summarise(
    `Mean unused count share, with unmatched permits`             = mean(unused.count.share, na.rm = TRUE),
    `Mean unused count share, without unmatched permits`          = mean(unused.count.share.matched, na.rm = TRUE),
    # Fishery-class vs permit-serial granularity, held constant at "with
    # unmatched permits" (owner-inclusive) since that is the version
    # 01_build_panel.R builds a permit-serial count for. See
    # NOTES_prior_prototype.md for why this comparison matters, a vessel or
    # owner that stacks two serials of the same Fishery and fishes only one
    # looks fully used at the fishery-class level but has one idle permit at
    # the serial level.
    `Mean unused count share, permit-serial level`                = mean(unused.count.share.permit, na.rm = TRUE),
    # "Fishable fishery-years only", held further restricted to fishery.year.
    # active (Section 4/7 of 01_build_panel.R), na.rm = TRUE here correctly
    # drops any owner-year whose only held permits were all in dead
    # fisheries that year (n.held.fishery.active == 0 makes the underlying
    # share NA by construction), not just averages over a differently-sized
    # population silently.
    `Mean unused count share, fishable fishery-years only`         = mean(unused.count.share.active, na.rm = TRUE),
    `Mean unused value share, with unmatched permits`              = mean(unused.value.share, na.rm = TRUE),
    `Mean unused value share, without unmatched permits`           = mean(unused.value.share.matched, na.rm = TRUE),
    `Mean unused value share, fishable fishery-years only`          = mean(unused.value.share.active, na.rm = TRUE),
    `Mean permits held per owner-year, with unmatched (fishery)`   = mean(n.held.fishery, na.rm = TRUE),
    `Mean permits held per owner-year, without unmatched (fishery)` = mean(n.held.fishery.matched, na.rm = TRUE),
    `Mean permits held per owner-year, fishable fishery-years only` = mean(n.held.fishery.active, na.rm = TRUE),
    `Mean permits held per owner-year, permit-serial level`        = mean(n.held.permit, na.rm = TRUE),
    `Owner-years in sample`                                        = n()
  ) %>%
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value") %>%
  mutate(Value = round(Value, 4))

print(table3, n = Inf)

# The gap between the with and without count-share rows is the headline
# number, how much permit_link.R's current drop of vessel-unmatched permits
# understates the wedge Section 3 is trying to establish.
gap_count <- table3$Value[table3$Statistic == "Mean unused count share, with unmatched permits"] -
             table3$Value[table3$Statistic == "Mean unused count share, without unmatched permits"]
cat("Understatement from dropping vessel-unmatched permits, count share:", round(gap_count, 4), "\n")

# The permit-stacking gap, how much unused.count.share (fishery-class)
# understates unused.count.share.permit (serial-level) purely from
# collapsing stacked same-Fishery permits into one held/fished fact.
gap_stacking <- table3$Value[table3$Statistic == "Mean unused count share, permit-serial level"] -
                table3$Value[table3$Statistic == "Mean unused count share, with unmatched permits"]
cat("Understatement from fishery-class collapsing (permit stacking), count share:", round(gap_stacking, 4), "\n")

# How much counting permits for effectively defunct fisheries (zero
# fleet-wide landings from anyone that year) overstates ex-ante portfolio
# breadth and the wedge alike, the concern that motivated the "fishable
# fishery-years only" rows above.
gap_portfolio_defunct <- table3$Value[table3$Statistic == "Mean permits held per owner-year, with unmatched (fishery)"] -
                         table3$Value[table3$Statistic == "Mean permits held per owner-year, fishable fishery-years only"]
cat("Overstatement of held portfolio breadth from counting effectively defunct fisheries:",
    round(gap_portfolio_defunct, 4), "\n")

gap_count_defunct <- table3$Value[table3$Statistic == "Mean unused count share, with unmatched permits"] -
                      table3$Value[table3$Statistic == "Mean unused count share, fishable fishery-years only"]
cat("Overstatement of the unused count-share wedge from counting effectively defunct fisheries:",
    round(gap_count_defunct, 4), "\n")

# Display-only copy, Value becomes character here so "Owner-years in sample"
# renders as a comma-grouped integer rather than xtable's default numeric
# formatting, table3 itself (numeric) is left alone since gap_count/
# gap_stacking above already depend on it. round(Value) inside the first
# branch matters, format() picks a single decimal width for the WHOLE
# vector it is given, not per element, so format(Value, ...) on the raw
# mixed vector (shares needing 4 decimals sitting next to a whole-number
# count) padded the count out to "534,502.0000" the first time this ran,
# rounding to a whole number first removes any decimal need from every
# element of that call, so the selected count row comes out as "534,502".
table3_display <- table3 %>%
  mutate(Value = if_else(
    Statistic == "Owner-years in sample",
    format(round(Value), big.mark = ","),
    sprintf("%.4f", Value)
  ))

print(xtable(table3_display, caption = "Held-versus-fished wedge, with and without permits missing a vessel identifier",
             label = "tab:ch3-table3"),
      file = file.path(table_dir, "table3_wedge_missing_vessel_id.tex"),
      include.rownames = FALSE)

cat("Wrote table3_wedge_missing_vessel_id.tex to", table_dir, "\n")
