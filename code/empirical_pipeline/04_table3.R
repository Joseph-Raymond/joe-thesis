# Chapter 3 empirical pipeline, Chapter3_outline.md Section 3, Table 3
#
# The wedge computed with and without permits missing a vessel identifier.
# permit_link.R drops these permits outright before any analysis. A permit
# held but attached to no boat is the cleanest example of holding without
# fishing in the whole dataset (chapter3_plan.md Section 6), so this
# comparison is treated as central, not a robustness footnote.
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
    `Mean unused count share, with unmatched permits`      = mean(unused.count.share, na.rm = TRUE),
    `Mean unused count share, without unmatched permits`   = mean(unused.count.share.matched, na.rm = TRUE),
    `Mean unused value share, with unmatched permits`      = mean(unused.value.share, na.rm = TRUE),
    `Mean unused value share, without unmatched permits`   = mean(unused.value.share.matched, na.rm = TRUE),
    `Mean permits held per owner-year, with unmatched`     = mean(n.held.fishery, na.rm = TRUE),
    `Mean permits held per owner-year, without unmatched`  = mean(n.held.fishery.matched, na.rm = TRUE),
    `Owner-years in sample`                                = n()
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

print(xtable(table3, caption = "Held-versus-fished wedge, with and without permits missing a vessel identifier",
             label = "tab:ch3-table3"),
      file = file.path(table_dir, "table3_wedge_missing_vessel_id.tex"),
      include.rownames = FALSE)

cat("Wrote table3_wedge_missing_vessel_id.tex to", table_dir, "\n")
