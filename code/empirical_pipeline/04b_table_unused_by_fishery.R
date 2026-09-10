# Chapter 3 empirical pipeline, companion to Table 3 (Chapter3_outline.md Section 3)
#
# Table 3, appendix breakdown. Which fisheries carry the held-but-unfished
# wedge, and not just the single fleet-wide 0.68 mean unused share Table 3
# reports. Built at the owner level, on held_owner_fishery's own population
# (every permit-register row with a File.Number, vessel-matched or not), so
# this decomposes the same population 04_table3.R already summarizes, one
# row per fishery instead of one number for the whole fleet.
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_fishery_year")) load(panel_path)

# A fishery held only a handful of times ever would show a noisy 0 percent or
# 100 percent unused share off a tiny denominator. Ranking is restricted to
# fisheries with at least this many owner-year holdings across the whole
# 1991-2021 panel so the ranking reflects a real pattern, not small-sample
# noise. This is a floor on the ranking only, not a change to Table 3's own
# fleet-wide number, which pools every fishery regardless of size.
MIN_HELD_FOR_RANKING <- 30

year_range <- range(owner_fishery_year$Batch.Year)

table3_by_fishery <- owner_fishery_year %>%
  filter(held) %>%
  group_by(Fishery) %>%
  summarise(
    n.held       = n(),
    n.unfished   = sum(!fished),
    unused.share = n.unfished / n.held,
    .groups = "drop"
  ) %>%
  filter(n.held >= MIN_HELD_FOR_RANKING) %>%
  arrange(desc(unused.share)) %>%
  slice_head(n = 15) %>%
  transmute(
    Fishery = Fishery,
    `Held (owner-years)` = format(n.held, big.mark = ","),
    `Unused share` = sprintf("%.2f", unused.share)
  )

print(table3_by_fishery, n = Inf)

print(xtable(table3_by_fishery,
             caption = paste0("Fisheries with the highest owner-level unused share (held but never fished), ",
                               "pooled across ", year_range[1], "-", year_range[2],
                               ", restricted to fisheries held at least ", MIN_HELD_FOR_RANKING,
                               " owner-years"),
             label = "tab:ch3-table3-by-fishery"),
      file = file.path(table_dir, "table3_unused_share_by_fishery.tex"),
      include.rownames = FALSE)

cat("Wrote table3_unused_share_by_fishery.tex to", table_dir, "\n")
