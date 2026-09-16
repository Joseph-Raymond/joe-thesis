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

fishery_unused_full <- owner_fishery_year %>%
  filter(held) %>%
  group_by(Fishery) %>%
  summarise(
    n.held             = n(),
    n.unfished         = sum(!fished),
    n.fished           = sum(fished),
    n.held.no.vessel   = sum(!held.vessel.matched),
    n.distinct.owners  = n_distinct(File.Number),
    n.distinct.years   = n_distinct(Batch.Year),
    unused.share       = n.unfished / n.held,
    .groups = "drop"
  ) %>%
  filter(n.held >= MIN_HELD_FOR_RANKING)

# Same population as above (fisheries kept in the register as of this run,
# so gear 04/08/18 and the five non-harvest codes never appear here at all),
# but ranked by raw COUNT rather than share. A fishery can carry a modest
# unused SHARE and still be the largest single contributor to the gap in
# absolute owner-years if it is simply a big fishery, share and count answer
# different questions and neither implies the other. Two separate counts
# printed, held-but-never-fished (the Table 3 concept, "unmatched" in the
# sense the fished side never shows this owner) and held-with-no-vessel-ID
# (the held.vessel.matched concept, "unmatched" in the sense the register
# side never attached a real vessel to this owner's holding). A fishery can
# rank high on one and not the other, they are counting different things.
cat("\n===== Held fisheries ranked by raw count of held-but-never-fished owner-years (held >=",
    MIN_HELD_FOR_RANKING, ") =====\n")
print(fishery_unused_full %>% arrange(desc(n.unfished)) %>%
        select(Fishery, n.held, n.unfished, unused.share, n.distinct.owners) %>%
        mutate(unused.share = round(unused.share, 3)),
      n = 20)

cat("\n===== Held fisheries ranked by raw count of held-with-no-vessel-ID owner-years (held >=",
    MIN_HELD_FOR_RANKING, ") =====\n")
print(fishery_unused_full %>% mutate(no.vessel.share = round(n.held.no.vessel / n.held, 3)) %>%
        arrange(desc(n.held.no.vessel)) %>%
        select(Fishery, n.held, n.held.no.vessel, no.vessel.share, n.distinct.owners),
      n = 20)

# Diagnostic, unrounded and untruncated. The rendered table below prints
# sprintf("%.2f", ...) and keeps only the top 15, so a fishery at say 0.997
# reads identically to one at exactly 1.000, and an unknown number of exact
# ties beyond rank 15 never get printed at all (arrange() %>% slice_head()
# truncates silently). That distinction mattered here, S04T and S04X turned
# out to sit just under 1.000 (order-of-magnitude fewer than 1 in 1000
# owner-years actually fished) rather than at exactly 1.000 like the
# alphabetical block of codes ahead of them in the sorted table, a
# difference invisible at 2 decimals but load-bearing for which explanation
# applies to which fishery. Print every fishery above 0.95 with 4 decimals
# so this stops hiding in future runs.
cat("\n===== Every fishery with unused share > 0.95, unrounded, untruncated (held >=", MIN_HELD_FOR_RANKING, ") =====\n")
print(fishery_unused_full %>% filter(unused.share > 0.95) %>% arrange(desc(unused.share)) %>%
        mutate(unused.share = round(unused.share, 4)), n = Inf)

table3_by_fishery <- fishery_unused_full %>%
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
