# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone.
#
# Investigates the "temporary reversal around 2002" in Figure 1's fleet-wide
# mean unused share (chapter3_writeup.tex sec:ch3-wedge, "with a temporary
# reversal around 2002 that we note but do not attribute to any particular
# cause"). Two candidate explanations came up in chat, neither yet checked
# against the data.
#
# (1) The early-2000s Alaska salmon ex-vessel price collapse (competition
#     from farmed Atlantic salmon), which would push down PARTICIPATION
#     broadly, many salmon fisheries, many regions, not one area.
# (2) The Chignik Salmon Cooperative, which ran 2002 through 2004 before
#     being struck down as unconstitutional. Co-op members pooled their
#     harvest into a subset of the fleet while still sharing proceeds, a
#     near-exact mechanical match to "held but not fished." This is narrow,
#     Chignik salmon purse seine only, CFEC code S01L (Context_papers/CFEC
#     codes/Current CFEC Fishery Codes.txt), already flagged as a policy
#     quasi-experiment candidate in chapter3_plan.md's R5(d).
#
# If the 2002 bump is broad, salmon-heavy, and spread across many area
# codes, that favors (1). If it is concentrated almost entirely in S01L,
# that favors (2). The two are not mutually exclusive.
#
# Built at the vessel x fishery x year grain (vessel_fishery_year), the same
# object that feeds vessel_year (01_build_panel.R Section 6), which Figure 1
# itself is built from (03_figure1_figure2.R). Deliberately NOT
# owner_fishery_year (04b_/04c_'s grain), since the question here is about
# Figure 1 specifically, and owner- vs vessel-grain unused share are already
# documented elsewhere in this pipeline as not identical.
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_fishery_year")) load(panel_path)

SPIKE_YEAR <- 2002
BASELINE_YEARS <- c(2000, 2001, 2003, 2004)
MIN_HELD_FOR_RANKING <- 30
CHIGNIK_SALMON_CODE <- "S01L"

held_vfy <- vessel_fishery_year %>% filter(held)

# Fishery-year level unused share, matching Figure 1's own population
# (vessel_year's n.held.fishery > 0 filter is a vessel-year, not a
# fishery-year, restriction, so nothing beyond held is filtered here).
fishery_year_unused <- held_vfy %>%
  group_by(Fishery, Batch.Year) %>%
  summarise(n.held = n(), unused.share = mean(!fished), .groups = "drop")

cat("\n===== Fleet-wide mean unused share (count, vessel-fishery-year grain), 1998-2007, for context =====\n")
print(vessel_fishery_year %>% filter(held, Batch.Year %in% 1998:2007) %>%
        group_by(Batch.Year) %>% summarise(unused.share = round(mean(!fished), 4), n.held = n(), .groups = "drop"),
      n = Inf)

# Restricting to fisheries actually held in 2002 avoids a zero-length
# Batch.Year == SPIKE_YEAR index inside summarise() below for any fishery
# that only appears in the baseline years.
fisheries_held_2002 <- fishery_year_unused %>% filter(Batch.Year == SPIKE_YEAR) %>% pull(Fishery)

# Per-fishery 2002 value vs. its own baseline (years right around 2002,
# excluding 2002 itself), so a fishery that sits near 1.00 in EVERY year
# (an administrative-ceiling fishery, 04b_table_unused_by_fishery.R's own
# caveat) does not show up as a "mover" just for being persistently high.
fishery_2002_gap <- fishery_year_unused %>%
  filter(Batch.Year %in% c(SPIKE_YEAR, BASELINE_YEARS), Fishery %in% fisheries_held_2002) %>%
  group_by(Fishery) %>%
  summarise(
    n.held.2002      = n.held[Batch.Year == SPIKE_YEAR],
    unused.2002      = unused.share[Batch.Year == SPIKE_YEAR],
    n.baseline.years = sum(Batch.Year != SPIKE_YEAR),
    unused.baseline  = mean(unused.share[Batch.Year != SPIKE_YEAR]),
    gap              = unused.2002 - unused.baseline,
    .groups = "drop"
  ) %>%
  filter(n.held.2002 >= MIN_HELD_FOR_RANKING, n.baseline.years >= 1) %>%
  arrange(desc(gap))

cat("\n===== Fisheries with the largest jump in unused share INTO", SPIKE_YEAR,
    "vs. their own", paste(BASELINE_YEARS, collapse = ","), "average (held >=",
    MIN_HELD_FOR_RANKING, "vessel-years in 2002) =====\n")
print(fishery_2002_gap %>% mutate(across(c(unused.2002, unused.baseline, gap), ~round(.x, 4))), n = 20)

# Species-letter breakdown (first character of Fishery, S = salmon, G =
# herring roe, H = herring food/bait, D = Dungeness crab, T = Tanner crab,
# P = shrimp, Q = sea cucumber, R = clams, per the CFEC dictionary), weighted
# by n.held so a handful of big fisheries are not drowned out by many small
# ones. Answers whether the 2002 bump is a salmon story specifically or
# spread across species.
cat("\n===== Mean unused share by species letter, ", SPIKE_YEAR, "vs.",
    paste(BASELINE_YEARS, collapse = ","), "average =====\n")
print(
  fishery_year_unused %>%
    filter(Batch.Year %in% c(SPIKE_YEAR, BASELINE_YEARS)) %>%
    mutate(
      species = substr(Fishery, 1, 1),
      period  = if_else(Batch.Year == SPIKE_YEAR, "yr2002", "baseline")
    ) %>%
    group_by(species, period) %>%
    summarise(mean.unused = weighted.mean(unused.share, n.held), n.held = sum(n.held), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = c(mean.unused, n.held)) %>%
    mutate(gap = mean.unused_yr2002 - mean.unused_baseline) %>%
    arrange(desc(gap)),
  n = Inf
)

# Salmon-only, broken out by AREA letter (last character of the Fishery
# code, e.g. S01L = Chignik, S01H = Cook Inlet, S03T = Bristol Bay). Answers
# the co-op-vs-price-collapse question directly, concentrated in "L" alone
# points to Chignik, spread across many area letters points to a
# statewide salmon effect.
cat("\n===== Salmon only (species letter S), mean unused share by AREA letter, ",
    SPIKE_YEAR, "vs.", paste(BASELINE_YEARS, collapse = ","), "average =====\n")
print(
  fishery_year_unused %>%
    filter(Batch.Year %in% c(SPIKE_YEAR, BASELINE_YEARS), substr(Fishery, 1, 1) == "S") %>%
    mutate(
      area   = substr(Fishery, nchar(Fishery), nchar(Fishery)),
      period = if_else(Batch.Year == SPIKE_YEAR, "yr2002", "baseline")
    ) %>%
    group_by(area, period) %>%
    summarise(mean.unused = weighted.mean(unused.share, n.held), n.held = sum(n.held), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = c(mean.unused, n.held)) %>%
    mutate(gap = mean.unused_yr2002 - mean.unused_baseline) %>%
    arrange(desc(gap)),
  n = Inf
)

# Direct check on the Chignik co-op hypothesis, S01L year by year across its
# known 2002-2004 run plus a few years on either side for contrast.
cat("\n===== S01L (Chignik salmon purse seine) unused share by year, 1998-2007 =====\n")
print(fishery_year_unused %>% filter(Fishery == CHIGNIK_SALMON_CODE, Batch.Year %in% 1998:2007) %>%
        arrange(Batch.Year) %>% mutate(unused.share = round(unused.share, 4)),
      n = Inf)

# How much of the fleet-wide 2002 population S01L alone accounts for, in
# held vessel-year terms, answers "is this one fishery even large enough to
# move a fleet-wide mean" directly rather than leaving it to inference from
# the tables above.
n_s01l_2002 <- fishery_year_unused %>% filter(Fishery == CHIGNIK_SALMON_CODE, Batch.Year == SPIKE_YEAR) %>% pull(n.held)
n_total_2002 <- fishery_year_unused %>% filter(Batch.Year == SPIKE_YEAR) %>% summarise(n = sum(n.held)) %>% pull(n)
if (length(n_s01l_2002) == 0) n_s01l_2002 <- 0

cat("\nS01L 2002 held vessel-year count:", n_s01l_2002, "of", n_total_2002,
    "total held vessel-fishery-year rows in 2002 (",
    round(100 * n_s01l_2002 / n_total_2002, 3), "% )\n")
