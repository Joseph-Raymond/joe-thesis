# Chapter 3 empirical pipeline, shared setup
#
# Sourced by every other script in this folder. Sets working directory, loads
# packages, and defines constants and small helpers that more than one script
# needs.
#
# THIS CANNOT BE RUN LOCALLY. Per chapter3_plan.md Section 0.3 the CFEC/AKFIN
# data lives only on the secure server. Move this whole folder to
# ~/Rprojects/joe-thesis/code/empirical_pipeline/ on the server and run there.
# ssh -L localhost:8989:localhost:3389 jraymond@makena.ucdavis.edu, then connect
# to localhost:8989.
#
# Column names below are inferred from the existing scripts in Chpt3/code/
# (mainly permit_link.R and myfunctions.R) and from chapter3_plan.md Section 1.
# They have not been checked against real headers on the server. Search for
# "CHECK:" comments below before trusting any output.

rm(list = ls())

packs <- c("tidyverse", "lubridate", "stringr", "data.table", "fixest", "xtable")
new.packages <- packs[!(packs %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
invisible(lapply(packs, require, character.only = TRUE))

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
source("code/myfunctions.R")

# ---- paths -------------------------------------------------------------

pipeline_dir     <- "code/empirical_pipeline"
intermediate_dir <- "intermediate data"
output_dir       <- file.path("Chpt3", "output")
table_dir        <- file.path(output_dir, "tables")
figure_dir       <- file.path(output_dir, "figures")

for (d in c(intermediate_dir, output_dir, table_dir, figure_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

panel_path <- file.path(intermediate_dir, "ch3_panel.rdata")

# ---- constants -----------------------------------------------------------

# First year of coverage matches permit_link.R's Batch.Year >= 1991 filter
MIN_YEAR <- 1991

# A vessel needs at least this many active years to enter vessel_summary.
# Flagged as a design choice worth revisiting (chapter3_plan.md Section 9.3),
# not a fact derived from the data.
MIN_ACTIVE_YEARS <- 5

# Sentinel values that mean "no real vessel", matching permit_link.R
BAD_VESSEL_IDS <- c(0, 99999)

# Number of roughly-equal calendar periods H_bar/H_LR/Phi also get computed
# over, alongside the whole-panel version, see the "Period-specific
# decomposition" section of 01_build_panel.R. The two period breakpoints
# are computed once from the observed data range (not hardcoded years), so
# they stay correct if the panel's coverage changes, but they are fixed for
# the run, the same two thresholds bucket every vessel and owner.
N_PERIODS <- 3

# A vessel/owner needs at least this many active years WITHIN a period to
# enter that period's summary. Deliberately lower than MIN_ACTIVE_YEARS (5),
# a period is roughly a third as long as the full panel, so a proportionally
# smaller minimum is the natural analogue, not a separately justified number.
MIN_ACTIVE_YEARS_PERIOD <- 4

# Gear digit lookup for salmon fisheries only, taken verbatim from CLAUDE.md
# and chapter3_plan.md Section 1. Not defined for non-salmon species letters,
# those fall to "Other/unclassified" in build_gear_class() (03_figure1_figure2.R).
SALMON_GEAR_DIGITS <- c(
  "01" = "Purse seine",
  "02" = "Beach seine",
  "03" = "Drift gillnet",
  "04" = "Set gillnet",
  "05" = "Hand troll/jig",
  "15" = "Power troll"
)

# Junk gear codes, checked directly against Context_papers/CFEC codes/Current
# CFEC Fishery Codes.txt on 2026-09-14. "77" is Hatchery Permit (special
# harvest area, not open commercial fishing) and "99" is Experimental/Special
# Permit, both confirmed non-commercial there. "13" is left in on the
# strength of Permit_Variance.R's precedent alone (a single 1982-only
# Southeast dip-net experimental code per that file's own comment), not
# independently re-checked against the dictionary here. "08" was REMOVED
# from this list on 2026-09-14, it is FISH WHEEL, a real, active,
# limited-entry salmon gear (S 08P, SALMON, FISH WHEEL, UPPER YUKON, Limited,
# 1976-2021, per the same dictionary), wrongly treated as junk for one round
# based only on Permit_Variance.R's comment and S08P's absence from
# SALMON_GEAR_DIGITS above (an incomplete convenience lookup, not an
# authority). That mistake briefly filtered 4,046 real held owner-years out
# of the panel. Before adding anything back to this list, check the
# dictionary directly rather than trusting what an old exploratory script
# happened to exclude.
JUNK_GEAR_CODES <- c("13", "77", "99")

# Gear digits excluded from the held-vs-fished wedge for a DIFFERENT reason
# than JUNK_GEAR_CODES above, these are real, valid, often economically
# major fisheries (Bristol Bay set gillnet alone issues roughly 1,000
# permits a year per CFEC's own Report 25-4N, "CFEC Salmon Set Gillnet
# Permits and DNR Shore Fishery Leases..."), not non-commercial permit
# types. The problem is a severe register-vs-ticket data gap specific to
# this extract, a lifetime footprint check (any role, any fishery, any
# year, ever, 01_build_panel.R Section 7) found roughly 73 percent of gear-
# "04" permit holders never appear anywhere in 16.3 million ticket rows,
# not once, in 30+ years, a rate far too extreme to be real economic
# behavior (CFEC's own published latency rate for comparable fisheries
# runs roughly 18.5 percent, not 95-100 percent). "04" is set gillnet
# across every species letter it appears under, confirmed against
# Context_papers/CFEC codes/Current CFEC Fishery Codes.txt for the salmon
# codes specifically. "08" (fish wheel) is included on direct evidence of
# the identical failure mode, S08P alone carries zero ticket-side rows
# under EITHER ticket-side fishery column (CFEC.Permit.Fishery and
# Permit.Fishery) across the whole 1991-2021 panel, exactly the pattern
# S04P showed. This is a decision to stop treating a known-untrustworthy
# measurement as if it were data, not a claim that these fisheries are
# actually unfished at this rate, chapter3_writeup.tex should say so
# explicitly wherever Table 3's wedge is discussed. "18" was ADDED on
# 2026-09-16, the gear_ghost_by_code diagnostic (01_build_panel.R Section 7)
# found a 70.1% lifetime ghost rate for it, comparable to what 04 and 08
# showed, and every fishery code under gear "18" in both the current and
# historical CFEC dictionary is SHOVEL-harvested clams (R 18B, statewide,
# 1975-2021, plus two smaller historical variants), a hand-tool, no-vessel
# method exactly like the other two. Gear "12" (dive/hand-pick, herring
# spawn on kelp, includes L12T) also runs elevated but was deliberately
# NOT added here, that is the same vessel-less category chapter3_writeup.tex
# already documents as a genuine example rather than a data problem, and its
# unused share (0.97) is visibly less extreme than the three excluded gears
# were before exclusion (0.997+). Gears "02" (beach seine, partly
# vessel-based) and "05" (hand troll, fully vessel-based) also run
# moderately elevated (34-36%) without a clean no-vessel-needed story behind
# them, left in for now for lack of evidence, not because they were checked
# and cleared. CHECK the gear_ghost_by_code diagnostic on every future run
# for any other gear digit developing the same signature before assuming
# this list is complete.
# Different problem again. Not a gear digit at all, this table lives in
# 04b_table_unused_by_fishery.R showing three codes stuck at a 1.00 unused
# share (E91QV, W22BV, W2ABV), each traced individually against
# Historical CFEC Fishery Codes.txt on 2026-09-16. All three carry a Status
# field of "Vessel Entry Permit" or "Vessel Moratorium" rather than an
# ordinary harvest status (Limited, Interim-Use), and the same grep turned
# up two more with that exact status not yet seen at the top of any ranking
# (W22HV, W2BBV). All five are historical only (expired 1997-2020, none in
# the current dictionary) and all five are a companion permit certain
# vessel-limitation programs (Bering Sea hair crab, weathervane scallop)
# issue to a VESSEL as a separate ownership/eligibility slot, distinct from
# the harvest permit a person holds under that fishery's base code (the
# harvest for W22BV happens under "W22B", not W22BV itself). No fish ticket
# was ever going to record revenue against the "V" code, so held-vs-fished
# is not measuring a gap for these at all, it is comparing a person's
# holding of an administrative slot against a harvest record that cannot
# exist under that code by construction. Excluded for the same underlying
# reason JUNK_GEAR_CODES is (not a real harvest permit), just identified by
# exact code rather than gear digit since these five don't share one.
NON_HARVEST_FISHERY_CODES <- c("E91QV", "W22BV", "W22HV", "W2ABV", "W2BBV")

EXCLUDED_GEAR_DIGITS_DATA_GAP <- c("04", "08", "18")

# ---- small helpers ---------------------------------------------------

strip_fishery_space <- function(x) str_replace_all(x, " ", "")

# Splits a no-space fishery code like "S03T" into species letter, two-digit
# gear code, and region letter. Assumes the CLAUDE.md-documented structure
# (species letter + 2 gear digits + region letter). CHECK against real
# fishery codes that may not follow this exactly (e.g. codes with 1 digit
# gear or multi-letter species are known to exist per Linking_code.R).
split_fishery_code <- function(fishery) {
  tibble(
    fishery = fishery,
    species = str_sub(fishery, 1, 1),
    gear    = str_sub(fishery, 2, 3),
    region  = str_sub(fishery, 4, 4)
  )
}

# Derives a within-year week number from Date.Landed (int, YYYYMMDD), used by
# 06_within_season_reallocation.R and 09_seasonal_overlap.R for anything at
# weekly grain. chapter3_plan.md Section 1's data dictionary lists both
# Statistical.Week and Week.Ending.Date as raw fish-ticket columns, but
# checked directly against the real catch_data_temp object on the server,
# neither is actually present, only Date.Landed, Date.Fishing.Began, and
# Batch.Year are. So this is a PROXY built from the one date field reliably
# there, not ADFG's own regulatory statistical-week code. week() buckets by
# day-of-year (1 + (yday-1) %/% 7, range 1-53) rather than
# lubridate::isoweek(), so a week never crosses a calendar year boundary,
# which matters since every grouping that uses this nests week inside
# Batch.Year.
derive_statistical_week <- function(date_landed_int) {
  lubridate::week(as.Date(as.character(date_landed_int), format = "%Y%m%d"))
}

# CPI deflator, optional. Put a two-column csv (Year, CPI) at
# Chpt3/data/cpi_deflator.csv, indexed to any base year, to deflate revenue.
# Without it every dollar figure downstream stays nominal, which biases CV
# (a variance measure) across the 1991-2021 panel. See chapter3_plan.md
# Section 9.3 on why CPI rather than a seafood price index is the right call.
load_deflator <- function() {
  deflator_path <- file.path("Chpt3", "data", "cpi_deflator.csv")
  if (!file.exists(deflator_path)) {
    warning(
      "No CPI deflator found at ", deflator_path,
      ". Proceeding with nominal dollars. ",
      "This is a known limitation, not a bug, until that file is added."
    )
    return(NULL)
  }
  read_csv(deflator_path, show_col_types = FALSE) %>%
    select(Year, CPI) %>%
    mutate(deflator = CPI / CPI[Year == max(Year)])
}

# Applies a deflator (or passes through nominal dollars with a message printed
# once) to any data frame with a Batch.Year column and a dollar column.
deflate <- function(df, dollar_col, deflator) {
  if (is.null(deflator)) return(df)
  df %>%
    left_join(deflator %>% select(Batch.Year = Year, deflator), by = "Batch.Year") %>%
    mutate(across(all_of(dollar_col), ~ .x / deflator)) %>%
    select(-deflator)
}

cat("00_setup.R loaded. MIN_ACTIVE_YEARS =", MIN_ACTIVE_YEARS,
    ", MIN_YEAR =", MIN_YEAR, "\n")
