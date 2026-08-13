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

# A fishery needs at least this many years of fleet-mean-revenue observations
# to enter the return covariance matrix behind Figure 3 (05_table4_figure3.R).
# Thin fisheries produce noisy variance/covariance estimates that would
# otherwise dominate the passive benchmark for any vessel invested in them.
MIN_FISHERY_RETURN_YEARS <- 10

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

# Junk gear codes filtered elsewhere in the existing code (Permit_Variance.R)
JUNK_GEAR_CODES <- c("08", "13", "77", "99")

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
