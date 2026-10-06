# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone (on the server, where data/ch3_panel.rdata lives).
#
# External benchmark for the held and fished permit counts this pipeline
# builds, against CFEC's own fishery-year permit totals in
# context_data/data info/BIT.csv (downloaded from the CFEC website). BIT
# reports, by fishery and year, Total Permits Issued/Renewed (the held
# benchmark) and Total Permits Fished (the fished benchmark), with
# residents and nonresidents summed.
#
# Both sides are compared at the permit-serial level, distinct
# CFEC.Permit.Serial.Number per Fishery x Batch.Year, so a permit sold
# mid-year, or held by two owners in one year, counts once. That avoids
# depending on the File.Number match between register and tickets, the
# open leasing question chapter3_writeup.tex's own caveat raises for
# halibut and sablefish.
#
# Known structural differences, labelled in the output rather than hidden:
#   - Excluded codes (junk gear, data-gap gear 04/08/18, non-harvest codes)
#     are dropped from our side before the held set is built, so BIT rows
#     for them will show as differences by construction. The main
#     comparison below is restricted to kept codes, with excluded codes
#     reported separately.
#   - Our held set requires Permit.Status == "Current Owner", and our fished
#     set requires positive ticket revenue under a ticket File.Number, so
#     ticket rows with a missing File.Number do not appear in owner_permit_year.
#   - BIT starts in 1975 and runs to 2025 (2025 preliminary), so both sides
#     are restricted to the panel's own year range (MIN_YEAR to MAX_YEAR).
#
# Reads data/ch3_panel.rdata (owner_permit_year, MAX_YEAR) built by
# 01_build_panel.R, and BIT.csv from the repo's context_data folder.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_permit_year") || !exists("fished_vessel_permit_year") || !exists("MAX_YEAR")) {
  load(panel_path)
}

BIT_PATH <- file.path(intermediate_dir, "BIT.csv")
if (!file.exists(BIT_PATH)) stop("BIT.csv not found at ", BIT_PATH, ", check the working directory.")

bit <- read.csv(BIT_PATH, check.names = FALSE, na.strings = ".", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  transmute(
    Fishery      = gsub(" ", "", Fishery),
    Batch.Year   = as.integer(Year),
    bit.issued   = as.numeric(gsub(",", "", `Total Permits Issued/Renewed`)),
    bit.fished   = as.numeric(gsub(",", "", `Total Permits Fished`))
  ) %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

cat("BIT fishery-years in panel range:", nrow(bit), "\n")

# Held comes from the register, keyed on owner. Fished comes from ticket
# permit serials regardless of which filing number the ticket carries, so a
# filing-number mismatch or a missing filing number does not drop a landing.
ours_held <- owner_permit_year %>%
  group_by(Fishery, Batch.Year) %>%
  summarise(our.held = n_distinct(CFEC.Permit.Serial.Number[held]), .groups = "drop")

ours_fished <- fished_vessel_permit_year %>%
  filter(revenue > 0) %>%
  group_by(Fishery, Batch.Year) %>%
  summarise(our.fished = n_distinct(CFEC.Permit.Serial.Number), .groups = "drop")

ours <- full_join(ours_held, ours_fished, by = c("Fishery", "Batch.Year"))

cmp <- full_join(bit, ours, by = c("Fishery", "Batch.Year")) %>%
  mutate(
    gear = substr(Fishery, 2, 3),
    treatment = case_when(
      gear %in% JUNK_GEAR_CODES ~ "excluded: junk gear",
      gear %in% EXCLUDED_GEAR_DIGITS_DATA_GAP ~ "excluded: data-gap gear",
      Fishery %in% NON_HARVEST_FISHERY_CODES ~ "excluded: non-harvest code",
      TRUE ~ "kept"
    ),
    in.bit = !is.na(bit.issued),
    in.ours = !is.na(our.held),
    held.diff = our.held - bit.issued,
    held.pct  = 100 * held.diff / bit.issued,
    fished.diff = our.fished - bit.fished,
    fished.pct  = 100 * fished.diff / bit.fished
  )

cat("\n===== Coverage: fishery-years in BIT vs in our panel, by treatment =====\n")
print(cmp %>% count(treatment, in.bit, in.ours), n = Inf)

kept <- cmp %>% filter(treatment == "kept", in.bit, in.ours, bit.issued > 0)

summarise_bench <- function(d, bit_col, our_col, pct_col) {
  d <- d %>% filter(!is.na(.data[[pct_col]]))
  tibble(
    n.fishery.years = nrow(d),
    total.bit = sum(d[[bit_col]], na.rm = TRUE),
    total.ours = sum(d[[our_col]], na.rm = TRUE),
    aggregate.ratio = round(total.ours / total.bit, 3),
    median.pct.diff = round(median(d[[pct_col]]), 2),
    mean.abs.pct.diff = round(mean(abs(d[[pct_col]])), 2),
    share.within.5pct = round(mean(abs(d[[pct_col]]) <= 5), 3),
    share.within.10pct = round(mean(abs(d[[pct_col]]) <= 10), 3),
    spearman.cor = round(cor(d[[bit_col]], d[[our_col]], method = "spearman"), 3)
  )
}

cat("\n===== Held side (Total Permits Issued/Renewed vs our held permit serials), kept codes =====\n")
print(summarise_bench(kept, "bit.issued", "our.held", "held.pct"), width = Inf)

cat("\n===== Fished side (Total Permits Fished vs our fished permit serials), kept codes =====\n")
kept_fished <- cmp %>% filter(treatment == "kept", in.bit, in.ours, bit.fished > 0)
print(summarise_bench(kept_fished, "bit.fished", "our.fished", "fished.pct"), width = Inf)

cat("\n===== Year totals, kept codes, MATCHED fishery-years only (both sides present) =====\n")
year_matched <- cmp %>%
  filter(treatment == "kept", in.bit, in.ours) %>%
  group_by(Batch.Year) %>%
  summarise(
    bit.issued = sum(bit.issued, na.rm = TRUE),
    our.held   = sum(our.held, na.rm = TRUE),
    bit.fished = sum(bit.fished, na.rm = TRUE),
    our.fished = sum(our.fished, na.rm = TRUE),
    held.ratio   = round(our.held / bit.issued, 3),
    fished.ratio = round(our.fished / bit.fished, 3),
    .groups = "drop"
  )
print(year_matched, n = Inf, width = Inf)

cat("\n===== Kept fishery-years in our panel but NOT in BIT (ours-only), by year =====\n")
year_ours_only <- cmp %>%
  filter(treatment == "kept", in.ours, !in.bit) %>%
  group_by(Batch.Year) %>%
  summarise(n.fishery.years = n(), our.held = sum(our.held, na.rm = TRUE),
            our.fished = sum(our.fished, na.rm = TRUE), .groups = "drop")
print(year_ours_only, n = Inf, width = Inf)

cat("\n===== Ours-only fishery-years, by fishery (top 40 by held permits) =====\n")
print(cmp %>% filter(treatment == "kept", in.ours, !in.bit) %>%
        group_by(Fishery) %>%
        summarise(n.years = n(), first.year = min(Batch.Year), last.year = max(Batch.Year),
                  our.held = sum(our.held, na.rm = TRUE), our.fished = sum(our.fished, na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(our.held)) %>% head(40),
      n = 40, width = Inf)

cat("\n===== Largest held-side discrepancies, kept codes (top 25 by absolute difference) =====\n")
print(kept %>% arrange(desc(abs(held.diff))) %>%
        select(Fishery, Batch.Year, bit.issued, our.held, held.diff, held.pct) %>% head(25),
      n = 25)

cat("\n===== Largest fished-side discrepancies, kept codes (top 25 by absolute difference) =====\n")
print(kept_fished %>% arrange(desc(abs(fished.diff))) %>%
        select(Fishery, Batch.Year, bit.fished, our.fished, fished.diff, fished.pct) %>% head(25),
      n = 25)

DIAG_FISHERIES <- c("B06B", "L12T")

cat("\n===== Serial-level linkage for DIAG_FISHERIES (owner-serial rows per year) =====\n")
cat("held.fished = held and fished, held.not.fished = held but no ticket revenue,\n",
    "fished.not.held = ticket revenue under an owner/serial with no register holding that year\n")
diag_linkage <- owner_permit_year %>%
  filter(Fishery %in% DIAG_FISHERIES) %>%
  group_by(Fishery, Batch.Year) %>%
  summarise(
    rows.held.fished     = sum(held & fished),
    rows.held.not.fished = sum(held & !fished),
    rows.fished.not.held = sum(fished & !held),
    .groups = "drop"
  ) %>%
  left_join(cmp %>% select(Fishery, Batch.Year, bit.issued, bit.fished),
            by = c("Fishery", "Batch.Year"))
print(diag_linkage, n = Inf, width = Inf)

cat("\n===== Fished-not-held rows, DIAG_FISHERIES, 2016-2017, largest revenue first =====\n")
cat("Each row is a File.Number with ticket revenue under a serial the register does not list as held for them that year.\n")
print(owner_permit_year %>%
        filter(Fishery %in% DIAG_FISHERIES, Batch.Year %in% c(2016, 2017), fished, !held) %>%
        arrange(desc(revenue)) %>%
        select(Fishery, Batch.Year, File.Number, CFEC.Permit.Serial.Number, revenue) %>%
        head(30),
      n = 30, width = Inf)

cat("\n===== Excluded codes, for reference only (what the exclusions remove) =====\n")
print(cmp %>% filter(treatment != "kept", in.bit) %>%
        group_by(treatment) %>%
        summarise(bit.issued = sum(bit.issued, na.rm = TRUE),
                  bit.fished = sum(bit.fished, na.rm = TRUE),
                  our.held   = sum(our.held, na.rm = TRUE),
                  .groups = "drop"))

if (exists("table_dir")) {
  write.csv(cmp, file.path(table_dir, "bit_benchmark_fishery_year.csv"), row.names = FALSE)
  cat("\nWrote bit_benchmark_fishery_year.csv to", table_dir, "\n")
}
