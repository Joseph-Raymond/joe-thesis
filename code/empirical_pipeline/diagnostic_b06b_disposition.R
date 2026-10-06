# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R
# run this standalone on the server.
#
# Reads the raw yearly AKFIN fish-ticket files directly rather than the saved
# catch_data_temp object, so every field is checked against the source files.
# It keeps all halibut (species B) lines
# so the B06B total can be compared with the whole halibut fleet. It checks the
# zero-value landings, the fished-permit definition, and the 2016 and 2017 BIT
# drop, under several rules for what counts as fished.
#
# Raw headers are passed through make.names() before matching, the same
# renaming base read.csv applies in the pipeline, so "Batch Year" matches
# Batch.Year.
#
# Disposition codes come from the eLandings list in
# Context_papers/CFEC codes/elanding_codes.pdf. Sale-type codes are 60 (sold),
# 61 (sold for bait), 62 (overage), 64 (tagged IFQ fish) and 87 (retained for
# future sale, IFQ halibut and sablefish only). A blank disposition also counts
# as sale-type unless its delivery price category is 92, 95, 98 or 99. The
# other codes are non-sale, including 92 (bait, not sold), 95 (personal use), 98
# (discard at sea), 99 (discard onshore) and 63 (confiscated).
#
# Parts
#   0.  Raw header names, and which key fields each file has.
#   0b. Field coverage by year for B06B.
#   A.  Pounds and zero-value share by disposition code, all years pooled.
#   B.  Share of B06B pounds by disposition code, by year.
#   C.  Disposition of zero-value landings that have positive pounds.
#   D.  Sale-type pounds against BIT total pounds, by year.
#   E.  Fished B06B permit serials per year under four definitions, against BIT.
#   F.  All halibut pounds per year, and B06B's share of the total.

source("code/empirical_pipeline/00_setup.R")

if (!exists("MAX_YEAR")) load(panel_path)

RAW_DIR <- "/home/akfin/"
DIAG_CODE <- "B06B"
SALE_DISPOSITIONS <- c("60", "61", "62", "64", "87")
NONSALE_DELIVERY <- c("92", "95", "98", "99")
KEY_COLS <- c("CFEC.Permit.Fishery", "Permit.Fishery", "Batch.Year",
              "CFEC.Permit.Serial.Number", "Disposition.Code",
              "Pounds..Detail.", "CFEC.Value..Detail.")
FISH_CANDIDATES <- c("CFEC.Permit.Fishery", "Permit.Fishery")
KEEP_COLS <- c(
  "Batch.Year", "Vessel.ADFG.Number", "CFEC.Permit.Serial.Number",
  "CFEC.Permit.Holder.Filing.Number", "Ticket.Type", "Species.Code",
  "Gear.Code", "Harvest.Code", "Disposition.Code", "Delivery.Code",
  "CFEC.Price.Category.Delivery", "Pounds..Detail.", "CFEC.Value..Detail.",
  "Whole.Pounds..Detail.", "CFEC.Whole.Pounds..Detail.",
  "CFEC.Price.per.Pound", "Price", "Meal.Flag", "Ancillary.Primary",
  "CFEC.Landing.Status", "Statistical.Area"
)

# IPHC 2016 Annual Report, Alaska IFQ and CDQ landings in dressed weight
# (eviscerated, head-off). Round weight is dressed weight divided by 0.75,
# per the same report.
IPHC_ALASKA_2016_DRESSED <- 17677000

raw_files <- list.files(RAW_DIR, pattern = "\\.csv$", full.names = TRUE)
if (length(raw_files) == 0) stop("No CSV files found in ", RAW_DIR)
cat("Raw files found", length(raw_files), "\n")

raw_header <- function(f) {
  names(read_csv(f, n_max = 0, col_types = cols(.default = col_character()),
                 show_col_types = FALSE, progress = FALSE))
}

cat("\n===== 0. Key fields present in each file, after make.names() =====\n")
header_check <- tibble(file = basename(raw_files)) %>%
  mutate(
    cleaned = lapply(raw_files, function(f) make.names(raw_header(f), unique = TRUE)),
    n.cols  = lengths(cleaned)
  )
for (k in KEY_COLS) {
  header_check[[k]] <- vapply(header_check$cleaned, function(h) k %in% h, logical(1))
}
print(header_check %>% select(-cleaned), n = Inf, width = Inf)

read_halibut <- function(f) {
  raw   <- raw_header(f)
  clean <- make.names(raw, unique = TRUE)
  fish_clean <- FISH_CANDIDATES[FISH_CANDIDATES %in% clean][1]
  if (is.na(fish_clean)) {
    cat("Skipped", basename(f), "with no fishery column\n")
    return(NULL)
  }
  keep_raw <- raw[clean %in% c(fish_clean, KEEP_COLS)]
  d <- read_csv(f, col_select = all_of(keep_raw),
                col_types = cols(.default = col_character()),
                show_col_types = FALSE, progress = FALSE)
  names(d) <- make.names(names(d), unique = TRUE)
  for (cc in setdiff(KEEP_COLS, names(d))) d[[cc]] <- NA_character_
  d %>%
    rename(fishery.raw = all_of(fish_clean)) %>%
    mutate(fishery.clean = gsub(" ", "", fishery.raw)) %>%
    filter(substr(fishery.clean, 1, 1) == "B") %>%
    mutate(source.file = basename(f))
}

halibut_all <- bind_rows(lapply(raw_files, read_halibut)) %>%
  mutate(
    Batch.Year = as.integer(Batch.Year),
    pounds     = as.numeric(gsub(",", "", Pounds..Detail.)),
    value      = as.numeric(gsub(",", "", CFEC.Value..Detail.)),
    has.disp   = !is.na(Disposition.Code) & trimws(Disposition.Code) != "",
    disp       = ifelse(has.disp, trimws(Disposition.Code), "(blank)"),
    whole.pounds      = as.numeric(gsub(",", "", Whole.Pounds..Detail.)),
    cfec.whole.pounds = as.numeric(gsub(",", "", CFEC.Whole.Pounds..Detail.)),
    sale.type  = ifelse(has.disp,
                        disp %in% SALE_DISPOSITIONS,
                        !(CFEC.Price.Category.Delivery %in% NONSALE_DELIVERY)),
    zero.value = is.na(value) | value == 0,
    zero.value.pos.pounds = zero.value & !is.na(pounds) & pounds > 0
  ) %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

b06 <- halibut_all %>% filter(fishery.clean == DIAG_CODE)

cat(sprintf("\nAll-halibut rows in panel years, count %d\n", nrow(halibut_all)))
cat(sprintf("B06B rows in panel years, count %d\n", nrow(b06)))
cat("Years present", paste(sort(unique(b06$Batch.Year)), collapse = ", "), "\n")

cat("\n===== 0b. Field coverage by year (share of B06B rows with each field) =====\n")
print(b06 %>%
        group_by(Batch.Year) %>%
        summarise(
          rows           = n(),
          disp.present   = round(mean(has.disp), 3),
          pounds.present = round(mean(!is.na(pounds)), 3),
          value.present  = round(mean(!is.na(value)), 3),
          .groups = "drop"
        ),
      n = Inf, width = Inf)

cat("\n===== A. B06B by disposition code, all years pooled =====\n")
disp_summary <- b06 %>%
  group_by(disp) %>%
  summarise(
    rows                  = n(),
    pounds                = sum(pounds, na.rm = TRUE),
    zero.value.rows       = sum(zero.value),
    zero.value.pos.pounds = sum(zero.value.pos.pounds),
    .groups = "drop"
  ) %>%
  mutate(
    share.rows       = round(rows / sum(rows), 4),
    share.pounds     = round(pounds / sum(pounds), 4),
    share.zero.value = round(zero.value.rows / rows, 3)
  ) %>%
  arrange(desc(rows))
print(disp_summary, n = Inf, width = Inf)

cat("\n===== B. Share of B06B pounds by disposition, by year (dispositions with at least 2% of pounds, rest pooled) =====\n")
major_disp <- disp_summary %>% filter(share.pounds >= 0.02) %>% pull(disp)
by_year_disp <- b06 %>%
  mutate(disp.group = ifelse(disp %in% major_disp, disp, "other")) %>%
  group_by(Batch.Year, disp.group) %>%
  summarise(pounds = sum(pounds, na.rm = TRUE), .groups = "drop") %>%
  group_by(Batch.Year) %>%
  mutate(share = round(pounds / sum(pounds), 3)) %>%
  ungroup() %>%
  select(Batch.Year, disp.group, share) %>%
  tidyr::pivot_wider(names_from = disp.group, values_from = share, values_fill = 0) %>%
  arrange(Batch.Year)
print(by_year_disp, n = Inf, width = Inf)

cat("\n===== C. Zero-value landings with positive pounds, by disposition and delivery price category =====\n")
print(b06 %>%
        filter(zero.value.pos.pounds) %>%
        count(disp, CFEC.Price.Category.Delivery, sort = TRUE) %>%
        head(30),
      n = 30, width = Inf)

bit_path <- file.path(intermediate_dir, "BIT.csv")
bit_b06 <- read.csv(bit_path, check.names = FALSE, na.strings = ".", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  transmute(
    Fishery     = gsub(" ", "", Fishery),
    Batch.Year  = as.integer(Year),
    bit.pounds  = as.numeric(gsub(",", "", `Total Pounds`)),
    bit.fished  = as.numeric(gsub(",", "", `Total Permits Fished`))
  ) %>%
  filter(Fishery == DIAG_CODE, Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

cat("\n===== D. Pounds against BIT total pounds, by basis and year (all, sale-type, whole, CFEC whole) =====\n")
sale_vs_bit <- b06 %>%
  group_by(Batch.Year) %>%
  summarise(
    all.pounds        = sum(pounds, na.rm = TRUE),
    sale.pounds       = sum(pounds[sale.type], na.rm = TRUE),
    pounds.whole      = sum(whole.pounds, na.rm = TRUE),
    pounds.cfec.whole = sum(cfec.whole.pounds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(bit_b06, by = "Batch.Year") %>%
  mutate(
    all.over.bit        = round(all.pounds / bit.pounds, 3),
    sale.over.bit       = round(sale.pounds / bit.pounds, 3),
    whole.over.bit      = round(pounds.whole / bit.pounds, 3),
    cfec.whole.over.bit = round(pounds.cfec.whole / bit.pounds, 3)
  )
print(sale_vs_bit, n = Inf, width = Inf)

cat("\n===== E. Fished B06B permit serials per year under four definitions, against BIT Total Permits Fished =====\n")
cat("pounds.pos is any positive pounds. value.pos is any positive value.\n",
    "either is positive pounds or positive value.\n",
    "sale.pounds is a sale-type disposition with positive pounds.\n")
fished_counts <- b06 %>%
  group_by(Batch.Year) %>%
  summarise(
    serials.pounds.pos  = n_distinct(CFEC.Permit.Serial.Number[!is.na(pounds) & pounds > 0]),
    serials.value.pos   = n_distinct(CFEC.Permit.Serial.Number[!is.na(value) & value > 0]),
    serials.either      = n_distinct(CFEC.Permit.Serial.Number[(!is.na(pounds) & pounds > 0) |
                                                              (!is.na(value) & value > 0)]),
    serials.sale.pounds = n_distinct(CFEC.Permit.Serial.Number[sale.type & !is.na(pounds) & pounds > 0]),
    .groups = "drop"
  ) %>%
  left_join(bit_b06 %>% select(Batch.Year, bit.fished), by = "Batch.Year") %>%
  mutate(
    ratio.pounds.pos = round(serials.pounds.pos / bit.fished, 3),
    ratio.value.pos  = round(serials.value.pos / bit.fished, 3),
    ratio.either     = round(serials.either / bit.fished, 3),
    ratio.sale       = round(serials.sale.pounds / bit.fished, 3)
  )
print(fished_counts, n = Inf, width = Inf)

cat("\n===== F. All halibut pounds per year (every B-coded fishery) and B06B's share =====\n")
halibut_total <- halibut_all %>%
  group_by(Batch.Year) %>%
  summarise(
    all.halibut.pounds = sum(pounds, na.rm = TRUE),
    b06b.pounds        = sum(pounds[fishery.clean == DIAG_CODE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(b06b.share = round(b06b.pounds / all.halibut.pounds, 3))
print(halibut_total, n = Inf, width = Inf)

bit_halibut_total <- read.csv(bit_path, check.names = FALSE, na.strings = ".", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  transmute(
    Fishery    = gsub(" ", "", Fishery),
    Batch.Year = as.integer(Year),
    bit.pounds = as.numeric(gsub(",", "", `Total Pounds`))
  ) %>%
  filter(substr(Fishery, 1, 1) == "B", Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  group_by(Batch.Year) %>%
  summarise(bit.all.halibut.pounds = sum(bit.pounds, na.rm = TRUE), .groups = "drop")

cat("\n===== F2. All-halibut pounds, ours against BIT summed over every B-coded fishery =====\n")
print(halibut_total %>%
        left_join(bit_halibut_total, by = "Batch.Year") %>%
        mutate(ours.over.bit = round(all.halibut.pounds / bit.all.halibut.pounds, 3)),
      n = Inf, width = Inf)

h2016 <- halibut_total %>% filter(Batch.Year == 2016) %>% pull(all.halibut.pounds)
cat(sprintf("\nIPHC 2016 Alaska IFQ and CDQ landings in dressed lb is %s\n",
            format(IPHC_ALASKA_2016_DRESSED, big.mark = ",")))
cat(sprintf("Our 2016 all-halibut pounds, raw, is %s\n", format(h2016, big.mark = ",")))
cat(sprintf("Ratio of raw to IPHC dressed is %.3f\n", h2016 / IPHC_ALASKA_2016_DRESSED))
cat(sprintf("Ratio of raw to IPHC round (dressed divided by 0.75) is %.3f\n",
            h2016 / (IPHC_ALASKA_2016_DRESSED / 0.75)))
