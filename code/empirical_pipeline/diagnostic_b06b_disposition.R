# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone on the server.
#
# Reads the raw yearly AKFIN fish-ticket files directly (not the 68-column
# catch_data_temp.rdata, which dropped the disposition fields), keeps only
# B06B landing lines, and checks whether the zero-value landings and the
# 2016 and 2017 BIT drop line up with ADFG_I_DISPOSITION_CODE.
#
# Disposition codes 60, 62, 63, and 64, or a blank disposition, are priced by
# delivery condition. Any other disposition (bait, fishmeal, personal use)
# is priced by disposition, per the AKFIN Comprehensive Fish Ticket guide.
#
# Parts
#   A. Pounds and zero-value share by disposition code, all years pooled.
#   B. Share of B06B pounds by disposition code, by year.
#   C. Disposition of zero-value landings that have positive pounds.
#   D. Sale-type pounds against BIT's total pounds, by year.

source("code/empirical_pipeline/00_setup.R")

RAW_DIR <- "/home/akfin/"
DIAG_CODE <- "B06B"
SALE_DISPOSITIONS <- c("60", "62", "63", "64")

KEEP_COLS <- c(
  "Batch.Year", "Vessel.ADFG.Number", "CFEC.Permit.Fishery",
  "CFEC.Permit.Serial.Number", "CFEC.Permit.Holder.Filing.Number",
  "Ticket.Type", "Species.Code", "Gear.Code", "Harvest.Code",
  "Disposition.Code", "Delivery.Code", "CFEC.Price.Category.Delivery",
  "Pounds..Detail.", "CFEC.Value..Detail.", "CFEC.Price.per.Pound", "Price",
  "Meal.Flag", "Ancillary.Primary", "CFEC.Landing.Status", "Statistical.Area"
)

raw_files <- list.files(RAW_DIR, pattern = "\\.csv$", full.names = TRUE)
if (length(raw_files) == 0) stop("No CSV files found in ", RAW_DIR)
cat("Raw files found:", length(raw_files), "\n")

read_b06b <- function(f) {
  d <- read_csv(f, col_select = any_of(KEEP_COLS),
                col_types = cols(.default = col_character()),
                show_col_types = FALSE, progress = FALSE)
  d %>%
    filter(gsub(" ", "", CFEC.Permit.Fishery) == DIAG_CODE) %>%
    mutate(source.file = basename(f))
}

b06 <- bind_rows(lapply(raw_files, read_b06b)) %>%
  mutate(
    Batch.Year = as.integer(Batch.Year),
    pounds     = as.numeric(gsub(",", "", `Pounds..Detail.`)),
    value      = as.numeric(gsub(",", "", `CFEC.Value..Detail.`)),
    disp       = ifelse(is.na(Disposition.Code) | trimws(Disposition.Code) == "",
                        "(blank)", trimws(Disposition.Code)),
    sale.type  = disp %in% c(SALE_DISPOSITIONS, "(blank)"),
    zero.value = is.na(value) | value == 0,
    zero.value.pos.pounds = zero.value & !is.na(pounds) & pounds > 0
  ) %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

cat("B06B landing lines in panel years:", nrow(b06), "\n")
cat("Years present:", paste(sort(unique(b06$Batch.Year)), collapse = ", "), "\n")

cat("\n===== A. B06B by disposition code, all years pooled =====\n")
disp_summary <- b06 %>%
  group_by(disp) %>%
  summarise(
    rows              = n(),
    pounds            = sum(pounds, na.rm = TRUE),
    zero.value.rows   = sum(zero.value),
    zero.value.pos.pounds = sum(zero.value.pos.pounds),
    .groups = "drop"
  ) %>%
  mutate(
    share.rows        = round(rows / sum(rows), 4),
    share.pounds      = round(pounds / sum(pounds), 4),
    share.zero.value  = round(zero.value.rows / rows, 3)
  ) %>%
  arrange(desc(rows))
print(disp_summary, n = Inf, width = Inf)

cat("\n===== B. Share of B06B pounds by disposition, by year (dispositions above 2% pooled) =====\n")
major_disp <- disp_summary %>% filter(share.pounds >= 0.02) %>% pull(disp)
by_year_disp <- b06 %>%
  mutate(disp.group = ifelse(disp %in% major_disp, disp, "other")) %>%
  group_by(Batch.Year, disp.group) %>%
  summarise(pounds = sum(pounds, na.rm = TRUE), .groups = "drop_last") %>%
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

cat("\n===== D. Sale-type pounds against BIT total pounds, by year =====\n")
bit_path <- file.path(intermediate_dir, "BIT.csv")
bit_b06 <- read.csv(bit_path, check.names = FALSE, na.strings = ".", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  transmute(
    Fishery    = gsub(" ", "", Fishery),
    Batch.Year = as.integer(Year),
    bit.pounds = as.numeric(gsub(",", "", `Total Pounds`))
  ) %>%
  filter(Fishery == DIAG_CODE, Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

sale_vs_bit <- b06 %>%
  group_by(Batch.Year) %>%
  summarise(
    all.pounds  = sum(pounds, na.rm = TRUE),
    sale.pounds = sum(pounds[sale.type], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(bit_b06, by = "Batch.Year") %>%
  mutate(
    all.over.bit  = round(all.pounds / bit.pounds, 3),
    sale.over.bit = round(sale.pounds / bit.pounds, 3)
  )
print(sale_vs_bit, n = Inf, width = Inf)
