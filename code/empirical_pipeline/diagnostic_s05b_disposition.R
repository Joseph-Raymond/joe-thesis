# Chapter 3 empirical pipeline, one-off diagnostic for S05B (salmon, hand
# troll, statewide), NOT part of run_all.R. Run standalone on the server.
#
# Reads the raw yearly AKFIN fish-ticket files directly and keeps S05B tickets
# only. Disposition codes come from the eLandings list in
# Context_papers/CFEC codes/elanding_codes.pdf. Sale-type codes are 60 (sold),
# 61 (sold for bait), 62 (overage), 64 (tagged IFQ fish), and 87 (retained for
# future sale). A blank disposition is sale-type unless its delivery price
# category is 92, 95, 98, or 99, which are the non-sale codes.
#
# Parts
#   0.  Ticket rows and key field coverage by year.
#   A.  S05B by disposition code, all years pooled.
#   B.  Share of S05B pounds by disposition, by year.
#   C.  Zero-value landings with positive pounds, by disposition and delivery
#       price category.
#   D.  Permit-years by the kind of pounds they carry.
#   E.  Fished S05B permits per year under the value rule, against BIT Total
#       Permits Fished, with BIT earnings per fished permit.

source("code/empirical_pipeline/00_setup.R")

if (!exists("MAX_YEAR")) load(panel_path)

RAW_DIR <- "/home/akfin/"
DIAG_CODE <- "S05B"
SALE_DISPOSITIONS <- c("60", "61", "62", "64", "87")
NONSALE_DELIVERY <- c("92", "95", "98", "99")
FISH_CANDIDATES <- c("CFEC.Permit.Fishery", "Permit.Fishery")
KEEP_COLS <- c(
  "Batch.Year", "Vessel.ADFG.Number", "CFEC.Permit.Serial.Number",
  "CFEC.Permit.Holder.Filing.Number", "Ticket.Type", "Species.Code",
  "Gear.Code", "Disposition.Code", "Delivery.Code",
  "CFEC.Price.Category.Delivery", "Pounds..Detail.", "CFEC.Value..Detail.",
  "Whole.Pounds..Detail."
)

raw_files <- list.files(RAW_DIR, pattern = "\\.csv$", full.names = TRUE)
if (length(raw_files) == 0) stop("No CSV files found in ", RAW_DIR)

raw_header <- function(f) {
  names(read_csv(f, n_max = 0, col_types = cols(.default = col_character()),
                 show_col_types = FALSE, progress = FALSE))
}

read_s05b <- function(f) {
  raw   <- raw_header(f)
  clean <- make.names(raw, unique = TRUE)
  fish_clean <- FISH_CANDIDATES[FISH_CANDIDATES %in% clean][1]
  keep_raw <- raw[clean %in% c(fish_clean, KEEP_COLS)]
  d <- read_csv(f, col_select = all_of(keep_raw),
                col_types = cols(.default = col_character()),
                show_col_types = FALSE, progress = FALSE)
  names(d) <- make.names(names(d), unique = TRUE)
  for (cc in setdiff(KEEP_COLS, names(d))) d[[cc]] <- NA_character_
  d %>%
    rename(fishery.raw = all_of(fish_clean)) %>%
    mutate(fishery.clean = gsub(" ", "", fishery.raw)) %>%
    filter(fishery.clean == DIAG_CODE)
}

s05 <- bind_rows(lapply(raw_files, read_s05b)) %>%
  mutate(
    Batch.Year                = as.integer(Batch.Year),
    CFEC.Permit.Serial.Number = as.integer(CFEC.Permit.Serial.Number),
    pounds     = as.numeric(gsub(",", "", Pounds..Detail.)),
    value      = as.numeric(gsub(",", "", CFEC.Value..Detail.)),
    has.disp   = !is.na(Disposition.Code) & trimws(Disposition.Code) != "",
    disp       = ifelse(has.disp, trimws(Disposition.Code), "(blank)"),
    sale.type  = ifelse(has.disp,
                        disp %in% SALE_DISPOSITIONS,
                        !(CFEC.Price.Category.Delivery %in% NONSALE_DELIVERY)),
    zero.value = is.na(value) | value == 0,
    zero.value.pos.pounds = zero.value & !is.na(pounds) & pounds > 0
  ) %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

cat(sprintf("\nS05B ticket rows in panel years, count %d\n", nrow(s05)))

cat("\n===== 0. Ticket rows and key field coverage by year =====\n")
print(s05 %>%
        group_by(Batch.Year) %>%
        summarise(
          rows           = n(),
          disp.present   = round(mean(has.disp), 3),
          pounds.present = round(mean(!is.na(pounds)), 3),
          value.present  = round(mean(!is.na(value)), 3),
          .groups = "drop"
        ),
      n = Inf, width = Inf)

cat("\n===== A. S05B by disposition code, all years pooled =====\n")
disp_summary <- s05 %>%
  group_by(disp) %>%
  summarise(
    rows                  = n(),
    pounds                = sum(pounds, na.rm = TRUE),
    value                 = sum(value, na.rm = TRUE),
    zero.value.rows       = sum(zero.value),
    zero.value.pos.pounds = sum(zero.value.pos.pounds),
    .groups = "drop"
  ) %>%
  mutate(
    share.rows       = round(rows / sum(rows), 4),
    share.pounds     = round(pounds / sum(pounds), 4),
    share.value      = round(value / sum(value), 4),
    share.zero.value = round(zero.value.rows / rows, 3)
  ) %>%
  arrange(desc(rows))
print(disp_summary, n = Inf, width = Inf)

cat("\n===== B. Share of S05B pounds by disposition, by year (codes with at least 2% of pounds, rest pooled) =====\n")
major_disp <- disp_summary %>% filter(share.pounds >= 0.02) %>% pull(disp)
by_year_disp <- s05 %>%
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
print(s05 %>%
        filter(zero.value.pos.pounds) %>%
        count(disp, CFEC.Price.Category.Delivery, sort = TRUE) %>%
        head(30),
      n = 30, width = Inf)

cat("\n===== D. Permit-years by the kind of pounds they carry =====\n")
cat("Value rule means any ticket with positive value. Pounds but no value means\n",
    "positive pounds on some ticket and no positive value. No pounds means neither.\n")
permit_kind <- s05 %>%
  filter(!is.na(CFEC.Permit.Serial.Number)) %>%
  group_by(Batch.Year, CFEC.Permit.Serial.Number) %>%
  summarise(
    any.value  = any(!is.na(value) & value > 0),
    any.pounds = any(!is.na(pounds) & pounds > 0),
    .groups = "drop"
  ) %>%
  mutate(
    kind = case_when(
      any.value  ~ "value rule fished",
      any.pounds ~ "pounds but no value",
      TRUE       ~ "no pounds"
    )
  )
print(permit_kind %>%
        count(Batch.Year, kind) %>%
        tidyr::pivot_wider(names_from = kind, values_from = n, values_fill = 0) %>%
        arrange(Batch.Year),
      n = Inf, width = Inf)

cat("\n===== E. Fished S05B permits per year against BIT =====\n")
bit_s05 <- read.csv(file.path(intermediate_dir, "BIT.csv"), check.names = FALSE,
                    na.strings = ".", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  transmute(
    Fishery      = gsub(" ", "", Fishery),
    Batch.Year   = as.integer(Year),
    bit.issued   = as.numeric(gsub(",", "", `Total Permits Issued/Renewed`)),
    bit.fished   = as.numeric(gsub(",", "", `Total Permits Fished`)),
    bit.earnings = as.numeric(gsub("[$,]", "", `Total Earnings`)),
    bit.price    = as.numeric(gsub("[$,]", "", `Average Permit Price`))
  ) %>%
  filter(Fishery == DIAG_CODE, Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

ours_s05 <- s05 %>%
  filter(!is.na(CFEC.Permit.Serial.Number)) %>%
  group_by(Batch.Year) %>%
  summarise(
    our.value.fished  = n_distinct(CFEC.Permit.Serial.Number[!is.na(value) & value > 0]),
    our.pounds.fished = n_distinct(CFEC.Permit.Serial.Number[!is.na(pounds) & pounds > 0]),
    .groups = "drop"
  )

compare_s05 <- bit_s05 %>%
  left_join(ours_s05, by = "Batch.Year") %>%
  mutate(
    value.over.bit        = round(our.value.fished / bit.fished, 3),
    pounds.over.bit       = round(our.pounds.fished / bit.fished, 3),
    bit.earnings.per.fish = round(bit.earnings / bit.fished)
  ) %>%
  select(Batch.Year, bit.issued, bit.fished, our.value.fished,
         our.pounds.fished, value.over.bit, pounds.over.bit,
         bit.price, bit.earnings.per.fish)
print(compare_s05, n = Inf, width = Inf)
