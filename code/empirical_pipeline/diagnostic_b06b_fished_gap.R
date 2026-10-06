# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone (on the server, where intermediate data/ lives).
#
# Explains the B06B (halibut, longline under 60 feet) fished-side gap against
# CFEC's BIT.csv, in three parts.
#
#   Part A. Ticket audit by year. For each year, how many B06B tickets have a
#           missing value but positive pounds (the zero-fill in 01_build_panel.R
#           Section 2 turns these into revenue 0, so they read as unfished),
#           how many have a missing filing number, and how many have a missing
#           permit serial. Also compares our ticket pounds and our permit count
#           with BIT's Total Pounds and Total Permits Fished.
#
#   Part B. Fallback serial match for held-not-fished rows. For each B06B
#           owner-serial-year our register says is held but unfished, look for
#           the same permit serial in the same year under any filing number,
#           then in the raw tickets with positive pounds, to sort these rows
#           into: owner mismatch, zero-fill, or no ticket at all.
#
#   Part C. Summary by year of the three counts side by side.
#
# Reads the raw ticket object catch_data_temp.rdata, the panel
# (owner_permit_year, fished_vessel_permit_year, MAX_YEAR), and BIT.csv.

source("code/empirical_pipeline/00_setup.R")

DIAG_CODE <- "B06B"

if (!exists("owner_permit_year") || !exists("fished_vessel_permit_year") || !exists("MAX_YEAR")) {
  load(panel_path)
}
load(file.path(intermediate_dir, "catch_data_temp.rdata"))

bit_path <- file.path(intermediate_dir, "BIT.csv")
if (!file.exists(bit_path)) stop("BIT.csv not found at ", bit_path)
bit_b06 <- read.csv(bit_path, check.names = FALSE, na.strings = ".", stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  transmute(
    Fishery      = gsub(" ", "", Fishery),
    Batch.Year   = as.integer(Year),
    bit.fished   = as.numeric(gsub(",", "", `Total Permits Fished`)),
    bit.pounds   = as.numeric(gsub(",", "", `Total Pounds`)),
    bit.earnings = as.numeric(gsub("[$,]", "", `Total Earnings`))
  ) %>%
  filter(Fishery == DIAG_CODE, Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

b06_tickets <- catch_data_temp %>%
  mutate(
    Fishery                   = strip_fishery_space(CFEC.Permit.Fishery),
    Vessel.ADFG.Number        = as.integer(Vessel.ADFG.Number),
    CFEC.Permit.Serial.Number = as.integer(CFEC.Permit.Serial.Number),
    pounds                    = as.numeric(Pounds..Detail.),
    value                     = as.numeric(CFEC.Value..Detail.)
  ) %>%
  filter(Fishery == DIAG_CODE, Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR)

ours_fished <- fished_vessel_permit_year %>%
  filter(Fishery == DIAG_CODE, revenue > 0) %>%
  group_by(Batch.Year) %>%
  summarise(our.fished.permits = n_distinct(CFEC.Permit.Serial.Number), .groups = "drop")

cat("\n===== Part A. B06B ticket audit by year (raw tickets, before any cleaning) =====\n")
cat("zero.value.pos.pounds = rows with value exactly 0 but positive pounds\n",
    "na.value.pos.pounds = rows with missing value but positive pounds (zero-filled to 0)\n",
    "fpv.na.file = rows in fished_vessel_permit_year with missing File.Number\n",
    "our.pounds vs bit.pounds: ticket pounds against CFEC's own yearly total\n")

audit <- b06_tickets %>%
  group_by(Batch.Year) %>%
  summarise(
    rows                  = n(),
    na.value.pos.pounds   = sum(is.na(value) & pounds > 0, na.rm = TRUE),
    zero.value.pos.pounds = sum(value == 0 & pounds > 0, na.rm = TRUE),
    na.filing.number      = sum(is.na(CFEC.Permit.Holder.Filing.Number)),
    na.serial             = sum(is.na(CFEC.Permit.Serial.Number)),
    sentinel.vessel       = sum(Vessel.ADFG.Number %in% BAD_VESSEL_IDS),
    our.pounds            = sum(pounds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    fished_vessel_permit_year %>%
      filter(Fishery == DIAG_CODE) %>%
      group_by(Batch.Year) %>%
      summarise(fpv.na.file = sum(is.na(File.Number)), .groups = "drop"),
    by = "Batch.Year"
  ) %>%
  left_join(ours_fished, by = "Batch.Year") %>%
  left_join(bit_b06, by = "Batch.Year") %>%
  mutate(
    pounds.ratio.ours.to.bit   = round(our.pounds / bit.pounds, 3),
    permits.ratio.ours.to.bit  = round(our.fished.permits / bit.fished, 3)
  )
print(audit, n = Inf, width = Inf)

cat("\n===== Part B. Held-not-fished B06B owner-serial-years, fallback serial match =====\n")
cat("Categories, checked in this order:\n",
    "  owner mismatch: the same serial has a positive-value ticket this year under another filing number\n",
    "  zero-fill: the serial has positive pounds this year but no positive value\n",
    "  tickets without pounds: the serial appears on a ticket this year with no pounds\n",
    "  no ticket: nothing for this serial this year\n")

held_not_fished <- owner_permit_year %>%
  filter(Fishery == DIAG_CODE, held, !fished) %>%
  select(File.Number, Batch.Year, CFEC.Permit.Serial.Number)

serial_value <- fished_vessel_permit_year %>%
  filter(Fishery == DIAG_CODE, revenue > 0) %>%
  distinct(Batch.Year, CFEC.Permit.Serial.Number) %>%
  mutate(serial.has.value = TRUE)

serial_tickets <- b06_tickets %>%
  filter(!is.na(CFEC.Permit.Serial.Number)) %>%
  group_by(Batch.Year, CFEC.Permit.Serial.Number) %>%
  summarise(
    serial.has.pounds = any(pounds > 0, na.rm = TRUE),
    serial.has.ticket = TRUE,
    .groups = "drop"
  )

fallback <- held_not_fished %>%
  left_join(serial_value, by = c("Batch.Year", "CFEC.Permit.Serial.Number")) %>%
  left_join(serial_tickets, by = c("Batch.Year", "CFEC.Permit.Serial.Number")) %>%
  mutate(
    category = case_when(
      !is.na(serial.has.value) ~ "owner mismatch",
      !is.na(serial.has.pounds) & serial.has.pounds ~ "zero-fill",
      !is.na(serial.has.ticket) ~ "tickets without pounds",
      TRUE ~ "no ticket"
    )
  )

cat("Held-not-fished B06B owner-serial-years in total:", nrow(fallback), "\n")
print(fallback %>% count(category, sort = TRUE), n = Inf, width = Inf)

cat("\n===== Part C. Held-not-fished category by year =====\n")
print(fallback %>%
        count(Batch.Year, category) %>%
        tidyr::pivot_wider(names_from = category, values_from = n, values_fill = 0) %>%
        arrange(Batch.Year),
      n = Inf, width = Inf)
