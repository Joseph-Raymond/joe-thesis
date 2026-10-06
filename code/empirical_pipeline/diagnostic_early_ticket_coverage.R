# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone (on the server, where intermediate data/ lives).
#
# Checks whether early-year fish tickets (1991 to about 1997) are less usable
# than later ones, which would explain why our fished counts run 9 to 13
# percent below CFEC's BIT.csv totals in 1991 to 1994 while our held counts
# match (diagnostic_external_benchmark_bit.R). If early tickets lack a
# vessel ID, a permit serial, or a fishery code, or if they fail to match
# any register row, the early unused share in Figure 1 is overstated.
#
# Works on the raw ticket object (catch_data_temp.rdata) before any cleaning
# in 01_build_panel.R, so sentinel and missing vessel IDs are still present.
# The register match uses every permit_clean row, not only Current Owner rows,
# so the match rate is an upper bound on what the held set can see.
#
# Also reports L12T (herring spawn, Bristol Bay hand pick) on its own, since
# that fishery has almost no fished permits in our data in 1991 to 1996 while
# BIT reports some.

source("code/empirical_pipeline/00_setup.R")

if (!exists("MAX_YEAR")) load(panel_path)

load(file.path(intermediate_dir, "catch_data_temp.rdata"))
load(file.path(intermediate_dir, "permit_clean.rdata"))

register_keys <- permit_clean %>%
  rename(all_of(c(Vessel.ADFG.Number = "Vessel.ADFG", Batch.Year = "Year"))) %>%
  mutate(
    Vessel.ADFG.Number        = as.integer(Vessel.ADFG.Number),
    CFEC.Permit.Serial.Number = as.integer(substr(Permit.Number, 1, 5))
  ) %>%
  distinct(Vessel.ADFG.Number, Batch.Year, CFEC.Permit.Serial.Number) %>%
  mutate(in.register = TRUE)

tickets <- catch_data_temp %>%
  mutate(
    Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number),
    CFEC.Permit.Serial.Number = as.integer(CFEC.Permit.Serial.Number),
    Fishery            = strip_fishery_space(CFEC.Permit.Fishery),
    permit.fishery.clean = strip_fishery_space(Permit.Fishery),
    revenue            = CFEC.Value..Detail.
  ) %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  left_join(register_keys, by = c("Vessel.ADFG.Number", "Batch.Year", "CFEC.Permit.Serial.Number")) %>%
  mutate(
    in.register  = replace_na(in.register, FALSE),
    pos.revenue  = !is.na(revenue) & revenue > 0,
    no.vessel    = is.na(Vessel.ADFG.Number) | Vessel.ADFG.Number %in% BAD_VESSEL_IDS,
    no.serial    = is.na(CFEC.Permit.Serial.Number),
    no.fishery   = is.na(Fishery) | Fishery == ""
  )

cat("\n===== Ticket rows by year: usability for the held-vs-fished match =====\n")
cat("pct.pos.revenue: rows with positive recorded revenue\n",
    "pct.no.vessel: vessel ID missing or sentinel (0 or 99999)\n",
    "pct.no.serial: permit serial missing\n",
    "pct.no.fishery: CFEC.Permit.Fishery blank\n",
    "pct.in.register: row matches a register row on (vessel, year, permit serial)\n",
    "pct.revenue.in.register: share of year's revenue on rows that match the register\n")
coverage_year <- tickets %>%
  group_by(Batch.Year) %>%
  summarise(
    rows                    = n(),
    pct.pos.revenue         = round(100 * mean(pos.revenue), 1),
    pct.no.vessel           = round(100 * mean(no.vessel), 1),
    pct.no.serial           = round(100 * mean(no.serial), 1),
    pct.no.fishery          = round(100 * mean(no.fishery), 1),
    pct.in.register         = round(100 * mean(in.register), 1),
    revenue.total           = sum(revenue, na.rm = TRUE),
    pct.revenue.in.register = round(100 * sum(revenue[in.register], na.rm = TRUE) /
                                      sum(revenue, na.rm = TRUE), 1),
    distinct.vessels        = n_distinct(Vessel.ADFG.Number[!no.vessel]),
    .groups = "drop"
  )
print(coverage_year, n = Inf, width = Inf)

cat("\n===== Early versus later periods, pooled =====\n")
print(tickets %>%
        mutate(period = case_when(
          Batch.Year <= 1996 ~ "1991-1996",
          Batch.Year <= 2005 ~ "1997-2005",
          TRUE ~ "2006-2021"
        )) %>%
        group_by(period) %>%
        summarise(
          rows                    = n(),
          pct.pos.revenue         = round(100 * mean(pos.revenue), 1),
          pct.no.vessel           = round(100 * mean(no.vessel), 1),
          pct.no.serial           = round(100 * mean(no.serial), 1),
          pct.in.register         = round(100 * mean(in.register), 1),
          pct.revenue.in.register = round(100 * sum(revenue[in.register], na.rm = TRUE) /
                                            sum(revenue, na.rm = TRUE), 1),
          .groups = "drop"
        ),
      width = Inf)

cat("\n===== L12T (herring spawn, Bristol Bay hand pick) by year =====\n")
cat("n.blank.cfec.but.permit.L12T: rows with a blank CFEC.Permit.Fishery whose\n",
    "  Permit.Fishery (the second ticket-side fishery column) is L12T\n")
print(tickets %>%
        filter(Fishery == "L12T" | permit.fishery.clean == "L12T") %>%
        group_by(Batch.Year) %>%
        summarise(
          rows                              = sum(Fishery == "L12T", na.rm = TRUE),
          n.na.vessel                       = sum(is.na(Vessel.ADFG.Number) & Fishery == "L12T", na.rm = TRUE),
          n.sentinel.vessel                 = sum(Vessel.ADFG.Number %in% BAD_VESSEL_IDS & Fishery == "L12T", na.rm = TRUE),
          n.na.serial                       = sum(no.serial & Fishery == "L12T", na.rm = TRUE),
          n.pos.revenue                     = sum(pos.revenue & Fishery == "L12T", na.rm = TRUE),
          n.in.register                     = sum(in.register & Fishery == "L12T", na.rm = TRUE),
          revenue                           = sum(revenue[Fishery == "L12T"], na.rm = TRUE),
          n.blank.cfec.but.permit.L12T      = sum(no.fishery & permit.fishery.clean == "L12T", na.rm = TRUE),
          .groups = "drop"
        ),
      n = Inf, width = Inf)
