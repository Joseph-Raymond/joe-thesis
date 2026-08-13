# Chapter 3 empirical pipeline, Chapter3_outline.md Section 2
#
# Table 1. Summary statistics, scale and time coverage of the panel.
# Table 2. Data-quality diagnostics (match rate, missing vessel ID, zero-fill).
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_year")) load(panel_path)

# ============================================================================
# Table 1. Summary statistics
# ============================================================================

n_vessel_years <- nrow(vessel_year)
n_vessels      <- n_distinct(vessel_year$Vessel.ADFG.Number)
year_range     <- range(vessel_year$Batch.Year)

panel_length <- vessel_summary %>%
  summarise(
    mean.years   = mean(n.years),
    median.years = median(n.years),
    min.years    = min(n.years),
    max.years    = max(n.years)
  )

attrition <- vessel_summary %>%
  summarise(
    n.vessels          = n(),
    n.meets.min.years  = sum(meets.min.years),
    share.meets.min.years = mean(meets.min.years)
  )

table1 <- tibble(
  Statistic = c(
    "Vessel-years",
    "Distinct vessels",
    "First year",
    "Last year",
    "Fisheries held per vessel-year, mean",
    "Fisheries held per vessel-year, median",
    "Fisheries fished per vessel-year, mean",
    "Fisheries fished per vessel-year, median",
    "Vessel-year revenue, mean",
    "Vessel-year revenue, median",
    "Panel length per vessel (years), mean",
    "Panel length per vessel (years), median",
    paste0("Vessels meeting the ", MIN_ACTIVE_YEARS, "-year minimum"),
    paste0("Share of vessels meeting the ", MIN_ACTIVE_YEARS, "-year minimum")
  ),
  Value = c(
    n_vessel_years,
    n_vessels,
    year_range[1],
    year_range[2],
    round(mean(vessel_year$n.held.fishery, na.rm = TRUE), 2),
    median(vessel_year$n.held.fishery, na.rm = TRUE),
    round(mean(vessel_year$n.fished.fishery, na.rm = TRUE), 2),
    median(vessel_year$n.fished.fishery, na.rm = TRUE),
    round(mean(vessel_year$vessel.year.rev, na.rm = TRUE), 0),
    round(median(vessel_year$vessel.year.rev, na.rm = TRUE), 0),
    round(panel_length$mean.years, 2),
    panel_length$median.years,
    attrition$n.meets.min.years,
    round(attrition$share.meets.min.years, 3)
  )
)

print(table1)
print(xtable(table1, caption = "Panel summary statistics", label = "tab:ch3-table1"),
      file = file.path(table_dir, "table1_summary_stats.tex"),
      include.rownames = FALSE)

# ============================================================================
# Table 2. Data-quality diagnostics
# ============================================================================
# match_diag was built in 01_build_panel.R. Renamed here for the table only.

table2 <- match_diag %>%
  mutate(
    label = case_match(metric,
      "ticket_serial_match_rate"        ~ "Fish tickets whose permit serial matches the CFEC register",
      "share_permits_missing_vessel_id" ~ "Permit register rows with no vessel ID (NA, 0, or 99999)",
      "share_revenue_zero_filled"       ~ "Fish ticket rows with CFEC.Value..Detail. filled from NA to 0"
    ),
    value = round(value, 4)
  ) %>%
  select(Diagnostic = label, Value = value)

print(table2)
print(xtable(table2, caption = "Data quality diagnostics", label = "tab:ch3-table2"),
      file = file.path(table_dir, "table2_data_quality.tex"),
      include.rownames = FALSE)

cat("Wrote table1_summary_stats.tex and table2_data_quality.tex to", table_dir, "\n")
