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

n_owner_years <- nrow(owner_year)
n_owners      <- n_distinct(owner_year$File.Number)

panel_length <- vessel_summary %>%
  summarise(
    mean.years   = mean(n.years),
    median.years = median(n.years),
    min.years    = min(n.years),
    max.years    = max(n.years)
  )

owner_panel_length <- owner_summary %>%
  summarise(
    mean.years   = mean(n.years),
    median.years = median(n.years)
  )

attrition <- vessel_summary %>%
  summarise(
    n.vessels          = n(),
    n.meets.min.years  = sum(meets.min.years),
    share.meets.min.years = mean(meets.min.years)
  )

owner_attrition <- owner_summary %>%
  summarise(
    n.owners              = n(),
    n.meets.min.years     = sum(meets.min.years),
    share.meets.min.years = mean(meets.min.years)
  )

# Mean/median pairs collapsed into one row each ("mean (median)") and the
# year range moved out of the body into the caption, so the table reports
# scale and distribution rather than repeating the coverage dates the text
# around Table~\ref{tab:ch3-table1} already states. Vessel and owner
# (File.Number, the permit holder, see Section~7 of 01_build_panel.R) sit
# side by side since every row here has a clean owner-level analogue,
# rather than needing Table 3 to see the panel at the owner grain at all.
table1 <- tibble(
  Statistic = c(
    "Panel-years",
    "Distinct units",
    "Fisheries held per unit-year, mean (median)",
    "Fisheries fished per unit-year, mean (median)",
    "Revenue per unit-year, mean (median)",
    "Panel length per unit (in years), mean (median)",
    paste0("Units meeting the ", MIN_ACTIVE_YEARS, "-year minimum (share)")
  ),
  Vessel = c(
    format(n_vessel_years, big.mark = ","),
    format(n_vessels, big.mark = ","),
    sprintf("%.2f (%.2f)",
            mean(vessel_year$n.held.fishery, na.rm = TRUE),
            median(vessel_year$n.held.fishery, na.rm = TRUE)),
    sprintf("%.2f (%.2f)",
            mean(vessel_year$n.fished.fishery, na.rm = TRUE),
            median(vessel_year$n.fished.fishery, na.rm = TRUE)),
    sprintf("$%s ($%s)",
            format(round(mean(vessel_year$vessel.year.rev, na.rm = TRUE)), big.mark = ","),
            format(round(median(vessel_year$vessel.year.rev, na.rm = TRUE)), big.mark = ",")),
    sprintf("%.2f (%.2f)", panel_length$mean.years, panel_length$median.years),
    sprintf("%s (%.0f%%)",
            format(attrition$n.meets.min.years, big.mark = ","),
            attrition$share.meets.min.years * 100)
  ),
  Owner = c(
    format(n_owner_years, big.mark = ","),
    format(n_owners, big.mark = ","),
    sprintf("%.2f (%.2f)",
            mean(owner_year$n.held.fishery, na.rm = TRUE),
            median(owner_year$n.held.fishery, na.rm = TRUE)),
    sprintf("%.2f (%.2f)",
            mean(owner_year$n.fished.fishery, na.rm = TRUE),
            median(owner_year$n.fished.fishery, na.rm = TRUE)),
    sprintf("$%s ($%s)",
            format(round(mean(owner_year$owner.year.rev, na.rm = TRUE)), big.mark = ","),
            format(round(median(owner_year$owner.year.rev, na.rm = TRUE)), big.mark = ",")),
    sprintf("%.2f (%.2f)", owner_panel_length$mean.years, owner_panel_length$median.years),
    sprintf("%s (%.0f%%)",
            format(owner_attrition$n.meets.min.years, big.mark = ","),
            owner_attrition$share.meets.min.years * 100)
  )
)

print(table1)
print(xtable(table1,
             caption = paste0("Panel summary statistics, vessel versus owner, ", year_range[1], "-", year_range[2]),
             label = "tab:ch3-table1"),
      file = file.path(table_dir, "table1_summary_stats.tex"),
      include.rownames = FALSE)

# ============================================================================
# Table 2. Data-quality diagnostics
# ============================================================================
# match_diag was built in 01_build_panel.R. Renamed here for the table only.
# Plain named-vector lookup rather than case_match()/recode(), both of which
# have been through dplyr lifecycle churn, this has no lifecycle to churn.
metric_labels <- c(
  ticket_serial_match_rate            = "Fish tickets whose permit serial matches the CFEC register",
  share_permits_missing_vessel_id     = "Permit register rows with no vessel ID (NA, 0, or 99999)",
  share_revenue_zero_filled           = "Fish ticket rows with CFEC.Value..Detail. filled from NA to 0",
  share_zero_fill_has_positive_pounds = "Of those zero-filled rows, share with positive Pounds..Detail. (real landing, price just missing)"
)

table2 <- match_diag %>%
  mutate(
    label = unname(metric_labels[metric]),
    value = round(value, 4)
  ) %>%
  select(Diagnostic = label, Value = value)

print(table2)
# digits = c(0, 0, 4): first slot is xtable's implicit row-name column
# (irrelevant, rownames are dropped below), second is Diagnostic (character,
# ignored), third is Value. Without this xtable defaults to 2 decimals,
# which is fine for 0.96/0.28/0.87 but flattens share_revenue_zero_filled
# from 0.0073 to a misleading 0.01.
print(xtable(table2, caption = "Data quality diagnostics", label = "tab:ch3-table2", digits = c(0, 0, 4)),
      file = file.path(table_dir, "table2_data_quality.tex"),
      include.rownames = FALSE)

cat("Wrote table1_summary_stats.tex and table2_data_quality.tex to", table_dir, "\n")
