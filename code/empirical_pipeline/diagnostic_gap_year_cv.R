# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone.
#
# Every rev.cv in this pipeline (lifetime, period and rolling, vessel and
# owner) is sd/mean of annual revenue over ACTIVE years only. A year with no
# revenue anywhere is dropped rather than entered as a zero (01_build_panel.R
# Sections 6 and 7, 01b_build_rolling_panel.R Section 6 and its owner twin).
# That is the right call for years before a unit first fishes and after it
# last fishes, those are entry and exit. It is less obviously right for a
# year with no revenue in the MIDDLE of a career. A unit that sits out a year
# and comes back had a genuine zero-income year, and dropping it removes the
# worst outcome from the CV.
#
# The worry is that this is not neutral with respect to diversification. A
# specialist whose one fishery closes or collapses has nowhere else to fish
# and is the unit most likely to produce a zero year, while a diversified
# unit usually has a second fishery to fall back on. If so, dropping gap
# years understates CV most for the most concentrated units and biases the
# CV-on-HHI slope toward zero.
#
# This script rebuilds rev.cv two more ways, holding every regression sample
# fixed, and refits Table 4 on each.
#   rev.cv             the pipeline's own, active years only (rebuilt here
#                      and checked against the saved value before anything
#                      else runs)
#   rev.cv.gap0.short  interior gap years count as zero revenue if they sit
#                      in a run of at most MAX_GAP_RUN consecutive gap years,
#                      longer runs stay missing (read as exit and re-entry)
#   rev.cv.gap0.all    every interior gap year counts as zero revenue
# An interior gap year is a year strictly between a unit's first and last
# active year (over its whole panel) with no positive revenue. Years before
# first entry and after last exit stay missing in every version. In the
# rolling design a window's gap years are the career-interior gap years that
# fall inside that window. Eligibility (4 of 6 ACTIVE years) is unchanged, so
# the vessel-window and owner-window samples are identical across versions
# and only the CV moves.
#
# The owner grain is the cleaner income measure here. A vessel with a gap
# year may belong to an owner who fished a different vessel that year, which
# is a zero for the hull but not for the business. Read the vessel and owner
# results side by side for that reason.
#
# Reports, at four grains (vessel and owner, lifetime and rolling),
#   1. how long gap runs are, as a share of runs and of gap years
#   2. the gap-year rate by calendar year. A spike in one year looks more
#      like a ticket coverage problem than behavior, follow that up with
#      diagnostic_early_ticket_coverage.R. The rate falls mechanically
#      toward zero at both ends of the panel, since an interior gap needs an
#      active year on each side.
#   3. what share of gap years the unit still held a permit in. A held but
#      idle year is a stronger case for a real zero than a year with no
#      record at all.
#   4. gap prevalence and the CV change by specialist status and, among
#      multi-fishery units, by H_bar quintile, the direct test of the bias
#      story above
#   5. Table 4's baseline slope, decomposed coefficients and standardized
#      g2 share under each CV version, for the main-text (multi-fishery) and
#      pooled samples, with the same fixed effects and standard errors as
#      05_table4_figure3.R, 05_table4_figure3_owner.R and their 05b_ twins
#
# Reads intermediate data/ch3_panel.rdata, intermediate data/ch3_rolling.rdata
# and intermediate data/ch3_rolling_owner.rdata. Writes four CSVs to
# Chpt3/output/tables/ (diagnostic_gap_year_*.csv) and nothing else.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

load(panel_path)
load(file.path(intermediate_dir, "ch3_rolling.rdata"))
load(file.path(intermediate_dir, "ch3_rolling_owner.rdata"))

# A gap run longer than this is read as exit and re-entry rather than a year
# sat out. A judgment call, not a fact. CHECK how much of the gap-year mass
# sits above it (Section 3 prints the run-length distribution) before
# leaning on rev.cv.gap0.short over rev.cv.gap0.all.
MAX_GAP_RUN <- 2

CV_VERSIONS <- c("rev.cv", "rev.cv.gap0.short", "rev.cv.gap0.all")

# ============================================================================
# 1. Helpers
# ============================================================================

# One row per unit per year from its first to its last active year. year.rev
# is the unit's annual revenue in active years and 0 in gap years. gap.run.id
# numbers the runs of consecutive same-status years within a unit, so a gap
# run is identified by (unit, gap.run.id) among the is.gap rows.
# held.in.gap flags a gap year where the unit still held at least one permit.
build_gap_panel <- function(unit_year, unit_col, rev_col, max_gap_run) {
  dt <- as.data.table(unit_year)[, .(
    unit           = get(unit_col),
    Batch.Year     = as.integer(Batch.Year),
    year.rev       = get(rev_col),
    n.held.fishery = n.held.fishery
  )]
  active <- dt[year.rev > 0]
  span   <- active[, .(first.year = min(Batch.Year), last.year = max(Batch.Year)), by = unit]
  full   <- span[, .(Batch.Year = first.year:last.year), by = unit]
  full   <- merge(full, active[, .(unit, Batch.Year, year.rev)], by = c("unit", "Batch.Year"), all.x = TRUE)
  full   <- merge(full, dt[, .(unit, Batch.Year, n.held.fishery)], by = c("unit", "Batch.Year"), all.x = TRUE)

  full[, is.gap := is.na(year.rev)]
  full[is.gap == TRUE, year.rev := 0]
  setorder(full, unit, Batch.Year)
  full[, gap.run.id := rleid(is.gap), by = unit]
  full[, gap.run.length := fifelse(is.gap, .N, 0L), by = .(unit, gap.run.id)]
  full[, gap.short := is.gap & gap.run.length <= max_gap_run]
  full[, held.in.gap := is.gap & !is.na(n.held.fishery) & n.held.fishery > 0]
  setnames(full, "unit", unit_col)
  full[]
}

# The three CV versions plus gap counts, grouped by `by_cols` (the unit for
# the lifetime grain, the unit and window.start for the rolling grain).
cv_by_group <- function(panel, by_cols) {
  panel[, .(
    n.active.years    = sum(!is.gap),
    n.gap.years       = sum(is.gap),
    n.gap.years.short = sum(gap.short),
    n.gap.years.held  = sum(held.in.gap),
    rev.cv.rebuilt    = sd(year.rev[!is.gap]) / mean(year.rev[!is.gap]),
    rev.cv.gap0.short = sd(year.rev[!is.gap | gap.short]) / mean(year.rev[!is.gap | gap.short]),
    rev.cv.gap0.all   = sd(year.rev) / mean(year.rev)
  ), by = by_cols]
}

# Maps the gap panel onto the rolling window grid and keeps only the
# (unit, window) pairs in `analysis_keys`, so the aggregation never touches
# a window that is not in a regression sample.
cv_by_window <- function(panel, unit_col, analysis_keys) {
  windowed <- as.data.table(roll_expand_to_windows(panel, "Batch.Year", window_grid.rolling))
  keys <- unique(as.data.table(analysis_keys)[, c(unit_col, "window.start"), with = FALSE])
  keys[, window.start := as.integer(window.start)]
  windowed <- windowed[keys, on = c(unit_col, "window.start"), nomatch = NULL]
  cv_by_group(windowed, c(unit_col, "window.start"))
}

# Joins the rebuilt CVs onto an analysis sample and stops if a row went
# missing or the rebuilt active-years CV does not reproduce the saved
# rev.cv, either would mean this script is not measuring the same thing as
# the pipeline it is checking.
attach_cv_versions <- function(analysis, cv_table, by_cols, label) {
  out <- analysis %>% inner_join(as_tibble(cv_table), by = by_cols)
  if (nrow(out) != nrow(analysis)) {
    stop(label, " - lost ", nrow(analysis) - nrow(out), " rows joining the rebuilt CVs")
  }
  max_diff <- max(abs(out$rev.cv.rebuilt - out$rev.cv))
  cat(label, "- max abs difference between rebuilt and saved rev.cv =", signif(max_diff, 3), "\n")
  if (!is.finite(max_diff) || max_diff > 1e-8) {
    stop(label, " - rebuilt active-years CV does not match the saved rev.cv")
  }
  out
}

gap_run_lengths <- function(panel, unit_col, units, label) {
  runs <- panel[is.gap == TRUE & get(unit_col) %in% units,
                .(run.length = gap.run.length[1]), by = c(unit_col, "gap.run.id")]
  runs[, bucket := fifelse(run.length >= 5, "5+", as.character(run.length))]
  out <- runs[, .(n.runs = .N, n.gap.years = sum(run.length)), by = bucket][order(bucket)]
  out[, `:=`(
    share.runs      = n.runs / sum(n.runs),
    share.gap.years = n.gap.years / sum(n.gap.years),
    counted.as.zero.in.short = bucket %in% as.character(seq_len(MAX_GAP_RUN)),
    grain           = label
  )]
  as_tibble(out)
}

# Fleet-wide, every unit in the gap panel, not only regression samples,
# since a coverage problem would show up fleet-wide. flag.spike marks years
# whose gap rate is more than twice the median year's.
gap_rate_by_year <- function(panel, label) {
  out <- panel[, .(
    n.units.in.span = .N,
    n.gap           = sum(is.gap),
    n.gap.held      = sum(held.in.gap)
  ), by = Batch.Year][order(Batch.Year)]
  out[, `:=`(
    gap.rate          = n.gap / n.units.in.span,
    held.share.of.gap = fifelse(n.gap > 0, n.gap.held / n.gap, NA_real_)
  )]
  out[, flag.spike := gap.rate > 2 * median(gap.rate)]
  out[, grain := label]
  as_tibble(out)
}

summarise_gap_group <- function(df) {
  df %>%
    summarise(
      n                    = n(),
      share.any.gap        = mean(n.gap.years > 0),
      share.any.gap.short  = mean(n.gap.years.short > 0),
      mean.gap.years       = mean(n.gap.years),
      held.share.of.gap    = if_else(sum(n.gap.years) > 0,
                                     sum(n.gap.years.held) / sum(n.gap.years), NA_real_),
      mean.rev.cv          = mean(rev.cv),
      mean.rev.cv.gap0.short = mean(rev.cv.gap0.short),
      mean.rev.cv.gap0.all = mean(rev.cv.gap0.all),
      mean.change.short    = mean(rev.cv.gap0.short - rev.cv),
      mean.change.all      = mean(rev.cv.gap0.all - rev.cv),
      .groups = "drop"
    )
}

# Specialists as one row, multi-fishery units split into H_bar quintiles
# (quintiles are taken among multi-fishery units only, specialists all sit
# at H_bar = 1 and would swamp the top bin), plus all-multi and all rows.
gap_by_group <- function(analysis, label) {
  tagged <- analysis %>%
    mutate(
      hbar.q = ntile(if_else(is.specialist, NA_real_, H_bar), 5),
      group  = if_else(is.specialist, "Specialist", paste0("Multi-fishery, H_bar Q", hbar.q))
    )
  bind_rows(
    tagged %>% group_by(group) %>% summarise_gap_group(),
    tagged %>% filter(!is.specialist) %>% summarise_gap_group() %>% mutate(group = "All multi-fishery"),
    tagged %>% summarise_gap_group() %>% mutate(group = "All")
  ) %>%
    mutate(grain = label) %>%
    relocate(grain, group)
}

# Table 4's baseline, decomposed and standardized decomposed models, once
# per CV version, on the same rows. g2.share matches 05_/05b_'s own
# definition, z.Phi / (z.H_LR + z.Phi).
refit_table4 <- function(data, fe, vcov_spec, grain, sample_label) {
  bind_rows(lapply(CV_VERSIONS, function(cv) {
    m_base <- feols(as.formula(paste(cv, "~ H_bar |", fe)), data = data, vcov = vcov_spec)
    m_dec  <- feols(as.formula(paste(cv, "~ H_LR + Phi |", fe)), data = data, vcov = vcov_spec)
    data_z <- data %>%
      mutate(
        z.cv   = as.numeric(scale(.data[[cv]])),
        z.H_LR = as.numeric(scale(H_LR)),
        z.Phi  = as.numeric(scale(Phi))
      )
    m_z <- feols(as.formula(paste("z.cv ~ z.H_LR + z.Phi |", fe)), data = data_z, vcov = vcov_spec)
    tibble(
      grain      = grain,
      sample     = sample_label,
      cv.version = cv,
      n          = nobs(m_dec),
      b1         = unname(coef(m_base)["H_bar"]), se.b1 = unname(se(m_base)["H_bar"]),
      g1         = unname(coef(m_dec)["H_LR"]),   se.g1 = unname(se(m_dec)["H_LR"]),
      g2         = unname(coef(m_dec)["Phi"]),    se.g2 = unname(se(m_dec)["Phi"]),
      z.g1       = unname(coef(m_z)["z.H_LR"]),
      z.g2       = unname(coef(m_z)["z.Phi"])
    ) %>%
      mutate(g2.share = z.g2 / (z.g1 + z.g2))
  }))
}

# ============================================================================
# 2. Gap panels and CV versions
# ============================================================================

gap_vessel <- build_gap_panel(vessel_year, "Vessel.ADFG.Number", "vessel.year.rev", MAX_GAP_RUN)
gap_owner  <- build_gap_panel(owner_year, "File.Number", "owner.year.rev", MAX_GAP_RUN)

cat("Vessel gap panel -", nrow(gap_vessel), "vessel-years in span,", sum(gap_vessel$is.gap),
    "interior gap years\n")
cat("Owner gap panel -", nrow(gap_owner), "owner-years in span,", sum(gap_owner$is.gap),
    "interior gap years\n")

# Lifetime samples, identical filters to 05_table4_figure3.R and
# 05_table4_figure3_owner.R.
n_fish_vessel <- vessel_mean_share %>% count(Vessel.ADFG.Number, name = "n.fisheries.fished")
n_fish_owner  <- owner_mean_share %>% count(File.Number, name = "n.fisheries.fished")

life_vessel <- vessel_summary %>%
  filter(meets.min.years, is.finite(rev.cv), !is.na(prime.fishery)) %>%
  left_join(n_fish_vessel, by = "Vessel.ADFG.Number") %>%
  mutate(is.specialist = n.fisheries.fished == 1) %>%
  attach_cv_versions(cv_by_group(gap_vessel, "Vessel.ADFG.Number"),
                     "Vessel.ADFG.Number", "Vessel, lifetime")

life_owner <- owner_summary %>%
  filter(meets.min.years, is.finite(rev.cv), !is.na(prime.fishery)) %>%
  left_join(n_fish_owner, by = "File.Number") %>%
  mutate(is.specialist = n.fisheries.fished == 1) %>%
  attach_cv_versions(cv_by_group(gap_owner, "File.Number"),
                     "File.Number", "Owner, lifetime")

# Rolling samples, identical filters to 05b_table4_figure3_rolling.R and
# 05b_table4_figure3_rolling_owner.R.
roll_vessel_base <- vessel_window_summary.rolling %>%
  filter(is.finite(rev.cv)) %>%
  mutate(is.specialist = is.specialist.window)
roll_vessel <- roll_vessel_base %>%
  attach_cv_versions(cv_by_window(gap_vessel, "Vessel.ADFG.Number", roll_vessel_base),
                     c("Vessel.ADFG.Number", "window.start"), "Vessel, rolling")

roll_owner_base <- owner_window_summary.rolling %>%
  filter(is.finite(rev.cv)) %>%
  mutate(is.specialist = is.specialist.window)
roll_owner <- roll_owner_base %>%
  attach_cv_versions(cv_by_window(gap_owner, "File.Number", roll_owner_base),
                     c("File.Number", "window.start"), "Owner, rolling")

# ============================================================================
# 3. Gap run lengths (lifetime samples)
# ============================================================================

run_lengths <- bind_rows(
  gap_run_lengths(gap_vessel, "Vessel.ADFG.Number", life_vessel$Vessel.ADFG.Number, "Vessel, lifetime"),
  gap_run_lengths(gap_owner, "File.Number", life_owner$File.Number, "Owner, lifetime")
)

cat("\n===== Gap run lengths, lifetime samples (MAX_GAP_RUN =", MAX_GAP_RUN, ") =====\n")
print(run_lengths, n = Inf)

# ============================================================================
# 4. Gap-year rate by calendar year (fleet-wide)
# ============================================================================

by_year <- bind_rows(
  gap_rate_by_year(gap_vessel, "Vessel"),
  gap_rate_by_year(gap_owner, "Owner")
)

cat("\n===== Interior gap-year rate by calendar year, fleet-wide =====\n")
print(by_year, n = Inf)
cat("Years flagged as spikes (gap rate above twice the median year) -",
    paste(unique(by_year$Batch.Year[by_year$flag.spike]), collapse = ", "), "\n")

# ============================================================================
# 5. Prevalence and CV change by specialist status and H_bar quintile
# ============================================================================

by_group <- bind_rows(
  gap_by_group(life_vessel, "Vessel, lifetime"),
  gap_by_group(roll_vessel, "Vessel, rolling"),
  gap_by_group(life_owner, "Owner, lifetime"),
  gap_by_group(roll_owner, "Owner, rolling")
)

cat("\n===== Gap prevalence and CV change, by specialist status and H_bar quintile =====\n")
print(by_group, n = Inf, width = Inf)

# ============================================================================
# 6. Table 4 under each CV version
# ============================================================================

table4_versions <- bind_rows(
  refit_table4(life_vessel %>% filter(!is.specialist), "prime.fishery", "hetero",
               "Vessel, lifetime", "Multi-fishery"),
  refit_table4(life_vessel, "prime.fishery", "hetero",
               "Vessel, lifetime", "Pooled"),
  refit_table4(roll_vessel %>% filter(!is.specialist), "prime.fishery.window + window.start",
               ~Vessel.ADFG.Number + window.start, "Vessel, rolling", "Multi-fishery"),
  refit_table4(roll_vessel, "prime.fishery.window + window.start",
               ~Vessel.ADFG.Number + window.start, "Vessel, rolling", "Pooled"),
  refit_table4(life_owner %>% filter(!is.specialist), "prime.fishery", "hetero",
               "Owner, lifetime", "Multi-fishery"),
  refit_table4(life_owner, "prime.fishery", "hetero",
               "Owner, lifetime", "Pooled"),
  refit_table4(roll_owner %>% filter(!is.specialist), "prime.fishery.window + window.start",
               ~File.Number + window.start, "Owner, rolling", "Multi-fishery"),
  refit_table4(roll_owner, "prime.fishery.window + window.start",
               ~File.Number + window.start, "Owner, rolling", "Pooled")
)

# The rev.cv rows should reproduce the published Table 4 columns exactly,
# compare them against table4_decomposition_regression*.tex before reading
# the other two versions.
cat("\n===== Table 4 under each CV version =====\n")
print(
  table4_versions %>% mutate(across(where(is.double), ~ round(.x, 4))),
  n = Inf, width = Inf
)

# ============================================================================
# 7. Write
# ============================================================================

write.csv(run_lengths, file.path(table_dir, "diagnostic_gap_year_run_lengths.csv"), row.names = FALSE)
write.csv(by_year, file.path(table_dir, "diagnostic_gap_year_by_calendar_year.csv"), row.names = FALSE)
write.csv(by_group, file.path(table_dir, "diagnostic_gap_year_by_group.csv"), row.names = FALSE)
write.csv(table4_versions, file.path(table_dir, "diagnostic_gap_year_table4.csv"), row.names = FALSE)

cat("\nWrote diagnostic_gap_year_run_lengths.csv, diagnostic_gap_year_by_calendar_year.csv,",
    "diagnostic_gap_year_by_group.csv and diagnostic_gap_year_table4.csv to", table_dir, "\n")
