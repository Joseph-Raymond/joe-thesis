# Chapter 3 empirical pipeline, rolling-window (vessel-period) analysis
#
# Shared constants and functions for the rolling-window parallel analysis,
# see rolling_periods_design.md for the full design. This file mirrors
# 00_setup.R's role for the baseline pipeline, EXCEPT it has no side effects
# of its own, no rm(list = ls()), no load(), no setwd(). It only defines
# constants and functions.
#
# Sourced by every rolling script AFTER that script's own
# source("code/empirical_pipeline/00_setup.R") call, never before, since
# 00_setup.R's rm(list = ls()) would otherwise wipe everything this file
# defines. Every rolling script therefore starts
#
#   source("code/empirical_pipeline/00_setup.R")
#   source("code/empirical_pipeline/00b_rolling_periods.R")
#
# Nothing here reassigns any baseline object, and this file never defines a
# function named period_of (01_build_panel.R and 06_within_season_reallocation.R
# each define their own period_of(), a live example of exactly the naming
# collision this whole convention exists to avoid, see design Section 8.3).

# ============================================================================
# 1. Constants
# ============================================================================

# Width of a rolling window in years, author-specified ("rolling 6 year
# periods"). Every window is [window.start, window.start + ROLL_WINDOW_WIDTH - 1].
ROLL_WINDOW_WIDTH <- 6

# A vessel needs at least this many ACTIVE years (vessel.year.rev > 0, not
# calendar years) inside a 6-year window to be eligible in that window,
# author-specified ("4 of the 6"). This is a count, not a contiguity
# requirement, a vessel active in years 1, 2, 5, 6 of a window is eligible
# on the same footing as one active in years 1-4. See design Section 1.3 and
# trap #1, this is built from active_vessel_years/vessel_share_panel
# directly and meets.min.years must never be applied anywhere downstream of
# this file.
ROLL_MIN_ACTIVE_YEARS <- 4

# A vessel-window needs at least this many years with a computable
# per-transition switching measure d_it (i.e., at least 2 active weeks that
# year) for tau.window to be defined. The 4-of-6 activity floor above does
# NOT guarantee this, a year with a single active week has no consecutive-week
# transition to difference, see design Section 3.5.
ROLL_MIN_TAU_YEARS <- 2

# Minimum ACTIVE years required in the preceding (classifier/lookback)
# window for Table 8-rolling's tau.pre and 08b_'s predetermined.primary.window.
# Deliberately lower than ROLL_MIN_ACTIVE_YEARS (3 rather than 4), stacking a
# 4-of-6 outcome floor with a 4-of-6 lookback floor would shrink the sample
# twice over for no separately-justified reason, see design Section 5.3.
ROLL_MIN_LOOKBACK_YEARS <- 3

# Number of phases for the stride-6 non-overlapping robustness check
# (design Section 2.2, Layer 3). Equal to ROLL_WINDOW_WIDTH by construction,
# a window and the next non-overlapping window in the same phase are exactly
# ROLL_N_PHASES years apart.
ROLL_N_PHASES <- ROLL_WINDOW_WIDTH

# Where the rolling scripts persist the accumulating overlap-robustness
# ledger (design Section 2.2's table_rolling_overlap_robustness.tex). Every
# script that calls roll_phase_check() on a headline model loads this file
# if present, appends its own rows, and re-saves, since 00_setup.R's
# rm(list = ls()) means nothing survives in memory between scripts (design
# Section 8.1). A path string, not a load, so defining it here carries no
# side effect.
ROLL_PHASE_CHECK_PATH <- file.path("intermediate data", "ch3_rolling_phase_check.rdata")

# ============================================================================
# 2. Window grid and eligibility
# ============================================================================

# Common calendar grid, stride 1, width ROLL_WINDOW_WIDTH. Every vessel's
# "1997-2002" is the same six calendar years, this is what makes window.start
# usable as a fixed effect and a clustering dimension (design Section 1.2).
# Trailing partial windows are dropped, a window must have all
# ROLL_WINDOW_WIDTH calendar years inside [min_year, max_year].
roll_window_grid <- function(min_year, max_year, width = ROLL_WINDOW_WIDTH) {
  tibble(window.start = min_year:(max_year - width + 1)) %>%
    mutate(
      window.end   = window.start + width - 1,
      window.label = paste0(window.start, "-", window.end)
    )
}

# Expands one row per (df row, candidate window.start) for every window in
# `grid` that could contain df[[year_col]], i.e. window.start in
# (year - width + 1) .. year, clipped to the grid's own [min(window.start),
# max(window.start)] range. Deliberately does NOT cross-join the full grid
# (design Section 1.3, that materializes width-times-more rows than needed
# before filtering), instead expands each row into its own `width` candidate
# starts directly. data.table row-repetition (dt[idx]) rather than
# tidyr::complete/crossing, per trap #15's performance note, this runs
# against a ~300,000-vessel-year panel on the server.
#
# Generic in `year_col` so this is reusable wherever a (unit, year) table
# needs mapping onto the window grid, not just active_vessel_years.
roll_expand_to_windows <- function(df, year_col, grid) {
  width <- unique(grid$window.end - grid$window.start + 1)
  stopifnot(length(width) == 1)
  min_start <- min(grid$window.start)
  max_start <- max(grid$window.start)

  dt      <- as.data.table(df)
  years   <- dt[[year_col]]
  n       <- nrow(dt)
  offsets <- 0:(width - 1)

  idx <- rep(seq_len(n), each = width)
  out <- dt[idx]
  out[, window.start := rep(years, each = width) - rep(offsets, times = n)]
  out <- out[window.start >= min_start & window.start <= max_start]
  out[, window.end := window.start + width - 1L]
  as_tibble(out)
}

# The single canonical eligibility rule (design Section 1.3 and trap #1).
# Intended to be called ONCE, in 01b_build_rolling_panel.R, to produce
# vessel_window_eligibility.rolling, which every downstream rolling script
# then semi_joins against rather than recomputing its own eligibility filter,
# so there is exactly one sample definition for "this vessel is eligible in
# this window" across the whole rolling pipeline.
#
# active_vessel_years must be built from vessel_year's own
# vessel.year.rev > 0 gate (never from a filtered vessel_summary), see
# design Section 7.2, this is the most likely implementation error in the
# whole document.
roll_eligibility <- function(active_vessel_years, grid, min_active) {
  expanded <- roll_expand_to_windows(active_vessel_years, "Batch.Year", grid)
  dt   <- as.data.table(expanded)
  elig <- dt[, .(n.years.window = .N), by = .(Vessel.ADFG.Number, window.start)]
  elig <- elig[n.years.window >= min_active]
  as_tibble(elig)
}

# ============================================================================
# 3. Phase (stride-6 non-overlapping subset) helpers
# ============================================================================

# Phase index 0..(n_phases - 1) for a given window.start. Windows sharing a
# phase are non-overlapping in their underlying vessel-years by construction
# (e.g. phase 0 with min_year = 1991 is 1991-1996, 1997-2002, 2003-2008, ...).
roll_phase <- function(window_start, min_year, n_phases = ROLL_N_PHASES) {
  (window_start - min_year) %% n_phases
}

# The mandatory three-layer-inference diagnostic (design Section 2.2, Layer
# 3, and the reading rule in Section 2.3). Fits `fml` once on the full
# rolling sample with two-way clustering (falling back to vessel-only
# clustering if the two-way variance matrix cannot be inverted, which can
# happen with only 20-26 window.start clusters, design Section 2.2), then
# refits the IDENTICAL specification separately within each of the
# ROLL_N_PHASES non-overlapping phases, clustering on vessel only within a
# phase (a long-tenured vessel still appears in several non-overlapping
# windows of the same phase, so vessel clustering is still required there).
#
# `coef_name` is the single coefficient this call is tracking, call this
# function once per coefficient of interest for a model with more than one
# (e.g. a decomposed or interaction model), the returned $summary row is
# keyed on (model, coefficient) so multiple calls for the same model do not
# collide.
#
# Reading rule (verbatim from design Section 2.3, give this to the reader
# alongside the printed output): if the six phases were independent samples
# of size N/6, pooling would give SE_full ~ SE_phase / sqrt(6) ~ 0.41 *
# SE_phase. Full redundancy would give SE_full ~ SE_phase. The vessel-
# clustered two-way SE_full should land much closer to SE_phase than to
# SE_phase / sqrt(6) if the clustering is doing its job. If SE_full comes
# back near SE_phase / sqrt(6), the rolling panel is manufacturing precision
# and the phase standard errors are the ones to quote in the writeup.
# Caveat, the phases share most of the same vessels, so they are not
# independent even in the vessel dimension, this is a calibration rather
# than a formal test.
#
# If the full-panel point estimate falls outside the min-max range of the
# six phase estimates, this function prints a loud warning rather than
# halting execution (a diagnostic that stops an unattended server run would
# block every other headline model's table from being built too, out of
# step with this pipeline's house style of printing every judgment call
# rather than erroring on it), so the caller/reader must check
# flag.outside.phase.range in the returned summary before trusting that
# model, which is exactly the point of the printed warning.
#
# The broad tryCatch(warning = ...) below deliberately treats ANY warning
# from the full two-way-clustered fit as grounds to fall back to vessel-only
# clustering, not just a non-positive-definite-matrix warning specifically,
# since fixest does not raise a distinctly typed condition for that failure
# mode and a silently-broken two-way SE is a worse outcome than an
# unnecessarily conservative fallback.
roll_phase_check <- function(fml, data, coef_name, label,
                              cluster = ~Vessel.ADFG.Number + window.start,
                              min_year = MIN_YEAR, n_phases = ROLL_N_PHASES,
                              ...) {

  used_twoway <- TRUE
  m_full <- tryCatch(
    feols(fml, data = data, cluster = cluster, ...),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  needs_fallback <- is.null(m_full) ||
    !(coef_name %in% names(coef(m_full))) ||
    !is.finite(se(m_full)[coef_name])

  if (needs_fallback) {
    used_twoway <- FALSE
    m_full <- feols(fml, data = data, cluster = ~Vessel.ADFG.Number, ...)
  }

  est_full <- unname(coef(m_full)[coef_name])
  se_full  <- unname(se(m_full)[coef_name])

  phase_list <- lapply(0:(n_phases - 1), function(p) {
    data_p <- data %>% filter(roll_phase(window.start, min_year, n_phases) == p)
    m_p <- tryCatch(
      feols(fml, data = data_p, cluster = ~Vessel.ADFG.Number, ...),
      error = function(e) NULL
    )
    if (is.null(m_p) || !(coef_name %in% names(coef(m_p)))) {
      return(tibble(phase = p, n.obs = nrow(data_p), estimate = NA_real_, se = NA_real_))
    }
    tibble(phase = p, n.obs = nrow(data_p),
           estimate = unname(coef(m_p)[coef_name]), se = unname(se(m_p)[coef_name]))
  })
  phase_tbl <- bind_rows(phase_list)

  se_phase_median <- median(phase_tbl$se, na.rm = TRUE)
  se_ratio        <- se_full / se_phase_median
  phase_min       <- min(phase_tbl$estimate, na.rm = TRUE)
  phase_median    <- median(phase_tbl$estimate, na.rm = TRUE)
  phase_max       <- max(phase_tbl$estimate, na.rm = TRUE)
  out_of_range    <- is.finite(est_full) && is.finite(phase_min) && is.finite(phase_max) &&
    (est_full < phase_min || est_full > phase_max)

  cat("\n--- roll_phase_check:", label, "( coefficient:", coef_name, ") ---\n")
  cat("  full-panel estimate:", round(est_full, 4),
      " SE (", if (used_twoway) "two-way vessel+window" else "vessel-only, two-way clustering failed/degenerate",
      "):", round(se_full, 4), "\n")
  cat("  phase estimates, min:", round(phase_min, 4), " median:", round(phase_median, 4),
      " max:", round(phase_max, 4), "\n")
  cat("  median phase SE:", round(se_phase_median, 4),
      " SE_full / SE_phase =", round(se_ratio, 3),
      " (~1.0 = full redundancy, ~0.41 = independent-sample pooling, per the Section 2.3 reading rule)\n")
  if (out_of_range) {
    cat("  *** WARNING:", label, "(", coef_name, ") full-panel point estimate",
        round(est_full, 4), "falls OUTSIDE the phase min-max range [", round(phase_min, 4), ",",
        round(phase_max, 4), "]. This is a signal something may be wrong with the rolling",
        "construction for this model, inspect before trusting it. ***\n")
  }

  list(
    summary = tibble(
      model = label, coefficient = coef_name,
      estimate.full = est_full, se.full = se_full, used.twoway.cluster = used_twoway,
      phase.min = phase_min, phase.median = phase_median, phase.max = phase_max,
      se.phase.median = se_phase_median, se.ratio = se_ratio,
      flag.outside.phase.range = out_of_range
    ),
    phases = phase_tbl
  )
}

# ============================================================================
# 4. Attrition ledger helper
# ============================================================================

# Appends one row (both N and the distinct vessel count, design Section 7.5
# and trap #12) to an attrition ledger tibble. `df` is whatever survives a
# given filtering step, `vessel_col` defaults to Vessel.ADFG.Number since
# every rolling object in this pipeline is keyed at least partly on it.
roll_attrition_row <- function(ledger, label, df, vessel_col = "Vessel.ADFG.Number") {
  bind_rows(
    ledger,
    tibble(step = label, n.obs = nrow(df), n.vessels = n_distinct(df[[vessel_col]]))
  )
}

cat("00b_rolling_periods.R loaded. ROLL_WINDOW_WIDTH =", ROLL_WINDOW_WIDTH,
    ", ROLL_MIN_ACTIVE_YEARS =", ROLL_MIN_ACTIVE_YEARS,
    ", ROLL_N_PHASES =", ROLL_N_PHASES, "\n")
