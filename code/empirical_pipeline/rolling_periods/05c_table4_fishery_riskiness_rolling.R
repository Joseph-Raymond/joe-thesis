# Chapter 3 empirical pipeline, Table 4-rolling with fishery riskiness as a
# control, vessel and owner grain
#
# THE QUESTION. Table 4-rolling finds that the CV-on-HHI relationship runs
# mostly through Phi. The CV breakdown (14_cv_decomposition*.R and
# writeup/cv_decomposition_note.tex) shows that high-Phi units are also in
# fisheries that are riskier for everyone, and that fishery riskiness
# accounts for about half of the rise in log CV across the Phi bins. Table
# 4's primary-fishery fixed effect only absorbs the main fishery, and only
# as a level that does not change across windows. So part of the Phi
# coefficient could reflect being in riskier fisheries rather than
# reallocation itself. This script adds fishery riskiness as a control.
#
# THE CONTROL. For unit i in window s, with T_is its active years in the
# window,
#
#   U_fleet_is = sum_j omega_ij * CV(f_j | T_is)
#
# where omega_ij is fishery j's share of the unit's mean revenue over the
# weight years and f_jt is the fleet's mean revenue per active unit in j in
# year t (leaving the unit out in years it fished j, the full mean in the
# rare year it was the only participant, and 0 in a closure year). This is
# exactly the fishery riskiness factor of the CV breakdown, and it is
# checked against the saved 14_ output when that output exists.
#
# WHY TWO VERSIONS. U_fleet is built from the same window as CV and Phi, so a
# unit that moves into a volatile fishery raises its own U_fleet. It is then
# partly an outcome of reallocation (a mediator), and any drop in the Phi
# coefficient is an upper bound on how much of Phi is riskier fisheries. The
# second version keeps the fleet CVs from the current window but takes the
# weights from the unit's preceding 6 years ([s - 6, s - 1], the same
# lookback 01b_ uses, with at least ROLL_MIN_LOOKBACK_YEARS active years).
# Those weights are fixed before the window starts, so this version does not
# move when the unit reallocates inside the window. If the Phi coefficient
# falls with the current-weights version but much less with the
# prior-weights one, the drop reflects moving into risky fisheries, which is
# still behavior. A fishery in the prior portfolio that was closed in every
# year of the current window has no fleet CV, and its weight is spread over
# the unit's other prior fisheries (how much weight this moves is printed).
#
# THE MODELS. All on multi-fishery unit-windows, decomposed model only,
# clustered on the unit and the window start year, as in Table 4-rolling.
#   Panel A (unit-windows with U_fleet and the passive benchmark)
#     (1) No control, same sample as the columns that follow
#     (2) + U_fleet, in levels like the rest of Table 4
#     (3) + U_fleet decile fixed effects, a check on functional form, since
#         closure years give U_fleet a long right tail
#     (4) + U_fleet, with primary fishery x window fixed effects in place of
#         the two separate ones, so a primary fishery's riskiness can differ
#         by window
#     (5) + the passive benchmark CV in place of U_fleet. It follows fleet
#         revenue in the unit's own mix, so it also nets out the fleet-level
#         offsetting between fisheries
#   Panel B (unit-windows that also have a preceding window)
#     (6) No control, same sample as (7) and (8)
#     (7) + U_fleet, current weights
#     (8) + U_fleet, prior-window weights
#
# HOW TO READ IT. If the Phi coefficient stays near 1 and well above H_LR in
# (2) through (5), the Table 4 headline gets stronger. If it falls a lot,
# the "reallocation adds instability" reading in Section 4 should be
# softened, unless (8) falls much less than (7). This is an appendix
# robustness table rather than a headline model, so it is not run through
# the stride-6 phase check.
#
# Reads intermediate data/ch3_panel.rdata, intermediate data/ch3_rolling.rdata
# and intermediate data/ch3_rolling_owner.rdata, so both 01b_ scripts must
# have run. Optionally reads intermediate data/ch3_cv_decomposition.rdata
# and intermediate data/ch3_cv_decomposition_owner.rdata for the U_fleet
# check. Writes table4_fishery_riskiness_rolling.tex,
# table4_fishery_riskiness_rolling_owner.tex and
# table4_fishery_riskiness_summary_rolling.csv (coefficients only, safe to
# bring off the server).

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

load(panel_path)
load(file.path(intermediate_dir, "ch3_rolling.rdata"))
load(file.path(intermediate_dir, "ch3_rolling_owner.rdata"))

# ============================================================================
# 1. Fishery riskiness for any unit-window
# ============================================================================
#
# weight_years and cv_years are data.tables of (unit, window.start,
# Batch.Year). Passing the same years for both gives the breakdown's U_fleet.
# The fishery-level series is built exactly as in 14_cv_decomposition.R
# Section 2.

u_fleet <- function(weight_years, cv_years, unit_rev, fishery_rows, fleet_stats, id_col) {
  keys <- c(id_col, "window.start")

  # Weights, fishery j's share of the unit's mean revenue over the weight
  # years (years without revenue in j count as zero), floored at 0 as in 14_
  wy <- merge(weight_years, unit_rev, by = c(id_col, "Batch.Year"))
  totals <- wy[, .(mean.R = mean(R), n.w = .N), by = keys]
  rev_w <- merge(weight_years, fishery_rows, by = c(id_col, "Batch.Year"), allow.cartesian = TRUE)
  w <- rev_w[, .(sum.y = sum(revenue)), by = c(keys, "Fishery")]
  w <- merge(w, totals, by = keys)
  w[, omega := pmax(sum.y / n.w, 0) / mean.R]
  w <- w[omega > 0, c(keys, "Fishery", "omega"), with = FALSE]

  # Fishery-level series over the CV years
  grid <- merge(w, cv_years, by = keys, allow.cartesian = TRUE)
  grid <- merge(grid, fishery_rows[, c(id_col, "Batch.Year", "Fishery", "revenue", "fished"), with = FALSE],
                by = c(id_col, "Batch.Year", "Fishery"), all.x = TRUE)
  grid[is.na(revenue), `:=`(revenue = 0, fished = FALSE)]
  grid <- merge(grid, fleet_stats, by = c("Batch.Year", "Fishery"), all.x = TRUE)
  grid[, fishery.series := fcase(
    is.na(fleet_mean_revenue), 0,
    fished & n_active > 1, (fleet_mean_revenue * n_active - revenue) / (n_active - 1),
    default = fleet_mean_revenue
  )]

  by_fishery <- grid[, .(
    omega      = omega[1],
    cv.f       = sd(fishery.series) / mean(fishery.series),
    any.closed = any(is.na(fleet_mean_revenue))
  ), by = c(keys, "Fishery")]
  by_fishery[, defined := is.finite(cv.f)]

  # A weighted average over the fisheries with a fleet CV, scaled back up to
  # the full weight, so it equals 14_'s U_fleet whenever every fishery has one
  by_fishery[, .(
    U                = sum(omega[defined] * cv.f[defined]) / sum(omega[defined]) * sum(omega),
    weight.undefined = sum(omega[!defined]) / sum(omega),
    weight.closure   = sum(omega[any.closed]) / sum(omega)
  ), by = keys]
}

# The unit's active years in [s - 6, s - 1], kept only when there are at
# least ROLL_MIN_LOOKBACK_YEARS of them
prior_years <- function(targets, unit_rev, id_col) {
  keys <- c(id_col, "window.start")
  x <- merge(targets, unit_rev[, c(id_col, "Batch.Year"), with = FALSE], by = id_col, allow.cartesian = TRUE)
  x <- x[Batch.Year >= window.start - ROLL_WINDOW_WIDTH & Batch.Year <= window.start - 1]
  x[, n.prior := .N, by = keys]
  x[n.prior >= ROLL_MIN_LOOKBACK_YEARS, c(keys, "Batch.Year"), with = FALSE]
}

# ============================================================================
# 2. Inputs for each grain
# ============================================================================

vessel_fleet_stats <- as.data.table(fleet_mean_revenue)[, .(Batch.Year, Fishery, fleet_mean_revenue, n_active)]

# fleet_mean_revenue_owner carries no count of active owners, so it is
# rebuilt from the rows its mean was taken over and checked, as in
# 14_cv_decomposition_owner.R Section 1
owner_fleet_stats <- as.data.table(owner_fishery_year)[
  fished == TRUE & !is.na(revenue),
  .(fleet_mean_rebuilt = mean(revenue), n_active = .N),
  by = .(Batch.Year, Fishery)
]
owner_fleet_stats <- merge(
  owner_fleet_stats,
  as.data.table(fleet_mean_revenue_owner)[, .(Batch.Year, Fishery, fleet_mean_revenue)],
  by = c("Batch.Year", "Fishery"), all = TRUE
)
owner_rebuild_gap <- owner_fleet_stats[, max(abs(fleet_mean_rebuilt - fleet_mean_revenue) / fleet_mean_revenue)]
cat("Max relative gap between the rebuilt and saved owner fleet mean (should be about 0) -",
    signif(owner_rebuild_gap, 3), "\n")
stopifnot(is.finite(owner_rebuild_gap), owner_rebuild_gap < 1e-8)
owner_fleet_stats <- owner_fleet_stats[, .(Batch.Year, Fishery, fleet_mean_revenue, n_active)]

GRAINS <- list(
  vessel = list(
    label        = "vessel",
    id           = "Vessel.ADFG.Number",
    summary      = vessel_window_summary.rolling,
    window_years = vessel_year_window_eligible.rolling,
    unit_rev     = as.data.table(vessel_year)[vessel.year.rev > 0, .(Vessel.ADFG.Number, Batch.Year, R = vessel.year.rev)],
    fishery_rows = as.data.table(vessel_fishery_year)[!is.na(revenue) & revenue != 0,
                                                       .(Vessel.ADFG.Number, Batch.Year, Fishery, revenue, fished)],
    fleet_stats  = vessel_fleet_stats,
    passive      = passive_benchmark_window.rolling,
    check_path   = file.path(intermediate_dir, "ch3_cv_decomposition.rdata"),
    check_object = "cv_decomp.rolling",
    check_column = "U.fleet",
    table_file   = "table4_fishery_riskiness_rolling.tex"
  ),
  owner = list(
    label        = "owner",
    id           = "File.Number",
    summary      = owner_window_summary.rolling,
    window_years = owner_year_window_eligible.rolling,
    unit_rev     = as.data.table(owner_year)[owner.year.rev > 0, .(File.Number, Batch.Year, R = owner.year.rev)],
    fishery_rows = as.data.table(owner_fishery_year)[!is.na(revenue) & revenue != 0,
                                                      .(File.Number, Batch.Year, Fishery, revenue, fished)],
    fleet_stats  = owner_fleet_stats,
    passive      = passive_benchmark_window_owner.rolling,
    check_path   = file.path(intermediate_dir, "ch3_cv_decomposition_owner.rdata"),
    check_object = "cv_decomp_owner.rolling",
    check_column = "U.fleet.all",
    table_file   = "table4_fishery_riskiness_rolling_owner.tex"
  )
)

# ============================================================================
# 3. Build the samples, fit the models, write the table, for one grain
# ============================================================================

coef_row <- function(m, term) {
  if (term %in% names(coef(m))) c(unname(coef(m)[term]), unname(se(m)[term])) else c(NA_real_, NA_real_)
}

run_grain <- function(g) {
  id <- g$id
  keys <- c(id, "window.start")
  cat("\n===== Table 4-rolling with fishery riskiness,", g$label, "=====\n")

  # Table 4-rolling's own main-text sample (05b_ Section 1)
  multi <- g$summary %>%
    filter(is.finite(rev.cv), !is.specialist.window) %>%
    select(all_of(keys), rev.cv, H_LR, Phi, prime.fishery.window)
  targets <- as.data.table(multi)[, ..keys]
  cv_years <- merge(as.data.table(g$window_years)[, c(keys, "Batch.Year"), with = FALSE], targets, by = keys)

  # Current-weights U_fleet, the breakdown's fishery riskiness factor
  u_now <- u_fleet(cv_years, cv_years, g$unit_rev, g$fishery_rows, g$fleet_stats, id)
  setnames(u_now, c("U", "weight.closure"), c("U.fleet", "weight.closure.now"))
  u_now[, weight.undefined := NULL]

  chk <- new.env()
  if (file.exists(g$check_path)) load(g$check_path, envir = chk)
  if (exists(g$check_object, envir = chk) && g$check_column %in% names(chk[[g$check_object]])) {
    saved <- as.data.table(chk[[g$check_object]])
    if ("mode" %in% names(saved)) saved <- saved[mode == "zero"]
    saved <- unique(saved[, c(keys, g$check_column), with = FALSE])
    # Renamed so it cannot clash with U.fleet in u_now (the vessel column has the same name)
    setnames(saved, g$check_column, "U.saved")
    both <- merge(u_now, saved, by = keys)
    cat("U_fleet against the saved", g$check_object, "- units compared -", nrow(both),
        ", max relative gap (should be about 0) -",
        signif(max(abs(both$U.fleet / both$U.saved - 1)), 3), "\n")
  } else {
    cat("No saved", g$check_object, "with", g$check_column, "to check U_fleet against",
        "(run the 14_ script first for that check)\n")
  }

  # Prior-window-weights U_fleet
  w_prior <- prior_years(targets, g$unit_rev, id)
  u_prior <- u_fleet(w_prior, merge(cv_years, unique(w_prior[, ..keys]), by = keys),
                     g$unit_rev, g$fishery_rows, g$fleet_stats, id)
  setnames(u_prior, c("U", "weight.undefined", "weight.closure"),
           c("U.fleet.prior", "weight.undefined.prior", "weight.closure.prior"))

  passive <- g$passive %>% select(all_of(keys), passive.cv)
  data <- multi %>%
    left_join(as_tibble(u_now), by = keys) %>%
    left_join(as_tibble(u_prior), by = keys) %>%
    left_join(passive, by = keys)

  panel_a <- data %>% filter(is.finite(U.fleet), is.finite(passive.cv)) %>%
    mutate(ufleet.decile = ntile(U.fleet, 10))
  panel_b <- data %>% filter(is.finite(U.fleet), is.finite(U.fleet.prior))

  cat("Multi-fishery unit-windows in Table 4-rolling -", nrow(multi),
      ", with U_fleet and the passive benchmark (panel A) -", nrow(panel_a),
      ", also with a preceding window (panel B) -", nrow(panel_b), "\n")
  cat("U_fleet quantiles (1, 10, 50, 90, 99 percent) -",
      paste(round(quantile(panel_a$U.fleet, c(0.01, 0.1, 0.5, 0.9, 0.99)), 3), collapse = ", "), "\n")
  cat("Correlation of U_fleet with Phi -", round(cor(panel_a$U.fleet, panel_a$Phi), 3),
      ", with H_LR -", round(cor(panel_a$U.fleet, panel_a$H_LR), 3),
      ", with the passive CV -", round(cor(panel_a$U.fleet, panel_a$passive.cv), 3), "\n")
  cat("Mean revenue weight on fisheries with a closure year in the window -",
      round(mean(panel_a$weight.closure.now), 4), "\n")
  cat("Panel B, correlation of current- and prior-weights U_fleet -",
      round(cor(panel_b$U.fleet, panel_b$U.fleet.prior), 3),
      ", mean prior weight spread over other fisheries because the fishery was closed all window -",
      round(mean(panel_b$weight.undefined.prior), 4), "\n")

  cl <- as.formula(paste("~", id, "+ window.start"))
  fe <- "prime.fishery.window + window.start"
  fit <- function(rhs, fes, d) feols(as.formula(paste("rev.cv ~", rhs, "|", fes)), data = d, cluster = cl)

  models <- list(
    a1 = fit("H_LR + Phi", fe, panel_a),
    a2 = fit("H_LR + Phi + U.fleet", fe, panel_a),
    a3 = fit("H_LR + Phi", paste(fe, "+ ufleet.decile"), panel_a),
    a4 = fit("H_LR + Phi + U.fleet", "prime.fishery.window^window.start", panel_a),
    a5 = fit("H_LR + Phi + passive.cv", fe, panel_a),
    b1 = fit("H_LR + Phi", fe, panel_b),
    b2 = fit("H_LR + Phi + U.fleet", fe, panel_b),
    b3 = fit("H_LR + Phi + U.fleet.prior", fe, panel_b)
  )

  # Plain headers for the console and the summary CSV
  headers <- c("No control", "U fleet", "U fleet deciles", "Prime x window FE", "Passive CV",
               "No control (B)", "U fleet (B)", "Prior weights (B)")
  # The LaTeX table uses the chapter's notation (TABLE4_DICT from 00_setup.R
  # plus this table's own controls), with a top row naming the two samples
  dict_tex <- c(
    TABLE4_DICT,
    U.fleet = "$U^{\\text{fleet}}$", U.fleet.prior = "$U^{\\text{fleet}}$, prior-window weights",
    passive.cv = "Passive benchmark CV", ufleet.decile = "$U^{\\text{fleet}}$ decile",
    "prime.fishery.window^window.start" = "Primary fishery $\\times$ window start year"
  )
  # Columns 2 and 4 share a header and differ only in the fixed-effect rows
  headers_tex <- list(
    ":_:Sample" = list("All multi-fishery windows" = 5, "With a preceding window" = 3),
    "Control" = c("None", "$U^{\\text{fleet}}$", "$U^{\\text{fleet}}$ deciles", "$U^{\\text{fleet}}$",
                  "Passive CV", "None", "$U^{\\text{fleet}}$", "Prior weights")
  )
  etable(models, headers = headers_tex, dict = dict_tex, tex = TRUE,
         file = file.path(table_dir, g$table_file), replace = TRUE)
  # Same star cutoffs as the LaTeX table (1, 5 and 10 percent)
  print(etable(models, headers = headers,
               dict = c(U.fleet = "Fishery riskiness (U fleet)",
                        U.fleet.prior = "Fishery riskiness, prior-window weights",
                        passive.cv = "Passive benchmark CV", ufleet.decile = "U fleet decile"),
               signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)))
  cat("Wrote", g$table_file, "\n")

  # How far each control moves the Phi and H_LR coefficients, against the
  # no-control column on the same sample
  base_of <- c(a1 = "a1", a2 = "a1", a3 = "a1", a4 = "a1", a5 = "a1", b1 = "b1", b2 = "b1", b3 = "b1")
  map_dfr(names(models), function(k) {
    m <- models[[k]]
    control <- intersect(c("U.fleet", "U.fleet.prior", "passive.cv"), names(coef(m)))
    phi <- coef_row(m, "Phi")
    hlr <- coef_row(m, "H_LR")
    ctl <- if (length(control)) coef_row(m, control) else c(NA_real_, NA_real_)
    phi_base <- unname(coef(models[[base_of[[k]]]])["Phi"])
    tibble(
      grain = g$label, model = k, header = headers[match(k, names(models))],
      control = if (length(control)) control else "none", n = nobs(m),
      H_LR = hlr[1], H_LR.se = hlr[2], Phi = phi[1], Phi.se = phi[2],
      control.coef = ctl[1], control.se = ctl[2],
      Phi.change.pct = 100 * (phi[1] / phi_base - 1)
    )
  })
}

summary_tbl <- map_dfr(GRAINS, run_grain)

cat("\n===== Summary, change in the Phi coefficient against the no-control column on the same sample =====\n")
print(summary_tbl %>% mutate(across(where(is.double), ~ round(.x, 4))), n = Inf, width = Inf)

write.csv(summary_tbl, file.path(table_dir, "table4_fishery_riskiness_summary_rolling.csv"), row.names = FALSE)
cat("Wrote table4_fishery_riskiness_summary_rolling.csv\n")
