# Chapter 3 empirical pipeline, owner-level twin of 14_cv_decomposition.R,
# run two ways, once counting the years an owner did not fish a fishery as
# zeros (as 14_ does) and once leaving those years out
#
# File.Number means the permit holder, as everywhere else in this pipeline
# (see 05_table4_figure3_owner.R's header). The owner is the unit here because
# an owner running several vessels can move effort across its vessels as well
# as across fisheries, and that margin is invisible at the vessel level.
#
# THE IDENTITY. The same as 14_, with owner i in place of vessel i. Over a set
# of active years (owner.year.rev > 0), with y_ijt the owner's revenue in
# fishery j in year t and R_it = sum_j y_ijt,
#
#   CV_i = U_fleet_i * (U_i / U_fleet_i) * (CV_i / U_i)
#
# where U_i = sum_j omega_ij * CV_ij, U_fleet_i = sum_j omega_ij *
# CV_fleet_ij, and omega_ij = mean(y_ij) / mean(R_i) is fishery j's share of
# the owner's mean revenue (floored at 0, as in 14_). The fishery-level series
# behind CV_fleet_ij is the same leave-one-out fleet mean as in 14_, here per
# active owner rather than per active vessel. The two versions differ only in
# how CV_ij and CV_fleet_ij treat a year the owner was active somewhere but
# did not fish j.
#
#   "zero"    The year counts as a zero in y_ij, exactly as in 14_. Then U_i =
#             sum_j sd(y_ij) / mean(R_i), so the third factor DR_i = CV_i /
#             U_i can never exceed 1 and reads as diversification. But an
#             owner that moves from fishery A to fishery B gets a run of zeros
#             in each, so CV_iA and CV_iB both look large (pushing the
#             owner-specific factor up) and A and B look almost perfectly
#             opposed (pushing DR_i down). The two errors cancel in CV_i.
#   "active"  The year is left out. CV_ij is the CV of y_ij over the years the
#             owner fished j (revenue > 0), and CV_fleet_ij is the CV of the
#             fishery-level series over those same years. The owner-specific
#             factor then compares the owner with the fleet over the same
#             years, with no entry and exit in it. The third factor CV_i / U_i
#             is no longer bounded by 1. It holds whatever combining the
#             fisheries does to risk, which is the offsetting between
#             fisheries fished in the same years plus the risk of moving
#             between fisheries that pay different amounts (entry, exit and
#             switching). Above 1 means the moves added more risk than the
#             offsetting took away.
#
# FOUR FACTORS IN THE "ACTIVE" VERSION. Taken over the owner's spells only,
# U_fleet_i would depend on the owner's own entry and exit. Short spells look
# calm, a one-year spell looks riskless, and closures drop out because the
# owner cannot fish a closed fishery. So fishery riskiness is always the
# fleet's CV over all the unit's active years (U_fleet_all_i, the "zero"
# version's U_fleet_i), and the gap to the spell-years U_fleet_i gets its own
# factor,
#
#   CV_i = U_fleet_all_i * (U_fleet_i / U_fleet_all_i) * (U_i / U_fleet_i) * (CV_i / U_i)
#
# The new second factor (the spell effect) is below 1 when the fleet looks
# calmer during the owner's spells than over all its years, because the
# spells are short, miss closures or fall in the fishery's calmer years (the
# three cannot be told apart here). In the "zero" version both fleet terms
# cover the same years, so the spell effect is exactly 1 and the split is the
# three factors of 14_. The spell effect only cleans the fishery factor. The
# owner's own CVs are still taken over its spells, so any short-spell effect
# on them stays in the third factor.
#
# Two things about the "active" version worth knowing before reading it.
# (1) A fishery fished in only one of a unit's active years has no CV of its
# own. Its CV_ij and CV_fleet_ij are set to 0, so the owner's side of its risk
# lands in the third factor and the fleet's side in the spell effect. The
# revenue weight on one-year spells, and on two-year spells (whose CV rests on
# two numbers), is printed and saved. (2) A fishery-wide closure still counts
# as fishery riskiness, but the spell effect takes it back out, since the
# owner did not fish j that year either. Its effect on R_it lands in the third
# factor.
#
# THE COMPARISON. Both versions are computed on the same units, so realized CV
# and its variance are identical in the two, and only the split differs. A
# unit enters only if every piece is finite in both versions. The "active"
# version loses any unit whose fisheries were all one-year spells (U_i = 0),
# and the script prints how many go. A unit that fished every fishery in its
# portfolio in every active year has no zeros to treat, so its pieces must be
# identical in the two versions, which is checked.
#
# Variance shares, Phi bins and specialists work as in 14_ (see its header).
# A specialist has no zeros either, so it is identical in the two versions,
# with a third factor of exactly 1.
#
# Reads intermediate data/ch3_panel.rdata (01_build_panel.R) and
# intermediate data/ch3_rolling_owner.rdata
# (rolling_periods/01b_build_rolling_panel_owner.R). Writes the same outputs
# as 14_ with an "_owner" suffix, now with both versions side by side, plus
# cv_decomposition_mode_comparison_owner.csv (aggregates only, safe to bring
# off the server). Saves the owner-level and owner-window-level pieces to
# intermediate data/ch3_cv_decomposition_owner.rdata.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_fishery_year") || !exists("owner_year") || !exists("fleet_mean_revenue_owner") ||
    !exists("owner_summary") || !exists("owner_mean_share")) {
  load(panel_path)
}

rolling_owner_panel_path <- file.path(intermediate_dir, "ch3_rolling_owner.rdata")
if (!exists("owner_window_summary.rolling") || !exists("owner_year_window_eligible.rolling")) {
  load(rolling_owner_panel_path)
}

MODES <- c(zero = "Missing years as zeros", active = "Active years only")

PIECES <- c(
  p.fishery = "Fishery riskiness",
  p.spell   = "Spell effect",
  p.owner   = "Owner-specific volatility",
  p.combine = "Third factor"
)

# The spell effect is exactly 1 with zeros, so that version's shares and table
# leave it out
mode_pieces <- function(mode) if (mode == "zero") setdiff(names(PIECES), "p.spell") else names(PIECES)

# The third factor means something different in each version
THIRD_LABEL <- c(zero = "Diversification (DR)", active = "Combining and switching")

piece_label <- function(piece, mode) {
  unname(ifelse(piece == "p.combine", THIRD_LABEL[mode], PIECES[piece]))
}

# ============================================================================
# 1. Inputs shared by both grains
# ============================================================================
#
# Zero-revenue rows are dropped and negative-revenue rows kept, for the
# reasons given in 14_ Section 1. A negative-revenue year is not a year the
# owner fished j, so the "active" version leaves it out of CV_ij like any
# other year without a positive landing, and its effect on R_it lands in the
# third factor.

fishery_revenue_rows_owner <- as.data.table(owner_fishery_year)[
  !is.na(revenue) & revenue != 0,
  .(File.Number, Batch.Year, Fishery, revenue, fished)
]
stopifnot(!anyDuplicated(fishery_revenue_rows_owner, by = c("File.Number", "Batch.Year", "Fishery")))

# Unlike the vessel-level fleet_mean_revenue, fleet_mean_revenue_owner carries
# no count of active owners, and the leave-one-out series needs one. It is
# rebuilt here from the same rows the mean was taken over (fished owners in
# owner_fishery_year), and the rebuilt mean is checked against the saved one,
# so the count is the mean's true denominator.
fleet_stats_owner <- as.data.table(owner_fishery_year)[
  fished == TRUE & !is.na(revenue),
  .(fleet_mean_rebuilt = mean(revenue), n_active = .N),
  by = .(Batch.Year, Fishery)
]
fleet_stats_owner <- merge(
  fleet_stats_owner,
  as.data.table(fleet_mean_revenue_owner)[, .(Batch.Year, Fishery, fleet_mean_revenue)],
  by = c("Batch.Year", "Fishery"), all = TRUE
)
fleet_rebuild_gap <- fleet_stats_owner[, max(abs(fleet_mean_rebuilt - fleet_mean_revenue) / fleet_mean_revenue)]
cat("Max relative gap between the rebuilt and saved owner fleet mean (should be about 0)",
    signif(fleet_rebuild_gap, 3), "\n")
stopifnot(is.finite(fleet_rebuild_gap), fleet_rebuild_gap < 1e-8)
fleet_stats_owner <- fleet_stats_owner[, .(Batch.Year, Fishery, fleet_mean_revenue, n_active)]

# ============================================================================
# 2. The decomposition, both versions, for any unit (owner or owner-window)
# ============================================================================
#
# The fishery-level series is built exactly as in 14_ Section 2 (closures as
# 0, leave-one-out when the owner fished j alongside others, the full mean
# when it was the only one). In a year the owner fished j the fleet was
# active by definition, so the spell-years fleet CV never sees a closure zero.
# The all-years fleet CV behind fishery riskiness does.

cv_decompose_owner <- function(unit_years, unit_cols, label) {
  unit_years <- as.data.table(unit_years)[, c(unit_cols, "Batch.Year", "owner.year.rev"), with = FALSE]

  # Every fishery the unit has nonzero revenue in during its own active years
  own <- merge(unit_years[, c(unit_cols, "Batch.Year"), with = FALSE], fishery_revenue_rows_owner,
               by = c("File.Number", "Batch.Year"), allow.cartesian = TRUE)
  portfolio <- unique(own[, c(unit_cols, "Fishery"), with = FALSE])

  # One row per unit, portfolio fishery and active year, zero-filled
  grid <- merge(portfolio, unit_years, by = unit_cols, allow.cartesian = TRUE)
  grid <- merge(grid, own[, c(unit_cols, "Batch.Year", "Fishery", "revenue", "fished"), with = FALSE],
                by = c(unit_cols, "Batch.Year", "Fishery"), all.x = TRUE)
  grid[is.na(revenue), `:=`(revenue = 0, fished = FALSE)]

  grid <- merge(grid, fleet_stats_owner, by = c("Batch.Year", "Fishery"), all.x = TRUE)
  grid[, loo.fallback := fished & !is.na(n_active) & n_active == 1]
  grid[, fishery.series := fcase(
    is.na(fleet_mean_revenue), 0,
    fished & n_active > 1, (fleet_mean_revenue * n_active - revenue) / (n_active - 1),
    default = fleet_mean_revenue
  )]

  # The fishery series must add up to R_it in every unit-year, or the "zero"
  # version's DR is not bounded by 1
  year_check <- grid[, .(sum.y = sum(revenue), R = owner.year.rev[1]), by = c(unit_cols, "Batch.Year")]
  max_rel_gap <- year_check[, max(abs(sum.y - R) / R)]

  # "zero" version, over all the unit's active years
  by_fishery <- grid[, .(
    mean.y     = mean(revenue),
    sd.y       = sd(revenue),
    mean.f     = mean(fishery.series),
    sd.f       = sd(fishery.series),
    n.fallback = sum(loo.fallback),
    n.years    = .N
  ), by = c(unit_cols, "Fishery")]

  # "active" version, over the years the unit fished j only. A one-year spell
  # gets a CV of 0 (see the header).
  spells <- grid[fished == TRUE, .(
    n.spell  = .N,
    mean.y.a = mean(revenue),
    sd.y.a   = sd(revenue),
    mean.f.a = mean(fishery.series),
    sd.f.a   = sd(fishery.series)
  ), by = c(unit_cols, "Fishery")]
  by_fishery <- merge(by_fishery, spells, by = c(unit_cols, "Fishery"), all.x = TRUE)
  by_fishery[is.na(n.spell), n.spell := 0L]
  by_fishery[, `:=`(
    cv.y.active = fifelse(n.spell >= 2, sd.y.a / mean.y.a, 0),
    cv.f.active = fifelse(n.spell >= 2, sd.f.a / mean.f.a, 0)
  )]

  totals <- unit_years[, .(mean.R = mean(owner.year.rev), sd.R = sd(owner.year.rev), n.years = .N),
                       by = unit_cols]

  by_fishery <- merge(by_fishery, totals[, c(unit_cols, "mean.R"), with = FALSE], by = unit_cols)
  by_fishery[, omega.pos := pmax(mean.y, 0) / mean.R]
  by_fishery[, `:=`(
    fleet.term        = fifelse(omega.pos > 0, omega.pos * sd.f / mean.f, 0),
    own.term.active   = omega.pos * cv.y.active,
    fleet.term.active = omega.pos * cv.f.active,
    fallback.weight   = omega.pos * n.fallback / n.years
  )]

  units <- by_fishery[, .(
    sum.sd.y        = sum(sd.y),
    U.fleet.zero    = sum(fleet.term),
    U.active        = sum(own.term.active),
    U.fleet.active  = sum(fleet.term.active),
    weight.spell1   = sum(omega.pos[n.spell == 1]),
    weight.spell2   = sum(omega.pos[n.spell == 2]),
    no.gaps         = all(n.spell == n.years),
    n.fisheries     = .N,
    fallback.weight = sum(fallback.weight)
  ), by = unit_cols]
  units <- as_tibble(merge(totals, units, by = unit_cols))

  # One row per unit and version. U.fleet covers the same years as U, and
  # U.fleet.all all the unit's active years, so with zeros the two are equal
  # and the spell effect is exactly 1.
  long <- bind_rows(
    units %>% mutate(mode = "zero", U.fleet = U.fleet.zero, U = sum.sd.y / mean.R),
    units %>% mutate(mode = "active", U.fleet = U.fleet.active, U = U.active)
  ) %>%
    mutate(
      U.fleet.all = U.fleet.zero,
      CV        = sd.R / mean.R,
      combine   = CV / U,
      log.cv    = log(CV),
      p.fishery = log(U.fleet.all),
      p.spell   = log(U.fleet / U.fleet.all),
      p.owner   = log(U / U.fleet),
      p.combine = log(combine)
    ) %>%
    select(all_of(unit_cols), mode, n.years, n.fisheries, CV, U.fleet.all, U.fleet, U, combine,
           log.cv, all_of(names(PIECES)), weight.spell1, weight.spell2, no.gaps, fallback.weight)

  zero_rows   <- long %>% filter(mode == "zero")
  active_rows <- long %>% filter(mode == "active")
  no_gap_diff <- rowSums(abs(as.matrix(zero_rows[names(PIECES)]) - as.matrix(active_rows[names(PIECES)])))
  no_gap_diff <- no_gap_diff[zero_rows$no.gaps & is.finite(no_gap_diff)]

  cat("\n===== CV decomposition,", label, "=====\n")
  cat("Units", nrow(units), " unit-fishery rows", nrow(by_fishery), "\n")
  cat("Max relative gap between summed fishery revenue and owner.year.rev (should be about 0)",
      signif(max_rel_gap, 3), "\n")
  cat("Max DR in the zero version (must not exceed 1)", signif(max(zero_rows$combine, na.rm = TRUE), 6), "\n")
  cat("Units with no gap years in any fishery", length(no_gap_diff),
      " max piece difference between versions among them (should be about 0)",
      signif(if (length(no_gap_diff)) max(no_gap_diff) else 0, 3), "\n")
  cat("Mean revenue weight on one-year spells", round(mean(units$weight.spell1), 4),
      " on two-year spells", round(mean(units$weight.spell2), 4), "\n")
  cat("Share of units with the active-years third factor above 1",
      round(mean(active_rows$combine > 1, na.rm = TRUE), 4), "\n")
  cat("Geometric mean spell effect in the active version (1 means the spells see the same fleet",
      "risk as all years)", round(exp(mean(active_rows$p.spell[is.finite(active_rows$p.spell)])), 4), "\n")
  cat("Mean revenue weight on fishery-years where the owner was the only participant",
      "(leave-one-out fallback)", round(mean(units$fallback.weight), 4),
      " share of units with any", round(mean(units$fallback.weight > 0), 4), "\n")

  long
}

# Keeps a unit only if every piece is finite in both versions, so the two are
# compared on the same units, and counts what went, for all units and for
# multi-fishery units alone
keep_common_units <- function(df, unit_cols, specialist_col, grain) {
  df <- df %>%
    mutate(ok = is.finite(log.cv) & is.finite(p.fishery) & is.finite(p.spell) & is.finite(p.owner) &
             is.finite(p.combine))
  unit_ok <- df %>%
    group_by(across(all_of(c(unit_cols, specialist_col)))) %>%
    summarise(ok.zero = all(ok[mode == "zero"]), ok.active = all(ok[mode == "active"]), .groups = "drop")
  dropped <- bind_rows(
    unit_ok %>% mutate(subset = "all"),
    unit_ok %>% filter(!.data[[specialist_col]]) %>% mutate(subset = "multi")
  ) %>%
    group_by(subset) %>%
    summarise(
      n.units.before   = n(),
      n.dropped.zero   = sum(!ok.zero),
      n.dropped.active = sum(!ok.active),
      n.dropped        = sum(!(ok.zero & ok.active)),
      .groups = "drop"
    ) %>%
    mutate(grain = grain)
  cat(grain, "units dropped for a non-finite piece, by version\n")
  print(dropped)
  list(
    data = df %>% semi_join(unit_ok %>% filter(ok.zero, ok.active), by = unit_cols) %>% select(-ok),
    dropped = dropped
  )
}

# ============================================================================
# 3. Lifetime grain, the same sample as Table 4 (owner)
# ============================================================================

active_owner_years <- owner_year %>%
  filter(owner.year.rev > 0) %>%
  select(File.Number, Batch.Year, owner.year.rev)

n_fisheries_fished_owner <- owner_mean_share %>%
  count(File.Number, name = "n.fisheries.fished")

owner_analysis <- owner_summary %>%
  filter(meets.min.years, is.finite(rev.cv), !is.na(prime.fishery)) %>%
  left_join(n_fisheries_fished_owner, by = "File.Number") %>%
  mutate(is.specialist = n.fisheries.fished == 1) %>%
  select(File.Number, rev.cv, H_LR, Phi, is.specialist, prime.fishery)

cv_decomp_owner_lifetime <- cv_decompose_owner(
  active_owner_years %>% semi_join(owner_analysis, by = "File.Number"),
  "File.Number", "owner lifetime"
) %>%
  inner_join(owner_analysis, by = "File.Number")

cat("Max |CV - rev.cv| against owner_summary (should be about 0)",
    signif(max(abs(cv_decomp_owner_lifetime$CV - cv_decomp_owner_lifetime$rev.cv)), 3), "\n")
cat("Specialists with a third factor of 1, by version (should be about 1)\n")
print(cv_decomp_owner_lifetime %>%
        filter(is.specialist) %>%
        group_by(mode) %>%
        summarise(share.at.1 = mean(abs(combine - 1) < 1e-9), .groups = "drop"))

lifetime_common <- keep_common_units(cv_decomp_owner_lifetime, "File.Number", "is.specialist", "lifetime")
cv_decomp_owner_lifetime <- lifetime_common$data

# ============================================================================
# 4. Rolling grain, the same sample as Table 4-rolling (owner)
# ============================================================================

owner_analysis.rolling <- owner_window_summary.rolling %>%
  filter(is.finite(rev.cv)) %>%
  select(File.Number, window.start, rev.cv, H_LR, Phi, is.specialist.window, prime.fishery.window)

cv_decomp_owner.rolling <- cv_decompose_owner(
  owner_year_window_eligible.rolling %>%
    semi_join(owner_analysis.rolling, by = c("File.Number", "window.start")),
  c("File.Number", "window.start"), "owner rolling"
) %>%
  inner_join(owner_analysis.rolling, by = c("File.Number", "window.start"))

cat("Max |CV - rev.cv| against owner_window_summary.rolling (should be about 0)",
    signif(max(abs(cv_decomp_owner.rolling$CV - cv_decomp_owner.rolling$rev.cv)), 3), "\n")
cat("Window specialists with a third factor of 1, by version (should be about 1)\n")
print(cv_decomp_owner.rolling %>%
        filter(is.specialist.window) %>%
        group_by(mode) %>%
        summarise(share.at.1 = mean(abs(combine - 1) < 1e-9), .groups = "drop"))

rolling_common <- keep_common_units(cv_decomp_owner.rolling, c("File.Number", "window.start"),
                                    "is.specialist.window", "rolling")
cv_decomp_owner.rolling <- rolling_common$data

dropped_tbl_owner <- bind_rows(lifetime_common$dropped, rolling_common$dropped)

# ============================================================================
# 5. Variance shares, both versions
# ============================================================================
#
# The same five columns as 14_, with an owner fixed effect in place of the
# vessel one. Within a column both versions run on the same units.

variance_shares <- function(data, column, fe, vcov_spec) {
  rhs <- if (is.null(fe)) "log.cv" else paste("log.cv |", fe)
  map_dfr(names(MODES), function(md) {
    mode_data <- data %>% filter(mode == md)
    map_dfr(mode_pieces(md), function(p) {
      m <- feols(as.formula(paste(p, "~", rhs)), data = mode_data, vcov = vcov_spec)
      tibble(mode = md, column = column, piece = p,
             share = unname(coef(m)["log.cv"]), se = unname(se(m)["log.cv"]), n = nobs(m))
    })
  }) %>%
    mutate(piece.label = piece_label(piece, mode))
}

lifetime_multi_owner <- cv_decomp_owner_lifetime %>% filter(!is.specialist)
rolling_multi_owner  <- cv_decomp_owner.rolling %>% filter(!is.specialist.window)
twoway_cluster_owner <- ~File.Number + window.start

decomp_columns <- list(
  list(column = "Lifetime, multi", data = lifetime_multi_owner, fe = NULL,
       vcov = "hetero", fe.label = "None", grain = "lifetime", subset = "multi"),
  list(column = "Lifetime, all", data = cv_decomp_owner_lifetime, fe = NULL,
       vcov = "hetero", fe.label = "None", grain = "lifetime", subset = "all"),
  list(column = "Rolling, multi", data = rolling_multi_owner, fe = "window.start",
       vcov = twoway_cluster_owner, fe.label = "Window", grain = "rolling", subset = "multi"),
  list(column = "Rolling, all", data = cv_decomp_owner.rolling, fe = "window.start",
       vcov = twoway_cluster_owner, fe.label = "Window", grain = "rolling", subset = "all"),
  list(column = "Rolling, multi, owner FE", data = rolling_multi_owner, fe = "File.Number + window.start",
       vcov = twoway_cluster_owner, fe.label = "Owner and window", grain = "rolling", subset = "multi")
)

shares_tbl_owner <- map_dfr(decomp_columns, ~ variance_shares(.x$data, .x$column, .x$fe, .x$vcov))

# Typical size of each factor, by version. The geometric means multiply out
# to the geometric mean of realized CV exactly, because the logs add.
geo_means_tbl_owner <- map_dfr(decomp_columns, function(cl) {
  cl$data %>%
    group_by(mode) %>%
    summarise(
      geo.cv         = exp(mean(log.cv)),
      geo.U.fleet    = exp(mean(p.fishery)),
      geo.spell      = exp(mean(p.spell)),
      geo.owner      = exp(mean(p.owner)),
      geo.combine    = exp(mean(p.combine)),
      share.combine.above.1 = mean(combine > 1),
      .groups = "drop"
    ) %>%
    mutate(column = cl$column, fixed.effects = cl$fe.label)
})

print(shares_tbl_owner %>% select(mode, column, piece.label, share, se, n), n = Inf, width = Inf)
cat("Sum of the shares by version and column (must be 1)\n")
print(shares_tbl_owner %>% group_by(mode, column) %>% summarise(sum.share = sum(share), .groups = "drop"),
      n = Inf)
print(geo_means_tbl_owner, n = Inf, width = Inf)

write.csv(
  shares_tbl_owner %>% left_join(geo_means_tbl_owner, by = c("mode", "column")),
  file.path(table_dir, "cv_decomposition_shares_owner.csv"), row.names = FALSE
)
cat("Wrote cv_decomposition_shares_owner.csv\n")

# ============================================================================
# 6. How the two versions differ, unit by unit
# ============================================================================
#
# One row per sample (the owner FE column uses the same units as "Rolling,
# multi", so it is not repeated). Fishery riskiness is the same in both
# versions by construction, so the correlations cover the owner-specific and
# third factors, and say how much each reorders units when the zeros are
# dropped. The spell weights say how much of the active-years version's third
# factor is forced there by one-year and two-year spells rather than measured.

compare_modes <- function(cl, unit_cols) {
  wide <- cl$data %>%
    select(all_of(unit_cols), mode, p.owner, p.combine, combine, weight.spell1, weight.spell2) %>%
    pivot_wider(names_from = mode, values_from = c(p.owner, p.combine, combine))
  dropped <- dropped_tbl_owner %>% filter(grain == cl$grain, subset == cl$subset)
  tibble(
    column                   = cl$column,
    n.units                  = nrow(wide),
    n.units.before           = dropped$n.units.before,
    n.dropped.active         = dropped$n.dropped.active,
    n.dropped.zero           = dropped$n.dropped.zero,
    cor.owner               = cor(wide$p.owner_zero, wide$p.owner_active),
    cor.combine              = cor(wide$p.combine_zero, wide$p.combine_active),
    share.active.combine.gt1 = mean(wide$combine_active > 1),
    mean.weight.spell1       = mean(wide$weight.spell1),
    mean.weight.spell2       = mean(wide$weight.spell2),
    share.any.spell1         = mean(wide$weight.spell1 > 0)
  )
}

mode_comparison_owner <- map_dfr(
  keep(decomp_columns, ~ .x$fe.label != "Owner and window"),
  function(cl) compare_modes(cl, if (cl$grain == "lifetime") "File.Number" else c("File.Number", "window.start"))
)

print(mode_comparison_owner, width = Inf)
write.csv(mode_comparison_owner, file.path(table_dir, "cv_decomposition_mode_comparison_owner.csv"),
          row.names = FALSE)
cat("Wrote cv_decomposition_mode_comparison_owner.csv\n")

# ============================================================================
# 7. LaTeX table, both versions stacked
# ============================================================================

column_order <- map_chr(decomp_columns, "column")

mode_rows <- map_dfr(names(MODES), function(md) {
  s <- shares_tbl_owner %>% filter(mode == md)
  g <- geo_means_tbl_owner %>% filter(mode == md)

  share_rows <- s %>%
    mutate(Statistic = paste0("Share, ", tolower(substr(piece.label, 1, 1)), substring(piece.label, 2)),
           cell = sprintf("%.3f (%.3f)", share, se)) %>%
    select(Statistic, column, cell)
  sum_row <- s %>%
    group_by(column) %>%
    summarise(cell = sprintf("%.3f", sum(share)), .groups = "drop") %>%
    mutate(Statistic = "Sum of shares")
  geo_rows <- g %>%
    transmute(
      column,
      `Geometric mean, fishery riskiness factor` = sprintf("%.3f", geo.U.fleet),
      `Geometric mean, spell effect`             = sprintf("%.3f", geo.spell),
      `Geometric mean, owner-specific factor`    = sprintf("%.3f", geo.owner),
      `Geometric mean, third factor (CV / U)`    = sprintf("%.3f", geo.combine)
    ) %>%
    pivot_longer(-column, names_to = "Statistic", values_to = "cell")
  if (!"p.spell" %in% mode_pieces(md)) {
    geo_rows <- geo_rows %>% filter(Statistic != "Geometric mean, spell effect")
  }

  bind_rows(share_rows, sum_row, geo_rows) %>%
    mutate(Statistic = factor(Statistic, levels = unique(Statistic))) %>%
    pivot_wider(names_from = column, values_from = cell) %>%
    mutate(Statistic = as.character(Statistic),
           `Missing years` = c(MODES[[md]], rep("", n() - 1)))
})

common_rows <- bind_rows(
  geo_means_tbl_owner %>%
    filter(mode == "zero") %>%
    transmute(column, Statistic = "Geometric mean, realized CV", cell = sprintf("%.3f", geo.cv)),
  geo_means_tbl_owner %>%
    filter(mode == "zero") %>%
    transmute(column, Statistic = "Fixed effects", cell = fixed.effects),
  shares_tbl_owner %>%
    filter(mode == "zero") %>%
    distinct(column, n) %>%
    transmute(column, Statistic = "Observations", cell = formatC(n, format = "d", big.mark = ",")),
  map_dfr(decomp_columns, function(cl) {
    d <- dropped_tbl_owner %>% filter(grain == cl$grain, subset == cl$subset)
    tibble(column = cl$column, Statistic = "Units dropped by the active-years version",
           cell = formatC(d$n.dropped.active, format = "d", big.mark = ","))
  })
) %>%
  mutate(Statistic = factor(Statistic, levels = unique(Statistic))) %>%
  pivot_wider(names_from = column, values_from = cell) %>%
  mutate(Statistic = as.character(Statistic), `Missing years` = "")

cv_decomp_table_owner <- bind_rows(mode_rows, common_rows) %>%
  select(`Missing years`, Statistic, all_of(column_order))

print(xtable(
  cv_decomp_table_owner,
  caption = paste(
    "Exact decomposition of realized owner revenue CV, with the years an owner did not fish a",
    "fishery counted as zeros (top) or left out (bottom). The share rows give each piece's share of",
    "the variance in log CV after removing the listed fixed effects, with standard errors in",
    "parentheses (heteroskedasticity-robust for lifetime, clustered on owner and window for",
    "rolling). Fishery riskiness is the fleet's CV over all of the owner's active years in both",
    "versions. With the years left out, the owner's CV in each fishery is taken over the years it",
    "fished there, and the spell effect is the fleet's CV over those same years relative to all",
    "years (below 1 when the spells are short, miss closures or fall in calmer years). With zeros",
    "the spell effect is 1 and the third factor is the diversification ratio, which is at most 1.",
    "With the years left out the third factor also holds the risk of moving between fisheries and",
    "can exceed 1. Both versions use the same units, so realized CV is the same in both."
  ),
  label = "tab:ch3-cv-decomposition-owner",
  align = c("l", "l", "l", rep("r", length(column_order)))
),
file = file.path(table_dir, "table_cv_decomposition_owner.tex"),
include.rownames = FALSE, size = "\\small")

cat("Wrote table_cv_decomposition_owner.tex\n")

# ============================================================================
# 8. The pieces by Phi bin, both versions
# ============================================================================
#
# Same binning and two-stage averaging as 14_ Section 6, with owners in place
# of vessels. Both versions use the same units and the same Phi, so the bins
# hold the same units in each.

N_DECOMP_BINS <- 8
decomp_vars <- c("log.cv", names(PIECES))

two_stage_piece_summary <- function(df) {
  owner_means <- df %>%
    group_by(File.Number) %>%
    summarise(across(all_of(decomp_vars), mean), .groups = "drop")
  out <- tibble(n = nrow(df), n.owners = nrow(owner_means), mean.Phi = mean(df$Phi))
  for (v in decomp_vars) {
    out[[paste0(v, ".mean")]] <- mean(owner_means[[v]])
    out[[paste0(v, ".se")]]   <- sd(owner_means[[v]]) / sqrt(nrow(owner_means))
  }
  out
}

pieces_by_phi <- function(data, specialist_col, grain) {
  map_dfr(names(MODES), function(md) {
    mode_data <- data %>% filter(mode == md)
    specialists <- mode_data %>%
      filter(.data[[specialist_col]]) %>%
      two_stage_piece_summary() %>%
      mutate(bin.label = "Specialists", bin.order = 0)
    multi <- mode_data %>%
      filter(!.data[[specialist_col]]) %>%
      mutate(phi.bin = ntile(Phi, N_DECOMP_BINS)) %>%
      group_by(phi.bin) %>%
      group_modify(~ two_stage_piece_summary(.x)) %>%
      ungroup() %>%
      mutate(bin.label = paste0("Q", phi.bin), bin.order = phi.bin) %>%
      select(-phi.bin)
    bind_rows(specialists, multi) %>% mutate(mode = md)
  }) %>%
    mutate(grain = grain) %>%
    relocate(grain, mode, bin.label, bin.order)
}

cv_decomp_owner_by_phi <- bind_rows(
  pieces_by_phi(cv_decomp_owner_lifetime, "is.specialist", "lifetime"),
  pieces_by_phi(cv_decomp_owner.rolling, "is.specialist.window", "rolling")
)

print(cv_decomp_owner_by_phi %>%
        mutate(across(ends_with(".mean"), exp, .names = "geo.{.col}")) %>%
        select(grain, mode, bin.label, n, mean.Phi, starts_with("geo.")),
      n = Inf, width = Inf)

write.csv(cv_decomp_owner_by_phi, file.path(table_dir, "cv_decomposition_by_phi_owner.csv"), row.names = FALSE)
cat("Wrote cv_decomposition_by_phi_owner.csv\n")

# Figure, rolling grain, one panel per factor with both versions overlaid.
# Realized CV and fishery riskiness are the same in both by construction, so
# their two series sit on top of each other, and the spell effect is 1 with
# zeros. The dashed line at 1 marks no effect for the three multiplier
# factors.

FACET_LABELS <- c(
  "Realized CV",
  "Fishery riskiness (U fleet, all years)",
  "Spell effect (U fleet, spells / all years)",
  "Owner-specific factor (U / U fleet)",
  "Third factor (CV / U)"
)
MODE_COLORS <- c("#2a78d6", "#eb6834")

fig_decomp_data <- cv_decomp_owner_by_phi %>%
  filter(grain == "rolling") %>%
  select(mode, bin.label, bin.order, ends_with(".mean"), ends_with(".se")) %>%
  pivot_longer(-c(mode, bin.label, bin.order), names_to = c("quantity", ".value"),
               names_pattern = "(.*)\\.(mean|se)$") %>%
  mutate(
    geo = exp(mean),
    lo  = exp(mean - 1.96 * se),
    hi  = exp(mean + 1.96 * se),
    quantity = factor(quantity, levels = decomp_vars, labels = FACET_LABELS),
    bin.label = factor(bin.label, levels = c("Specialists", paste0("Q", seq_len(N_DECOMP_BINS))),
                       labels = c("Specialists\n(Phi = 0)", paste0("Q", seq_len(N_DECOMP_BINS)))),
    mode = factor(mode, levels = names(MODES), labels = MODES),
    is.specialist.bin = bin.order == 0
  )

ref_lines <- tibble(quantity = factor(FACET_LABELS[3:5], levels = FACET_LABELS), y = 1)
dodge <- position_dodge(width = 0.45)

figure_cv_decomp_owner <- fig_decomp_data %>%
  ggplot(aes(x = bin.label, y = geo, color = mode, shape = mode)) +
  geom_hline(data = ref_lines, aes(yintercept = y), linetype = "dashed", color = "gray60",
             inherit.aes = FALSE) +
  geom_line(data = fig_decomp_data %>% filter(!is.specialist.bin), aes(group = mode), position = dodge) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, position = dodge) +
  geom_point(size = 2.2, position = dodge) +
  # The line layer has no specialists and would otherwise set the x order,
  # pushing the specialist point to the far right
  scale_x_discrete(limits = levels(fig_decomp_data$bin.label)) +
  scale_color_manual(values = setNames(MODE_COLORS, MODES), name = NULL) +
  scale_shape_manual(values = setNames(c(16, 17), MODES), name = NULL) +
  facet_wrap(~quantity, ncol = 3, scales = "free_y") +
  labs(
    # The identity and the factor definitions belong in the caption.
    title = "Realized CV and its factors (owner, rolling)",
    subtitle = "Geometric means by reallocation intensity, two-stage owner-clustered 95% CIs",
    x = "Reallocation intensity (Phi), specialists then increasing bins",
    y = "Geometric mean"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(file.path(figure_dir, "figure_cv_decomposition_by_phi_rolling_owner.png"),
       figure_cv_decomp_owner, width = 12, height = 7, dpi = 300)

cat("Wrote figure_cv_decomposition_by_phi_rolling_owner.png\n")

# ============================================================================
# 9. Save
# ============================================================================

cv_decomp_owner_path <- file.path(intermediate_dir, "ch3_cv_decomposition_owner.rdata")
save(cv_decomp_owner_lifetime, cv_decomp_owner.rolling, shares_tbl_owner, geo_means_tbl_owner,
     mode_comparison_owner, dropped_tbl_owner, cv_decomp_owner_by_phi,
     file = cv_decomp_owner_path)
cat("Saved owner CV decomposition objects to", cv_decomp_owner_path, "\n")
