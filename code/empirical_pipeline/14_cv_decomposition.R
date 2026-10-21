# Chapter 3 empirical pipeline, exact decomposition of realized revenue
# variability into fishery riskiness, vessel-specific volatility, and
# diversification
#
# table_cv_decomposition.tex. Each piece's share of the variance in log CV
#   across vessels (lifetime) and vessel-windows (rolling), plus the typical
#   (geometric mean) size of each piece.
# figure_cv_decomposition_by_phi_rolling.png. The pieces by Phi bin at the
#   rolling grain, on the same binning rule as Figure 3b-rolling.
# cv_decomposition_shares.csv, cv_decomposition_by_phi.csv. The same numbers
#   as plain aggregates, safe to bring off the server.
#
# THE IDENTITY. For vessel i over a set of active years (its whole panel at
# the lifetime grain, or one window's active years at the rolling grain),
# with y_ijt its revenue in fishery j in year t and R_it = sum_j y_ijt,
#
#   CV_i = sd(R_i) / mean(R_i)
#   U_i  = sum_j sd(y_ij) / mean(R_i) = sum_j omega_ij * CV_ij
#   DR_i = CV_i / U_i = sd(R_i) / sum_j sd(y_ij)
#
# where omega_ij = mean(y_ij) / mean(R_i) is fishery j's share of the
# vessel's mean revenue and CV_ij is the CV of its revenue in j, counting a
# zero for any active year it did not fish j. U_i is the CV the vessel would
# have if all its fishery revenue streams moved in lockstep. DR_i can never
# exceed 1, because the standard deviation of a sum is at most the sum of
# the standard deviations, so 1 - DR_i is the part of U_i that cancels out
# because the vessel's fisheries do not move together. That is the
# diversification benefit, measured directly rather than through HHI.
#
# U_i is split once more by swapping the vessel's own fishery CVs for
# fishery-level ones,
#
#   U_fleet_i = sum_j omega_ij * CV_fleet_ij
#
# where CV_fleet_ij is the CV, over the vessel's same active years, of fleet
# mean revenue per active vessel in fishery j (see Section 2). Then
#
#   log CV_i = log U_fleet_i + log(U_i / U_fleet_i) + log DR_i
#
# holds exactly for every vessel, and the three pieces are
#   fishery riskiness   log U_fleet_i, how volatile the fisheries the vessel
#                       is in are at the fishery level, weighted by its own
#                       revenue mix
#   vessel-specific     log(U_i / U_fleet_i), how much more volatile the
#                       vessel's own revenue in those fisheries is than the
#                       fishery-level series. This includes catch luck and
#                       skill but also entry and exit, since a year the
#                       vessel skipped a fishery counts as a zero in CV_ij
#   diversification     log DR_i, how much the vessel's fishery revenue
#                       streams offset each other
#
# VARIANCE SHARES. Because the pieces add to log CV exactly, Var(log CV) =
# sum_k Cov(piece_k, log CV). The slope from regressing a piece on log CV is
# therefore that piece's share of the variance in log CV, and the three
# slopes sum to 1 (the device Eaton, Kortum and Kramarz 2004 use to split
# trade into its margins). With fixed effects the same holds for the
# variance left after the fixed effects are removed. A share can be negative
# or above 1. A negative share means that piece tends to be smaller for
# vessels with higher CV, so it offsets the other two.
#
# Single-fishery specialists fit the identity with DR_i = 1 (one revenue
# stream, nothing to offset), so unlike Table 4 they do not need to be
# dropped. Versions with and without them are both reported.
#
# Reads intermediate data/ch3_panel.rdata (01_build_panel.R) and
# intermediate data/ch3_rolling.rdata (rolling_periods/01b_build_rolling_panel.R).
# Saves the vessel-level and vessel-window-level pieces to
# intermediate data/ch3_cv_decomposition.rdata for later use.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_fishery_year") || !exists("vessel_year") || !exists("fleet_mean_revenue") ||
    !exists("vessel_summary") || !exists("vessel_mean_share")) {
  load(panel_path)
}

rolling_panel_path <- file.path(intermediate_dir, "ch3_rolling.rdata")
if (!exists("vessel_window_summary.rolling") || !exists("vessel_year_window_eligible.rolling")) {
  load(rolling_panel_path)
}

PIECES <- c(
  p.fishery = "Fishery riskiness",
  p.vessel  = "Vessel-specific volatility",
  p.divers  = "Diversification"
)

# ============================================================================
# 1. Inputs shared by both grains
# ============================================================================
#
# Rows with exactly zero revenue (held but not fished) add nothing to any sum
# or standard deviation below, so they are dropped up front. Negative-revenue
# rows (correction or refund tickets, see the hhi check in 01b_) are kept,
# because vessel.year.rev includes them and the identity needs the fishery
# series to add up to R_it exactly.

fishery_revenue_rows <- as.data.table(vessel_fishery_year)[
  !is.na(revenue) & revenue != 0,
  .(Vessel.ADFG.Number, Batch.Year, Fishery, revenue, fished)
]
stopifnot(!anyDuplicated(fishery_revenue_rows, by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")))

fleet_stats <- as.data.table(fleet_mean_revenue)[, .(Batch.Year, Fishery, fleet_mean_revenue, n_active)]

# ============================================================================
# 2. The decomposition, for any unit (vessel or vessel-window)
# ============================================================================
#
# THE FISHERY-LEVEL SERIES. For vessel i, fishery j and active year t it is
# fleet mean revenue per active vessel in (j, t), the same fleet_mean_revenue
# the passive benchmark uses, with three cases.
#   (1) Nobody in the fleet fished j in t. The series is 0. A fishery-wide
#       closure is risk that belongs to the fishery rather than the vessel,
#       so it counts toward fishery riskiness.
#   (2) Vessel i fished j in t alongside others. Vessel i is left out of the
#       mean, so a vessel that is a large part of a small fishery does not
#       pass its own volatility into the fishery-level series.
#   (3) Vessel i was the only one to fish j in t. Leaving it out leaves
#       nothing, so the series falls back to the full mean, which is the
#       vessel's own revenue. That fishery-year's volatility then counts as
#       fishery riskiness rather than vessel-specific volatility. How much
#       weight this carries is printed below.
# If vessel i did not fish j in t but others did, the full mean is used as
# is, since vessel i is not in it.
#
# omega_ij is floored at 0 when building U_fleet. A fishery whose only
# revenue in the unit's years is negative has no meaningful revenue share,
# but its revenue still moves R_it, so it stays in U_i and DR_i. The floor
# only moves weight between the fishery and vessel-specific pieces, it never
# affects whether the identity holds.

cv_decompose <- function(unit_years, unit_cols, label) {
  unit_years <- as.data.table(unit_years)[, c(unit_cols, "Batch.Year", "vessel.year.rev"), with = FALSE]

  # Every fishery the unit has nonzero revenue in during its own active years
  own <- merge(unit_years[, c(unit_cols, "Batch.Year"), with = FALSE], fishery_revenue_rows,
               by = c("Vessel.ADFG.Number", "Batch.Year"), allow.cartesian = TRUE)
  portfolio <- unique(own[, c(unit_cols, "Fishery"), with = FALSE])

  # One row per unit, portfolio fishery and active year, zero-filled
  grid <- merge(portfolio, unit_years, by = unit_cols, allow.cartesian = TRUE)
  grid <- merge(grid, own[, c(unit_cols, "Batch.Year", "Fishery", "revenue", "fished"), with = FALSE],
                by = c(unit_cols, "Batch.Year", "Fishery"), all.x = TRUE)
  grid[is.na(revenue), `:=`(revenue = 0, fished = FALSE)]

  grid <- merge(grid, fleet_stats, by = c("Batch.Year", "Fishery"), all.x = TRUE)
  grid[, loo.fallback := fished & !is.na(n_active) & n_active == 1]
  grid[, fishery.series := fcase(
    is.na(fleet_mean_revenue), 0,
    fished & n_active > 1, (fleet_mean_revenue * n_active - revenue) / (n_active - 1),
    default = fleet_mean_revenue
  )]

  # The fishery series must add up to R_it in every unit-year, or DR_i is
  # not bounded by 1 and the identity is not an identity
  year_check <- grid[, .(sum.y = sum(revenue), R = vessel.year.rev[1]), by = c(unit_cols, "Batch.Year")]
  max_rel_gap <- year_check[, max(abs(sum.y - R) / R)]

  by_fishery <- grid[, .(
    mean.y     = mean(revenue),
    sd.y       = sd(revenue),
    mean.f     = mean(fishery.series),
    sd.f       = sd(fishery.series),
    n.fallback = sum(loo.fallback),
    n.years    = .N
  ), by = c(unit_cols, "Fishery")]

  totals <- unit_years[, .(mean.R = mean(vessel.year.rev), sd.R = sd(vessel.year.rev), n.years = .N),
                       by = unit_cols]

  by_fishery <- merge(by_fishery, totals[, c(unit_cols, "mean.R"), with = FALSE], by = unit_cols)
  by_fishery[, omega.pos := pmax(mean.y, 0) / mean.R]
  by_fishery[, fleet.term := fifelse(omega.pos > 0, omega.pos * sd.f / mean.f, 0)]
  by_fishery[, fallback.weight := omega.pos * n.fallback / n.years]

  units <- by_fishery[, .(
    sum.sd.y        = sum(sd.y),
    U.fleet         = sum(fleet.term),
    n.fisheries     = .N,
    fallback.weight = sum(fallback.weight)
  ), by = unit_cols]
  units <- merge(totals, units, by = unit_cols)
  units[, `:=`(CV = sd.R / mean.R, U = sum.sd.y / mean.R, DR = sd.R / sum.sd.y)]
  units[, `:=`(
    log.cv    = log(CV),
    p.fishery = log(U.fleet),
    p.vessel  = log(U / U.fleet),
    p.divers  = log(DR)
  )]

  cat("\n===== CV decomposition,", label, "=====\n")
  cat("Units:", nrow(units), " unit-fishery rows:", nrow(by_fishery), "\n")
  cat("Max relative gap between summed fishery revenue and vessel.year.rev (should be ~0):",
      signif(max_rel_gap, 3), "\n")
  cat("Max DR (must not exceed 1):", signif(max(units$DR, na.rm = TRUE), 6), "\n")
  cat("Mean revenue weight on fishery-years where the vessel was the only participant",
      "(leave-one-out fallback):", round(mean(units$fallback.weight), 4),
      " share of units with any:", round(mean(units$fallback.weight > 0), 4), "\n")

  as_tibble(units)
}

# Drops units where a piece is not finite (a constant revenue series, or a
# fishery-level series that never moves) and says how many went
keep_finite_pieces <- function(df, label) {
  ok <- with(df, is.finite(log.cv) & is.finite(p.fishery) & is.finite(p.vessel) & is.finite(p.divers))
  cat(label, "units dropped for a non-finite piece:", sum(!ok), "of", nrow(df), "\n")
  df[ok, ]
}

# ============================================================================
# 3. Lifetime grain, the same sample as Table 4 (05_table4_figure3.R)
# ============================================================================

active_vessel_years <- vessel_year %>%
  filter(vessel.year.rev > 0) %>%
  select(Vessel.ADFG.Number, Batch.Year, vessel.year.rev)

n_fisheries_fished <- vessel_mean_share %>%
  count(Vessel.ADFG.Number, name = "n.fisheries.fished")

vessel_analysis <- vessel_summary %>%
  filter(meets.min.years, is.finite(rev.cv), !is.na(prime.fishery)) %>%
  left_join(n_fisheries_fished, by = "Vessel.ADFG.Number") %>%
  mutate(is.specialist = n.fisheries.fished == 1) %>%
  select(Vessel.ADFG.Number, rev.cv, H_LR, Phi, is.specialist, prime.fishery)

cv_decomp_lifetime <- cv_decompose(
  active_vessel_years %>% semi_join(vessel_analysis, by = "Vessel.ADFG.Number"),
  "Vessel.ADFG.Number", "lifetime"
) %>%
  inner_join(vessel_analysis, by = "Vessel.ADFG.Number")

cat("Max |CV - rev.cv| against vessel_summary (should be ~0):",
    signif(max(abs(cv_decomp_lifetime$CV - cv_decomp_lifetime$rev.cv)), 3), "\n")
cat("Specialists with DR = 1 (should be ~1, below that means a negative-revenue row in a second fishery):",
    round(mean(abs(cv_decomp_lifetime$DR[cv_decomp_lifetime$is.specialist] - 1) < 1e-9), 4), "\n")

cv_decomp_lifetime <- keep_finite_pieces(cv_decomp_lifetime, "Lifetime")

# ============================================================================
# 4. Rolling grain, the same sample as Table 4-rolling (05b_)
# ============================================================================

vessel_analysis.rolling <- vessel_window_summary.rolling %>%
  filter(is.finite(rev.cv)) %>%
  select(Vessel.ADFG.Number, window.start, rev.cv, H_LR, Phi, is.specialist.window, prime.fishery.window)

cv_decomp.rolling <- cv_decompose(
  vessel_year_window_eligible.rolling %>%
    semi_join(vessel_analysis.rolling, by = c("Vessel.ADFG.Number", "window.start")),
  c("Vessel.ADFG.Number", "window.start"), "rolling"
) %>%
  inner_join(vessel_analysis.rolling, by = c("Vessel.ADFG.Number", "window.start"))

cat("Max |CV - rev.cv| against vessel_window_summary.rolling (should be ~0):",
    signif(max(abs(cv_decomp.rolling$CV - cv_decomp.rolling$rev.cv)), 3), "\n")
cat("Window specialists with DR = 1 (should be ~1):",
    round(mean(abs(cv_decomp.rolling$DR[cv_decomp.rolling$is.specialist.window] - 1) < 1e-9), 4), "\n")

cv_decomp.rolling <- keep_finite_pieces(cv_decomp.rolling, "Rolling")

# ============================================================================
# 5. Variance shares
# ============================================================================
#
# Five columns. The rolling columns carry a window fixed effect, the same
# delta_s Table 4-rolling uses, so a fleet-wide bad stretch of years does not
# count as a difference between vessels. The last column adds a vessel fixed
# effect, so its shares describe why the same vessel's CV differs across
# windows of its own career rather than why vessels differ from each other.
# There is no primary-fishery fixed effect anywhere here, because which
# fishery a vessel is in is exactly what the fishery riskiness piece measures.

variance_shares <- function(data, column, fe, vcov_spec) {
  rhs <- if (is.null(fe)) "log.cv" else paste("log.cv |", fe)
  map_dfr(names(PIECES), function(p) {
    m <- feols(as.formula(paste(p, "~", rhs)), data = data, vcov = vcov_spec)
    tibble(column = column, piece = PIECES[[p]],
           share = unname(coef(m)["log.cv"]), se = unname(se(m)["log.cv"]), n = nobs(m))
  })
}

lifetime_multi <- cv_decomp_lifetime %>% filter(!is.specialist)
rolling_multi  <- cv_decomp.rolling %>% filter(!is.specialist.window)
twoway_cluster <- ~Vessel.ADFG.Number + window.start

decomp_columns <- list(
  list(column = "Lifetime, multi", data = lifetime_multi, fe = NULL,
       vcov = "hetero", fe.label = "None"),
  list(column = "Lifetime, all", data = cv_decomp_lifetime, fe = NULL,
       vcov = "hetero", fe.label = "None"),
  list(column = "Rolling, multi", data = rolling_multi, fe = "window.start",
       vcov = twoway_cluster, fe.label = "Window"),
  list(column = "Rolling, all", data = cv_decomp.rolling, fe = "window.start",
       vcov = twoway_cluster, fe.label = "Window"),
  list(column = "Rolling, multi, vessel FE", data = rolling_multi, fe = "Vessel.ADFG.Number + window.start",
       vcov = twoway_cluster, fe.label = "Vessel and window")
)

shares_tbl <- map_dfr(decomp_columns, ~ variance_shares(.x$data, .x$column, .x$fe, .x$vcov))

# Typical size of each factor. The geometric means multiply out to the
# geometric mean of realized CV exactly, because the logs add.
geo_means_tbl <- map_dfr(decomp_columns, function(cl) {
  cl$data %>%
    summarise(
      geo.cv        = exp(mean(log.cv)),
      geo.U.fleet   = exp(mean(p.fishery)),
      geo.vessel    = exp(mean(p.vessel)),
      geo.DR        = exp(mean(p.divers)),
      mean.one.m.DR = mean(1 - DR)
    ) %>%
    mutate(column = cl$column, fixed.effects = cl$fe.label)
})

print(shares_tbl, n = Inf, width = Inf)
cat("Sum of the three shares by column (must be 1):\n")
print(shares_tbl %>% group_by(column) %>% summarise(sum.share = sum(share), .groups = "drop"))
print(geo_means_tbl, width = Inf)

write.csv(
  shares_tbl %>% left_join(geo_means_tbl, by = "column"),
  file.path(table_dir, "cv_decomposition_shares.csv"), row.names = FALSE
)
cat("Wrote cv_decomposition_shares.csv\n")

# LaTeX table, one column per specification
column_order <- map_chr(decomp_columns, "column")

share_rows <- shares_tbl %>%
  mutate(cell = sprintf("%.3f (%.3f)", share, se)) %>%
  select(piece, column, cell) %>%
  pivot_wider(names_from = column, values_from = cell) %>%
  rename(Statistic = piece)

sum_row <- shares_tbl %>%
  group_by(column) %>%
  summarise(cell = sprintf("%.3f", sum(share)), .groups = "drop") %>%
  pivot_wider(names_from = column, values_from = cell) %>%
  mutate(Statistic = "Sum of shares")

geo_rows <- geo_means_tbl %>%
  transmute(
    column,
    `Fixed effects`                              = fixed.effects,
    `Geometric mean, fishery riskiness factor`   = sprintf("%.3f", geo.U.fleet),
    `Geometric mean, vessel-specific factor`     = sprintf("%.3f", geo.vessel),
    `Geometric mean, diversification factor`     = sprintf("%.3f", geo.DR),
    `Geometric mean, realized CV`                = sprintf("%.3f", geo.cv)
  ) %>%
  pivot_longer(-column, names_to = "Statistic", values_to = "cell") %>%
  pivot_wider(names_from = column, values_from = cell)

n_row <- shares_tbl %>%
  distinct(column, n) %>%
  mutate(cell = formatC(n, format = "d", big.mark = ",")) %>%
  select(column, cell) %>%
  pivot_wider(names_from = column, values_from = cell) %>%
  mutate(Statistic = "Observations")

cv_decomp_table <- bind_rows(share_rows, sum_row, geo_rows, n_row) %>%
  select(Statistic, all_of(column_order))

print(xtable(
  cv_decomp_table,
  caption = paste(
    "Exact decomposition of realized revenue CV into fishery riskiness, vessel-specific volatility",
    "and diversification. The first three rows give each piece's share of the variance in log CV",
    "across vessels (lifetime) or vessel-windows (rolling) after removing the listed fixed effects,",
    "with standard errors in parentheses (heteroskedasticity-robust for lifetime, clustered on vessel",
    "and window for rolling). The geometric-mean rows give the typical size of each factor, and their",
    "product is the geometric mean of realized CV. A diversification factor of 0.8 means offsetting",
    "fishery revenue streams leave CV 20 percent below what it would be if they moved in lockstep."
  ),
  label = "tab:ch3-cv-decomposition",
  align = c("l", "l", rep("r", length(column_order)))
),
file = file.path(table_dir, "table_cv_decomposition.tex"),
include.rownames = FALSE, size = "\\small")

cat("Wrote table_cv_decomposition.tex\n")

# ============================================================================
# 6. The pieces by Phi bin
# ============================================================================
#
# Same binning rule as Figure 3b-rolling (specialists as their own point,
# multi-fishery units in 8 Phi quantiles) and the same two-stage averaging
# (mean within vessel inside a bin, then across vessels), so a vessel with
# many overlapping windows in one bin counts once. At the lifetime grain
# each vessel has one row, so this is a plain mean. Means are of the log
# pieces, so exponentiated they are geometric means that multiply out to the
# bin's geometric-mean CV.

N_DECOMP_BINS <- 8
decomp_vars <- c("log.cv", names(PIECES))

two_stage_piece_summary <- function(df) {
  vessel_means <- df %>%
    group_by(Vessel.ADFG.Number) %>%
    summarise(across(all_of(decomp_vars), mean), .groups = "drop")
  out <- tibble(n = nrow(df), n.vessels = nrow(vessel_means), mean.Phi = mean(df$Phi))
  for (v in decomp_vars) {
    out[[paste0(v, ".mean")]] <- mean(vessel_means[[v]])
    out[[paste0(v, ".se")]]   <- sd(vessel_means[[v]]) / sqrt(nrow(vessel_means))
  }
  out
}

pieces_by_phi <- function(data, specialist_col, grain) {
  specialists <- data %>%
    filter(.data[[specialist_col]]) %>%
    two_stage_piece_summary() %>%
    mutate(bin.label = "Specialists", bin.order = 0)
  multi <- data %>%
    filter(!.data[[specialist_col]]) %>%
    mutate(phi.bin = ntile(Phi, N_DECOMP_BINS)) %>%
    group_by(phi.bin) %>%
    group_modify(~ two_stage_piece_summary(.x)) %>%
    ungroup() %>%
    mutate(bin.label = paste0("Q", phi.bin), bin.order = phi.bin) %>%
    select(-phi.bin)
  bind_rows(specialists, multi) %>%
    mutate(grain = grain) %>%
    relocate(grain, bin.label, bin.order)
}

cv_decomp_by_phi <- bind_rows(
  pieces_by_phi(cv_decomp_lifetime, "is.specialist", "lifetime"),
  pieces_by_phi(cv_decomp.rolling, "is.specialist.window", "rolling")
)

print(cv_decomp_by_phi %>%
        mutate(across(ends_with(".mean"), exp, .names = "geo.{.col}")) %>%
        select(grain, bin.label, n, mean.Phi, starts_with("geo.")),
      n = Inf, width = Inf)

write.csv(cv_decomp_by_phi, file.path(table_dir, "cv_decomposition_by_phi.csv"), row.names = FALSE)
cat("Wrote cv_decomposition_by_phi.csv\n")

# Figure, rolling grain. One panel per factor with its own y scale, since
# the factors sit on very different scales. The dashed line at 1 marks no
# effect for the two multiplier factors (a vessel no more volatile than its
# fisheries, and no offsetting across fisheries).

FACET_LABELS <- c(
  "Realized CV",
  "Fishery riskiness (U fleet)",
  "Vessel-specific factor (U / U fleet)",
  "Diversification factor (DR)"
)

fig_decomp_data <- cv_decomp_by_phi %>%
  filter(grain == "rolling") %>%
  select(bin.label, bin.order, ends_with(".mean"), ends_with(".se")) %>%
  pivot_longer(-c(bin.label, bin.order), names_to = c("quantity", ".value"),
               names_pattern = "(.*)\\.(mean|se)$") %>%
  mutate(
    geo = exp(mean),
    lo  = exp(mean - 1.96 * se),
    hi  = exp(mean + 1.96 * se),
    quantity = factor(quantity, levels = decomp_vars, labels = FACET_LABELS),
    bin.label = factor(bin.label, levels = c("Specialists", paste0("Q", seq_len(N_DECOMP_BINS))),
                       labels = c("Specialists\n(Phi = 0)", paste0("Q", seq_len(N_DECOMP_BINS)))),
    is.specialist.bin = bin.order == 0
  )

ref_lines <- tibble(quantity = factor(FACET_LABELS[3:4], levels = FACET_LABELS), y = 1)

figure_cv_decomp <- fig_decomp_data %>%
  ggplot(aes(x = bin.label, y = geo, color = is.specialist.bin)) +
  geom_hline(data = ref_lines, aes(yintercept = y), linetype = "dashed", color = "gray60",
             inherit.aes = FALSE) +
  geom_point(size = 2.2) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  geom_line(
    data = fig_decomp_data %>% filter(!is.specialist.bin),
    aes(x = bin.label, y = geo, group = quantity), color = "steelblue", inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("TRUE" = "gray40", "FALSE" = "steelblue"), guide = "none") +
  facet_wrap(~quantity, ncol = 2, scales = "free_y") +
  labs(
    # The identity and the factor definitions belong in the caption.
    title = "Realized CV and its three factors (rolling)",
    subtitle = "Geometric means by reallocation intensity, two-stage vessel-clustered 95% CIs",
    x = "Reallocation intensity (Phi), specialists then increasing bins",
    y = "Geometric mean"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure_cv_decomposition_by_phi_rolling.png"),
       figure_cv_decomp, width = 9, height = 6.5, dpi = 300)

cat("Wrote figure_cv_decomposition_by_phi_rolling.png\n")

# ============================================================================
# 7. Save
# ============================================================================

cv_decomp_path <- file.path(intermediate_dir, "ch3_cv_decomposition.rdata")
save(cv_decomp_lifetime, cv_decomp.rolling, shares_tbl, geo_means_tbl, cv_decomp_by_phi,
     file = cv_decomp_path)
cat("Saved CV decomposition objects to", cv_decomp_path, "\n")
