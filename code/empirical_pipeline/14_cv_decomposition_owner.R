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
# FOUR FACTORS IN THE "ACTIVE" VERSION. Taken over only the years the owner
# was active in each fishery, U_fleet_i would depend on the owner's own entry and
# exit. A few active years look calm, a single year looks riskless, and
# closures drop out because the owner cannot fish a closed fishery. So fishery
# riskiness is always the fleet's CV over all the unit's active years
# (U_fleet_all_i, the "zero" version's U_fleet_i), and the gap to the
# years-active U_fleet_i gets its own factor,
#
#   CV_i = U_fleet_all_i * (U_fleet_i / U_fleet_all_i) * (U_i / U_fleet_i) * (CV_i / U_i)
#
# The new second factor (the years-active effect) is below 1 when the fleet
# looks calmer in the years the owner was active in each fishery than over all its
# years, because those years are few, miss closures or fall in the fishery's
# calmer years (the three cannot be told apart here). In the "zero" version
# both fleet terms cover the same years, so the years-active effect is exactly
# 1 and the split is the three factors of 14_. The years-active effect only
# cleans the fishery factor. The owner's own CVs are still taken over its
# years active in each fishery, so any short-series effect on them stays in the
# third factor.
#
# Two things about the "active" version worth knowing before reading it.
# (1) A fishery fished in only one of a unit's active years has no CV of its
# own. Its CV_ij and CV_fleet_ij are set to 0, so the owner's side of its risk
# lands in the third factor and the fleet's side in the years-active effect.
# The revenue weight on fisheries fished in only one year, and in only two
# (whose CV rests on two numbers), is printed and saved. (2) A fishery-wide
# closure still counts as fishery riskiness, but the years-active effect takes
# it back out, since the owner did not fish j that year either. Its effect on
# R_it lands in the third factor.
#
# THE COMPARISON. Both versions are computed on the same units, so realized CV
# and its variance are identical in the two, and only the split differs. A
# unit enters only if every piece is finite in both versions. The "active"
# version loses any unit that fished each of its fisheries in only one year (U_i = 0),
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
# off the server). Section 8b adds the fleet-swapped version, the short-stretch
# check and a fleet-size check (cv_decomposition_swap_owner.csv,
# cv_decomposition_swap_by_phi_owner.csv,
# cv_decomposition_short_stretch_owner.csv and
# cv_decomposition_fleet_size_owner.csv, all aggregates). Saves the owner-level
# and owner-window-level pieces to
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

MODES <- c(zero = "Skipped years as zeros", active = "Skipped years left out")

PIECES <- c(
  p.fishery = "Fishery riskiness",
  p.years   = "Years-active effect",
  p.owner   = "Owner-specific volatility",
  p.combine = "Third factor"
)

# The years-active effect is exactly 1 with zeros, so that version's shares and table
# leave it out
mode_pieces <- function(mode) if (mode == "zero") setdiff(names(PIECES), "p.years") else names(PIECES)

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
# active by definition, so the years-active fleet CV never sees a closure zero.
# The all-years fleet CV behind fishery riskiness does.

# Short-stretch check (Section 8b). Takes every unit-fishery that the unit
# fished in all n of its active years, recomputes the owner's CV and the
# fleet's CV over every subset of k of those years, and divides each by its CV
# over all n. The mean of that ratio is how far a stretch of k years shrinks a
# CV by itself, for the owner's own revenue and for the fleet series.
# Consecutive subsets match entering or leaving a fishery partway through.
# A single year has a CV of 0 and all n years a ratio of 1, so k runs from 2
# to n - 1. Only run on the rolling grain, where n is at most 6.
stretch_shrinkage <- function(grid, by_fishery, unit_cols) {
  full <- by_fishery[n.fished == n.years & n.years >= 3, c(unit_cols, "Fishery", "n.years"), with = FALSE]
  g <- merge(grid[fished == TRUE, c(unit_cols, "Fishery", "Batch.Year", "revenue", "fishery.series"), with = FALSE],
             full, by = c(unit_cols, "Fishery"))
  setorderv(g, c(unit_cols, "Fishery", "Batch.Year"))
  res <- list()
  for (n in sort(unique(g$n.years))) {
    gn <- g[n.years == n]
    sides <- list(owner = matrix(gn$revenue, ncol = n, byrow = TRUE),
                  fleet = matrix(gn$fishery.series, ncol = n, byrow = TRUE))
    masks <- as.matrix(expand.grid(rep(list(0:1), n)))
    masks <- masks[rowSums(masks) >= 2 & rowSums(masks) < n, , drop = FALSE]
    contiguous <- apply(masks, 1, function(b) all(diff(which(b == 1)) == 1))
    for (side in names(sides)) {
      x <- sides[[side]]
      m_full <- rowMeans(x)
      cv_full <- sqrt(rowSums((x - m_full)^2) / (n - 1)) / m_full
      ok <- is.finite(cv_full) & cv_full > 0 & m_full > 0
      for (k in 2:(n - 1)) {
        in_k <- rowSums(masks) == k
        S <- t(masks[in_k, , drop = FALSE])
        mk <- (x[ok, , drop = FALSE] %*% S) / k
        vk <- ((x[ok, , drop = FALSE]^2) %*% S - k * mk^2) / (k - 1)
        ratio <- (sqrt(pmax(vk, 0)) / mk) / cv_full[ok]
        for (type in c("consecutive", "any")) {
          cols <- if (type == "consecutive") contiguous[in_k] else rep(TRUE, sum(in_k))
          r <- rowMeans(ratio[, cols, drop = FALSE])
          r <- r[is.finite(r)]
          res[[length(res) + 1]] <- data.table(n.years = n, k = k, subset.type = type, side = side,
                                               n.pairs = length(r), mean.ratio = mean(r),
                                               median.ratio = median(r))
        }
      }
    }
  }
  if (!length(res)) return(NULL)
  table <- dcast(rbindlist(res), n.years + k + subset.type ~ side,
                 value.var = c("n.pairs", "mean.ratio", "median.ratio"))
  # Lookup for every stretch length a unit can have, 0 or 1 year (CV of 0)
  # through all n years (ratio 1), from the consecutive-years means
  lookup <- rbindlist(lapply(sort(unique(by_fishery$n.years)), function(n) {
    data.table(n.years = n, k = 0:n)
  }))
  lookup <- merge(lookup, table[subset.type == "consecutive",
                                .(n.years, k, s.owner = mean.ratio_owner, s.fleet = mean.ratio_fleet)],
                  by = c("n.years", "k"), all.x = TRUE)
  lookup[k <= 1, `:=`(s.owner = 0, s.fleet = 0)]
  lookup[k == n.years & k >= 2, `:=`(s.owner = 1, s.fleet = 1)]
  list(table = table, lookup = lookup)
}

cv_decompose_owner <- function(unit_years, unit_cols, label, stretch_check = FALSE) {
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

  # "active" version, over the years the unit fished j only. A fishery fished
  # in only one year gets a CV of 0 (see the header).
  fished_years <- grid[fished == TRUE, .(
    n.fished  = .N,
    mean.y.a = mean(revenue),
    sd.y.a   = sd(revenue),
    mean.f.a = mean(fishery.series),
    sd.f.a   = sd(fishery.series),
    log.n.active = mean(log(n_active))
  ), by = c(unit_cols, "Fishery")]
  by_fishery <- merge(by_fishery, fished_years, by = c(unit_cols, "Fishery"), all.x = TRUE)
  by_fishery[is.na(n.fished), n.fished := 0L]
  by_fishery[, `:=`(
    cv.y.active = fifelse(n.fished >= 2, sd.y.a / mean.y.a, 0),
    cv.f.active = fifelse(n.fished >= 2, sd.f.a / mean.f.a, 0)
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
    weight.1yr   = sum(omega.pos[n.fished == 1]),
    weight.2yr   = sum(omega.pos[n.fished == 2]),
    no.gaps         = all(n.fished == n.years),
    n.fisheries     = .N,
    fallback.weight = sum(fallback.weight)
  ), by = unit_cols]
  # Fleet-swapped series (Section 8b). In each year the unit fished j, its
  # revenue there is replaced by the fleet series, scaled so that its mean over
  # those years is the unit's own. Every other row (zero-filled years, and the
  # rare nonzero revenue in a year the unit did not fish j) is kept as it is.
  # The swapped total then has the unit's own mean, the same years in each
  # fishery and the same jumps between fisheries, but only fleet-level ups and
  # downs inside each fishery.
  swap <- merge(grid[, c(unit_cols, "Batch.Year", "Fishery", "revenue", "fished", "fishery.series"), with = FALSE],
                fished_years[, c(unit_cols, "Fishery", "mean.y.a", "mean.f.a"), with = FALSE],
                by = c(unit_cols, "Fishery"), all.x = TRUE)
  swap[, can.swap := fished & !is.na(mean.f.a) & mean.f.a > 0]
  swap[, revenue.swap := fifelse(can.swap, fishery.series * mean.y.a / mean.f.a, revenue)]
  n_not_swapped <- swap[fished & !can.swap, .N]
  swap_units <- swap[, .(R.swap = sum(revenue.swap), R = sum(revenue)), by = c(unit_cols, "Batch.Year")][
    , .(CV.swap = sd(R.swap) / mean(R.swap), swap.mean.gap = abs(mean(R.swap) / mean(R) - 1)),
    by = unit_cols]

  # Fleet size behind the unit's fleet series (Section 8b), the revenue-weighted
  # mean over its fisheries of the log number of active owners in the years it
  # fished each one (the owner itself included)
  fleet_size <- by_fishery[n.fished > 0 & omega.pos > 0,
                           .(log.fleet.size = sum(omega.pos * log.n.active) / sum(omega.pos)),
                           by = unit_cols]

  # Years-active effect and last factor that stretch length alone would give
  # (Section 8b). The fleet's CVs over all years stand in for the owner's,
  # which are not seen for years it did not fish a fishery.
  shrinkage <- NULL
  if (stretch_check) {
    shrinkage <- stretch_shrinkage(grid, by_fishery, unit_cols)
  }
  if (!is.null(shrinkage)) {
    len <- merge(by_fishery[, c(unit_cols, "n.years", "n.fished", "fleet.term"), with = FALSE],
                 shrinkage$lookup, by.x = c("n.years", "n.fished"), by.y = c("n.years", "k"), all.x = TRUE)
    length_only <- len[, .(YA.len = sum(fleet.term * s.fleet) / sum(fleet.term),
                           LF.len = sum(fleet.term) / sum(fleet.term * s.owner)), by = unit_cols]
  } else {
    length_only <- unique(by_fishery[, unit_cols, with = FALSE])[, `:=`(YA.len = NA_real_, LF.len = NA_real_)]
  }

  units <- merge(totals, units, by = unit_cols)
  units <- merge(units, swap_units, by = unit_cols, all.x = TRUE)
  units <- merge(units, fleet_size, by = unit_cols, all.x = TRUE)
  units <- as_tibble(merge(units, length_only, by = unit_cols, all.x = TRUE))

  # One row per unit and version. U.fleet covers the same years as U, and
  # U.fleet.all all the unit's active years, so with zeros the two are equal
  # and the years-active effect is exactly 1.
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
      p.years   = log(U.fleet / U.fleet.all),
      p.owner   = log(U / U.fleet),
      p.combine = log(combine)
    ) %>%
    select(all_of(unit_cols), mode, n.years, n.fisheries, CV, U.fleet.all, U.fleet, U, combine,
           log.cv, all_of(names(PIECES)), weight.1yr, weight.2yr, no.gaps, fallback.weight,
           CV.swap, YA.len, LF.len, log.fleet.size)

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
  cat("Mean revenue weight on fisheries fished in one year", round(mean(units$weight.1yr), 4),
      " in two years", round(mean(units$weight.2yr), 4), "\n")
  cat("Share of units with the active-years third factor above 1",
      round(mean(active_rows$combine > 1 + 1e-9, na.rm = TRUE), 4), "\n")
  cat("Geometric mean years-active effect in the active version (1 means the active years in each fishery see the same fleet",
      "risk as all years)", round(exp(mean(active_rows$p.years[is.finite(active_rows$p.years)])), 4), "\n")
  cat("Mean revenue weight on fishery-years where the owner was the only participant",
      "(leave-one-out fallback)", round(mean(units$fallback.weight), 4),
      " share of units with any", round(mean(units$fallback.weight > 0), 4), "\n")
  cat("Fleet-swapped series, max relative gap between its mean and the unit's own (should be about 0)",
      signif(max(units$swap.mean.gap, na.rm = TRUE), 3),
      " fished unit-fishery-years left unswapped because the fleet mean was not positive", n_not_swapped, "\n")

  list(long = long, shrinkage = if (is.null(shrinkage)) NULL else shrinkage$table)
}

# Keeps a unit only if every piece is finite in both versions, so the two are
# compared on the same units, and counts what went, for all units and for
# multi-fishery units alone
keep_common_units <- function(df, unit_cols, specialist_col, grain) {
  df <- df %>%
    mutate(ok = is.finite(log.cv) & is.finite(p.fishery) & is.finite(p.years) & is.finite(p.owner) &
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
)$long %>%
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

rolling_raw_owner <- cv_decompose_owner(
  owner_year_window_eligible.rolling %>%
    semi_join(owner_analysis.rolling, by = c("File.Number", "window.start")),
  c("File.Number", "window.start"), "owner rolling", stretch_check = TRUE
)
short_stretch_tbl_owner <- rolling_raw_owner$shrinkage
cv_decomp_owner.rolling <- rolling_raw_owner$long %>%
  inner_join(owner_analysis.rolling, by = c("File.Number", "window.start"))
rm(rolling_raw_owner)

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
# Specialists have a third factor of exactly 1, which rounding can put on
# either side of 1, so "above 1" uses a small tolerance.
geo_means_tbl_owner <- map_dfr(decomp_columns, function(cl) {
  cl$data %>%
    group_by(mode) %>%
    summarise(
      geo.cv         = exp(mean(log.cv)),
      geo.U.fleet    = exp(mean(p.fishery)),
      geo.years      = exp(mean(p.years)),
      geo.owner      = exp(mean(p.owner)),
      geo.combine    = exp(mean(p.combine)),
      share.combine.above.1 = mean(combine > 1 + 1e-9),
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
# dropped. The one-year and two-year weights say how much of the active-years
# version's third factor is forced there by fisheries fished in only one or two
# years rather than measured.

compare_modes <- function(cl, unit_cols) {
  wide <- cl$data %>%
    select(all_of(unit_cols), mode, p.owner, p.combine, combine, weight.1yr, weight.2yr) %>%
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
    share.active.combine.gt1 = mean(wide$combine_active > 1 + 1e-9),
    mean.weight.1yr          = mean(wide$weight.1yr),
    mean.weight.2yr          = mean(wide$weight.2yr),
    share.any.1yr            = mean(wide$weight.1yr > 0)
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
      `Geometric mean, years-active effect`             = sprintf("%.3f", geo.years),
      `Geometric mean, owner-specific factor`    = sprintf("%.3f", geo.owner),
      `Geometric mean, third factor (CV / U)`    = sprintf("%.3f", geo.combine)
    ) %>%
    pivot_longer(-column, names_to = "Statistic", values_to = "cell")
  if (!"p.years" %in% mode_pieces(md)) {
    geo_rows <- geo_rows %>% filter(Statistic != "Geometric mean, years-active effect")
  }

  bind_rows(share_rows, sum_row, geo_rows) %>%
    mutate(Statistic = factor(Statistic, levels = unique(Statistic))) %>%
    pivot_wider(names_from = column, values_from = cell) %>%
    mutate(Statistic = as.character(Statistic),
           `Skipped years` = c(MODES[[md]], rep("", n() - 1)))
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
  mutate(Statistic = as.character(Statistic), `Skipped years` = "")

cv_decomp_table_owner <- bind_rows(mode_rows, common_rows) %>%
  select(`Skipped years`, Statistic, all_of(column_order))

print(xtable(
  cv_decomp_table_owner,
  caption = paste(
    "Exact decomposition of realized owner revenue CV, with the years an owner did not fish a",
    "fishery counted as zeros (top) or left out (bottom). The share rows give each piece's share of",
    "the variance in log CV after removing the listed fixed effects, with standard errors in",
    "parentheses (heteroskedasticity-robust for lifetime, clustered on owner and window for",
    "rolling). Fishery riskiness is the fleet's CV over all of the owner's active years in both",
    "versions. With the years left out, the owner's CV in each fishery is taken over the years it",
    "fished there, and the years-active effect is the fleet's CV over those same years relative to all",
    "years (below 1 when those years are few, miss closures or fall in calmer years). With zeros",
    "the years-active effect is 1 and the third factor is the diversification ratio, which is at most 1.",
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

two_stage_piece_summary <- function(df, vars = decomp_vars) {
  owner_means <- df %>%
    group_by(File.Number) %>%
    summarise(across(all_of(vars), mean), .groups = "drop")
  out <- tibble(n = nrow(df), n.owners = nrow(owner_means), mean.Phi = mean(df$Phi))
  for (v in vars) {
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
# their two series sit on top of each other, and the years-active effect is 1 with
# zeros. The dashed line at 1 marks no effect for the three multiplier
# factors.

# Same names and symbols as the note (cv_decomposition_note.tex). With zeros
# the last factor is the diversification ratio, said in the note's caption.
FACET_LABELS <- c(
  "Realized CV",
  "Fishery riskiness (U fleet)",
  "Years-active effect (U fleet_A / U fleet)",
  "Owner-specific factor (U_A / U fleet_A)",
  "Combining and switching (CV / U_A)"
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
# 8b. Fleet-swapped version, short-stretch check and fleet size
# ============================================================================
#
# All three use the version with skipped years left out.
#
# THE FLEET-SWAPPED VERSION (cv_decomposition_note.tex, next steps). CV_swap
# is the CV of the unit's fleet-swapped total (built in Section 2). Its own CV
# in each fishery is the fleet's by construction, so its last factor
# CV_swap / U_fleet_A is what the unit's pattern of active years alone does
# (the jumps between fisheries plus fleet-level offsetting). Pairing the four
# factors gives a three-factor split,
#
#   CV = U_fleet * (CV_swap / U_fleet) * (CV / CV_swap)
#
# The middle factor (the pattern of active years) is the years-active effect
# times the fleet-swapped last factor. The last (the owner's own volatility)
# is the owner-specific factor times what is left of the last factor. U_A and
# U_fleet_A cancel inside each pair, and CV_swap and CV are both taken over all
# of the unit's active years, so short stretches in a fishery no longer move
# either factor through how a CV is measured.
#
# THE SHORT-STRETCH CHECK (rolling grain only). short_stretch_tbl_owner holds
# the mean shrinkage of a CV taken over k of a unit's n active years, for the
# owner's own revenue and for the fleet series (stretch_shrinkage, Section 2).
# Applying the consecutive-years shrinkage to each unit's own stretch lengths
# gives the years-active effect and the last factor that stretch length alone
# would produce (YA.len and LF.len). If the owner's and the fleet's CVs shrink
# alike, YA.len * LF.len is about 1, which is the assumption behind the note's
# reading of the two factors.
#
# FLEET SIZE. log.fleet.size is the revenue-weighted mean log number of active
# owners behind the unit's fleet series. An average over few owners moves
# more, which raises fishery riskiness and lowers the owner-specific factor
# for reasons that have nothing to do with the owner.

SWAP_PIECES <- c(
  p.fishery      = "Fishery riskiness (U fleet)",
  p.pattern      = "Pattern of active years (CV swap / U fleet)",
  p.ownerall     = "Owner's own volatility (CV / CV swap)",
  p.combine.swap = "Last factor, fleet-swapped part (CV swap / U fleet_A)",
  p.combine.own  = "Last factor, owner part (the rest)"
)
SWAP_MAIN <- c("p.fishery", "p.pattern", "p.ownerall")

add_swap_pieces <- function(df) {
  df %>%
    filter(mode == "active") %>%
    mutate(
      p.pattern      = log(CV.swap / U.fleet.all),
      p.ownerall     = log(CV / CV.swap),
      p.combine.swap = log(CV.swap / U.fleet),
      p.combine.own  = p.combine - p.combine.swap,
      p.years.len    = log(YA.len),
      p.combine.len  = log(LF.len)
    )
}

for (g in list(list(name = "lifetime", data = cv_decomp_owner_lifetime, spec = "is.specialist"),
               list(name = "rolling", data = cv_decomp_owner.rolling, spec = "is.specialist.window"))) {
  d <- add_swap_pieces(g$data)
  ok <- is.finite(d$p.pattern) & is.finite(d$p.ownerall)
  d <- d[ok, ]
  cat("\n===== Fleet-swapped version,", g$name, "=====\n")
  cat("Units dropped for a non-finite fleet-swapped piece", sum(!ok), "of", length(ok), "\n")
  cat("Max gap in log CV = fishery + pattern + owner's own (should be about 0)",
      signif(max(abs(d$p.fishery + d$p.pattern + d$p.ownerall - d$log.cv)), 3), "\n")
  cat("Max gap in pattern = years-active effect + fleet-swapped last factor (should be about 0)",
      signif(max(abs(d$p.years + d$p.combine.swap - d$p.pattern)), 3), "\n")
  cat("Max gap in owner's own = owner-specific factor + owner part of the last factor (should be about 0)",
      signif(max(abs(d$p.owner + d$p.combine.own - d$p.ownerall)), 3), "\n")
  cat("Specialists with a pattern factor of 1 (should be about 1)",
      round(mean(abs(d$p.pattern[d[[g$spec]]]) < 1e-9), 4), "\n")
}

swap_columns <- map(decomp_columns, function(cl) {
  cl$data <- add_swap_pieces(cl$data) %>% filter(is.finite(p.pattern), is.finite(p.ownerall))
  cl
})

swap_shares_tbl_owner <- map_dfr(swap_columns, function(cl) {
  rhs <- if (is.null(cl$fe)) "log.cv" else paste("log.cv |", cl$fe)
  map_dfr(names(SWAP_PIECES), function(p) {
    m <- feols(as.formula(paste(p, "~", rhs)), data = cl$data, vcov = cl$vcov)
    tibble(column = cl$column, piece = p, piece.label = SWAP_PIECES[[p]],
           share = unname(coef(m)["log.cv"]), se = unname(se(m)["log.cv"]), n = nobs(m))
  })
})

swap_geo_tbl_owner <- map_dfr(swap_columns, function(cl) {
  cl$data %>%
    summarise(
      geo.cv           = exp(mean(log.cv)),
      geo.U.fleet      = exp(mean(p.fishery)),
      geo.pattern      = exp(mean(p.pattern)),
      geo.ownerall     = exp(mean(p.ownerall)),
      geo.years        = exp(mean(p.years)),
      geo.combine.swap = exp(mean(p.combine.swap)),
      geo.owner        = exp(mean(p.owner)),
      geo.combine.own  = exp(mean(p.combine.own)),
      share.pattern.above.1  = mean(p.pattern > 1e-9),
      share.ownerall.below.1 = mean(p.ownerall < 0)
    ) %>%
    mutate(column = cl$column, fixed.effects = cl$fe.label)
})

print(swap_shares_tbl_owner %>% select(column, piece.label, share, se, n), n = Inf, width = Inf)
cat("Sum of the three main shares by column (must be 1)\n")
print(swap_shares_tbl_owner %>% filter(piece %in% SWAP_MAIN) %>%
        group_by(column) %>% summarise(sum.share = sum(share), .groups = "drop"))
print(swap_geo_tbl_owner, width = Inf)

write.csv(swap_shares_tbl_owner %>% left_join(swap_geo_tbl_owner, by = "column"),
          file.path(table_dir, "cv_decomposition_swap_owner.csv"), row.names = FALSE)
cat("Wrote cv_decomposition_swap_owner.csv\n")

# By Phi bin, with the same two-stage averaging as Section 8. The rolling grain
# also carries the length-only years-active effect and last factor.
SWAP_PHI_VARS <- c("log.cv", "p.fishery", "p.pattern", "p.ownerall", "p.years", "p.combine.swap",
                   "p.owner", "p.combine.own", "p.combine")

swap_by_phi <- function(data, specialist_col, grain, vars) {
  d <- add_swap_pieces(data) %>%
    filter(if_all(all_of(vars), is.finite))
  specialists <- d %>%
    filter(.data[[specialist_col]]) %>%
    two_stage_piece_summary(vars) %>%
    mutate(bin.label = "Specialists", bin.order = 0)
  multi <- d %>%
    filter(!.data[[specialist_col]]) %>%
    mutate(phi.bin = ntile(Phi, N_DECOMP_BINS)) %>%
    group_by(phi.bin) %>%
    group_modify(~ two_stage_piece_summary(.x, vars)) %>%
    ungroup() %>%
    mutate(bin.label = paste0("Q", phi.bin), bin.order = phi.bin) %>%
    select(-phi.bin)
  bind_rows(specialists, multi) %>%
    mutate(grain = grain) %>%
    relocate(grain, bin.label, bin.order)
}

swap_by_phi_owner <- bind_rows(
  swap_by_phi(cv_decomp_owner_lifetime, "is.specialist", "lifetime", SWAP_PHI_VARS),
  swap_by_phi(cv_decomp_owner.rolling, "is.specialist.window", "rolling",
              c(SWAP_PHI_VARS, "p.years.len", "p.combine.len"))
)

print(swap_by_phi_owner %>%
        mutate(across(ends_with(".mean"), exp, .names = "geo.{.col}")) %>%
        select(grain, bin.label, n, mean.Phi, starts_with("geo.")),
      n = Inf, width = Inf)
write.csv(swap_by_phi_owner, file.path(table_dir, "cv_decomposition_swap_by_phi_owner.csv"), row.names = FALSE)
cat("Wrote cv_decomposition_swap_by_phi_owner.csv\n")

# Short-stretch check, rolling multi-fishery owner-windows
cat("\n===== Short-stretch check, rolling =====\n")
if (is.null(short_stretch_tbl_owner)) {
  cat("No unit-fishery was fished in every active year, so there is no shrinkage to measure\n")
} else {
  cat("Mean ratio of a CV over k of n active years to the CV over all n, owner's own revenue against the",
      "fleet series (1 means no shrinkage, owner close to fleet means the two shrink alike)\n")
  print(short_stretch_tbl_owner %>%
          as_tibble() %>%
          mutate(owner.over.fleet = mean.ratio_owner / mean.ratio_fleet),
        n = Inf, width = Inf)
  write.csv(short_stretch_tbl_owner, file.path(table_dir, "cv_decomposition_short_stretch_owner.csv"),
            row.names = FALSE)
  cat("Wrote cv_decomposition_short_stretch_owner.csv\n")

  ss <- add_swap_pieces(rolling_multi_owner) %>%
    filter(is.finite(p.years.len), is.finite(p.combine.len))
  cat("Rolling multi-fishery owner-windows with a length-only prediction", nrow(ss), "of",
      sum(rolling_multi_owner$mode == "active"), "\n")
  cat("Geometric means, years-active effect actual", round(exp(mean(ss$p.years)), 3),
      " from stretch length alone", round(exp(mean(ss$p.years.len)), 3), "\n")
  cat("Geometric means, last factor actual", round(exp(mean(ss$p.combine)), 3),
      " pushed up by short stretches on the owner side alone", round(exp(mean(ss$p.combine.len)), 3), "\n")
  cat("Length-only years-active effect times length-only last factor (about 1 if owner and fleet",
      "CVs shrink alike)", round(exp(mean(ss$p.years.len + ss$p.combine.len)), 3), "\n")
  cat("Share of the log years-active effect that stretch length alone predicts",
      round(mean(ss$p.years.len) / mean(ss$p.years), 3), "\n")
}

# Fleet size, rolling multi-fishery owner-windows
fs <- add_swap_pieces(rolling_multi_owner) %>%
  filter(is.finite(log.fleet.size), is.finite(p.ownerall))
cat("\n===== Fleet size, rolling multi-fishery owner-windows =====\n")
cat("Correlation of log fleet size with fishery riskiness", round(cor(fs$log.fleet.size, fs$p.fishery), 3),
    " with the owner-specific factor", round(cor(fs$log.fleet.size, fs$p.owner), 3),
    " with the owner's own volatility (CV / CV swap)", round(cor(fs$log.fleet.size, fs$p.ownerall), 3), "\n")
fleet_size_tbl_owner <- fs %>%
  mutate(size.bin = ntile(log.fleet.size, 5)) %>%
  group_by(size.bin) %>%
  summarise(
    n                = n(),
    geo.fleet.size   = exp(mean(log.fleet.size)),
    geo.cv           = exp(mean(log.cv)),
    geo.U.fleet      = exp(mean(p.fishery)),
    geo.owner        = exp(mean(p.owner)),
    geo.ownerall     = exp(mean(p.ownerall)),
    share.owner.below.1 = mean(p.owner < 0),
    .groups = "drop"
  )
print(fleet_size_tbl_owner, width = Inf)
write.csv(fleet_size_tbl_owner, file.path(table_dir, "cv_decomposition_fleet_size_owner.csv"), row.names = FALSE)
cat("Wrote cv_decomposition_fleet_size_owner.csv\n")

# ============================================================================
# 9. Save
# ============================================================================

cv_decomp_owner_path <- file.path(intermediate_dir, "ch3_cv_decomposition_owner.rdata")
save(cv_decomp_owner_lifetime, cv_decomp_owner.rolling, shares_tbl_owner, geo_means_tbl_owner,
     mode_comparison_owner, dropped_tbl_owner, cv_decomp_owner_by_phi,
     swap_shares_tbl_owner, swap_geo_tbl_owner, swap_by_phi_owner, short_stretch_tbl_owner,
     fleet_size_tbl_owner,
     file = cv_decomp_owner_path)
cat("Saved owner CV decomposition objects to", cv_decomp_owner_path, "\n")
