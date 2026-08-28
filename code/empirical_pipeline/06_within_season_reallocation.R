# Chapter 3 empirical pipeline, Chapter3_outline.md Section 5
#
# Figure 5. Distribution of weekly fishery-share turnover within a season.
# Figure 6. Empirical season windows per fishery-year, length and drift over
#           time.
# Table 6.  Regression of vessel x period Phi and rev.cv (from
#           vessel_period_summary) on within-season turnover, with
#           prime.fishery and period fixed effects.
#
# Skips the cross-fishery co-participation network itself, none of Figure 5,
# Figure 6, or Table 6 above need it. Figure 7 (switch events weighted by
# that network), marked [maybe] in Chapter3_outline.md, is now built in
# 11_switch_event_weights.R instead, at trip grain rather than the week
# grain this script uses, see that script's header for why it is a separate
# file rather than a new section here.
#
# Everything here follows chapter3_plan.md Section 9.3's "5A, expanded" spec
# (the resolved version of the plan, superseding the earlier draft in
# Section 4). Do not use Period fleetwide (too much missingness, per that
# section), which is why this uses Batch.Year and Statistical.Week only.
#
# Reads intermediate data/ch3_panel.rdata (period_bounds, vessel_period_summary,
# vessel_summary for prime.fishery) AND reloads the raw catch_data_temp.rdata
# ticket file directly, since week-level granularity never makes it into the
# saved panel, only annual aggregates do. catch_data_temp has no
# Statistical.Week or Week.Ending.Date column (checked directly against the
# real object on the server), so Statistical.Week below is DERIVED from
# Date.Landed via derive_statistical_week() in 00_setup.R, see the comment
# there for the exact definition and why. Saves switching_by_vessel_year and
# season_windows to intermediate data/ch3_within_season.rdata so
# 07_behavioral_heterogeneity.R does not have to redo this reload.

source("code/empirical_pipeline/00_setup.R")

if (!exists("period_bounds") || !exists("vessel_period_summary") || !exists("vessel_summary")) load(panel_path)

# ============================================================================
# 1. Reload raw fish tickets, weekly grain this time
# ============================================================================
#
# Same cleaning steps as 01_build_panel.R Section 2 (vessel ID fix,
# BAD_VESSEL_IDS drop, MIN_YEAR/MAX_YEAR window, fishery-code stripping),
# duplicated here rather than factored out, since this is the only other
# script in the pipeline that needs ticket-level rather than annual
# granularity, and 01_build_panel.R's own Section 2 is already the tested
# version of this cleaning. MAX_YEAR is loaded from panel_path above (saved
# there by 01_build_panel.R Section 2b) rather than recomputed, so this
# script can never drift out of sync with whatever trailing-year coverage
# trim that section decided on. The one addition beyond 01's Section 2 is
# Statistical.Week itself, see derive_statistical_week() in 00_setup.R.

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp[["CFEC.Value..Detail."]][is.na(catch_data_temp[["CFEC.Value..Detail."]])] <- 0

# Pounds..Detail. loads as an R integer. A single week's sum fits, but a
# season's worth of weeks cumsum()-ed together for a high-volume
# fishery-year can exceed 32-bit int range partway through, after which
# every later cumsum() entry in that group silently becomes NA rather than
# erroring, confirmed against a real run (12 summarise() warnings pointing
# at weighted_percentile_week()'s cumsum(), 5 fishery-years missing from
# Figure 6 as a result). Coercing to double here, before anything sums it,
# is cheap and removes the ceiling entirely.
catch_data_temp[["Pounds..Detail."]] <- as.numeric(catch_data_temp[["Pounds..Detail."]])

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(
    Fishery = strip_fishery_space(CFEC.Permit.Fishery),
    Statistical.Week = derive_statistical_week(Date.Landed)
  ) %>%
  filter(Fishery != "", !is.na(Statistical.Week))

cat("Ticket rows entering the within-season panel:", nrow(catch_data_temp), "\n")

# ============================================================================
# 2. Vessel x year x week x fishery revenue, restricted to real portfolios
# ============================================================================
#
# Deflation is skipped here on purpose. Every object in this script (weekly
# shares, the 5th/95th percentile landing week) only compares dollars WITHIN
# a single Batch.Year, where a CPI deflator is a constant multiplier that
# cancels out of a share (numerator and denominator scale together) and out
# of a within-year percentile rank. It would only matter for a level
# comparison across years, which nothing here does.
#
# MIN_LANDINGS originally excluded vessel-fishery-years with fewer than 3
# ticket rows, per chapter3_plan.md Section 9.3 ("single-delivery permits
# produce degenerate switching measures"). Lowered to 1 (i.e. no exclusion)
# after a methodological review flagged that this threshold did NOT match
# 01_build_panel.R's H_bar/rev.cv, which apply no landings floor at all, so
# a vessel's thin, few-landing side fishery could disappear from its
# switching measure (making it look like a specialist) while still fully
# counting toward H_bar and rev.cv, a mismatch that could manufacture a
# spurious H_bar-CV relationship inside whichever turnover group absorbed
# those vessels. At 1, both objects now condition on the same thing, any
# positive activity, closing that gap. Checked against a real run, Table 6
# is not sensitive to this change (if anything the Phi relationship is
# slightly stronger at 1 than at 3), and a single real landing is treated
# as meaningful activity rather than noise, not a data-quality problem to
# filter out.
MIN_LANDINGS <- 1

vessel_fishery_year_landings <- catch_data_temp %>%
  count(Vessel.ADFG.Number, Batch.Year, Fishery, name = "n.landings")

vessel_week_fishery <- catch_data_temp %>%
  semi_join(
    vessel_fishery_year_landings %>% filter(n.landings >= MIN_LANDINGS),
    by = c("Vessel.ADFG.Number", "Batch.Year", "Fishery")
  ) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Statistical.Week, Fishery) %>%
  summarise(revenue = sum(CFEC.Value..Detail., na.rm = TRUE), .groups = "drop")

# ============================================================================
# 3. Weekly fishery-share target switching (Figure 5, and Section 6's type
#    classifier)
# ============================================================================
#
# s_jw = vessel i's revenue in fishery j during week w, over i's total
# revenue across all fisheries in week w, the same style as the annual share
# s_jt = revenue_jt / vessel.year.rev used everywhere else in this pipeline
# (01_build_panel.R Section 6), just at week grain instead of year grain. It
# answers "what did this week's effort mix look like," not "what share of
# the whole year did this week contribute."
#
# Zero-filled the same way the annual share panel is, complete(Fishery,
# active week) within (vessel, year), scoped to the fisheries that vessel
# actually fished THAT YEAR (not its whole panel), so a week where it fished
# only fishery A correctly reads as a zero share in fishery B rather than a
# missing row.
vessel_year_week_share <- vessel_week_fishery %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  complete(Fishery, Statistical.Week, fill = list(revenue = 0)) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Statistical.Week) %>%
  mutate(week.revenue = sum(revenue), share = if_else(week.revenue > 0, revenue / week.revenue, 0)) %>%
  ungroup()

n_active_weeks <- vessel_year_week_share %>%
  distinct(Vessel.ADFG.Number, Batch.Year, Statistical.Week) %>%
  count(Vessel.ADFG.Number, Batch.Year, name = "n.active.weeks")

# Target switching is summed over CONSECUTIVE ACTIVE weeks, not consecutive
# calendar weeks. A vessel active in week 5 and next active in week 20 (an
# off-season gap, salmon runs do not run year-round) gets one increment of
# switching between weeks 5 and 20, not fifteen phantom zero-to-zero
# increments for the weeks in between it was not fishing at all. Comparing
# calendar-consecutive weeks instead would manufacture "reallocation" out of
# nothing but a seasonal gap. A vessel-year with only one active week has no
# consecutive pair to compare and drops out of the summarise below
# entirely, not zero-filled, since "no turnover measurable" is not the same
# claim as "turnover was zero."
switching_by_vessel_year <- vessel_year_week_share %>%
  arrange(Vessel.ADFG.Number, Batch.Year, Fishery, Statistical.Week) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  mutate(share.lag = lag(share, order_by = Statistical.Week)) %>%
  ungroup() %>%
  filter(!is.na(share.lag)) %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  summarise(weekly.switching = sum(abs(share - share.lag)), .groups = "drop") %>%
  left_join(n_active_weeks, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  # weekly.switching is a SUM over consecutive-active-week transitions, so
  # it is mechanically larger for a vessel that simply fishes more weeks,
  # holding per-week reallocation intensity fixed, a vessel active 20 weeks
  # has up to 19 transitions to accumulate switching over, one active 3
  # weeks has only 2. Active weeks correlate with vessel size/activity,
  # which also correlate with H_bar and CV, so the raw sum risks the
  # Section 6 classifier partly capturing "fishes more" rather than
  # "reallocates more." weekly.switching.per.transition divides by the
  # number of available transitions (n.active.weeks - 1, always >= 1 here
  # since every row surviving the filter above has at least one valid
  # share.lag) to give a per-transition intensity instead. n.active.weeks
  # itself is also carried forward as a Table 6 control below rather than
  # left unused.
  mutate(weekly.switching.per.transition = weekly.switching / (n.active.weeks - 1))

cat("Vessel-years with a computable weekly switching measure:", nrow(switching_by_vessel_year), "\n")

figure5 <- switching_by_vessel_year %>%
  ggplot(aes(x = weekly.switching)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    # Full definition (target switching = sum of |share change| across
    # active weeks, Statistical.Week basis, the <3-landings exclusion)
    # belongs in the caption, not a subtitle this figure's width can't hold.
    title = "Distribution of within-season turnover",
    subtitle = "One observation per vessel-year",
    x = "Weekly target switching",
    y = "Vessel-years"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure5_weekly_turnover_distribution.png"),
       figure5, width = 7, height = 5, dpi = 300)

cat("Wrote figure5_weekly_turnover_distribution.png\n")

# Second view of the same distribution, added as an ADDITIONAL figure
# alongside figure5 above, not a replacement. A pseudo-log x-axis transform
# was tried first and rejected, geom_histogram() bins on the untransformed
# scale before the axis remaps bar positions, so it barely changed how the
# plot looked, checked directly against a real run. The actual problem is
# the Y-axis, most vessel-years sit at exactly weekly.switching = 0
# (single-fishery specialists, whose weekly mix never changes), a bar tens
# of times taller than anything else on a linear count axis, which crushes
# the shape of the rest of the distribution flat regardless of how the
# X-axis is scaled. A log Y-axis would fix that but is an awkward read for
# a general audience, so instead this follows the same pattern figure3's
# specialist split already uses elsewhere in the chapter, report the
# zero-switching share as a number, and plot the shape of what is left on
# an ordinary linear scale.
zero_switching_share <- mean(switching_by_vessel_year$weekly.switching == 0)
cat("Share of vessel-years with exactly zero weekly switching:",
    round(zero_switching_share, 3), "\n")

figure5_nonzero <- switching_by_vessel_year %>%
  filter(weekly.switching > 0) %>%
  ggplot(aes(x = weekly.switching)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of within-season turnover, excluding zero switching",
    subtitle = paste0("Vessel-years with weekly.switching > 0 only (",
                       scales::percent(1 - zero_switching_share, accuracy = 0.1),
                       " of all vessel-years)"),
    x = "Weekly target switching",
    y = "Vessel-years"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure5b_weekly_turnover_nonzero.png"),
       figure5_nonzero, width = 7, height = 5, dpi = 300)

cat("Wrote figure5b_weekly_turnover_nonzero.png\n")

# ============================================================================
# 4. Empirical season windows per fishery-year (Figure 6)
# ============================================================================
#
# Fleet-wide, not vessel-specific, per chapter3_outline.md's own framing
# ("empirical season windows per fishery-year"). Start/end are the
# 5th/95th percentile landing week within that fishery-year, weighted by
# REVENUE (CFEC.Value..Detail.), not landed pounds. Originally pounds-
# weighted specifically to keep a price signal out of a timing question,
# switched to revenue because pounds turned out to carry its own
# contamination risk, a ticket row logging incidental bycatch of a
# non-target species under a permit can carry positive landed weight with
# little or no recorded value, which would pull a pounds-weighted season
# window toward weeks with heavy bycatch rather than heavy TARGET-species
# effort. Revenue does not have that problem, a near-zero-value bycatch row
# barely moves a revenue-weighted percentile. The trade-off accepted in
# exchange is the zero-filled-price issue Table 2 documents, a fishery-year
# with poor price coverage has some of its landed weeks effectively zeroed
# out of the weighting by the catch_data_temp[["CFEC.Value..Detail."]]
# [is.na(...)] <- 0 fill in Section 2b, understating those weeks'
# contribution to the window (share_zero_fill_has_positive_pounds in
# 01_build_panel.R quantifies how much of a live concern that is
# fleet-wide). This also breaks consistency with
# 08_state_contingent_activation.R's shock, which stays pounds-weighted on
# purpose there, that shock IS the quantity/timing object pounds are meant
# to capture, not a proxy standing in for it the way this season window is.
# A fishery-year needs at least MIN_SEASON_LANDINGS ticket rows fleet-wide
# for the percentiles to mean anything, a handful of landings cannot
# support a stable estimate, and is dropped entirely if every one of those
# landings has zero recorded value (rare, but the alternative is a
# divide-by-zero).
MIN_SEASON_LANDINGS <- 10

weighted_percentile_week <- function(week, weight, p) {
  ord <- order(week)
  week <- week[ord]
  # as.numeric() defends against integer overflow in cumsum() below, not
  # actually needed for CFEC.Value..Detail. (unlike Pounds..Detail., see
  # the comment in Section 1, it does not load as an R integer) but kept
  # as a general defense since this function has no other way to guarantee
  # whatever it is handed is not integer-typed.
  weight <- as.numeric(weight[ord])
  cum_share <- cumsum(weight) / sum(weight)
  week[which(cum_share >= p)[1]]
}

fishery_year_landings <- catch_data_temp %>% count(Fishery, Batch.Year, name = "n.landings")

season_windows <- catch_data_temp %>%
  semi_join(fishery_year_landings %>% filter(n.landings >= MIN_SEASON_LANDINGS),
            by = c("Fishery", "Batch.Year")) %>%
  group_by(Fishery, Batch.Year, Statistical.Week) %>%
  summarise(revenue = sum(CFEC.Value..Detail., na.rm = TRUE), .groups = "drop") %>%
  group_by(Fishery, Batch.Year) %>%
  filter(sum(revenue) > 0) %>%
  summarise(
    season.start = weighted_percentile_week(Statistical.Week, revenue, 0.05),
    season.end   = weighted_percentile_week(Statistical.Week, revenue, 0.95),
    .groups = "drop"
  ) %>%
  mutate(season.length = season.end - season.start)

cat("Fishery-years with a computable season window:", nrow(season_windows), "\n")

# Restricted to the 8 fisheries with the most total revenue across the
# panel, purely for legibility, the same reasoning as Figure 2's gear-class
# grouping. CHECK this reads cleanly once run against real fishery codes,
# some may need a friendlier label than the raw code.
top_fisheries <- catch_data_temp %>%
  group_by(Fishery) %>%
  summarise(total.revenue = sum(CFEC.Value..Detail.), .groups = "drop") %>%
  slice_max(total.revenue, n = 8) %>%
  pull(Fishery)

# One ribbon per fishery, spanning season.start to season.end each year, so
# ribbon WIDTH shows season length and ribbon POSITION shows drift, both in
# a single figure rather than two.
figure6 <- season_windows %>%
  filter(Fishery %in% top_fisheries) %>%
  ggplot(aes(x = Batch.Year)) +
  geom_ribbon(aes(ymin = season.start, ymax = season.end), fill = "steelblue", alpha = 0.4) +
  geom_line(aes(y = season.start), color = "steelblue4", linewidth = 0.4) +
  geom_line(aes(y = season.end), color = "steelblue4", linewidth = 0.4) +
  facet_wrap(~ Fishery, scales = "free_y") +
  labs(
    # Weighted by revenue, not pounds, see the "REVENUE" comment above
    # Section 4. Ribbon width/position (season length/drift) and the
    # top-8-by-revenue selection belong in the caption, not here.
    title = "Season windows, top 8 fisheries by revenue",
    subtitle = "Ribbon spans the revenue-weighted 5th-95th percentile week",
    x = "Year", y = "Statistical week"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure6_season_windows.png"),
       figure6, width = 9, height = 6, dpi = 300)

cat("Wrote figure6_season_windows.png\n")

# ============================================================================
# 5. Table 6. Vessel x period Phi and rev.cv on within-season target switching
# ============================================================================
#
# vessel_period_summary (loaded from panel_path) already computes Phi and
# rev.cv within each of the panel's N_PERIODS calendar periods, exactly the
# grain "primary-fishery and period fixed effects" calls for. "period" here
# means these N_PERIODS calendar thirds (everyone's "Period 2" is the same
# calendar years), a DIFFERENT object from the vessel-relative first/second
# active-year halves 07_behavioral_heterogeneity.R and
# 08_state_contingent_activation.R use for their split-sample designs, the
# two should not be conflated when reading results across sections.
# period_of() reconstructs the SAME period boundaries 01_build_panel.R
# Section 6b used, read off the saved period_bounds tibble rather than
# recomputed, so this can never define periods differently than the panel
# already does. Run in levels, matching Table 4's convention for anything
# involving Phi (Phi can sit at or near 0, where a log is undefined or
# unstable), not Table 7/8's log-linear convention, which exists there
# specifically to compare against Chapter 2's own log-linear regression.
period_of <- function(batch_year) {
  period_bounds$period[findInterval(batch_year, period_bounds$start)]
}

switching_by_vessel_period <- switching_by_vessel_year %>%
  mutate(period = period_of(Batch.Year)) %>%
  group_by(Vessel.ADFG.Number, period) %>%
  summarise(
    n.years.switching        = n(),
    within.season.switching  = mean(weekly.switching),
    mean.active.weeks        = mean(n.active.weeks),
    .groups = "drop"
  )

table6_data <- vessel_period_summary %>%
  filter(meets.min.years.period, is.finite(rev.cv)) %>%
  inner_join(switching_by_vessel_period, by = c("Vessel.ADFG.Number", "period")) %>%
  left_join(vessel_summary %>% select(Vessel.ADFG.Number, prime.fishery), by = "Vessel.ADFG.Number")

cat("Vessel x period observations entering Table 6:", nrow(table6_data), "\n")

# mean.active.weeks is added as a control alongside within.season.switching,
# not left computed-and-unused. weekly.switching is mechanically larger for
# a vessel that simply fishes more weeks (more consecutive-week transitions
# to accumulate switching over), and active weeks correlate with vessel
# size/activity, which also correlate with Phi and CV, so without this
# control the switching coefficient risked partly reading "fishes more" as
# "reallocates more."
#
# Clustered on Vessel.ADFG.Number explicitly, not left to fixest's default.
# feols clusters on the FIRST fixed effect listed when no cluster is given,
# which here would be prime.fishery, a small handful of fishery classes,
# too few clusters for reliable inference, and it would not account for the
# same vessel appearing in up to three period rows either. Vessel is both
# the repeated unit and a much larger cluster count.
model_phi_on_switching <- feols(Phi ~ within.season.switching + mean.active.weeks | prime.fishery + period,
                                 data = table6_data, cluster = ~Vessel.ADFG.Number)
model_cv_on_switching  <- feols(rev.cv ~ within.season.switching + mean.active.weeks | prime.fishery + period,
                                 data = table6_data, cluster = ~Vessel.ADFG.Number)

# Within-vessel robustness columns, vessel fixed effects in place of
# prime.fishery, identified only off vessels with two or more valid
# periods. This absorbs any fixed vessel-level tendency (a vessel that just
# generally switches a lot and runs high-Phi throughout) and asks whether
# MORE switching in one of ITS OWN periods goes with MORE Phi/CV in that
# same period, a stronger within-unit version of the same test, at the cost
# of dropping single-period vessels.
model_phi_vessel_fe <- feols(Phi ~ within.season.switching + mean.active.weeks | Vessel.ADFG.Number + period,
                              data = table6_data, cluster = ~Vessel.ADFG.Number)
model_cv_vessel_fe  <- feols(rev.cv ~ within.season.switching + mean.active.weeks | Vessel.ADFG.Number + period,
                              data = table6_data, cluster = ~Vessel.ADFG.Number)

# dict relabels within.season.switching for the printed/exported table only,
# the column itself stays within.season.switching so nothing else in this
# script needs to change if the display label is tweaked again later.
table6_dict <- c(within.season.switching = "Target switching", mean.active.weeks = "Active weeks (mean)")

etable(
  model_phi_on_switching, model_cv_on_switching, model_phi_vessel_fe, model_cv_vessel_fe,
  headers = c("Phi", "rev.cv", "Phi (vessel FE)", "rev.cv (vessel FE)"),
  dict = table6_dict,
  tex = TRUE,
  file = file.path(table_dir, "table6_annual_instability_on_within_season_switching.tex"),
  replace = TRUE
)

print(etable(model_phi_on_switching, model_cv_on_switching, model_phi_vessel_fe, model_cv_vessel_fe, dict = table6_dict))

cat("Wrote table6_annual_instability_on_within_season_switching.tex\n")

# ============================================================================
# 6. Save
# ============================================================================
#
# switching_by_vessel_year is the object 07_behavioral_heterogeneity.R needs
# for the Section 6 type classifier, saved separately from ch3_panel.rdata
# since it comes from a ticket-level reload this script does and
# 01_build_panel.R does not.

# vessel_fisheries_06, each vessel's distinct ever-fished Fishery set per
# THIS script's own reload, MIN_LANDINGS-gated (currently 1, any ticket row,
# no revenue threshold). Saved so 07_behavioral_heterogeneity.R's specialist
# diagnostic can compare it directly against vessel_mean_share's Fishery set
# from 01_build_panel.R, which gates on a DIFFERENT criterion
# (vessel_fishery_year's fished = revenue > 0, an ANNUAL total, see
# 01_build_panel.R Section 4). The two are not guaranteed to agree, a
# fishery whose annual revenue total nets to zero or negative (a
# correction/refund ticket offsetting a real landing, for instance) reads as
# never-fished to 01_'s gate even if it had genuine positive-revenue weeks
# that would show up here.
vessel_fisheries_06 <- vessel_week_fishery %>% distinct(Vessel.ADFG.Number, Fishery)

within_season_path <- file.path(intermediate_dir, "ch3_within_season.rdata")
save(switching_by_vessel_year, season_windows, vessel_fisheries_06, file = within_season_path)
cat("Saved within-season objects to", within_season_path, "\n")
