# Chapter 3 empirical pipeline, Chapter3_outline.md Section 5
#
# Figure 5. Distribution of weekly fishery-share turnover within a season.
# Figure 6. Empirical season windows per fishery-year, length and drift over
#           time.
# Table 6.  Regression of vessel x period Phi and rev.cv (from
#           vessel_period_summary) on within-season turnover, with
#           prime.fishery and period fixed effects.
#
# Skips the cross-fishery co-participation network and Figure 7 (switch
# events weighted by that network), both marked [maybe] in
# Chapter3_outline.md. Neither required output above needs the network, it
# only feeds the optional Figure 7 and Section 7's optional Table 12.
#
# Everything here follows chapter3_plan.md Section 9.3's "5A, expanded" spec
# (the resolved version of the plan, superseding the earlier draft in
# Section 4). Do not use Period fleetwide (too much missingness, per that
# section), which is why this uses Batch.Year and Statistical.Week only.
#
# Reads intermediate data/ch3_panel.rdata (period_bounds, vessel_period_summary,
# vessel_summary for prime.fishery) AND reloads the raw catch_data_temp.rdata
# ticket file directly, since Statistical.Week never makes it into the saved
# panel, only annual aggregates do. Saves churn_by_vessel_year and
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
# trim that section decided on.

load(file.path(intermediate_dir, "catch_data_temp.rdata"))

catch_data_temp$Vessel.ADFG.Number[catch_data_temp$Vessel.ADFG.Number == 62.39] <- 62339
catch_data_temp <- catch_data_temp %>% filter(!(Vessel.ADFG.Number %in% BAD_VESSEL_IDS))
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp[["CFEC.Value..Detail."]][is.na(catch_data_temp[["CFEC.Value..Detail."]])] <- 0

catch_data_temp <- catch_data_temp %>%
  filter(Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  mutate(Fishery = strip_fishery_space(CFEC.Permit.Fishery)) %>%
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
# MIN_LANDINGS restricts to vessel-fishery-years with at least a handful of
# ticket rows, per chapter3_plan.md Section 9.3 ("single-delivery permits
# produce degenerate churn measures"). CHECK this threshold once run on real
# data, it is a judgment call, not a fact.
MIN_LANDINGS <- 3

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
# 3. Weekly fishery-share churn (Figure 5, and Section 6's type classifier)
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

# Churn is summed over CONSECUTIVE ACTIVE weeks, not consecutive calendar
# weeks. A vessel active in week 5 and next active in week 20 (an off-season
# gap, salmon runs do not run year-round) gets one churn increment between
# weeks 5 and 20, not fifteen phantom zero-to-zero increments for the weeks
# in between it was not fishing at all. Comparing calendar-consecutive weeks
# instead would manufacture "reallocation" out of nothing but a seasonal
# gap. A vessel-year with only one active week has no consecutive pair to
# compare and drops out of the summarise below entirely, not zero-filled,
# since "no turnover measurable" is not the same claim as "turnover was
# zero."
churn_by_vessel_year <- vessel_year_week_share %>%
  arrange(Vessel.ADFG.Number, Batch.Year, Fishery, Statistical.Week) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery) %>%
  mutate(share.lag = lag(share, order_by = Statistical.Week)) %>%
  ungroup() %>%
  filter(!is.na(share.lag)) %>%
  group_by(Vessel.ADFG.Number, Batch.Year) %>%
  summarise(weekly.churn = sum(abs(share - share.lag)), .groups = "drop") %>%
  left_join(n_active_weeks, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  # weekly.churn is a SUM over consecutive-active-week transitions, so it is
  # mechanically larger for a vessel that simply fishes more weeks, holding
  # per-week reallocation intensity fixed, a vessel active 20 weeks has up
  # to 19 transitions to accumulate churn over, one active 3 weeks has only
  # 2. Active weeks correlate with vessel size/activity, which also
  # correlate with H_bar and CV, so the raw sum risks the Section 6
  # classifier partly capturing "fishes more" rather than "reallocates
  # more." weekly.churn.per.transition divides by the number of available
  # transitions (n.active.weeks - 1, always >= 1 here since every row
  # surviving the filter above has at least one valid share.lag) to give a
  # per-transition intensity instead. n.active.weeks itself is also carried
  # forward as a Table 6 control below rather than left unused.
  mutate(weekly.churn.per.transition = weekly.churn / (n.active.weeks - 1))

cat("Vessel-years with a computable weekly churn measure:", nrow(churn_by_vessel_year), "\n")

figure5 <- churn_by_vessel_year %>%
  ggplot(aes(x = weekly.churn)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    # Full definition (Statistical.Week basis, the <3-landings exclusion)
    # belongs in the caption, not a subtitle this figure's width can't hold.
    title = "Distribution of within-season turnover",
    subtitle = "One observation per vessel-year",
    x = "Weekly fishery-share churn within a season (sum of |share change| across active weeks)",
    y = "Vessel-years"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure5_weekly_turnover_distribution.png"),
       figure5, width = 7, height = 5, dpi = 300)

cat("Wrote figure5_weekly_turnover_distribution.png\n")

# ============================================================================
# 4. Empirical season windows per fishery-year (Figure 6)
# ============================================================================
#
# Fleet-wide, not vessel-specific, per chapter3_outline.md's own framing
# ("empirical season windows per fishery-year"). Start/end are the
# 5th/95th percentile landing week within that fishery-year, weighted by
# landed POUNDS rather than revenue. "When is this fishery active" is a
# timing/effort question, and revenue drags a price signal into the
# weights it does not need, plus it would inherit the zero-filled-price
# issue Table 2 documents (a fishery-year with poor price coverage gets its
# weeks mis-weighted, and a fishery-year with zero recorded value everywhere
# would divide by zero). Pounds is also consistent with the quantity-first
# choice 08_state_contingent_activation.R makes for its shock, for the same
# reason (strip price out of a purely biological/timing question). A
# fishery-year needs at least MIN_SEASON_LANDINGS ticket rows fleet-wide for
# the percentiles to mean anything, a handful of landings cannot support a
# stable estimate, and is dropped entirely if every one of those landings
# has missing/zero recorded weight (rare, but the alternative is a
# divide-by-zero).
MIN_SEASON_LANDINGS <- 10

weighted_percentile_week <- function(week, weight, p) {
  ord <- order(week)
  week <- week[ord]
  weight <- weight[ord]
  cum_share <- cumsum(weight) / sum(weight)
  week[which(cum_share >= p)[1]]
}

fishery_year_landings <- catch_data_temp %>% count(Fishery, Batch.Year, name = "n.landings")

season_windows <- catch_data_temp %>%
  semi_join(fishery_year_landings %>% filter(n.landings >= MIN_SEASON_LANDINGS),
            by = c("Fishery", "Batch.Year")) %>%
  group_by(Fishery, Batch.Year, Statistical.Week) %>%
  summarise(pounds = sum(Pounds..Detail., na.rm = TRUE), .groups = "drop") %>%
  group_by(Fishery, Batch.Year) %>%
  filter(sum(pounds) > 0) %>%
  summarise(
    season.start = weighted_percentile_week(Statistical.Week, pounds, 0.05),
    season.end   = weighted_percentile_week(Statistical.Week, pounds, 0.95),
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
    # Weighted by pounds, not revenue, see the "Pounds is also consistent"
    # comment above. Ribbon width/position (season length/drift) and the
    # top-8-by-revenue selection belong in the caption, not here.
    title = "Season windows, top 8 fisheries by revenue",
    subtitle = "Ribbon spans the pounds-weighted 5th-95th percentile week",
    x = "Year", y = "Statistical week"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure6_season_windows.png"),
       figure6, width = 9, height = 6, dpi = 300)

cat("Wrote figure6_season_windows.png\n")

# ============================================================================
# 5. Table 6. Vessel x period Phi and rev.cv on within-season churn
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

churn_by_vessel_period <- churn_by_vessel_year %>%
  mutate(period = period_of(Batch.Year)) %>%
  group_by(Vessel.ADFG.Number, period) %>%
  summarise(
    n.years.churn        = n(),
    within.season.churn  = mean(weekly.churn),
    mean.active.weeks    = mean(n.active.weeks),
    .groups = "drop"
  )

table6_data <- vessel_period_summary %>%
  filter(meets.min.years.period, is.finite(rev.cv)) %>%
  inner_join(churn_by_vessel_period, by = c("Vessel.ADFG.Number", "period")) %>%
  left_join(vessel_summary %>% select(Vessel.ADFG.Number, prime.fishery), by = "Vessel.ADFG.Number")

cat("Vessel x period observations entering Table 6:", nrow(table6_data), "\n")

# mean.active.weeks is added as a control alongside within.season.churn,
# not left computed-and-unused. weekly.churn is mechanically larger for a
# vessel that simply fishes more weeks (more consecutive-week transitions
# to accumulate churn over), and active weeks correlate with vessel
# size/activity, which also correlate with Phi and CV, so without this
# control the churn coefficient risked partly reading "fishes more" as
# "reallocates more."
#
# Clustered on Vessel.ADFG.Number explicitly, not left to fixest's default.
# feols clusters on the FIRST fixed effect listed when no cluster is given,
# which here would be prime.fishery, a small handful of fishery classes,
# too few clusters for reliable inference, and it would not account for the
# same vessel appearing in up to three period rows either. Vessel is both
# the repeated unit and a much larger cluster count.
model_phi_on_churn <- feols(Phi ~ within.season.churn + mean.active.weeks | prime.fishery + period,
                             data = table6_data, cluster = ~Vessel.ADFG.Number)
model_cv_on_churn  <- feols(rev.cv ~ within.season.churn + mean.active.weeks | prime.fishery + period,
                             data = table6_data, cluster = ~Vessel.ADFG.Number)

# Within-vessel robustness columns, vessel fixed effects in place of
# prime.fishery, identified only off vessels with two or more valid
# periods. This absorbs any fixed vessel-level tendency (a vessel that is
# just generally high-churn and high-Phi throughout) and asks whether MORE
# churn in one of ITS OWN periods goes with MORE Phi/CV in that same
# period, a stronger within-unit version of the same test, at the cost of
# dropping single-period vessels.
model_phi_vessel_fe <- feols(Phi ~ within.season.churn + mean.active.weeks | Vessel.ADFG.Number + period,
                              data = table6_data, cluster = ~Vessel.ADFG.Number)
model_cv_vessel_fe  <- feols(rev.cv ~ within.season.churn + mean.active.weeks | Vessel.ADFG.Number + period,
                              data = table6_data, cluster = ~Vessel.ADFG.Number)

etable(
  model_phi_on_churn, model_cv_on_churn, model_phi_vessel_fe, model_cv_vessel_fe,
  headers = c("Phi", "rev.cv", "Phi (vessel FE)", "rev.cv (vessel FE)"),
  tex = TRUE,
  file = file.path(table_dir, "table6_annual_instability_on_within_season_churn.tex"),
  replace = TRUE
)

print(etable(model_phi_on_churn, model_cv_on_churn, model_phi_vessel_fe, model_cv_vessel_fe))

cat("Wrote table6_annual_instability_on_within_season_churn.tex\n")

# ============================================================================
# 6. Save
# ============================================================================
#
# churn_by_vessel_year is the object 07_behavioral_heterogeneity.R needs for
# the Section 6 type classifier, saved separately from ch3_panel.rdata since
# it comes from a ticket-level reload this script does and 01_build_panel.R
# does not.

within_season_path <- file.path(intermediate_dir, "ch3_within_season.rdata")
save(churn_by_vessel_year, season_windows, file = within_season_path)
cat("Saved within-season objects to", within_season_path, "\n")
