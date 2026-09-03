# Chapter 3 empirical pipeline, rolling-window (vessel-period) master script
#
# Runs the rolling-window parallel analysis, 01b_build_rolling_panel.R
# through 13b_predicted_bh_revenue_figures_rolling.R, in order. Mirrors
# run_all.R's own structure exactly (source() with no local =, so every
# script shares one global session and each wipes it at its own start via
# source("code/empirical_pipeline/00_setup.R")'s rm(list = ls())), see
# design Section 8.1.
#
# run_all.R MUST BE RUN FIRST, and must have completed through at least
# 06_within_season_reallocation.R. The rolling scripts read
# intermediate data/ch3_panel.rdata (built by 01_build_panel.R) and
# intermediate data/ch3_within_season.rdata (built by
# 06_within_season_reallocation.R) read-only, neither is rebuilt here.
# run_all.R itself is untouched by this file or by anything it sources.
#
# 01b_ writes table_rolling_sample_attrition.tex and
# figure_rolling_eligible_vessels_by_window.png BEFORE anything else runs,
# per design Section 7.4 these should be inspected before trusting 05b_
# through 09b_, if eligible vessels/windows come back far below the design
# document's expected order of magnitude (roughly 11,000-14,000 vessels,
# 120,000-170,000 vessel-windows), 01b_ prints a prominent warning, stop and
# look before letting the rest of this run.
#
# 06b_ must run before 07b_ (07b_ loads intermediate data/ch3_rolling_tau.rdata,
# built by 06b_). All other rolling scripts only depend on 01b_'s
# intermediate data/ch3_rolling.rdata and/or the baseline's own saved
# intermediate data, not on each other, and could in principle be sourced
# out of order except for that one pair, this file keeps them in a single
# fixed order regardless so that dependency is never left to chance.
#
# 12b_/13b_, the predicted buy-and-hold effort benchmark (no baseline
# counterpart, rolling-window-only concept). 12b_ only needs 01b_'s own
# saved objects (intermediate data/ch3_rolling.rdata) plus the baseline
# panel, not anything from 05b_ through 10b_, and is placed at the end here
# purely to avoid disturbing the previously-tested 01b_-10b_ order, not
# because of any real dependency on those scripts. 13b_ must run after 12b_
# (it reads intermediate data/ch3_predicted_bh.rdata, which only 12b_
# builds).
#
# 12c_/13c_, the predicted buy-and-hold Phi benchmark (Chapter 2's
# Phi = H_bar - H_LR decomposition, empirical analog, see 12c_'s own header
# note). Placed immediately after 12b_/13b_ for the same reason those two
# are placed at the end, 12c_ needs only 01b_'s own saved objects
# (intermediate data/ch3_rolling.rdata) plus the baseline panel, nothing
# from 12b_/13b_ themselves (12c_ duplicates, rather than shares, 12b_'s own
# Sections 1-2 machinery, see 12c_'s header note on that choice), so 12c_
# could in principle run anywhere after 01b_, kept here purely to group the
# two predicted-BH benchmark pairs together. 13c_ must run after 12c_ (it
# reads intermediate data/ch3_predicted_bh_phi.rdata, which only 12c_
# builds) AND after 06b_ (it reads intermediate data/ch3_rolling_tau.rdata
# for its own tau-quartile external validation check, built by 06b_, already
# guaranteed to have run by this point in the fixed order below).
#
# 01b_owner/05b_owner, the owner-level (File.Number = permit holder) twin of
# 01b_build_rolling_panel.R/05b_table4_figure3_rolling.R, the rolling-window
# analogue of the baseline 05_table4_figure3_owner.R. Placed at the end for
# the identical reasoning 12b_/13b_ above are, 01b_build_rolling_panel_owner.R
# needs only intermediate data/ch3_panel.rdata (specifically owner_year,
# owner_share_panel, owner_mean_share, owner_summary, owner_fishery_year,
# fleet_mean_revenue_owner, all built by 01_build_panel.R), not anything from
# 05b_ through 13c_, and 05b_table4_figure3_rolling_owner.R needs only
# 01b_build_rolling_panel_owner.R's own intermediate
# data/ch3_rolling_owner.rdata, so this pair could in principle run
# immediately after 01_build_panel.R, appended here purely to avoid
# disturbing the previously-tested 01b_ through 13c_ order.
#
# THIS CANNOT BE RUN LOCALLY, same as run_all.R, see 00_setup.R.

source("code/empirical_pipeline/rolling_periods/01b_build_rolling_panel.R")
source("code/empirical_pipeline/rolling_periods/05b_table4_figure3_rolling.R")
source("code/empirical_pipeline/rolling_periods/06b_within_season_reallocation_rolling.R")
source("code/empirical_pipeline/rolling_periods/07b_behavioral_heterogeneity_rolling.R")
source("code/empirical_pipeline/rolling_periods/08b_state_contingent_activation_rolling.R")
source("code/empirical_pipeline/rolling_periods/09b_seasonal_overlap_rolling.R")
source("code/empirical_pipeline/rolling_periods/10b_network_similarity_rolling.R")
source("code/empirical_pipeline/rolling_periods/12b_predicted_bh_revenue_rolling.R")
source("code/empirical_pipeline/rolling_periods/13b_predicted_bh_revenue_figures_rolling.R")
source("code/empirical_pipeline/rolling_periods/12c_predicted_bh_phi_rolling.R")
source("code/empirical_pipeline/rolling_periods/13c_predicted_bh_phi_figures_rolling.R")
source("code/empirical_pipeline/rolling_periods/01b_build_rolling_panel_owner.R")
source("code/empirical_pipeline/rolling_periods/05b_table4_figure3_rolling_owner.R")
