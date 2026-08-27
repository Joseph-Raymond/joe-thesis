# Chapter 3 empirical pipeline, rolling-window (vessel-period) master script
#
# Runs the rolling-window parallel analysis, 01b_build_rolling_panel.R
# through 10b_network_similarity_rolling.R, in order. Mirrors run_all.R's own
# structure exactly (source() with no local =, so every script shares one
# global session and each wipes it at its own start via
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
# THIS CANNOT BE RUN LOCALLY, same as run_all.R, see 00_setup.R.

source("code/empirical_pipeline/rolling_periods/01b_build_rolling_panel.R")
source("code/empirical_pipeline/rolling_periods/05b_table4_figure3_rolling.R")
source("code/empirical_pipeline/rolling_periods/06b_within_season_reallocation_rolling.R")
source("code/empirical_pipeline/rolling_periods/07b_behavioral_heterogeneity_rolling.R")
source("code/empirical_pipeline/rolling_periods/08b_state_contingent_activation_rolling.R")
source("code/empirical_pipeline/rolling_periods/09b_seasonal_overlap_rolling.R")
source("code/empirical_pipeline/rolling_periods/10b_network_similarity_rolling.R")
