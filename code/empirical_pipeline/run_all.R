# Chapter 3 empirical pipeline, master script
#
# Runs 01_build_panel.R through 11_switch_event_weights.R in order. Each
# script sources 00_setup.R itself and reloads whatever intermediate data it
# needs, so any of them can also be source()-d on its own instead of through
# this file, see the run order notes in README.md. 11_switch_event_weights.R
# is numbered last for convenience only, its real dependencies are 01_ and
# 06_, not 07 through 10, see its own header.
#
# 05_table4_figure3_owner.R is the owner-level (File.Number = permit holder)
# twin of 05_table4_figure3.R, run immediately after it since both read the
# same intermediate data/ch3_panel.rdata and neither depends on anything
# from 06_ onward. See its own header for why "owner" means permit holder,
# not vessel owner.
#
# 04b_table_unused_by_fishery.R is a by-fishery breakdown of 04_table3.R's
# own fleet-wide unused-share number, run immediately after it for the same
# reason 05_table4_figure3_owner.R runs immediately after 05_. 04c_ is the
# by-year companion to 04b_'s by-fishery ranking, same top fisheries, run
# right after it. 04d_ is a second by-fishery companion, a scatter of held
# owner-years against unused share, complementary to 04b_'s table rather
# than a duplicate of it, 04b_ ranks by SHARE (top 15), which surfaces the
# small administrative/data-gap codes, 04d_ shows every fishery at once so
# the large fisheries that carry the wedge in absolute terms (bubble size)
# are visible too, run right after 04c_ since all three share one ranking
# logic over the same owner_fishery_year population. 04e_ is a different
# cut of the same Table 3 wedge, split by the owner's residency instead of
# by fishery, run right after 04d_ for the same "companion to Table 3"
# reason as the rest of the 04_ group, see its own header for the match-
# rate confound it checks before reading anything into a residency gap.
#
# THIS CANNOT BE RUN LOCALLY, same as every other script in this folder, see
# 00_setup.R.

source("code/empirical_pipeline/01_build_panel.R")
source("code/empirical_pipeline/02_table1_table2.R")
source("code/empirical_pipeline/03_figure1_figure2.R")
source("code/empirical_pipeline/04_table3.R")
source("code/empirical_pipeline/04b_table_unused_by_fishery.R")
source("code/empirical_pipeline/04c_figure_unused_by_fishery_year.R")
source("code/empirical_pipeline/04d_figure_unused_share_vs_held.R")
source("code/empirical_pipeline/04e_table_wedge_by_residency.R")
source("code/empirical_pipeline/05_table4_figure3.R")
source("code/empirical_pipeline/05_table4_figure3_owner.R")
source("code/empirical_pipeline/06_within_season_reallocation.R")
source("code/empirical_pipeline/07_behavioral_heterogeneity.R")
source("code/empirical_pipeline/08_state_contingent_activation.R")
source("code/empirical_pipeline/09_seasonal_overlap.R")
source("code/empirical_pipeline/10_network_similarity.R")
source("code/empirical_pipeline/11_switch_event_weights.R")
