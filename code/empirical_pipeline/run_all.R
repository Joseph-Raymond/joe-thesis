# Chapter 3 empirical pipeline, master script
#
# Runs 01_build_panel.R through 11_switch_event_weights.R in order. Each
# script sources 00_setup.R itself and reloads whatever intermediate data it
# needs, so any of them can also be source()-d on its own instead of through
# this file, see the run order notes in README.md. 11_switch_event_weights.R
# is numbered last for convenience only, its real dependencies are 01_ and
# 06_, not 07 through 10, see its own header.
#
# THIS CANNOT BE RUN LOCALLY, same as every other script in this folder, see
# 00_setup.R.

source("code/empirical_pipeline/01_build_panel.R")
source("code/empirical_pipeline/02_table1_table2.R")
source("code/empirical_pipeline/03_figure1_figure2.R")
source("code/empirical_pipeline/04_table3.R")
source("code/empirical_pipeline/05_table4_figure3.R")
source("code/empirical_pipeline/06_within_season_reallocation.R")
source("code/empirical_pipeline/07_behavioral_heterogeneity.R")
source("code/empirical_pipeline/08_state_contingent_activation.R")
source("code/empirical_pipeline/09_seasonal_overlap.R")
source("code/empirical_pipeline/10_network_similarity.R")
source("code/empirical_pipeline/11_switch_event_weights.R")
