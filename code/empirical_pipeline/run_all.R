# Chapter 3 empirical pipeline, master script
#
# Runs 01_build_panel.R through 09_seasonal_overlap.R in order, each as its
# own Rscript process. Every script sources 00_setup.R and starts with
# rm(list = ls()), so running as fresh subprocesses (rather than source()-ing
# all nine into one session) is what each script already expects. Stops at
# the first failure rather than continuing on to scripts downstream of it.
#
# THIS CANNOT BE RUN LOCALLY, same as every other script in this folder, see
# 00_setup.R. Run from the server, from anywhere, e.g.
# Rscript code/empirical_pipeline/run_all.R

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")

scripts <- c(
  "01_build_panel.R",
  "02_table1_table2.R",
  "03_figure1_figure2.R",
  "04_table3.R",
  "05_table4_figure3.R",
  "06_within_season_reallocation.R",
  "07_behavioral_heterogeneity.R",
  "08_state_contingent_activation.R",
  "09_seasonal_overlap.R"
)

t0 <- Sys.time()

for (s in scripts) {
  path <- file.path("code/empirical_pipeline", s)
  cat("\n============================================================\n")
  cat("Running", s, "\n")
  cat("============================================================\n")

  t_start <- Sys.time()
  status <- system2("Rscript", shQuote(path))
  elapsed <- round(difftime(Sys.time(), t_start, units = "mins"), 1)

  if (status != 0) {
    stop(s, " failed (exit code ", status, ") after ", elapsed,
         " min. Stopping pipeline.")
  }
  cat(s, "finished in", elapsed, "min\n")
}

cat("\nAll", length(scripts), "scripts finished in",
    round(difftime(Sys.time(), t0, units = "mins"), 1), "min total\n")
