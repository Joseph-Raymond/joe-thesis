# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of the regular
# rolling build (not sourced by run_all_rolling.R, run this standalone).
#
# Checks why the owner fixed-effects Phi coefficient in Table 4-rolling
# (owner), column 5, fails roll_phase_check_owner(). The full-panel estimate
# (1.149 as of 2026-10-16) sits below all six non-overlapping phase
# estimates (1.168 to 1.244).
#
# The leading explanation is sample composition. In the full panel an owner
# needs only two eligible windows to stay in an owner fixed-effects fit, and
# an owner with as few as 4 consecutive active years already has about three
# overlapping windows. Inside a phase, windows are 6 years apart, so that
# same owner has at most one window per phase and is dropped as a singleton
# from every phase fit. The phase fits therefore rest only on owners with
# roughly 8 or more active years spread across two separate 6-year blocks
# (median phase retention is about 75 percent against about 99 percent in
# the full panel).
#
# The test refits the full-panel owner FE model on two complementary groups.
#   survivors      owners with at least two windows inside at least one
#                  phase, the owners a phase fit can actually use
#   non-survivors  everyone else
# If the survivors-only estimate lands inside the phase range and the
# non-survivors sit lower, the flag is a composition difference (short-tenure
# owners have a flatter Phi slope), not a problem with the rolling
# construction. If the survivors-only estimate is still below the range,
# composition does not explain it and the overlapping-window structure
# itself is the more likely cause. A tenure-bin breakdown is printed
# alongside as a direct look at whether the slope rises with tenure.
#
# The first row of the output table should reproduce 05b_'s column 5
# (Phi = 1.149) and the six phase rows should reproduce its phase check
# (1.1681 to 1.2437). If they do not, the sample here has drifted from 05b_'s
# and nothing else in the table should be read.
#
# Reads intermediate data/ch3_rolling_owner.rdata (built by
# 01b_build_rolling_panel_owner.R). Writes diagnostic_owner_fe_phase_tenure.csv
# to table_dir, coefficients and counts only, safe to bring off the server.

source("code/empirical_pipeline/00_setup.R")
source("code/empirical_pipeline/rolling_periods/00b_rolling_periods.R")

rolling_owner_panel_path <- file.path(intermediate_dir, "ch3_rolling_owner.rdata")
if (!exists("owner_window_summary.rolling") || !exists("active_owner_years.rolling")) {
  load(rolling_owner_panel_path)
}

# ============================================================================
# 1. Estimation sample and who survives in a phase
# ============================================================================

# Same sample as 05b_table4_figure3_rolling_owner.R's owner_multi.rolling
owner_multi.rolling <- owner_window_summary.rolling %>%
  filter(is.finite(rev.cv), !is.specialist.window) %>%
  mutate(phase = roll_phase(window.start, MIN_YEAR, ROLL_N_PHASES))

# An owner survives in phase p if it has at least two windows in that phase,
# the File.Number singleton rule fixest applies inside each phase fit.
phase_survivors <- owner_multi.rolling %>%
  count(File.Number, phase, name = "n.windows.in.phase") %>%
  group_by(File.Number) %>%
  summarise(n.phases.survived = sum(n.windows.in.phase >= 2), .groups = "drop") %>%
  mutate(phase.survivor = n.phases.survived > 0)

# Tenure is total active years across the whole panel, whatever the fishery mix
owner_tenure <- active_owner_years.rolling %>%
  group_by(File.Number) %>%
  summarise(n.active.years = n_distinct(Batch.Year), .groups = "drop")

TENURE_BREAKS <- c(4, 8, 13, 21, Inf)
TENURE_LABELS <- c("4-7", "8-12", "13-20", "21+")

owner_multi.rolling <- owner_multi.rolling %>%
  left_join(phase_survivors, by = "File.Number") %>%
  left_join(owner_tenure, by = "File.Number") %>%
  mutate(tenure.bin = cut(n.active.years, breaks = TENURE_BREAKS, labels = TENURE_LABELS, right = FALSE))

cat("\n===== Who the phase fits can use =====\n")
print(
  phase_survivors %>%
    left_join(owner_tenure, by = "File.Number") %>%
    group_by(phase.survivor) %>%
    summarise(
      n.owners            = n(),
      min.active.years    = min(n.active.years),
      median.active.years = median(n.active.years),
      .groups = "drop"
    )
)

# ============================================================================
# 2. Fits
# ============================================================================

fml_ownerfe <- rev.cv ~ H_LR + Phi | File.Number + window.start

# Two-way clustering by default, matching 05b_'s full-panel fit. Falls back
# to owner-only clustering if two-way fails, the same fallback
# roll_phase_check_owner() uses. A group too small to fit at all comes back
# as a row of NAs rather than stopping the script.
fit_ownerfe <- function(data, label, cluster = ~File.Number + window.start) {
  cluster_used <- deparse(cluster)
  m <- tryCatch(feols(fml_ownerfe, data = data, cluster = cluster), error = function(e) NULL)
  if (is.null(m)) {
    cluster_used <- "~File.Number"
    m <- tryCatch(feols(fml_ownerfe, data = data, cluster = ~File.Number), error = function(e) NULL)
  }
  if (is.null(m)) return(tibble(sample = label, n.obs = nrow(data)))
  tibble(
    sample   = label,
    n.obs    = nrow(data),
    n.fit    = nobs(m),
    n.owners = n_distinct(data$File.Number),
    H_LR     = unname(coef(m)["H_LR"]),
    se.H_LR  = unname(se(m)["H_LR"]),
    Phi      = unname(coef(m)["Phi"]),
    se.Phi   = unname(se(m)["Phi"]),
    cluster  = cluster_used
  )
}

# Phase fits use owner-only clustering, matching roll_phase_check_owner()
phase_fits <- bind_rows(lapply(sort(unique(owner_multi.rolling$phase)), function(p) {
  fit_ownerfe(owner_multi.rolling %>% filter(phase == p), paste("Phase", p), cluster = ~File.Number)
}))
phase_min <- min(phase_fits$Phi, na.rm = TRUE)
phase_max <- max(phase_fits$Phi, na.rm = TRUE)

tenure_fits <- bind_rows(lapply(TENURE_LABELS, function(b) {
  fit_ownerfe(owner_multi.rolling %>% filter(tenure.bin == b), paste("Full panel, tenure", b, "active years"))
}))

results <- bind_rows(
  fit_ownerfe(owner_multi.rolling, "Full panel (05b_ column 5)"),
  fit_ownerfe(owner_multi.rolling %>% filter(phase.survivor), "Full panel, phase survivors only"),
  fit_ownerfe(owner_multi.rolling %>% filter(!phase.survivor), "Full panel, non-survivors only"),
  phase_fits,
  tenure_fits
) %>%
  mutate(Phi.inside.phase.range = Phi >= phase_min & Phi <= phase_max)

cat("\n===== Owner FE decomposed model, by sample =====\n")
print(results, n = Inf, width = Inf)

# ============================================================================
# 3. Verdict
# ============================================================================

phi_survivors     <- results$Phi[results$sample == "Full panel, phase survivors only"]
phi_non_survivors <- results$Phi[results$sample == "Full panel, non-survivors only"]

cat("\nPhase range for Phi -", round(phase_min, 4), "to", round(phase_max, 4), "\n")
cat("Survivors only -", round(phi_survivors, 4), ", non-survivors only -", round(phi_non_survivors, 4), "\n")

if (isTRUE(phi_survivors >= phase_min && phi_survivors <= phase_max && phi_non_survivors < phi_survivors)) {
  cat("Survivors-only lands inside the phase range and non-survivors sit lower.",
      "The flag looks like a composition difference (short-tenure owners have a flatter Phi slope),",
      "not a problem with the rolling construction.\n")
} else if (isTRUE(phi_survivors < phase_min)) {
  cat("Survivors-only is still below the phase range, so composition does not explain the flag.",
      "The overlapping-window structure itself is the more likely cause.\n")
} else {
  cat("Mixed result, neither reading fits cleanly. Read the table directly.\n")
}

write.csv(results, file.path(table_dir, "diagnostic_owner_fe_phase_tenure.csv"), row.names = FALSE)
cat("Wrote diagnostic_owner_fe_phase_tenure.csv\n")
