# Chapter 3 empirical pipeline, validation check for Section 5/6's classifier
#
# Table, appendix candidate. Does a vessel's own within-season target
# switching respond to that same vessel's own leave-one-out primary-fishery
# shock, or is switching better explained as a fixed calendar habit (a
# vessel running two seasonally back-to-back fisheries every year switches
# the same amount regardless of how either fishery is doing)? Section 6
# treats tau as a behavioral trait, but nothing in the chapter as written
# tests behavior against the calendar alternative directly. This script is
# that test, reusing Section 7's own shock construction rather than building
# a second one.
#
# NUMBERED 06c FOR FAMILY, RUN ORDER IS AFTER 08. Despite the "06" name
# (it validates 06_'s own switching measure), this script's real dependency
# is 08_state_contingent_activation.R's vessel_year_shock object, not
# anything from 07_ or 09_ through 11_. Mirrors 11_switch_event_weights.R's
# own numbering-versus-dependency mismatch, see that script's header.
#
# WHERE THIS GOES IN THE WRITEUP, once real numbers exist. Appendix
# subsection "Construction and inference detail for Sections~\ref{sec:ch3-
# decomp} through~\ref{sec:ch3-heterogeneity}" (chapter3_writeup.tex,
# app:ch3-construction), as a new \paragraph right after the existing
# switch-event network-distance weighting paragraph, since that is the
# appendix's other check on what the switching measure is actually
# capturing. Do NOT wire in the \input until this has actually been rerun,
# same reasoning as every other new exhibit added this pass.
#
# Reads intermediate data/ch3_within_season.rdata (switching_by_vessel_year,
# built by 06_within_season_reallocation.R) and intermediate
# data/ch3_activation.rdata (vessel_year_shock, built by
# 08_state_contingent_activation.R).

source("code/empirical_pipeline/00_setup.R")

within_season_path <- file.path(intermediate_dir, "ch3_within_season.rdata")
if (!exists("switching_by_vessel_year")) load(within_season_path)

activation_path <- file.path(intermediate_dir, "ch3_activation.rdata")
if (!exists("vessel_year_shock")) load(activation_path)

# ============================================================================
# 1. Sample
# ============================================================================
#
# vessel_year_shock is already restricted to each vessel's second half of
# active years (the predetermined-primary design Section 7 uses), so this
# inherits that same restriction rather than testing on the first-half years
# the primary fishery itself was fixed from. inner_join rather than
# left_join, a vessel-year with no computable switching measure (fewer than
# 2 active weeks, see 06_'s own construction) or no computable shock (sole
# lander in its own primary fishery that year) contributes nothing to either
# side of this test and is dropped rather than kept with an NA.
tau_shock_data <- switching_by_vessel_year %>%
  inner_join(vessel_year_shock, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
  filter(is.finite(shock))

cat("Vessel x year observations entering the tau-shock validation check:", nrow(tau_shock_data),
    " distinct vessels:", n_distinct(tau_shock_data$Vessel.ADFG.Number), "\n")

# ============================================================================
# 2. Regression
# ============================================================================
#
# Vessel and year fixed effects, not Section 7's own Vessel.ADFG.Number +
# fishery.year. The outcome here is a vessel-year scalar (switching has no
# second "which fishery" dimension the way activation does), so a
# fishery.year cell has no meaning to attach to it, ordinary Batch.Year does
# the same job of absorbing whatever moved every vessel in the fleet at once
# in a given year. The vessel fixed effect is what makes this a real test,
# it asks whether THIS vessel switches more in ITS OWN worse years, not
# whether vessels with worse average shocks happen to switch more, which a
# fixed calendar rotation confounded with fishery choice could produce on
# its own.
#
# n.active.weeks enters as a control for the same mechanical reason it does
# in Table 6, switching accumulates over week-to-week transitions and is
# larger for a vessel that simply fishes more weeks.
#
# Clustered on primary.fishery, matching Section 7's own reasoning exactly
# (shock is a leave-one-out fleet-wide mean at the (primary.fishery,
# Batch.Year) level, so a vessel-clustered standard error would treat every
# vessel sharing a primary fishery in a year as an independent draw of what
# is one common shock).
#
# tau (per-transition normalized) is the headline outcome, since that is
# Section 6's actual classifier. Raw weekly.switching is reported alongside
# it for completeness, not because the chapter uses it as a classifier
# anywhere.
model_tau_on_shock <- feols(
  weekly.switching.per.transition ~ shock + n.active.weeks | Vessel.ADFG.Number + Batch.Year,
  data = tau_shock_data, cluster = ~primary.fishery
)
model_switching_on_shock <- feols(
  weekly.switching ~ shock + n.active.weeks | Vessel.ADFG.Number + Batch.Year,
  data = tau_shock_data, cluster = ~primary.fishery
)

# A negative shock coefficient is the signature a behavioral reading
# predicts, a worse year in the vessel's own primary fishery (shock more
# negative) raising how much it reallocates that same year (switching
# higher). A coefficient indistinguishable from zero does not prove tau is
# purely calendar, but it removes the one piece of direct evidence that
# would have favored behavior over calendar, and leaves the calendar
# alternative standing.
tau_shock_dict <- c(shock = "Shock (own primary fishery)", n.active.weeks = "Active weeks")

print(etable(model_tau_on_shock, model_switching_on_shock,
             headers = c("Tau (per-transition)", "Weekly switching (raw)"),
             dict = tau_shock_dict))

etable(
  model_tau_on_shock, model_switching_on_shock,
  headers = c("Tau (per-transition)", "Weekly switching (raw)"),
  dict = tau_shock_dict,
  tex = TRUE,
  file = file.path(table_dir, "table_tau_shock_validation.tex"),
  replace = TRUE
)

cat("Wrote table_tau_shock_validation.tex to", table_dir, "\n")
