# Chapter 3 empirical pipeline, rolling-window (vessel-period) analysis

A parallel analysis alongside the vessel-level baseline (`README.md`), where
the unit of observation becomes a **vessel-window**, a vessel observed
inside one particular rolling 6-year window, active in at least 4 of the 6
years, rather than a vessel aggregated over its whole panel lifetime. This
answers a question the baseline cannot ask, whether the relationships in
Tables 4 through 12 hold **within a vessel's own career over time**, not
just across vessels averaged over their whole histories. See
`rolling_periods_design.md` (the design document this implementation
follows) for the full reasoning. This file documents what was actually
built, the run order, and what was deliberately left out.

Every file here is new and additive. **Zero edits were made to any existing
script** (`00_setup.R` through `09_seasonal_overlap.R`, `run_all.R`), and no
existing file in `Chpt3/output/` is ever overwritten, every rolling output
filename carries the word "rolling."

## Window definition and eligibility

A true sliding window, stride 1 year, width 6, on a **common calendar
grid** (every vessel's "1997-2002" is the same six calendar years). With
`MIN_YEAR = 1991` and `MAX_YEAR` read from the saved panel (never
hardcoded), the grid runs `window.start` 1991 through `MAX_YEAR - 5`.
Trailing partial windows are dropped, a window must have all 6 calendar
years inside `[MIN_YEAR, MAX_YEAR]`.

A vessel is **eligible** in a window if it has at least `ROLL_MIN_ACTIVE_YEARS`
(4) **active** years inside that window, where "active" is the pipeline's
existing `vessel.year.rev > 0`. This is a count, not a contiguity
requirement. All `ROLL_*` constants live in `00b_rolling_periods.R`.

**The single most important rule in this pipeline**: `meets.min.years` (the
baseline's lifetime 5-year floor) is never applied anywhere in the rolling
scripts. Eligibility is built from `active_vessel_years`/`vessel_share_panel`
directly. Applying `meets.min.years` would silently make the rolling sample
a strict subset of the baseline sample and defeat the point of the whole
exercise, since a vessel with exactly 4 contiguous active years (a large
share of the fleet, given a median tenure of 4) clears the rolling floor but
fails the baseline's lifetime floor entirely.

The rolling sample is **neither a subset nor a superset** of the baseline
sample, it trades lifetime breadth for local density. `01b_build_rolling_panel.R`
prints and exports a mandatory attrition ladder
(`table_rolling_sample_attrition.tex`) and an eligible-vessels-by-window
figure (`figure_rolling_eligible_vessels_by_window.png`) so this is visible
rather than assumed, **inspect both before trusting anything downstream**.
If eligible vessels come back far below 8,000 or eligible vessel-windows far
below 40,000, `01b_` prints a loud warning, this is a decision point for the
author.

## Inference protocol

Every rolling regression in this pipeline follows the same three layers:

1. **`window.start` in the fixed-effect slot**, absorbing fleet-wide common
   movement across windows (a bad fleet-wide year enters up to 6
   consecutive overlapping windows for every vessel simultaneously
   otherwise).
2. **Two-way clustering**, `cluster = ~Vessel.ADFG.Number + window.start`.
   The vessel dimension handles the mechanical within-vessel overlap
   between adjacent windows exactly. The window dimension is a
   floor-raising correction for residual common-time correlation, not a
   fully reliable asymptotic object on its own (only 20-26 window clusters).
   Falls back to vessel-only clustering when the two-way variance matrix
   cannot be inverted.
3. **A mandatory stride-6 non-overlapping phase check** on every headline
   model, via `roll_phase_check()` in `00b_rolling_periods.R`. Splits the
   sample into 6 phases of genuinely non-overlapping windows (phase 0 is
   1991-1996, 1997-2002, ...), refits the identical specification on each,
   and reports the full-panel two-way-clustered estimate against the phase
   min/median/max. **Reading rule**: if `SE_full` sits near `SE_phase`, the
   vessel clustering is doing its job, if it sits near `SE_phase / sqrt(6) ~
   0.41 * SE_phase`, the rolling panel is manufacturing precision and the
   phase SEs are the ones to trust. If the full-panel point estimate falls
   outside the phase min-max range, `roll_phase_check()` prints a loud
   warning, that model needs inspection before it is trusted. All results
   accumulate into one table, `table_rolling_overlap_robustness.tex`,
   rebuilt (not overwritten with a partial version) by every script that
   adds a row to it.

One inverse-window-count weighting robustness column (`weights =
~inv.window.count`) is added to the Table 4-rolling and Table 7-rolling
headline models, restoring the baseline's one-vessel-one-vote estimand as a
check (unweighted vessel-window regressions give a 26-window vessel 26 times
the weight of a 3-window vessel).

## Naming convention

- **Data objects** carry a `.rolling` suffix (`vessel_window_summary.rolling`,
  `table7_data.rolling`, ...).
- **Columns** that are a window-local recomputation of a baseline quantity
  keep the baseline name (`H_bar`, `H_LR`, `Phi`, `rev.cv`), so the
  regression formulas are literally identical strings to the baseline's.
  New columns get descriptive names (`window.start`, `tau.window`,
  `is.specialist.window`, `predetermined.primary.window`, ...).
- **Model objects** are prefixed `m_` and suffixed `_roll` (`m_baseline_roll`,
  `m_interaction_vfe_roll`, ...), deliberately different from the baseline's
  `model_*` scheme.
- **Functions** are prefixed `roll_`.
- **Output files** carry "rolling" in the filename, never overwriting a
  baseline file.
- No script here ever assigns to any name on the design document's
  do-not-reassign list (Section 8.3), and none defines a function called
  `period_of`.

## Run order

`run_all.R` **must be run first**, through at least
`06_within_season_reallocation.R` (the rolling scripts read `intermediate
data/ch3_panel.rdata` and `intermediate data/ch3_within_season.rdata`
read-only). Then:

```
run_all_rolling.R                              # sources everything below in order
  01b_build_rolling_panel.R                     # window grid, eligibility, attrition ladder -- inspect before proceeding
  05b_table4_figure3_rolling.R                  # Table 4-rolling (+pooled), Figure 3/3b-rolling, figure4b path
  06b_within_season_reallocation_rolling.R      # Table 6-rolling, tau_by_vessel_window.rolling (needed by 07b_)
  07b_behavioral_heterogeneity_rolling.R        # Table 7-rolling, Table 8-rolling, figure8b path
  08b_state_contingent_activation_rolling.R     # Table 10/11-rolling, figure10b path
  09b_seasonal_overlap_rolling.R                # Table 12-rolling, pre/post-1995 overlap diagnostic
  10b_network_similarity_rolling.R              # Table 13-rolling (needs 08b_'s saved activation panel)
```

There is no `11b_`. Figure 7 (`11_switch_event_weights.R` in the baseline
chain, switch events between consecutive trips weighted by co-participation
network distance) is a pooled distribution over switch events, no fixed
effect, no regression, and the network it weights by is all-years-pooled
and period-invariant by explicit design in both `10_` and `10b_`. It carries
no period structure to roll, the same reasoning that already keeps Figure
5/Figure 6 out of this rolling chain (see `06b_`'s own header). It runs once,
in the baseline chain, and applies unchanged regardless of which of
baseline/rolling is primary for the chapter.

`10b_` is a rolling-window twin of the baseline's `10_network_similarity.R`, added after the
baseline Table 13 addition to the writeup, not part of the original three-tier build. The
co-participation network itself stays all-years pooled, unchanged from baseline, same reasoning
`09b_` gives for keeping seasonal overlap pooled rather than rebuilt per window. Only the
regression is refit on the rolling activation sample, with `window.start` added to the fixed
effects and to clustering. Unlike `09b_`'s own Table 12-rolling, `10b_` runs the mandatory
stride-6 phase check (`roll_phase_check()`) on every headline coefficient it reports, six rows
appended to `table_rolling_overlap_robustness.tex`, since the whole point of adding it was to
check whether the two-way-clustered standard errors on the network results are trustworthy or
whether the overlapping-window stacking is manufacturing precision. `09b_`'s own Table 12-rolling
was never run through this check and is a candidate for the same treatment later.

`06b_` must run before `07b_` (07b_ loads `ch3_rolling_tau.rdata`, built by
06b_). Every other script only depends on `01b_`'s `ch3_rolling.rdata`
and/or the baseline's own saved intermediate data.

Every script wipes the global environment at its own start (each begins
`source("00_setup.R")`, which calls `rm(list = ls())`), then re-sources
`00b_rolling_periods.R` for the shared constants/functions, exactly
mirroring how the baseline pipeline already works. Anything a rolling
script needs from an earlier rolling script is persisted to its own
`intermediate data/ch3_rolling*.rdata` file rather than assumed to still be
in memory.

## What got built, and at what tier

Built in the priority order the design document's Section 9.2 specifies.

**Tier 1** (complete): the attrition ladder and eligible-vessels figure,
Table 6-rolling (all four columns, including the vessel-FE pair), Table
7-rolling (all six columns, including column 6, the within-vessel
interaction), `figure8b_slope_by_type_path_rolling.png`, Table 4-rolling
(baseline/decomposed/standardized/vessel-FE/weighted-robustness columns,
`g2_share` recomputed), and `table_rolling_overlap_robustness.tex`.

**Tier 2** (complete): Table 8-rolling (adjacent-window predetermination,
all four columns plus the strict-lookback robustness line), Figure
3b-rolling (two-stage vessel-clustered bin SEs), `figure4b_decomposition_path_rolling.png`,
and Section 7's activation design (`08b_`, per-window regressions +
`figure10b_activation_path_rolling.png`, the pooled Table 10-rolling, the
pooled Table 11-rolling placebo).

**Tier 3** (complete, lighter-touch than Tier 1/2): Table 4-pooled-rolling,
Figure 3-rolling (plain scatter), `09b_seasonal_overlap_rolling.R` (Table
12-rolling pooled, plus the one pre-1995-vs-post-1995 overlap diagnostic).

**Tier 3, NOT built, left for a follow-up pass**: Figure 8-rolling in the
baseline's own four-slope-vs-Chapter-2 layout, Figure 9-rolling and Table
9-rolling (the Phi-sorted appendix check). `table7_data.rolling` and
`table8_data.rolling` are already saved to `intermediate
data/ch3_rolling_behavioral.rdata` by `07b_`, so a follow-up pass building
these does not need to reconstruct the classifier/sample machinery from
scratch, it can load that file directly.

## What was deliberately never built, and why (design Section 9.2)

- **Figure 3-appendix, specialists only, rolling version.** That figure's
  whole purpose is to characterize a **lifetime** specialist's idiosyncratic
  noise floor. A within-window specialist is a different, much less
  interesting population (window-local specialization is common and
  temporary, not evidence of a career-long strategy).
- **Figures 5, 5b, and 6, rolling versions.** Fleet-level distributions with
  no vessel-period unit to change.
- **Tables 1, 2, and 3, rolling versions.** Out of scope, already at
  vessel-year or owner-year grain, nothing to localize to a window.
- **Any owner-level rolling object.** Nothing from Table 4 onward reads
  `owner_summary` or `owner_period_summary` in either the baseline or the
  rolling pipeline.
- **26 rolling seasonal-overlap matrices.** A fishery's season is treated as
  a fixed calendar characteristic on purpose (design Section 3.7), pooled
  fleet-wide across all years specifically so no single vessel's own timing
  drives the signature used to explain that vessel's own choices. A 6-year
  rolling version would reintroduce exactly the leave-one-out concern the
  pooling exists to avoid. Handled instead with the one pre/post-1995
  diagnostic in `09b_`.
- **Twenty per-window Table 11 placebos.** Table 11-rolling is pooled only
  (design Section 6.3), the placebo's identifying logic (does next year's
  shock add anything beyond this year's) does not need or benefit from a
  per-window split the way Table 10's headline coefficient does.
- **Recomputing the Table 10 shock's `ref.mean`/`ref.sd` within a window.**
  Deliberately unchanged, full-series only (design Section 3.6), the
  reference exists to put a fishery-year on that fishery's own long-run
  scale, recomputing it from 6 points would redefine "a bad year" as "bad
  relative to these six years" and rest `ref.sd` on far too little data.
- **Aggregating Section 7's activation outcome to the window grain.** The
  unit stays vessel-fishery-year throughout (design Section 6.1), activation
  is a discrete yearly event whose whole logic is a within-year response to
  a within-year shock, and Table 11's placebo is a lead-lag test on the year
  index, both would be destroyed by a 6-year rate.
