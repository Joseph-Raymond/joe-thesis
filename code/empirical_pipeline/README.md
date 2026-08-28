# Chapter 3 empirical pipeline (Sections 2-7)

Reproducible R scripts for the figures and tables in `Chapter3_outline.md`,
Section 2 (Table 1, Table 2), Section 3 (Figure 1, Figure 2, Table 3),
Section 4 (Table 4, Figure 3), Section 5 (Figure 5, Figure 6, Table 6,
Figure 7), Section 6 (Table 7, Table 8, Figure 8, Figure 9), and Section 7
(Table 10, Figure 10, Table 11, Table 12, and Table 13). Every item marked
`[maybe]` in the outline other than Figure 7 (so Table 3b, Figure 4, and
Table 9) and the "if I have time" block at the end of Section 6 are skipped, not
built here. Table 2, Figure 7, Table 12, and Table 13 are the exceptions,
Table 2 turned out cheap enough to build alongside Table 1, and Table 12
became buildable once `09_seasonal_overlap.R` gave it an interaction
ingredient neither originally-scoped one (the Section 5 co-participation
network, the Figure 3 return correlation) ever was, see that script's
header. `09_seasonal_overlap.R` also builds Figure 11, which is not in the
outline at all, a re-cut of Section 3's wedge by whether a held-but-unfished
permit's season conflicted with what the vessel actually fished that year.

`10_network_similarity.R` builds the second of those two originally-scoped
interaction ingredients, the Section 5 co-participation network (Kroetz et al.
2019, Addicott et al. 2018 style), previously listed below as skipped
entirely for `06_within_season_reallocation.R`'s purposes. It is now built
off the saved panel alone for Table 13, a simple pairwise Jaccard similarity
of co-held vessel sets rather than the closeness-centrality version the
outline names, see that script's header for why. This adds Table 13 (a
three-column extension of Table 12) and the optional Table 14 (a small
face-validity exhibit), neither in the outline.

`11_switch_event_weights.R` builds Figure 7 (switch events between
consecutive trips, weighted by the same co-participation distance measure),
using a locally rebuilt copy of `10_`'s network rather than a shared object,
see that script's header for why. It runs at trip grain
(`Vessel.ADFG.Number` x `Date.Landed`, matching `get.trip()` in
`myfunctions.R`) rather than the week grain `06_within_season_reallocation.R`
uses for Figure 5/6/Table 6, and is descriptive only, no table, no
regression, matching the outline's own scope for this item.

## Rerun 01 through 08 before trusting ANY existing output, `H_LR` was wrong

A methodological review caught a real bug in `H_LR`, present since before this
pipeline's Section 5-7 additions and used by every script that reads
`vessel_summary`/`vessel_period_summary`/`owner_summary`/`owner_period_summary`.
`H_LR = sum(unique(mean.share.fishery)^2)` sums over DISTINCT VALUES of the
long-run mean share, not over distinct FISHERIES the way `H_LR = sum_j
(mean_t s_ijt)^2` requires, so any two fisheries that happen to land on the
same long-run mean share silently collapsed into one term. This is not a
floating-point curiosity, a vessel that alternates between two fisheries in
an even split gets IDENTICAL means by construction (a synthetic check found
`H_LR = 0.25` instead of the correct `0.5` for a vessel splitting its years
evenly between two fisheries), which understates `H_LR` and inflates `Phi =
H_bar - H_LR` for exactly the switcher vessels this chapter is about. Fixed
in `01_build_panel.R` (all four locations, vessel/vessel-period/owner/owner-period)
by summing `mean.share.fishery[!duplicated(Fishery)]^2` instead.

This means every already-generated table and figure that touches `Phi`
(Table 4, Figure 3b, and anything a future run of Table 6/Figure 9 would
produce) was built on a biased `Phi` and needs to be regenerated. The
existing output in `Chpt3/output/` (and the numbers already written into
`writeup/chapter3_writeup.tex`) predate this fix. Re-run `01_build_panel.R`
first, then everything downstream.

A second bug, in `08_state_contingent_activation.R`'s leave-one-out shock,
is also fixed, see the "Section 7" bullet below.

## This cannot be run locally

Per `chapter3_plan.md` Section 0.3, the CFEC/AKFIN data lives only on the
secure server and never leaves it. Move this folder to
`~/Rprojects/joe-thesis/code/empirical_pipeline/` on the server and run it
there.

```
ssh -L localhost:8989:localhost:3389 jraymond@makena.ucdavis.edu
# then connect to localhost:8989
```

These scripts were written by reading `code/Permit_Linking/permit_link.R`,
`code/myfunctions.R`, `code/data load module.R`, `code/vessel_clean.R`, and
`code/Permit_Variance.R` for column names and cleaning conventions, and by
following `Chapter3_outline.md` and `chapter3_plan.md` for what each figure
and table needs to show. `00_setup.R`, `01_build_panel.R`, and
`02_table1_table2.R` have since been run against real data on the server and
produced sane-looking output (see `match_diag`/Table 2's real numbers, a
96% ticket-to-register match rate, 28% of permit register rows missing a
vessel ID, 0.7% of ticket rows zero-filled, 87% of those zero-filled rows
carrying positive landed weight), catching and fixing two real bugs along
the way (`ticket_serial_match_rate` was measuring field completeness rather
than an actual register join, and `case_match()` triggered a dplyr
deprecation warning on the server's dplyr version). `00_setup.R` through
`05_table4_figure3.R` have since been run against real data and produced the
output in `Chpt3/output/`, including the trailing-year coverage fix in
Section 2b of `01_build_panel.R`. `06_within_season_reallocation.R` through
`08_state_contingent_activation.R` have not been run yet. Search for "CHECK:"
comments and read every script's header before trusting output from those,
several column names and one file path are inferred rather than confirmed
against real headers (`chapter3_plan.md` Section 1 says the same about the
existing code).

## Run order

```
00_setup.R                       # sourced automatically by every other script, not run directly
01_build_panel.R                 # builds intermediate data/ch3_panel.rdata, run this first
02_table1_table2.R                # Table 1, Table 2
03_figure1_figure2.R              # Figure 1, Figure 2 (needs the CFEC vessel register, see below)
04_table3.R                       # Table 3
05_table4_figure3.R               # Table 4, Figure 3, Figure 3 appendix (specialists)
06_within_season_reallocation.R   # Figure 5 (+ pseudo-log version), Figure 6, Table 6
07_behavioral_heterogeneity.R     # Table 7, Table 8, Figure 8, Figure 9 (appendix)
08_state_contingent_activation.R  # Table 10, Figure 10, Table 11
09_seasonal_overlap.R             # Table 12, Figure 11
10_network_similarity.R           # Table 13, Table 14
11_switch_event_weights.R         # Figure 7, Figure 7b
```

`11_switch_event_weights.R` is numbered last purely for `run_all.R`
ordering convenience. Its real dependencies are `01_build_panel.R`
(`vessel_fishery_year`, for a locally rebuilt copy of the network) and
`06_within_season_reallocation.R` (`switching_by_vessel_year`, for a
face-validity cross-check), not 07 through 10, see its own header for why
it does not load anything saved by `10_network_similarity.R` even though
both build the same network.

Each of 02 through 05 loads `intermediate data/ch3_panel.rdata` if the panel
objects are not already in memory, so they can be run independently in a
fresh session as long as 01 has been run at least once. Outputs land in
`Chpt3/output/tables/` (`.tex`, via `xtable` and `fixest::etable`) and
`Chpt3/output/figures/` (`.png`, via `ggplot2`).

06 through 09 also need `01_build_panel.R` to have run at least once (for
`period_bounds`, `vessel_summary`, `vessel_fishery_year`, `vessel_share_panel`,
`vessel_year`, and `MAX_YEAR`), but reload the raw
`intermediate data/catch_data_temp.rdata` ticket file directly on top of
that, since week-level granularity never makes it into the saved panel, only
annual revenue aggregates do (`Pounds..Detail.` also lives only in the raw
ticket file, not the saved panel). `06_within_season_reallocation.R` and
`09_seasonal_overlap.R` both need `Statistical.Week`, but `catch_data_temp`
has no such column, or `Week.Ending.Date` either, checked directly against
the real object on the server, only `Date.Landed`, `Date.Fishing.Began`, and
`Batch.Year` are actually there. Both scripts now derive `Statistical.Week`
from `Date.Landed` via `derive_statistical_week()` in `00_setup.R` (a
day-of-year `%/% 7` bucket, 1-53), which is a proxy, not ADFG's own
regulatory statistical-week code, see that function's comment for why.
`06_within_season_reallocation.R`
additionally saves `intermediate data/ch3_within_season.rdata`
(`switching_by_vessel_year`, `season_windows`), which `07_behavioral_heterogeneity.R`
loads rather than redoing that reload, so 06 needs to run before 07. 08 does
its own independent reload and does not depend on 06 or 07, but now saves
`intermediate data/ch3_activation.rdata` (`activation_data`), which
`09_seasonal_overlap.R` loads for Table 12, so 08 needs to run before 09.
`09_seasonal_overlap.R` in turn now saves `intermediate data/ch3_seasonal_overlap.rdata`
(`overlap_long`), which `10_network_similarity.R` loads alongside `ch3_panel.rdata`
and `ch3_activation.rdata`, so 09 needs to run before 10. `10_network_similarity.R`
needs no raw ticket reload at all, everything it builds comes off these three
saved objects, so it runs in seconds.

`01_build_panel.R` also saves `vessel_period_summary`/`owner_period_summary`,
`H_bar`/`H_LR`/`Phi`/`rev.cv` computed separately within three roughly-equal
calendar periods (`N_PERIODS` in `00_setup.R`) rather than over each
vessel/owner's whole active history, see Section 6b/7b of that script and
`NOTES_prior_prototype.md`. Nothing in 02-05 reads from these yet, they are
available for whichever downstream table or figure ends up using them.

`Chpt3/output/figures/` (`.png`, via `ggplot2`).

## What is genuinely new here versus the existing `Chpt3/code/` scripts

`permit_link.R` already computes most of the raw ingredients (held permits,
fished permits, per-fishery revenue shares, annual HHI) but does it
interactively, in one long script, with the vessel-ID-unmatched permits
dropped before anything downstream sees them, and without the `H_bar / H_LR /
Phi` decomposition. This pipeline

- keeps the vessel-ID-unmatched permits through Section 5 of
  `01_build_panel.R` instead of dropping them immediately, so Table 3 can
  compare the wedge with and without them,
- builds `H_bar`, `H_LR`, and `Phi` per vessel exactly as defined in
  `chapter3_plan.md` Section R2 (`H_bar = mean_t sum_j s_jt^2`, `H_LR = sum_j
  (mean_t s_jt)^2`, `Phi = H_bar - H_LR`), including the zero-fill step for
  years a vessel was active but did not fish a fishery it fished in some
  other year of its own panel, which the old code does not do,
- runs Table 4 in levels rather than logs, since `H_bar = H_LR + Phi` is
  additive and the decomposition claim only holds in levels
  (`chapter3_plan.md` Section 9.2 flags this as a live inconsistency in the
  existing code), and
- adds the passive buy-and-hold benchmark (Figure 3), which does not exist
  anywhere in `Chpt3/code/`, and
- links the owner-level held and fished panels on the permit holder's own ID
  (`File.Number` from the permit register, matched to
  `CFEC.Permit.Holder.Filing.Number` on fish tickets), not the vessel
  owner's ID (`CFEC.Vessel.Owner.Filing.Number`). The two are not always the
  same person, and an earlier version of this pipeline used the vessel-owner
  field by mistake, which would have misattributed fished revenue to the
  wrong owner whenever a permit holder fishes on a vessel they do not own.

## Design choices worth reviewing before trusting the output

These are places the outline and plan leave a genuine judgment call, not
places where I guessed at a fact. Each is also flagged with a comment at the
point it is made in the code.

- **Table 3's "with/without missing vessel ID" comparison is built at the
  owner (`File.Number`) level.** A permit with no vessel attached has no
  vessel to be a vessel-year row for, so the vessel-level panel can never
  include it. The owner is the only unit that can hold it at all.
- **Held and fished sets are defined at the Fishery-class level** (e.g.
  `"S03T"`) for every share-based object (`H_bar`, `H_LR`, `Phi`,
  `vessel_mean_share`), matching how `permit_link.R` already computes HHI, a
  share is inherently per fishery. The permit-COUNT objects are built at
  BOTH the Fishery-class level and the individual permit-serial level (see
  Section 4b/Section 7 of `01_build_panel.R`), so `unused.count.share` and
  `unused.count.share.permit` sit side by side in `vessel_year`/`owner_year`
  and Figure 1 plots both. A vessel holding two serials of the same Fishery
  class ("permit stacking") counts as holding that fishery once under the
  first, but has one idle permit under the second. `01_build_panel.R` prints
  a direct stacking-frequency check (`stacking_check`) and the mean gap
  between the two count shares the first time it runs, worth reading before
  deciding whether the gap is big enough to lead with the permit-serial
  version in the writeup rather than treating it as a robustness footnote.
- **Figure 1's held/fished panel is trimmed to drop trailing years where
  ticket coverage collapses relative to permit coverage.** `permit_register`
  (held) and `catch_data_temp` (fished) are two separate source pulls with
  no guaranteed shared end year, `chapter3_plan.md` Section 1 notes the
  permit/vessel registers run through 2022 while the fish-ticket pull's true
  end year is uncertain. A held permit in a year the ticket data barely
  covers reads as held-but-never-fished by construction, which would
  mechanically spike `unused.count.share`/`unused.value.share` fleet-wide in
  that year and looked like the likely cause of an end-of-series jump in
  Figure 1. `01_build_panel.R` Section 2b now walks backward from the last
  observed year and drops any run of trailing years whose ticketed-vessel
  count falls below half its own 3-year trailing baseline, applied to
  `permit_register`/`catch_data_temp` themselves so every downstream table
  and figure sees the corrected range, not just Figure 1. It prints the
  coverage table it decided on, CHECK that once this runs on real data, the
  half-of-baseline threshold is a judgment call. A parallel, cheaper check
  right after `deflate()` warns if `cpi_deflator.csv` is simply missing a
  final-year row, which would produce the identical symptom for a different
  reason.
- **Figure 2's gear class comes from the CFEC vessel register's own gear
  dummy columns**, not from the fishery code's gear digits, and picks a
  vessel's single modal gear class across its panel. A vessel rigged for more
  than one gear type is assigned by a fixed, arbitrary priority order (see
  `GEAR_COLUMNS` in `03_figure1_figure2.R`), which is documented but not
  validated against how a domain expert would actually want ties broken.
- **Figure 3's passive-benchmark weights use each vessel's own long-run
  realized shares** (the same weights that define `H_LR`), not equal weights
  across the full held set. `chapter3_plan.md` names both as options. The
  equal-weight version (which would give nonzero weight to fisheries a
  vessel held but never fished, unlike the historical-weight version) is not
  built, see the comment block at the top of the Figure 3 section in
  `05_table4_figure3.R` for how to add it.
- **`passive.cv` is computed on revenue levels, matching `rev.cv`, not on
  log returns.** An earlier version built a fleet-level return-covariance
  matrix and used `sqrt(w' Sigma w)`, which put the benchmark on a different
  statistical basis than the levels-based `rev.cv` it gets compared against,
  biasing the comparison toward "realized exceeds passive" for any vessel
  with a smooth multi-year revenue trend (inflation being one cause among
  several), independent of any real reallocation behavior. The current
  version constructs a counterfactual revenue-level series per vessel,
  `sum_j w_ij * fleet_mean_revenue_jt` over the vessel's own active years,
  and takes its CV the same way `rev.cv` is computed everywhere else in this
  pipeline, see the comment block at the top of the Figure 3 section in
  `05_table4_figure3.R`. This also matches Chapter 2's own CV definition,
  which is levels-based.
- **Table 4's main-text models exclude single-fishery specialists, and
  Figure 3 splits into a main-text plot (multi-fishery vessels) and a
  separate appendix plot (specialists only).** A vessel that only ever
  fished one fishery has `Phi = 0` exactly, no within-vessel reallocation to
  measure, so it contributes no identifying variation to `g2` and just sits
  at a fixed `(H_LR = 1, Phi = 0)` point inside its `prime.fishery` FE group.
  Binning vessels by `Phi` and checking the mean gap between `rev.cv` and
  `passive.cv` per bin on real data showed the gap is not smooth through
  `Phi = 0`, specialists sit above the gap of vessels with small positive
  `Phi`, before it climbs again at high `Phi`, evidence they sit on a
  different footing (idiosyncratic noise around the fleet mean, not
  reallocation) rather than the low end of the same relationship.
  `05_table4_figure3.R` now fits Table 4's four models on `vessel_multi`
  (specialists excluded) and writes that as the main
  `table4_decomposition_regression.tex`, with the old pooled-sample version
  of all four models kept as an explicit robustness comparison in
  `table4_decomposition_regression_pooled.tex`, not dropped. Figure 3 used
  to keep every vessel in one plot colored by `is.specialist`, it now writes
  `figure3_passive_benchmark.png` (multi-fishery vessels, main text) and
  `figure3_appendix_specialists.png` (specialists only, appendix) as two
  separate files, since a specialist's passive.cv is built off the fleet
  mean of the single fishery it holds and mixing that noise floor into the
  main-text scatter risked reading it as evidence of reallocation risk.
- **No CPI deflator ships with this pipeline.** `load_deflator()` in
  `00_setup.R` looks for `Chpt3/data/cpi_deflator.csv` (columns `Year`,
  `CPI`) and falls back to nominal dollars with a warning if it is missing.
  Add that file before trusting any dollar-denominated output across the
  1991-2021 panel, `chapter3_plan.md` Section 9.3 explains why CPI rather
  than a seafood price index is the right choice here.
- **`06_within_season_reallocation.R` skips the cross-fishery
  co-participation network entirely, none of Figure 5, Figure 6, or Table 6
  need it.** `chapter3_plan.md` Section 0.1 scopes that network (Kroetz et al.
  2019, Addicott et al. 2018 style) as the weight for switch events, which
  now feeds Figure 7 in `11_switch_event_weights.R` and Table 12/13 in
  `09_seasonal_overlap.R`/`10_network_similarity.R`. `MIN_LANDINGS = 1`
  (vessel-fishery-years, `06_`) and `MIN_SEASON_LANDINGS = 10` (fleet-wide
  fishery-years, `06_`) are judgment calls, not facts, CHECK them once run
  against real data.
- **`11_switch_event_weights.R`'s trip-fishery assignment is a judgment
  call, not a fact.** A same-day landing under two permits is assigned to
  its higher-revenue fishery for the headline (PRIMARY) sequence, tie-broken
  on pounds landed, then on Fishery code. A SENSITIVITY sequence built
  alongside it instead drops those trip-days rather than picking a winner,
  and the script prints both switch counts and mean distances so the two
  can be compared once run against real data, CHECK how far apart they land
  before treating the PRIMARY figure as settled.
- **Table 7/8's regression is `log(rev.cv) ~ H_bar`, not the levels
  specification Table 4 and Table 6 use.** Table 4 runs in levels
  specifically because `H_bar = H_LR + Phi` only decomposes additively in
  levels, `chapter3_plan.md` Section 9.2. That reasoning does not apply to
  Table 7/8, nothing is being decomposed there, and Chapter 2's own
  per-regime slopes that Figure 8 compares against (`0.74`/`0.75`/`0.78`/`0.87`
  in `writeup/simulation_results.tex`) come from a log-linear
  `log(CV) ~ H_bar` regression. Matching that specification is what makes
  the comparison meaningful, using levels here would put Figure 8's two
  halves on different scales.
- **Section 7's "predetermined primary fishery" and "held set" reuse Table
  8's split-sample idea rather than the whole-panel `prime.fishery`.**
  `chapter3_plan.md` Section 9.2 flags the original R4 draft as circular,
  defining "primary" using data from the same year (or vessel's whole
  panel, which includes the year) the shock is measured in risks the shock
  being what made a fishery non-primary in the first place.
  `08_state_contingent_activation.R` fixes primary fishery once from each
  vessel's own first half of active years and restricts the activation
  sample to its second half, and uses held-in-`t-1` rather than held-in-`t`
  for the held set, both per Section 9.3's resolution. The leave-one-out
  quantity shock is standardized against each fishery's own complete
  multi-year series (not a per-vessel leave-one-out series), see the comment above
  `fishery_quantity_stats` in that script for why.
- **A methodological review pass on Sections 5-7 (beyond the two bugs at the
  top of this file) changed several specifications, each explained inline
  where it happens.** `06_within_season_reallocation.R`, weights season
  windows by landed pounds rather than revenue (a timing question does not
  need a price signal in it), adds `weekly.switching.per.transition`
  (switching is mechanically larger for a vessel that simply fishes more
  weeks, this divides by the number of available week-to-week transitions
  instead), and Table 6 now clusters on `Vessel.ADFG.Number` explicitly
  (fixest's default would cluster on `prime.fishery`, too few fishery
  classes for reliable inference), controls for mean active weeks, and adds
  a vessel-fixed-effects robustness column. `07_behavioral_heterogeneity.R`
  adds a continuous `H_bar * within.season.switching` interaction to Table 7
  as the headline statistic (the median split is now secondary, kept for
  Figure 8's visual), a robustness refit using the normalized switching
  classifier, a stricter-floor robustness refit for Table 8, and
  heteroskedasticity-robust standard errors throughout (both scripts'
  cross-sectional models had been reporting iid OLS SEs, which understate
  the true uncertainty). Figure 8's framing was softened to compare the
  ORDERING of empirical slopes against Chapter 2's, not their exact level,
  since the empirical `H_bar` distribution and the simulated 3-fishery grid
  are not on the same footing. `08_state_contingent_activation.R` adds a
  minimum-years floor to the predetermined-primary and second-half samples
  (mirroring Table 8's), an identification diagnostic reporting how many
  fishery-year cells actually carry heterogeneous primaries, and rebuilds
  Table 11's placebo entirely, see that script's Section 5 header comment,
  the original forward-shift-only version could not distinguish a genuine
  contamination from ordinary persistence in the shock series. Figure 10 is
  now a fixed-effects-residualized binned scatter rather than a raw one,
  so it cannot visually contradict Table 10's within-vessel, within-fishery-year
  estimate.
- **`MIN_ACTIVE_YEARS = 5` and `MIN_FISHERY_RETURN_YEARS = 10`** in
  `00_setup.R` are defaults, not derived facts. `chapter3_plan.md` Section
  9.3 calls a minimum-years filter "central rather than optional" without
  naming a number.
- **Table 4 has no controls beyond the `prime.fishery` fixed effect.** The
  outline's `Table 4` formula includes a generic `+ controls` term that
  Section 4 never specifies. Vessel-level controls (length, etc.) first show
  up in Section 6 of the outline: add them here once a control set is
  chosen.
- **Zero-filling `CFEC.Value..Detail.` is not assumed neutral, it is
  checked.** `match_diag`'s `share_zero_fill_has_positive_pounds` reports
  what fraction of the zero-filled rows have positive `Pounds..Detail.`,
  i.e. look like real landings with an unrecorded price rather than
  genuinely empty rows. A high value there means the zero-fill is
  manufacturing false zeros that mechanically inflate measured HHI (shares
  are convex in revenue), not just adding noise, see
  `NOTES_prior_prototype.md` for the full mechanism. This pipeline does not
  yet act on that diagnostic (e.g. by imputing a fishery-year median price
  for those rows), it only measures the problem.
- **Two different CFEC filing-number fields exist and mean different
  things**, `File.Number` in the permit register (and
  `CFEC.Permit.Holder.Filing.Number` on fish tickets) identify the *permit
  holder*, while `CFEC.Vessel.Owner.Filing.Number` on fish tickets identifies
  the *vessel owner*. `01_build_panel.R`'s owner-level panel uses the permit
  holder consistently on both the held and fished side, since Table 3
  specifically needs to attribute permits with no vessel attached to
  somebody, and there is no vessel owner to speak of in that case. The
  vessel-owner field is not currently used anywhere in this pipeline, it
  would matter for a genuinely different question (5E in
  `chapter3_plan.md`, multi-vessel owners), not for the permit-holder wedge
  Sections 2-4 build.
- **`09_seasonal_overlap.R` measures a fishery's SEASON as a fixed,
  fleet-wide, all-years-pooled weekly landings distribution, not a
  per-year, per-vessel object, and compares two fisheries' seasons with the
  Bhattacharyya coefficient, not correlation or a shared-week count.**
  Pooling across the whole panel (rather than per fishery-year, the way
  `season_windows` in `06_within_season_reallocation.R` works) treats a
  fishery's calendar as a mostly-fixed characteristic of its biology and
  regulation, and means no single vessel's own behavior meaningfully
  drives the signature used to explain that same vessel's own choices
  (unlike the Section 7 shock, which needs an explicit leave-one-out
  adjustment because one vessel CAN dominate a single fishery-year). The
  Bhattacharyya coefficient, `sum_w sqrt(p_w * q_w)`, is a standard bounded
  `[0, 1]` distributional-overlap measure, chosen over correlation because
  correlation can be negative or exceed what "share of weeks in common"
  should mean, and over a raw shared-week count because it is weighted by
  how much revenue is concentrated in the overlapping weeks, not just
  whether they overlap at all. `MIN_FISHERY_WEEKS = 3` (a fishery needs
  landings in at least this many distinct weeks, pooled across the whole
  panel, before it gets a season signature) and
  `SEASONAL_OVERLAP_CUTOFF = 0.5` (Figure 11's "seasonally blocked" split)
  are judgment calls, not facts, CHECK both once run on real data. This
  script exists because a deep-reasoning review of whether to import
  Abbott, Sakai & Holland (2023)'s temporal-diversification measure
  (`Context_papers/`, a Shannon index over a fisher's OWN weekly revenue
  shares) concluded that measure should NOT be imported directly, this
  chapter's structural advantage is observing the HELD option set, which
  has no analogue on the West Coast data that paper uses, and adopting
  their fished-revenue-based measure would abandon that advantage rather
  than use it. The seasonal-overlap-of-the-HELD-portfolio measure here is
  the Alaska-specific object their framework does not (and structurally
  cannot) build, used to fill Table 12 (Section 7) and re-cut the Section 3
  wedge (Figure 11), not as a copy of their index.
