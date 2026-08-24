# Chapter 3 empirical pipeline (Sections 2-4)

Reproducible R scripts for the figures and tables in `Chapter3_outline.md` up
through Figure 3 and Table 4, that is, Section 2 (Table 1, Table 2), Section 3
(Figure 1, Figure 2, Table 3), and Section 4 (Table 4, Figure 3). Table 5,
Figure 4, and everything from Section 5 onward are not built here.

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
deprecation warning on the server's dplyr version). `03_figure1_figure2.R`
through `05_table4_figure3.R` have not been run yet. Search for "CHECK:"
comments and read every script's header before trusting output from those,
several column names and one file path are inferred rather than confirmed
against real headers (`chapter3_plan.md` Section 1 says the same about the
existing code).

## Run order

```
00_setup.R              # sourced automatically by every other script, not run directly
01_build_panel.R        # builds intermediate data/ch3_panel.rdata, run this first
02_table1_table2.R      # Table 1, Table 2
03_figure1_figure2.R    # Figure 1, Figure 2 (needs the CFEC vessel register, see below)
04_table3.R             # Table 3
05_table4_figure3.R     # Table 4, Figure 3
```

Each of 02 through 05 loads `intermediate data/ch3_panel.rdata` if the panel
objects are not already in memory, so they can be run independently in a
fresh session as long as 01 has been run at least once. Outputs land in
`Chpt3/output/tables/` (`.tex`, via `xtable` and `fixest::etable`) and
`Chpt3/output/figures/` (`.png`, via `ggplot2`).

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
- **No CPI deflator ships with this pipeline.** `load_deflator()` in
  `00_setup.R` looks for `Chpt3/data/cpi_deflator.csv` (columns `Year`,
  `CPI`) and falls back to nominal dollars with a warning if it is missing.
  Add that file before trusting any dollar-denominated output across the
  1991-2021 panel, `chapter3_plan.md` Section 9.3 explains why CPI rather
  than a seafood price index is the right choice here.
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
