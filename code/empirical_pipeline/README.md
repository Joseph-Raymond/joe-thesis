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

Nothing in this repo has executed these scripts. They were written by reading
`code/Permit_Linking/permit_link.R`, `code/myfunctions.R`,
`code/data load module.R`, `code/vessel_clean.R`, and
`code/Permit_Variance.R` for column names and cleaning conventions, and by
following `Chapter3_outline.md` and `chapter3_plan.md` for what each figure
and table needs to show. Treat the first run as a debugging pass, not a
victory lap. Search for "CHECK:" comments and read every script's header
before trusting output, several column names and one file path are inferred
rather than confirmed against real headers (`chapter3_plan.md` Section 1 says
the same about the existing code).

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
  anywhere in `Chpt3/code/`.

## Design choices worth reviewing before trusting the output

These are places the outline and plan leave a genuine judgment call, not
places where I guessed at a fact. Each is also flagged with a comment at the
point it is made in the code.

- **Table 3's "with/without missing vessel ID" comparison is built at the
  owner (`File.Number`) level.** A permit with no vessel attached has no
  vessel to be a vessel-year row for, so the vessel-level panel can never
  include it. The owner is the only unit that can hold it at all.
- **Held and fished sets are defined at the Fishery-class level** (e.g.
  `"S03T"`), not the individual permit serial level, matching how
  `permit_link.R` already computes HHI. A vessel holding two serials of the
  same Fishery class ("permit stacking") counts as holding that fishery
  once.
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
