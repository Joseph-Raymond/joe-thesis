# Notes from a prior Figure 1 / Table 4 prototype

Before `code/empirical_pipeline/` existed, a narrower prototype covering only
Figure 1 and Table 4 (vessel level) was built at `Chpt3/fig1_table4/` and
validated against a small hand-built synthetic register and fish-ticket
panel, since the real CFEC/AKFIN data was not reachable from where it was
written. This pipeline has since superseded it in scope and in most design
choices, and `Chpt3/fig1_table4/` is kept around as an archived reference
rather than something meant to be run going forward. This file preserves the
handful of points from that comparison that are still live, either because
they surfaced a real bug worth checking for, or because the two versions
made a different judgment call that has not been resolved either way.

## Resolved: permit stacking now tracked both ways, not decided in favor of one

**Update:** rather than picking one granularity, `01_build_panel.R` now
builds the permit-count objects (not the share objects) at both the
Fishery-class level and the individual permit-serial level, and carries
both through to `vessel_year`/`owner_year` as `unused.count.share` versus
`unused.count.share.permit` (and the `n.held.*`/`n.unfished.*` pairs behind
them). Figure 1 (`03_figure1_figure2.R`) plots both count-share lines
alongside the value-share line, and Table 3 (`04_table3.R`) reports both.
The rest of this section is kept as background for why the two versions can
differ and what to look at once real numbers exist, the disagreement itself
is resolved by not disagreeing, showing both.

**Held and fished sets are tracked at the Fishery-class level for every
share-based object** (`H_bar`, `H_LR`, `Phi`, `vessel_mean_share`), since a
share is inherently per fishery, not per permit, there is no serial-level
analogue to build there. A vessel holding two permit serials in the same
Fishery class ("permit stacking") counts as holding that fishery once for
these objects, and if either serial is fished, the fishery counts as
fished.

The prototype instead tracked `did.fish` per permit serial number
throughout. Under that version, a vessel holding two S03T permits and
fishing only one of them would show one fished permit and one genuinely
idle one. Under this pipeline's fishery-class version, both permits fold
into a single "S03T, held and fished" fact, and the idle second permit is
invisible, which is exactly the gap the permit-serial-level columns above
now close for the count objects specifically.

Whether the gap between the two versions is large enough to matter for the
writeup depends on how common permit stacking actually is in the data.
`01_build_panel.R` now prints this directly (`stacking_check`, a
`count(Vessel.ADFG.Number, Batch.Year, Fishery) %>% filter(n > 1)` style
check on `permit_register`) along with the mean fishery-class-versus-serial
count-share gap, the first time it runs. If stacking turns out to be rare,
the simpler fishery-class version is fine to lead with and the serial-level
columns are a one-line robustness note. If it is common, the serial-level
version is probably the one that belongs in the main text. Neither has been
checked against real data yet.

## Points the prototype flagged that this pipeline already handles

These came up while comparing the two versions and turned out to be
resolved here, not gaps.

- **CPI deflation is correctly wired through**, not just present as a stub.
  `01_build_panel.R` calls `deflate(vessel_fishery_year, "revenue",
  deflator)` before `vessel_year` and `fleet_mean_revenue` are built from it,
  so `rev.cv`, `H_bar`, `H_LR`, `fished.value`, and `forgone.value` are all
  computed from already-deflated dollars whenever
  `Chpt3/data/cpi_deflator.csv` exists. It currently does not exist, so the
  pipeline runs nominal with a warning until that file is added, exactly as
  the README says.
- **`MIN_ACTIVE_YEARS` is applied**, via `meets.min.years` in
  `vessel_summary` / `owner_summary`, and used to filter `vessel_analysis`
  before Table 4 and Figure 3 are built.
- **The prototype's `first(hhi)` bug does not exist here.** That bug came
  from computing HHI on one table and re-attaching it via a join keyed on a
  column that was sometimes missing, which meant `first()` could grab a row
  where HHI had not been attached yet. This pipeline computes `hhi` (and
  `H_bar`/`H_LR`) directly inside the same `summarise()` that builds
  `vessel_year`/`vessel_summary`, so there is no join-then-first step for
  this particular value to go stale on.

## Resolved: zero-filled revenue now has a diagnostic, not just a flag

**Update:** the open question here was whether zero-filling a missing
`CFEC.Value..Detail.` is a neutral, harmless default or whether it
manufactures a false zero for a real landing. `01_build_panel.R` now checks
this directly rather than assuming either way, `match_diag` includes
`share_zero_fill_has_positive_pounds`, the share of zero-filled rows that
have positive `Pounds..Detail.`. A high value there is direct evidence that
most of the zero-filled rows are real landings whose weight is recorded but
whose price never got attached, not genuinely empty rows, since a
zero-revenue row is not supposed to still have landed weight. Table 2
(`02_table1_table2.R`) reports it. This pipeline does not yet act on the
diagnostic, no price imputation is built, it only measures how big the
problem is. The mechanism below explains why the answer matters for Table 4
specifically, not just for Table 2's match-rate reporting.

`01_build_panel.R` already captures `share_revenue_zero_filled` in
`match_diag` before the zero-fill happens, which is the right instinct, that
diagnostic is worth taking seriously rather than treating as a formality.
The reason it matters specifically for Table 4, not just for Table 2's
match-rate reporting, is mechanical. Zero-filling a missing dollar value
that was actually a real, unrecorded landing pushes that fishery's share
toward zero and some other fishery's share up to compensate. Because HHI is
a sum of squared shares, and squaring is convex, that push does not average
out, it mechanically raises measured HHI relative to the vessel's true
portfolio concentration. In the worst case, a fishery where every landing
happened to have a missing value in a given year would show as a
zero-revenue, zero-share fishery for that vessel-year, i.e. as if it had
never been fished at all. If `share_revenue_zero_filled` comes back
non-trivial once this runs on real data, and especially if it is not spread
evenly across fisheries or years, it is worth treating as a source of
correlated measurement error in `H_bar`/`H_LR`, not just noise, since it
would bias the vessel toward looking more specialized than it actually was.

## A structural difference worth flagging, not necessarily fixing

The prototype (and the original `permit_link.R`) split each vessel's panel
into a pre/post-2004 period, one row per vessel-period, with period as a
second fixed effect alongside `prime.fishery`. That split was meant to mark
the AFA (American Fisheries Act) policy change, not an arbitrary midpoint,
per an earlier conversation about this comparison.

This pipeline's `vessel_summary` collapses straight to one row per vessel
across its whole active panel, with no period dimension at all. That is a
reasonable simplification for now (the AFA split was flagged as something to
"ignore for now" when this came up), but it is worth remembering that
restoring it later means adding a period grouping variable back into the
`vessel_summary`/`owner_summary` construction in `01_build_panel.R`, not
just changing a constant in `00_setup.R`, since the current code was written
assuming one cross-sectional row per vessel.

## Two smaller, still-open specification choices

- **Standardization in Table 4.** This pipeline z-scores both the outcome
  and the regressors and reports raw and standardized coefficients side by
  side (four columns in `table4_decomposition_regression.tex`). The
  prototype only standardized the regressors, not `rev.cv` itself, and
  reported only the standardized version. Both are defensible readings of
  the outline's "standardized coefficients" instruction, this pipeline's
  version keeps an interpretable raw-scale column around too, which seems
  like the better default, but it is worth a second look once real
  coefficients exist to interpret.
- **Controls in Table 4.** This pipeline runs no controls beyond the
  `prime.fishery` fixed effect, documented as a deliberate placeholder since
  Section 4 of the outline never specifies a control set. The prototype
  added `median.fished.fishery` and `median.unfished.fishery` as controls.
  Neither choice is wrong given the outline leaves this open, but the two
  pipelines are not answering quite the same regression until this gets
  decided one way.

## A validation step worth porting over

Neither version of this pipeline has been run against real data. The
prototype could not be either, but it was checked against a small,
hand-built synthetic register and fish-ticket panel with known-exact
answers, which caught the `first(hhi)` bug above before it ever reached the
server. Worth doing something similar here before the first real run,
particularly these checks, since they are cheap, general, and independent
of whatever the real numbers turn out to be

- a vessel that only ever holds and fishes one fishery should get
  `H_bar = H_LR = 1` and `Phi = 0` exactly
- `Phi` should never be negative for any vessel, it is a sum of
  within-vessel, across-year variances by construction
- `unused.count.share` and `unused.value.share` should both fall in `[0, 1]`
  wherever defined, and should be exactly `0` for a vessel that fishes every
  fishery it holds
