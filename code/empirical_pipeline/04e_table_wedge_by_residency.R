# Chapter 3 empirical pipeline, companion to Table 3 (Chapter3_outline.md Section 3)
#
# The held-versus-fished wedge, split by the owner's residency
# (CFEC.Permit.Holder.Residency, File.Number level). chapter3_plan.md flags
# this as an optional heterogeneity cut, and also flags a confound, permits
# that fail to match a vessel already skew out-of-state (Section 6), so a
# raw residency gap in the wedge could just be reading permit-match quality
# rather than real behavioral heterogeneity. This table reports both the
# with-unmatched and without-unmatched wedge by residency group side by
# side, so a reader can see directly how much of any residency gap survives
# dropping the vessel-unmatched permits. 01_build_panel.R's own Section 7
# prints the match-rate-by-residency number this comparison is checked
# against.
#
# residency is only observable on fish-ticket rows (01_build_panel.R
# Section 2, the permit register carries no residency field), and is
# attached at the File.Number level as the MODAL value across that owner's
# entire ticket history, not year by year, so it covers owner-years with no
# tickets too (held but entirely unfished that year) as long as the same
# File.Number fished in some OTHER year of the panel. An owner who never
# generated a single ticket across the whole 1991-2021 panel (held-only,
# every year) has no residency observation at all and is dropped from this
# table rather than assigned a value. See owner_residency_lookup's own
# construction in 01_build_panel.R Section 2 for the mover/noise diagnostic
# on how often a File.Number's ticket rows carry more than one residency
# value.
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_year")) load(panel_path)

owner_year_valid <- owner_year %>% filter(n.held.fishery > 0)

cat("\n===== Residency coverage, owner-years with n.held.fishery > 0 =====\n")
cat("Non-missing residency:", sum(!is.na(owner_year_valid$residency)), "of", nrow(owner_year_valid),
    "(", round(100 * mean(!is.na(owner_year_valid$residency)), 1), "% )\n")
cat("Distinct residency values seen (CHECK these against the real category labels",
    "before writing anything about them, not yet confirmed from inside this script):\n")
print(owner_year_valid %>% count(residency, sort = TRUE), n = Inf)

# A residency category held by only a handful of owner-years would show a
# noisy mean off a tiny denominator, same reasoning as 04b_'s
# MIN_HELD_FOR_RANKING. Floor is on owner-years, not distinct owners.
MIN_OWNER_YEARS_FOR_RESIDENCY_TABLE <- 30

residency_wedge <- owner_year_valid %>%
  filter(!is.na(residency)) %>%
  group_by(residency) %>%
  summarise(
    n.owner.years               = n(),
    n.distinct.owners           = n_distinct(File.Number),
    unused.count.share.with     = mean(unused.count.share, na.rm = TRUE),
    unused.count.share.without  = mean(unused.count.share.matched, na.rm = TRUE),
    unused.value.share.with     = mean(unused.value.share, na.rm = TRUE),
    n.held.fishery.mean         = mean(n.held.fishery, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n.owner.years >= MIN_OWNER_YEARS_FOR_RESIDENCY_TABLE) %>%
  arrange(desc(n.owner.years)) %>%
  # How much of a residency group's "with unmatched" wedge survives
  # dropping the vessel-unmatched permits, directly checking the confound
  # this table's own header comment describes.
  mutate(gap.from.unmatched = unused.count.share.with - unused.count.share.without)

cat("\n===== Held-versus-fished wedge by residency, owner-years with n.held.fishery > 0",
    "(floor", MIN_OWNER_YEARS_FOR_RESIDENCY_TABLE, "owner-years) =====\n")
print(residency_wedge %>%
        mutate(across(c(unused.count.share.with, unused.count.share.without,
                         unused.value.share.with, n.held.fishery.mean, gap.from.unmatched),
                       ~round(.x, 4))),
      n = Inf)

table3_by_residency <- residency_wedge %>%
  transmute(
    Residency                    = residency,
    `Owner-years`                = format(n.owner.years, big.mark = ","),
    UnusedCountWith               = sprintf("%.4f", unused.count.share.with),
    UnusedCountWithout            = sprintf("%.4f", unused.count.share.without),
    UnusedValueWith               = sprintf("%.4f", unused.value.share.with),
    PermitsHeld                   = sprintf("%.4f", n.held.fishery.mean)
  )

print(table3_by_residency, n = Inf)

# Column headers built as a plain character vector rather than as the
# transmute()'s own backtick-quoted names, backtick identifiers do not
# collapse a "\\\\" the way a normal double-quoted string literal does, so
# getting a literal LaTeX line break into a backtick name is unreliable,
# not something worth risking on a pipeline that cannot be run locally to
# check (00_setup.R). \shortstack{} (core LaTeX, no package needed) wraps
# the four wordy headers onto two lines each so xtable does not size those
# columns off a single long header line while the short columns (Residency,
# Owner-years) stay narrow, which is what was making the printed table run
# over the page width. sanitize.colnames.function below is required for
# this, xtable's default sanitizer escapes the backslashes in \shortstack
# itself and would print the raw macro text instead of running it.
residency_table_headers <- c(
  "Residency",
  "Owner-years",
  "\\shortstack{Unused count share\\\\with unmatched}",
  "\\shortstack{Unused count share\\\\without unmatched}",
  "\\shortstack{Unused value share\\\\with unmatched}",
  "\\shortstack{Permits held\\\\per owner-year}"
)
colnames(table3_by_residency) <- residency_table_headers

# floating = FALSE, caption and label dropped from xtable() itself and
# moved into chapter3_writeup.tex's own \begin{table} wrapper instead, the
# same pattern every other wide table in the chapter uses. The shortstack
# header wrapping above still helps, but on its own it was not enough to
# keep this six-column table under \textwidth (Overfull \hbox), and a bare
# tabular is what lets the writeup wrap it in \resizebox, which a table
# float cannot be nested inside.
print(xtable(table3_by_residency),
      file = file.path(table_dir, "table3_wedge_by_residency.tex"),
      floating = FALSE,
      include.rownames = FALSE,
      sanitize.colnames.function = function(x) x)

cat("Wrote table3_wedge_by_residency.tex to", table_dir, "\n")
