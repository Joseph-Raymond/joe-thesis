# Chapter 3 empirical pipeline, year-by-year companion to 04b_ and 04c_
#
# For every year, the ten fisheries carrying the most held-but-never-fished
# owner-level permits, ranked two ways, by raw count and by unused share.
# Same population as 04b_ and 04c_, every permit-register row with a
# File.Number, vessel-matched or not. The unit is an owner-fishery-year, so
# an owner holding two permits in the same fishery counts once here, the same
# grain 04b_ and 04c_ already use.
#
# Run twice, once over every fishery and once restricted to limited-entry
# fisheries (cfec_limited_status.csv, same source and filter as 01_build_panel.R
# Section 5), so the limited-entry rankings sit alongside the all-fishery ones.
#
# Output is four CSV tables in Chpt3/output/tables/, a count and a share
# ranking for each of the two populations. Rows are rank 1 through 10, columns
# are years, and each cell reads "CODE (value)". The limited-entry files carry
# a _limited suffix.
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R, so the
# rest of the pipeline does not need to be rerun first.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_fishery_year")) load(panel_path)

TOP_N_PER_YEAR <- 10

# The share ranking needs a floor on held owner-years within the fishery-year,
# otherwise a fishery with one or two holders can sit at 100 percent unused
# and fill the top of a year's list on a tiny denominator. Unlike 04b_, this
# floor applies within a single year rather than across the whole panel.
MIN_HELD_PER_YEAR_FOR_SHARE <- 10

# Off by default. A fishery closed fleet-wide in a year has no landings from
# anyone, so every held owner-year in it reads as unfished. Setting this to
# TRUE keeps only fishery-years with fleet-wide landings (fishery.year.active,
# 01_build_panel.R Section 4), the same restriction 04_table3.R reports as its
# fishable-years rows.
RESTRICT_TO_FISHABLE_YEARS <- FALSE

limited_status_path <- file.path(intermediate_dir, "cfec_limited_status.csv")
if (!file.exists(limited_status_path)) {
  stop("cfec_limited_status.csv not found at ", limited_status_path,
       ". Copy it from code/empirical_pipeline/ into intermediate data/ first.")
}
limited_status <- read.csv(limited_status_path, stringsAsFactors = FALSE) %>%
  filter(source != "historical-outside-range") %>%
  transmute(Fishery, Batch.Year = as.integer(Batch.Year), limited = as.logical(limited))

held_rows_all <- owner_fishery_year %>% filter(held)
if (RESTRICT_TO_FISHABLE_YEARS) held_rows_all <- held_rows_all %>% filter(fishery.year.active)

held_rows_limited <- held_rows_all %>%
  left_join(limited_status, by = c("Fishery", "Batch.Year")) %>%
  mutate(limited = replace_na(limited, FALSE)) %>%
  filter(limited)

to_grid <- function(ranked, value_col, value_fmt) {
  ranked %>%
    mutate(cell = paste0(Fishery, " (", sprintf(value_fmt, .data[[value_col]]), ")")) %>%
    select(rank, Batch.Year, cell) %>%
    pivot_wider(names_from = Batch.Year, values_from = cell) %>%
    arrange(rank)
}

write_top_tables <- function(held_rows, suffix) {
  fishery_year_held <- held_rows %>%
    group_by(Batch.Year, Fishery) %>%
    summarise(
      n.held       = n(),
      n.unfished   = sum(!fished),
      unused.share = n.unfished / n.held,
      .groups = "drop"
    )

  # The count ranking favors big fisheries, which is the point, it answers
  # where the most idle permits sit in absolute terms. Ties broken by code so
  # the ordering is reproducible from run to run.
  rank_count <- fishery_year_held %>%
    filter(n.unfished > 0) %>%
    group_by(Batch.Year) %>%
    arrange(desc(n.unfished), Fishery, .by_group = TRUE) %>%
    slice_head(n = TOP_N_PER_YEAR) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  # The share ranking favors fisheries that are idle relative to their own size.
  # Ties at 100 percent are broken by held count, so a larger fishery sits above
  # a smaller one that is equally idle.
  rank_share <- fishery_year_held %>%
    filter(n.held >= MIN_HELD_PER_YEAR_FOR_SHARE, n.unfished > 0) %>%
    group_by(Batch.Year) %>%
    arrange(desc(unused.share), desc(n.held), Fishery, .by_group = TRUE) %>%
    slice_head(n = TOP_N_PER_YEAR) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  grid_count <- to_grid(rank_count, "n.unfished", "%.0f")
  grid_share <- to_grid(rank_share, "unused.share", "%.3f")

  write.csv(grid_count,
            file.path(table_dir, paste0("top10_held_unfished_count_by_year", suffix, ".csv")),
            row.names = FALSE, na = "")
  write.csv(grid_share,
            file.path(table_dir, paste0("top10_held_unfished_share_by_year", suffix, ".csv")),
            row.names = FALSE, na = "")

  short_count_years <- rank_count %>% count(Batch.Year) %>% filter(n < TOP_N_PER_YEAR)
  short_share_years <- rank_share %>% count(Batch.Year) %>% filter(n < TOP_N_PER_YEAR)

  cat("[", if (suffix == "") "all fisheries" else "limited-entry fisheries", "]\n")
  cat("Years in panel -", n_distinct(fishery_year_held$Batch.Year), "\n")
  cat("Years with fewer than", TOP_N_PER_YEAR, "fisheries ranked by count -", nrow(short_count_years),
      "(blank cells in the count table)\n")
  cat("Years with fewer than", TOP_N_PER_YEAR, "fisheries ranked by share -", nrow(short_share_years),
      "(blank cells in the share table)\n")
  cat("Wrote top10_held_unfished_count_by_year", suffix, ".csv and top10_held_unfished_share_by_year",
      suffix, ".csv to ", table_dir, "\n", sep = "")

  print(grid_count[, 1:6])
  print(grid_share[, 1:6])
}

write_top_tables(held_rows_all, "")
write_top_tables(held_rows_limited, "_limited")
