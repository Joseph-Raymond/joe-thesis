# Chapter 3 empirical pipeline, Chapter3_outline.md Section 3
#
# Figure 1. Fleet-wide time series of the mean unused share (count, value,
#           and a permit-serial-level count that is stacking-aware).
# Figure 2. Distribution of the unused share across vessels, by gear class
#           and vessel length.
#
# Figure 2 needs the CFEC vessel register for gear and length, which
# vessel_year alone does not carry. Path below matches chapter3_plan.md
# Section 0.2 ("~/JoeData/clean_data/vessels_clean_1978_2022.csv"). CHECK this
# path and the gear dummy column names once run on the server, they are
# inferred from vessel_clean.R and have not been checked against real headers.

source("code/empirical_pipeline/00_setup.R")

if (!exists("vessel_year")) load(panel_path)

# ============================================================================
# Figure 1. Fleet-wide unused share over time (count and value)
# ============================================================================

# Count share (fishery-class) vs count share (permit-serial) is the direct
# comparison for the permit-stacking decision, see NOTES_prior_prototype.md.
# The fishery-class version collapses a vessel's stacked permits within one
# Fishery code (e.g. two "S03T" serials) into a single held/fished fact, so
# it reads unused.count.share.permit >= unused.count.share whenever stacking
# is present, since the permit-serial version can see an idle second serial
# that the fishery-class version cannot.
if (!exists("vessel_year_limited")) load(panel_path)

# Left panel uses every fishery, right panel only fisheries CFEC labels
# Limited (vessel_year_limited, 01_build_panel.R Section 5). Same three
# measures and same vessel-year averaging in both panels.
fig1_all <- vessel_year %>%
  filter(n.held.fishery > 0) %>%
  group_by(Batch.Year) %>%
  summarise(
    `Count share (fishery-class)` = mean(unused.count.share, na.rm = TRUE),
    `Count share (permit-serial)` = mean(unused.count.share.permit, na.rm = TRUE),
    `Value share`                 = mean(unused.value.share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(panel = "All fisheries")

fig1_limited <- vessel_year_limited %>%
  filter(n.held.lim > 0) %>%
  group_by(Batch.Year) %>%
  summarise(
    `Count share (fishery-class)` = mean(unused.count.share.lim, na.rm = TRUE),
    `Count share (permit-serial)` = mean(unused.count.share.permit.lim, na.rm = TRUE),
    `Value share`                 = mean(unused.value.share.lim, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(panel = "Limited-entry fisheries only")

fig1_data <- bind_rows(fig1_all, fig1_limited) %>%
  pivot_longer(c(`Count share (fishery-class)`, `Count share (permit-serial)`, `Value share`),
               names_to = "measure", values_to = "mean_unused_share") %>%
  mutate(panel = factor(panel, levels = c("All fisheries", "Limited-entry fisheries only")))

figure1 <- fig1_data %>%
  ggplot(aes(x = Batch.Year, y = mean_unused_share, color = measure)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ panel, ncol = 2) +
  labs(
    # Count vs. value share, and fishery-class vs. permit-serial, are defined
    # in the caption, the legend already names the three series distinctly.
    title = "Mean unused share of held permits, fleet-wide",
    x = "Year", y = "Mean unused share", color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(figure_dir, "figure1_unused_share_timeseries.png"),
       figure1, width = 11, height = 5, dpi = 300)

# ============================================================================
# Figure 2. Distribution across vessels, by gear class and vessel length,
# split into open-entry and limited-entry fisheries (two separate figures,
# replacing the earlier single all-fisheries version, by request).
# ============================================================================

if (!exists("vessel_year_open")) load(panel_path)

vessel_register_path <- "~/JoeData/clean_data/vessels_clean_1978_2022.csv"

GEAR_COLUMNS <- c(
  "Purse.Seine", "Beach.Seine", "Drift.Gill.Net", "Set.Gill.Net",
  "Hand.Troll", "Power.Troll", "Long.Line", "Otter.Trawl", "Pots",
  "Fish.Wheel", "Beam.Trawl", "Scallop.Dredge", "Mechanical.Jig",
  "Double.Otter.Trawl", "Herring.Gill.Net", "Pair.Trawl", "Diving.Hand.Picking"
)

# Figure 2's x-axis order, gear families with similar names grouped next to
# each other (the two seines, the three gill nets, the two trolls, the four
# trawls) rather than the plain alphabetical order a character x-axis would
# default to. Fish Wheel omitted, it is filtered out of fig2_data below.
# Unclassified pinned last as the catch-all it is, not a gear family.
GEAR_CLASS_ORDER <- c(
  "Purse Seine", "Beach Seine",
  "Drift Gill Net", "Set Gill Net", "Herring Gill Net",
  "Hand Troll", "Power Troll",
  "Otter Trawl", "Double Otter Trawl", "Beam Trawl", "Pair Trawl",
  "Long Line", "Pots", "Scallop Dredge", "Mechanical Jig",
  "Diving Hand Picking", "Unclassified"
)

# Long format, one row per (vessel, year, gear class) for every gear dummy
# marked "Yes" that year, plus one "Unclassified" row for a vessel-year
# with no gear dummy marked "Yes" at all. A vessel-year with more than one
# gear dummy "Yes" gets more than one row here on purpose. Replaces the old
# single modal-gear-per-vessel assignment (one gear, picked by a fixed
# priority order, for the vessel's whole career), so a vessel with more
# than one gear type, or one that changed gear partway through its life,
# contributes to every gear box it actually carried, in the specific years
# it carried it, rather than being folded into one label for its whole
# career (chat 2026-10-08, see that thread for the tradeoffs this raised,
# most notably that gear here is what the vessel register says it is
# EQUIPPED for in a given year, not necessarily what it actually fished
# that year, no fix on this side of the data closes that gap).
if (file.exists(vessel_register_path)) {

  vessel_register <- read_csv(vessel_register_path, show_col_types = FALSE) %>%
    rename(Vessel.ADFG.Number = ADFG.Number, Batch.Year = Year) %>%
    mutate(Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number))

  gear_flags_long <- vessel_register %>%
    select(Vessel.ADFG.Number, Batch.Year, Length, any_of(GEAR_COLUMNS)) %>%
    pivot_longer(any_of(GEAR_COLUMNS), names_to = "gear_col", values_to = "flag")

  gear_long <- gear_flags_long %>%
    filter(flag == "Yes") %>%
    mutate(gear_class = gsub("\\.", " ", gear_col)) %>%
    distinct(Vessel.ADFG.Number, Batch.Year, Length, gear_class)

  unclassified_long <- gear_flags_long %>%
    group_by(Vessel.ADFG.Number, Batch.Year, Length) %>%
    summarise(any.yes = any(flag == "Yes", na.rm = TRUE), .groups = "drop") %>%
    filter(!any.yes) %>%
    transmute(Vessel.ADFG.Number, Batch.Year, Length, gear_class = "Unclassified")

  gear_long <- bind_rows(gear_long, unclassified_long)

  # Median length per vessel across its own panel, used only by the
  # appendix vessel-length histogram below. Figure 2 itself now bins length
  # per (vessel, gear class) from only the years that gear was carried
  # (inside build_figure2), not one whole-career length reused in every box
  # a vessel appears in.
  vessel_char <- vessel_register %>%
    group_by(Vessel.ADFG.Number) %>%
    summarise(vessel.length = median(Length, na.rm = TRUE), .groups = "drop")

  LENGTH_BREAKS3 <- c(0, 30, 40, 60, Inf)
  LENGTH_LABELS3 <- c("0-30", "30-40", "40-60", "60+")

  # One call per entry-status split (open, limited). Joins that status's
  # vessel-year measure onto every (year, gear class) row the vessel
  # carried that year, then averages within (vessel, gear class) over only
  # the matched years, so a vessel that carried two gears in different
  # periods gets its own correct average in each box rather than one
  # blended whole-career number copied into both.
  build_figure2 <- function(vessel_year_obj, n_held_col, measure_col, subtitle, file_name) {
    vessel_year_measure <- vessel_year_obj %>%
      filter(.data[[n_held_col]] > 0) %>%
      transmute(Vessel.ADFG.Number, Batch.Year, measure = .data[[measure_col]])

    df <- vessel_year_measure %>%
      inner_join(gear_long, by = c("Vessel.ADFG.Number", "Batch.Year")) %>%
      group_by(Vessel.ADFG.Number, gear_class) %>%
      summarise(
        mean.unused.count.share = mean(measure, na.rm = TRUE),
        vessel.length           = median(Length, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(length.bin = cut(vessel.length, breaks = LENGTH_BREAKS3,
                               labels = LENGTH_LABELS3, right = FALSE)) %>%
      filter(!is.na(length.bin))

    # Fish wheel is a non-motorized subsistence/personal-use gear, out of
    # place next to genuine commercial gear classes in a fleet
    # diversification figure, dropped from Figure 2 by request rather than
    # folded into "Unclassified" or left as its own (likely tiny) box.
    n_fish_wheel <- sum(df$gear_class == "Fish Wheel")
    df <- df %>%
      filter(gear_class != "Fish Wheel") %>%
      mutate(gear_class = factor(gear_class, levels = GEAR_CLASS_ORDER))
    cat("Vessel-gear boxes excluded from", file_name, "for Fish Wheel gear class:", n_fish_wheel, "\n")

    fig <- df %>%
      ggplot(aes(x = gear_class, y = mean.unused.count.share, fill = length.bin)) +
      geom_boxplot(outlier.size = 0.5, linewidth = 0.3) +
      labs(
        title = "Distribution of the unused permit share across vessels",
        subtitle = subtitle,
        x = "Gear class", y = "Mean unused count share (per vessel, per gear carried)", fill = "Length (ft)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(file.path(figure_dir, file_name), fig, width = 9, height = 5.5, dpi = 300)
    cat("Wrote", file_name, "\n")
  }

  build_figure2(vessel_year_open, "n.held.open", "unused.count.share.open",
                "Open-entry fisheries only. A vessel appears once per gear type it carried, averaged over the years it carried that gear",
                "figure2_unused_share_distribution_open.png")

  build_figure2(vessel_year_limited, "n.held.lim", "unused.count.share.lim",
                "Limited-entry fisheries only. A vessel appears once per gear type it carried, averaged over the years it carried that gear",
                "figure2_unused_share_distribution_limited.png")

  # ==========================================================================
  # Appendix figure. Active vessel length distribution, no gear or entry-
  # status split, just the raw size distribution Figure 2's length bins are
  # drawn from.
  # ==========================================================================
  # "Active" matches the chapter's own definition elsewhere (Table 1,
  # Appendix lifetime robustness), vessel_summary$meets.min.years, at least
  # MIN_ACTIVE_YEARS years of positive revenue, not just n.held.fishery > 0
  # in a single year.
  if (!exists("vessel_summary")) load(panel_path)

  active_vessels <- vessel_summary %>% filter(meets.min.years) %>% pull(Vessel.ADFG.Number)

  fig_length_data <- vessel_char %>%
    filter(Vessel.ADFG.Number %in% active_vessels, !is.na(vessel.length))

  # 5ft bins up to 100ft, then one open-ended "100+" bin, so the rare large
  # tender/mothership vessel does not stretch a continuous-binwidth x-axis
  # out past where the rest of the fleet sits.
  length_breaks <- c(seq(0, 100, by = 5), Inf)
  length_labels <- c(paste(seq(0, 95, by = 5), seq(5, 100, by = 5), sep = "-"), "100+")
  n_over_100 <- sum(fig_length_data$vessel.length >= 100)
  cat("Active vessels at or above 100ft, collapsed into the \"100+\" bin:", n_over_100, "\n")

  fig_length_data <- fig_length_data %>%
    mutate(length.bin5 = cut(vessel.length, breaks = length_breaks,
                              labels = length_labels, right = FALSE))

  figure_vessel_length <- fig_length_data %>%
    count(length.bin5) %>%
    ggplot(aes(x = length.bin5, y = n)) +
    geom_col(fill = "steelblue") +
    labs(
      title = "Distribution of active vessel length",
      subtitle = paste0("Vessels active at least ", MIN_ACTIVE_YEARS, " years (n = ",
                         nrow(fig_length_data), ")"),
      x = "Vessel length (feet)", y = "Number of vessels"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(figure_dir, "figure_active_vessel_length_histogram.png"),
         figure_vessel_length, width = 7, height = 5, dpi = 300)

  cat("Wrote figure_active_vessel_length_histogram.png\n")

} else {
  warning(
    "Vessel register not found at ", vessel_register_path,
    ". Figure 2 needs it for gear class and length, skipping. ",
    "Check the path once running on the server (chapter3_plan.md Section 0.2)."
  )
}

cat("Wrote figure1_unused_share_timeseries.png to", figure_dir, "\n")
