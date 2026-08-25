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
fig1_data <- vessel_year %>%
  filter(n.held.fishery > 0) %>%
  group_by(Batch.Year) %>%
  summarise(
    `Count share (fishery-class)` = mean(unused.count.share, na.rm = TRUE),
    `Count share (permit-serial)` = mean(unused.count.share.permit, na.rm = TRUE),
    `Value share`                 = mean(unused.value.share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-Batch.Year, names_to = "measure", values_to = "mean_unused_share")

figure1 <- fig1_data %>%
  ggplot(aes(x = Batch.Year, y = mean_unused_share, color = measure)) +
  geom_line(linewidth = 0.8) +
  labs(
    # Count vs. value share, and fishery-class vs. permit-serial, are defined
    # in the caption, the legend already names the three series distinctly.
    title = "Mean unused share of held permits, fleet-wide",
    x = "Year", y = "Mean unused share", color = NULL
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

ggsave(file.path(figure_dir, "figure1_unused_share_timeseries.png"),
       figure1, width = 8, height = 5, dpi = 300)

# ============================================================================
# Figure 2. Distribution across vessels, by gear class and vessel length
# ============================================================================

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

# Picks the first gear dummy coded "Yes" for a vessel-year, in the priority
# order of GEAR_COLUMNS above. A vessel rigged for more than one gear type is
# assigned to whichever comes first in that list, which is an arbitrary but
# documented tie-break, not a claim about which gear matters most.
classify_gear <- function(df) {
  gear_matrix <- df %>% select(any_of(GEAR_COLUMNS)) %>% mutate(across(everything(), ~ .x == "Yes"))
  present <- intersect(GEAR_COLUMNS, names(gear_matrix))
  gear_class <- rep(NA_character_, nrow(df))
  for (g in present) {
    gear_class[is.na(gear_class) & gear_matrix[[g]]] <- gsub("\\.", " ", g)
  }
  gear_class[is.na(gear_class)] <- "Unclassified"
  gear_class
}

if (file.exists(vessel_register_path)) {

  vessel_register <- read_csv(vessel_register_path, show_col_types = FALSE) %>%
    rename(Vessel.ADFG.Number = ADFG.Number, Batch.Year = Year) %>%
    mutate(Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number))

  vessel_register <- vessel_register %>% mutate(gear_class = classify_gear(vessel_register))

  # Modal gear class and median length per vessel, across its own panel,
  # since both can change year to year (vessel_clean.R detects exactly this).
  vessel_char <- vessel_register %>%
    group_by(Vessel.ADFG.Number) %>%
    summarise(
      gear_class    = names(sort(table(gear_class), decreasing = TRUE))[1],
      vessel.length = median(Length, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # 40-50 and 50-60 merged into one 40-60 bin, both were the two
      # smallest of the original six (a 10ft-bin histogram of vessel_char's
      # own vessel.length gave 2644 and 1362 respectively, versus 19327 for
      # 0-20 and 20465 for 20-30), so combining them trims a group without
      # flattening resolution in the two bins where most of the fleet sits.
      length.bin = cut(
        vessel.length,
        breaks = c(0, 20, 30, 40, 60, Inf),
        labels = c("0-20", "20-30", "30-40", "40-60", "60+"),
        right = FALSE
      )
    )

  fig2_data <- vessel_year %>%
    filter(n.held.fishery > 0) %>%
    group_by(Vessel.ADFG.Number) %>%
    summarise(mean.unused.count.share = mean(unused.count.share, na.rm = TRUE), .groups = "drop") %>%
    inner_join(vessel_char, by = "Vessel.ADFG.Number") %>%
    filter(!is.na(length.bin))

  n_fish_wheel <- sum(fig2_data$gear_class == "Fish Wheel")
  # Fish wheel is a non-motorized subsistence/personal-use gear, out of place
  # next to genuine commercial gear classes in a fleet diversification
  # figure, dropped from Figure 2 by request rather than folded into
  # "Unclassified" or left as its own (likely tiny) box. classify_gear()
  # itself is untouched, so this only affects what gets plotted here.
  fig2_data <- fig2_data %>% filter(gear_class != "Fish Wheel")
  cat("Vessels excluded from Figure 2 for Fish Wheel gear class:", n_fish_wheel, "\n")

  fig2_data <- fig2_data %>% mutate(gear_class = factor(gear_class, levels = GEAR_CLASS_ORDER))

  figure2 <- fig2_data %>%
    ggplot(aes(x = gear_class, y = mean.unused.count.share, fill = length.bin)) +
    geom_boxplot(outlier.size = 0.5, linewidth = 0.3) +
    labs(
      title = "Distribution of the unused permit share across vessels",
      subtitle = "By modal gear class and median vessel length (feet)",
      x = "Gear class", y = "Mean unused count share (per vessel)", fill = "Length (ft)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(figure_dir, "figure2_unused_share_distribution.png"),
         figure2, width = 9, height = 5.5, dpi = 300)

  cat("Wrote figure2_unused_share_distribution.png\n")

} else {
  warning(
    "Vessel register not found at ", vessel_register_path,
    ". Figure 2 needs it for gear class and length, skipping. ",
    "Check the path once running on the server (chapter3_plan.md Section 0.2)."
  )
}

cat("Wrote figure1_unused_share_timeseries.png to", figure_dir, "\n")
