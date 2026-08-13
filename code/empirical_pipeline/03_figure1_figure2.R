# Chapter 3 empirical pipeline, Chapter3_outline.md Section 3
#
# Figure 1. Fleet-wide time series of the mean unused share (count and value).
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

fig1_data <- vessel_year %>%
  filter(n.held.fishery > 0) %>%
  group_by(Batch.Year) %>%
  summarise(
    `Count share`   = mean(unused.count.share, na.rm = TRUE),
    `Value share`   = mean(unused.value.share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-Batch.Year, names_to = "measure", values_to = "mean_unused_share")

figure1 <- fig1_data %>%
  ggplot(aes(x = Batch.Year, y = mean_unused_share, color = measure)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Mean unused share of held permits, fleet-wide",
    subtitle = "Count share: permits held but not fished, over permits held. Value share: fleet-mean forgone revenue over forgone-plus-fished value.",
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

# Picks the first gear dummy coded "Yes" for a vessel-year, in the priority
# order of GEAR_COLUMNS above. A vessel rigged for more than one gear type is
# assigned to whichever comes first in that list, which is an arbitrary but
# documented tie-break, not a claim about which gear matters most.
classify_gear <- function(df) {
  gear_matrix <- df %>% select(any_of(GEAR_COLUMNS)) %>% mutate(across(everything(), ~ .x == "Yes"))
  present <- intersect(GEAR_COLUMNS, names(gear_matrix))
  gear_class <- rep(NA_character_, nrow(df))
  for (g in present) {
    gear_class[is.na(gear_class) & gear_matrix[[g]]] <- g
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
      length.bin = cut(
        vessel.length,
        breaks = c(0, 20, 30, 40, 50, 60, Inf),
        labels = c("0-20", "20-30", "30-40", "40-50", "50-60", "60+"),
        right = FALSE
      )
    )

  fig2_data <- vessel_year %>%
    filter(n.held.fishery > 0) %>%
    group_by(Vessel.ADFG.Number) %>%
    summarise(mean.unused.count.share = mean(unused.count.share, na.rm = TRUE), .groups = "drop") %>%
    inner_join(vessel_char, by = "Vessel.ADFG.Number") %>%
    filter(!is.na(length.bin))

  figure2 <- fig2_data %>%
    ggplot(aes(x = gear_class, y = mean.unused.count.share, fill = length.bin)) +
    geom_boxplot(outlier.size = 0.5) +
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
