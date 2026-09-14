# Chapter 3 empirical pipeline, companion to Table 3 and 04b_ (Chapter3_outline.md Section 3)
#
# Figure, appendix. When the held-but-unfished wedge is coming from, for the
# fisheries 04b_table_unused_by_fishery.R already flagged as carrying the
# most of it. 04b_ answers "which fisheries", this answers "in which years",
# owner level, same population as Table 3 and 04b_ (every permit-register
# row with a File.Number, vessel-matched or not).
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_fishery_year")) load(panel_path)

# Same floor as 04b_, a fishery held only a handful of times ever should not
# be in the running. A separate, smaller top-N than 04b_'s table (8 rather
# than 15), a time series with 15 overlapping lines is unreadable, this
# figure is meant to show a handful of clear trajectories, not the full
# ranking, see 04b_table_unused_by_fishery.R's own table for that.
MIN_HELD_FOR_RANKING <- 30
TOP_N_FISHERIES <- 8

top_fisheries <- owner_fishery_year %>%
  filter(held) %>%
  group_by(Fishery) %>%
  summarise(
    n.held       = n(),
    unused.share = mean(!fished),
    .groups = "drop"
  ) %>%
  filter(n.held >= MIN_HELD_FOR_RANKING) %>%
  arrange(desc(unused.share)) %>%
  slice_head(n = TOP_N_FISHERIES) %>%
  pull(Fishery)

fig_unused_by_fishery_year <- owner_fishery_year %>%
  filter(held, Fishery %in% top_fisheries) %>%
  group_by(Fishery, Batch.Year) %>%
  summarise(
    n.held       = n(),
    unused.share = mean(!fished),
    .groups = "drop"
  )

cat("Top", TOP_N_FISHERIES, "fisheries by owner-level unused share, held at least",
    MIN_HELD_FOR_RANKING, "owner-years overall:", paste(top_fisheries, collapse = ", "), "\n")

figure_unused_by_fishery_year <- fig_unused_by_fishery_year %>%
  ggplot(aes(x = Batch.Year, y = unused.share, color = Fishery)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = paste0("Owner-level unused share by year, top ", TOP_N_FISHERIES, " fisheries"),
    subtitle = "Fisheries ranked by mean unused share pooled across the full panel (see accompanying table)",
    x = "Year", y = "Unused share (held but unfished)", color = "Fishery"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

ggsave(file.path(figure_dir, "figure_unused_share_by_fishery_year.png"),
       figure_unused_by_fishery_year, width = 9, height = 5.5, dpi = 300)

cat("Wrote figure_unused_share_by_fishery_year.png to", figure_dir, "\n")
