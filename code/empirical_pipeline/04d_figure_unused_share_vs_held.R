# Chapter 3 empirical pipeline, companion to Table 3 and 04b_ (Chapter3_outline.md Section 3)
#
# Figure, appendix. 04b_'s own table ranks fisheries by unused SHARE, which
# surfaces the small administrative/data-gap codes (A11A, D91H, etc), not
# the fisheries that carry the most of the wedge in absolute terms (B06B
# halibut longline, S05B salmon hand troll, S03T Bristol Bay drift gillnet).
# This figure puts both populations on one plot instead of needing two
# separate rankings, held owner-years on the x axis (log scale, the size of
# the fishery) against unused share on the y axis (the intensity), bubble
# size the raw count of unfished owner-years (n.unfished, literally how much
# of the wedge this fishery contributes in absolute terms). The two
# populations should separate visibly, a cluster of small, high-share codes
# near the top left and a cluster of large, moderate-share fisheries spread
# across the bottom right with the biggest bubbles.
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_fishery_year")) load(panel_path)

# Same floor as 04b_/04c_, a fishery held only a handful of times ever
# should not be on this plot at all, its unused share would be noise.
MIN_HELD_FOR_RANKING <- 30

fishery_scatter_data <- owner_fishery_year %>%
  filter(held) %>%
  group_by(Fishery) %>%
  summarise(
    n.held       = n(),
    n.unfished   = sum(!fished),
    unused.share = mean(!fished),
    .groups = "drop"
  ) %>%
  filter(n.held >= MIN_HELD_FOR_RANKING)

cat("\nFisheries plotted on the held-vs-unused scatter (n.held >=", MIN_HELD_FOR_RANKING, "):",
    nrow(fishery_scatter_data), "\n")

# Labeled points only, everything else would overlap unreadably with 200+
# fisheries on one plot. Two separate top-N selections, not one, by
# n.unfished (the raw-count story, largest absolute contributors to the
# wedge) and by unused.share restricted to LABEL_HELD_FLOOR or more held
# owner-years (a much higher floor than the plot's own MIN_HELD_FOR_RANKING,
# so a tiny code sitting at 1.00 share does not crowd out the handful of
# labels that floor exists to surface, e.g. L12T/L21E, which are already
# large enough to show up in the count-based selection anyway).
#
# No label-collision package (e.g. ggrepel) is loaded in 00_setup.R, kept
# that way here rather than adding a new dependency for about a dozen
# labels. nudge_y below is a manual, one-time placement, not a general
# solution. CHECK the rendered PNG for overlapping or clipped labels (a
# point near unused.share = 1.0 pushed up by nudge_y can run off the top
# edge) and adjust nudge_y/nudge_x or ylim here if so.
TOP_N_BY_COUNT <- 8
LABEL_HELD_FLOOR <- 5000
TOP_N_BY_SHARE <- 3

labels_by_count <- fishery_scatter_data %>%
  arrange(desc(n.unfished)) %>%
  slice_head(n = TOP_N_BY_COUNT)

labels_by_share <- fishery_scatter_data %>%
  filter(n.held >= LABEL_HELD_FLOOR) %>%
  arrange(desc(unused.share)) %>%
  slice_head(n = TOP_N_BY_SHARE)

fishery_scatter_labels <- bind_rows(labels_by_count, labels_by_share) %>%
  distinct(Fishery, .keep_all = TRUE)

cat("Labeled on the scatter:", paste(fishery_scatter_labels$Fishery, collapse = ", "), "\n")

figure_unused_scatter <- fishery_scatter_data %>%
  ggplot(aes(x = n.held, y = unused.share)) +
  geom_point(aes(size = n.unfished), alpha = 0.45, color = "steelblue") +
  geom_text(
    data = fishery_scatter_labels,
    aes(label = Fishery),
    size = 3, fontface = "bold", nudge_y = 0.03, color = "black"
  ) +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25)) +
  scale_size_continuous(name = "Unfished\nowner-years", labels = scales::comma) +
  labs(
    title = "Held owner-years versus unused share, by fishery",
    subtitle = paste0("Fisheries held at least ", format(MIN_HELD_FOR_RANKING, big.mark = ","),
                       " owner-years, 1991-2021 pooled, bubble size is the raw count of unfished owner-years"),
    x = "Held owner-years (log scale)", y = "Unused share (held but unfished)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure_unused_share_vs_held_scatter.png"),
       figure_unused_scatter, width = 9, height = 6.5, dpi = 300)

cat("Wrote figure_unused_share_vs_held_scatter.png to", figure_dir, "\n")
