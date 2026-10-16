# Chapter 3 empirical pipeline, persistence diagnostic for a single gear
# digit, S05B and the other gear-05 (hand troll/jig) fisheries by default.
#
# 00_setup.R flags gear 05 (and gear 02) as running an elevated unused
# share with no clean no-vessel story, left in for lack of evidence rather
# than cleared. The lifetime ghost-holder check in 01_build_panel.R answers
# a broader question, whether a holder ever appears on ANY ticket, in ANY
# fishery, across the whole 31-year panel. This asks the narrower,
# fishery-specific version instead. Among owners who ever held a given
# gear-05 fishery, how many never once fish THAT fishery across their own
# tenure in it, versus how many fish it in some years and sit idle in
# others. The first pattern looks like a true ghost holder or a linkage
# problem. The second looks like the backup-permit behavior the chapter's
# own prose already describes for S05B.
#
# An owner's tenure in a fishery is every year they held it, not just the
# years around a single flagged gap, so someone who buys in late or sells
# out early is judged only on the years they actually had the permit.
#
# Output is a CSV in Chpt3/output/tables/ with one row per fishery per
# pattern. Not wired into run_all.R, run standalone after 01_build_panel.R
# has produced data/ch3_panel.rdata.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_fishery_year")) load(panel_path)

TARGET_GEAR_DIGITS <- "05"

gear_rows <- owner_fishery_year %>%
  filter(held, substr(Fishery, 2, 3) == TARGET_GEAR_DIGITS)

owner_fishery_tenure <- gear_rows %>%
  group_by(File.Number, Fishery) %>%
  summarise(
    n.years.held     = n(),
    n.years.fished   = sum(fished),
    n.years.unfished = n.years.held - n.years.fished,
    .groups = "drop"
  ) %>%
  mutate(
    pattern = case_when(
      n.years.fished == 0   ~ "Never fished",
      n.years.unfished == 0 ~ "Always fished",
      TRUE                  ~ "Sometimes idle"
    )
  )

# "Always fished" owners never contribute an unfished owner-year by
# construction, kept here only as a denominator check against the other two.
summarise_patterns <- function(tenure) {
  tenure %>%
    group_by(pattern) %>%
    summarise(
      n.owners             = n(),
      owner.years.held     = sum(n.years.held),
      owner.years.unfished = sum(n.years.unfished),
      .groups = "drop"
    ) %>%
    mutate(
      total.unfished.years = sum(owner.years.unfished),
      share.of.owners       = round(n.owners / sum(n.owners), 3),
      share.of.unfished.years = if_else(total.unfished.years > 0,
                                         round(owner.years.unfished / total.unfished.years, 3),
                                         NA_real_)
    ) %>%
    select(-total.unfished.years) %>%
    arrange(desc(owner.years.unfished))
}

cat("\n===== Persistence pattern, S05B only =====\n")
print(summarise_patterns(owner_fishery_tenure %>% filter(Fishery == "S05B")))

cat("\n===== Persistence pattern, every fishery with gear digit", TARGET_GEAR_DIGITS, "pooled =====\n")
print(summarise_patterns(owner_fishery_tenure))

by_fishery <- owner_fishery_tenure %>%
  group_by(Fishery, pattern) %>%
  summarise(
    n.owners             = n(),
    owner.years.unfished = sum(n.years.unfished),
    .groups = "drop"
  ) %>%
  group_by(Fishery) %>%
  mutate(
    total.unfished.years = sum(owner.years.unfished),
    share.of.unfished.years = if_else(total.unfished.years > 0,
                                       round(owner.years.unfished / total.unfished.years, 3),
                                       NA_real_)
  ) %>%
  select(-total.unfished.years) %>%
  ungroup() %>%
  arrange(Fishery, desc(owner.years.unfished))

cat("\n===== Persistence pattern, by individual gear-", TARGET_GEAR_DIGITS, " fishery =====\n", sep = "")
print(by_fishery, n = Inf)

write.csv(by_fishery,
          file.path(table_dir, paste0("gear", TARGET_GEAR_DIGITS, "_persistence_by_fishery.csv")),
          row.names = FALSE)

cat("\nWrote gear", TARGET_GEAR_DIGITS, "_persistence_by_fishery.csv to ", table_dir, "\n", sep = "")
