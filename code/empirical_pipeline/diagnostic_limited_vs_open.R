# Chapter 3 empirical pipeline, descriptive comparison of limited-entry and
# open fisheries, NOT part of run_all.R. Run standalone on the server.
#
# Permit-year counts come from owner_permit_year, so the unit is an owner and
# a permit serial in a fishery-year. Limited status comes from
# cfec_limited_status.csv (CFEC dictionary, by fishery and year). Rows marked
# "historical-outside-range" are dropped, the same as 01_build_panel.R Section 5.
# Fisheries with no status row count as Open.
#
# Parts
#   A.  Held and unfished permit-years by status, pooled and by year.
#   B.  Fishery-year unused shares by status, for fishery-years with at least
#       10 held permits, so tiny fisheries do not dominate the averages.
#   C.  Share of fished-permit revenue from limited fisheries, by year.
#   D.  Value rule against pounds rule for held permit-years, with the
#       cross-tab of the two flags and unused shares under each rule.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_permit_year") || !exists("fished_vessel_permit_year") || !exists("MAX_YEAR")) {
  load(panel_path)
}

limited_status <- read.csv(file.path(intermediate_dir, "cfec_limited_status.csv"),
                           stringsAsFactors = FALSE) %>%
  filter(source != "historical-outside-range") %>%
  transmute(Fishery, Batch.Year = as.integer(Batch.Year), limited = as.logical(limited))

held_panel <- owner_permit_year %>%
  filter(held, Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  left_join(limited_status, by = c("Fishery", "Batch.Year")) %>%
  mutate(
    status = if_else(replace_na(limited, FALSE), "Limited", "Open")
  )

# Permit-year pounds at the same grain as the pipeline's revenue. Tickets are
# summed by vessel and permit serial first, and the filing number is the first
# one on the serial's tickets, the pipeline's rule. The vessel-serial total is
# then summed to owner level by that filing number, so the pounds flag and the
# value flag assign each serial to the same owner. Zero-value landings count
# here, so this flag includes landings the value rule treats as unfished. Those
# rows are mostly discards, bait, and personal use (disposition codes 92 to 99).
pounds_vessel_serial <- catch_data_temp %>%
  filter(!is.na(CFEC.Permit.Serial.Number)) %>%
  mutate(
    Fishery                   = strip_fishery_space(CFEC.Permit.Fishery),
    Batch.Year                = as.integer(Batch.Year),
    CFEC.Permit.Serial.Number = as.integer(CFEC.Permit.Serial.Number),
    pounds                    = as.numeric(Pounds..Detail.)
  ) %>%
  group_by(Vessel.ADFG.Number, Batch.Year, Fishery, CFEC.Permit.Serial.Number) %>%
  summarise(
    permit.pounds = sum(pounds, na.rm = TRUE),
    File.Number   = first(CFEC.Permit.Holder.Filing.Number),
    .groups = "drop"
  )

pounds_by_permit <- pounds_vessel_serial %>%
  group_by(File.Number, Batch.Year, Fishery, CFEC.Permit.Serial.Number) %>%
  summarise(permit.pounds = sum(permit.pounds), .groups = "drop")

held_panel <- held_panel %>%
  mutate(CFEC.Permit.Serial.Number = as.integer(CFEC.Permit.Serial.Number)) %>%
  left_join(pounds_by_permit,
            by = c("Fishery", "Batch.Year", "CFEC.Permit.Serial.Number", "File.Number")) %>%
  mutate(
    permit.pounds = replace_na(permit.pounds, 0),
    fished.value  = fished,
    fished.pounds = permit.pounds > 0
  )

cat("\n===== A. Held permit-years by status, pooled =====\n")
pooled <- held_panel %>%
  group_by(status) %>%
  summarise(
    held.permit.years = n(),
    unfished          = sum(!fished),
    unused.share      = round(mean(!fished), 3),
    fisheries         = n_distinct(Fishery),
    .groups = "drop"
  )
print(pooled, n = Inf, width = Inf)

cat("\n===== A2. Held permit-years and unused share by status and year =====\n")
by_year <- held_panel %>%
  group_by(Batch.Year, status) %>%
  summarise(
    held   = n(),
    unused = round(mean(!fished), 3),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = status, values_from = c(held, unused))
print(by_year, n = Inf, width = Inf)

cat("\n===== B. Fishery-year unused share by status (at least 10 held permits) =====\n")
fishery_year <- held_panel %>%
  group_by(Fishery, Batch.Year, status) %>%
  summarise(
    held   = n(),
    unused = mean(!fished),
    .groups = "drop"
  ) %>%
  filter(held >= 10)
print(
  fishery_year %>%
    group_by(status) %>%
    summarise(
      fishery.years = n(),
      median.unused = round(median(unused), 3),
      mean.unused   = round(mean(unused), 3),
      .groups = "drop"
    ),
  n = Inf, width = Inf
)

cat("\n===== C. Share of fished-permit revenue from limited fisheries, by year =====\n")
revenue_share <- fished_vessel_permit_year %>%
  filter(revenue > 0, Batch.Year >= MIN_YEAR, Batch.Year <= MAX_YEAR) %>%
  left_join(limited_status, by = c("Fishery", "Batch.Year")) %>%
  mutate(status = if_else(replace_na(limited, FALSE), "Limited", "Open")) %>%
  group_by(Batch.Year, status) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = status, values_from = revenue, values_fill = 0) %>%
  mutate(limited.revenue.share = round(Limited / (Limited + Open), 3))
print(revenue_share, n = Inf, width = Inf)

cat("\n===== D. Value rule against pounds rule, held permit-years =====\n")
cat("The value rule is revenue above zero, the pipeline rule.\n",
    "The pounds rule is any positive pounds, including zero-value landings.\n")
print(held_panel %>% count(fished.value, fished.pounds), n = Inf, width = Inf)

cat("\n===== D2. Unused share under each rule, by status and year =====\n")
# Count names differ from the flag names, so mean() reads the flags, not counts.
rule_by_year <- held_panel %>%
  group_by(Batch.Year, status) %>%
  summarise(
    held            = n(),
    n.fished.value  = sum(fished.value),
    n.fished.pounds = sum(fished.pounds),
    unused.value    = round(mean(!fished.value), 3),
    unused.pounds   = round(mean(!fished.pounds), 3),
    .groups = "drop"
  )
print(rule_by_year, n = Inf, width = Inf)

cat("\n===== D3. Pooled unused share under each rule, by status =====\n")
print(
  held_panel %>%
    group_by(status) %>%
    summarise(
      held          = n(),
      unused.value  = round(mean(!fished.value), 3),
      unused.pounds = round(mean(!fished.pounds), 3),
      .groups = "drop"
    ),
  n = Inf, width = Inf
)
