# Chapter 3 empirical pipeline, one-off diagnostic, NOT part of run_all.R,
# run this standalone.
#
# Checks whether a permit that changed hands within a year shows a
# different unused share than a permit with a single stable owner that
# year. Chat raised this after asking whether a fisher who sells (or buys)
# a permit mid-year could show up as "held but unfished" for a reason that
# has nothing to do with genuinely holding idle backup access.
#
# 01_build_panel.R's held set requires Permit.Status == "Current Owner"
# (Section 1), and that same script's own resale diagnostic found that some
# permit-years have MORE than one distinct File.Number surviving that
# filter, so a transfer can leave a trace on the outgoing party, the
# incoming party, or both within the same nominal year, this diagnostic
# does not resolve which, only whether the transfer signal correlates with
# unused share at all. There is no exact transfer DATE in this register
# (chapter3_plan.md's own data dictionary lists Permit.Sequence and
# Permit.Status, not a date field), so a late-in-the-year purchase cannot
# be distinguished from an early one here, this can only test "did this
# permit-year involve more than one owner", not "when".
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R,
# specifically permit_year_owners (the per-permit-year distinct-owner
# count, 01_build_panel.R Section 1) and owner_permit_year (the permit-
# serial-grain held/fished object, 01_build_panel.R Section 7), both added
# to that script's save() list for exactly this diagnostic.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_permit_year") || !exists("permit_year_owners")) load(panel_path)

held_permit_transfer <- owner_permit_year %>%
  filter(held) %>%
  left_join(permit_year_owners, by = c("Batch.Year", "CFEC.Permit.Serial.Number"))

cat("\n===== Coverage check, held permit-serial-owner-years matched to a transfer-signal row =====\n")
cat("Held rows:", nrow(held_permit_transfer), ", matched to permit_year_owners:",
    sum(!is.na(held_permit_transfer$multi.owner.year)), "\n")

cat("\n===== Unused share by transfer signal (held owner-permit-years, multi.owner.year = more than",
    "one distinct File.Number carried Permit.Status == \"Current Owner\" for that permit that year) =====\n")
print(held_permit_transfer %>%
        filter(!is.na(multi.owner.year)) %>%
        group_by(multi.owner.year) %>%
        summarise(n.held = n(), unused.share = round(mean(!fished), 4), .groups = "drop"))

cat("\n===== Same check, multi-owner permit-years only, split further by exactly how many distinct",
    "owners that specific permit-year had =====\n")
print(held_permit_transfer %>%
        filter(multi.owner.year) %>%
        group_by(n.distinct.owners) %>%
        summarise(n.held = n(), unused.share = round(mean(!fished), 4), .groups = "drop"),
      n = Inf)

# Same comparison but restricted to owner-permit-year rows where THIS
# owner's OWN fishery-year is a multi-fishery owner-year (n.held.fishery >
# 1 elsewhere that year), since a transfer showing up as unused on an
# owner's ONLY permit that year is a different (and arguably less
# interesting) case than one showing up as unused alongside other permits
# that same owner did fish.
if (exists("owner_year")) {
  multi_permit_owners <- owner_year %>% filter(n.held.permit > 1) %>% distinct(File.Number, Batch.Year)

  cat("\n===== Same check, restricted to owner-years holding more than one permit-serial overall",
      "(n.held.permit > 1) =====\n")
  print(held_permit_transfer %>%
          filter(!is.na(multi.owner.year)) %>%
          semi_join(multi_permit_owners, by = c("File.Number", "Batch.Year")) %>%
          group_by(multi.owner.year) %>%
          summarise(n.held = n(), unused.share = round(mean(!fished), 4), .groups = "drop"))
} else {
  cat("\nowner_year not loaded, skipping the multi-permit-owner restriction above.\n")
}
