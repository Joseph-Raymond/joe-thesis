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
# CONFIRMED against a real run (2026-09-24 console output), Section 1 below
# found only 6 of 579,178 held permit-serial-owner-years (0.001 percent)
# show more than one distinct current owner, so that signal is real but far
# too rare to say anything about the underlying question. That is expected,
# multi.owner.year only fires in the rare case where the extract happens to
# show two DIFFERENT File.Numbers both carrying Permit.Status == "Current
# Owner" for the same permit-year, an ordinary clean handoff (seller's row
# demoted to Former Owner, buyer's row Current Owner) never trips it at
# all, that ordinary case is invisible to Section 1's signal by
# construction. Section 2 below uses permit_ownership_history instead
# (every File.Number who ever appears on a permit-year, current OR former,
# 01_build_panel.R Section 1, captured before the Current-Owner filter
# drops the former-owner rows for good) to catch the ordinary case too.
#
# Reads intermediate data/ch3_panel.rdata built by 01_build_panel.R,
# specifically permit_year_owners (the per-permit-year distinct-CURRENT-
# owner count, Section 1 below), owner_permit_year (the permit-serial-grain
# held/fished object, 01_build_panel.R Section 7), and
# permit_ownership_history (every current-or-former holder by permit-year,
# 01_build_panel.R Section 1), all added to that script's save() list for
# exactly this diagnostic.

source("code/empirical_pipeline/00_setup.R")

if (!exists("owner_permit_year") || !exists("permit_year_owners") || !exists("permit_ownership_history")) {
  load(panel_path)
}

cat("\n\n########## Section 1. Multi-Current-Owner signal (rare edge case) ##########\n")

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

cat("\n\n########## Section 2. Former-Owner signal (catches an ordinary clean handoff too) ##########\n")

# Every permit-year with at least one Former Owner row, a genuine observed
# sale that year, someone other than the eventual Current Owner held it at
# some point in the same nominal year. This is the general case Section 1
# cannot see, it does not require two DIFFERENT File.Numbers to both carry
# Current Owner status at once, only that a Former Owner row exists at all.
sold_permit_years <- permit_ownership_history %>%
  filter(Permit.Status == "Former Owner") %>%
  distinct(Batch.Year, CFEC.Permit.Serial.Number) %>%
  mutate(sold.this.year = TRUE)

n.permit.years.total <- permit_ownership_history %>% distinct(Batch.Year, CFEC.Permit.Serial.Number) %>% nrow()

cat("Distinct permit x year cells in permit_ownership_history:", n.permit.years.total,
    "\nOf those, cells with at least one Former Owner row (an observed sale that year):",
    nrow(sold_permit_years), "(", round(100 * nrow(sold_permit_years) / n.permit.years.total, 3), "% )\n")

# ---- Buyer side, does the CURRENT owner of a just-sold permit show a
# different unused share than the current owner of a permit nobody sold
# that year? This is the direct test of "does a fresh purchase read as an
# idle backup permit", using owner_permit_year's held rows (Current-Owner
# survivors only, same population the chapter's own wedge is built on).
buyer_check <- owner_permit_year %>%
  filter(held) %>%
  left_join(sold_permit_years, by = c("Batch.Year", "CFEC.Permit.Serial.Number")) %>%
  mutate(sold.this.year = replace_na(sold.this.year, FALSE))

cat("\n===== Buyer side, unused share for the current owner, by whether this permit-year was sold",
    "at all (Former Owner row present) =====\n")
print(buyer_check %>%
        group_by(sold.this.year) %>%
        summarise(n.held = n(), unused.share = round(mean(!fished), 4), .groups = "drop"))

# ---- Seller side, of the File.Numbers who show up as a Former Owner
# somewhere that year (i.e., sold a permit they once held), how many left a
# ticket trail under that same permit-serial before selling versus none at
# all. A seller who fished it before selling is not a concern, held-and-
# genuinely-used. A seller who never fished it that year IS the scenario
# chat originally asked about, held (briefly) and unfished, but currently
# invisible to the chapter's held set entirely, since their register row is
# dropped by the Current-Owner filter rather than counted as unused.
sellers <- permit_ownership_history %>%
  filter(Permit.Status == "Former Owner") %>%
  distinct(Batch.Year, CFEC.Permit.Serial.Number, File.Number)

seller_check <- sellers %>%
  left_join(owner_permit_year %>% select(File.Number, Batch.Year, CFEC.Permit.Serial.Number, fished),
            by = c("File.Number", "Batch.Year", "CFEC.Permit.Serial.Number")) %>%
  mutate(fished.before.sale = replace_na(fished, FALSE))

cat("\n===== Seller side, sellers (Former Owner rows) by whether they left any ticket revenue under",
    "that permit-serial that year before selling =====\n")
print(seller_check %>%
        count(fished.before.sale) %>%
        mutate(share = round(n / sum(n), 4)))

cat("\nOf", nrow(sellers), "seller-permit-year rows,", sum(!seller_check$fished.before.sale),
    "(", round(100 * mean(!seller_check$fished.before.sale), 2),
    "% ) never generated a ticket under that permit-serial that year, held briefly and unfished",
    "before selling, currently excluded from the held set entirely rather than counted as unused.\n")
