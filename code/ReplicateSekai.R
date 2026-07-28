# List of required packages
packages <- c(
  'readr', 'tidyverse', 'dplyr', 'ggplot2', 'xlsx', 'data.table',
  'fixest', 'lubridate', 'corrplot', 'RColorBrewer', 'knitr',
  'cluster', 'factoextra', 'zoo', 'purrr', 'ggtext', 'gganimate', 'modelsummary', 'lme4'
)

# Install missing packages
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) install.packages(to_install)

# Load all packages quietly
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
wd_dir <- "/home/akfin/jraymond/Rprojects/joe-thesis"
setwd(wd_dir)
source("code/myfunctions.R")

# Read the combined data
combined_data <- readRDS(file.path("intermediate data", "combined_data_20250625_131406.rds"))

{
  trip.data <- get.trip.new(combined_data)
  trip.data$trip.duration <- as.numeric(trip.data$trip.length)+1
  trip.data <- trip.data %>% mutate(trip.duration=ifelse(trip.duration>=0,trip.duration, NA))#these trips have trip-date entry errors. Not throwing out the data for now but indicating the error by replacing any trips with negative trip duration as NA; going out and back the same day should lead to trip duration = 1
  rm(combined_data)
}
# want the number of permits tied a vessel owner
# want the number of vessels tied to a permit owner in a year
period_length <- 10

effort.stats <- get.effort.markers(trip.data)
# ---- Build owner-fishery-year with shares and per-year totals ----
vessel.owner.info <- trip.data %>%
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>%
  summarise(
    num.stat.areas = n_distinct(Statistical.Area),
    n_fish_perms   = n_distinct(Permit.Fishery),
    year.revenue   = sum(CFEC.Value.Detail, na.rm = TRUE),   # owner–fishery–year revenue
    num.trips      = n_distinct(day.vessel.id),
    fishing.days   = sum(trip.duration, na.rm = TRUE),
    max.seq.num    = max(CFEC.Permit.Sequence),
    Vessel.Length  = max(AKR.Vessel.Length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # owner–year totals
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>%
  mutate(owner.year.revenue = sum(year.revenue, na.rm = TRUE),
         num.fisheries      = n_distinct(Permit.Fishery)) %>%
  ungroup() %>%
  mutate(
    period = floor((Batch.Year - 1991) / period_length) + 1,
    fishery.annual.share = year.revenue / owner.year.revenue
  )

# active years per owner–period (years with positive owner revenue)
active_years <- vessel.owner.info %>%
  filter(is.finite(owner.year.revenue), owner.year.revenue > 0) %>%
  distinct(CFEC.Vessel.Owner.Filing.Number, period, Batch.Year) %>%
  count(CFEC.Vessel.Owner.Filing.Number, period, name = "num.active.years")

# ---- Equal-weighted long-run average shares across ACTIVE years ----
avg_share <- vessel.owner.info %>%
  filter(is.finite(owner.year.revenue), owner.year.revenue > 0) %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period, Permit.Fishery) %>%
  summarise(
    sum_share = sum(fishery.annual.share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(active_years, by = c("CFEC.Vessel.Owner.Filing.Number","period")) %>%
  mutate(avg.fishery.share = sum_share / num.active.years)

# Long-run HHI = sum_j (avg share_j)^2
lr_tbl <- avg_share %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
  summarise(lr.hhi = sum(avg.fishery.share^2, na.rm = TRUE), .groups = "drop")

# ---- Avg annual HHI (equal-weighted across ACTIVE years) ----
Hbar_tbl <- vessel.owner.info %>%
  filter(is.finite(owner.year.revenue), owner.year.revenue > 0) %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period, Batch.Year) %>%
  summarise(H_t = sum(fishery.annual.share^2, na.rm = TRUE), .groups = "drop") %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
  summarise(avg_annual_hhi = mean(H_t), .groups = "drop")

# ---- CV using ONE row per (owner, year) ----
owner_year_totals <- vessel.owner.info %>%
  filter(is.finite(owner.year.revenue), owner.year.revenue > 0) %>%
  distinct(CFEC.Vessel.Owner.Filing.Number, period, Batch.Year, owner.year.revenue,
           num.fisheries, Vessel.Length)  # keep per-year attributes if you need means later

cv_tbl <- owner_year_totals %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
  summarise(
    CV = sd(owner.year.revenue, na.rm = TRUE) / mean(owner.year.revenue, na.rm = TRUE),
    avg.fisheries = mean(num.fisheries, na.rm = TRUE),
    max.vessel.length = max(Vessel.Length, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Assemble final owner–period table with size bins ----
vessel.owner.period <- cv_tbl %>%
  left_join(lr_tbl,     by = c("CFEC.Vessel.Owner.Filing.Number","period")) %>%
  left_join(Hbar_tbl,   by = c("CFEC.Vessel.Owner.Filing.Number","period")) %>%
  mutate(
    rot_var = avg_annual_hhi - lr.hhi,
    vessel.size = case_when(
      max.vessel.length < 40 ~ "20-39",
      max.vessel.length < 60 ~ "40-59",
      TRUE                   ~ "60+"
    ),
    vessel.size = factor(vessel.size, levels = c("20-39","40-59","60+"))
  )

# ---- Sanity checks ----
# 1) Avg shares sum to ~1
check1 <- avg_share %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
  summarise(sum_avg_share = sum(avg.fishery.share), .groups = "drop")
summary(abs(check1$sum_avg_share - 1))    # should be near 0

# 2) Nonnegativity of rot_var (tolerance for FP noise)
summary(vessel.owner.period$rot_var)
sum(vessel.owner.period$rot_var < -1e-10) # should be 0


period.fishery.owner <- vessel.owner.info %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>% 
  summarize(period.fishery.revenue = sum(year.revenue, na.rm = TRUE)) %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
  mutate(period.total.revenue = sum(period.fishery.revenue, na.rm = TRUE), fishery.period.share = period.fishery.revenue/period.total.revenue, lr.hhi = sum(fishery.period.share^2,na.rm = TRUE)) %>%
  select(period, CFEC.Vessel.Owner.Filing.Number, lr.hhi)
  
#LR hhi data for CV regressions
vessel.owner.year <- vessel.owner.info %>%
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
  summarise(num.fisheries=first(num.fisheries), 
            annual.hhi=first(annual.hhi), 
            shannon.id=first(shannon.id), 
            num.species = n_distinct(species.letter), 
            period = first(period),
            Vessel.Length = max(Vessel.Length, na.rm = TRUE),
            owner.year.revenue = first(owner.year.revenue)) %>% 
  group_by(CFEC.Vessel.Owner.Filing.Number) %>%
  arrange(CFEC.Vessel.Owner.Filing.Number, Batch.Year) %>%#treating missing years as missing instead of zeros. i.e. if you leave for a year and come back then it's taking the hhi from the last year you fished
  mutate(annual.hhi.t1 = lead(annual.hhi), num.fisheries.t1 = lead(num.fisheries), shannon.id.t1 = lead(shannon.id)) %>%
  ungroup() %>% 
  left_join(period.fishery.owner, join_by(period, CFEC.Vessel.Owner.Filing.Number), multiple = "any")
  
vessel.owner.year <- vessel.owner.year %>% 
  filter(Vessel.Length >= 20) %>% 
  mutate(vessel.size = case_when(
    Vessel.Length < 40 ~ "20-39",
    Vessel.Length < 60 ~ "40-59",
    TRUE ~ "60+"
  ),
  vessel.size = factor(vessel.size, levels = c("20-39", "40-59", "60+")))


  

model.1 <- vessel.owner.year %>% 
  filter(!is.na(shannon.id.t1)) %>% 
  feols(shannon.id.t1 ~ num.fisheries | CFEC.Vessel.Owner.Filing.Number + Batch.Year)

model.2 <- vessel.owner.year %>% 
  filter(!is.na(shannon.id.t1)) %>% 
  feols(shannon.id.t1 ~ num.species | CFEC.Vessel.Owner.Filing.Number + Batch.Year)

model.1a <- vessel.owner.year %>%
  filter(!is.na(shannon.id.t1)) %>%
  feols(shannon.id.t1 ~ num.fisheries:vessel.size | CFEC.Vessel.Owner.Filing.Number + Batch.Year)

model.2a <- vessel.owner.year %>% 
  filter(!is.na(shannon.id.t1)) %>% 
  feols(shannon.id.t1 ~ num.species:vessel.size | CFEC.Vessel.Owner.Filing.Number + Batch.Year)

summary(model.1)
summary(model.2)
summary(model.1a)
summary(model.2a)


#need to reduce to the owner-period level to get the CV regression, will have Fe for period and owner vessel

model.3 <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ avg.fisheries | CFEC.Vessel.Owner.Filing.Number + period)
model.3a <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ avg.fisheries:vessel.size | CFEC.Vessel.Owner.Filing.Number + period)

model.4 <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ avg.species | CFEC.Vessel.Owner.Filing.Number + period)
model.4a <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ avg.species:vessel.size | CFEC.Vessel.Owner.Filing.Number + period)

summary(model.3)
summary(model.3a)
summary(model.4)
summary(model.4a)

# Create a named list of models
models1 <- list(
  "Shannon ∼ # Fisheries"           = model.1,
  "Shannon ∼ # Species"           = model.2,
  "Shannon ∼ Fisheries × Size"      = model.1a,
  "Shannon ∼ Species × Size"      = model.2a
)

models2 <- list(
  "log(CV) ∼ Avg Fisheries"         = model.3,
  "log(CV) ∼ Fisheries × Size"      = model.3a,
  "log(CV) ∼ Avg Species"         = model.4,
  "log(CV) ∼ Species × Size"      = model.4a
)
# Export LaTeX code (view in console or write to file)
modelsummary(models1, output = "latex")  # prints LaTeX
modelsummary(models2, output = "latex")  # prints LaTeX
# OR save to file:
modelsummary(models, output = "regression_results.tex")


#need to reduce to the owner-period level to get the CV regression, will have Fe for period and owner vessel

model.5 <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ avg.fisheries | CFEC.Vessel.Owner.Filing.Number + period)
model.5a <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ avg.fisheries:vessel.size | CFEC.Vessel.Owner.Filing.Number + period)

model.6 <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ log(lr.hhi) | CFEC.Vessel.Owner.Filing.Number + period)
model.6a <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ log(lr.hhi):vessel.size | CFEC.Vessel.Owner.Filing.Number + period)

model.5b <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ avg.fisheries:vessel.size | period)
model.6b <- vessel.owner.period %>% 
  filter(!is.na(CV)) %>%
  feols(log(CV) ~ log(lr.hhi):vessel.size | period)

summary(model.5)
summary(model.5a)
summary(model.6)
summary(model.6a)
summary(model.6b)
summary(model.6c)

models2 <- list(
  "No. Unique Permits"         = model.5,
  "No. Unique Permits"      = model.5a,
  "No. Unique Permits"      = model.5b,
  "Long Run HHI"         = model.6,
  "Long Run HHI"    = model.6a,
  "Long Run HHI"    = model.6b
)
# Export LaTeX code (view in console or write to file)
modelsummary(models1, output = "latex")  # prints LaTeX
modelsummary(models2, output = "latex")  # prints LaTeX


# Build period-level stats incl. mean & sd to decompose CV
vo_period <- vessel.owner.year %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
  summarise(
    mean_rev = mean(owner.year.revenue, na.rm = TRUE),
    sd_rev   = sd(owner.year.revenue,   na.rm = TRUE),
    CV       = sd_rev / mean_rev,
    avg_annual_hhi   = mean(annual.hhi,   na.rm = TRUE),
    avg_annual_shan  = mean(shannon.id,   na.rm = TRUE),
    vessel.size = first(vessel.size),
    years.fished = n(),
    .groups = "drop"
  ) %>%
  left_join(period.fishery.owner, by = c("CFEC.Vessel.Owner.Filing.Number","period"), multiple = "any") # adds lr.hhi


# 1) Long-run vs avg annual HHI side-by-side
model1_naomit <- vo_period %>% filter(years.fished>=2) %>% feols(log(CV) ~ avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)
model2_naomit <- vo_period %>% filter(years.fished>=2) %>% feols(log(CV) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
compare_model_hhi <- vo_period %>% filter(years.fished>=2) %>% feols(log(CV) ~ lr.hhi + avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)





# 2) Decompose the CV channel
model_mean <- vo_period %>% filter(years.fished>=2) %>% feols(log(mean_rev) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
model_sd <- vo_period %>% filter(years.fished>=2) %>%  feols(log(sd_rev)   ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)

models_decomp <- list(
  "(1)"         = model2_naomit,
  "(2)"      = model_mean,
  "(3)"      = model_sd
)

# 3) Sensitivity: treat inactive years as zeros
period_length <- 10

fill_zeros <- vessel.owner.year %>%
  group_by(CFEC.Vessel.Owner.Filing.Number) %>%
  # complete the owner’s timeline (only between min/max observed years for that owner)
  complete(Batch.Year = full_seq(Batch.Year, 1)) %>%
  # fill missing revenue with 0 for the newly created gap years
  mutate(owner.year.revenue = tidyr::replace_na(owner.year.revenue, 0)) %>%
  # recompute period *after* completion so gap rows get a period
  mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>%
  ungroup() %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
  summarise(
    mean_rev = mean(owner.year.revenue),        # zeros are included
    sd_rev   = sd(owner.year.revenue),
    CV0      = ifelse(mean_rev > 0, sd_rev / mean_rev, NA_real_),  # safe CV
    .groups  = "drop"
  ) %>%
  # bring in the baseline CV (computed without gap-filling) on the same keys
  left_join(
    vo_period %>% 
      select(CFEC.Vessel.Owner.Filing.Number, period, CV, avg_annual_hhi, lr.hhi),
    by = c("CFEC.Vessel.Owner.Filing.Number","period"), 
    multiple = "any"
  ) %>%
  mutate(cv.diff = CV0 - CV)


fill_zero_model1 <- fill_zeros %>% feols(log(CV0) ~ avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)
fill_zero_model2 <- fill_zeros %>% feols(log(CV0) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
fill_zero_model3 <- fill_zeros %>% feols(log(CV0) ~ lr.hhi + avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)

models3 <- list(
  "(1)"         = model1_naomit,
  "(2)"      = model2_naomit,
  "(3)"      = compare_model_hhi,
  "(4)"         = fill_zero_model1,
  "(5)"    = fill_zero_model2,
  "(6)"    = fill_zero_model3
)
modelsummary(models_decomp, output = "latex")  # prints LaTeX
modelsummary(models3, output = "latex")  # prints LaTeX




variance_model1 <- vo_period %>% filter(years.fished>=2) %>% mutate(rot_var = avg_annual_hhi-lr.hhi) %>% feols(log(CV) ~ rot_var | CFEC.Vessel.Owner.Filing.Number + period)
variance_model2 <- vo_period %>% filter(years.fished>=2) %>% mutate(rot_var = avg_annual_hhi-lr.hhi) %>% feols(log(CV) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)
variance_model3 <- vo_period %>% filter(years.fished>=2) %>% mutate(rot_var = avg_annual_hhi-lr.hhi) %>% feols(log(CV) ~ rot_var +lr.hhi:vessel.size | CFEC.Vessel.Owner.Filing.Number + period)
models_variance <- list(
  "(1)"         = variance_model1,
  "(2)"      = variance_model2,
  "(3)"      = variance_model3
)
modelsummary(models_variance, output = "latex")  # prints LaTeX

#Table 5
rm(reg.data)
reg.data <- vessel.owner.period %>% filter(!is.na(CV) & !is.na(CFEC.Vessel.Owner.Filing.Number)) 

model1 <- reg.data %>% 
  feols(log(CV) ~ log(avg_annual_hhi) | CFEC.Vessel.Owner.Filing.Number + period)
model2 <- reg.data %>%
  feols(log(CV) ~ log(lr.hhi) | CFEC.Vessel.Owner.Filing.Number + period)
model3 <- reg.data %>% 
  feols(log(CV) ~ log(rot_var) | CFEC.Vessel.Owner.Filing.Number + period)
model4 <- reg.data %>% 
  feols(log(CV) ~ log(rot_var) + log(avg_annual_hhi):vessel.size | CFEC.Vessel.Owner.Filing.Number + period)
model5 <- reg.data %>% 
  feols(log(CV) ~ log(lr.hhi) + log(rot_var) | CFEC.Vessel.Owner.Filing.Number + period)
model6 <- reg.data %>% 
  feols(log(CV) ~ log(rot_var) + log(avg_annual_hhi) | CFEC.Vessel.Owner.Filing.Number + period)

model1 <- reg.data %>% 
  feols(log(CV) ~ avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)
model2 <- reg.data %>%
  feols(log(CV) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
model3 <- reg.data %>% 
  feols(log(CV) ~ rot_var | CFEC.Vessel.Owner.Filing.Number + period)
model4 <- reg.data %>% 
  feols(log(CV) ~ rot_var + lr.hhi:vessel.size | CFEC.Vessel.Owner.Filing.Number + period)
model5 <- reg.data %>% filter(lr.hhi<1) %>% 
  feols(log(CV) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)
model6 <- reg.data %>% 
  feols(log(CV) ~ rot_var + avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)

Table5 <- list(
  "(1)"         = model1,
  "(2)"      = model2,
  "(3)"      = model3,
  "(4)"         = model5,
  "(5)"    = model6,
  "(6)"    = model4
)

etable(Table5, se="cluster", style = "aer", file = "feols_table.html")
modelsummary(Table5, output = "latex")  # prints LaTeX
modelsummary(Table5, output = "markdown")


m1 <- reg.data %>% 
  feols(log(CV) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
m2 <- reg.data %>%
  feols(log(mean_rev) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
m3 <- reg.data %>% 
  feols(log(sd_rev) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
m4 <- reg.data %>% 
  feols(log(CV) ~ rot_var | CFEC.Vessel.Owner.Filing.Number + period)
m5 <- reg.data %>% 
  feols(log(mean_rev) ~ rot_var | CFEC.Vessel.Owner.Filing.Number + period)
m6 <- reg.data %>% 
  feols(log(sd_rev) ~ rot_var | CFEC.Vessel.Owner.Filing.Number + period)
m7 <- reg.data %>% 
  feols(log(CV) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)
m8 <- reg.data %>% 
  feols(log(mean_rev) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)
m9 <- reg.data %>% 
  feols(log(sd_rev) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)


Table6 <- list(
"(1)" = m1,
"(2)" = m2,  
"(3)" = m3,  
"(4)" = m4,  
"(5)" = m5,  
"(6)" = m6,
"(7)" = m7,  
"(8)" = m8,  
"(9)" = m9
)
modelsummary(Table6, output = "markdown")
modelsummary(Table5, output = "latex")  # prints LaTeX

plot_data <- reg.data %>%
  mutate(
    rot_var = avg_annual_hhi - lr.hhi
  ) %>%
  filter(
    is.finite(lr.hhi), is.finite(rot_var),
    lr.hhi >= 0, lr.hhi <= 1,
    rot_var >= 0, rot_var <= 1 - lr.hhi
  )

# ---- Base heatmap (2D histogram) ----
p_heat <- reg.data %>% filter(lr.hhi<1 & period<4) %>% 
  ggplot( aes(x = lr.hhi, y = rot_var)) +
  geom_bin2d(bins = 50, show.legend = TRUE) +
  scale_fill_viridis_c(
    trans = "sqrt",   # helps reveal structure in dense bins
    option = "plasma",
    name = "Owner–periods"
  ) +
  # Feasible boundary: avg annual HHI ≤ 1  => rot_var ≤ 1 - L
  geom_abline(intercept = 1, slope = -1, linetype = "dashed", color = "grey40") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title = "Density of owner–periods by Long-run HHI vs Rotation variance",
    x = "Long-run HHI (L)",
    y = "Rotation variance (rot_var = avg_annual_hhi − L)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

p_heat

# add iso-lines for constant average annual HHI (Hbar = L + rot_var)
c_vals <- c(0.3, 0.5, 0.7, 0.9)  # choose any set in (0,1)
p_heat_iso <- p_heat +
  geom_abline(
    data = data.frame(c = c_vals),
    aes(intercept = c, slope = -1), inherit.aes = FALSE,
    linetype = "dotted", color = "grey55"
  ) +
  annotate("text",
           x = pmax(-0.05, c_vals - 0.98), y = pmin(0.98, c_vals - 0.02),
           label = paste0("H = ", c_vals),
           hjust = 0, vjust = 1, size = 3, color = "grey35")
p_heat_iso


effort.ratios <- effort.stats %>% 
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>% 
  summarise(num.weeks = n_distinct(week.number), 
            num.days = sum(trip.duration, na.rm = TRUE), 
            active.days.ratio = first(active.days.ratio),
            total.days.ratio = first(total.days.ratio)) %>% 
  ungroup() %>%
  mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>%
  mutate(avg.days = mean(num.days, na.rm = TRUE), avg.weeks = mean(num.weeks, na.rm = TRUE), avg.active.ratio = mean(active.days.ratio, na.rm = TRUE), avg.tot.ratio = mean(total.days.ratio, na.rm = TRUE)) %>% 
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
  mutate(num.active.weeks = sum(num.weeks, na.rm = TRUE)) %>% 
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>%
  mutate(A_t = num.weeks/num.active.weeks) %>% #proportion of active weeks dedicated to fishery 
  left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
    vessel.owner.info %>% 
      select(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, year.revenue),
    by = join_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery)
  ) %>% #now calculate k_t (CPUE)
  ungroup() %>% 
  mutate(R_t = year.revenue/num.weeks) %>% #average weekly revenue per week from the the specific fishery
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
  mutate(s_t = A_t*R_t/sum(A_t*R_t, na.rm = TRUE)) %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
  mutate(num.active.years = n_distinct(Batch.Year)) %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>% 
  mutate(A_0 = mean(A_t, na.rm = TRUE), R_0 = mean(R_t, na.rm = TRUE)) %>% #average fishery-owner level number of fishing weeks (A_0) and the revenue per week (R_0) averaged across years in which the fishers are active in some fishery across a period
  left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
    vessel.owner.info %>% select(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, fishery.annual.share),
    by = join_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery)
  ) %>% #check that the s_t is the same as a calculated share fisheries shares from earlier
  left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
    avg_share %>% select(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, avg.fishery.share),
    by = join_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery)
  ) #will use check that the s_0 is the same as a calculated share fisheries shares from earlier
rev0_tbl <- effort.ratios %>% ungroup() %>% distinct(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, A_0, R_0) %>%
  group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
  summarise(sum_rev_0 = sum(A_0*R_0))
effort.ratios <- effort.ratios %>% 
  left_join(rev0_tbl, by = join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
  mutate(s_0 = A_0*R_0/sum_rev_0, s_e = A_t*R_0/sum(A_t*R_0, na.rm = TRUE), s_k = A_0*R_t/sum(A_0*R_t, na.rm = TRUE)) %>%  # s_e is effort only updated and s_k is productivity only updated from the base cases
  mutate(H_0 = sum(s_0^2,na.rm = TRUE), H_e = sum(s_e^2 ,na.rm = TRUE), H_k = sum(s_k^2 ,na.rm = TRUE), H_t = sum(s_t^2 ,na.rm = TRUE),
         H_0c = sum(s_0,na.rm = TRUE), H_ec = sum(s_e ,na.rm = TRUE), H_kc = sum(s_k ,na.rm = TRUE), H_tc = sum(s_t ,na.rm = TRUE),
         delta_H = H_t - H_0,
         delta_H_e = 0.5*((H_e - H_0) + (H_t - H_k)),
         delta_H_k = 0.5*((H_k - H_0) + (H_t - H_e)),
         delta_H_int = delta_H - delta_H_e - delta_H_k) %>% #calculated the partial annual HHI's
  mutate(ME_e = H_e-H_0,
         ME_k = H_k-H_0,
         ME_int = H_t - H_e - H_k + H_0)

### CHECK THAT THE S_0 SUM TO ONE 
effort.ratios %>% ungroup() %>% distinct(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, s_0, avg.fishery.share) %>%
  group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
  summarise(s_0 = sum(s_0), avg.fishery.share = sum(avg.fishery.share)) %>% View()

### Make Alternative R_0
R_0_tbl <- effort.ratios %>% select(period, Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, R_t) %>% 
  left_join(reg.data %>% select(period, CFEC.Vessel.Owner.Filing.Number, vessel.size), by = join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
  group_by(Batch.Year, Permit.Fishery, vessel.size) %>% 
  mutate(R_t_fishery = median(R_t, na.rm = TRUE))
{
  effort.ratios.alt <- effort.stats %>% 
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>% 
    summarise(num.weeks = n_distinct(week.number), 
              num.days = sum(trip.duration, na.rm = TRUE), 
              active.days.ratio = first(active.days.ratio),
              total.days.ratio = first(total.days.ratio)) %>% 
    ungroup() %>%
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% 
    group_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>%
    mutate(avg.days = mean(num.days, na.rm = TRUE), avg.weeks = mean(num.weeks, na.rm = TRUE), avg.active.ratio = mean(active.days.ratio, na.rm = TRUE), avg.tot.ratio = mean(total.days.ratio, na.rm = TRUE)) %>% 
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
    mutate(num.active.weeks = sum(num.weeks, na.rm = TRUE)) %>% 
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>%
    mutate(A_t = num.weeks/num.active.weeks) %>% #proportion of active weeks dedicated to fishery 
    left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
      vessel.owner.info %>% 
        select(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, year.revenue),
      by = join_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery)
    ) %>% #now calculate k_t (CPUE)
    ungroup() %>% 
    mutate(R_t = year.revenue/num.weeks) %>% #average weekly revenue per week from the the specific fishery
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
    mutate(s_t = A_t*R_t/sum(A_t*R_t, na.rm = TRUE)) %>% 
    group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
    mutate(num.active.years = n_distinct(Batch.Year)) %>% 
    group_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>% 
    mutate(A_0 = mean(A_t, na.rm = TRUE)) %>% #use the alt version of the R_0
    left_join(R_0_tbl %>% ungroup() %>% select(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, R_t_fishery), join_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery)) %>% 
    mutate(R_0 = R_t_fishery) %>% 
    left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
      vessel.owner.info %>% select(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, fishery.annual.share),
      by = join_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery)
    ) %>% #check that the s_t is the same as a calculated share fisheries shares from earlier
    left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
      avg_share %>% select(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, avg.fishery.share),
      by = join_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery)
    ) #will use check that the s_0 is the same as a calculated share fisheries shares from earlier
  rev0_tbl_alt <- effort.ratios.alt %>% ungroup() %>% distinct(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, A_0, R_0) %>%
    group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
    summarise(sum_rev_0 = sum(A_0*R_0))
  effort.ratios.alt <- effort.ratios.alt %>% 
    left_join(rev0_tbl_alt, by = join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
    mutate(s_0 = A_0*R_0/sum_rev_0, s_e = A_t*R_0/sum(A_t*R_0, na.rm = TRUE), s_k = A_0*R_t/sum(A_0*R_t, na.rm = TRUE)) %>%  # s_e is effort only updated and s_k is productivity only updated from the base cases
    mutate(H_0 = sum(s_0^2,na.rm = TRUE), H_e = sum(s_e^2 ,na.rm = TRUE), H_k = sum(s_k^2 ,na.rm = TRUE), H_t = sum(s_t^2 ,na.rm = TRUE),
           H_0c = sum(s_0,na.rm = TRUE), H_ec = sum(s_e ,na.rm = TRUE), H_kc = sum(s_k ,na.rm = TRUE), H_tc = sum(s_t ,na.rm = TRUE),
           delta_H = H_t - H_0,
           delta_H_e = 0.5*((H_e - H_0) + (H_t - H_k)),
           delta_H_k = 0.5*((H_k - H_0) + (H_t - H_e)),
           delta_H_int = delta_H - delta_H_e - delta_H_k) %>% #calculated the partial annual HHI's
    mutate(ME_e = H_e-H_0,
           ME_k = H_k-H_0,
           ME_int = H_t - H_e - H_k + H_0)
  
  lr.hhi.tbl_2.alt <- effort.ratios.alt %>% 
    group_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>% 
    summarise(s_0 = first(s_0), avg.fishery.share = first(avg.fishery.share)) %>% 
    group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
    summarise(lr.hhi.alt = sum(s_0^2), lr.hhi = sum(avg.fishery.share^2))
  
  effort.reg.data.alt <- effort.ratios.alt %>% 
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
    summarise(delta_H = first(delta_H), delta_H_e = first(delta_H_e), delta_H_k = first(delta_H_k), delta_H_int = first(delta_H_int), period = first(period),
              ME_e = first(ME_e),
              ME_k = first(ME_k),
              ME_int = first(ME_int)) %>% 
    group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
    summarise(phi_H = mean(delta_H), phi_H_e = mean(delta_H_e), phi_H_k = mean(delta_H_k), phi_H_int = mean(delta_H_int), avg.ME_e = mean(ME_e), avg.ME_k = mean(ME_k), avg.ME_int = mean(ME_int)) %>% 
    left_join(lr.hhi.tbl_2.alt, join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
    left_join(cv_tbl, join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
    left_join(Hbar_tbl, join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
    mutate(rot_var = avg_annual_hhi - lr.hhi, rot_var_alt = avg_annual_hhi - lr.hhi.alt) %>% 
    filter(!is.na(CV) & !is.na(CFEC.Vessel.Owner.Filing.Number)) %>% 
    ungroup() %>% 
    mutate(check_sum_diff = avg.ME_e+avg.ME_k+avg.ME_int - rot_var_alt)
  
  hist(effort.reg.data.alt$check_sum_diff)
}

lr.hhi.tbl_2 <- effort.ratios %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>% 
  summarise(s_0 = first(s_0)) %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
  summarise(lr.hhi = sum(s_0^2))

effort.reg.data <- effort.ratios %>% 
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
  summarise(delta_H = first(delta_H), delta_H_e = first(delta_H_e), delta_H_k = first(delta_H_k), delta_H_int = first(delta_H_int), period = first(period),
            ME_e = first(ME_e),
            ME_k = first(ME_k),
            ME_int = first(ME_int)) %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
  summarise(phi_H = mean(delta_H), phi_H_e = mean(delta_H_e), phi_H_k = mean(delta_H_k), phi_H_int = mean(delta_H_int), avg.ME_e = mean(ME_e), avg.ME_k = mean(ME_k), avg.ME_int = mean(ME_int)) %>% 
  left_join(lr.hhi.tbl_2, join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
  left_join(cv_tbl, join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
  left_join(Hbar_tbl, join_by(period, CFEC.Vessel.Owner.Filing.Number)) %>% 
  mutate(rot_var = avg_annual_hhi - lr.hhi) %>% 
  filter(!is.na(CV) & !is.na(CFEC.Vessel.Owner.Filing.Number)) %>% 
  group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
  mutate(is.period.specialist = ifelse(lr.hhi==1, "YES", "NO"))
#R_0 leave one out
#R_t for fisheries that not participated in with others mean/median
effort_model_1 <- effort.reg.data %>% filter(lr.hhi<1) %>% 
  feols(log(CV) ~ lr.hhi  | CFEC.Vessel.Owner.Filing.Number + period)

effort_model_2 <- effort.reg.data %>% 
  feols(log(CV) ~ lr.hhi + is.period.specialist | CFEC.Vessel.Owner.Filing.Number + period)

effort_model_3a <- effort.reg.data %>% 
  feols(log(CV) ~ lr.hhi + avg.ME_e  + avg.ME_k + avg.ME_int | CFEC.Vessel.Owner.Filing.Number + period)
effort_model_3b <- effort.reg.data %>% filter(lr.hhi<1) %>% 
  feols(log(CV) ~ lr.hhi + avg.ME_e  + avg.ME_k + avg.ME_int | CFEC.Vessel.Owner.Filing.Number + period)
effort_model_4 <- effort.reg.data %>% filter(lr.hhi<1) %>% 
  feols(log(CV) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)

summary(effort_model_1)
summary(effort_model_2)
summary(effort_model_3a)
summary(effort_model_3b)
summary(effort_model_4)



{
  effort_model_3a.alt <- effort.reg.data.alt %>% filter(lr.hhi<1) %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_e  + avg.ME_k + avg.ME_int | CFEC.Vessel.Owner.Filing.Number + period)
  effort_model_3b.alt <- effort.reg.data.alt %>%
    feols(log(CV) ~ lr.hhi + avg.ME_e  + avg.ME_k + avg.ME_int | CFEC.Vessel.Owner.Filing.Number + period)
  effort_model_4.alt <- effort.reg.data.alt %>% filter(lr.hhi<1) %>% 
    feols(log(CV) ~ lr.hhi + rot_var_alt | CFEC.Vessel.Owner.Filing.Number + period)
  
  ffort_model_3a.alt <- effort.reg.data.alt %>% filter(lr.hhi<1) %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_e  + avg.ME_k | CFEC.Vessel.Owner.Filing.Number + period)
  effort_model_3b.alt <- effort.reg.data.alt %>%
    feols(log(CV) ~ lr.hhi + avg.ME_e  + avg.ME_k | CFEC.Vessel.Owner.Filing.Number + period)
  effort_model_4.alt <- effort.reg.data.alt %>% filter(lr.hhi<1) %>% 
    feols(log(CV) ~ lr.hhi + rot_var_alt | CFEC.Vessel.Owner.Filing.Number + period)
  
  
  summary(effort_model_3a.alt)
  summary(effort_model_3b.alt)
  summary(effort_model_4.alt)
  
}





{
  # ---------- 1) Build a clean plotting dataset ----------
  # We take one row per owner–year–fishery with H_e, H_k (levels) and delta_H_int (change).
  plot_df <- effort.ratios %>%
    # keep only the columns we need
    dplyr::select(
      CFEC.Vessel.Owner.Filing.Number, Batch.Year, period, Permit.Fishery,
      H_e, H_k, delta_H_int
    ) %>%
    # guard against accidental duplicates within owner–year–fishery
    distinct(CFEC.Vessel.Owner.Filing.Number, Batch.Year, Permit.Fishery, .keep_all = TRUE) %>%
    # long format: one row per component
    tidyr::pivot_longer(
      cols = c(H_e, H_k, delta_H_int),
      names_to = "component",
      values_to = "value"
    ) %>%
    # nicer labels
    dplyr::mutate(
      component = dplyr::recode(
        component,
        "H_e"         = "Effort-only H^e (level)",
        "H_k"         = "Productivity-only H^k (level)",
        "delta_H_int" = "Interaction (ΔH_int, change)"
      )
    )
  
  # Optional: if you have many fisheries, focus on the top-N by count in each period
  # topN <- 12
  # keep_fish <- plot_df %>%
  #   count(Permit.Fishery, period, name = "n") %>%
  #   group_by(period) %>% slice_max(n, n = topN) %>% ungroup()
  # plot_df <- plot_df %>% inner_join(keep_fish, by = c("Permit.Fishery","period"))
  
  # ---------- 2) Distributions by Permit.Fishery × period ----------
  library(ggplot2)
  
  # A) Density plots (good for seeing shape)
  p_density <- effort.ratios %>% filter(Permit.Fishery=="S03T") %>% 
    ggplot(aes(x = value, fill = component)) +
    geom_density(alpha = 0.35) +
    facet_grid(Permit.Fishery ~ period, scales = "free_y") +
    labs(
      title = "Distributions of H-components by Permit.Fishery × period",
      x = "Value",
      y = "Density",
      fill = "Component"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  # Print
  p_density
  
  # B) Violin + median (compact across components)
  p_violin <- plot_df %>%
    ggplot(aes(x = component, y = value, fill = component)) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    stat_summary(fun = median, geom = "point", size = 1.3, color = "black") +
    facet_grid(Permit.Fishery ~ period, scales = "free_y") +
    labs(
      title = "H^k, H^e (levels) and Interaction (ΔH_int) by Permit.Fishery × period",
      x = NULL, y = "Value"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 20, hjust = 1))
  
  p_violin
  
  # ---------- 3) Compact summary table (n, mean, median, p10/p90) ----------
  sumtab <- plot_df %>%
    group_by(Permit.Fishery, period, component) %>%
    summarise(
      n      = n(),
      mean   = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      p10    = quantile(value, 0.10, na.rm = TRUE),
      p90    = quantile(value, 0.90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(period, Permit.Fishery, component)
  
  print(sumtab, n = 200)
  
  # ---------- 4) Tips ----------
  # • H^e and H^k are HHI "levels" computed under counterfactual worlds,
  #   so their support is [0,1]. ΔH_int can be negative or positive (it’s a change).
  # • If you prefer one distribution per panel (less clutter), plot separate figures:
  #     filter(component == "Effort-only H^e (level)") %>% ...  etc.
  # • If the facet grid is too large, filter to a subset of key fisheries or periods,
  #   or use facet_wrap(~ Permit.Fishery + period, ncol = 4).
  
}
  



