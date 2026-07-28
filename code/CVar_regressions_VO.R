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

period_length <- 10

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
  ) %>% 
  group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
  mutate(annual.hhi = sum(fishery.annual.share^2, na.rm = TRUE), shannon.id = -sum(fishery.annual.share*log(fishery.annual.share),na.rm = TRUE)) %>% ungroup() %>% 
  filter(!is.na(CFEC.Vessel.Owner.Filing.Number), Vessel.Length>0)


streamline_m <- function(df){
  list <- c("M6AB","M6AG","M6BB","M6BG","M7FB","M7FG","M7GB","M7GG", "M7HB","M7HG","M7IB","M7IG")
  df %>% mutate(Permit.Fishery=ifelse(Permit.Fishery %in% list, paste0(substr(Permit.Fishery,1,1), "0",substr(Permit.Fishery,2,2),substr(Permit.Fishery,4,4)),Permit.Fishery)) %>% return()
}
streamline_c <- function(df){
  list <- c("C5AE","C5BE","C5CE","C5DE","C4AE","C4BE","C4CE","C4DE")
  df %>% mutate(Permit.Fishery=ifelse(Permit.Fishery %in% list, paste0(substr(Permit.Fishery,1,1), "0",substr(Permit.Fishery,2,2),substr(Permit.Fishery,4,4)),Permit.Fishery)) %>% return()
}

vessel.owner.info <- streamline_m(vessel.owner.info)
vessel.owner.info <- streamline_c(vessel.owner.info)

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

mean_sd_tbl <- owner_year_totals %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
  summarise(
    rev.SD = sd(owner.year.revenue, na.rm = TRUE),
    rev.mean = mean(owner.year.revenue, na.rm = TRUE),
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

#LR hhi data for CV regressions
vessel.owner.year <- vessel.owner.info %>%
  mutate(species.letter = substr(Permit.Fishery,1,1)) %>% filter(species.letter != "0" & species.letter != "9") %>% 
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
  left_join(vessel.owner.period %>% select(period, CFEC.Vessel.Owner.Filing.Number, lr.hhi), join_by(period, CFEC.Vessel.Owner.Filing.Number), multiple = "any") %>% 
  ungroup() %>% 
  filter(Vessel.Length >= 20) %>% 
  mutate(vessel.size = case_when(
    Vessel.Length < 40 ~ "20-39",
    Vessel.Length < 60 ~ "40-59",
    TRUE ~ "60+"
      ),
  vessel.size = factor(vessel.size, levels = c("20-39", "40-59", "60+")))

reg.data <- vessel.owner.period %>% filter(!is.na(CV) & !is.na(CFEC.Vessel.Owner.Filing.Number)) 







# ===========================
# (A) Downside risk: CVaR25
# ===========================

# Helper: CVaR_q for a numeric vector (lower tail)
cvar_lower <- function(x, q = 0.25) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  thr <- as.numeric(quantile(x, probs = q, type = 7, na.rm = TRUE))
  mean(x[x <= thr], na.rm = TRUE)
}

# Build (owner, period) downside-risk stats from owner-year revenues
# Uses your existing 'vessel.owner.year' (has owner.year.revenue, period, etc.)
downside_tbl <- vessel.owner.year %>%
  filter(is.finite(owner.year.revenue)) %>%
  group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
  summarise(
    years_fished  = n(),
    mean_rev      = mean(owner.year.revenue),
    sd_rev        = sd(owner.year.revenue),
    CV            = sd_rev / mean_rev,
    q25_rev       = as.numeric(quantile(owner.year.revenue, 0.25, type = 7)),
    cvar25_rev    = cvar_lower(owner.year.revenue, q = 0.25),   # mean of worst 25% years
    # Scale CVaR by mean to be comparable across owners (unitless, in [0, +∞))
    cvar25_ratio  = cvar25_rev / mean_rev,                      # bigger is worse downside
    # Optional: a shortfall intensity (how far left-tail mean sits below the 25th pct), normalized
    shortfall25   = (q25_rev - cvar25_rev) / mean_rev,
    .groups = "drop"
  )

# Merge downside risk into your owner–period regression data
reg.data.vo <- reg.data %>%
  select(-CV) %>%                                # if present; we'll re-attach from downside_tbl
  left_join(downside_tbl %>% select(CFEC.Vessel.Owner.Filing.Number, period, CV, cvar25_ratio, shortfall25),
            by = c("CFEC.Vessel.Owner.Filing.Number","period"))

#models with just average annual HHI and HHI^{LR}
m_lr_short_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(shortfall25) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
m_lr_cvar_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(cvar25_ratio) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)
m_lr_CV_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(CV) ~ lr.hhi | CFEC.Vessel.Owner.Filing.Number + period)

m_aa_short_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(shortfall25) ~ avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)
m_aa_cvar_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(cvar25_ratio) ~ avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)
m_aa_CV_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(CV) ~ avg_annual_hhi | CFEC.Vessel.Owner.Filing.Number + period)

m_aa_short_vo
m_aa_cvar_vo
m_aa_CV_vo

m_lr_short_vo
m_lr_cvar_vo
m_lr_CV_vo

#PRINT REGRESSION RESULTS
{
  library(fixest)
  
  # Coefficient labels
  dict <- c(
    "lr.hhi"                   = "Long-run HHI",
    "avg_annual_hhi"           = "Avg. annual HHI"
  )
  
  # Name each model with the desired column title
  models_named <- list(
    "Tail depth (log shortfall25)"    = m_lr_short_vo,
    "Downside Risk (log CVaR25/mean)" = m_lr_cvar_vo,
    "Volatility (log CV)"             = m_lr_CV_vo,
    "Tail depth (log shortfall25)"    = m_aa_short_vo,
    "Downside Risk (log CVaR25/mean)"= m_aa_cvar_vo,
    "Volatility (log CV)"             = m_aa_CV_vo
  )
  
  # Build LaTeX table (no mnames)
  latex_tbl <- do.call(
    etable,
    c(
      models_named,   # spliced as six separate, named model args
      list(
        headers     = list("Long-run HHI" = 3, "Avg. annual HHI" = 3),
        dict        = dict,
        vcov        = ~ CFEC.Vessel.Owner.Filing.Number + period,  # two-way clustered SEs
        se.below    = TRUE,
        digits      = 3,
        fixef_sizes = FALSE,
        fitstat     = ~ n + r2,
        # If your fixest is very old and errors on 'style', just remove the next line:
        # style       = "aer",
        tex         = TRUE
      )
    )
  )
  
  cat(latex_tbl)                                # print to console (Rmd: results='asis')
  cat(latex_tbl, file = "table_minimal_diversification_risk.tex")  # write to file
  
}

# Example: run the same models with CVaR25_ratio instead of CV
# (semi-elasticity: log(CVaR25_ratio) on HLR and Phi)
m_cvar_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(cvar25_ratio) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)

# You can also test shortfall25 directly (already unitless, positive = worse downside)
m_short_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(shortfall25) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)

m_CV_vo <- reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(log(CV) ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)

#summary(m_cvar_po)
summary(m_cvar_vo)
#summary(m_short_po)
summary(m_short_vo)
#summary(m_CV_po)
summary(m_CV_vo)

######
#test results
######



reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(avg_annual_hhi ~ avg.fisheries | CFEC.Vessel.Owner.Filing.Number + period)

reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(lr.hhi ~ avg.fisheries | CFEC.Vessel.Owner.Filing.Number + period)

reg.data.vo %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(rot_var ~ avg.fisheries | CFEC.Vessel.Owner.Filing.Number + period)

reg.data.vo %>% filter(period==2) %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
  feols(rot_var ~ avg.fisheries)

reg.data.vo %>%filter(period==3) %>%
  filter(is.finite(CV), CV > 0, lr.hhi!=1) %>% 
  feols(avg.fisheries ~ lr.hhi + rot_var)

reg.data.vo %>% 
  left_join(active_years, join_by(CFEC.Vessel.Owner.Filing.Number, period)) %>% 
  filter(is.finite(CV), CV > 0, lr.hhi!=1, num.active.years >7) %>%
  feols(log(CV) ~ avg.fisheries + lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period)

reg.data.map %>% filter(is.finite(CV), CV > 0, lr.hhi!=1) %>% 
  group_by(period, Permit.Fishery) %>% 
  summarise(median_rot_var = median(rot_var), 
            mean_rot_var = mean(rot_var), 
            count = n()) %>% filter(count>20) %>% View()

{
  # Make sure the directory exists
  dir.create("tables", showWarnings = FALSE)
  # Bundle your already-estimated models with readable column titles
  models_vo <- list(
    "Downside (log CVaR25/mean)" = m_cvar_vo,
    "Tail depth (log shortfall25)" = m_short_vo,
    "Volatility (log CV)" = m_CV_vo
  )
  
  # Pretty labels for coefficients
  coef_map <- c(
    "lr.hhi"                 = "Long-run HHI ($H^{LR}$)",
    "rot_var"                = "Share variance ($\\Phi$)"
  )
  
  # Goodness-of-fit rows to display (works with fixest)
  gof_map <- tibble::tribble(
    ~raw,          ~clean,          ~fmt,
    "nobs",        "Observations",   0,
    "r2.within",   "R$^2$ (within)", 3,
    "rmse",        "RMSE",           3
  )
  
  # Build the LaTeX table with two-way clustered SEs (owner + period)
  modelsummary(
    models_vo,
    output   = "tables/results_vo.tex",
    coef_map = coef_map,
    gof_map  = gof_map,
    vcov     = ~ CFEC.Vessel.Owner.Filing.Number + period,   # two-way clustered SEs
    stars    = c('*' = .10, '**' = .05, '***' = .01),
    title    = "Determinants of downside risk and volatility (Vessel owner × period)",
    notes    = "All models include permit-holder and period fixed effects. Standard errors are two-way clustered by permit-holder and period."
  )
  
  cat("\nSaved LaTeX table -> tables/results_vo.tex\n")
}


# Run function for three outcome vars to decompose the avg annual HHI effect
samp <- reg.data.vo %>% filter(is.finite(CV), CV > 0, lr.hhi != 1)

out_CV   <- decompose_avgH_effect(samp, yvar = "CV")
out_CVaR <- decompose_avgH_effect(samp, yvar = "cvar25_ratio")
out_SF   <- decompose_avgH_effect(samp, yvar = "shortfall25")

bind_rows(out_CV, out_CVaR, out_SF)






















# ============================================================
# (B) Who/what is driving the CV regressions? Two approaches
# ============================================================

# --- B1. Fast "influence score" (no re-fit):
# Residualize Y and regressors by FE (two-way within transform),
# fit plain OLS w/out FE, and compute group influence ~ |x * resid|
# ----------------------------------------------------------------

# Two-way within transform by (fe1, fe2):  y~ and X~ (additive FE)
{
  within_tw <- function(df, y, xs, fe1, fe2) {
    stopifnot(all(c(y, xs, fe1, fe2) %in% names(df)))
    tmp <- df %>%
      mutate(
        ..y = .data[[y]]
      )
    # Overall means
    y_bar  <- mean(tmp$..y, na.rm = TRUE)
    x_bars <- sapply(xs, function(v) mean(tmp[[v]], na.rm = TRUE))
    
    # FE1 means
    fe1m <- tmp %>% group_by(.data[[fe1]]) %>%
      summarise(y_fe1 = mean(..y, na.rm = TRUE),
                across(all_of(xs), ~ mean(.x, na.rm = TRUE), .names = "x_{col}_fe1"),
                .groups = "drop")
    # FE2 means
    fe2m <- tmp %>% group_by(.data[[fe2]]) %>%
      summarise(y_fe2 = mean(..y, na.rm = TRUE),
                across(all_of(xs), ~ mean(.x, na.rm = TRUE), .names = "x_{col}_fe2"),
                .groups = "drop")
    
    out <- tmp %>%
      left_join(fe1m, by = setNames(fe1, fe1)) %>%
      left_join(fe2m, by = setNames(fe2, fe2)) %>%
      mutate(
        y_within = ..y - y_fe1 - y_fe2 + y_bar
      )
    
    for (v in xs) {
      out[[paste0(v, "_within")]] <-
        out[[v]] - out[[paste0("x_", v, "_fe1")]] - out[[paste0("x_", v, "_fe2")]] + x_bars[[v]]
    }
    
    keep_cols <- c(fe1, fe2, "y_within", paste0(xs, "_within"), xs)
    out %>% select(all_of(keep_cols))
  }
  
  # Influence scoring for a feols spec with y ~ X | fe1 + fe2
  # Returns per-group (owner OR fishery OR period) aggregate influence on the coefficient of 'term'
  influence_score <- function(df, y, xs, fe1, fe2, term, group_var) {
    stopifnot(term %in% xs, group_var %in% names(df))
    W <- within_tw(df, y, xs, fe1, fe2)
    
    y_tilde <- W$y_within
    X_tilde <- as.matrix(W[paste0(xs, "_within")])
    colnames(X_tilde) <- xs
    
    # Plain OLS on within-variables
    fit <- lm.fit(x = X_tilde, y = y_tilde)
    res <- y_tilde - X_tilde %*% fit$coefficients
    
    # Observation-level score for 'term'
    score_i <- abs(X_tilde[, term]) * abs(res)
    
    tibble::tibble(
      !!group_var := df[[group_var]],
      score = score_i
    ) %>%
      group_by(.data[[group_var]]) %>%
      summarise(influence = sum(score, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(influence))
  }
  
  # Example: who drives the 'rot_var' coefficient in CV regression?
  # Use the same reg.data already regressed on.
  # make sure the regressors are in the data
  reg.data.vo <- reg.data.vo %>%
    mutate(
      rot_var = ifelse(!is.finite(rot_var) & is.finite(avg_annual_hhi) & is.finite(lr.hhi),
                       avg_annual_hhi - lr.hhi, rot_var),
      logCV   = log(CV)
    )
  
  drivers_owner_rot <- influence_score(
    df       = reg.data.vo %>% filter(is.finite(logCV), is.finite(lr.hhi), is.finite(rot_var)),
    y        = "logCV",                                  # <-- a column name, not an expression
    xs       = c("lr.hhi", "rot_var"),
    fe1      = "CFEC.Vessel.Owner.Filing.Number",
    fe2      = "period",
    term     = "rot_var",
    group_var= "CFEC.Vessel.Owner.Filing.Number"
  )
  
  # Top 20 owners:
  head(drivers_owner_rot, 20)
  
  # The same for Permit.Fishery: first join a representative fishery per row (e.g., the
  # modal fishery in the period or long-run main fishery). If you already have a mapping
  # reg.data -> dominant fishery, use that column instead of the example below.
  dominant_fishery <- avg_share %>%
    group_by(CFEC.Vessel.Owner.Filing.Number, period) %>%
    slice_max(order_by = avg.fishery.share, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(CFEC.Vessel.Owner.Filing.Number, period, Permit.Fishery)
  
  reg.data.map <- reg.data.vo %>%
    left_join(dominant_fishery, by = c("CFEC.Vessel.Owner.Filing.Number","period"))
  
  drivers_fishery_rot <- influence_score(
    df     = reg.data.map %>% filter(is.finite(CV)),
    y      = "logCV",
    xs     = c("lr.hhi", "rot_var"),
    fe1    = "CFEC.Vessel.Owner.Filing.Number",
    fe2    = "period",
    term   = "rot_var",
    group_var = "Permit.Fishery"
  )
  
  head(drivers_fishery_rot, 20)
  
  # Visual: lollipop plot of top drivers
  
  plot_top_drivers <- function(tab, group_var, top_n = 25, title = "Top drivers") {
    gv <- rlang::sym(group_var)
    tab %>%
      slice_max(influence, n = top_n) %>%
      mutate(rank = rank(-influence),
             label = as.character(!!gv)) %>%
      ggplot(aes(x = reorder(label, influence), y = influence)) +
      geom_segment(aes(xend = label, y = 0, yend = influence)) +
      geom_point(size = 2) +
      coord_flip() +
      labs(x = group_var, y = "Influence score", title = title) +
      theme_minimal(base_size = 11)
  }
  
  p_owner <- plot_top_drivers(drivers_owner_rot, "CFEC.Vessel.Owner.Filing.Number",
                              title = "Top owners driving the rot_var coefficient (vessel owners)")
  p_fish  <- plot_top_drivers(drivers_fishery_rot, "Permit.Fishery",
                              title = "Top fisheries driving the rot_var coefficient (vessel owners)")
  
  p_owner; p_fish
}

# __________________
# SECTION TO FIND FISHERIES THAT HAVE OPPOSITE RELATIONSHIP WITH (CV ~ DIVERSIFICATION) THAN THE AVERAGE
# __________________
{
  # ---------- SAFE PREP FOR CONTRARIAN ANALYSIS ----------
  # -------------------------------
  # 0) Prune CV == 0 and prep data
  # -------------------------------
  
  safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)
  
  # start from your reg.data and vessel.owner.info
  reg.data.vo <- reg.data %>%
    # (A) remove CV<=0 owner–periods
    filter(is.finite(CV), CV > 0) %>%
    # (B) (re)build rot_var if needed
    mutate(
      rot_var = ifelse(is.finite(rot_var),
                       rot_var,
                       ifelse(is.finite(avg_annual_hhi) & is.finite(lr.hhi),
                              avg_annual_hhi - lr.hhi, NA_real_)),
      logCV = safe_log(CV)
    )
  
  # Dominant fishery per owner–period (for mapping groups)
  dom_fish <- vessel.owner.info %>%
    filter(is.finite(year.revenue), year.revenue > 0) %>%
    group_by(CFEC.Vessel.Owner.Filing.Number, period, Permit.Fishery) %>%
    summarise(period_rev = sum(year.revenue, na.rm = TRUE), .groups = "drop_last") %>%
    slice_max(order_by = period_rev, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(CFEC.Vessel.Owner.Filing.Number, period, Permit.Fishery)
  
  reg.data.map <- reg.data.vo %>%
    left_join(dom_fish, by = c("CFEC.Vessel.Owner.Filing.Number","period"))
  # -------------------------------
  # 1) Two-way within transform
  # -------------------------------
  within_tw_any <- function(df, y, vars, fe1, fe2){
    y_bar <- mean(df[[y]], na.rm = TRUE)
    x_bar <- sapply(vars, function(v) mean(df[[v]], na.rm = TRUE))
    
    fe1m <- df %>% group_by(.data[[fe1]]) %>%
      summarise(y_fe1 = mean(.data[[y]], na.rm = TRUE),
                across(all_of(vars), ~ mean(.x, na.rm = TRUE), .names = "x_{col}_fe1"),
                .groups = "drop")
    fe2m <- df %>% group_by(.data[[fe2]]) %>%
      summarise(y_fe2 = mean(.data[[y]], na.rm = TRUE),
                across(all_of(vars), ~ mean(.x, na.rm = TRUE), .names = "x_{col}_fe2"),
                .groups = "drop")
    
    out <- df %>%
      left_join(fe1m, by = setNames(fe1, fe1)) %>%
      left_join(fe2m, by = setNames(fe2, fe2)) %>%
      mutate(y_w = .data[[y]] - y_fe1 - y_fe2 + y_bar)
    
    for(v in vars){
      out[[paste0(v, "_w")]] <-
        out[[v]] - out[[paste0("x_", v, "_fe1")]] - out[[paste0("x_", v, "_fe2")]] + x_bar[[v]]
    }
    out
  }
  
  # -------------------------------
  # 2) FWL local slopes by group
  # -------------------------------
  fwl_local_by <- function(df, y, z, controls, fe1, fe2, group_var,
                           min_n = 25, min_var = 1e-10){
    
    need <- unique(c(y, z, controls, fe1, fe2, group_var))
    
    # typed finite filter: numeric -> is.finite, non-numeric -> !is.na
    d0 <- df %>%
      filter(if_all(all_of(need), ~ {
        if (is.numeric(.x)) {
          is.finite(.x)
        } else {
          !is.na(.x)
        }
      }))
    
    if (nrow(d0) == 0L) stop("No rows left after typed finite filter. Check inputs.")
    
    W <- within_tw_any(d0, y = y, vars = c(z, controls), fe1 = fe1, fe2 = fe2)
    y_w <- W$y_w
    z_w <- W[[paste0(z, "_w")]]
    
    if (length(controls)) {
      Xc_w <- as.matrix(W[paste0(controls, "_w")])
      if (nrow(Xc_w) > 0 && ncol(Xc_w) > 0) {
        ry <- as.numeric(y_w - Xc_w %*% lm.fit(x = Xc_w, y = y_w)$coefficients)
        rz <- as.numeric(z_w - Xc_w %*% lm.fit(x = Xc_w, y = z_w)$coefficients)
      } else { ry <- as.numeric(y_w); rz <- as.numeric(z_w) }
    } else { ry <- as.numeric(y_w); rz <- as.numeric(z_w) }
    
    dd <- d0 %>% mutate(.ry = ry, .rz = rz) %>% filter(is.finite(.ry), is.finite(.rz))
    
    S_all <- sum(dd$.rz * dd$.ry); V_all <- sum(dd$.rz^2)
    beta_all <- S_all / V_all
    
    out <- dd %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        n            = n(),
        S_num        = sum(.rz * .ry),
        V_den        = sum(.rz^2),
        local_beta   = ifelse(n >= min_n & V_den > min_var, S_num / V_den, NA_real_),
        contrib_share = ifelse(S_all != 0, S_num / S_all, NA_real_),  # signed share of the slope
        pushes_sign   = sign(S_num),
        .groups = "drop"
      ) %>%
      mutate(
        contrarian = ifelse(sign(beta_all) > 0, local_beta < 0,
                            ifelse(sign(beta_all) < 0, local_beta > 0, NA))
      ) %>%
      arrange(desc(abs(contrib_share)))
    
    attr(out, "global_beta") <- beta_all
    out
  }
  # CV ~ Phi | HLR, with owner + period FE
  fish_phi <- fwl_local_by(
    df        = reg.data.map,
    y         = "logCV",
    z         = "rot_var",
    controls  = c("lr.hhi"),
    fe1       = "CFEC.Vessel.Owner.Filing.Number",
    fe2       = "period",
    group_var = "Permit.Fishery",
    min_n     = 25
  )
  
  attr(fish_phi, "global_beta")  # sample-wide partial slope
  fish_phi %>% filter(!is.na(local_beta), contrarian) %>% arrange(local_beta) %>% head(20)
  
  # CV ~ HLR | Phi
  fish_hlr <- fwl_local_by(
    df        = reg.data.map,
    y         = "logCV",
    z         = "lr.hhi",
    controls  = c("rot_var"),
    fe1       = "CFEC.Vessel.Owner.Filing.Number",
    fe2       = "period",
    group_var = "Permit.Fishery",
    min_n     = 25
  )
  attr(fish_hlr, "global_beta")
  fish_hlr %>% filter(!is.na(local_beta), contrarian) %>% arrange(local_beta) %>% head(20)
  
  
}

# --------------------------------------------
# Per-fishery FE regressions on dominant owners
# --------------------------------------------
{
  
  # 0) Helper(s)
  safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)
  
  # 1) Dominant fishery per owner×period (largest revenue within the period)
  dom_fish <- vessel.owner.info %>%
    filter(is.finite(year.revenue), year.revenue > 0) %>%
    group_by(CFEC.Vessel.Owner.Filing.Number, period, Permit.Fishery) %>%
    summarise(period_rev = sum(year.revenue, na.rm = TRUE), .groups = "drop_last") %>%
    slice_max(order_by = period_rev, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(CFEC.Vessel.Owner.Filing.Number, period, DomFish = Permit.Fishery)
  
  # 2) Owner–period panel for regressions (ensure CV>0, build rot_var + logCV)
  panel <- reg.data %>%
    filter(is.finite(CV), CV > 0) %>%
    mutate(
      rot_var = ifelse(is.finite(rot_var),
                       rot_var,
                       ifelse(is.finite(avg_annual_hhi) & is.finite(lr.hhi),
                              avg_annual_hhi - lr.hhi, NA_real_)),
      logCV = safe_log(CV)
    ) %>%
    left_join(dom_fish, by = c("CFEC.Vessel.Owner.Filing.Number","period"))
  
  # 3) Function to run the subset FE model for a given fishery f
  run_model_for_fish <- function(f,
                                 df = panel,
                                 min_n = 50,     # require enough rows
                                 min_owners = 5, # and enough variation for FE
                                 min_periods = 2){
    
    d <- df %>%
      filter(DomFish == f) %>%
      filter(is.finite(logCV), is.finite(lr.hhi), is.finite(rot_var))
    
    # basic sample-size/variation checks
    n_obs    <- nrow(d)
    n_own    <- dplyr::n_distinct(d$CFEC.Vessel.Owner.Filing.Number)
    n_per    <- dplyr::n_distinct(d$period)
    
    if (n_obs < min_n || n_own < min_owners || n_per < min_periods) {
      return(tibble(
        Permit.Fishery = f,
        nobs           = n_obs,
        owners         = n_own,
        periods        = n_per,
        beta_lr        = NA_real_, se_lr = NA_real_, t_lr = NA_real_, p_lr = NA_real_,
        beta_phi       = NA_real_, se_phi = NA_real_, t_phi = NA_real_, p_phi = NA_real_,
        r2_within      = NA_real_, rmse  = NA_real_
      ))
    }
    
    # FE model on the subset (two-way FE; cluster owner+period)
    mod <- tryCatch(
      feols(
        logCV ~ lr.hhi + rot_var | CFEC.Vessel.Owner.Filing.Number + period,
        data = d
      ),
      error = function(e) NULL
    )
    if (is.null(mod)) {
      return(tibble(
        Permit.Fishery = f,
        nobs           = n_obs,
        owners         = n_own,
        periods        = n_per,
        beta_lr        = NA_real_, se_lr = NA_real_, t_lr = NA_real_, p_lr = NA_real_,
        beta_phi       = NA_real_, se_phi = NA_real_, t_phi = NA_real_, p_phi = NA_real_,
        r2_within      = NA_real_, rmse  = NA_real_
      ))
    }
    
    # Clustered SEs (owner + period)
    ct <- tryCatch(
      coeftable(mod, vcov = ~ CFEC.Vessel.Owner.Filing.Number + period),
      error = function(e) coeftable(mod) # fallback to default vcov if needed
    )
    
    pick <- function(name, col) {
      if (!is.null(ct) && name %in% rownames(ct)) as.numeric(ct[name, col]) else NA_real_
    }
    
    r2w  <- tryCatch(as.numeric(fitstat(mod, "r2.within")), error = function(e) NA_real_)
    rmse <- tryCatch(as.numeric(fitstat(mod, "rmse")),       error = function(e) NA_real_)
    
    tibble(
      Permit.Fishery = f,
      nobs           = nobs(mod),
      owners         = n_own,
      periods        = n_per,
      beta_lr        = pick("lr.hhi",  "Estimate"),
      se_lr          = pick("lr.hhi",  "Std. Error"),
      t_lr           = pick("lr.hhi",  "t value"),
      p_lr           = pick("lr.hhi",  "Pr(>|t|)"),
      beta_phi       = pick("rot_var", "Estimate"),
      se_phi         = pick("rot_var", "Std. Error"),
      t_phi          = pick("rot_var", "t value"),
      p_phi          = pick("rot_var", "Pr(>|t|)"),
      r2_within      = r2w,
      rmse           = rmse
    )
  }
  
  # 4) Run for every fishery present as a dominant fishery
  fish_list <- sort(unique(panel$DomFish))
  per_fishery_fe <- purrr::map_dfr(fish_list, run_model_for_fish)
  
  # 5) Inspect / save
  print(per_fishery_fe %>% arrange(desc(nobs)) %>% head(20))
  # write_csv(per_fishery_fe, "tables/per_fishery_FE_results.csv")
  
}




# -------------------------------------------------------------
# (B2) Exact leave-one-group-out (jackknife) delta coefficient
# -------------------------------------------------------------
# This is slower but exact: re-fit the feols after dropping each group,
# and record the change in the coefficient of interest.

jackknife_delta <- function(df, formula, fes, group_var, coef_name, max_groups = 50) {
  stopifnot(group_var %in% names(df))
  
  # Fit baseline
  base_fit <- feols(formula, data = df, fixef = fes)
  base_beta <- coef(base_fit)[[coef_name]]
  
  groups <- unique(df[[group_var]])
  
  # Optionally preselect most "influential-looking" groups to save time
  # Here we use the fast influence score as a preselector if many groups
  if (length(groups) > max_groups) {
    xs <- all.vars(formula[[3]])  # crude parse of RHS; adjust if needed
    xs <- xs[xs != "+"]; xs <- xs[xs != "|"]
    xs <- xs[!xs %in% fes]
    xs <- setdiff(xs, c("", " "))
    
    # fallback: if parse is messy, just keep max_groups randomly
    if (length(xs) >= 1) {
      term <- xs[length(xs)] # last regressor (e.g., "rot_var")
      infl <- tryCatch(
        influence_score(df, y = all.vars(formula[[2]]), xs = xs,
                        fe1 = fes[1], fe2 = fes[2], term = term,
                        group_var = group_var),
        error = function(e) NULL
      )
      if (!is.null(infl)) {
        groups <- infl %>% slice_max(influence, n = max_groups) %>% pull(!!rlang::sym(group_var))
      } else {
        groups <- sample(groups, max_groups)
      }
    } else {
      groups <- sample(groups, max_groups)
    }
  }
  
  pb <- txtProgressBar(min = 0, max = length(groups), style = 3)
  out <- vector("list", length(groups))
  
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    df_drop <- df[df[[group_var]] != g, , drop = FALSE]
    fit_i <- feols(formula, data = df_drop, fixef = fes)
    beta_i <- coef(fit_i)[[coef_name]]
    out[[i]] <- tibble::tibble(!!group_var := g,
                               beta_drop = beta_i,
                               delta = beta_i - base_beta)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  dplyr::bind_rows(out) %>% arrange(desc(abs(delta)))
}

# Example: which owners move the rot_var coefficient most if removed?
jk_owner <- jackknife_delta(
  df        = reg.data %>% filter(is.finite(CV)),
  formula   = log(CV) ~ lr.hhi + rot_var,
  fes       = ~ CFEC.Vessel.Owner.Filing.Number + period,
  group_var = "CFEC.Vessel.Owner.Filing.Number",
  coef_name = "rot_var",
  max_groups = 75   # increase if you can afford more re-fits
)

head(jk_owner, 20)

# Same idea by fishery (using the dominant fishery mapping created above):
reg.data.map2 <- reg.data.map %>% filter(is.finite(CV), !is.na(Permit.Fishery))
jk_fish <- jackknife_delta(
  df        = reg.data.map2,
  formula   = log(CV) ~ lr.hhi + rot_var,
  fes       = ~ CFEC.Vessel.Owner.Filing.Number + period,
  group_var = "Permit.Fishery",
  coef_name = "rot_var",
  max_groups = 50
)

head(jk_fish, 20)



#-----------------------
#effort breakdown
#-----------------------
{
  get.effort.markers <- function(df){
    df <- df %>%
      mutate(
        week.number = as.integer(strftime(Date.Landed, "%V"))
      )
    df.trip.duration <- df %>% 
      mutate(days.in.year = case_when(
        is.na(Date.Landed) ~ NA_integer_,
        leap_year(Date.Landed) ~ 366L,
        TRUE ~ 365L
      )) %>% 
      group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, day.vessel.id) %>% 
      summarise(trip.duration = first(trip.duration), week.number = first(week.number), days.in.year = first(days.in.year)) %>% 
      group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
      mutate(annual.vessel.active.days = n_distinct(day.vessel.id)) %>% 
      group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>%
      mutate(active.days.ratio = sum(trip.duration,na.rm = TRUE)/annual.vessel.active.days, 
             total.days.ratio = sum(trip.duration,na.rm = TRUE)/days.in.year, 
             active.weeks = n_distinct(week.number))
    return(df.trip.duration)
  }
  
  effort.stats <- get.effort.markers(trip.data)
  
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
    left_join(vessel.owner.info %>% 
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
  
  lr.hhi.tbl_2 <- effort.ratios %>% 
    group_by(period, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>% 
    summarise(s_0 = first(s_0)) %>% 
    group_by(period, CFEC.Vessel.Owner.Filing.Number) %>% 
    summarise(lr.hhi = sum(s_0^2))
  
  effort.reg.data_vo <- effort.ratios %>% 
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
  samp <- effort.reg.data_vo %>% filter(!is.na(CV) & CV>0 & CV!=1)
  effort_model_3a <- samp %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_e  + avg.ME_k + avg.ME_int | CFEC.Vessel.Owner.Filing.Number + period)
  effort_model_3b <- samp %>%  
    feols(log(CV) ~ lr.hhi + avg.ME_e | CFEC.Vessel.Owner.Filing.Number + period)
  effort_model_3c <- samp %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_k | CFEC.Vessel.Owner.Filing.Number + period)
  effort_model_3d <- samp %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_int | CFEC.Vessel.Owner.Filing.Number + period)
  
  # Correlations among regressors in the alt spec
  effort.reg.data_vo %>% ungroup() %>% 
    select(lr.hhi, rot_var, avg.ME_e, avg.ME_k, avg.ME_int) %>%
    mutate(across(everything(), as.numeric)) %>%
    cor(use = "pairwise.complete.obs")
  
  summary(effort_model_3a)
  summary(effort_model_3b)
  summary(effort_model_3c)
  summary(effort_model_3d)
  
  df_rs <- reg.data.map %>%
    # keep rows where log(CV) is defined and RHS is finite
    filter(is.finite(CV), CV > 0,
           is.finite(lr.hhi), is.finite(rot_var)) %>%
    # make grouping variables explicit factors in the data
    mutate(
      owner   = factor(CFEC.Vessel.Owner.Filing.Number),
      fishery = factor(Permit.Fishery),
      period_f = factor(period)
    ) %>%
    # drop unused levels created by filtering (helps convergence)
    droplevels()
  
  # recommended: center/scale slopes to help convergence (optional)
  df_rs <- df_rs %>%
    mutate(
      lr.hhi_c  = scale(lr.hhi,  center = TRUE, scale = TRUE)[,1],
      rot_var_c = scale(rot_var, center = TRUE, scale = TRUE)[,1]
    )
  
  # random slopes by fishery, plus owner and period intercepts
  m_rs.vo <- lmer(
    log(CV) ~ lr.hhi + rot_var +
      (lr.hhi + rot_var | fishery) +
      (1 | owner) +
      (1 | period_f),
    data = df_rs,
    control = lmerControl(optimizer = "bobyqa",
                          optCtrl = list(maxfun = 1e5),
                          check.nobs.vs.nlev = "warning",
                          check.nobs.vs.nRE  = "warning")
  )
  
  summary(m_rs.vo)
}
## SAVING REGRESSION RESULTS
{
  # Make sure the directory exists
  dir.create("tables", showWarnings = FALSE)
  
  fixest::etable(
    effort_model_3a, effort_model_3b, effort_model_3c, effort_model_3d,
    tex  = TRUE,
    file = "tables/results_vo_etable.tex",
    vcov = ~ CFEC.Vessel.Owner.Filing.Number + period,       # two-way clustered SEs
    dict = c("lr.hhi"="Long-run HHI ($H^{LR}$)",
             "avg.ME_e"="Effort Effect ($\\overline{H}^E$)",
             "avg.ME_k"="Productivity Effect ($\\overline{H}^K$)",
             "avg.ME_int"="Interaction Effect ($\\overline{\text{INT}}$)"),
    fitstat = c("n","wr2","rmse"),   # n=Observations, wr2=within R2
    title = "Determinants of CV with $\\Phi$ decomposition (Vessel owner-period)"
  ) 
  
  cat("\nSaved LaTeX table -> tables/effort_results_po.tex\n")
  
  
  # Bundle your already-estimated models with readable column titles
  models_vo <- list(
    "Downside (log CVaR25/mean)" = m_cvar_vo,
    "Tail depth (log shortfall25)" = m_short_vo,
    "Volatility (log CV)" = m_CV_vo
  )
  
  # Pretty labels for coefficients
  coef_map <- c(
    "lr.hhi"                 = "Long-run HHI ($H^{LR}$)",
    "rot_var"                = "Share variance ($\\Phi$)"
  )
  
  # Goodness-of-fit rows to display (works with fixest)
  gof_map <- tibble::tribble(
    ~raw,          ~clean,          ~fmt,
    "nobs",        "Observations",   0,
    "r2.within",   "R$^2$ (within)", 3,
    "rmse",        "RMSE",           3
  )
  
  # Build the LaTeX table with two-way clustered SEs (owner + period)
  modelsummary(
    models_vo,
    output   = "tables/results_vo.tex",
    coef_map = coef_map,
    gof_map  = gof_map,
    vcov     = ~ CFEC.Vessel.Owner.Filing.Number + period,   # two-way clustered SEs
    stars    = c('*' = .10, '**' = .05, '***' = .01),
    title    = "Determinants of downside risk and volatility (Permit owner × period)",
    notes    = "All models include permit-holder and period fixed effects. Standard errors are two-way clustered by permit-holder and period."
  )
  
  cat("\nSaved LaTeX table -> tables/results_vo.tex\n")
}


