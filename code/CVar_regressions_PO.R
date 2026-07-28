{
  permit.owner.info <- trip.data %>%
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery) %>%
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
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>%
    mutate(owner.year.revenue = sum(year.revenue, na.rm = TRUE),
           num.fisheries      = n_distinct(Permit.Fishery)) %>%
    ungroup() %>%
    mutate(
      period = floor((Batch.Year - 1991) / period_length) + 1,
      fishery.annual.share = year.revenue / owner.year.revenue
    ) %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(annual.hhi = sum(fishery.annual.share^2, na.rm = TRUE), shannon.id = -sum(fishery.annual.share*log(fishery.annual.share),na.rm = TRUE)) %>% ungroup() %>% 
    filter(!is.na(CFEC.Permit.Holder.Filing.Number), Vessel.Length>0)
  
  
  streamline_m <- function(df){
    list <- c("M6AB","M6AG","M6BB","M6BG","M7FB","M7FG","M7GB","M7GG", "M7HB","M7HG","M7IB","M7IG")
    df %>% mutate(Permit.Fishery=ifelse(Permit.Fishery %in% list, paste0(substr(Permit.Fishery,1,1), "0",substr(Permit.Fishery,2,2),substr(Permit.Fishery,4,4)),Permit.Fishery)) %>% return()
  }
  streamline_c <- function(df){
    list <- c("C5AE","C5BE","C5CE","C5DE","C4AE","C4BE","C4CE","C4DE")
    df %>% mutate(Permit.Fishery=ifelse(Permit.Fishery %in% list, paste0(substr(Permit.Fishery,1,1), "0",substr(Permit.Fishery,2,2),substr(Permit.Fishery,4,4)),Permit.Fishery)) %>% return()
  }
  
  permit.owner.info <- streamline_m(permit.owner.info)
  permit.owner.info <- streamline_c(permit.owner.info)
  
  # active years per owner–period (years with positive owner revenue)
  active_years <- permit.owner.info %>%
    filter(is.finite(owner.year.revenue), owner.year.revenue > 0) %>%
    distinct(CFEC.Permit.Holder.Filing.Number, period, Batch.Year) %>%
    count(CFEC.Permit.Holder.Filing.Number, period, name = "num.active.years")
  
  # ---- Equal-weighted long-run average shares across ACTIVE years ----
  avg_share <- permit.owner.info %>%
    filter(is.finite(owner.year.revenue), owner.year.revenue > 0) %>%
    group_by(CFEC.Permit.Holder.Filing.Number, period, Permit.Fishery) %>%
    summarise(
      sum_share = sum(fishery.annual.share, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(active_years, by = c("CFEC.Permit.Holder.Filing.Number","period")) %>%
    mutate(avg.fishery.share = sum_share / num.active.years)
  
  # Long-run HHI = sum_j (avg share_j)^2
  lr_tbl <- avg_share %>%
    group_by(CFEC.Permit.Holder.Filing.Number, period) %>%
    summarise(lr.hhi = sum(avg.fishery.share^2, na.rm = TRUE), .groups = "drop")
  
  # ---- Avg annual HHI (equal-weighted across ACTIVE years) ----
  Hbar_tbl <- permit.owner.info %>%
    filter(is.finite(owner.year.revenue), owner.year.revenue > 0) %>%
    group_by(CFEC.Permit.Holder.Filing.Number, period, Batch.Year) %>%
    summarise(H_t = sum(fishery.annual.share^2, na.rm = TRUE), .groups = "drop") %>%
    group_by(CFEC.Permit.Holder.Filing.Number, period) %>%
    summarise(avg_annual_hhi = mean(H_t), .groups = "drop")
  
  # ---- CV using ONE row per (owner, year) ----
  owner_year_totals <- permit.owner.info %>%
    filter(is.finite(owner.year.revenue), owner.year.revenue > 0) %>%
    distinct(CFEC.Permit.Holder.Filing.Number, period, Batch.Year, owner.year.revenue,
             num.fisheries, Vessel.Length)  # keep per-year attributes if you need means later
  
  cv_tbl <- owner_year_totals %>%
    group_by(CFEC.Permit.Holder.Filing.Number, period) %>%
    summarise(
      CV = sd(owner.year.revenue, na.rm = TRUE) / mean(owner.year.revenue, na.rm = TRUE),
      avg.fisheries = mean(num.fisheries, na.rm = TRUE),
      max.vessel.length = max(Vessel.Length, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Assemble final owner–period table with size bins ----
  permit.owner.period <- cv_tbl %>%
    left_join(lr_tbl,     by = c("CFEC.Permit.Holder.Filing.Number","period")) %>%
    left_join(Hbar_tbl,   by = c("CFEC.Permit.Holder.Filing.Number","period")) %>%
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
  permit.owner.year <- permit.owner.info %>%
    mutate(species.letter = substr(Permit.Fishery,1,1)) %>% filter(species.letter != "0" & species.letter != "9") %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    summarise(num.fisheries=first(num.fisheries), 
              annual.hhi=first(annual.hhi), 
              shannon.id=first(shannon.id), 
              num.species = n_distinct(species.letter), 
              period = first(period),
              Vessel.Length = max(Vessel.Length, na.rm = TRUE),
              owner.year.revenue = first(owner.year.revenue)) %>% 
    group_by(CFEC.Permit.Holder.Filing.Number) %>%
    arrange(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>%#treating missing years as missing instead of zeros. i.e. if you leave for a year and come back then it's taking the hhi from the last year you fished
    mutate(annual.hhi.t1 = lead(annual.hhi), num.fisheries.t1 = lead(num.fisheries), shannon.id.t1 = lead(shannon.id)) %>%
    ungroup() %>% 
    left_join(permit.owner.period %>% select(period, CFEC.Permit.Holder.Filing.Number, lr.hhi), join_by(period, CFEC.Permit.Holder.Filing.Number), multiple = "any") %>% 
    ungroup() %>% 
    filter(Vessel.Length >= 20) %>% 
    mutate(vessel.size = case_when(
      Vessel.Length < 40 ~ "20-39",
      Vessel.Length < 60 ~ "40-59",
      TRUE ~ "60+"
    ),
    vessel.size = factor(vessel.size, levels = c("20-39", "40-59", "60+")))
  
  reg.data <- permit.owner.period %>% filter(!is.na(CV) & !is.na(CFEC.Permit.Holder.Filing.Number)) 

  
  
  
  
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
  # Uses your existing 'permit.owner.year' (has owner.year.revenue, period, etc.)
  downside_tbl <- permit.owner.year %>%
    filter(is.finite(owner.year.revenue)) %>%
    group_by(CFEC.Permit.Holder.Filing.Number, period) %>%
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
  reg.data.po <- reg.data %>%
    select(-CV) %>%                                # if present; we'll re-attach from downside_tbl
    left_join(downside_tbl %>% select(CFEC.Permit.Holder.Filing.Number, period, CV, cvar25_ratio, shortfall25),
              by = c("CFEC.Permit.Holder.Filing.Number","period"))
  
  
  #models with just average annual HHI and HHI^{LR}
  m_lr_short_po <- reg.data.po %>%
    filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
    feols(log(shortfall25) ~ lr.hhi | CFEC.Permit.Holder.Filing.Number + period)
  m_lr_cvar_po <- reg.data.po %>%
    filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
    feols(log(cvar25_ratio) ~ lr.hhi | CFEC.Permit.Holder.Filing.Number + period)
  m_lr_CV_po <- reg.data.po %>%
    filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
    feols(log(CV) ~ lr.hhi | CFEC.Permit.Holder.Filing.Number + period)
  
  m_aa_short_po <- reg.data.po %>%
    filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
    feols(log(shortfall25) ~ avg_annual_hhi | CFEC.Permit.Holder.Filing.Number + period)
  m_aa_cvar_po <- reg.data.po %>%
    filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
    feols(log(cvar25_ratio) ~ avg_annual_hhi | CFEC.Permit.Holder.Filing.Number + period)
  m_aa_CV_po <- reg.data.po %>%
    filter(is.finite(CV), CV > 0, lr.hhi!=1) %>%
    feols(log(CV) ~ avg_annual_hhi | CFEC.Permit.Holder.Filing.Number + period)
  
  m_aa_short_po
  m_aa_cvar_po
  m_aa_CV_po
  
  m_lr_short_po
  m_lr_cvar_po
  m_lr_CV_po
  
  {
    library(fixest)
    
    # Coefficient labels
    dict <- c(
      "lr.hhi"                   = "Long-run HHI",
      "avg_annual_hhi"           = "Avg. annual HHI"
    )
    
    # Name each model with the desired column title
    models_named <- list(
      "Tail depth (log shortfall25)"    = m_lr_short_po,
      "Downside Risk (log CVaR25/mean)" = m_lr_cvar_po,
      "Volatility (log CV)"             = m_lr_CV_po,
      "Tail depth (log shortfall25)"    = m_aa_short_po,
      "Downside Risk (log CVaR25/mean)"= m_aa_cvar_po,
      "Volatility (log CV)"             = m_aa_CV_po
    )
    
    # Build LaTeX table (no mnames)
    latex_tbl <- do.call(
      etable,
      c(
        models_named,   # spliced as six separate, named model args
        list(
          headers     = list("Long-run HHI" = 3, "Avg. annual HHI" = 3),
          dict        = dict,
          vcov        = ~ CFEC.Permit.Holder.Filing.Number + period,  # two-way clustered SEs
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
  m_cvar_po <- reg.data.po %>%
    mutate(is.period.specialist = ifelse(lr.hhi==1, "YES", "NO")) %>% 
    filter(is.finite(cvar25_ratio), cvar25_ratio > 0) %>%
    feols(log(cvar25_ratio) ~ lr.hhi + rot_var | CFEC.Permit.Holder.Filing.Number + period)
  
  # You can also test shortfall25 directly (already unitless, positive = worse downside)
  m_short_po <- reg.data.po %>%
    mutate(is.period.specialist = ifelse(lr.hhi==1, "YES", "NO")) %>% 
    filter(is.finite(shortfall25), shortfall25 > 0) %>%
    feols(log(shortfall25) ~ lr.hhi + rot_var | CFEC.Permit.Holder.Filing.Number + period)
  
  m_CV_po <- reg.data.po %>%
    mutate(is.period.specialist = ifelse(lr.hhi==1, "YES", "NO")) %>% 
    filter(is.finite(CV), CV > 0 )%>%
    feols(log(CV) ~ lr.hhi + rot_var | CFEC.Permit.Holder.Filing.Number + period)
  
  summary(m_cvar_po)
  summary(m_cvar_vo)
  summary(m_short_po)
  summary(m_short_vo)
  summary(m_CV_po)
  summary(m_CV_vo)
  
  {
    # Make sure the directory exists
    dir.create("tables", showWarnings = FALSE)
    # Bundle your already-estimated models with readable column titles
    models_po <- list(
      "Downside (log CVaR25/mean)" = m_cvar_po,
      "Tail depth (log shortfall25)" = m_short_po,
      "Volatility (log CV)" = m_CV_po
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
      models_po,
      output   = "tables/results_po.tex",
      coef_map = coef_map,
      gof_map  = gof_map,
      vcov     = ~ CFEC.Permit.Holder.Filing.Number + period,   # two-way clustered SEs
      stars    = c('*' = .10, '**' = .05, '***' = .01),
      title    = "Determinants of downside risk and volatility (Permit owner × period)",
      notes    = "All models include permit-holder and period fixed effects. Standard errors are two-way clustered by permit-holder and period."
    )
    
    cat("\nSaved LaTeX table -> tables/results_po.tex\n")
  }
 
  # ============================================================
  # (B) Who/what is driving the CV regressions? Two approaches
  # ============================================================
  
  # --- B1. Fast "influence score" (no re-fit):
  # Residualize Y and regressors by FE (two-way within transform),
  # fit plain OLS w/out FE, and compute group influence ~ |x * resid|
  # ----------------------------------------------------------------
  
  # Two-way within transform by (fe1, fe2):  y~ and X~ (additive FE)
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
  # Use the same reg.data.po already regressed on.
  # make sure the regressors are in the data
  reg.data.po <- reg.data.po %>%
    mutate(
      rot_var = ifelse(!is.finite(rot_var) & is.finite(avg_annual_hhi) & is.finite(lr.hhi),
                       avg_annual_hhi - lr.hhi, rot_var),
      logCV   = log(CV)
    )
  
  drivers_owner_rot <- influence_score(
    df       = reg.data.po %>% filter(is.finite(logCV), is.finite(lr.hhi), is.finite(rot_var)),
    y        = "logCV",                                  # <-- a column name, not an expression
    xs       = c("lr.hhi", "rot_var"),
    fe1      = "CFEC.Permit.Holder.Filing.Number",
    fe2      = "period",
    term     = "rot_var",
    group_var= "CFEC.Permit.Holder.Filing.Number"
  )
  
  # Top 20 owners:
  head(drivers_owner_rot, 20)
  
  # The same for Permit.Fishery: first join a representative fishery per row (e.g., the
  # modal fishery in the period or long-run main fishery). If you already have a mapping
  # reg.data.po -> dominant fishery, use that column instead of the example below.
  dominant_fishery <- avg_share %>%
    group_by(CFEC.Permit.Holder.Filing.Number, period) %>%
    slice_max(order_by = avg.fishery.share, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(CFEC.Permit.Holder.Filing.Number, period, Permit.Fishery)
  
  reg.data.po.map <- reg.data.po %>%
    left_join(dominant_fishery, by = c("CFEC.Permit.Holder.Filing.Number","period"))
  
  drivers_fishery_rot <- influence_score(
    df     = reg.data.po.map %>% filter(is.finite(CV)),
    y      = "logCV",
    xs     = c("lr.hhi", "rot_var"),
    fe1    = "CFEC.Permit.Holder.Filing.Number",
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
  
  p_owner <- plot_top_drivers(drivers_owner_rot, "CFEC.Permit.Holder.Filing.Number",
                              title = "Top owners driving the rot_var coefficient (permit owners)")
  p_fish  <- plot_top_drivers(drivers_fishery_rot, "Permit.Fishery",
                              title = "Top fisheries driving the rot_var coefficient (permit owners)")
  
  p_owner; p_fish
}





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
      group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery, day.vessel.id) %>% 
      summarise(trip.duration = first(trip.duration), week.number = first(week.number), days.in.year = first(days.in.year)) %>% 
      group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
      mutate(annual.vessel.active.days = n_distinct(day.vessel.id)) %>% 
      group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery) %>%
      mutate(active.days.ratio = sum(trip.duration,na.rm = TRUE)/annual.vessel.active.days, 
             total.days.ratio = sum(trip.duration,na.rm = TRUE)/days.in.year, 
             active.weeks = n_distinct(week.number))
    return(df.trip.duration)
  }
  
  effort.stats <- get.effort.markers(trip.data)
  
  effort.ratios <- effort.stats %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery) %>%
    summarise(num.weeks = n_distinct(week.number), 
              num.days = sum(trip.duration, na.rm = TRUE), 
              active.days.ratio = first(active.days.ratio),
              total.days.ratio = first(total.days.ratio)) %>% 
    ungroup() %>%
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% 
    group_by(period, CFEC.Permit.Holder.Filing.Number, Permit.Fishery) %>%
    mutate(avg.days = mean(num.days, na.rm = TRUE), avg.weeks = mean(num.weeks, na.rm = TRUE), avg.active.ratio = mean(active.days.ratio, na.rm = TRUE), avg.tot.ratio = mean(total.days.ratio, na.rm = TRUE)) %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(num.active.weeks = sum(num.weeks, na.rm = TRUE)) %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery) %>%
    mutate(A_t = num.weeks/num.active.weeks) %>% #proportion of active weeks dedicated to fishery 
    left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
      permit.owner.info %>% 
        select(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery, year.revenue),
      by = join_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery)
    ) %>% #now calculate k_t (CPUE)
    ungroup() %>% 
    mutate(R_t = year.revenue/num.weeks) %>% #average weekly revenue per week from the the specific fishery
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(s_t = A_t*R_t/sum(A_t*R_t, na.rm = TRUE)) %>% 
    group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(num.active.years = n_distinct(Batch.Year)) %>% 
    group_by(period, CFEC.Permit.Holder.Filing.Number, Permit.Fishery) %>% 
    mutate(A_0 = mean(A_t, na.rm = TRUE), R_0 = mean(R_t, na.rm = TRUE)) %>% #average fishery-owner level number of fishing weeks (A_0) and the revenue per week (R_0) averaged across years in which the fishers are active in some fishery across a period
    left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
      permit.owner.info %>% select(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery, fishery.annual.share),
      by = join_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Permit.Fishery)
    ) %>% #check that the s_t is the same as a calculated share fisheries shares from earlier
    left_join(#need annual owner-fishery-revenue R_jt so R_jt / A_t = k_jt (CPUE)
      avg_share %>% select(period, CFEC.Permit.Holder.Filing.Number, Permit.Fishery, avg.fishery.share),
      by = join_by(period, CFEC.Permit.Holder.Filing.Number, Permit.Fishery)
    ) #will use check that the s_0 is the same as a calculated share fisheries shares from earlier
  
  rev0_tbl <- effort.ratios %>% ungroup() %>% distinct(period, CFEC.Permit.Holder.Filing.Number, Permit.Fishery, A_0, R_0) %>%
    group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
    summarise(sum_rev_0 = sum(A_0*R_0))
  
  effort.ratios <- effort.ratios %>% 
    left_join(rev0_tbl, by = join_by(period, CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
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
  effort.ratios %>% ungroup() %>% distinct(period, CFEC.Permit.Holder.Filing.Number, Permit.Fishery, s_0, avg.fishery.share) %>%
    group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
    summarise(s_0 = sum(s_0), avg.fishery.share = sum(avg.fishery.share)) %>% View()
  
  lr.hhi.tbl_2 <- effort.ratios %>% 
    group_by(period, CFEC.Permit.Holder.Filing.Number, Permit.Fishery) %>% 
    summarise(s_0 = first(s_0)) %>% 
    group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
    summarise(lr.hhi = sum(s_0^2))
  
  effort.reg.data_po <- effort.ratios %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    summarise(delta_H = first(delta_H), delta_H_e = first(delta_H_e), delta_H_k = first(delta_H_k), delta_H_int = first(delta_H_int), period = first(period),
              ME_e = first(ME_e),
              ME_k = first(ME_k),
              ME_int = first(ME_int)) %>% 
    group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
    summarise(phi_H = mean(delta_H), phi_H_e = mean(delta_H_e), phi_H_k = mean(delta_H_k), phi_H_int = mean(delta_H_int), avg.ME_e = mean(ME_e), avg.ME_k = mean(ME_k), avg.ME_int = mean(ME_int)) %>% 
    left_join(lr.hhi.tbl_2, join_by(period, CFEC.Permit.Holder.Filing.Number)) %>% 
    left_join(cv_tbl, join_by(period, CFEC.Permit.Holder.Filing.Number)) %>% 
    left_join(Hbar_tbl, join_by(period, CFEC.Permit.Holder.Filing.Number)) %>% 
    mutate(rot_var = avg_annual_hhi - lr.hhi) %>% 
    filter(!is.na(CV) & !is.na(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(is.period.specialist = ifelse(lr.hhi==1, "YES", "NO"))
  #R_0 leave one out
  #R_t for fisheries that not participated in with others mean/median
  samp <- effort.reg.data_po %>% filter(!is.na(CV) & CV>0 & CV!=1)
  effort_model_4a <- samp %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_e  + avg.ME_k + avg.ME_int | CFEC.Permit.Holder.Filing.Number + period)
  effort_model_4b <- samp %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_e | CFEC.Permit.Holder.Filing.Number + period)
  effort_model_4c <- samp %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_k | CFEC.Permit.Holder.Filing.Number + period)
  effort_model_4d <- samp %>% 
    feols(log(CV) ~ lr.hhi + avg.ME_int | CFEC.Permit.Holder.Filing.Number + period)
  
  summary(effort_model_4a)
  summary(effort_model_4b)
  summary(effort_model_4c)
  summary(effort_model_4d)
}

{
  fixest::etable(
    effort_model_4a, effort_model_4b, effort_model_4c, effort_model_4d,
    tex  = TRUE,
    file = "tables/results_po_etable.tex",
    vcov = ~ CFEC.Permit.Holder.Filing.Number + period,       # two-way clustered SEs
    dict = c("lr.hhi"="Long-run HHI ($H^{LR}$)",
             "avg.ME_e"="Effort Effect ($\\overline{H}^E$)",
             "avg.ME_k"="Productivity Effect ($\\overline{H}^K$)",
             "avg.ME_int"="Interaction Effect ($\\overline{\text{INT}}$)",
             "is.period.specialistYES"="Period specialist (=1)"),
    fitstat = c("n","wr2","rmse"),   # n=Observations, wr2=within R2
    title = "Determinants of CV with $\\Phi$ decomposition (Permit holder-period)"
  ) 
  
  cat("\nSaved LaTeX table -> tables/effort_results_po.tex\n")
}