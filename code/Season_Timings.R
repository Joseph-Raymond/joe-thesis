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
{
  # Read the combined data
  combined_data <- readRDS(file.path("intermediate data", "combined_data_20250625_131406.rds"))
  trip.data <- get.trip.new(combined_data)
  trip.data$trip.duration <- as.numeric(trip.data$trip.length)+1
  trip.data <- trip.data %>% mutate(trip.duration=ifelse(trip.duration>=0,trip.duration, NA))
  rm(combined_data)
}

# --- (Optional) If your columns have spaces instead of dots, uncomment and run this once:
# trip.data <- trip.data %>%
#   rename(
#     Permit.Fishery        = `Permit Fishery`,
#     Statistical.Area      = `Statistical Area`,
#     Batch.Year            = `Batch Year`,
#     Date.Fishing.Began    = `Date Fishing Began`,
#     CFEC.Value.Detail     = `CFEC Value (Detail)`
#   )

# Main helper ---------------------------------------------------------------

compute_fishery_statarea_season_and_quantile_dates <- function(trip.data,
                                                      n_quantiles = 10,
                                                      min_positive_revenue = 0) {
  stopifnot(n_quantiles >= 1)
  
  # Basic cleanup / coercions
  trip.data_clean <- trip.data %>%
    mutate(
      Date.Fishing.Began = as.Date(Date.Fishing.Began),
      CFEC.Value.Detail  = suppressWarnings(as.numeric(CFEC.Value.Detail))
    )
  
  # Keep only rows that can contribute to season timing & revenue
  trip.data_pos <- trip.data_clean %>%
    filter(
      !is.na(Permit.Fishery),
      !is.na(Statistical.Area),
      !is.na(Batch.Year),
      !is.na(Date.Fishing.Began),
      !is.na(CFEC.Value.Detail),
      CFEC.Value.Detail > min_positive_revenue
    )
  
  # 1) Season start/end & totals (per fishery-year)
  season_bounds <- trip.data_pos %>%
    group_by(Permit.Fishery, Statistical.Area, Batch.Year) %>%
    summarise(
      start_date        = min(Date.Fishing.Began),
      end_date          = max(Date.Fishing.Began),
      season_days       = as.integer(end_date - start_date) + 1L,
      total_revenue     = sum(CFEC.Value.Detail, na.rm = TRUE),
      trip.data_n           = dplyr::n(),
      active_dates_n    = n_distinct(Date.Fishing.Began),
      .groups = "drop"
    )
  
  # 2) Date-level revenue (aggregate multiple trip.data on the same day)
  rev_by_date <- trip.data_pos %>%
    group_by(Permit.Fishery, Statistical.Area, Batch.Year, Date.Fishing.Began) %>%
    summarise(revenue = sum(CFEC.Value.Detail, na.rm = TRUE), .groups = "drop")
  
  # Vector of quantile probabilities: 1/n, 2/n, ..., n/n
  probs <- (1:n_quantiles) / n_quantiles
  
  # Helper to get first date at which cumulative revenue reaches each prob
  get_q_dates <- function(df, probs) {
    if (nrow(df) == 0) {
      return(tibble(prob = probs, q_date = as.Date(NA)))
    }
    df2 <- df %>%
      arrange(Date.Fishing.Began) %>%
      mutate(
        total = sum(revenue),
        cum_frac = cumsum(revenue) / total
      )
    
    q_dates <- map(probs, function(p) {
      idx <- which(df2$cum_frac >= p)[1]
      if (is.na(idx)) max(df2$Date.Fishing.Began) else df2$Date.Fishing.Began[idx]
    }) %>% unlist()
    
    tibble(prob = probs, q_date = as.Date(q_dates, origin = "1970-01-01"))
  }
  
  # 3) Long-format quantile dates per fishery-year
  qdates_long <- rev_by_date %>%
    group_by(Permit.Fishery, Statistical.Area, Batch.Year) %>%
    group_modify(~ get_q_dates(.x, probs)) %>%
    ungroup() %>%
    mutate(
      quantile_index = as.integer(round(prob * n_quantiles)),
      q_label = paste0("q", sprintf("%02d", quantile_index))  # e.g., q10, q20, ...
    )
  
  # 4) Wide-format quantile dates (one column per quantile)
  qdates_wide <- qdates_long %>%
    select(Permit.Fishery, Statistical.Area, Batch.Year, q_label, q_date) %>%
    distinct() %>%
    tidyr::pivot_wider(names_from = q_label, values_from = q_date)
  
  # 5) Join season bounds + quantile dates
  season_with_quantiles <- season_bounds %>%
    left_join(qdates_wide, by = c("Permit.Fishery", "Statistical.Area", "Batch.Year"))
  
  list(
    season_bounds         = season_bounds,          # start/end/duration/total revenue
    quantile_dates_long   = qdates_long,            # long format (prob + date)
    quantile_dates_wide   = qdates_wide,            # wide date columns q10, q20, ...
    season_quantiles_wide = season_with_quantiles   # joined final table
  )
}

compute_fishery_season_and_quantile_dates <- function(trip.data,
                                                      n_quantiles = 10,
                                                      min_positive_revenue = 0) {
  stopifnot(n_quantiles >= 1)
  
  # Basic cleanup / coercions
  trip.data_clean <- trip.data %>%
    mutate(
      Date.Fishing.Began = as.Date(Date.Fishing.Began),
      CFEC.Value.Detail  = suppressWarnings(as.numeric(CFEC.Value.Detail))
    )
  
  # Keep only rows that can contribute to season timing & revenue
  trip.data_pos <- trip.data_clean %>%
    filter(
      !is.na(Permit.Fishery),
      !is.na(Batch.Year),
      !is.na(Date.Fishing.Began),
      !is.na(CFEC.Value.Detail),
      CFEC.Value.Detail > min_positive_revenue
    )
  
  # 1) Season start/end & totals (per fishery-year)
  season_bounds <- trip.data_pos %>%
    group_by(Permit.Fishery, Batch.Year) %>%
    summarise(
      start_date        = min(Date.Fishing.Began),
      end_date          = max(Date.Fishing.Began),
      season_days       = as.integer(end_date - start_date) + 1L,
      total_revenue     = sum(CFEC.Value.Detail, na.rm = TRUE),
      trip.data_n           = dplyr::n(),
      active_dates_n    = n_distinct(Date.Fishing.Began),
      .groups = "drop"
    )
  
  # 2) Date-level revenue (aggregate multiple trip.data on the same day)
  rev_by_date <- trip.data_pos %>%
    group_by(Permit.Fishery, Batch.Year, Date.Fishing.Began) %>%
    summarise(revenue = sum(CFEC.Value.Detail, na.rm = TRUE), .groups = "drop")
  
  # Vector of quantile probabilities: 1/n, 2/n, ..., n/n
  probs <- (1:n_quantiles) / n_quantiles
  
  # Helper to get first date at which cumulative revenue reaches each prob
  get_q_dates <- function(df, probs) {
    if (nrow(df) == 0) {
      return(tibble(prob = probs, q_date = as.Date(NA)))
    }
    df2 <- df %>%
      arrange(Date.Fishing.Began) %>%
      mutate(
        total = sum(revenue),
        cum_frac = cumsum(revenue) / total
      )
    
    q_dates <- map(probs, function(p) {
      idx <- which(df2$cum_frac >= p)[1]
      if (is.na(idx)) max(df2$Date.Fishing.Began) else df2$Date.Fishing.Began[idx]
    }) %>% unlist()
    
    tibble(prob = probs, q_date = as.Date(q_dates, origin = "1970-01-01"))
  }
  
  # 3) Long-format quantile dates per fishery-year
  qdates_long <- rev_by_date %>%
    group_by(Permit.Fishery, Batch.Year) %>%
    group_modify(~ get_q_dates(.x, probs)) %>%
    ungroup() %>%
    mutate(
      quantile_index = as.integer(round(prob * n_quantiles)),
      q_label = paste0("q", sprintf("%02d", quantile_index))  # e.g., q10, q20, ...
    )
  
  # 4) Wide-format quantile dates (one column per quantile)
  qdates_wide <- qdates_long %>%
    select(Permit.Fishery, Batch.Year, q_label, q_date) %>%
    distinct() %>%
    tidyr::pivot_wider(names_from = q_label, values_from = q_date)
  
  # 5) Join season bounds + quantile dates
  season_with_quantiles <- season_bounds %>%
    left_join(qdates_wide, by = c("Permit.Fishery", "Batch.Year"))
  
  list(
    season_bounds         = season_bounds,          # start/end/duration/total revenue
    quantile_dates_long   = qdates_long,            # long format (prob + date)
    quantile_dates_wide   = qdates_wide,            # wide date columns q10, q20, ...
    season_quantiles_wide = season_with_quantiles   # joined final table
  )
}
# --- Example usage ---------------------------------------------------------

result.statarea <- compute_fishery_statarea_season_and_quantile_dates(trip.data, n_quantiles = 10)
result <- compute_fishery_season_and_quantile_dates(trip.data, n_quantiles = 10)
# result$season_quantiles_wide  # start/end + q10, q20, ..., q100
# result$quantile_dates_long    # tidy long form (one row per quantile per fishery-year)

# If you prefer quartiles instead of deciles, use n_quantiles = 4
# result_q <- compute_fishery_season_and_quantile_dates(trip.data, n_quantiles = 4)
