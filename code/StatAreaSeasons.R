### Setup code ###

# List of required packages
packages <- c(
  'readr', 'tidyverse', 'dplyr', 'ggplot2', 'xlsx', 'data.table',
  'fixest', 'lubridate', 'corrplot', 'RColorBrewer', 'knitr',
  'cluster', 'factoextra', 'zoo', 'purrr', 'ggtext', 'gganimate'
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
}


permit.owner.info <- trip.data %>%
  group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>% 
  summarise(num.stat.areas = n_distinct(Statistical.Area),
            year.revenue=sum(CFEC.Value.Detail, na.rm = TRUE), 
            num.trips = n_distinct(day.vessel.id), 
            fishing.days = sum(trip.duration, na.rm = TRUE), 
            unique.checks = n_distinct(CFEC.Permit.Check), #these should all be =1. include to check for any errors
            CFEC.Permit.Check = first(CFEC.Permit.Check), 
            max.seq.num = max(CFEC.Permit.Sequence), 
            fisher.change.statarea = !any(fished.prev.year),
            CFEC.Vessel.Owner.Filing.Number = ifelse(all(is.na(CFEC.Vessel.Owner.Filing.Number)), NA, first(na.omit(CFEC.Vessel.Owner.Filing.Number))), 
            CFEC.Permit.Fishery = ifelse(all(is.na(CFEC.Permit.Fishery)), NA, first(na.omit(CFEC.Permit.Fishery))),
            revenue.per.trip = year.revenue/(num.trips)) %>% #if some of the entries for the permit fishery code are missing but are still listed to the same Batch.Year, CFEC.Permit.Holder.Filing.Number, Vessel.ADFG.Number, and CFEC.Permit.Serial.Number then replace the missing values with the first observed permit fishery code in the group
  group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
  mutate(num.fisheries = n_distinct(CFEC.Permit.Fishery), owner.year.revenue = sum(year.revenue, na.rm = TRUE))


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

stat_area_plot("B 06B")
stat_area_plot("S 03T")
stat_area_plot("S 03A")
stat_area_plot("S 03E")
stat_area_plot("S 03H")
stat_area_plot("S 03M")

change_stat_area_plot("B 06B")


#rotating specialists and what is their behavior in terms of statistical areas 
#permit.owner.info is at the Year-Fishery-Owner level. Having each fishery is important for the statistical area because the number of statistical areas fished can mean different things for different fisheries. 

#add stat area change info based on the previous years activity. within the year/permit owner/fishery group, want to look for the presence of the stat area in the previous year 

#in trip data, only one stat area is listed per row (I'm pretty sure but not 100% certain)
#for every CFEC.Permit.Holder.Filing.Number/CFEC.Permit.Fishery combination I want to look at the data from the previous batch.year (but same CFEC.Permit.Holder.Filing.Number/CFEC.Permit.Fishery) and check if the value of a variable called "Statistical.Area" is present. If so, this row gets a value of TRUE for a new variable called 'fished.prev.year'

# Step 1: Create the lookup table with previous year's areas, but DO NOT rename Statistical.Area yet
prev_areas <- trip.data %>%
  mutate(Batch.Year = Batch.Year + 1) %>%
  distinct(CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery, Batch.Year, Statistical.Area) %>%
  mutate(matched = TRUE)  # Add explicit flag to detect matches

# Step 2: Left join using same column names
trip.data <- trip.data %>%
  left_join(prev_areas,
            by = c("CFEC.Permit.Holder.Filing.Number",
                   "CFEC.Permit.Fishery",
                   "Batch.Year",
                   "Statistical.Area")) %>%
  mutate(fished.prev.year = if_else(is.na(matched), FALSE, matched)) %>%
  select(-matched)

#now in any given year-owner-fishery, did the stat area change
{#code to find rotating specialists
  #
}




#Notes/Questions:
#do statewide permits matter vs location restricted
#enforcement having impacts



stat_area_plot <- function(fishery){
  Fishery <- fishery
  # Create the revenue quartile variable and prepare data
  plot_data <- permit.owner.info %>% 
    filter(CFEC.Permit.Fishery==Fishery) %>% 
    group_by(Batch.Year) %>% 
    mutate(
      revenue_quartile = ntile(owner.year.revenue, 4), #these are yearly quartiles
      revenue_quartile = factor(revenue_quartile, 
                                levels = 1:4,
                                labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))
    ) %>%
    ungroup() %>%
    # Convert num.stat.areas to integer
    mutate(num.stat.areas = as.integer(num.stat.areas))
  
  # Create the grid of histograms
  grid_plot <- plot_data %>%
    ggplot(aes(x = num.stat.areas, fill = revenue_quartile)) +
    geom_bar(alpha = 0.8, position = "stack") +
    facet_wrap(~ Batch.Year, scales = "fixed") +  # Changed from "free_y" to "fixed"
    labs(
      title = "Number of Statistical Areas Fished by Year",
      subtitle = paste0("CFEC Permit Fishery: ", Fishery),
      x = "Number of Statistical Areas",
      y = "Count",
      fill = "Owner Revenue Quartile"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    scale_fill_viridis_d(option = "plasma", alpha = 0.8) +
    xlim(0,12)#scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 2))  # Integer breaks on x-axis
  return(grid_plot)
}

change_stat_area_plot <- function(fishery){
  Fishery <- fishery
  # Create the revenue quartile variable and prepare data
  plot_data <- permit.owner.info %>% 
    filter(CFEC.Permit.Fishery==Fishery) %>% 
    group_by(Batch.Year) %>% 
    mutate(
      revenue_quartile = ntile(owner.year.revenue, 4), #these are yearly quartiles
      revenue_quartile = factor(revenue_quartile, 
                                levels = 1:4,
                                labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))
    ) %>%
    ungroup() %>%
    # Convert num.stat.areas to integer
    mutate(num.stat.areas = as.integer(num.stat.areas))
    
  
  # Create the grid of histograms
  grid_plot <- plot_data %>%
    ggplot(aes(x = fisher.change.statarea, fill = revenue_quartile)) +
    geom_bar(alpha = 0.8, position = "stack") +
    facet_wrap(~ Batch.Year, scales = "fixed") +  # Changed from "free_y" to "fixed"
    labs(
      title = "Counts of Permit Owners who Changed a Statistical Area by Year",
      subtitle = paste0("CFEC Permit Fishery: ", Fishery),
      x = "Number of Statistical Areas",
      y = "Count",
      fill = "Owner Revenue Quartile"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    scale_fill_viridis_d(option = "plasma", alpha = 0.8)
    #scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 2))  # Integer breaks on x-axis
  return(grid_plot)
}
