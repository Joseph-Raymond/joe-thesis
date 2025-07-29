
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', 'xtable', 'corrplot', 'data.table', 'fixest')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work

{packs <- c('readr', 'tidyverse', 'dplyr', 'ggplot2', 'xlsx','data.table', 'fixest', 'lubridate', 'corrplot', 'RColorBrewer')
  new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work


permits <- read_csv("~/JoeData/permits/scraped_permits/FullCFECPermitData.csv")
permits <- permits %>% distinct()
save(permits, file = "intermediate data/permits.rdata")

rm(permits)

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
source("code/myfunctions.R")
load("intermediate data/permits.rdata")
#extract the permit number
permit_clean <- permits %>% mutate(CFEC.Permit.Serial.Number = as.numeric(substr(PermitNumber,1,5)), ADFG = as.integer(ADFG)) %>% filter(Year >= 1991)
rm(permits)
gc()

{
  load("intermediate data/catch_data_temp.rdata")
  #catch_data_temp %>% ungroup %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% nrow()#this filter would filter to jsut the boats that have fished salmon permit at some point
  catch_data_temp$Vessel.ADFG.Number[which(catch_data_temp$Vessel.ADFG.Number==62.39)] <- 62339#fix a typo in the data
  catch_data_temp <- catch_data_temp %>% filter(Vessel.ADFG.Number!=0) #8 missing values with vessel numbers == 0
  catch_data_temp <- catch_data_temp %>% mutate(Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number))
  #catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp[""])] <- 0#fill the NA's in the value of catch with 0's
  catch_data_temp <- catch_data_temp %>% mutate(CFEC.Value..Detail. = if_else(is.na(CFEC.Value..Detail.),0,CFEC.Value..Detail.))
  #filter out boats that made landings but did not report the permit/fishery in which they made the landing
  #Fishery = str_replace_all(string=CFEC.Permit.Fishery, pattern=" ", repl="")
  catch_data_temp <- catch_data_temp %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% filter(!any(is.na(Permit.Serial.Number)) ) %>% ungroup()
  #CFEC.Permit.Serial.Number. Different serial number ranges are used depending upon the permit type (CFEC_PMT_TYPE). Mariculture (acquatic farm) is 10000-10499, experimental is 10500-10599, test fishing is 10600-10699, educational is 10700-10799, reservation is 10800-10899, hatchery cost recovery is 10900-10999, interim-use in an unlimited fishery is 11000-49999, interimentry in a limited fishery is 50000-54999, moratorium is 50000-54999, vessel moratorium is 50000-54999, permanent in a limited fishery is 55000-99999, vessel permanent in a vessel limited fishery is 55000-99999. This field will be blank if the permit information on the fish ticket could not be matched to the CFEC permit file. The variable "Permit.Serial.Number" is from the fish ticket data but there are some observations that can't be matched to the permit data 
}


{
  trip.data <- get.trip(catch_data_temp)
  trip.data$trip.duration <- as.numeric(trip.data$trip.length)+1
  trip.data <- trip.data %>% mutate(trip.duration=ifelse(trip.duration>0,trip.duration, NA))#going out and back the same day should lead to trip duration = 1
}
{
  permit.annual.rev <- trip.data %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Vessel.ADFG.Number, CFEC.Permit.Serial.Number) %>% 
    summarise(year.revenue=sum(CFEC.Value..Detail., na.rm = TRUE), 
              num.trips = n_distinct(trip.id), 
              fishing.days = sum(trip.duration, na.rm = TRUE), 
              unique.checks = n_distinct(CFEC.Permit.Check), 
              CFEC.Permit.Check = first(CFEC.Permit.Check), 
              max.seq.num = max(CFEC.Permit.Sequence), 
              CFEC.Vessel.Owner.Filing.Number = ifelse(all(is.na(CFEC.Vessel.Owner.Filing.Number)), NA, first(na.omit(CFEC.Vessel.Owner.Filing.Number))), 
              CFEC.Permit.Fishery = ifelse(all(is.na(CFEC.Permit.Fishery)), NA, first(na.omit(CFEC.Permit.Fishery)))) %>% 
    mutate(revenue.per.trip = year.revenue/(num.trips))
  
  rm(catch_data_temp)
  gc()
}  
  period_length <- 10
  permit.annual.rev %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>%
    group_by(Batch.Year,CFEC.Permit.Fishery) %>% 
    summarise(year.revenue = sum(year.revenue, na.rm = TRUE)) %>% 
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% 
    group_by(period,CFEC.Permit.Fishery) %>% 
    summarise(fishery.cv = var(year.revenue, na.rm = TRUE)/mean(year.revenue, na.rm = TRUE), fishery.mean = mean(year.revenue, na.rm = TRUE))
  
 
  #owner level CV across other owners in the same fishery for total revenue
  permit.annual.rev %>%
    group_by(Batch.Year,CFEC.Permit.Holder.Filing.Number,CFEC.Permit.Fishery) %>% 
    summarise(year.revenue = sum(year.revenue, na.rm = TRUE)) %>% #yearly revenue of fishery to the owner
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% #6-year time period
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(tot.owner.revenue = sum(year.revenue, na.rm = TRUE)) %>% #all the money each permit owner made in a year
    group_by(Batch.Year,CFEC.Permit.Fishery) %>% 
    summarise(within.fishery.cv = var(tot.owner.revenue, na.rm = TRUE)/mean(tot.owner.revenue, na.rm = TRUE), within.fishery.mean = mean(tot.owner.revenue, na.rm = TRUE)) %>% View()#owner-level
  
  #owner level CV across other owners in the same fishery for revenue from that fishery
  permit.annual.rev %>%
    group_by(Batch.Year,CFEC.Permit.Holder.Filing.Number,CFEC.Permit.Fishery) %>% 
    summarise(year.revenue = sum(year.revenue, na.rm = TRUE)) %>% #yearly revenue OF EACH FISHERY to the owner
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% #6-year time period
    group_by(Batch.Year,CFEC.Permit.Fishery) %>% 
    summarise(within.fishery.cv = var(tot.owner.revenue, na.rm = TRUE)/mean(tot.owner.revenue, na.rm = TRUE), within.fishery.mean = mean(tot.owner.revenue, na.rm = TRUE)) %>% View()#fishery-level
  
  #owner level across time
  permit.annual.rev %>%
    group_by(Batch.Year,CFEC.Permit.Holder.Filing.Number,CFEC.Permit.Fishery) %>% 
    summarise(year.revenue = sum(year.revenue, na.rm = TRUE)) %>% #yearly revenue of fishery to the owner
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% #6-year time period 
    group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(owner.cv = var(year.revenue, na.rm = TRUE)/mean(year.revenue, na.rm = TRUE), 
           owner.mean = mean(year.revenue, na.rm = TRUE)) %>% #all the money each permit owner made in a year
    ggplot(aes(x = owner.cv)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(x = "Owner CV", y = "Density", title = "Density plot of Owner CV")
{#fishery specifc revenue CV    
    df <- permit.annual.rev %>%
      group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>%
      summarise(year.revenue = sum(year.revenue, na.rm = TRUE)) %>%
      mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>%
      filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
      group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
      mutate(active_years = n_distinct(Batch.Year)) %>% 
      group_by(period, CFEC.Permit.Fishery) %>% 
      mutate(active_fishers = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
      filter(active_fishers>20) %>% 
      filter(substr(CFEC.Permit.Fishery,1,3)!="S 7") %>%
      group_by(period, CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>%
      summarise(owner.cv = sd(year.revenue, na.rm = TRUE)/mean(year.revenue, na.rm = TRUE), owner.mean = mean(year.revenue, na.rm = TRUE), nobs = n()) %>% 
      filter(nobs!=1)

    # Calculate the median for each group
    medians <- df %>% 
      group_by(CFEC.Permit.Fishery, period) %>% 
      summarise(median = median(owner.cv, na.rm = TRUE))
    
    # Plot
    plotlist <- list()
    for (i in 1:3) {
      plotlist[[i]] <- df %>% 
        filter(period==i) %>% 
        ggplot(aes(x = owner.cv)) +
        geom_density(fill = "blue", alpha = 0.5) +
        facet_wrap(~ CFEC.Permit.Fishery) +
        labs(x = "Owner CV", y = "Density", title = paste0("Density plot of Owner CV: Period==",i)) +
        xlim(c(0, 2.3)) +
        geom_vline(data = medians %>% filter(period==i), aes(xintercept = median), color = "red", linetype = "dashed")
    }
    print(plotlist[[1]])
    print(plotlist[[2]])
    print(plotlist[[3]])
}
{#total revenue CV
  df_tot <- permit.annual.rev %>%
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>%
    summarise(year.revenue = sum(year.revenue, na.rm = TRUE)) %>%
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>%
    filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
    filter(substr(CFEC.Permit.Fishery,1,3)!="S 7") %>%
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(owner.tot.rev = sum(year.revenue, na.rm = TRUE)) %>% #owner total annual revenue
    group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(active_years = n_distinct(Batch.Year)) %>% 
    group_by(period, CFEC.Permit.Fishery) %>% 
    mutate(active_fishers = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    filter(active_fishers>20) %>% 
    group_by(period, CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>%
    summarise(owner.cv = sd(owner.tot.rev, na.rm = TRUE)/mean(owner.tot.rev, na.rm = TRUE), owner.mean = mean(owner.tot.rev, na.rm = TRUE), nobs = n()) %>% 
    filter(nobs!=1)
  
  # Calculate the median for each group
  medians_tot <- df_tot %>% 
    group_by(CFEC.Permit.Fishery, period) %>% 
    summarise(median = median(owner.cv, na.rm = TRUE))
  
  
  # Plot
  plotlist_tot <- list()
  for (i in 1:3) {
    plotlist_tot[[i]] <- df_tot %>% 
      filter(period==i) %>% 
      ggplot(aes(x = owner.cv)) +
      geom_density(data = df %>% filter(period==i), aes(fill = "Density - Fishery Specific Revenue"), alpha = 0.35) +
      geom_density(aes(fill = "Density - Total Owner Revenue"), alpha = 0.35) +
      facet_wrap(~ CFEC.Permit.Fishery) +
      labs(x = "Owner CV", y = "Density", title = paste0("Density plot of Owner CV: Period==",i)) +
      xlim(c(0, 2.3)) +
      geom_vline(data = medians %>% filter(period==i), aes(xintercept = median, color = "Median - Fishery Specific Revenue"), linetype = "dashed") +
      geom_vline(data = medians_tot %>% filter(period==i), aes(xintercept = median, color = "Median - Total Owner Revenue"), linetype = "dashed") +
      scale_fill_manual(name = "", values = c("Density - Fishery Specific Revenue" = "red", "Density - Total Owner Revenue" = "blue2")) +
      scale_color_manual(name = "", values = c("Median - Fishery Specific Revenue" = "red", "Median - Total Owner Revenue" = "blue2"))
  }#plot total revenue and fishery specifc revnue CV
  print(plotlist_tot[[1]])
  print(plotlist_tot[[2]])
  print(plotlist_tot[[3]])
}#total revenue CV
  #permit level
  permit.annual.rev %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>%
    group_by(Batch.Year,CFEC.Permit.Holder.Filing.Number,CFEC.Permit.Serial.Number) %>% 
    summarise(year.revenue = sum(year.revenue, na.rm = TRUE),
              CFEC.Permit.Fishery=ifelse(all(is.na(CFEC.Permit.Fishery)), NA, first(na.omit(CFEC.Permit.Fishery)))) %>% #yearly revenue of each permit to the owner
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% #6-year time period
    group_by(Batch.Year,CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(year.revenue = sum(year.revenue, na.rm = TRUE)) %>% 
    group_by(period,CFEC.Permit.Holder.Filing.Number) %>% 
    mutate(owner.cv = var(year.revenue, na.rm = TRUE)/mean(year.revenue, na.rm = TRUE), owner.mean = mean(year.revenue, na.rm = TRUE)) %>% 
    View()#owner-level
  
{
  fishery.rev <- permit.annual.rev %>% ungroup() %>% 
    filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>%
    filter(substr(CFEC.Permit.Fishery,1,3)!="S 7") %>%
    group_by(Batch.Year, CFEC.Permit.Fishery) %>%
    summarise(fishery.annual.rev = sum(year.revenue, na.rm = TRUE), active_fishers = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    filter(active_fishers!=1) %>% 
    mutate(period = floor((Batch.Year - 1991) / period_length) + 1)
  
  fishery.cv <- fishery.rev %>% 
    group_by(period, CFEC.Permit.Fishery) %>%
    summarise(fishery.cv = sd(fishery.annual.rev, na.rm = TRUE)/mean(fishery.annual.rev, na.rm = TRUE), 
           fishery.mean = mean(fishery.annual.rev, na.rm = TRUE), 
           mean.active.fishers = mean(active_fishers, na.rm = TRUE),
           num.years = n()) %>% 
    filter(period!=4)
  
  df.corr <- fishery.rev %>% select(Batch.Year, CFEC.Permit.Fishery ,fishery.annual.rev) %>% 
    spread(key = CFEC.Permit.Fishery, value = fishery.annual.rev) %>% ungroup() %>% 
    select(-Batch.Year) %>% 
    select_if(~ !any(is.na(.)))
  
  salmon_cor <- cor(df.corr,
                     method = c("spearman"))
  
  corrplot(salmon_cor, tl.col = "black",
                 type ="upper", col = brewer.pal(n = 10, name = "RdYlBu"))
}
  
  
{ 
   permit_clean_join <- permit_clean %>% rename(Batch.Year = Year) %>% 
    left_join(permit.annual.rev, by = join_by(Batch.Year, CFEC.Permit.Serial.Number), relationship = "many-to-many") %>% 
    filter(!is.na(Fishery)) %>% 
    ungroup() %>% group_by(Batch.Year, CFECID) %>% 
    mutate(num.owner.permits.linked = n_distinct(CFEC.Permit.Serial.Number, na.rm = TRUE)) %>% 
    ungroup() %>% group_by(Batch.Year, CFECID) %>% 
    mutate(num.vessel.permits.linked = n_distinct(Vessel.ADFG.Number, na.rm = TRUE)) %>% 
    ungroup() %>% filter(Fishery!="")#join the permit and fish ticket data
  
  sum(is.na(permit.annual.rev$year.revenue))#no missing values so any missing value in permit_clean_join is from a non-match in the left_join()
  permit_clean_join <- permit_clean_join %>% mutate(permit.fished = (!is.na(year.revenue) & year.revenue>0))#indicator for a matched permit indicating it was fished
  save(permit_clean_join, file = "intermediate data/permit_clean_join.rdata")
}


load("intermediate data/permit_clean_join.rdata")
permit_clean_join %>% mutate(perm_is_vess_owner = (CFEC.Vessel.Owner.Filing.Number==CFECID)) %>% group_by(perm_is_vess_owner) %>% count()
permit_clean_join %>% group_by(permit.fished) %>% count()

######## Season GROUPING ######## 
season_def <- trip.data %>% filter(CFEC.Permit.Fishery != "") %>% 
  group_by(Batch.Year, CFEC.Permit.Fishery, Date.Landed) %>% 
  summarise(day.revenue=sum(CFEC.Value..Detail., na.rm = TRUE)) %>% 
  group_by(Batch.Year, CFEC.Permit.Fishery) %>% 
  mutate(fishery.revenue = sum(day.revenue)) %>% 
  ungroup() %>% 
  mutate(date.share = day.revenue/fishery.revenue) %>% mutate(date.landed = as.Date(as.character(Date.Landed),format = "%Y%m%d"), date.landed.yday = yday(as.Date(as.character(Date.Landed),format = "%Y%m%d")))

season_def <- season_def %>%
  arrange(Batch.Year, CFEC.Permit.Fishery, date.landed) %>%
  group_by(Batch.Year, CFEC.Permit.Fishery) %>%
  mutate(start.day = min(date.landed.yday), 
         close.day = max(date.landed.yday), 
         cum.day.share = cumsum(date.share))

period_length <- 6
season_def_firstlast <- season_def %>% 
  mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% 
  group_by(CFEC.Permit.Fishery, period) %>% View()
  summarise(open.day.median = median(start.day), close.day.median = median(close.day))

season_def_rev <- season_def %>% 
  mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>% 
  group_by(CFEC.Permit.Fishery, period) %>%
  arrange(cum.day.share) %>%
  mutate(cum.start.day = first(start.day[cum.day.share >= 0.05]),
         cum.close.day = first(close.day[cum.day.share >= 0.95])) %>%
  group_by(CFEC.Permit.Fishery, period) %>% View()
  summarise(open.day.median = median(cum.start.day), close.day.median = median(cum.close.day))

  group_by(Batch.Year, Vessel.ADFG.Number, CFEC.Permit.Serial.Number, CFEC.Permit.Fishery) %>% 
  summarise(year.revenue=sum(CFEC.Value..Detail., na.rm = TRUE), num.trips = n_distinct(trip.id), fishing.days = sum(trip.duration, na.rm = TRUE), unique.checks = n_distinct(CFEC.Permit.Check), CFEC.Permit.Check = first(CFEC.Permit.Check), max.seq.num = max(CFEC.Permit.Sequence), CFEC.Vessel.Owner.Filing.Number = first(CFEC.Vessel.Owner.Filing.Number)) %>% 
  mutate(revenue.per.trip = year.revenue/(num.trips))

# Create a new variable 'week' that represents the week of each date
season_def <- season_def %>%
  mutate(week = floor_date(date.landed, "week"), month = floor_date(date.landed, "month"))
  
  # Summarize your data by 'week'
  monthly_data <- season_def %>%
    group_by(CFEC.Permit.Fishery, Batch.Year, month) %>%
    summarise(monthly_share = sum(date.share, na.rm = TRUE))
  weekly_data <- season_def %>%
    group_by(CFEC.Permit.Fishery, Batch.Year, week) %>%
    summarise(weekly_share = sum(date.share, na.rm = TRUE)) %>% arrange(CFEC.Permit.Fishery, Batch.Year, week)
  
  # Plot the histogram
  weekly_data %>% mutate(week_num = week) %>%  filter(CFEC.Permit.Fishery=="S 03T") %>% 
    ggplot(aes(x = as.Date(week_num), y = weekly_share)) +
    geom_col() +
    labs(x = "Week", y = "Cumulative Day Share", title = "Weekly Cumulative Day Share") +
    facet_wrap(~Batch.Year, scales = "free_x")+
    scale_x_date(date_breaks = "1 month", date_labels = "%b")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  monthly_data %>% filter(CFEC.Permit.Fishery=="S 03T") %>% 
    ggplot(aes(x = month, y = monthly_share)) +
    geom_col() +
    labs(x = "Month", y = "Cumulative Day Share", title = "Monthly Cumulative Day Share") +
    facet_wrap(~Batch.Year)+
    scale_x_date(date_labels="%b-%d",date_breaks  ="3 month")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  monthly_data %>% 
    filter(CFEC.Permit.Fishery=="S 03T" & Batch.Year>=2001 & Batch.Year<=2009) %>% 
    ggplot(aes(x = month, y = monthly_share)) +
    geom_col() +
    labs(x = "Month", y = "Cumulative Day Share", title = "Monthly Cumulative Day Share") +
    facet_wrap(~Batch.Year) +
    scale_x_continuous(breaks = seq(1, 12, 1), labels = month.abb) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  

  season_def <- season_def %>%
    mutate(open.day.yday = yday(open.day), close.day.yday = yday(close.day))
  
  season_def %>%
    group_by(CFEC.Permit.Fishery) %>%
    summarise(open.day.median = median(open.day.yday), close.day.median = median(close.day.yday))
  
######## OWNER GROUPING ######## 
  load("intermediate data/permit_clean_join.rdata")#permit-year level data
# CV , HHI, Num. Permits
permit_clean_join %>% group_by(CFECID, Batch.Year) %>% summarise(annual.revenue = sum(year.revenue, na.rm = TRUE), num.boats = n_distinct(Vessel.ADFG.Number, na.rm = TRUE)) %>% View()
  

permit_clean_join %>% group_by(CFECID, Batch.Year) %>% 
  mutate(permit.owns.fpermit = (CFECID==CFEC.Permit.Holder.Filing.Number), 
         permit.owns.vessel = (CFECID==CFEC.Vessel.Owner.Filing.Number), 
         fpermit.owns.vessel = (CFEC.Vessel.Owner.Filing.Number==CFEC.Permit.Holder.Filing.Number),
         permit.owns.a.fpermit = any(CFECID==CFEC.Permit.Holder.Filing.Number)) %>% 
  ungroup() %>% count(permit.owns.a.fpermit)
  count(permit.owns.fpermit, permit.owns.vessel, fpermit.owns.vessel)

permit_clean_join %>% group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>% 
  mutate(is.vowner.powner = (CFEC.Permit.Holder.Filing.Number == CFEC.Vessel.Owner.Filing.Number)) %>% #ungroup() %>% count(is.vowner.powner)
  summarise(annual.revenue = sum(year.revenue, na.rm = TRUE), num.boats = n_distinct(Vessel.ADFG.Number, na.rm = TRUE), num.non.owned.boat = sum(!is.vowner.powner)) %>% View() #DOESN"T work becasue not counting the number of unique boats
#number of unique boat owners under a permit-owner-year that are not the permit owner
#does permit owner use boat that is not owned by them

permit_clean_join %>% group_by(CFEC.Vessel.Owner.Filing.Number, Batch.Year) %>% summarise(annual.revenue = sum(year.revenue, na.rm = TRUE), num.boats = n_distinct(Vessel.ADFG.Number, na.rm = TRUE)) %>% View()


permit_clean_join %>% group_by(CFECID, Batch.Year) %>% 
  mutate(permit.owns.fpermit = (CFECID==CFEC.Permit.Holder.Filing.Number), 
         permit.owns.vessel = (CFECID==CFEC.Vessel.Owner.Filing.Number), 
         fpermit.owns.vessel = (CFEC.Vessel.Owner.Filing.Number==CFEC.Permit.Holder.Filing.Number),
         permit.owns.a.fpermit = any(CFECID==CFEC.Permit.Holder.Filing.Number)) %>% 
  ungroup() %>% count(permit.owns.a.fpermit)


permit_clean_join %>% group_by(CFECID, Batch.Year) %>% 
  filter(any((CFECID==CFEC.Permit.Holder.Filing.Number))) %>% #permit holder in the permit data actually held a permit that was fished. Excludes permits that were not joined (na value, that is, the permit was not fished at all) and those that were sold to another permit holder (value == FALSE)
  mutate(permit.owns.fpermit = (CFECID==CFEC.Permit.Holder.Filing.Number), 
         permit.owns.vessel = (CFECID==CFEC.Vessel.Owner.Filing.Number), 
         fpermit.owns.vessel = (CFEC.Vessel.Owner.Filing.Number==CFEC.Permit.Holder.Filing.Number)) %>% 
  group_by(CFECID, Batch.Year, permit.owns.vessel) %>% 
  summarise(fvessels.owned = n_distinct(Vessel.ADFG.Number)) %>% filter(permit.owns.vessel==TRUE) %>%  View()#now only consider the TRUE values of permit.owns.vessel
  
summarise(annual.revenue = sum(year.revenue, na.rm = TRUE),
            permit.owns.a.vessel = any(CFECID==CFEC.Vessel.Owner.Filing.Number), 
            num.boats = n_distinct(Vessel.ADFG.Number, na.rm = TRUE)) %>% 
  mutate(unowned) %>% View()

######## BOAT GROUPING ######## 














{permit_clean %>% 
    left_join(permit.annual.rev, by = join_by(Year == Batch.Year, CFEC.Permit.Serial.Number == CFEC.Permit.Serial.Number, ADFG == Vessel.ADFG.Number), na_matches = "never") %>%
    filter(!is.na(Fishery.y)) %>% mutate(same.fishery = str_equal(Fishery.x, Fishery.y)) %>% count(same.fishery) %>% View()
  }#Check that the matched permit serial numbers always have the same Fishery classification (e.g., "S03T") 
sum(is.na(permit.annual.rev$Vessel.ADFG.Number))









{
  permit_clean_join %>% ungroup() %>% count(Permit.Sequence) %>% View() #View the distribution of permit.sequence
  permit_clean_join <- permit_clean_join %>%
    group_by(Vessel.ADFG.Number, Batch.Year, CFEC.Permit.Serial.Number) %>%
    filter(Permit.Sequence==max(Permit.Sequence)) %>% ungroup()
}#filter out the multiple entries of permits caused by the buying/selling of the permits


permit_clean_join %>% ungroup() %>% filter(is.na(CFEC.Permit.Check.y)) %>% count(linked_permits) %>% View()#unjoined permits count by number of permits held by the boat in the same year
permit_clean_join %>% ungroup() %>% filter(is.na(CFEC.Permit.Check.y)) %>% View()#unjoined permits
#filter out the vessel-year landings which have missing permit serial number
sum(permit.annual.rev$CFEC.Permit.Fishery=="")==sum(is.na(permit.annual.rev$CFEC.Permit.Serial.Number))
missing_permit_vessels <- permit.annual.rev %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% filter(any(is.na(CFEC.Permit.Serial.Number))) %>% ungroup()
permit.annual.rev <- permit.annual.rev %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% filter(all(!is.na(CFEC.Permit.Serial.Number))) %>% ungroup()
{
  #now get the permit class ("S03T") revenue shares
  test <- permit.annual.rev %>% #permit number level data ticket data
    group_by(Batch.Year, Vessel.ADFG.Number) %>%  
    group_by(Batch.Year, Vessel.ADFG.Number, CFEC.Permit.Fishery) %>% 
    summarise(annual.revenue = sum(year.revenue, na.rm = TRUE)) %>% #permit class revenue
    group_by(Batch.Year, Vessel.ADFG.Number) %>% 
    mutate(vessel.year.rev = sum(annual.revenue, na.rm = TRUE)) %>% #vessel annual total revenue
    ungroup() %>% 
    mutate(revenue.share = annual.revenue/vessel.year.rev, sq.share = (annual.revenue/vessel.year.rev)^2) %>% 
    mutate(Fishery = str_replace_all(string=CFEC.Permit.Fishery, pattern=" ", repl="")) %>% #adjust so can link back
    filter(Fishery!="") %>% select(-CFEC.Permit.Fishery) %>%
    group_by(Batch.Year, Vessel.ADFG.Number) %>% mutate(hhi = sum(sq.share)) %>% ungroup() %>% #this is fishery hhi
    right_join(permit_clean_join, join_by(Vessel.ADFG.Number, Batch.Year, Fishery), na_matches = "never") 
  
  test %>% filter(is.na(annual.revenue)) %>% count()
  test %>% filter(is.na(unique.checks)) %>% count()#This number is less than the previous because there are some boat-years that have multiple of the same type of permit (like S03T) but they only use some of their permits (permits here meaning unique permit serial numbers) %this is broken right now 
  test %>% filter(is.na(annual.revenue) & is.na(unique.checks)) %>% count()#Same as number of missing values for "annual.revenue". Thus, missing values for unique.checks are also missing for annual.revenue but not vice versa. This is because of what is described in the previous comment.
  
  test <- test %>% ungroup() %>% mutate(did.fish = if_else(is.na(year.revenue),0,1)) %>% 
    group_by(Batch.Year, Vessel.ADFG.Number) %>% 
    mutate(num.dist.fishery = length(unique(Fishery)), num.dist.permit = length(unique(Permit.Number)), num.fished.permits = sum(did.fish), num.fished.fishery = n_distinct(Fishery[did.fish %in% c(1)])) %>% ungroup() %>% 
    mutate(num.unfished.fishery = num.dist.fishery- num.fished.fishery, num.unfished.permits = num.dist.permit - num.fished.permits)
  # now add on the long run variance of the vessel
  
  datareg <- test %>% mutate(period = if_else(Batch.Year>2004, 1,0)) %>% 
    group_by(Vessel.ADFG.Number, Batch.Year) %>% 
    summarise(period = first(period), vessel.year.rev = first(vessel.year.rev), annual.revenue = sum(annual.revenue, na.rm = TRUE), hhi = first(hhi), prime.fishery = first(Fishery[annual.revenue==max(annual.revenue, na.rm = TRUE)]), linked.permits = first(linked_permits), num.dist.fishery = first(num.dist.fishery), num.fished.fishery = first(num.fished.fishery), num.unfished.fishery = first(num.unfished.fishery), num.trips = sum(num.trips, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(vessel.year.rev=if_else(is.na(vessel.year.rev), 0, vessel.year.rev), hhi_dist = hhi-(1/num.dist.fishery))#(1/num.dist.fishery) is the lowest (most diverse HHI)
  
  datareg %>% filter(identical(vessel.year.rev, annual.revenue)) %>% View()
  
  datareg2 <- datareg %>% group_by(Vessel.ADFG.Number, period) %>% 
    summarise(rev.cv = sd(vessel.year.rev, na.rm = TRUE)/mean(vessel.year.rev, na.rm = TRUE), mean.hhi = mean(hhi, na.rm = TRUE), med.hhi_dist = median(hhi_dist, na.rm=TRUE), prime.fishery = first(prime.fishery[vessel.year.rev==max(vessel.year.rev, na.rm = TRUE)]), median.dist.fish = median(num.dist.fishery, na.rm = TRUE), median.fished.fishery = median(num.fished.fishery, na.rm = TRUE), median.unfished.fishery = median(num.unfished.fishery, na.rm = TRUE), median.trips = median(num.trips, na.rm = TRUE))
}


catch_data_temp %>% ungroup() %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "M")) %>% group_by(Batch.Year, CFEC.Permit.Fishery) %>% summarise(Value = sum(CFEC.Value..Detail., na.rm = TRUE)) %>% View()
catch_data_temp %>% ungroup() %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "M")) %>% group_by(Batch.Year, CFEC.Permit.Fishery, CFEC.Vessel.Owner.State) %>% count(CFEC.Vessel.Owner.State) %>% View()
