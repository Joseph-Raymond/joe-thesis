
{packs <- c('readr', 'tidyverse', 'dplyr', 'ggplot2', 'xlsx','data.table', 'fixest', 'lubridate', 'corrplot', 'RColorBrewer', 'knitr', 'cluster','factoextra', 'zoo', 'purrr','ggtext')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
source("code/myfunctions.R")


{
  load("intermediate data/catch_data_temp.rdata")
  #catch_data_temp %>% ungroup %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% nrow()#this filter would filter to jsut the boats that have fished salmon permit at some point
  catch_data_temp$Vessel.ADFG.Number[which(catch_data_temp$Vessel.ADFG.Number==62.39)] <- 62339#fix a typo in the data
  catch_data_temp <- catch_data_temp %>% filter(Vessel.ADFG.Number!=0) #8 missing values with vessel numbers == 0
  catch_data_temp <- catch_data_temp %>% mutate(Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number))
  catch_data_temp <- catch_data_temp %>% mutate(CFEC.Value..Detail. = if_else(is.na(CFEC.Value..Detail.),0,CFEC.Value..Detail.))
  #filter out boats that made landings but did not report the permit/fishery in which they made the landing
  catch_data_temp <- catch_data_temp %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% filter(!any(is.na(Permit.Serial.Number)) ) %>% ungroup()
}

{
  trip.data <- get.trip(catch_data_temp)
  trip.data$trip.duration <- as.numeric(trip.data$trip.length)+1
  trip.data <- trip.data %>% mutate(trip.duration=ifelse(trip.duration>=0,trip.duration, NA))#these trips have trip-date entry errors. Not throwing out the data for now but indicating the error by replacing any trips with negative trip duration as NA; going out and back the same day should lead to trip duration = 1
}
{
  permit.annual.rev <- trip.data %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, Vessel.ADFG.Number, CFEC.Permit.Serial.Number) %>% 
    summarise(year.revenue=sum(CFEC.Value..Detail., na.rm = TRUE), 
              num.trips = n_distinct(trip.id), 
              fishing.days = sum(trip.duration, na.rm = TRUE), 
              unique.checks = n_distinct(CFEC.Permit.Check), #these should all be =1. include to check for any errors
              CFEC.Permit.Check = first(CFEC.Permit.Check), 
              max.seq.num = max(CFEC.Permit.Sequence), 
              CFEC.Vessel.Owner.Filing.Number = ifelse(all(is.na(CFEC.Vessel.Owner.Filing.Number)), NA, first(na.omit(CFEC.Vessel.Owner.Filing.Number))), 
              CFEC.Permit.Fishery = ifelse(all(is.na(CFEC.Permit.Fishery)), NA, first(na.omit(CFEC.Permit.Fishery)))) %>% #if some of the entries for the permit fishery code are missing but are still listed to the same Batch.Year, CFEC.Permit.Holder.Filing.Number, Vessel.ADFG.Number, and CFEC.Permit.Serial.Number then replace the missing values with the first observed permit fishery code in the group
    mutate(revenue.per.trip = year.revenue/(num.trips))
  
  rm(catch_data_temp)
  gc()
}  
period_length <- 10 #number of years defining a "period" for some statistics that will be at the period-level

get.owner.ppermit <- function(df){ #gets the first permit for each owner-year; picking the permit for each vessel that has the most revenue in a given year
  df_test <- df %>% ungroup() %>%  group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>% summarise(tot.revenue = sum(year.revenue, na.rm = TRUE)) %>%
    filter(CFEC.Permit.Fishery!="") %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    summarise(first.permit = CFEC.Permit.Fishery[first(which(tot.revenue==max(tot.revenue)))], second.permit = ifelse(length(unique(CFEC.Permit.Fishery))==1, NA, CFEC.Permit.Fishery[first(which(tot.revenue==second_max(tot.revenue)))]), permit.fished.max = length(unique(CFEC.Permit.Fishery)))#second_max() is a function from myfunctions.r and it returns the second highest value if it exists (NA otherwise)
  return(df_test)
}

permit.annual.rev <- permit.annual.rev %>% ungroup() %>% mutate(period = floor((Batch.Year - 1991) / period_length) + 1)

rev.permits.changing <- permit.annual.rev %>% ungroup() %>%
  group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year, CFEC.Permit.Fishery) %>% 
  summarise(fishery.annual.rev = sum(year.revenue, na.rm = TRUE)) %>%
  group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
  mutate(owner.annual.rev = sum(fishery.annual.rev, na.rm = TRUE), annual.rev.share = fishery.annual.rev/sum(fishery.annual.rev, na.rm = TRUE), owner.annual.hhi = sum(annual.rev.share^2, na.rm = TRUE)) %>% 
  group_by(CFEC.Permit.Holder.Filing.Number, period) %>% 
  mutate(period.avg.owner.rev = mean(owner.annual.rev, na.rm = TRUE)) %>%
  group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>% 
  mutate(owner.rev.dev = owner.annual.rev-period.avg.owner.rev) %>% 
  group_by(CFEC.Permit.Holder.Filing.Number, period, CFEC.Permit.Fishery) %>% 
  mutate(period.avg.rev = mean(fishery.annual.rev, na.rm = TRUE), period.rev.share = period.avg.rev/period.avg.owner.rev) %>%
  group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year, CFEC.Permit.Fishery) %>% 
  mutate(permit.rev.deviation = (fishery.annual.rev - period.avg.rev)) %>% 
  filter(!is.na(CFEC.Permit.Holder.Filing.Number)) %>% 
  group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>%
  mutate(is.prime.permit = if_else(fishery.annual.rev == max(fishery.annual.rev), 1,0))

#variable to tell if the largest shock in a owner-year was caused by a primary permit shock or not
rev.permits.changing <- rev.permits.changing %>% 
  group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>% 
  mutate(is.biggest.change = if_else(abs(permit.rev.deviation) == max(abs(permit.rev.deviation), na.rm = TRUE), 1,0), 
         is.shock.pp = (is.biggest.change*is.prime.permit)) %>% 
  group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
  mutate(period.avg.hhi = mean(owner.annual.hhi, na.rm = TRUE)) %>% 
  group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
  arrange(Batch.Year) %>% 
  mutate(hhi.deviation = owner.annual.hhi - period.avg.hhi, rev.deviation = (owner.annual.rev - period.avg.rev)/period.avg.rev)# calculate change in hhi from the period mean

rev.permits.changing <- rev.permits.changing %>%
  group_by(CFEC.Permit.Holder.Filing.Number, period) %>% 
  mutate(period.share.sum = sum(annual.rev.share, na.rm = TRUE)) %>% #summing up the revenue shares from the entire period for each permit holder (should be equal to the number of years in which the permit holder had positve revenue during the period)
  group_by(CFEC.Permit.Holder.Filing.Number, period, CFEC.Permit.Fishery) %>% 
  mutate(fishery.share.share = sum(annual.rev.share, na.rm = TRUE)/period.share.sum)# proportion of period revenue for the given fishery



# calculating the "overall" period-level hhi (versus averaging the annual hhi's)
tenyear.owner.permits <- rev.permits.changing %>% filter(period.share.sum>1) %>% #filter out permit owner-periods where the owner only had one year of fishing (needs to have 2 or more in the period to count)
  ungroup() %>% 
  group_by(CFEC.Permit.Holder.Filing.Number, period) %>% 
  mutate(period.owner.tot.rev = sum(fishery.annual.rev, na.rm =TRUE)) %>% 
  group_by(CFEC.Permit.Holder.Filing.Number, period, CFEC.Permit.Fishery) %>% 
  summarise(period.total.share = sum(fishery.annual.rev, na.rm = TRUE)/first(period.owner.tot.rev), fishery.share.share = first(fishery.share.share), period.share.sum = first(period.share.sum), AnnualAverageHHI = first(period.avg.hhi), years_fished = n()) %>%
  group_by(CFEC.Permit.Holder.Filing.Number, period) %>% 
  mutate(period.owner.tot.hhi = sum(period.total.share^2, na.rm = TRUE), norm.period.hhi = sum(fishery.share.share^2, na.rm = TRUE), period.hhi.diff = period.owner.tot.hhi-norm.period.hhi)


trip.data %>% filter(CFEC.Permit.Fishery=="M 07B") %>% View()
# period.total.share -- share of fishing revenue from a given fishery of the total revenue from the period
# fishery.share.share -- proportion of period revenue shares accounted for by the given fishery for the entire sum of shares over the period



rev.permits.changing <- rev.permits.changing %>% 
  left_join(tenyear.owner.permits, join_by(CFEC.Permit.Holder.Filing.Number, period, CFEC.Permit.Fishery)) %>% 
  mutate(norm.hhi.diff = norm.period.hhi-period.avg.hhi, hhi.diff = period.owner.tot.hhi-period.avg.hhi)


#want to check of the permit in each row was also present in the previous year under the same owner 
#make a was present last year flag

rev.permits.changing %>% 
  arrange(CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery, Batch.Year) %>%
  group_by(CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>% 
  mutate(lag_fishery = lag(CFEC.Permit.Fishery, n=1))%>% select(CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery, Batch.Year, lag_fishery) %>%  View() #creates a lag fishery variable to see if the fishery was present in the previous year

{
  rspecialists <- tenyear.owner.permits %>% 
    group_by(CFEC.Permit.Holder.Filing.Number, period) %>% filter(AnnualAverageHHI>0.95 & period.owner.tot.hhi<0.8)
  
  nrow(rspecialists)/nrow(tenyear.owner.permits)#5.1% of all permit-owner-periods meet this relatively strict definition of a rotating specialist
  
  tenyear.owner.permits %>% 
    group_by(CFEC.Permit.Holder.Filing.Number, period) %>% filter(AnnualAverageHHI>0.95) %>% 
    ggplot()+
    geom_histogram(aes(period.owner.tot.hhi))+
    labs(title = "Distribution of period-wide hhi for permit owners that are annual specialists (avg. annual HHI>0.95)")
  
  rspecialists %>% 
    ggplot()+
    geom_histogram(aes(period.owner.tot.hhi))+
    labs(title = "Distribution of period-wide hhi for 'rotating specialists' (avg. annual HHI > 0.95 AND period-wide HHI < 0.80)")
  
  rspecialists %>% mutate(num_fishing_years = period.share.sum) %>% 
    ggplot()+
    geom_histogram(aes(num_fishing_years))
  
  rspecialists %>% 
    ggplot()+
    geom_histogram(aes(period))+
    labs(title = "Count of when rotating specialists are identified", subtitle = "period 1 = 1990-2000; period 2 = 2001-2010; period 3 = 2011-2020")
  
  rspecialists %>% 
    group_by(CFEC.Permit.Fishery) %>% 
    count %>% 
    filter(n>25) %>% 
    ggplot(aes(x=n, y=CFEC.Permit.Fishery))+
    geom_bar(stat = 'identity')+
    labs(title = "Counts of rotating specialists identification by fishery (filter for count>25)")
  
  rspecialists %>% 
    group_by(CFEC.Permit.Fishery) %>% filter(period==1) %>% 
    count %>% 
    filter(n>10) %>% 
    ggplot(aes(x=n, y=CFEC.Permit.Fishery))+
    geom_bar(stat = 'identity')+
    labs(title = "Counts of rotating specialists identification by fishery (filter for count>10); (1990-2000)")
  rspecialists %>% 
    group_by(CFEC.Permit.Fishery) %>% filter(period==2) %>% 
    count %>% 
    filter(n>10) %>% 
    ggplot(aes(x=n, y=CFEC.Permit.Fishery))+
    geom_bar(stat = 'identity')+
    labs(title = "Counts of rotating specialists identification by fishery (filter for count>10); (2001-2010)")
  rspecialists %>% 
    group_by(CFEC.Permit.Fishery) %>% filter(period==3) %>% 
    count %>% 
    filter(n>10) %>% 
    ggplot(aes(x=n, y=CFEC.Permit.Fishery))+
    geom_bar(stat = 'identity')+
    labs(title = "Counts of rotating specialists identification by fishery (filter for count>10); (2011-2020)")
  
  rspecialists %>% 
    ggplot(aes(y=CFEC.Permit.Fishery))+
    geom_bar()+
    labs(title = "Counts of rotating specialists identification by fishery (no filter)")
}


{
  tenyear.owner.permits %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", period==1) %>% 
    ggplot() +
    geom_point(aes(x = AnnualAverageHHI, y = period.owner.tot.hhi)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "(1990-2000) Annual Average HHI (x), Period Agggregate HHI (y)",
         x = "Annual Average HHI",
         y = "Period Agggregate HHI") +
    theme_minimal()
  
  tenyear.owner.permits %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", period==2) %>% 
    ggplot() +
    geom_point(aes(x = AnnualAverageHHI, y = period.owner.tot.hhi)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "(2000-2010) Annual Average HHI (x), Period Agggregate HHI (y)",
         x = "Annual Average HHI",
         y = "Period Agggregate HHI") +
    theme_minimal()
  
  tenyear.owner.permits %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", period==3) %>% 
    ggplot() +
    geom_point(aes(x = AnnualAverageHHI, y = period.owner.tot.hhi)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "(2010-2020) Annual Average HHI (x), Period Agggregate HHI (y)",
         x = "Annual Average HHI",
         y = "Period Agggregate HHI") +
    theme_minimal()
  
  tenyear.owner.permits %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
    ggplot() +
    geom_point(aes(x = AnnualAverageHHI, y = period.owner.tot.hhi)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "(1990-2020) Annual Average HHI (x), Period Agggregate HHI (y)",
         x = "Annual Average HHI",
         y = "Period Agggregate HHI") +
    theme_minimal()
  
  tenyear.owner.permits %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
    ggplot() +
    geom_point(aes(x = norm.period.hhi, y = period.owner.tot.hhi)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "(1990-2020)",
         x = "Norm. Period Agggregate HHI",
         y = "Period Agggregate HHI") +
    theme_minimal()
  
  tenyear.owner.permits %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", period==1) %>% 
    ggplot() +
    geom_point(aes(x = AnnualAverageHHI, y = norm.period.hhi)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "(1990-2020)",
         x = "Annual Average HHI",
         y = "Norm. Period Agggregate HHI") +
    theme_minimal()  
}


#investigating M07 fisheries
{
  #find any permit owner that has been associated with a M07 fishery
  library(viridis)
  p1_data <- permit.annual.rev %>% group_by(CFEC.Permit.Holder.Filing.Number) %>% 
    filter(any(CFEC.Permit.Fishery=="M 07B") | any(CFEC.Permit.Fishery=="M 07A")) %>% 
    group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>% mutate(n_fisheries = n_distinct(CFEC.Permit.Fishery)) %>% 
    group_by(Batch.Year) %>% 
    mutate(tot.num.permit.own = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year, n_fisheries) %>% 
    summarize(num.active.fisheries = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year) %>% 
    mutate(tot.num.permit.own = sum(num.active.fisheries), prop.active.fisheries = num.active.fisheries/tot.num.permit.own) %>%
    ungroup() %>% mutate(active_fisheries = as.character(n_fisheries)) 
  p1_data %>% 
    ggplot()+
    geom_line(aes(x=Batch.Year, y = num.active.fisheries, group = active_fisheries, colour = active_fisheries))+
    scale_color_viridis(discrete = TRUE)+
    ggtitle("Number of active fisheries for permit owners who have participated in the M07B fishery (misc. saltwater finfish otter trawl)")
  p1_data %>% 
    ggplot()+
    geom_line(aes(x=Batch.Year, y = prop.active.fisheries, group = active_fisheries, colour = active_fisheries))+
    scale_color_viridis(discrete = TRUE)+
    ggtitle("Proportion of active fisheries for permit owners who have participated in the M07B fishery (misc. saltwater finfish otter trawl)")
  
  
  p2_data <- permit.annual.rev %>% group_by(CFEC.Permit.Holder.Filing.Number) %>% 
    filter(any(CFEC.Permit.Fishery=="B 06B")) %>% 
    group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>% mutate(n_fisheries = n_distinct(CFEC.Permit.Fishery)) %>% 
    group_by(Batch.Year) %>% 
    mutate(tot.num.permit.own = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year, n_fisheries) %>% 
    summarize(num.active.fisheries = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year) %>% 
    mutate(tot.num.permit.own = sum(num.active.fisheries), prop.active.fisheries = num.active.fisheries/tot.num.permit.own) %>%
    ungroup() %>% mutate(active_fisheries = as.character(n_fisheries)) 
  p2_data %>% 
    ggplot()+
    geom_line(aes(x=Batch.Year, y = num.active.fisheries, group = active_fisheries, colour = active_fisheries))+
    scale_color_viridis(discrete = TRUE)+
    ggtitle("Number of active fisheries for permit owners who have participated in the B06B fishery (Halibut longline <5ton, statewide)")
  p2_data %>% 
    ggplot()+
    geom_line(aes(x=Batch.Year, y = prop.active.fisheries, group = active_fisheries, colour = active_fisheries))+
    scale_color_viridis(discrete = TRUE)+
    ggtitle("Proportion of active fisheries for permit owners who have participated in the B06B fishery (Halibut longline <5ton, statewide)")
  
  
  hb_active_data <- permit.annual.rev %>% group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    filter(any(CFEC.Permit.Fishery=="B 06B")) %>% 
    group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>% mutate(n_fisheries = n_distinct(CFEC.Permit.Fishery)) %>% 
    group_by(Batch.Year) %>% 
    mutate(tot.num.permit.own = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year, n_fisheries) %>% 
    summarize(num.active.fisheries = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year) %>% 
    mutate(tot.num.permit.own = sum(num.active.fisheries), prop.active.fisheries = num.active.fisheries/tot.num.permit.own) %>%
    ungroup() %>% mutate(active_fisheries = as.character(n_fisheries)) 
  hb_active_data %>% filter(active_fisheries!='10') %>% 
    ggplot()+
    geom_line(aes(x=Batch.Year, y = num.active.fisheries, group = active_fisheries, colour = active_fisheries), size = 1)+
    scale_color_viridis(discrete = TRUE)+
    ggtitle("Number of active fisheries for permit owners in years where they are fishing the B06B fishery (Halibut longline <5ton, statewide)")
  hb_active_data %>% filter(active_fisheries!='10') %>% 
    ggplot()+
    geom_line(aes(x=Batch.Year, y = prop.active.fisheries, group = active_fisheries, colour = active_fisheries), size = 1)+
    scale_color_viridis(discrete = TRUE)+
    ggtitle("Proportion of active fisheries for permit owners in years where they are fishing the B06B fishery (Halibut longline <5ton, statewide)")
  
  
  hb_inactive_data <- permit.annual.rev %>% 
    group_by(CFEC.Permit.Holder.Filing.Number) %>% filter(any(CFEC.Permit.Fishery=="B 06B")) %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% filter(!any(CFEC.Permit.Fishery=="B 06B")) %>% 
    group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>% mutate(n_fisheries = n_distinct(CFEC.Permit.Fishery)) %>% 
    group_by(Batch.Year) %>% 
    mutate(tot.num.permit.own = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year, n_fisheries) %>% 
    summarize(num.active.fisheries = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
    group_by(Batch.Year) %>% 
    mutate(tot.num.permit.own = sum(num.active.fisheries), prop.active.fisheries = num.active.fisheries/tot.num.permit.own) %>%
    ungroup() %>% mutate(active_fisheries = as.character(n_fisheries)) 
    
    hb_inactive_data %>% filter(active_fisheries!='10') %>% 
    ggplot()+
    geom_line(aes(x=Batch.Year, y = num.active.fisheries, group = active_fisheries, colour = active_fisheries), size = 1)+
    scale_color_viridis(discrete = TRUE)+
    ggtitle("Number of active fisheries for permit owners in years where they are not fishing the B06B fishery (Halibut longline <5ton, statewide)")
    
    hb_inactive_data %>% filter(active_fisheries!='10') %>% 
    ggplot()+
    geom_line(aes(x=Batch.Year, y = prop.active.fisheries, group = active_fisheries, colour = active_fisheries), size = 1)+
    scale_color_viridis(discrete = TRUE)+
    ggtitle("Proportion of active fisheries for permit owners in years where they are not fishing the B06B fishery (Halibut longline <5ton, statewide)")
  
  #in years where you are actually fishing halibut you are more likely to be fishing more than one fishery-permit than in any random year provided that you have fished halibut at some point
}

{
  hb_active_data <- permit.annual.rev %>% group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    filter(any(CFEC.Permit.Fishery=="B 06B")) %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    summarize(annual.rev = sum(year.revenue,na.rm=FALSE), fisheries = n_distinct(CFEC.Permit.Fishery)) %>% 
    group_by(CFEC.Permit.Holder.Filing.Number) %>%
    summarize(mean.rev = mean(annual.rev), cv_rev = var(annual.rev)/mean.rev)
    
  hb_inactive_data <- permit.annual.rev %>% 
    group_by(CFEC.Permit.Holder.Filing.Number) %>% filter(any(CFEC.Permit.Fishery=="B 06B")) %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% filter(!any(CFEC.Permit.Fishery=="B 06B")) %>% 
    group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
    summarize(annual.rev = sum(year.revenue,na.rm=FALSE), fisheries = n_distinct(CFEC.Permit.Fishery)) %>% 
    group_by(CFEC.Permit.Holder.Filing.Number) %>%
    summarize(mean.rev = mean(annual.rev), cv_rev = var(annual.rev)/mean.rev)
  
  joined_data <- hb_active_data %>% left_join(hb_inactive_data, join_by(CFEC.Permit.Holder.Filing.Number), keep = FALSE) %>% 
    mutate(mean.diff = mean.rev.x - mean.rev.y, cv_diff = cv_rev.x - cv_rev.y)
  joined_data %>% 
    filter(mean.diff<1000000 & mean.diff>-1000000) %>% #excluding these big guys is substantial. Not many of them but they are really big
    filter(!is.na(cv_diff)) %>% 
    ggplot()+
    geom_histogram(aes(mean.diff), bins = 20, boundary = 0)
  joined_data %>% 
    filter(!is.na(cv_diff)) %>% 
    ggplot()+
    geom_histogram(aes(cv_diff), bins = 20, boundary = 0)
  
  hist(joined_data$mean.diff)
  hist(joined_data$cv_diff)
  #on average when comparing all permit owners who have participated in the B06B fishery (halibut), the years in which the RS's are active in halibut more often exhibit higher mean annual revenue and a lower CV
}

{
  #how the mean for active hb rotating fishers compares to the rest of the means of their primary and secondary fishery
  joined_data %>% left_join(rspecialists, join_by(CFEC.Permit.Holder.Filing.Number), keep = FALSE) %>% 
    ungroup() %>% mutate(is.rotating.specialist=!is.na(period.total.share)) %>%
    ggplot(aes(x=mean.diff, fill=is.rotating.specialist))+
    geom_histogram(alpha=0.4, position="identity",bins = 20, boundary = 0)+
    xlim(-1000000,1000000)+
    labs(title = "Mean revenue difference in years active vs non-active years in fishery B06B (Halibut Longline Vessel under 60' Statewide)", subtitle = "Unit of observation is a permit-owner-period. Permit owner must have landed positive revenue under B06B to be included and must have spent at least one year landing fishing positive revenue but not in B06B. A positive 'mean.diff' indicates that the average total annual revenue of the permit owner is higher in years where the permit owner is active in the B06B compared to when they are inactive.")+
    theme(plot.subtitle = element_textbox_simple())
  #for rotating specialists, in years where they do/don't participate in B 06B, what does their mean revenue look like compared to the other fishers in B 06B and their secondary fishery?
  compare_data <- rspecialists %>% 
    group_by(CFEC.Permit.Holder.Filing.Number) %>% filter(any(CFEC.Permit.Fishery=="B 06B")) %>% #filter for the rotating specialists in B 06B
    select(CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery, period, period.share.sum) %>% #period.share.sum is just a variable that will be used as a join flag (and it counts the number of years the permit holder was active in the period)
    right_join(permit.annual.rev, join_by(CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery, period), keep = FALSE) %>%
    mutate(is.rotate.sp = !is.na(period.share.sum)) %>% 
    group_by(CFEC.Permit.Holder.Filing.Number, Batch.Year) %>% 
    mutate(in.halibut = any(CFEC.Permit.Fishery=="B 06B"), annual.tot.rev = sum(year.revenue, na.rm = TRUE)) %>%
    group_by(Batch.Year, CFEC.Permit.Fishery, is.rotate.sp, in.halibut) %>% 
    summarise(Fishery.Mean.Rev = mean(annual.tot.rev, na.rm = TRUE)) %>% 
    filter(CFEC.Permit.Fishery %in% c("B 06B", "B 61B" , "C 61B" , "C 06B" , "S 15B" , "S 03T" , "S 03A" , "S 03H" , "S 03E" , "S 01A" , "S 01K" , "S 05B" , "M 06B" , "M 61B" , "M 26B" , "G 34T" , "G 01T")) #Limit the number of fisheries to consider
    
    compare_data %>% arrange(Batch.Year) %>% filter(CFEC.Permit.Fishery=="B 61B") %>% ungroup() %>% 
      mutate(category = ifelse(in.halibut==TRUE, "Not Rotating Specialists; Active in Halibut", "placeholder")) %>% 
      mutate(category = ifelse(in.halibut!=TRUE, "Not Rotating Specialists; Not Active in Halibut", category)) %>%
      mutate(category = ifelse(is.rotate.sp, "Rotating Specialists", category)) %>%
      ggplot()+
      geom_line(aes(x = Batch.Year, y = Fishery.Mean.Rev, group = category, color = category))+
    labs(title = "Halibut (B 06B) rotating specialists in other fisheries; (B 61B)")
    
    
  permit.annual.rev %>% 
    group_by(CFEC.Permit.Holder.Filing.Number) %>% filter(any(CFEC.Permit.Fishery=="B 06B")) %>% #only consider those permit owners that have participated in B06B at least once
    group_by(CFEC.Permit.Fishery, Batch.Year) %>% 
    summarise(count = n()) %>% filter(CFEC.Permit.Fishery!='B 06B') %>%  View()
  
  
  #how does the CV compare for these rotating specialists compared to those in their primary/secondary fisheries
  
  #does their revenue go up or down in active years
  
  #how their overall 
  
  #permit owners with multiple boats
  #boats with multiple permit owners
  
}

