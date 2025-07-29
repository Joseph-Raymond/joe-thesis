{packs <- c('readr', 'tidyverse', 'dplyr', 'ggplot2', 'xlsx','data.table', 'fixest', 'lubridate', 'corrplot', 'RColorBrewer', 'knitr', 'cluster','factoextra')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
source("code/myfunctions.R")

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
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
  trip.data <- trip.data %>% mutate(trip.duration=ifelse(trip.duration>0,trip.duration, NA))#going out and back the same day should lead to trip duration = 1
}

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

df_change <- permit.annual.rev %>%
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
  group_by(CFEC.Permit.Fishery, CFEC.Permit.Holder.Filing.Number) %>% 
  mutate(year.revenue.change = year.revenue - lag(year.revenue)) # calculate change in owner.tot.rev

df_change_tot <- permit.annual.rev %>%
  group_by(CFEC.Permit.Holder.Filing.Number) %>% 
  filter(any(substr(CFEC.Permit.Fishery,1,5)=='S 01E')) %>% 
  group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>%
  summarise(owner.tot.rev = sum(year.revenue, na.rm = TRUE)) %>%  #owner total annual revenue
  ungroup %>% 
  complete(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
  replace_na(list(owner.tot.rev = 0)) %>% 
  group_by(CFEC.Permit.Holder.Filing.Number) %>% 
  arrange(Batch.Year) %>% 
  mutate(owner.tot.rev.change = owner.tot.rev - lag(owner.tot.rev), lag.revenue = lag(owner.tot.rev)) %>%  # calculate change in owner.tot.rev
  filter(lag.revenue!=0) %>% 
  filter(!is.na(owner.tot.rev.change) & !is.na(CFEC.Permit.Holder.Filing.Number))





df_change_tot_spread <- df_change_tot %>% 
  ungroup() %>% select(-owner.tot.rev, -lag.revenue) %>% 
  spread(key = Batch.Year, value = owner.tot.rev.change, convert = TRUE) %>% 
  arrange(CFEC.Permit.Holder.Filing.Number)

distance <- dist(df_change_tot_spread %>% select(-CFEC.Permit.Holder.Filing.Number), method = "euclidean") # the default method is euclidean distance
hclustering <- hclust(distance, method = "median") # check out the different methods in the documentation
res.diana <- diana(distance, trace.lev = 3)

# Plot the dendrogram
fviz_dend(res.diana, cex = 0.5,
          k = 4, # Cut in four groups
          palette = "jco" # Color palette
)

plot(hclustering) # or make a dendrogram with as.dendrogram

split_cl <- cutree(hclustering, k = 10)
matrix_transormed <- df_change_tot_spread %>% mutate(cluster = split_cl) # add cluster value

gathered_data <- gather(matrix_transormed, key = Batch.Year, value = owner.tot.rev.change, -CFEC.Permit.Holder.Filing.Number, -cluster) # get back the data in tidy format

gathered_data <- gathered_data %>% 
  mutate(Batch.Year = as.integer(Batch.Year)) %>% 
  left_join(df_change_tot, by = c("Batch.Year", "CFEC.Permit.Holder.Filing.Number"))



cluster.link <- permit.annual.rev %>%
  group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>%
  summarise(year.revenue = sum(year.revenue, na.rm = TRUE)) %>%
  mutate(period = floor((Batch.Year - 1991) / period_length) + 1) %>%
  group_by(Batch.Year, CFEC.Permit.Holder.Filing.Number) %>% 
  mutate(owner.tot.rev = sum(year.revenue, na.rm = TRUE)) %>% #owner total annual revenue
  group_by(period, CFEC.Permit.Holder.Filing.Number) %>% 
  mutate(active_years = n_distinct(Batch.Year)) %>% 
  group_by(period, CFEC.Permit.Fishery) %>% 
  mutate(active_fishers = n_distinct(CFEC.Permit.Holder.Filing.Number)) %>% 
  filter(active_fishers>20) %>% 
  filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
  filter(substr(CFEC.Permit.Fishery,1,3)!="S 7") %>%
  group_by(period, CFEC.Permit.Holder.Filing.Number, CFEC.Permit.Fishery) %>%
  summarise(owner.cv = sd(owner.tot.rev, na.rm = TRUE)/mean(owner.tot.rev, na.rm = TRUE), owner.mean = mean(owner.tot.rev, na.rm = TRUE), nobs = n()) %>% 
  filter(nobs!=1) %>% left_join(gathered_data, by = c("CFEC.Permit.Holder.Filing.Number"),relationship = "many-to-one", multiple = "first") %>% select(-Batch.Year, -owner.tot.rev.change.x, -owner.tot.rev.change.y)

cluster.link %>% filter(period==3) %>% filter(substr(CFEC.Permit.Fishery,1,4)=="S 03") %>%
  ggplot(aes(x = owner.mean, y = owner.cv, color = cluster)) +
  geom_point()