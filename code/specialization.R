#.rs.restartR()
#rm()
#loads my packages. If not installed in your environment, the missing ones are installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', 'xtable', 'corrplot')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work
getwd()

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
source("code/myfunctions.R")

load("intermediate data/catch_data_temp.rdata")

unique(catch_data_temp$Batch.Year)
catch_data_temp %>% ungroup %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% nrow()#this filter would filter to jsut the boats that have fished salmon permit at some point


catch_data_temp$Vessel.ADFG.Number[which(catch_data_temp$Vessel.ADFG.Number==62.39)] <- 62339#fix a typo in the data
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp["CFEC.Value..Detail."])] <- 0#fill the na's with 0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Length
{catch_data_temp$length_cat <- cut(catch_data_temp$AKR.Vessel.Length,
                                   breaks=c(0, 20, 30, 40, 50, 60, 1000),
                                   labels=c('0-20', '20-30', '30-40', '40-50', '50-60', '60+'))
}

catch_data_temp %>% get.trip()
prod <- get.trip(catch_data_temp)
prod$trip.duration <- as.numeric(prod$trip.length)+1
#prod <- prod %>% filter(trip.duration>0) %>% select(Vessel.ADFG.Number) %>% distinct() %>%  nrow()#filter out negative trips
prod <- 


#strategy
{
  prod <- catch_data_temp %>% ungroup() %>% get.trip() %>% mutate(trip.duration=(as.numeric((trip.length))+1)) %>% 
    mutate(trip.duration=ifelse(trip.duration>0,trip.duration, NA)) %>% 
    ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number, CFEC.Permit.Fishery) %>%
    summarise(fishery.revenue=sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id), fishing.days = sum(trip.duration)) %>%
    mutate(revenue.per.trip = fishery.revenue/(num.trips*fishing.days))
  
  prod <- prod %>% ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% 
    mutate(first.permit = CFEC.Permit.Fishery[first(which(fishery.revenue==max(fishery.revenue)))], second.permit = if_else(length(unique(CFEC.Permit.Fishery))==1, NA, CFEC.Permit.Fishery[first(which(fishery.revenue==second_max(fishery.revenue)))], missing=NULL), permits.fished = length(unique(CFEC.Permit.Fishery)), first.species = substr(first.permit, 1, 1), second.species = substr(second.permit, 1, 1))
  
  prod <- prod %>% ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% mutate(specialist_category = case_when(
    any(substr(CFEC.Permit.Fishery, 1, 1) == "S")==FALSE ~ "no salmon",
    any(substr(CFEC.Permit.Fishery, 1, 1) == "S")==TRUE & first.species!="S" ~ "primary non-salmon",
    first.species=="S" & any(substr(CFEC.Permit.Fishery, 1, 1) != "S")==TRUE ~ "primary salmon, non-specialist",
    first.species=="S" & (second.species=="S") & any(substr(CFEC.Permit.Fishery, 1, 1) != "S")==FALSE ~ "only salmon, multiple permits",
    first.species=="S" & (is.na(second.species)) ~ "only salmon, single permit",
    .default = "missing"))# %>% mutate(Salmon = ifelse(permit.species=='S', 'S', 'Non-Salmon'))
  
  }




