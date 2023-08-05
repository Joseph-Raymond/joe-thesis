#.rs.restartR()
#loads my packages. If not installed in your environment, the missing ones are installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}
getwd()

get_otherfishing <- function(path){
  df <- read.csv(file.path(path)) %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% arrange(by_group = TRUE) %>% ungroup()#filter VESSELS (not permits) that have active salmon permits in 2018
  {df$Date.Fishing.Began <- as.Date(as.character(df$Date.Fishing.Began),format = "%Y%m%d")
    df$Date.Landed <- as.Date(as.character(df$Date.Landed),format = "%Y%m%d")
    df$Week.Ending.Date <- as.Date(df$Week.Ending.Date,format = "%Y-%m-%d")
    df <- df %>% mutate(week = as.Date(cut(Date.Landed, "week")))}#format time variables
  df <- df %>% mutate(permit_species = substr(CFEC.Permit.Fishery, 1, 1))#create species letter from the permit
  df <- df %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
    group_by(Vessel.ADFG.Number) %>%  
    filter(any(permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D"))#filter landings that have done salmon or halibut or black cod or misc or dungeness crab
  boatlist <- df %>% distinct(Vessel.ADFG.Number) %>% select(Vessel.ADFG.Number)
  return(boatlist)
}
get_onlysalmon <- function(path){
  df <- read.csv(file.path(path)) %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% arrange(by_group = TRUE) %>% ungroup()#filter VESSELS (not permits) that have active salmon permits in 2018
  {df$Date.Fishing.Began <- as.Date(as.character(df$Date.Fishing.Began),format = "%Y%m%d")
    df$Date.Landed <- as.Date(as.character(df$Date.Landed),format = "%Y%m%d")
    df$Week.Ending.Date <- as.Date(df$Week.Ending.Date,format = "%Y-%m-%d")
    df <- df %>% mutate(week = as.Date(cut(Date.Landed, "week")))}#format time variables
  df <- df %>% mutate(permit_species = substr(CFEC.Permit.Fishery, 1, 1))#create species letter from the permit
  df <- df  %>%  group_by(Vessel.ADFG.Number) %>% 
    filter(!any(permit_species!="S")) %>% #filter out landings that were done with a boat that fished anything that is not salmon
    ungroup()
  boatlist <- df %>% distinct(Vessel.ADFG.Number) %>% select(Vessel.ADFG.Number)
  return(boatlist)
}

mypath <- "./../../../2015 fish ticket data (revised 04-14-17).csv"
boatlist_nss <- get_otherfishing(mypath)
boatlist_ss_2015 <- get_onlysalmon(mypath)# boats that
permpath <- "~/JoeData/permits/Permits2015.csv"
Permits2015 <- read.csv(file.path(permpath)) %>% mutate(permit_species = substr(Fishery, 1, 1))

Permits2015 %>% filter(is.na(Vessel.ADFG)) %>% View()#data with missing boat number
Permits2015 %>% filter(!is.na(Vessel.ADFG)) %>% View()#data with available boat number

aa <- Permits2015 %>% filter(is.na(Vessel.ADFG)) %>% count(permit_species)#data with missing boat number
ab <- Permits2015 %>% filter(!is.na(Vessel.ADFG)) %>% count(permit_species)#data with available boat number

Permits2015 %>% filter(!is.na(Vessel.ADFG)) %>% count(permit_species) %>% nrow()#data with available boat number
Permits2015 %>% 
  filter(!is.na(Vessel.ADFG)) %>% 
  group_by(Vessel.ADFG) %>% 
  summarise(count = n_distinct(permit_species))

sum(!is.na(Permits2015$Vessel.ADFG))

#permit diversity among salmon specialists
{
  df <- Permits2015 %>% filter(Vessel.ADFG %in% boatlist_ss_2015$Vessel.ADFG.Number)
  permitcount <- df %>% filter(!is.na(Vessel.ADFG)) %>% group_by(Vessel.ADFG) %>%
    summarise(count = n_distinct(permit_species))
  df <- df %>% group_by(Vessel.ADFG) %>%
    mutate(unique.fisheries=permitcount$count[which()])
permitplot <- permitcount %>% ggplot(aes(x = count)) +geom_bar() + ggtitle("Salmon Specialists Number of Unique Salmon Permits 2015")
    #mutate(unique.fisheries = (nrow(count(Fishery))))
}

{
  mypath <- "./../../../2015 fish ticket data (revised 04-14-17).csv"
  catch2015 <- read.csv(file.path(mypath)) %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% arrange(by_group = TRUE) %>% ungroup() %>% mutate(permit_species = substr(CFEC.Permit.Fishery, 1, 1))#filter VESSELS (not permits) that have active salmon permits in 2015 
  Vessels2015 <- read.csv("~/JoeData/vessels/Vessels2015.csv")
  View(Vessels2015)
  
  cathccount <- catch2015 %>% 
    filter(!is.na(Vessel.ADFG.Number)) %>%
    group_by(Vessel.ADFG.Number) %>%
    summarise(count = n_distinct(CFEC.Permit.Fishery))
  cathccount_new <- cathccount %>% 
    left_join(Vessels2015, by = join_by(Vessel.ADFG.Number == ADFG.Number))
  cathccount_inner<- cathccount %>% 
    inner_join(Vessels2015, by = join_by(Vessel.ADFG.Number == ADFG.Number))
  length(unique(cathccount_new$Vessel.ADFG.Number))
  length(unique(cathccount_inner$Vessel.ADFG.Number))
  length(unique(cathccount$Vessel.ADFG.Number))
  
  cathccount_inner$Length_cat <- cut(cathccount_inner$Length,
                breaks=c(10, 20, 30, 40, 50, 60, 300),
                labels=c('10-20', '20-30', '30-40', '40-50', '50-60', '60+'))
  catchplot <- cathccount_inner %>%
    ungroup() %>%
    ggplot(aes(x = count)) +geom_bar(aes(fill=Length_cat)) + ggtitle("Salmon Specialists Number of Unique Salmon Permits LANDINGS 2015")
  catchplot + theme_bw() + xlab("Number of Permits Used During a Season") + xlab("Number of Boats")
}


