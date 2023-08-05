#.rs.restartR()
#loads my packages. If not installed in your environment, the missing ones are installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}
getwd()


#read in 2018 vessel data
#path <- paste("./../../../2018 fish ticket data (revised 11-1-22).csv", sep = "")

mypath <- "./../../../2015 fish ticket data (revised 04-14-17).csv"

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
    filter(!any(permit_species!="S")) %>% #filter landings that were done with a boat that fished anything that is not salmon
    ungroup()
  boatlist <- df %>% distinct(Vessel.ADFG.Number) %>% select(Vessel.ADFG.Number)
  return(boatlist)
}

boatlist_nss <- get_otherfishing(mypath)
boatlist_ss <- get_onlysalmon(mypath)
permpath <- "~/JoeData/permits/Permits2014.csv"
Permits2015 <- read.csv(file.path(permpath))
Permits2015 %>% filter(is.na(Vessel.ADFG)) %>% View()
  
sum(is.na(Permits2015$Vessel.ADFG))

df <- Permits2015 %>% filter(Vessel.ADFG %in% boatlist_nss$Vessel.ADFG.Number)
df['Vessel.ADFG']
df <- df %>% mutate(Species = substr(Fishery, 1, 1))
unique(df$Species)
#df <- read_csv("~/JoeData/permits/Permits2015.csv") %>% group_by(`Vessel ADFG`) %>% filter(any(`Vessel ADFG` %in% blist)) %>% arrange(by_group = TRUE) %>%  ungroup()
df2 <- Permits2015 %>% filter(Vessel.ADFG %in% boatlist_ss$Vessel.ADFG.Number)
df2['Vessel.ADFG']
df2 <- df2 %>% mutate(Species = substr(Fishery, 1, 1))
unique(df2$Species)

plot1 <- df %>% ggplot(aes(x = Species)) +geom_bar() + ggtitle("Salmon Specialists Boats -- Permits 2014")
plot2 <- df2 %>% ggplot(aes(x = Species)) +geom_bar() + ggtitle("Non-Salmon Specialists Boats -- Permits 2014")


plot_salmon_notsalmon_boats <- function(data, blist, year){
  df <- data  %>% group_by(`Vessel ADFG`) %>% filter(any(`Vessel ADFG` %in% blist)) %>% arrange(by_group = TRUE) %>%  ungroup()
  plot1 <- {df %>% ggplot(aes(x = Fishery)) +geom_bar()}
  return(plot1) # requires 'patchwork' package
}
test <- plot_salmon_notsalmon_boats(data = Permits2015, blist = boatlist_ss$Vessel.ADFG.Number, year = 2018)




plot_salmon_notsalmon_boats <- function(data, blist, year){
  df <- data  %>% group_by(`Vessel ADFG`) %>% filter(any(`Vessel ADFG` %in% blist)) %>% arrange(by_group = TRUE) %>%  ungroup()
  plot1 <- {df %>% ggplot(aes(x = Fishery)) +geom_bar()}
  return(plot1) # requires 'patchwork' package
}
test <- plot_salmon_notsalmon_boats(data = Permits2015, blist = boatlist_ss$Vessel.ADFG.Number, year = 2018)
