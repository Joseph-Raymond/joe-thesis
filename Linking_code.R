rm(list = ls())
path2home <- ""#my available directory stops at "home" so this is just blank
datapath <- "/home/akfin/"
sub_dir <- "jraymond"
main_dir <- paste(path2home, datapath, sep = "")

# check if sub directory exists and if it doesn't then create it. Otherwise set wd to the sub directory
if (file.exists(file.path(main_dir, sub_dir))){
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))} else {
    # create a new sub directory inside
    # the main path
    dir.create(file.path(main_dir, sub_dir))
    # specifying the working directory
    setwd(file.path(main_dir, sub_dir))}

#loads my packages. If not installed in your environment, the missing ones are installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork')
  new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(packs, require, character.only = T)}
getwd()

vessels <- read.csv(file.path("~/JoeData/clean_data/vessels_clean_1978_2022.csv")) %>% filter(Year==2018)
vessels <- vessels %>% filter(Year==2018)

load("./data_2018.RData")

colnames(dlist[[1]]) 

uvessels <- sort(unique(vessels$ADFG.Number))
ulandings1 <- sort(unique(dlist[[1]]$Vessel.ADFG.Number))
ulandings2 <- sort(unique(dlist[[1]]$CFEC.Vessel.ADFG.Number))
ulandings3 <- sort(unique(dlist[[1]]$AKR.Vessel.ADFG.Number))
ulandings4 <- sort(unique(dlist[[1]]$CFEC.Permit.Vessel.ADFG.Number))

#lets use Vessel.ADFG.Number which seems to be the same as AKR.Vessel.ADFG.Number

setequal(ulandings1, ulandings3)
setequal(dlist[[1]]$Vessel.ADFG.Number, dlist[[1]]$AKR.Vessel.ADFG.Number)

#use Vessel.ADFG.Number in the akfin data for the linking. Might need AKR.Vessel.ADFG.Number for other years

all(uvessels %in% ulandings1)#not all the registered vessels are in akfin data base
all(ulandings1 %in% uvessels)#not all vessels in the akfin database are in the vessel data base
rogue_vessels <- dlist[[1]] %>% filter(!Vessel.ADFG.Number %in% uvessels)#isolate these
u_rogue_vessels <- sort(unique(rogue_vessels$Vessel.ADFG.Number))

max(ulandings1)


df <- dlist[[1]] %>% filter(Vessel.ADFG.Number %in% uvessels)
df <- dlist[[1]] %>% select(CFEC.Permit.Serial.Number,CFEC.Permit.Fishery,CFEC.Permit.Sequence,Permit.Serial.Number,CFEC.Permit.Type,Vessel.ADFG.Number)
#test <- df %>% filter(CFEC.Permit.Serial.Number != Permit.Serial.Number | is.na(Permit.Serial.Number) | is.na(CFEC.Permit.Serial.Number))#one boat has missing permit numbers

df_ag <- aggregate(Permit.Serial.Number~Vessel.ADFG.Number, unique(df), FUN=list)
 

for (i in 1:length(df_ag$Permit.Serial.Number)){
  df_ag$permit_count[[i]] <- length(df_ag$Permit.Serial.Number[[i]])
}
dlist <- dlist[[1]]
test <- dlist[[1]] %>% filter(Permit.Serial.Number==99)

#going back to the home directory to get the permit and vessel data
for (i in 1975:2022){
  path <- paste("~/JoeData/permits/Permits", as.char(i),".csv", sep = "")
  permit <- read.csv(file.path(path))
  sum(is.na(permit$Vessel.ADFG))/length(permit$Vessel.ADFG)
}












#read in vessel data
path <- paste("./../../../2018 fish ticket data (revised 11-1-22).csv", sep = "")
landings2018 <- read.csv(file.path(path))
unique(landings2018$CFEC.Permit.Fishery)
landings2018 <- landings2018 %>% filter(substr(CFEC.Permit.Fishery, 1, 1) == "S")
table(landings2018$CFEC.Permit.Fishery)

landings2018 <- landings2018 %>% filter(substr(CFEC.Permit.Fishery, 3, 3) == "0" | substr(CFEC.Permit.Fishery, 3, 3) == "1")
table(landings2018$CFEC.Permit.Fishery)
table(landings2018$Period, exclude=NULL)#too many missing values to use across the board for period variable


landings2018$Date.Fishing.Began <- as.Date(as.character(landings2018$Date.Fishing.Began),format = "%Y%m%d")
landings2018$Date.Landed <- as.Date(as.character(landings2018$Date.Landed),format = "%Y%m%d")
landings2018$Week.Ending.Date <- as.Date(landings2018$Week.Ending.Date,format = "%Y-%m-%d")

landings2018 <- landings2018 %>% mutate(week = as.Date(cut(Date.Landed, "week")))#use the date landed

# two overall maps
g <- landings2018 %>% ggplot(aes(x = week))
overall_c <- g +geom_bar() + scale_x_date(date_labels = "%b")+xlab("Week")+ylab("Weekly Counts of Landings")#count of landings
overall_weight <- g +geom_bar(aes(weight = Pounds..Detail.)) + scale_x_date(date_labels = "%b")+xlab("Week")+ylab("Sum of landings (lbs)")#weight of landings
(overall_c + ggtitle("Number of landings")) / (overall_weight + ggtitle("Weight of landings"))


landings2018 %>% select(Date.Landed, CFEC.Permit.Fishery, Pounds..Detail.,Whole.Pounds..Detail.,CFEC.Value..Detail.) %>% view()

mypath <- paste("./../../../2018 fish ticket data (revised 11-1-22).csv", sep = "")

#function that plots the weekly landings for all the salmon fisheries by number of landings and total weight
plot_fisheries <- function(path,year){
  df <- read.csv(file.path(path))
  df <- df %>% filter(substr(CFEC.Permit.Fishery, 1, 1) == "S")
  df <- df %>% filter(substr(CFEC.Permit.Fishery, 3, 3) == "0" | substr(CFEC.Permit.Fishery, 3, 3) == "1")
  df$Date.Fishing.Began <- as.Date(as.character(df$Date.Fishing.Began),format = "%Y%m%d")
  df$Date.Landed <- as.Date(as.character(df$Date.Landed),format = "%Y%m%d")
  df$Week.Ending.Date <- as.Date(df$Week.Ending.Date,format = "%Y-%m-%d")
  df <- df %>% mutate(week = as.Date(cut(Date.Landed, "week")))
  
  fisheries <- unique(df$CFEC.Permit.Fishery)
  plotlist <- list()
  j=1
  for (i in fisheries) {
    g <- df %>% filter(CFEC.Permit.Fishery == i) %>% ggplot(aes(x = week))+geom_bar() + 
      scale_x_date(date_labels = "%b")+
      xlab("Week")+
      ylab(paste0("Weekly Counts of Landings "))+
      ggtitle(paste0(i, " (", year, ")"))
    plotlist[[j]] <- g
    j=j+1
  }
  for (i in fisheries) {
    g <- df %>% filter(CFEC.Permit.Fishery == i) %>% ggplot(aes(x = week))+geom_bar(aes(weight = Pounds..Detail.)) + 
      scale_x_date(date_labels = "%b")+
      xlab("Week")+
      ylab(paste0("Sum of landings (lbs)"))+
      ggtitle(paste0(i, " (", year, ")"))
    plotlist[[j]] <- g
    j=j+1
  }
  return(plotlist)
}
graphs <- plot_fisheries(path = mypath, year = 2018)
#counts
(graphs[[1]]+graphs[[2]]+graphs[[3]] + graphs[[4]]+graphs[[5]]+graphs[[6]] + graphs[[7]]+graphs[[8]]+graphs[[9]]) + plot_layout(ncol = 3)
(graphs[[10]]+graphs[[11]]+graphs[[12]] + graphs[[13]]+graphs[[14]] + graphs[[16]]+graphs[[17]]) + plot_layout(ncol = 3)
#weight
(graphs[[20]]+graphs[[21]]+graphs[[22]] + graphs[[23]]+graphs[[24]]+graphs[[25]] + graphs[[26]]+graphs[[27]]+graphs[[28]]) + plot_layout(ncol = 3)
(graphs[[29]]+graphs[[30]]+graphs[[31]] + graphs[[32]]+graphs[[33]] + graphs[[35]]+graphs[[36]]) + plot_layout(ncol = 3)



sum(is.na(landings2018$Vessel.ADFG.Number))# number of na values
sum(is.na(landings2018$CFEC.Permit.Vessel.ADFG.Number))# number of na values
sum(is.na(landings2018$AKR.Vessel.ADFG.Number))# number of na values

#vessels where the permit and the ticket data do not add up
landings2018 %>% filter(Vessel.ADFG.Number != CFEC.Permit.Vessel.ADFG.Number | is.na(CFEC.Permit.Vessel.ADFG.Number)) %>% select(Vessel.ADFG.Number,CFEC.Permit.Vessel.ADFG.Number,AKR.Vessel.ADFG.Number) %>% view()
#graph of landings for vessels where the permit and the ticket data do not add up
landings2018 %>% filter(Vessel.ADFG.Number != CFEC.Permit.Vessel.ADFG.Number | is.na(CFEC.Permit.Vessel.ADFG.Number)) %>% 
  ggplot(aes(x = week))+geom_bar() + 
  scale_x_date(date_labels = "%b")+
  xlab("Week")+
  ylab(paste0("Weekly Counts of Landings "))+
  ggtitle(paste0("Salmon vessels where the permit and the ticket data do not match"))

#tickets for each vessel
salmon_vessels <- landings2018 %>% distinct(Vessel.ADFG.Number) %>% select(Vessel.ADFG.Number) 

salmonboat_data <- read.csv(file.path(path))

salmonboat_data$Date.Fishing.Began <- as.Date(as.character(salmonboat_data$Date.Fishing.Began),format = "%Y%m%d")
salmonboat_data$Date.Landed <- as.Date(as.character(salmonboat_data$Date.Landed),format = "%Y%m%d")
salmonboat_data$Week.Ending.Date <- as.Date(salmonboat_data$Week.Ending.Date,format = "%Y-%m-%d")
salmonboat_data <- salmonboat_data %>% mutate(week = as.Date(cut(Date.Landed, "week")))

salmonboat_data <- salmonboat_data  %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% arrange(by_group = TRUE)
salmonboat_data  %>% arrange(desc(Vessel.ADFG.Number)) %>% select(Vessel.ADFG.Number, CFEC.Vessel.ADFG.Number, CFEC.Permit.Fishery, Permit.Fishery, Permit.Serial.Number) %>% view()
salmonboat_data <- salmonboat_data %>% mutate(permit_species = substr(CFEC.Permit.Fishery, 1, 1))#create species letter from the permit

("Salmon","MISC. SALTWATER FINFISH", "HALIBUT", "DUNGENESS CRAB", "ROCKFISH", "SABLEFISH", "SHRIMP", "LING COD", "TANNER CRAB", "HERRING", "SEA CUCUMBER",
  "OCTOPI/SQUID ","GEODUCK","FRESH WATER FISH","KING CRAB", "SEA URCHIN", "MISC. MARINE INVERTEBRATES","CLAMS")

salmon_tab <- salmonboat_data %>% ungroup() %>% group_by(CFEC.Species.Code) %>% summarise(n_landings = n())
salmon_tab_permit <- salmonboat_data %>% ungroup() %>% group_by(permit_species) %>% summarise(n_permits = n())

salmonboat_data %>% ungroup() %>% filter(permit_species != "S") %>%
  mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "Y", "ROCKFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "P", "SHRIMP")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "I", "LING COD")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "T", "TANNER CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "H", "HERRING")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "Q", "SEA CUCUMBER")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "O", "OCTOPI/SQUID")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "J", "GEODUCK")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "F", "FRESH WATER FISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "K", "KING CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "U", "SEA URCHIN")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "Z", "MISC. MARINE INVERTEBRATES")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "R", "CLAMS")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "G", "	HERRING ROE")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "L", "HERRING SPAWN")) %>% 
  arrange() %>% 
  ggplot(aes(permit_species))+geom_bar() +theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+
  ggtitle("Count of landings by boats with active salmon permits")


salmonboat_data %>% ungroup() %>% filter(permit_species != "S") %>%
  mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "Y", "ROCKFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "P", "SHRIMP")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "I", "LING COD")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "T", "TANNER CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "H", "HERRING")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "Q", "SEA CUCUMBER")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "O", "OCTOPI/SQUID")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "J", "GEODUCK")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "F", "FRESH WATER FISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "K", "KING CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "U", "SEA URCHIN")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "Z", "MISC. MARINE INVERTEBRATES")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "R", "CLAMS")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "G", "	HERRING ROE")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "L", "HERRING SPAWN")) %>% 
  arrange() %>% 
  ggplot(aes(permit_species))+geom_bar(aes(weight = CFEC.Value..Detail.)) +theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+
  ggtitle("Total Value of catch of landings for boats with active salmon permits (2018)")+
  ylab("$ in value of catch")

salmonboat_data %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
  mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
  ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species)) + 
  scale_x_date(date_labels = "%b")+
  xlab("Week")+
  ylab("Weekly Counts of Landings")+
  ggtitle("Distribution of Landings by Permit Species (permit linked with the fish ticket)")

salmonboat_data %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
  mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
  ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species, weight = CFEC.Value..Detail.)) + 
  scale_x_date(date_labels = "%b")+
  xlab("Week")+
  ylab("Total $ worth of landings")+
  ggtitle("Distribution of Value by Permit Species (permit linked with the fish ticket)")

###############
salmonboat_data %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
  ungroup() %>% group_by(Vessel.ADFG.Number) %>%  
  filter(any(permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
  ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species)) + 
  scale_x_date(date_labels = "%b")+
  xlab("Week")+
  ylab("Weekly Counts of Landings")+
  ggtitle("Distribution of Landings by Permit Species (only boats that fish multiple species)")

salmonboat_data %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
  ungroup() %>% group_by(Vessel.ADFG.Number) %>%  
  filter(any(permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D")) %>%  
  mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
  ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species, weight = CFEC.Value..Detail.)) + 
  scale_x_date(date_labels = "%b")+
  xlab("Week")+
  ylab("Total $ worth of landings")+
  ggtitle("Distribution of Value by Permit Species (only boats that fish multiple species)")
###############

###############
salmonboat_data  %>%
  ungroup() %>%  group_by(Vessel.ADFG.Number) %>% 
  filter(!any(permit_species!="S")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
  ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species)) + 
  scale_x_date(date_labels = "%b")+
  xlab("Week")+
  ylab("Weekly Counts of Landings")+
  ggtitle("Distribution of Landings by Permit Species (permit linked with the fish ticket)")

salmonboat_data  %>%
  ungroup() %>%  group_by(Vessel.ADFG.Number) %>% 
  filter(!any(permit_species!="S")) %>%   
  mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
  mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
  ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species, weight = CFEC.Value..Detail.)) + 
  scale_x_date(date_labels = "%b")+
  xlab("Week")+
  ylab("Total $ worth of landings")+
  ggtitle("Distribution of Value by Permit Species (permit linked with the fish ticket)")
###############


#get the salmon boats that also fish other species (not salmon)
mypath <- "./../../../2018 fish ticket data (revised 11-1-22).csv"

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
boatlist <- get_otherfishing(mypath)

plot_salmon_notsalmon_boats <- function(path, blist, year){
  df <- read.csv(file.path(path)) %>% group_by(Vessel.ADFG.Number) %>% filter(any(Vessel.ADFG.Number %in% blist)) %>% 
    arrange(by_group = TRUE) %>% 
    ungroup()#filter VESSELS (not permits) that have active salmon permits in 2018 (in the blist)
  {df <- df %>% mutate(Date.Fishing.Began = as.Date(as.character(Date.Fishing.Began),format = "%Y%m%d")) %>% 
      mutate(Date.Landed = as.Date(as.character(Date.Landed),format = "%Y%m%d")) %>% 
      mutate(Week.Ending.Date = as.Date(as.character(Week.Ending.Date),format = "%Y-%m-%d")) %>% 
      mutate(week = as.Date(cut(Date.Landed, "week"))) }#format time variables
  df <- df %>% mutate(permit_species = substr(CFEC.Permit.Fishery, 1, 1))#create species letter from the permit
  plot1 <- {df %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
    ungroup() %>% 
    mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
    mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
    mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
    mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
    mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
    ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species)) + 
    scale_x_date(date_labels = "%b")+
    xlab("Week")+
    ylab("Weekly Counts of Landings")+
    ggtitle(paste0("Distribution of Landings by Permit Species (only boats that fish multiple species) ", year))}
  plot2 <- {df %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
    ungroup() %>% 
    mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
    mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
    mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
    mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
    mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
    ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species, weight = CFEC.Value..Detail.)) + 
    scale_x_date(date_labels = "%b")+
    xlab("Week")+
    ylab("Total $ worth of landings")+
    ggtitle(paste0("Distribution of Value by Permit Species (only boats that fish multiple species) ", year))}
  return(plot1 / plot2) # requires 'patchwork' package
}

plot_salmon_notsalmon_boats(path = mypath, blist = boatlist$Vessel.ADFG.Number, year = 2018)
#now want to find out what the boats in boatwhat are these boats doing in the past
datalist <- list.files("./../../../")
datalist[27:30]


for (i in datalist[27:30]) {
  mypath <- paste0("./../../../",i)
  j <- substr(i, 1, 4)
  plot(plot_salmon_notsalmon_boats(path = mypath, blist = boatlist$Vessel.ADFG.Number, year = j))
}
plot_salmon_notsalmon_boats(path = mypath, blist = boatlist$Vessel.ADFG.Number, year = 2018)
identical(sort(test$Vessel.ADFG.Number),sort(boatlist$Vessel.ADFG.Number))


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

mypath <- "./../../../2018 fish ticket data (revised 11-1-22).csv"
boatlist_onlysalmon <- get_onlysalmon(path)

plot_salmon_notsalmon_boats <- function(path, blist, year){
  df <- read.csv(file.path(path)) %>% group_by(Vessel.ADFG.Number) %>% filter(any(Vessel.ADFG.Number %in% blist)) %>% 
    arrange(by_group = TRUE) %>% 
    ungroup()#filter VESSELS (not permits) that have active salmon permits in 2018 (in the blist)
  {df <- df %>% mutate(Date.Fishing.Began = as.Date(as.character(Date.Fishing.Began),format = "%Y%m%d")) %>% 
      mutate(Date.Landed = as.Date(as.character(Date.Landed),format = "%Y%m%d")) %>% 
      mutate(Week.Ending.Date = as.Date(as.character(Week.Ending.Date),format = "%Y-%m-%d")) %>% 
      mutate(week = as.Date(cut(Date.Landed, "week"))) }#format time variables
  df <- df %>% mutate(permit_species = substr(CFEC.Permit.Fishery, 1, 1))#create species letter from the permit
  plot1 <- {df %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
      ungroup() %>% 
      mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
      ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species)) + 
      scale_x_date(date_labels = "%b")+
      xlab("Week")+
      ylab("Weekly Counts of Landings")+
      ggtitle(paste0("Distribution of Landings by Permit Species (only boats that fish multiple species) ", year))}
  plot2 <- {df %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
      ungroup() %>% 
      mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
      ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species, weight = CFEC.Value..Detail.)) + 
      scale_x_date(date_labels = "%b")+
      xlab("Week")+
      ylab("Total $ worth of landings")+
      ggtitle(paste0("Distribution of Value by Permit Species (only boats that fish multiple species) ", year))}
  return(plot1 / plot2) # requires 'patchwork' package
}




plot_salmon_notsalmon_boats(path = mypath, blist = boatlist_onlysalmon$Vessel.ADFG.Number, year = 2018)

#now want to find out what the boats in boatlist are doing in the past
datalist <- list.files("./../../../")
datalist[27:30]
for (i in datalist[27:30]) {
  mypathtemp <- paste0("./../../../",i)
  j <- substr(i, 1, 4)
  plot(plot_salmon_notsalmon_boats(path = mypathtemp, blist = boatlist_onlysalmon$Vessel.ADFG.Number, year = j))
}




plot_boatport <- function(path, blist, year){
  df <- read.csv(file.path(path)) %>% group_by(Vessel.ADFG.Number) %>% filter(any(Vessel.ADFG.Number %in% blist)) %>% 
    arrange(by_group = TRUE) %>% 
    ungroup()#filter VESSELS (not permits) that have active salmon permits in 2018 (in the blist)
  {df <- df %>% mutate(Date.Fishing.Began = as.Date(as.character(Date.Fishing.Began),format = "%Y%m%d")) %>% 
      mutate(Date.Landed = as.Date(as.character(Date.Landed),format = "%Y%m%d")) %>% 
      mutate(Week.Ending.Date = as.Date(as.character(Week.Ending.Date),format = "%Y-%m-%d")) %>% 
      mutate(week = as.Date(cut(Date.Landed, "week"))) }#format time variables
  df <- df %>% mutate(permit_species = substr(CFEC.Permit.Fishery, 1, 1))#create species letter from the permit
  plot1 <- {df %>% filter(permit_species=="S"|permit_species=="B"|permit_species=="M"|permit_species=="C"|permit_species=="D") %>%
      ungroup() %>% 
      mutate(permit_species = replace(permit_species, permit_species == "M", "MISC. SALTWATER FINFISH")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "B", "HALIBUT")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "D", "DUNGENESS CRAB")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "S", "SALMON")) %>% 
      mutate(permit_species = replace(permit_species, permit_species == "C", "SABLEFISH")) %>% 
      ggplot(aes(x = week)) +geom_bar(aes(fill = permit_species)) + 
      scale_x_date(date_labels = "%b")+
      xlab("Week")+
      ylab("Weekly Counts of Landings")+
      ggtitle(paste0("Distribution of Landings by Permit Species (only boats that fish multiple species) ", year))}

  return(plot1) # requires 'patchwork' package
}


