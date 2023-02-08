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
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc")
  new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(packs, require, character.only = T)}
getwd()

vessels <- read.csv(file.path("~/JoeData/clean_data/vessels_clean_1978_2022.csv"))
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
