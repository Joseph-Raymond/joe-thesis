rm(list = ls())
path2home <- ""#my available directory stops at "home" so this is just blank
datapath <- "/home/akfin/"
sub_dir <- "jraymond"
main_dir <- paste(path2home, datapath, sep = "")

# check if sub directory exists and if it doesn't then create it.
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

vessels <- read.csv(file.path("../../jraymond/JoeData/clean_data/vessels_clean_1978_2022.csv"))
vessels <- vessels %>% filter(Year==2018)

dlist <- load("data_2018.RData")

colnames(dlist[[1]])

uvessels <- sort(unique(vessels$ADFG.Number))
ulandings1 <- sort(unique(dlist[[1]]$Vessel.ADFG.Number))
ulandings2 <- sort(unique(dlist[[1]]$CFEC.Vessel.ADFG.Number))
ulandings3 <- sort(unique(dlist[[1]]$AKR.Vessel.ADFG.Number))
ulandings4 <- sort(unique(dlist[[1]]$CFEC.Permit.Vessel.ADFG.Number))

sum()
setequal(ulandings1, ulandings3)
setequal(dlist[[1]]$Vessel.ADFG.Number, dlist[[1]]$AKR.Vessel.ADFG.Number)

#use Vessel.ADFG.Number in the akfin data for the linking. Might need AKR.Vessel.ADFG.Number for other years

all(uvessels %in% ulandings1)
max(ulandings1)


df <- dlist[[1]] %>% filter(Vessel.ADFG.Number %in% uvessels)
df <- df %>% select(CFEC.Permit.Serial.Number,CFEC.Permit.Fishery,CFEC.Permit.Sequence,Permit.Serial.Number,CFEC.Permit.Type,Vessel.ADFG.Number)
df_ag <- aggregate(Permit.Serial.Number~Vessel.ADFG.Number, unique(df), FUN=list)
df_ag <- df_ag %>% mutate(nfisheries = length(Permit.Serial.Number))
df <- merge(x = df, y = uvessels)
df_ag$Permit.Serial.Number[[3]]
length(df_ag$Permit.Serial.Number[[3]])
class(df_ag$Permit.Serial.Number)

for (i in 1:length(df_ag$Permit.Serial.Number)){
  df_ag$nfisheries <- length(df_ag$Permit.Serial.Number[[i]])
}
