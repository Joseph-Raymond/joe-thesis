# data https://www.cfec.state.ak.us/plook/#permits
rm(list = ls())
#install packages not installed in your environment, some are included here in anticipation of future needs 28June2021
packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap', 
           'chron', 'zipcodeR', 'stringr')
lapply(packs, require, character.only = T)

setwd("C:/Users/josep/Box Sync/Thesis/data/CFEC/permits")


years <- 1978:2022
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

for(i in 1:length(myfiles)){
  #some of the files in the vessel data save the zip code as a string variable. this makes sure they are all numeric
  myfiles[[i]]$Zip.Code <- as.numeric(myfiles[[i]]$Zip.Code)
}

df <- myfiles[[1]]
for(i in 2:length(myfiles)){
  df <- rbind(df, myfiles[[i]]) 
}
sum(is.na(df$Zip.Code))
rm(myfiles)
mypath <- "C:/Users/josep/Box Sync/Thesis/data/clean_data/permit_clean.csv"
write.csv(df, mypath)
df <- read.csv("C:/Users/josep/Box Sync/Thesis/data/clean_data/permit_clean.csv")
summary(df$Vessel.ADFG)
df$Vessel.ADFG <- as.numeric(df$Vessel.ADFG)
table(is.na(df$Vessel.ADFG), df$Year)
df <- df %>% select(Year, Fishery, Vessel.ADFG)
df <- rename(df, ADFG.Number = Vessel.ADFG)
setwd("C:/Users/josep/Box Sync/Thesis/data/clean_data/")
setwd("C:/Users/Joseph Raymond/Box Sync/Thesis/data/clean_data/")
vessel_zip_geo_complete <- read.csv("vessels_clean_1978_2022.csv")
permit_vessel <- merge(df, vessel_zip_geo_complete, by=c("ADFG.Number", "Year"))
vessel_permit <- merge(vessel_zip_geo_complete, df, by=c("ADFG.Number", "Year"))


