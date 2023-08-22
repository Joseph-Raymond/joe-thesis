{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', 'xtable', 'corrplot')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work
getwd()

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
source("code/myfunctions.R")
#source("myfunctions.R")

data_dir <- "./../../../"
#want to load data filter it by salmon boats then store that year of data in a list
datalist <- list.files(data_dir, pattern = "*.csv")
permit_data <- list()
j <- 1
for (i in datalist[1:length(datalist)]) {#15 starts at 2005
  temp <- read.csv(file.path(paste0(data_dir, i))) %>%
    select(Pre.print.Ticket, Ticket.Type, Batch.Year, Vessel.ADFG.Number, contains("Home"), contains("Owner"), Date.Landed, Date.Fishing.Began, AKR.Vessel.Length, Port.Code, Port.Name, Port.State, Council.Port, Species.Code, Species.Common.Name, CFEC.Species.Code, CFEC.PACFIN.Species.Code, CFEC.Permit.Year:Permit.Serial.Number, BLEND.Target.Group:CFEC.Whole.Pounds..Detail.) %>% 
    mutate(permit.species = substr(CFEC.Permit.Fishery, 1, 1))
  catch_data[[j]] <- temp
  print(i)
  j <- j+1
}
#|CFEC.Species.Code=="B"|CFEC.Species.Code=="M"|CFEC.Species.Code=="C"|CFEC.Species.Code=="D"
for (i in 1:length(catch_data)) {
  catch_data[[i]]$Vessel.ADFG.Number <- as.numeric(catch_data[[i]]$Vessel.ADFG.Number)
  catch_data[[i]]$Permit.Serial.Number <- as.numeric(catch_data[[i]]$Permit.Serial.Number)
  catch_data[[i]]$Permit.Year.Sequence <- as.numeric(catch_data[[i]]$Permit.Year.Sequence)
  catch_data[[i]]$Pre.print.Ticket <- as.character(catch_data[[i]]$Pre.print.Ticket)
  catch_data[[i]]$CFEC.Vessel.Owner.Zip <- as.character(catch_data[[i]]$CFEC.Vessel.Owner.Zip)
  catch_data[[i]]$CFEC.Vessel.Owner.Historical.Zip <- as.character(catch_data[[i]]$CFEC.Vessel.Owner.Historical.Zip)
}#clean some variables before combining them all together
catch_data_temp <- bind_rows(catch_data)
save(catch_data_temp, file = "intermediate data/catch_data_temp.rdata")
#Now standardize the types of data from different years of data because some years are entered as different data types
for (i in 1:length(catch_data)) {
  catch_data[[i]]$Vessel.ADFG.Number <- as.numeric(catch_data[[i]]$Vessel.ADFG.Number)
  catch_data[[i]]$Permit.Year.Sequence <- as.numeric(catch_data[[i]]$Permit.Year.Sequence)
  catch_data[[i]]$Pre.print.Ticket <- as.character(catch_data[[i]]$Pre.print.Ticket)
  catch_data[[i]]$CFEC.Vessel.Owner.Zip <- as.character(catch_data[[i]]$CFEC.Vessel.Owner.Zip)
}#clean some variables before combining them all together
catch_data_temp <- bind_rows(catch_data) %>% group_by(Vessel.ADFG.Number)# %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% ungroup()#boats that have participated in salmon fishery in the sample
catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp["CFEC.Value..Detail."])] <- 0#fill the na's with 0
write.csv(catch_data_temp, "intermediate data/2005_2021_tickets.csv", row.names = TRUE)
catch_data_temp <- read.csv("intermediate data/2005_2021_tickets.csv")


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


