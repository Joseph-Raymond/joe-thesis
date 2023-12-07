{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', 'xtable', 'corrplot', 'data.table')
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

library(readr)
permit_clean <- read_csv("~/JoeData/clean_data/permit_clean.csv")
save(permit_clean, file = "intermediate data/permit_clean.rdata")

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





library(readr)
permit_clean <- read_csv("~/JoeData/clean_data/permit_clean.csv")
save(permit_clean, file = "intermediate data/permit_clean.rdata")

load("intermediate data/permit_clean.rdata")

sum(is.na(permit_clean$Zip.Code))

lookup <- c(Vessel.ADFG.Number = "Vessel.ADFG", Batch.Year = "Year")
permit_clean <- permit_clean %>% select(-"...1") %>% rename(all_of(lookup))
permit.portfolio <- permit_clean %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% summarise(permit.list = list(unique(Fishery)), permit.numbers = list(unique(Permit.Number)), permit.sequence = list(unique(Permit.Sequence)), num.entries = n(), max.permseq = max(Permit.Sequence)) #length of permit.list lists and permit.numbers list is equal for All observation

salmon_permits <- permit_clean %>% select(Fishery) %>% filter(substr(Fishery,1,1)=="S") %>% distinct()
permit.portfolio <- permit.portfolio %>% group_by(Vessel.ADFG.Number) %>% filter(any(salmon_permits$Fishery %in% permit.list), Vessel.ADFG.Number!=0) %>% mutate(Vessel.ADFG.Number=as.integer(Vessel.ADFG.Number))#limits to boats that have had at least one salmon permit in their lifetime
#rm(permit_clean)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#get the number of vessels registered to a owner (file number)
{
  #overall
  permit_clean %>% group_by(File.Number, Vessel.ADFG.Number) %>% summarise(mode.fishery = Mode(Fishery), count = n(), num.fisheries = length(unique(Fishery))) %>% View()
  #in a year
  owner.boats <- permit_clean %>% group_by(File.Number, Batch.Year) %>% filter(any(substr(Fishery,1,1)=='S')) %>%#filter out owners with no boats that have a salmon permit
    filter(!is.na(Vessel.ADFG.Number), Vessel.ADFG.Number!=99999, Vessel.ADFG.Number!=0) %>% 
    group_by(File.Number, Batch.Year) %>% 
    mutate(boats.unique = length(unique(Vessel.ADFG.Number))) %>% #number of boats
    mutate(salmon.boats.unique = length(unique(Vessel.ADFG.Number[substr(Fishery,1,1)=='S']))) %>% # number of salmon boats
    group_by(Fishery, Batch.Year) %>% 
    summarise(multi.owners=length(unique(File.Number[boats.unique>1])) , multi.owners.perc=length(unique(File.Number[boats.unique>1]))/length(unique(File.Number)), n.owners=length(unique(File.Number)))
  
  P1 <- owner.boats %>% filter(Fishery %like% "S03") %>% 
    filter(Batch.Year>1990 & Batch.Year<2022) %>% 
    ggplot(aes(x=Batch.Year, y=multi.owners, color=Fishery)) +
    geom_line()+
    labs(title = "Number of owners (unique file numbers) with multiple boats", x=("Year"), y=("Number of owners"))+
    geom_line(size = 0.7)+
    scale_color_brewer(palette = "Accent")
  P2 <- owner.boats %>% filter(Fishery %like% "S03") %>% 
    filter(Batch.Year>1990 & Batch.Year<2022) %>% 
    ggplot(aes(x=Batch.Year, y=multi.owners.perc, color=Fishery)) +
    geom_line()+
    labs(title = "Percentage of owners (unique file numbers) with multiple boats", x=("Year"), y=("ratio of multi-boat owners"))+
    geom_line(size = 0.7)+
    scale_color_brewer(palette = "Accent")
}


{
  #in a year
  owner.boat.catch <- catch_data_temp %>% group_by(CFEC.Vessel.Owner.Filing.Number, Batch.Year) %>% filter(any(substr(CFEC.Permit.Fishery,1,1)=='S')) %>%#filter out owners with no boats that have a salmon permit
    filter(!is.na(Vessel.ADFG.Number), Vessel.ADFG.Number!=99999, Vessel.ADFG.Number!=0) %>% 
    group_by(CFEC.Vessel.Owner.Filing.Number, Batch.Year) %>% 
    mutate(boats.unique = length(unique(Vessel.ADFG.Number))) %>% #number of boats
    mutate(salmon.boats.unique = length(unique(Vessel.ADFG.Number[substr(CFEC.Permit.Fishery,1,1)=='S']))) %>% # number of salmon boats
    group_by(CFEC.Permit.Fishery, Batch.Year) %>% 
    summarise(multi.owners=length(unique(CFEC.Vessel.Owner.Filing.Number[boats.unique>1])) , multi.owners.perc=length(unique(CFEC.Vessel.Owner.Filing.Number[boats.unique>1]))/length(unique(CFEC.Vessel.Owner.Filing.Number)), n.owners=length(unique(CFEC.Vessel.Owner.Filing.Number)))
  
  P3 <- owner.boat.catch %>% filter(CFEC.Permit.Fishery %like% "S 03") %>% 
    filter(Batch.Year>1990 & Batch.Year<2022) %>% 
    ggplot(aes(x=Batch.Year, y=multi.owners, color=CFEC.Permit.Fishery)) +
    geom_line()+
    labs(title = "Number of owners (unique file numbers) with multiple boats (CATCH DATA)", x=("Year"), y=("Number of owners"))+
    geom_line(size = 0.7)+
    scale_color_brewer(palette = "Accent")
  P4 <- owner.boat.catch %>% filter(CFEC.Permit.Fishery %like% "S 03") %>% 
    filter(Batch.Year>1990 & Batch.Year<2022) %>% 
    ggplot(aes(x=Batch.Year, y=multi.owners.perc, color=CFEC.Permit.Fishery)) +
    geom_line()+
    labs(title = "Percentage of owners (unique file numbers) with multiple boats (CATCH DATA)", x=("Year"), y=("Number of owners"))+
    geom_line(size = 0.7)+
    scale_color_brewer(palette = "Accent")
  
  
  
  catch_data_temp %>% filter(CFEC.Permit.Fishery == "S 03T") %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% summarise(permit.serial.numbers = length(unique(CFEC.Permit.Serial.Number)) , cfec.vessel.owners = length(unique(CFEC.Vessel.Owner.Filing.Number))) %>% group_by(Batch.Year) %>% mutate(permit.stacked = (permit.serial.numbers>1)) %>%
    filter(Batch.Year>1990 & Batch.Year<2022) %>% group_by(Batch.Year) %>% summarise(permit.stack.ratio = sum(permit.stacked)/n()) %>% 
    ggplot(aes(x=Batch.Year, y=permit.stack.ratio)) +
    geom_line()+
    labs(title = "percentage of boats with multiple BB permit serial numbers in a year (CATCH DATA)", x=("Year"), y=("multiple-permit boat ratio"))+
    scale_color_brewer(palette = "Accent")
}
#fishing.portfolio from catch data
fishing.portfolio <- trip.revenue.fishery %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise(catch.portfolio = list(c(gsub(" ","",CFEC.Permit.Fishery))), catch.revenue = list(c(year.revenue)))#use gsub to remove spaces so that it matches with the permit data
#join the permits
permit.portfolio <- permit.portfolio %>% ungroup() %>% left_join(fishing.portfolio, join_by(Vessel.ADFG.Number, Batch.Year))
permit.portfolio <- permit.portfolio %>% mutate(permit.fished = permit.list)



test <- permit.portfolio <- permit.portfolio %>%
  mutate(permit.fished = map2(permit.fished, catch.portfolio, ~ map(.x[[1]], ~ (.x %in% .y[[1]]))))

get.permit.result <- function(df, permitname){
  df <- df %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% filter(any(permit.list %like% permitname)) %>% mutate(permit.fished = ifelse(catch.portfolio %like% permitname, TRUE, FALSE)) %>% ungroup()
  return(df)
}

drift.gillnet <- salmon_permits %>% filter(Fishery %like% "03")
drift.gillnet <- drift.gillnet$Fishery
test <- get.permit.result(permit.portfolio, "S03T") %>% mutate(permit = "S03T")
test <- test[0,]
for (i in drift.gillnet){
  temp <- get.permit.result(permit.portfolio, i) %>% mutate(permit = i)
  test <- rbind(test,temp)
}
rm(temp)

test %>% group_by(Batch.Year, permit) %>% 
  summarise(ratio = sum(permit.fished)/n()) %>% 
  filter(Batch.Year>1990 & Batch.Year<2022) %>% 
  ggplot(aes(x=Batch.Year, y=ratio, color=permit)) +
  geom_line()+
  labs(title = "Proportion of Permits Fished: Drift Gillnet Salmon Fisheries", x=("Year"), y=("Proportion of Permits Fished That Recorded a Poditve Landing"))+
  geom_line(size = 0.7)+
  scale_color_brewer(palette = "Accent")

test %>% group_by(Batch.Year, permit) %>% 
  summarise(n.permitnoats = n()) %>% 
  filter(Batch.Year>1990 & Batch.Year<2022) %>% 
  ggplot(aes(x=Batch.Year, y=n.permitnoats, color=permit)) +
  geom_line()+
  labs(title = "Number of Unique Permited Boats: Drift Gillnet Salmon Fisheries", x=("Year"), y=("#boats"))+
  geom_line(size = 0.7)+
  scale_color_brewer(palette = "Accent")

test %>% ungroup() %>% left_join %>% filter()%>%View()


test %>% ungroup() %>% group_by(Batch.Year, permit) %>% summarise(mean.num.perm = mean(num.entries)) %>%
  filter(Batch.Year>1990 & Batch.Year<2022) %>% 
  ggplot(aes(x=Batch.Year, y=mean.num.perm, color=permit)) +
  geom_line()+
  labs(title = "Mean Number of Unique Permits of Boats: Drift Gillnet Salmon Fisheries", x=("Year"), y=("mean number of permits"))+
  geom_line(size = 0.7)+
  scale_color_brewer(palette = "Accent")

test %>% ungroup() %>% group_by(Batch.Year, permit, Vessel.ADFG.Number)%>% mutate(unfished.permits = num.entries-length(catch.portfolio[[1]])) %>% 
  ungroup() %>% group_by(Batch.Year, permit)%>% 
  summarise(mean.unfished.perm = mean(unfished.permits)) %>%
  filter(Batch.Year>1990 & Batch.Year<2022) %>% 
  ggplot(aes(x=Batch.Year, y=mean.unfished.perm, color=permit)) +
  geom_line()+
  labs(title = "Mean Number of unfished permits: Drift Gillnet Salmon Fisheries", x=("Year"), y=("# of permits"))+
  geom_line(size = 0.7)+
  scale_color_brewer(palette = "Accent")


trip.revenue.fishery %>% group_by(Batch.Year, CFEC.Permit.Fishery) %>% summarise(year.revenue = mean(year.revenue, na.rm = TRUE)) %>% 
  filter(Batch.Year>1990 & Batch.Year<2022) %>% filter(gsub(" ","",CFEC.Permit.Fishery) %in% drift.gillnet) %>% 
  ggplot(aes(x=Batch.Year, y=year.revenue, color=CFEC.Permit.Fishery)) +
  geom_line()+
  labs(title = "Mean boat revenue: Drift Gillnet Salmon Fisheries", x=("Year"), y=("$"))+
  geom_line(size = 0.7)+
  scale_color_brewer(palette = "Accent")
#Then for each element look to see if that element is in the fishing portfolio list


vessel_fishery_var %>% group_by(CFEC.Permit.Fishery) %>% summarise(year.revenue = mean(v.rev, na.rm = TRUE)) %>% filter(gsub(" ","",CFEC.Permit.Fishery) %in% drift.gillnet) %>% View()



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


