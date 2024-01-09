{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', 'xtable', 'corrplot', 'data.table')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work
getwd()

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
source("code/myfunctions.R")

load("intermediate data/permit_clean.rdata")

sum(is.na(permit_clean$Zip.Code))
sum(is.na(permit_clean$Vessel.ADFG))
permit_clean %>% filter(is.na(Vessel.ADFG)) %>% count(Fishery) %>% View()
permit_clean %>% filter(is.na(Vessel.ADFG)) %>% View()

lookup <- c(Vessel.ADFG.Number = "Vessel.ADFG", Batch.Year = "Year")
permit_clean <- permit_clean %>% select(-"...1") %>% rename(all_of(lookup)) %>% mutate(Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number))

permit_clean <- permit_clean %>% mutate(CFEC.Permit.Serial.Number = as.integer(substr(Permit.Number,1,5)), CFEC.Permit.Check = substr(Permit.Number,6,6))

{
  load("intermediate data/catch_data_temp.rdata")
  #catch_data_temp %>% ungroup %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% nrow()#this filter would filter to jsut the boats that have fished salmon permit at some point
  catch_data_temp$Vessel.ADFG.Number[which(catch_data_temp$Vessel.ADFG.Number==62.39)] <- 62339#fix a typo in the data
  catch_data_temp <- catch_data_temp %>% filter(Vessel.ADFG.Number!=0) #8 missing values with vessel numbers == 0
  catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
  catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp["CFEC.Value..Detail."])] <- 0#fill the NA's in the value of catch with 0's
  catch_data_temp %>% select(CFEC.Permit.Serial.Number, CFEC.Permit.Sequence, Permit.Year.Sequence, Permit.Serial.Number) %>% head()
  
#CFEC.Permit.Serial.Number. Different serial number ranges are used depending upon the permit type (CFEC_PMT_TYPE). Mariculture (acquatic farm) is 10000-10499, experimental is 10500-10599, test fishing is 10600-10699, educational is 10700-10799, reservation is 10800-10899, hatchery cost recovery is 10900-10999, interim-use in an unlimited fishery is 11000-49999, interimentry in a limited fishery is 50000-54999, moratorium is 50000-54999, vessel moratorium is 50000-54999, permanent in a limited fishery is 55000-99999, vessel permanent in a vessel limited fishery is 55000-99999. This field will be blank if the permit information on the fish ticket could not be matched to the CFEC permit file. The variable "Permit.Serial.Number" is from the fish ticket data but there are some observations that can't be matched to the permit data 
  
}


{
  permit.annual.rev <- get.trip(catch_data_temp)
  permit.annual.rev$trip.duration <- as.numeric(permit.annual.rev$trip.length)+1
  permit.annual.rev <- permit.annual.rev %>% mutate(trip.duration=ifelse(trip.duration>0,trip.duration, NA))
  permit.annual.rev <- permit.annual.rev %>% group_by(Vessel.ADFG.Number, Batch.Year, CFEC.Permit.Fishery, CFEC.Permit.Serial.Number) %>% summarise(year.revenue=sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id), fishing.days = sum(trip.duration), unique.checks = n_distinct(CFEC.Permit.Check), CFEC.Permit.Check = first(CFEC.Permit.Check)) %>% mutate(revenue.per.trip = year.revenue/(num.trips*fishing.days))
  View(permit.annual.rev)
}
permit_clean <- permit_clean %>% filter(!is.na(Vessel.ADFG.Number), Batch.Year>=1991, Vessel.ADFG.Number!=0, Vessel.ADFG.Number!=99999)


permit_clean_join <- permit_clean %>% left_join(permit.annual.rev, join_by(Vessel.ADFG.Number, Batch.Year, CFEC.Permit.Serial.Number), na_matches = "never") %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% mutate(linked_permits = length(unique(CFEC.Permit.Serial.Number))) %>% ungroup() %>% filter(Fishery!="")
sum(is.na(permit_clean$unique.checks))

permit_clean_join %>% ungroup() %>% filter(is.na(CFEC.Permit.Check.y)) %>% count(linked_permits) %>% View()
permit_clean_join %>% ungroup() %>% filter(is.na(CFEC.Permit.Check.y)) %>% View()

test <- permit.annual.rev %>% group_by(Batch.Year, Vessel.ADFG.Number, CFEC.Permit.Fishery) %>% 
  summarise(annual.revenue = sum(year.revenue, na.rm = TRUE)) %>% 
  group_by(Batch.Year, Vessel.ADFG.Number) %>% 
  mutate(vessel.year.rev = sum(annual.revenue, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(revenue.share = annual.revenue/vessel.year.rev) %>% 
  mutate(Fishery = str_replace_all(string=CFEC.Permit.Fishery, pattern=" ", repl="")) %>%
  filter(Fishery!="") %>% select(-CFEC.Permit.Fishery) %>% 
  right_join(permit_clean_join, join_by(Vessel.ADFG.Number, Batch.Year, Fishery), na_matches = "never") #%>% 

test %>% filter(is.na(annual.revenue)) %>% count()
test %>% filter(is.na(unique.checks)) %>% count()#This number is less than the previous because there are some boat-years that have multiple of the same type of permit (like S03T) but they only use some of their permits (permits here meaning unique permit serial numbers)
test %>% filter(is.na(annual.revenue) & is.na(unique.checks)) %>% count()#Same as number of missing values for "annual.revenue". Thus, missing values for unique.checks are also missing for annual.revenue but not vice versa. This is because of what is described in the previous comment.
test <- test %>% mutate(did.fish = if_else(is.na(year.revenue),0,1)) %>% 
  group_by(Batch.Year, Vessel.ADFG.Number) %>% 
  mutate(num.dist.fishery = length(unique(Fishery)), num.dist.permit = length(unique(Permit.Number)), num.fished.permits = sum(did.fish), num.fished.fishery = n_distinct(Fishery[did.fish %in% c(1)])) %>% 
  ungroup()
table(test$num.fished.fishery, test$did.fish)

rm(test)


permit_clean_join %>% filter(!is.na(unique.checks)) %>% count()
permit_clean_join %>% filter(is.na(unique.checks)) %>% count()





{ 
  vessel_fishery_var <- trip.revenue.fishery %>% group_by(Vessel.ADFG.Number, CFEC.Permit.Fishery) %>% summarise(v.rpt = var(revenue.per.trip), v.fdays = var(fishing.days), v.ntrips = var(num.trips), v.rev = var(year.revenue), mean.rpt = mean(revenue.per.trip), mean.fdays = mean(fishing.days), mean.ntrips = mean(num.trips), mean.rev = mean(year.revenue), years.fished = n())
  sum(is.na(vessel_fishery_var$v.rev))
  vessel_fishery_var <- vessel_fishery_var %>% left_join(salmon_list, join_by(Vessel.ADFG.Number))  %>% mutate(permit.species = substr(first.permit, 1, 1)) %>% mutate(Vessel.ADFG.Number=as.integer(Vessel.ADFG.Number)) %>% filter(permit.species!="")
  vessel_fishery_var <- vessel_fishery_var %>% mutate(CV = sqrt(v.rev)/mean.rev) %>% mutate(permit.species = substr(CFEC.Permit.Fishery, 1, 1))
}



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

setwd("C:/Users/Joseph Raymond/Box Sync/Thesis/data/clean_data/")
vessel_zip_geo_complete <- read.csv("vessels_clean_1978_2022.csv")
permit_vessel <- merge(df, vessel_zip_geo_complete, by=c("ADFG.Number", "Year"))
vessel_permit <- merge(vessel_zip_geo_complete, df, by=c("ADFG.Number", "Year"))







load("intermediate data/catch_data_temp.rdata")
source("code/myfunctions.R")
unique(catch_data_temp$Batch.Year)
#catch_data_temp %>% ungroup %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% nrow()#this filter would filter to jsut the boats that have fished salmon permit at some point
catch_data_temp$Vessel.ADFG.Number[which(catch_data_temp$Vessel.ADFG.Number==62.39)] <- 62339#fix a typo in the data
catch_data_temp <- catch_data_temp %>% filter(Vessel.ADFG.Number!=0) #8 missing values with vessel numbers == 0
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp["CFEC.Value..Detail."])] <- 0#fill the NA's in the value of catch with 0's
