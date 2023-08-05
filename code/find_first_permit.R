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


data_dir <- "~/JoeData/permits/"
datalist <- list.files(data_dir, pattern = "*.csv")
permit_list <- list()

j=1
for(i in datalist[26:length(datalist)]){
  df <- read.csv(file.path(paste0(data_dir, i)))
  permit_list[[j]] <- df
  j=j+1
}
for (i in 1:length(permit_list)) {
  permit_list[[i]]$Vessel.ADFG <- as.numeric(permit_list[[i]]$Vessel.ADFG)
  permit_list[[i]]$Zip.Code <- as.numeric(permit_list[[i]]$Zip.Code)
}
permit_df <- permit_list %>% bind_rows()
#permit_df <- permit_df %>% filter(Permit.Type!="Moratorium"&Permit.Type!="Vessel Moratorium")
{
  data_dir <- "./../../../"
  #want to load data filter it by salmon boats then store that year of data in a list
  datalist <- list.files(data_dir, pattern = "*.csv")
  catch_data <- list()
  j <- 1
  for (i in datalist[10:length(datalist)]) {#15 starts at 2005
    temp <- read.csv(file.path(paste0(data_dir, i)), stringsAsFactors = FALSE) %>%
      select(Pre.print.Ticket, Ticket.Type, Batch.Year, Vessel.ADFG.Number, contains("Home"), contains("Owner"), Date.Landed, Date.Fishing.Began, AKR.Vessel.Length, Port.Code, Port.Name, Port.State, Council.Port, Species.Code, Species.Common.Name, CFEC.Species.Code, CFEC.PACFIN.Species.Code, CFEC.Permit.Year:Permit.Serial.Number, BLEND.Target.Group:CFEC.Whole.Pounds..Detail.) %>% 
      mutate(permit.species = substr(CFEC.Permit.Fishery, 1, 1))
    catch_data[[j]] <- temp
    print(i)
    j <- j+1
  }
  #|CFEC.Species.Code=="B"|CFEC.Species.Code=="M"|CFEC.Species.Code=="C"|CFEC.Species.Code=="D"
  for (i in 1:length(catch_data)) {
    catch_data[[i]]$Vessel.ADFG.Number <- as.numeric(catch_data[[i]]$Vessel.ADFG.Number)
    catch_data[[i]]$Permit.Year.Sequence <- as.numeric(catch_data[[i]]$Permit.Year.Sequence)
    catch_data[[i]]$Pre.print.Ticket <- as.character(catch_data[[i]]$Pre.print.Ticket)
    catch_data[[i]]$CFEC.Vessel.Owner.Zip <- as.character(catch_data[[i]]$CFEC.Vessel.Owner.Zip)
    catch_data[[i]]$Permit.Serial.Number <- as.numeric(catch_data[[i]]$Permit.Serial.Number)
  }
  catch_data_temp <- bind_rows(catch_data)# %>% group_by(Vessel.ADFG.Number)# %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% ungroup()
  catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp["CFEC.Value..Detail."])] <- 0#fill the na's with 0
}
catch <- catch_data_temp %>% group_by(Batch.Year,Vessel.ADFG.Number,Permit.Fishery) %>% summarise(Revenue = sum(CFEC.Value..Detail.))
permit_df <- permit_df %>% left_join(catch, by = c("Year"="Batch.Year", "Fishery"="Permit.Fishery", "Vessel.ADFG"="Vessel.ADFG.Number"), na_matches = "never")

permit_df <- permit_df %>% filter(Vessel.ADFG!=99999 & !is.na(Vessel.ADFG))
permit_df <- permit_df %>% ungroup() %>% group_by(Year, Fishery, Vessel.ADFG) %>%  mutate(did.fish = sum(sum(is.na(Revenue))>0))
permit_df <- permit_df %>% mutate(Fishery=as.factor(Fishery))

mylogit <- glm(did.fish ~ Year, data = permit_df, family = "binomial")



fishing_communities <- read.csv("~/JoeData/PortNames/fishing_communities.csv")
fishing_communities <- fishing_communities %>% group_by(COMMUNITY) %>% summarise(lat = first(LATITUDE), long = first(LONGITUDE))
test <- get_permit_cities(path = "~/JoeData/vessels/")














mypath <- "./../../../2015 fish ticket data (revised 04-14-17).csv"
boatlist_nss_2015 <- get_otherfishing(mypath)
boatlist_ss_2015 <- get_onlysalmon(mypath)# boats that
permpath <- "~/JoeData/permits/Permits2015.csv"
Permits2015 <- read.csv(file.path(permpath)) %>% mutate(permit_species = substr(Fishery, 1, 1))


#permit diversity among salmon specialists
{
  df <- Permits2015 %>% filter(Vessel.ADFG %in% boatlist_ss_2015$Vessel.ADFG.Number)
  permitcount <- df %>% filter(!is.na(Vessel.ADFG)) %>% group_by(Vessel.ADFG) %>%
    summarise(count = n_distinct(permit_species))
  Vessels2015 <- read.csv("~/JoeData/vessels/Vessels2015.csv")
  View(Vessels2015)
  permicount_inner<- permitcount %>% 
    inner_join(Vessels2015, by = join_by(Vessel.ADFG == ADFG.Number))
  permicount_inner$Length_cat <- cut(permicount_inner$Length,
                                     breaks=c(10, 20, 30, 40, 50, 60, 300),
                                     labels=c('10-20', '20-30', '30-40', '40-50', '50-60', '60+'))
  permitplot <- permicount_inner %>% ggplot(aes(x = count)) +geom_bar(aes(fill=Length_cat)) + ggtitle("Salmon Specialists Number of Unique Salmon Permits 2015")
  permitplot + ylab("Count of Vessels") + xlab("Number of Salmon Permits REGISTERED FOR VESSEL")
  
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
  catchplot + theme_bw() + ylab("Count of Vessels") + xlab("Number of Salmon Permits Landed")
}



# data https://www.cfec.state.ak.us/plook/#permits
rm(list = ls())

get.firstpermit <- function(path){
  #"major" permit ranking function picking the permit for each vessel that has the most revenue
  #narrow down the data set to three variables (1) vessel number, (2) Permit [a few other permit identifiers later], (3) landing value
  vars <- c("Vessel.ADFG.Number", "CFEC.Permit.Fishery", "CFEC.Value..Detail.")#redundant vector of variables
  df <- read.csv(file.path(path))
  df <- df %>% select(all_of(vars)) 
  df <- df %>% ungroup() %>%  group_by(Vessel.ADFG.Number, CFEC.Permit.Fishery) 
  df <- df %>% summarise(tot.revenue = sum(CFEC.Value..Detail.))
  #reduce on the permit variable based on the value of the summed revenue from the previous step
  df_test <- df %>% ungroup() %>% group_by(Vessel.ADFG.Number) %>% summarise(first.permit = CFEC.Permit.Fishery[which(tot.revenue==max(tot.revenue))])
  return(df_test)
}
fp.2015 <- get.firstpermit(mypath)



catchplot_fishery <- cathccount_inner %>% 
  inner_join(fp.2015, by = join_by(Vessel.ADFG.Number == Vessel.ADFG.Number)) %>%
  ungroup() %>%
  ggplot(aes(x = count)) +geom_bar(aes(fill=first.permit)) + ggtitle("Salmon Boats Number of Unique Salmon Permits LANDINGS 2015")
catchplot_fishery + theme_bw() + ylab("Count of Vessels") + xlab("Number of Salmon Permits Landed")
catchplot_fishery_salmon <- cathccount_inner %>% 
  inner_join(fp.2015, by = join_by(Vessel.ADFG.Number == Vessel.ADFG.Number)) %>%
  ungroup() %>%
  mutate(is.salmon = (substr(first.permit, 1, 1) == "S")) %>% 
  ggplot(aes(x = count)) +geom_bar(aes(fill=is.salmon)) + ggtitle("Salmon Boats Number of Unique Salmon Permits LANDINGS 2015")
catchplot_fishery_salmon + theme_bw() + ylab("Count of Vessels") + xlab("Number of Salmon Permits Landed")

catchplot_fishery <- cathccount_inner %>% 
  filter(count > 1) %>% 
  inner_join(fp.2015, by = join_by(Vessel.ADFG.Number == Vessel.ADFG.Number)) %>%
  ungroup() %>%
  ggplot(aes(x = count)) +geom_bar(aes(fill=first.permit)) + ggtitle("Salmon Boats Number of Unique Salmon Permits LANDINGS 2015 (more than 1 permit)")
catchplot_fishery + theme_bw() + ylab("Count of Vessels") + xlab("Number of Salmon Permits Landed")


permitplot_fishery <- permicount_inner %>% 
  inner_join(fp.2015, by = join_by(Vessel.ADFG == Vessel.ADFG.Number)) %>% 
  ungroup() %>%
  ggplot(aes(x = count)) +geom_bar(aes(fill=first.permit)) + ggtitle("Salmon Specialists Number of Unique Salmon Permits (registered in 2015)")
permitplot_fishery + theme_bw() + ylab("Count of Vessels") + xlab("Number of Salmon Permits Landed")

permitplot_fishery <- permicount_inner %>% 
  filter(count > 1) %>% 
  inner_join(fp.2015, by = join_by(Vessel.ADFG == Vessel.ADFG.Number)) %>% 
  ungroup() %>%
  ggplot(aes(x = count)) +geom_bar(aes(fill=first.permit)) + ggtitle("Salmon Specialists Number of Unique Salmon Permits (registered in 2015)(more than 1 permit)")
permitplot_fishery + theme_bw() + ylab("Count of Vessels") + xlab("Number of Salmon Permits Landed")
