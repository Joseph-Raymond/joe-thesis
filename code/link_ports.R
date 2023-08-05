rm(list = ls())

source("code/myfunctions.R")
loadpacks()
getwd()
data_dir <- "./../../../"
#want to load data filter it by salmon boats then store that year of data in a list
datalist <- list.files(data_dir, pattern = "*.csv")
catch_data_temp <- read.csv(file.path(paste0(data_dir, datalist[20]))) %>% select(Port.Code, Port.Name, Port.State, Council.Port)
catch_data_temp <- catch_data_temp %>% distinct()
port_names <- read.delim("~/JoeData/PortNames/port_names", header=FALSE)

library(sjmisc)
catch_data_temp$loc.name <- NA
for (i in 1:nrow(catch_data_temp)) {
  for (j in 1:nrow(port_names)){
    if(str_contains(catch_data_temp$Port.Name[i], port_names$V1[j], ignore.case = TRUE)){
      catch_data_temp$loc.name[i] <- port_names$V1[j]
    }
  }
}


write.csv(catch_data_temp, file.path(paste0(data_dir, "jraymond/ports.csv")))
















{
  data_dir <- "./../../../"
  datalist <- list.files(data_dir, pattern = "*.csv")
  catch_data <- list()
  j <- 1
  for (i in datalist[length(datalist)-7]) {#15 starts at 2005
    temp <- read.csv(file.path(paste0(data_dir, i))) %>% filter(CFEC.Species.Code=="S"|CFEC.Species.Code=="B"|CFEC.Species.Code=="M"|CFEC.Species.Code=="C"|CFEC.Species.Code=="D") %>% select(Species.Code, Species.Common.Name, CFEC.Species.Code, Batch.Year, Vessel.ADFG.Number, Date.Landed, Date.Fishing.Began, AKR.Vessel.Length, CFEC.Permit.Fishery, Permit.Fishery, CFEC.Price.per.Pound, CFEC.Value..Detail., CFEC.Whole.Pounds..Detail., Permit.Serial.Number, CFEC.Permit.Holder.Status, Port.Code, Port.Name, Council.Port) %>% mutate(permit.species = substr(CFEC.Permit.Fishery, 1, 1))
    catch_data[[j]] <- temp
    print(i)
    j <- j+1
  }
  for (i in 1:length(catch_data)) {
    catch_data[[i]]$Vessel.ADFG.Number <- as.numeric(catch_data[[i]]$Vessel.ADFG.Number)
  }
  new <- bind_rows(catch_data)
  #step 1: get all the boats that fished salmon, halibut, misc saltwater finfish, sable fish and dungeness crab
  boats <- unique(new['Vessel.ADFG.Number'])
  #step 2: get a list of all unique permits from these boats
  upermits <- unique(new['Permit.Fishery']) %>% filter(substr(Permit.Fishery,1,1) != "9")#filter out the bad oberservations that are not matched
  #step 3: create dummy variable for each permit
  new <- new  %>% filter(substr(Permit.Fishery,1,1) != "9") %>% dummy_cols(select_columns = 'Permit.Fishery')
  pv <- new %>% select(Permit.Serial.Number, Batch.Year, Vessel.ADFG.Number)
  size_data <- new %>% group_by(Council.Port) %>% summarise(mean.Length=mean(AKR.Vessel.Length),sd.Length=sd(AKR.Vessel.Length))
  write.csv(size_data, file.path(paste0(data_dir, "jraymond/port_stats.csv")))

}


