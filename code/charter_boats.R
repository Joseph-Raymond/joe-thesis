vid <- read_csv("~/JoeData/vessels/vid")
vid$vessel_id <- vid$vessel_id
rm(list = ls())
source("code/myfunctions.R")
loadpacks()
getwd()
data_dir <- "./../../../"
#want to load data filter it by salmon boats then store that year of data in a list
datalist <- list.files(data_dir, pattern = "*.csv")

catch_data <- list()
j <- 1
for (i in datalist[1:length(datalist)]) {#15 starts at 2005
  temp <- read.csv(file.path(paste0(data_dir, i))) %>% 
    select(Batch.Year, Vessel.ADFG.Number, AKR.Vessel.Length, AKR.Vessel.ID, AKR.Coast.Guard.Number, AKR.Vessel.ADFG.Number, Tender.Vessel.ADFG.Number,CFEC.Coast.Guard.Number, Council.Vessel.ADFG.Number, ITO.Vessel.ADFG.Number) %>% distinct()
  catch_data[[j]] <- temp
  print(i)
  j <- j+1
}

for (i in 1:length(catch_data)) {
  catch_data[[i]]$AKR.Vessel.ADFG.Number <- as.character(catch_data[[i]]$AKR.Vessel.ADFG.Number)
  catch_data[[i]]$Vessel.ADFG.Number <- as.character(catch_data[[i]]$Vessel.ADFG.Number)
  catch_data[[i]]$Tender.Vessel.ADFG.Number <- as.character(catch_data[[i]]$Tender.Vessel.ADFG.Number)
  catch_data[[i]]$AKR.Vessel.ID <- as.character(catch_data[[i]]$AKR.Vessel.ID)
}
catch_data_temp <- bind_rows(catch_data)

vessel.years <- distinct(catch_data_temp)


vessel.years <- vessel.years %>% left_join(vid, by = c('Vessel.ADFG.Number'='vessel_id'), keep = TRUE) %>% filter(!is.na(vessel_id))
unique(vessel.years$vessel_id)
vessel.years <- vessel.years %>% select(-contains("vessel_id"))

vessel.years <- vessel.years %>% left_join(vid, by = c('AKR.Coast.Guard.Number'='vessel_id'), keep = TRUE) %>% filter(!is.na(vessel_id))
unique(vessel.years$vessel_id)
vessel.years <- vessel.years %>% select(-contains("vessel_id"))

vessel.years <- vessel.years %>% left_join(vid, by = c('CFEC.Coast.Guard.Number'='vessel_id'), keep = TRUE) %>% filter(!is.na(vessel_id))
unique(vessel.years$vessel_id)
vessel.years <- vessel.years %>% select(-contains("vessel_id"))

vessel.years <- vessel.years %>% left_join(vid, by = c('AKR.Vessel.ID'='vessel_id'), keep = TRUE) %>% filter(!is.na(vessel_id))
unique(vessel.years$vessel_id)
vessel.years <- vessel.years %>% select(-contains("vessel_id"))

save <- vessel.years %>% select(AKR.Vessel.Length, vessel_id) %>% filter(!is.na(vessel_id)) %>% distinct()

ggplot(save, aes(x=AKR.Vessel.Length))+geom_histogram(binwidth = 1, color="steelblue4", fill="white")

sum(is.na(vessel.years))
vid$vessel_id

summary(save$AKR.Vessel.Length)
