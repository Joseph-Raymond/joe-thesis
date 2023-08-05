rm(list = ls())
source("myfunctions.R")
loadpacks()
getwd()
data_dir <- "./../../../"
#want to load data filter it by salmon boats then store that year of data in a list
datalist <- list.files(data_dir, pattern = "*.csv")
{
  salmon_data <- list()
  j <- 1
  for (i in datalist[19:length(datalist)]) {#10 starts at 2000
    temp <- read.csv(file.path(paste0(data_dir, i))) %>% filter(substr(CFEC.Permit.Fishery, 1, 1)== "S") %>% select(Species.Code, Species.Common.Name, CFEC.Species.Code, Batch.Year, Vessel.ADFG.Number, Date.Landed, Date.Fishing.Began, AKR.Vessel.Length, CFEC.Permit.Fishery, Permit.Fishery, CFEC.Price.per.Pound, CFEC.Value..Detail., CFEC.Whole.Pounds..Detail.) %>% mutate(permit.species = substr(CFEC.Permit.Fishery, 1, 1))
    salmon_data[[j]] <- temp
    print(i)
    j <- j+1
  }
}

salmon_2009_2021 <- salmon_2000_2021
my_names <- c("CFEC.Value..Detail.", "permit.species", "CFEC.Permit.Fishery", "CFEC.Whole.Pounds..Detail.", "Vessel.ADFG.Number")
#ValueDF <- lapply(salmon_2000_2021, "[", , my_names)
test <- test %>% lapply(select, all_of(my_names))
for (i in 1:length(test)) {
  test[[i]]$Vessel.ADFG.Number <- as.numeric(test[[i]]$Vessel.ADFG.Number)
}
big <- bind_rows(test, .id = "column_label")
big$column_label <- as.numeric(big$column_label)+1999#year
vessel_year_df <- aggregate(big$CFEC.Value..Detail., by=list(Permit=big$CFEC.Permit.Fishery, Year = big$column_label, Vessel=big$Vessel.ADFG.Number), FUN=sum)
permit_variance <- aggregate(vessel_year_df$Catch.Value, by=list(Permit=vessel_year_df$Permit), FUN=sd, na.action = na.omit)
# create box and whisker plot

vessel_year_df <- rename(vessel_year_df, Catch.Value = x)
p <- vessel_year_df %>% filter(substr(Permit, 1, 4) != "S 77") %>% ggplot(aes(x = Permit, y = Catch.Value)) +
  geom_boxplot() +
  labs(x = "Permit", y = "Boat-Year Catch Value", title = "Boat-Year Catch Value distrubtion by permit from 2000 - 2021")  
p + theme(axis.text.x = element_text(angle = 65, vjust = 0.5, hjust=0.3))







{
  data_dir <- "./../../../"
  datalist <- list.files(data_dir, pattern = "*.csv")
  catch_data <- list()
  j <- 1
  for (i in datalist[15:length(datalist)]) {#15 starts at 2005
    temp <- read.csv(file.path(paste0(data_dir, i))) %>% filter(CFEC.Species.Code=="S"|CFEC.Species.Code=="B"|CFEC.Species.Code=="M"|CFEC.Species.Code=="C"|CFEC.Species.Code=="D") %>% select(Species.Code, Species.Common.Name, CFEC.Species.Code, Batch.Year, Vessel.ADFG.Number, Date.Landed, Date.Fishing.Began, AKR.Vessel.Length, CFEC.Permit.Fishery, Permit.Fishery, CFEC.Price.per.Pound, CFEC.Value..Detail., CFEC.Whole.Pounds..Detail., Permit.Serial.Number, CFEC.Permit.Holder.Status) %>% mutate(permit.species = substr(CFEC.Permit.Fishery, 1, 1))
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
  size_data <- new %>% select(AKR.Vessel.Length, Vessel.ADFG.Number) %>% group_by(Vessel.ADFG.Number) %>% summarise(AKR.Vessel.Length=median(AKR.Vessel.Length))
  size_data$cat_variable <- cut(size_data$AKR.Vessel.Length,
                                breaks=c(0,40, 60, 400),
                                labels=c('Small', 'Medium', 'Large'))
  #step 4: aggregate by permit-year (sums up binary observations which be default)
  #participate_data <- new %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% summarise(across(Permit.Fishery_B05B:Permit.Fishery_Y61A, sum, .names = "{.col}")) #this one aggs up to the vessel-year
  participate_data <- new %>% group_by(Batch.Year, Permit.Serial.Number, Vessel.ADFG.Number) %>% summarise(across(Permit.Fishery_B05B:Permit.Fishery_Y61A, sum, .names = "{.col}"))
  #participate_data <- participate_data %>% ungroup() %>% group_by(Batch.Year, Permit.Serial.Number) %>% summarise()
}#get the main dependent variable for logit regression: y = 1 if vessel fished a permit, and = 0 if it did not

{permpath <- "~/JoeData/permits/"
  permlist <- list.files(permpath, pattern = "*.csv")
  permit_data <- list()
  j <- 1
  for (i in permlist[31:length(permlist)]) {#31 starts at 2005
    temp <- read.csv(file.path(paste0(permpath, i)))
    permit_data[[j]] <- temp
    print(i)
    j <- j+1
  }
  for (i in 1:length(permit_data)) {
    permit_data[[i]]$Vessel.ADFG <- as.numeric(permit_data[[i]]$Vessel.ADFG)
    permit_data[[i]]$Zip.Code <- as.numeric(permit_data[[i]]$Zip.Code)
    print(sum(is.na(permit_data[[i]]$Vessel.ADFG)))
  }
  perm_data <- bind_rows(permit_data)
  perm_data <- perm_data %>% mutate(Permit.Serial.Number = as.numeric(substr(Permit.Number, 1,5))) %>% filter(Permit.Status=="Current Owner")#%>% group_by(Permit.Number, Vessel.ADFG) %>% summarise(across(Permit.Fishery_B05B:Permit.Fishery_Y61A, sum, .names = "{.col}"))
  perm_data <- perm_data %>% mutate(Fishery.Species=(substr(Fishery, 1,1))) %>% mutate(Fishery.Gear=(substr(Fishery, 2,3))) %>% mutate(Fishery.Region=(substr(Fishery, 4,4))) %>% filter(Fishery.Gear!="08" & Fishery.Gear!="13" &  Fishery.Gear!="77" & Fishery.Gear!="99") %>% mutate(Fishery.is.Salmon=ifelse(Fishery.Species=="S",1,0))
  
}#combine all permit data

vy_perm <- perm_data %>% ungroup() %>% group_by(Year, Vessel.ADFG) %>% summarise(vy_perm_count = n())
perm_test <- perm_data %>% ungroup() %>% left_join(vy_perm, by = join_by(Year == Year, Vessel.ADFG == Vessel.ADFG))

test <- participate_data %>% ungroup() %>% left_join(perm_test, by = join_by(Batch.Year == Year, Permit.Serial.Number == Permit.Serial.Number), relationship = "many-to-many", multiple = "first")

test_vy <- test %>% ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% summarise(vy_perm_count = n())
test <- test %>% ungroup() %>% left_join(test_vy, by = join_by(Batch.Year == Batch.Year, Vessel.ADFG.Number == Vessel.ADFG.Number))
test <- test %>% ungroup() %>% left_join(size_data, by = join_by(Vessel.ADFG.Number == Vessel.ADFG.Number))
test <- test %>% mutate(vessel.permit.ID = as.numeric(paste0(Vessel.ADFG.Number, Permit.Serial.Number))) %>% mutate(vessel.permit.year.ID = as.numeric(paste0(Vessel.ADFG.Number, Permit.Serial.Number, Batch.Year)))



length(unique(test$vessel.permit.year.ID))
n_occur <- data.frame(table(test$vessel.permit.year.ID))
n_occur[n_occur$Freq > 1,]
View(test[test$vessel.permit.year.ID %in% n_occur$Var1[n_occur$Freq > 1],])
View(test[91927:91931,])


test2 <- test %>% mutate(across(Permit.Fishery_B05B:Permit.Fishery_Y61A, ~ factor(ifelse(.x > 0,1,0)))) 
test2 <- test %>% mutate(across(Permit.Fishery_B05B:Permit.Fishery_Y61A), ~ factor(ifelse(.x > 0,1,0))) 








write.csv(test2, file.path(paste0(data_dir, "jraymond/output2.csv")))
table(mytemp$CFEC.Species.Code, useNA = "always")
mytemp %>% filter(CFEC.Species.Code=="M") %>% View()
permpath <- "~/JoeData/permits/Permits2015.csv"
Permits2015 <- read.csv(file.path(permpath)) %>% mutate(permit_species = substr(Fishery, 1, 1))
head(new)
temp <- permit_data[[1]] %>% filter(is.na(Vessel.ADFG))
table(temp$Fishery, useNA = "always")
sum(is.na(test$Vessel.ADFG))
perm_data %>% filter(Permit.Sequence>1) %>% summarise(n=n())
sum(test$Vessel.ADFG.Number==test$Vessel.ADFG)
perm_data %>% filter(Permit.Number=="56298Z") %>% View()
table(perm_data$Permit.Status, useNA = "always")
