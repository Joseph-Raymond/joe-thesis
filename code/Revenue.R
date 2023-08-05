rm(list = ls())
source("code/myfunctions.R")
loadpacks()
getwd()
data_dir <- "./../../../"
#want to load data filter it by salmon boats then store that year of data in a list
datalist <- list.files(data_dir, pattern = "*.csv")
catch_data <- list()
j <- 1
for (i in datalist[15:length(datalist)]) {#15 starts at 2005
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
  catch_data[[i]]$Permit.Year.Sequence <- as.numeric(catch_data[[i]]$Permit.Year.Sequence)
  catch_data[[i]]$Pre.print.Ticket <- as.character(catch_data[[i]]$Pre.print.Ticket)
  catch_data[[i]]$CFEC.Vessel.Owner.Zip <- as.character(catch_data[[i]]$CFEC.Vessel.Owner.Zip)
}
catch_data_temp <- bind_rows(catch_data) %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% ungroup()
catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp["CFEC.Value..Detail."])] <- 0#fill the na's with 0


{
  
  
  
  temp <- read.csv(file.path(paste0(data_dir, datalist[15])))
source("code/myfunctions.R")
test <- split_permit(catch_data_temp) %>% select(Permit.Species, Permit.Gear, Permit.Region)
catch_data_temp <- split_permit(catch_data_temp)
temp <- catch_data_temp %>% group_by(Vessel.ADFG.Number) %>%  filter(any(Permit.Fishery=="S04T" |Permit.Fishery=="S04W"| Permit.Fishery=="S03E"| Permit.Fishery=="S03T")) %>% ungroup()


temp <- temp %>% group_by(Vessel.ADFG.Number, Batch.Year, Permit.Fishery) %>% summarise(vlength = mean(AKR.Vessel.Length), gear=first(Permit.Gear), tr = sum(CFEC.Whole.Pounds..Detail.))
temp <- dummy_cols(temp, select_columns=c("Permit.Fishery"))#
test <- temp %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise_at(vars(starts_with("Permit.Fishery_")), sum, na.rm=TRUE)


unique(temp$Vessel.ADFG.Number)
temp$Permit.Gear <- as.numeric(temp$Permit.Gear)
unique(temp$Permit.Gear)

reduce <- catch_data_temp %>% group_by(Vessel.ADFG.Number, Permit.Fishery, Batch.Year) %>% summarise(rev = sum(CFEC.Value..Detail.))
test <- dummy_cols(reduce, select_columns=c("Permit.Fishery")) %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise_at(vars(starts_with("Permit.Fishery_")), sum, na.rm=TRUE)
xmat <- test_level %>% select(starts_with("Permit.Fishery_S"))
xmat <- xmat[rowSums(xmat[])>0,]

test_level <- test %>% select(contains("Permit.Fishery_"))
test_weighted <- test %>% mutate(across(contains("Permit.Fishery_"), ~ .x*rev, .names = "rev_{.col}")) %>% select(-starts_with("Permit.Fishery_"))

xmat <- test_weighted %>% select(starts_with("rev_"))
cor_mat <- rcorr(as.matrix(xmat), type = "spearman")
ggcorrplot(cor_mat$r, hc.order = TRUE, outline.col = "white")
+ labs(title = "Correlation Matrix", subtitle="All variables depict existence in individual gear specifcation except for Length, Fishing, and Charter")
# Save an object to a file
saveRDS(catch_data_temp, file = "my_data.rds")
# Restore the object
readRDS(file = "my_data.rds")}

temp <- temp %>% filter(!is.na(Permit.Gear))
cols <- paste0("has.", as.character(unique(temp$Permit.Gear)))
temp %>% group_by(Vessel.ADFG.Number) %>% mutate(across(all_of(cols), ~ contains(.x == ), .names = {.col}))

  
#var_names <- read.csv(file.path(paste0(data_dir, datalist[20]))) %>% names()
#catch_data_temp <- read.csv(file.path(paste0(data_dir, datalist[20]))) %>% select(Pre.print.Ticket, Ticket.Type, Batch.Year, Vessel.ADFG.Number, contains("Home"), contains("Owner"), Date.Landed, Date.Fishing.Began, AKR.Vessel.Length, Port.Code, Port.Name, Port.State, Council.Port, Species.Code, Species.Common.Name, CFEC.Species.Code, CFEC.PACFIN.Species.Code, CFEC.Permit.Year:Permit.Serial.Number, BLEND.Target.Group:CFEC.Whole.Pounds..Detail.)

catch_data_temp <- get.trip(catch_data_temp)
catch_data_temp$trip.duration <- as.numeric(catch_data_temp$trip.length)+1  

#vessel_revenue <- catch_data_temp %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise(Year.Revenue=sum(CFEC.Value..Detail.))
trip.revenue <- catch_data_temp %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise(year.revenue=sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id)) %>% mutate(revenue.per.trip = year.revenue/num.trips)


get.primeport <- function(df){
  df <- df %>% 
    group_by(Vessel.ADFG.Number, Council.Port, Batch.Year) %>% 
    summarise(port.revenue = sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id)) %>% 
    mutate(revenue.per.trip = port.revenue/num.trips)
  #reduce on the permit variable based on the value of the summed revenue from the previous step
  df_test <- df %>% ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% 
    mutate(max.rev = max(port.revenue)) %>% 
    filter(port.revenue==max(port.revenue), port.revenue>0) %>% 
    select(Vessel.ADFG.Number, Council.Port, Batch.Year) %>% 
    rename(prime.port=Council.Port)
        #summarise(max.rev = max(port.revenue))
  df <- df %>% ungroup() %>% left_join(df_test, by = join_by(Batch.Year, Vessel.ADFG.Number)) %>% mutate(is.primeport = (Council.Port==prime.port))
  return(df)
}

df.primmeport <- get.primeport(catch_data_temp)

test <- df.primmeport %>% mutate(prime.port=0) 
which(duplicated(test))

df.primmeport <- df.primmeport %>% ungroup() %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% mutate(year.revenue = sum(port.revenue)) %>% ungroup()

vessels <- data.frame(unique(catch_data_temp$Vessel.ADFG.Number)) %>% rename(Vessel.ADFG.Number = unique.catch_data_temp.Vessel.ADFG.Number.)
vessels <- vessels %>% left_join(catch_data_temp, by = join_by(Vessel.ADFG.Number), relationship = "one-to-many", multiple = "first") %>% select(Vessel.ADFG.Number, AKR.Vessel.Length)
df.primmeport <- df.primmeport %>% ungroup() %>% left_join(vessels, by = join_by(Vessel.ADFG.Number), relationship = "many-to-one")

nyears <- length(unique(df.primmeport$Batch.Year))
ID <- 2005:2021

p <- df.primmeport %>% ungroup() %>%  filter(!is.na(is.primeport)) %>% ggplot(aes(x = is.primeport))+
  geom_bar(aes(weight = num.trips/nyears), fill="steelblue")+
  ylab("Count of Landings Made at Port")
p
p1 <- df.primmeport %>% ungroup() %>%  filter(!is.na(is.primeport)) %>% ggplot(aes(x = is.primeport))+
  geom_bar(aes(weight = port.revenue/nyears), fill="steelblue")+
  ylab("Total Revenue Landed at Port")
p1


p <- df.primmeport %>% ungroup() %>% filter(!is.na(is.primeport)) %>% ggplot(aes(x = Batch.Year, fill = is.primeport))+
  geom_bar(aes(weight = num.trips), position = position_dodge())+
  ylab("Count of Landings Made at Port")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1))+
  scale_x_continuous("Year", labels = as.character(ID), breaks = ID)+ labs(title = "Number of landings at primary port")
p

p1 <- df.primmeport %>% ungroup() %>% filter(!is.na(is.primeport)) %>% ggplot(aes(x = Batch.Year, fill = is.primeport))+
  geom_bar(aes(weight = port.revenue), position = position_dodge())+
  ylab("Total Revenue Landed at Port")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1))+
  scale_x_continuous("Year", labels = as.character(ID), breaks = ID)+ labs(title = "Revenue landed at primary port")
p1


port_g <- df.primmeport %>% ungroup() %>% filter(!is.na(is.primeport)) %>% ggplot(aes(x = reorder(Council.Port, -num.trips), fill = is.primeport))+
  geom_bar(aes(weight = num.trips/nyears), position = position_dodge())+
  ylab("Count of Landings Made at Port per Year")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
port_g

port_g1 <- df.primmeport %>% ungroup() %>% filter(!is.na(is.primeport)) %>% ggplot(aes(reorder(Council.Port, -num.trips), fill = is.primeport))+
  geom_bar(aes(weight = port.revenue/nyears), position = position_dodge())+
  ylab("Total Revenue Landed at Port per Year")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
port_g1+ labs(title = "Revenue landed at primary port")

productivty <- df.primmeport %>% ungroup() %>% filter(!is.na(is.primeport)) %>% group_by(Batch.Year, is.primeport) %>% summarise(mean.trip.revenue = mean(revenue.per.trip), mean.length = mean(AKR.Vessel.Length)) %>% ggplot(aes(x = Batch.Year, y = mean.trip.revenue, fill = is.primeport))+
  geom_bar(stat = "identity", position = position_dodge())+
  ylab("Mean Revenue Per Trip")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  scale_x_continuous("Year", labels = as.character(ID), breaks = ID)+ labs(title = "Productivity by Primary Port")
productivty

df.primmeport$size.cat <- cut(df.primmeport$AKR.Vessel.Length,
                              breaks=c(0,10,20,30,40,50,60,70,80,90,100, 400),
                              labels=c('0-10','10-20','20-30','30-40','40-50','50-60','60-70','70-80','80-90','90-100','100+'))

productivty <- df.primmeport %>% ungroup() %>% filter(!is.na(is.primeport)) %>% group_by(size.cat, is.primeport) %>% summarise(mean.trip.revenue = mean(revenue.per.trip), mean.length = mean(AKR.Vessel.Length)) %>% ggplot(aes(size.cat))+
  geom_bar(aes(weight = mean.trip.revenue, fill = is.primeport), position = position_dodge())+
  ylab("Mean Revenue Per Trip")+xlab("Boat length")+labs(title = "Productivity of Primary/Non-Primary Port landings by Vessel Length")
productivty

tr <- df.primmeport %>% ungroup() %>% filter(!is.na(is.primeport)) %>% group_by(size.cat, is.primeport) %>% summarise(revenue = sum(port.revenue), mean.length = mean(AKR.Vessel.Length)) %>% ggplot(aes(size.cat))+
  geom_bar(aes(weight = revenue, fill = is.primeport), position = position_dodge())+
  ylab("Total Revenue of Landings")+xlab("Boat length")+labs(title = "Total Revenue Location by Primary/Non-Primary Port")
tr/productivty




ratio <- df.primmeport %>% ungroup() %>%  filter(!is.na(is.primeport)) %>% group_by(Batch.Year, is.primeport) %>% summarise(trips = sum(num.trips), revenue = sum(port.revenue)) #%>% summarise(ratio = sum(num.trips), revenue = sum(port.revenue)) 
table(ratio$is.primeport)
  
  
  ggplot(aes(x = Batch.Year, y = ))+
  geom_bar(aes(weight = num.trips), position = position_dodge())+
  ylab("Count of Landings Made at Port")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1))+
  scale_x_continuous("ID", labels = as.character(ID), breaks = ID)


p1 <- df.primmeport %>% ungroup() %>%  filter(!is.na(is.primeport)) %>% ggplot(aes(x = is.primeport))+
  geom_bar(aes(weight = port.revenue/nyears), fill="steelblue")+
  ylab("Total Revenue Landed at Port")
p1


