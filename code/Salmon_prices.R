#.rs.restartR()
#rm()
#loads my packages. If not installed in your environment, the missing ones are installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', "zoo")
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work
getwd()

source("code/myfunctions.R")
load("intermediate data/catch_data_temp.rdata")

unique(catch_data_temp$Batch.Year)
catch_data_temp %>% ungroup %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% nrow()#this filter would filter to jsut the boats that have fished salmon permit at some point

#Productivity
{
  prod <- get.trip(catch_data_temp)
  prod$trip.duration <- as.numeric(prod$trip.length)+1
  prod <- prod %>% filter(trip.duration>0)#filter out negative trips
  trip.revenue <- prod %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise(year.revenue=sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id), fishing.days = sum(trip.duration)) %>% mutate(revenue.per.trip = year.revenue/(num.trips*fishing.days))
  prod <- prod %>% mutate(Date.Fishing.Began = as.Date(as.character(Date.Fishing.Began),format = "%Y%m%d"), Date.Landed = as.Date(as.character(Date.Landed),format = "%Y%m%d"))
  price_reg_data <- prod %>% ungroup %>% group_by(Batch.Year, CFEC.Permit.Fishery, Species.Code) %>% summarise(permit.species = first(permit.species), Revenue = sum(CFEC.Value..Detail.), Weight = sum(CFEC.Whole.Pounds..Detail.), Species.Common.Name = first(Species.Common.Name), med.price.per.pound = median(CFEC.Price.per.Pound)) %>% mutate(calc.price = Revenue/Weight, Year = as.Date(as.character(Batch.Year),format = "%Y"))
  #remove the null fishery observations
  price_reg_data <- price_reg_data %>% drop_na()
  #unique(price_reg_data$CFEC.Permit.Fishery)[1]==""
  price_reg_data <- subset(price_reg_data, price_reg_data$CFEC.Permit.Fishery != "")
  trip.revenue <- trip.revenue %>% ungroup %>% group_by(Vessel.ADFG.Number) %>%  mutate(roll_avg = rollmean(year.revenue, k=5, fill=c("extend", year.revenue), align = "right"))%>% ungroup() %>% mutate(dev = abs(year.revenue-roll_avg))
  }

{
  fishery.list <- unique(price_reg_data$CFEC.Permit.Fishery)
  names <- unique(price_reg_data$Species.Common.Name)
  price_reg_data %>% filter(CFEC.Permit.Fishery==fishery.list[1]) %>% View()
}
{
  boatlist <- catch_data_temp %>% filter(CFEC.Permit.Fishery=="S 03T") %>% select(Vessel.ADFG.Number, CFEC.Permit.Fishery) %>% distinct() %>% left_join(trip.revenue, join_by(Vessel.ADFG.Number)) %>%  mutate(Year = as.Date(as.character(Batch.Year),format = "%Y"))
  boatlist %>% ungroup() %>% group_by(Batch.Year) %>%  ggplot(aes(x=Batch.Year, y=mean(dev)))+
    geom_line()+
    scale_y_continuous(limits = c(0, 500000))+
    ggtitle("Deviations of vessel annual revenue from a rolling average") +
    xlab("Year")
}
price_reg_data %>% filter(CFEC.Permit.Fishery=="S 03T") %>% ggplot(aes(x=Year, y=Revenue, color = Species.Common.Name)) +
  geom_line() + 
  xlab("Year") +
  ggtitle("Bristol Bay (S 03T) Revenue")
#  facet_wrap(~Species.Common.Name)

price_reg_data %>% filter(CFEC.Permit.Fishery=="S 03T") %>% ggplot(aes(x=Year, y=calc.price, color = Species.Common.Name)) +
  geom_line() + 
  xlab("Year") +
  ggtitle("Bristol Bay (S 03T) Prices")

price_reg_data %>% filter(CFEC.Permit.Fishery=="S 03T", Species.Common.Name=="salmon, chum" | Species.Common.Name=="salmon, sockeye") %>% ggplot(aes(x=Year, y=Weight, color = Species.Common.Name)) +
  geom_line() + 
  xlab("Year")
price_reg_data %>% filter(CFEC.Permit.Fishery=="S 03T", Species.Common.Name=="salmon, chum" | Species.Common.Name=="salmon, sockeye") %>% ggplot(aes(x=Year, y=Revenue, color = Species.Common.Name)) +
  geom_line() + 
  xlab("Year")




getPrices <- function(fishery){
  df_plot <- data.frame(matrix(ncol = 1, nrow = 0))
  colnames(df_plot) <- c("CFEC.Price.per.Pound")
  j=1
  for (i in datalist[1:10]) {
    temppath <- paste0("./../../../", i)
    fishery <- "S 03T" #drift, gillnet Bristol Bay
    plotvar <- c("CFEC.Price.per.Pound", "Batch.Year")
    df <- read.csv(file.path(temppath)) %>% filter(CFEC.Permit.Fishery==fishery) #%>% select(plotvar)
    df["Batch.Year"] <- as.Date(as.character(df["Batch.Year"]), format = "%Y")
    df_plot[j,1] <- Mode(df[plotvar[1]][,1])
    j <- j+1
    print(sum(df_plot['CFEC.Price.per.Pound']))
  }
}

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

df_statstore <- data.frame(CFEC.Price.per.Pound = double(), Year=integer(), Avg.Price=double(), Species=as.character())
j=1
for (i in datalist) {
  temppath <- paste0("./../../../", i)
  fishery <- "S 03T" #drift, gillnet Bristol Bay
  plotvar <- c("CFEC.Price.per.Pound", "Batch.Year", "CFEC.Value..Detail.", "CFEC.Whole.Pounds..Detail.", "Species.Common.Name")
  dftemp <- read.csv(file.path(temppath)) %>% filter(CFEC.Permit.Fishery==fishery) %>% select(plotvar)
  if(j==1){
    df <- dftemp
  }else{
    df <- df %>% bind_rows(read.csv(file.path(temppath)) %>% filter(CFEC.Permit.Fishery==fishery) %>% select(plotvar))
  }
  df_statstore[j,1] <- Mode(dftemp[,plotvar[1]])
  df_statstore[j,2] <-  max(dftemp[,"Batch.Year"])
  df_statstore[j,3] <-  sum(dftemp[,"CFEC.Value..Detail."])/sum(dftemp[,"CFEC.Whole.Pounds..Detail."])
  df_statstore[j,4] <-  dftemp[1,"Species.Common.Name"]
  print(df_statstore[j,])
  j <- j+1
}
write.xlsx(df_statstore, "df_statstore_bristolbay.xlsx")
#df[,"Batch.Year"] <- as.Date(as.character(df[,"Batch.Year"]), format = "%Y")#graph actually works better if year left as integer
g <- df %>% ggplot() + 
  geom_boxplot(aes(x = df[,plotvar[2]], y = df[,plotvar[1]], group = df[,plotvar[2]]))+xlim(1990,2022) #+  scale_x_date(limits = as.Date(c("1990-01-01", "1995-01-01")), date_labels = "%Y")
g+xlab("Year")+
  ylab("Landing prices")+
  ggtitle("Landing prices for Bristol Bay (S 03T)")

g_sock <- df %>% filter(df[,plotvar[5]]=="salmon, sockeye") %>% ggplot() +
  geom_boxplot(aes(x = df[,plotvar[2]], y = df[,plotvar[1]], group = df[,plotvar[2]]))+xlim(1990,2022)
g_sock+xlab("Year")+
  ylab("Landing prices")+
  ggtitle("Landing prices for Bristol Bay (S 03T) Sockeye")

g_chin <- df %>% filter(df[,plotvar[5]]=="salmon, chinook") %>% ggplot() + 
  geom_boxplot(aes(x = df[,plotvar[2]], y = df[,plotvar[1]], group = df[,plotvar[2]]))+xlim(1990,2022)
g_chin+xlab("Year")+
  ylab("Landing prices")+
  ggtitle("Landing prices for Bristol Bay (S 03T) Chinook")

g_hal <- df %>% filter(df[,plotvar[5]]=="halibut, Pacific") %>% ggplot() + 
  geom_boxplot(aes(x = df[,plotvar[2]], y = df[,plotvar[1]], group = df[,plotvar[2]]))+xlim(1990,2022)
g_hal+xlab("Year")+
  ylab("Landing prices")+
  ggtitle("Landing prices for Bristol Bay (S 03T) Halibut")


for (i in datalist) {
  temppath <- paste0("./../../../", i)
  
}
