#.rs.restartR()
#rm()
#loads my packages. If not installed in your environment, the missing ones are installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', 'xtable', 'corrplot', 'ggh4x')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work
getwd()

source("code/myfunctions.R")

data_dir <- "./../../../"
#want to load data filter it by salmon boats then store that year of data in a list
datalist <- list.files(data_dir, pattern = "*.csv")
catch_data <- list()
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
#catch_data_temp <- bind_rows(catch_data) %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% ungroup()#boats that have participated in salmon fishery in the sample
#catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp["CFEC.Value..Detail."])] <- 0#fill the na's with 0
#write.csv(catch_data_temp, "intermediate data/2005_2021_tickets.csv", row.names = TRUE)

#catch_data_temp <- bind_rows(catch_data)#boats that have participated
#save(catch_data_temp, file = "intermediate data/catch_data_temp.rdata")

#Trip don't run
{
  catch_data_temp <- get.trip(catch_data_temp)
  catch_data_temp$trip.duration <- as.numeric(catch_data_temp$trip.length)+1
  catch_data_temp <- catch_data_temp %>% filter(trip.duration>0)
  catch_data_temp <- catch_data_temp %>% mutate(trip.prod = CFEC.Value..Detail./trip.duration)
}













#start here to save some time 
rm(catch_data)
gc()
load("intermediate data/catch_data_temp.rdata")

unique(catch_data_temp$Batch.Year)
catch_data_temp %>% ungroup %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% nrow()#this filter would filter to jsut the boats that have fished salmon permit at some point


catch_data_temp$Vessel.ADFG.Number[which(catch_data_temp$Vessel.ADFG.Number==62.39)] <- 62339#fix a typo in the data
catch_data_temp$Vessel.ADFG.Number <- as.integer(catch_data_temp$Vessel.ADFG.Number)
catch_data_temp["CFEC.Value..Detail."][is.na(catch_data_temp["CFEC.Value..Detail."])] <- 0#fill the na's with 0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Length
{catch_data_temp$length_cat <- cut(catch_data_temp$AKR.Vessel.Length,
                                   breaks=c(0, 20, 30, 40, 50, 60, 1000),
                                   labels=c('0-20', '20-30', '30-40', '40-50', '50-60', '60+'))
}
#Specialists
{
  boatlist <- catch_data_temp  %>% mutate(permit_species=substr(CFEC.Permit.Fishery, 1, 1)) %>% group_by(Vessel.ADFG.Number) %>% mutate(s_specialist = if_else(all(permit_species=="S"), 1, 0)) %>% ungroup() %>% select(Vessel.ADFG.Number, s_specialist) %>% distinct(Vessel.ADFG.Number, s_specialist)#boatlist in list of boats that have fished under a salmon permit at some point
  catch_data_temp <- catch_data_temp %>% mutate(permit_species=substr(CFEC.Permit.Fishery, 1, 1)) %>% group_by(Vessel.ADFG.Number) %>% mutate(s_specialist = if_else(all(permit_species=="S"), 1, 0)) %>% ungroup()#s_specialist indicates whether the boat is a salmon specialist across all time
  #now add a check if the boat is a permit specialist for a given year
  catch_data_temp <- catch_data_temp %>% group_by(Batch.Year, Vessel.ADFG.Number) %>%  mutate(permit.specialist = as.integer(n_distinct(CFEC.Permit.Fishery) == 1)) %>% ungroup()
}

#Productivity
{
  prod <- get.trip(catch_data_temp)
  prod$trip.duration <- as.numeric(prod$trip.length)+1
  #prod <- prod %>% filter(trip.duration>0) %>% select(Vessel.ADFG.Number) %>% distinct() %>%  nrow()#filter out negative trips
  prod <- prod %>% mutate(trip.duration=ifelse(trip.duration>0,trip.duration, NA))
  trip.revenue <- prod %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise(year.revenue=sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id), fishing.days = sum(trip.duration)) %>% mutate(revenue.per.trip = year.revenue/(num.trips*fishing.days))
  #NOTE: There are still some trips with trip lengths that are too long (e.g., trips ~= 50 years), They are left in for now
}

#Prime port
{
  pport <- prod %>% mutate(trip.prod=CFEC.Value..Detail./trip.duration) %>% get.primeport()
  pport <- pport %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% mutate(year.rev=sum(port.revenue)) %>% ungroup()
  save(pport, file = "intermediate data/pport.rdata")
  }
#Prime permit
{
  ppermit_year <- get.firstpermit(catch_data_temp)
  ppermit <- get.firstpermit.alltime(catch_data_temp)
  ppermit <- ppermit %>% mutate(first.species = substr(first.permit, 1, 1), second.species = substr(second.permit, 1, 1))
  salmon_list <- catch_data_temp %>% ungroup() %>% group_by(Vessel.ADFG.Number) %>% summarise(any.salmon = any(substr(CFEC.Permit.Fishery, 1, 1) == "S"), any.nonsalmon = any(substr(CFEC.Permit.Fishery, 1, 1) != "S")) %>% mutate(Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number)) %>% left_join(ppermit, join_by(Vessel.ADFG.Number)) %>% mutate(is.primary.salmon = ifelse(substr(first.permit, 1, 1) == "S",1,0), is.secondary.salmon = ifelse(substr(second.permit, 1, 1) == "S",1,0))
 #the difference in length between ppermit and salmon_list is because salmon list includes some boats (16 to be exact) that never had a permit on record
}
#Variance at vessel level
{
  vessel_var <- trip.revenue %>% group_by(Vessel.ADFG.Number) %>% summarise(v.rpt = var(revenue.per.trip), v.fdays = var(fishing.days), v.ntrips = var(num.trips), v.rev = var(year.revenue), mean.rpt = mean(revenue.per.trip), mean.fdays = mean(fishing.days), mean.ntrips = mean(num.trips), mean.rev = mean(year.revenue), years.fished = n())
  sum(is.na(vessel_var$v.rev))
  vessel_var <- vessel_var %>% left_join(salmon_list, join_by(Vessel.ADFG.Number))  %>% mutate(permit.species = substr(first.permit, 1, 1)) %>% mutate(Vessel.ADFG.Number=as.integer(Vessel.ADFG.Number)) %>% filter(permit.species!="")
  vessel_var <- vessel_var %>% mutate(CV = sqrt(v.rev)/mean.rev)
}
#Variance by vessel-fishery
{
  trip.revenue.fishery <- prod %>% group_by(Vessel.ADFG.Number, Batch.Year, CFEC.Permit.Fishery) %>% summarise(year.revenue=sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id), fishing.days = sum(trip.duration)) %>% mutate(revenue.per.trip = year.revenue/(num.trips*fishing.days))
  vessel_fishery_var <- trip.revenue.fishery %>% group_by(Vessel.ADFG.Number, CFEC.Permit.Fishery) %>% summarise(v.rpt = var(revenue.per.trip), v.fdays = var(fishing.days), v.ntrips = var(num.trips), v.rev = var(year.revenue), mean.rpt = mean(revenue.per.trip), mean.fdays = mean(fishing.days), mean.ntrips = mean(num.trips), mean.rev = mean(year.revenue), years.fished = n())
  sum(is.na(vessel_fishery_var$v.rev))
  vessel_fishery_var <- vessel_fishery_var %>% left_join(salmon_list, join_by(Vessel.ADFG.Number))  %>% mutate(permit.species = substr(first.permit, 1, 1)) %>% mutate(Vessel.ADFG.Number=as.integer(Vessel.ADFG.Number)) %>% filter(permit.species!="")
  vessel_fishery_var <- vessel_fishery_var %>% mutate(CV = sqrt(v.rev)/mean.rev) %>% mutate(permit.species = substr(CFEC.Permit.Fishery, 1, 1))
}

#Permit Species
{}
#Fixed effect for stock (Permit Species) and maybe permit
{}
#attach data
{
  reg.data <- catch_data_temp %>% left_join(trip.revenue, join_by(Vessel.ADFG.Number, Batch.Year))
  reg.data <- reg.data %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise(leng_cat = first(length_cat), num.trips = first(num.trips), revenue.per.trip=first(revenue.per.trip), fishing.days = first(fishing.days), year.revenue = first(year.revenue), permit.specialist = first(permit.specialist), s_specialist = first(s_specialist), Vessel.Length = first(AKR.Vessel.Length))
  reg.data <- reg.data %>% left_join(vessel_var, join_by(Vessel.ADFG.Number))
}
#regressinon 
{
  reg.data <- reg.data %>% filter(revenue.per.trip>0)
  prod_reg <- lm(log(revenue.per.trip) ~ + leng_cat + s_specialist + permit.specialist + as.factor(Batch.Year), data = reg.data)
  rev_reg <- lm(log(year.revenue) ~ + leng_cat + s_specialist + permit.specialist + as.factor(Batch.Year), data = reg.data)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(stargazer)
  tab_model(prod_reg)
  tab_model(rev_reg)
  
  
  
  prod_reg_int <- lm(log(revenue.per.trip) ~ + leng_cat + s_specialist + permit.specialist + s_specialist:permit.specialist + as.factor(Batch.Year), data = reg.data)
  rev_reg_int <- lm(log(year.revenue) ~  + leng_cat + s_specialist + permit.specialist + s_specialist:permit.specialist + as.factor(Batch.Year), data = reg.data)
  stargazer(prod_reg,rev_reg,prod_reg_int, rev_reg_int,title="Regression Results", align=TRUE)  
  #mylogit <- glm(revenue.per.trip ~ leng_cat + s_specialist + permit.specialist, data = reg.data, family = "binomial")
  
  prod %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% summarise(s_specialist = first(s_specialist), permit.specialist = first(permit.specialist), CFEC.Permit.Fishery = c(first(CFEC.Permit.Fishery)), last(CFEC.Permit.Fishery)) %>% filter(s_specialist==1 & permit.specialist==0) %>% ungroup() %>% 
    ggplot()+
    geom_bar(aes(x = CFEC.Permit.Fishery))+
    ylab("Count of vessels")+
    theme(axis.text.x = element_text(angle = 90, vjust = 1))+
    labs(title = "Number of Salmon Specialists and Non-Permit Specialists in Different Fisheries")
}

#variance plot by species fishery
{
  # Libraries
  library(hrbrthemes)
  library(viridis)
  # Plot
  vessel_var %>% mutate(sd.rev = sqrt(v.rev)) %>% 
    ggplot( aes(x=permit.species, y=sd.rev, fill=permit.species)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 1000000))+
    ggtitle("Basic boxplot") +
    xlab("Fishery")
  
  sorted_group_median <- vessel_var %>% 
    filter(!is.na(CV)) %>%
    group_by(permit.species) %>%
    summarise(median.cv = median(CV), median.rev = median(mean.rev), count = n()) %>%
    arrange(median.cv)
  View(sorted_group_median %>% filter(count>50)) 
  vessel_var$permit.species <- factor(vessel_var$permit.species, levels = sorted_group_median$permit.species)#sort for graphing
  vessel_var %>% group_by(permit.species) %>% summarise(count = n()) %>% filter(count>50) %>% View() 
  
  vessel_var %>%
    filter(!is.na(CV)) %>% 
    group_by(permit.species) %>% filter(n()>50) %>% ungroup() %>% #filter out fishery species that have less than 50 vessels dedicated lifetime
    #mutate(permit.species = fct_reorder(permit.species, sd.rev, median)) %>%
    ggplot(aes(x=permit.species, y=CV, fill=permit.species)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=14),
        axis.text.x = element_text(angle = 0, vjust = 1)
      ) +
      scale_y_continuous(limits = c(0, 3))+
      ggtitle("CV of Vessel annual revenues") +
      xlab("Primary Species Fishery")
  
  vessel_var %>%
    filter(!is.na(v.rev)) %>% 
    group_by(permit.species) %>% filter(n()>50) %>% ungroup() %>% #filter out fishery species that have less than 50 vessels dedicated lifetime
    #mutate(permit.species = fct_reorder(permit.species, sd.rev, median)) %>%
    ggplot(aes(x=permit.species, y=mean.rev, fill=permit.species)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 500000))+
    ggtitle("Means of vessel annual revenues") +
    xlab("Primary Species Fishery")
}
#Salmon permit specialist vs non-permit specialist
{
  vessel_var <- vessel_var %>% ungroup() %>% mutate(specialist_category = case_when(
    any.salmon==0 ~ "no salmon",
    any.salmon==1 & first.species!="S" ~ "primary non-salmon",
    first.species=="S" & (second.species!="S" & !is.na(second.species)) ~ "primary salmon, non-specialist",
    first.species=="S" & (second.species=="S") ~ "only salmon, multiple permits",
    first.species=="S" & (is.na(second.species)) ~ "only salmon, single permit",
    .default = "missing"
  ))
  sorted_group_median_fc <- vessel_var %>%
    filter(!is.na(CV)) %>%
    ungroup %>% group_by(specialist_category) %>%
    summarise(median.cv = median(CV), mean.cv = mean(CV), count = n()) %>%
    arrange(median.cv)
  View(sorted_group_median_fc) 
  
  vessel_var %>% 
    ggplot(aes(x=specialist_category, y=CV, fill=specialist_category)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 3))+
    ggtitle("CV vessel annual revenues by fishing category") +
    xlab("Fishing Category")
  vessel_var %>% filter(!is.na(CV)) %>% 
    ggplot(aes(x=specialist_category, y=mean.rev, fill=specialist_category)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 250000))+
    ggtitle("Mean of vessel annual revenues by fishing category") +
    xlab("Fishing Category")  
  
  
  vessel_var %>% mutate(sd.rev = sqrt(v.rev)) %>% filter(sd.rev>250000) %>% nrow()#number of outliers
  View(vessel_var %>% mutate(sd.rev = sqrt(v.rev)) %>% ungroup() %>% group_by(specialist_category) %>% filter(!is.na(v.rev)) %>%  summarise(CV = (v.rev/mean.rev)))
  
  
  cor_table <- vessel_var %>% mutate(sd.rev = sqrt(v.rev)) %>% ungroup() %>% filter(!is.na(permit.species))%>% filter(!is.na(v.rev), mean.rev>0) %>% group_by(permit.species) %>% summarise(mean_mvessel_rev = mean(mean.rev), var_mvessel_rev = var(mean.rev), mean.Vessel.CV = mean(CV), sd.Vessel.CV = sd(CV), cor.CV_mean = cor(mean.rev, CV, use="complete.obs"), count=n()) %>% filter(count>50) %>% mutate(CV.mean.rev = sqrt(var_mvessel_rev)/mean_mvessel_rev) 
  xtable(cor_table)
  
  View(vessel_var %>% mutate(sd.rev = sqrt(v.rev)) %>% ungroup() %>% filter(!is.na(permit.species))%>% filter(!is.na(v.rev)) %>%   group_by(permit.species) %>% summarise(mv.corr = cor(v.rev, mean.rev, use="complete.obs"), count=n()) %>% filter(count>50))
  temp <- vessel_var %>% filter(!is.na(v.rev) & mean.rev>0)
  mv_trade <- lm(log(v.rev) ~ +log(mean.rev)+ as.factor(specialist_category)+AKR.Vessel.Length, data = temp)
  mv_trade2 <- lm(log(v.rev) ~ +log(mean.rev)+ as.factor(specialist_category)+as.factor(permit.species)+AKR.Vessel.Length, data = temp)
  stargazer(mv_trade,mv_trade2,title="Regression Results (MV Tradeoff)", align=TRUE)
  
}

#variance plot by species fishery 2
{
  # Libraries
  library(hrbrthemes)
  library(viridis)
  fish_categories <-  vessel_var %>% group_by(permit.species) %>%  filter(n()>50) %>% select(permit.species) %>% distinct()
    
  #sorted_group_median <- vessel_fishery_var %>% 
  #  filter(!is.na(CV)) %>%
  #  group_by(permit.species) %>%
  #  summarise(median.cv = median(CV), median.rev = median(mean.rev), count = n()) %>%
  #  arrange(median.cv)
  View(sorted_group_median %>% filter(count>50, permit.species!="")) 
  vessel_fishery_var$permit.species <- factor(vessel_fishery_var$permit.species, levels = sorted_group_median$permit.species)#sort for graphing
  vessel_fishery_var %>% group_by(permit.species) %>% summarise(count = n()) %>% filter(count>50, permit.species!="") %>% View() 
  
  vessel_fishery_var %>%
    filter(!is.na(CV)) %>% 
    group_by(permit.species) %>% filter(n()>50, permit.species!="") %>% ungroup() %>% 
    ggplot(aes(x=permit.species, y=CV, fill=permit.species)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 3))+
    ggtitle("CV of Vessel annual revenues") +
    xlab("Primary Species Fishery")
  vessel_fishery_var %>%
    filter(!is.na(CV)) %>% 
    group_by(permit.species) %>% filter(permit.species %in% fish_categories$permit.species) %>% ungroup() %>% 
    ggplot(aes(x=permit.species, y=CV, fill=permit.species)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 3))+
    ggtitle("CV of Vessel annual revenues") +
    xlab("Primary Species Fishery")
  
  vessel_fishery_var %>%
    filter(!is.na(v.rev)) %>% 
    group_by(permit.species) %>% filter(permit.species %in% fish_categories$permit.species) %>% ungroup() %>% 
    ggplot(aes(x=permit.species, y=mean.rev, fill=permit.species)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 500000))+
    ggtitle("Means of vessel annual revenues") +
    xlab("Primary Species Fishery")
}
#Salmon permit specialist vs non-permit specialist 2
{
  vessel_fishery_var <- vessel_fishery_var %>% ungroup() %>% mutate(specialist_category = case_when(
    any.salmon==0 ~ "no salmon",
    any.salmon==1 & first.species!="S" ~ "primary non-salmon",
    first.species=="S" & (second.species!="S" & !is.na(second.species)) ~ "primary salmon, non-specialist",
    first.species=="S" & (second.species=="S") ~ "only salmon, multiple permits",
    first.species=="S" & (is.na(second.species)) ~ "only salmon, single permit",
    .default = "missing"
  )) %>% mutate(Salmon = ifelse(permit.species=='S', 'S', 'Non-Salmon'))
  
  sorted_group_median_fc <- vessel_fishery_var %>%
    filter(!is.na(CV)) %>%
    ungroup %>% group_by(specialist_category) %>%
    summarise(median.cv = median(CV), mean.cv = mean(CV), count = n()) %>%
    arrange(median.cv)
  View(sorted_group_median_fc) 
  
  vessel_fishery_var %>% 
    ggplot(aes(x=specialist_category, y=CV, fill=specialist_category)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 3))+
    ggtitle("CV vessel annual revenues by fishing category") +
    xlab("Fishing Category")
  vessel_fishery_var %>% filter(!is.na(CV)) %>% filter(permit.species %in% fish_categories$permit.species) %>% # filter(n()>50, permit.species!="") %>%
    mutate(permit.is.primary = (permit.species==first.species)) %>% 
    ggplot(aes(x=permit.species, y=CV, fill=permit.is.primary)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6, option="E") +
      theme_ipsum() +
      theme(
        plot.title = element_text(size=12),
        axis.text.x = element_text(angle = 0, vjust = 1)
      ) +
      scale_y_continuous(limits = c(0, 3))+
      ggtitle("CV of vessel annual revenues by fishing category") +
      xlab("Fishing Category")  
  vessel_fishery_var %>% filter(!is.na(CV)) %>% filter(permit.species %in% fish_categories$permit.species) %>% # filter(n()>50, permit.species!="") %>%
    mutate(permit.is.primary = (permit.species==first.species)) %>% 
    ggplot(aes(x=permit.species, y=mean.rev, fill=permit.is.primary)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="E") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=12),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 2000000))+
    ggtitle("Mean of vessel annual revenues by fishing category") +
    xlab("Fishing Category")
  #salmon CV comparison
  vessel_fishery_var %>% filter(!is.na(CV)) %>% filter(permit.species=="S") %>%
    mutate(permit.is.primary = (permit.species==first.species)) %>% 
    ggplot(aes(x=CFEC.Permit.Fishery, y=CV, fill=permit.is.primary)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="E") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=12),
      axis.text.x = element_text(angle = 90, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 3))+
    ggtitle("CV of vessel annual revenues by fishing category") +
    xlab("Fishing Category")  
  
  secondary_fisheries <- vessel_fishery_var %>% ungroup() %>% group_by(Vessel.ADFG.Number) %>% mutate(tot.rev = sum(mean.rev*years.fished)) %>% ungroup() %>% 
    filter(!is.na(CV) & CFEC.Permit.Fishery!="") %>% mutate(perc.rev = (mean.rev*years.fished)/tot.rev) %>%
    mutate(permit.is.primary = (CFEC.Permit.Fishery==first.permit)) %>% filter(permit.is.primary==FALSE) %>% 
    group_by(CFEC.Permit.Fishery) %>% summarise(count = n(), rev.ratio = mean(perc.rev))
  
  secondary_fisheries %>% mutate(permit.species = substr(CFEC.Permit.Fishery, 1, 1)) %>% group_by(permit.species) %>% summarise(count = sum(count), mean.ratios=mean(rev.ratio)) %>% View()
  
  vessel_fishery_var %>% ungroup() %>% group_by(Vessel.ADFG.Number) %>% mutate(tot.rev = sum(mean.rev*years.fished)) %>% ungroup() %>% 
    filter(!is.na(CV) & CFEC.Permit.Fishery!="") %>% mutate(perc.rev = (mean.rev*years.fished)/tot.rev) %>%
    mutate(permit.is.primary = (CFEC.Permit.Fishery==first.permit)) %>% filter(permit.is.primary==FALSE) %>% 
    group_by(permit.species) %>% summarise(count = n(), rev.ratio = mean(perc.rev)) %>% View()
  vessel_fishery_var %>% ungroup() %>% group_by(Vessel.ADFG.Number) %>% mutate(tot.rev = sum(mean.rev*years.fished)) %>% ungroup() %>% 
    filter(!is.na(CV) & CFEC.Permit.Fishery!="") %>% mutate(perc.rev = (mean.rev*years.fished)/tot.rev) %>%
    mutate(permit.is.primary = (CFEC.Permit.Fishery==first.permit)) %>% filter(permit.is.primary==FALSE) %>% filter(first.species=="S") %>% 
    group_by(permit.species) %>% summarise(count = n(), rev.ratio = mean(perc.rev)) %>% View()
  
}





#variance plot by vessel length
{
  #link the vessel length with the vessel length
  #will have yearly observations so we use the median of all the lengths
  temp <- catch_data_temp %>% group_by(Vessel.ADFG.Number) %>% summarise(AKR.Vessel.Length = median(AKR.Vessel.Length), length_cat = length_cat[first(which.median(AKR.Vessel.Length))]) %>% na.omit()
  vessel_var <- vessel_var %>% left_join(temp, join_by(Vessel.ADFG.Number))
  vessel_fishery_var <- vessel_fishery_var %>% left_join(temp, join_by(Vessel.ADFG.Number))
  rm(temp)
  gc()
  
  table(catch_data_temp$length_cat, useNA = "always")
  catch_data_temp %>% filter(is.na(Vessel.ADFG.Number)) %>% view()
  catch_data_temp %>% filter(Vessel.ADFG.Number==0) %>% View()
  
  vessel_var %>% mutate(sd.rev = sqrt(v.rev)) %>%
    #filter(!is.na(sd.rev)) %>% 
    group_by(length_cat) %>% filter(n()>50) %>% ungroup() %>% #filter out fishery species that have less than 50 vessels dedicated lifetime
    #mutate(length_cat = fct_reorder(length_cat, sd.rev, median)) %>%
    ggplot(aes(x=length_cat, y=sd.rev, fill=length_cat)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=12),
      axis.text.x = element_text(angle = 0, vjust = 1)
    ) +
    scale_y_continuous(limits = c(0, 500000))+
    ggtitle("Standard deviation of vessel annual revenues by vessel length") +
    xlab("Vessel Length (feet)")
}
#variance scatterplot plot by vessel length
{
  #vessel length on x axis; vert is variance. color for species
  vessel_var %>% mutate(sd.rev = sqrt(v.rev)) %>%
    #filter(!is.na(sd.rev)) %>% 
    group_by(permit.species) %>% filter(n()>50) %>% ungroup() %>% filter(permit.species=="S") %>%  #filter out fishery species that have less than 50 vessels dedicated lifetime
    #mutate(length_cat = fct_reorder(length_cat, sd.rev, median)) %>%
    ggplot(aes(x=AKR.Vessel.Length, y=sd.rev, color=permit.species)) +
    geom_point(shape=1) +
    scale_y_continuous(limits = c(0, 500000))+
    scale_x_continuous(limits = c(0,100))+
    ggtitle("Standard deviation of vessel annual revenues") +
    xlab("Vessel Length (feet)")
    
    vessel_var %>% mutate(sd.rev = sqrt(v.rev)) %>%
      #filter(!is.na(sd.rev)) %>% 
      group_by(permit.species) %>% filter(n()>50) %>% ungroup() %>% filter(permit.species!="S") %>%  #filter out fishery species that have less than 50 vessels dedicated lifetime
    #mutate(length_cat = fct_reorder(length_cat, sd.rev, median)) %>%
    ggplot(aes(x=AKR.Vessel.Length, y=sd.rev, color=permit.species)) +
      geom_point(shape=1) +
      scale_y_continuous(limits = c(0, 500000))+
      scale_x_continuous(limits = c(0,100))+
      ggtitle("Standard deviation of vessel annual revenues") +
      xlab("Vessel Length (feet)")
}




#Only salmon dissection 
  {
    vessel_fishery_var <- vessel_fishery_var %>% ungroup() %>% mutate(specialist_category = case_when(
      any.salmon==0 ~ "no salmon",
      any.salmon==1 & first.species!="S" ~ "primary non-salmon",
      first.species=="S" & any.nonsalmon==TRUE ~ "primary salmon, non-specialist",
      first.species=="S" & (second.species=="S") & any.nonsalmon==FALSE ~ "only salmon, multiple permits",
      first.species=="S" & (is.na(second.species)) ~ "only salmon, single permit",
      .default = "missing"
    )) %>% mutate(Salmon = ifelse(permit.species=='S', 'S', 'Non-Salmon'))
    
    vessel_var <- vessel_var %>% ungroup() %>% mutate(specialist_category = case_when(
      any.salmon==0 ~ "no salmon",
      any.salmon==1 & first.species!="S" ~ "primary non-salmon",
      first.species=="S" & any.nonsalmon==TRUE ~ "primary salmon, non-specialist",
      first.species=="S" & (second.species=="S") & any.nonsalmon==FALSE ~ "only salmon, multiple permits",
      first.species=="S" & (is.na(second.species)) ~ "only salmon, single permit",
      .default = "missing"
    )) %>% mutate(Salmon = ifelse(permit.species=='S', 'S', 'Non-Salmon'), CFEC.Permit.Fishery=first.permit)
    
    Salmon_Fishery_Vessels <- catch_data_temp %>% ungroup() %>% group_by(Vessel.ADFG.Number, CFEC.Permit.Fishery) %>% summarise() %>% group_by(CFEC.Permit.Fishery) %>% summarise(total_boats = n())
    particip_data <- vessel_var %>% filter(specialist_category %in% c("only salmon, multiple permits", "only salmon, single permit", "primary salmon, non-specialist")) %>% group_by(CFEC.Permit.Fishery, specialist_category) %>% summarise(nboats = n() ) %>% ungroup() %>% complete(CFEC.Permit.Fishery, specialist_category, fill=list(nboats=0)) %>% left_join(Salmon_Fishery_Vessels, join_by(CFEC.Permit.Fishery)) %>% mutate(share = nboats/total_boats)
    
    particip_data <- particip_data %>% mutate(gear.class = case_when(
      substr(CFEC.Permit.Fishery, 3, 4)=="01" ~ "(01) purse seine",
      substr(CFEC.Permit.Fishery, 3, 4)=="02" ~ "(02) beach seine",
      substr(CFEC.Permit.Fishery, 3, 4)=="03" ~ "(03) drift gillnet",
      substr(CFEC.Permit.Fishery, 3, 4)=="04" ~ "(04) set gillnet",
      substr(CFEC.Permit.Fishery, 3, 4)=="05" ~ "(05) hand line/jig/troll",
      substr(CFEC.Permit.Fishery, 3, 4)=="15" ~ "(15) power gurdy troll",
      .default = "missing")) %>% mutate(gear.clase = as.factor(gear.class))
    p1 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery,1,4)!="S 77") %>% 
      ggplot(aes(x=CFEC.Permit.Fishery, y=nboats, fill=specialist_category)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer(palette = "Set1")+
      labs(title = "Boat Participation by Landing Permit (# boats)", x=("Salmon Permit"), y=("Number of Vessels in Fishery"))+
      theme(legend.position = c(0.912, 0.93), text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
      guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
    p3 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery,1,4)!="S 77") %>%  
      ggplot(aes(x=CFEC.Permit.Fishery, y=share, fill=specialist_category)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer(palette = "Set1")+
      labs(title = "Boat Participation by Landing Permit (Share)", x=("Salmon Permit"), y=("Share of Vessels in Fishery"))+
      theme(legend.position = c(0.912, 0.93), text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
      guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
    p2 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery,1,4)!="S 77") %>% 
      group_by(gear.class, specialist_category) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
      mutate(share = nboats/total_boats) %>% 
      ggplot(aes(x=gear.class, y=nboats, fill=specialist_category)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer(palette = "Set2")+
      labs(title = "Boat Participation by Fishing Strategy (# boats)", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Number of Vessels in Fishery"))+
      theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
      guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
    p4 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery,1,4)!="S 77") %>% 
      group_by(gear.class, specialist_category) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
      mutate(share = nboats/total_boats) %>% 
      ggplot(aes(x=gear.class, y=share, fill=specialist_category)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer(palette = "Set2")+
      labs(title = "Boat Participation by Fishing Strategy (Share)", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Share of Vessels in Fishery"))+
      theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
      guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
    (p1/p3)
    (p2/p4)
    
    
    sorted_group_median_fc <- vessel_fishery_var %>%
      filter(!is.na(CV)) %>%
      ungroup %>% group_by(specialist_category) %>%
      summarise(median.cv = median(CV), mean.cv = mean(CV), count = n()) %>%
      arrange(median.cv)
    View(sorted_group_median_fc) 
    
    vessel_fishery_var %>% 
      ggplot(aes(x=specialist_category, y=CV, fill=specialist_category)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6, option="H") +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=12),
        axis.text.x = element_text(angle = 0, vjust = 1)
      ) +
      scale_y_continuous(limits = c(0, 3))+
      ggtitle("CV vessel annual revenues by fishing category") +
      xlab("Fishing Category")
    
    
    permit_variance <- trip.revenue.fishery %>% group_by(Batch.Year, CFEC.Permit.Fishery) %>% summarise(year.revenue = sum(year.revenue)) %>%
      group_by(CFEC.Permit.Fishery) %>% summarise(fishery.var = var(year.revenue), fishery.CV = sd(year.revenue)/mean(year.revenue))
    
    particip_data %>% left_join(permit_variance, join_by(CFEC.Permit.Fishery)) %>% 
      filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery,1,4)!="S 77") %>%
      ggplot(aes(x=specialist_category, y=share, fill=fishery.CV)) +
      geom_bar(position = 'dodge', stat="identity") +
      facet_grid(. ~ CFEC.Permit.Fishery) + 
      scale_fill_continuous(type = "viridis")+
      scale_x_discrete(name = "Specialist Category", labels = c("(2+)","(1)","(NS)"))
    #2+ - Only Salmon, Multiple Permits
    #1 - Only Salmon, Single Permit
    #NS - Non-Specialist, Primary Salmon
    particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery,1,4)!="S 77") %>% 
      left_join(permit_variance, join_by(CFEC.Permit.Fishery)) %>%
      group_by(gear.class, specialist_category) %>% 
      summarise(nboats = sum(nboats), total_boats = sum(total_boats), mean.cv = mean(fishery.CV, na.rm = TRUE)) %>% 
      mutate(share = nboats/total_boats) %>% 
      ggplot(aes(x=specialist_category, y=share, fill=mean.cv)) +
      geom_bar(position = 'dodge', stat="identity") +
      facet_grid(. ~ gear.class) + 
      scale_fill_continuous(type = "viridis")+
      scale_x_discrete(name = "Specialist Category", labels = c("(2+)","(1)","(NS)"))
}

#, fill = fishery.var
#ggplot(aes(x=CFEC.Permit.Fishery, y=nboats, fill=specialist_category)) +
ggplot(data = data, aes(x = interaction(Group,Category, sep = "!"), y = Value, fill = Group)) + 
geom_col(position = 'dodge', show.legend = FALSE) +
  geom_text(aes(label = paste(Value, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = "Category")


reg.data %>% ungroup() %>%  group_by(Vessel.ADFG.Number) %>% summarise(CFEC.Permit.Fishery = paste(unique(CFEC.Permit.Fishery), collapse = " ")) %>% View()
vessel_var %>% ggplot()+
  geom_bar(aes(x = first.permit))+
  ylab("Count of vessels")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1))+
  labs(title = "Number of Salmon Specialists and Non-Permit Specialists in Different Fisheries")

vessel_var %>% filter(!is.na(v.rev)) %>% summarise(q = quantile(v.rev, c(0.25, 0.5, 0.75, 0.8, 0.9, 0.95)), x = c(0.25, 0.5, 0.75, 0.8, 0.9, 0.95))

vessel_var %>% filter(!is.na(v.rev)) %>%  # Summary by group using dplyr
  group_by(permit.species) %>% 
  summarise(median = median(v.rev),
            mean = mean(v.rev),
            n = n()) %>% View()
vessel_var %>% filter(!is.na(v.rev)) %>% mutate(sd.rev = sqrt(v.rev)) %>%   # Summary by group using dplyr
  group_by(permit.species) %>% 
  summarise(median = median(sd.rev),
            mean = mean(sd.rev),
            n = n()) %>% filter(n>50) %>% View()



