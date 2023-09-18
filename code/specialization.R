#.rs.restartR()
#rm()
#loads my packages. If not installed in your environment, the missing ones are installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', 'xtable', 'corrplot')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}#probably could just use loadpacks but this should work
getwd()

setwd("/home/akfin/jraymond/Rprojects/joe-thesis")
source("code/myfunctions.R")

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

#strategy
{
  prod <- catch_data_temp %>% ungroup() %>% get.trip() %>% mutate(trip.duration=(as.numeric((trip.length))+1)) %>% 
    mutate(trip.duration=ifelse(trip.duration>0,trip.duration, NA)) %>% 
    ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number, CFEC.Permit.Fishery) %>%
    summarise(fishery.revenue=sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id), fishing.days = sum(trip.duration)) %>%
    mutate(revenue.per.trip = fishery.revenue/(num.trips*fishing.days))
  
  prod <- prod %>% ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% 
    mutate(first.permit = CFEC.Permit.Fishery[first(which(fishery.revenue==max(fishery.revenue)))], second.permit = if_else(length(unique(CFEC.Permit.Fishery))==1, NA, CFEC.Permit.Fishery[first(which(fishery.revenue==second_max(fishery.revenue)))], missing=NULL), permits.fished = length(unique(CFEC.Permit.Fishery)), first.species = substr(first.permit, 1, 1), second.species = substr(second.permit, 1, 1))
  
  prod <- prod %>% ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% mutate(specialist_category = case_when(
    any(substr(CFEC.Permit.Fishery, 1, 1) == "S")==FALSE ~ "no salmon",
    any(substr(CFEC.Permit.Fishery, 1, 1) == "S")==TRUE & first.species!="S" ~ "primary non-salmon",
    first.species=="S" & any(substr(CFEC.Permit.Fishery, 1, 1) != "S")==TRUE ~ "primary salmon, non-specialist",
    first.species=="S" & (second.species=="S") & any(substr(CFEC.Permit.Fishery, 1, 1) != "S")==FALSE ~ "only salmon, multiple permits",
    first.species=="S" & (is.na(second.species)) ~ "only salmon, single permit",
    .default = "missing"))# %>% mutate(Salmon = ifelse(permit.species=='S', 'S', 'Non-Salmon'))
  
  Total_Fishery_Vessels <- prod %>% ungroup() %>% group_by(CFEC.Permit.Fishery, Batch.Year) %>% summarise(total_boats = n())#total_boats is the number of boats in a fishery for a specific year
  particip_data <- prod %>% group_by(CFEC.Permit.Fishery, specialist_category, Batch.Year) %>% summarise(nboats = n() ) %>% 
    ungroup() %>%
    complete(CFEC.Permit.Fishery, specialist_category, fill=list(nboats=0)) %>%
    left_join(Total_Fishery_Vessels, join_by(CFEC.Permit.Fishery, Batch.Year)) %>% mutate(share = nboats/total_boats)
  
  particip_data <- particip_data %>% mutate(gear.class = case_when(
    substr(CFEC.Permit.Fishery, 3, 4)=="01" ~ "(01) purse seine",
    substr(CFEC.Permit.Fishery, 3, 4)=="02" ~ "(02) beach seine",
    substr(CFEC.Permit.Fishery, 3, 4)=="03" ~ "(03) drift gillnet",
    substr(CFEC.Permit.Fishery, 3, 4)=="04" ~ "(04) set gillnet",
    substr(CFEC.Permit.Fishery, 3, 4)=="05" ~ "(05) hand line/jig/troll",
    substr(CFEC.Permit.Fishery, 3, 4)=="15" ~ "(15) power gurdy troll",
    .default = "missing")) %>% mutate(gear.class = as.factor(gear.class))
  }
#time average plots 
{
  p1 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
    ungroup() %>% group_by(CFEC.Permit.Fishery, specialist_category) %>% summarise(avg.share = mean(share, na.rm = TRUE), nboats=mean(nboats, na.rm = TRUE)) %>% 
    ggplot(aes(x=CFEC.Permit.Fishery, y=nboats, fill=specialist_category)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set1")+
    labs(title = "Boat Participation by Landing Permit (# boats)", x=("Salmon Permit"), y=("Number of Vessels in Fishery"))+
    theme(legend.position = c(0.912, 0.93), text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  p3 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
    ungroup() %>% group_by(CFEC.Permit.Fishery, specialist_category) %>% summarise(avg.share = mean(share, na.rm = TRUE), nboats=mean(nboats, na.rm = TRUE)) %>%   
    ggplot(aes(x=CFEC.Permit.Fishery, y=avg.share, fill=specialist_category)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set1")+
    labs(title = "Boat Participation by Landing Permit (Share)", x=("Salmon Permit"), y=("Share of Vessels in Fishery"))+
    theme(legend.position = c(0.912, 0.93), text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  p2 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
    group_by(gear.class, specialist_category, Batch.Year) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
    mutate(share = nboats/total_boats) %>% 
    group_by(gear.class, specialist_category) %>% summarise(nboats = sum(nboats), avg.share = mean(share, na.rm = TRUE)) %>% 
    ggplot(aes(x=gear.class, y=nboats, fill=specialist_category)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Boat Participation by Fishing Strategy (# boats)", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Number of Vessels in Fishery"))+
    theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  p4 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S") %>% 
    group_by(gear.class, specialist_category, Batch.Year) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
    mutate(share = nboats/total_boats) %>% 
    group_by(gear.class, specialist_category) %>% summarise(nboats = sum(nboats), avg.share = mean(share, na.rm = TRUE)) %>%
    ggplot(aes(x=gear.class, y=avg.share, fill=specialist_category)) +
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Boat Participation by Fishing Strategy (Share)", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Share of Vessels in Fishery"))+
    theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  (p1/p3)
  (p2/p4)
  
}

{
  t1 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery, 3, 4)=="01") %>% 
    group_by(gear.class, specialist_category, Batch.Year) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
    mutate(share = nboats/total_boats) %>%
    ggplot(aes(x=Batch.Year, y=share, color=specialist_category)) +
    geom_line(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Boat Participation by Fishing Strategy (Share) - (01) purse seine", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Share of Vessels in Fishery"))+
    theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  t2 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery, 3, 4)=="02") %>% 
    group_by(gear.class, specialist_category, Batch.Year) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
    mutate(share = nboats/total_boats) %>%
    ggplot(aes(x=Batch.Year, y=share, color=specialist_category)) +
    geom_line(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Boat Participation by Fishing Strategy (Share) - (02) beach seine", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Share of Vessels in Fishery"))+
    theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  t3 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery, 3, 4)=="03") %>% 
    group_by(gear.class, specialist_category, Batch.Year) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
    mutate(share = nboats/total_boats) %>%
    ggplot(aes(x=Batch.Year, y=share, color=specialist_category)) +
    geom_line(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Boat Participation by Fishing Strategy (Share) - (03) drift gillnet", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Share of Vessels in Fishery"))+
    theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  t4 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery, 3, 4)=="04") %>% 
    group_by(gear.class, specialist_category, Batch.Year) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
    mutate(share = nboats/total_boats) %>%
    ggplot(aes(x=Batch.Year, y=share, color=specialist_category)) +
    geom_line(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Boat Participation by Fishing Strategy (Share) - (04) set gillnet", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Share of Vessels in Fishery"))+
    theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  t5 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery, 3, 4)=="05") %>% 
    group_by(gear.class, specialist_category, Batch.Year) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
    mutate(share = nboats/total_boats) %>%
    ggplot(aes(x=Batch.Year, y=share, color=specialist_category)) +
    geom_line(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Boat Participation by Fishing Strategy (Share) - (05) hand line/jig/troll", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Share of Vessels in Fishery"))+
    theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
  t6 <- particip_data %>% filter(substr(CFEC.Permit.Fishery,1,1)=="S", substr(CFEC.Permit.Fishery, 3, 4)=="15") %>% 
    group_by(gear.class, specialist_category, Batch.Year) %>% summarise(nboats = sum(nboats), total_boats = sum(total_boats)) %>% 
    mutate(share = nboats/total_boats) %>%
    ggplot(aes(x=Batch.Year, y=share, color=specialist_category)) +
    geom_line(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette = "Set2")+
    labs(title = "Boat Participation by Fishing Strategy (Share) - (15) power gurdy troll", x=("Aggregated Salmon Fisheries by Gear Class"), y=("Share of Vessels in Fishery"))+
    theme(text = element_text(size = 18),axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + 
    guides(fill=guide_legend(title="Fishing Strategy"))#width 1300, ht 600 works well
}

t1

