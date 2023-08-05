# #C:/Users/josep/Box Sync/my_thesis/vessel_clean.R
# # data https://www.cfec.state.ak.us/plook/#vessels
# rm(list = ls())
# 
# #install packages not installed in your environment, some are included here in anticipation of future needs 
# packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap', 
#            'chron', 'zipcodeR', 'stringr','RColorBrewer', 'cowplot', 'ggcorrplot', "Hmisc")
# lapply(packs, require, character.only = T)
# 
# #vessel_list <- unique(vessel_zip_geo_complete$ADFG.Number)
# #length(vessel_list)
# #length(unique(df$ADFG.Number))
# #num_intl_boats <- length(unique(df$ADFG.Number)) - length(vessel_list)
# #num_intl_boats
# #the difference between the two lengths is that the cleaned data excludes internationally registered boats (outside the US). out of the ~7k international boats most are canadian
# setwd("C:/Users/josep/Box Sync/Thesis/data/clean_data/")
# setwd("C:/Users/Joseph Raymond/Box Sync/Thesis/data/clean_data/")
# vessel_zip_geo_complete <- read.csv("vessels_clean_1978_2022.csv")
# vessels_list <- readRDS("C:/Users/josep/Box Sync/Thesis/data/clean_data/vessels_list.rds")
# test <- vessels_list
# age <- c()
# for (i in 1:length(test)){
#   age <- c(age, nrow(test[[i]]))
# }
# hist_age <- hist(age,
#      main="Distribution of Maximum Boat Ages",
#      xlab="Number of Active Years",
#      ylab = "Boat Count",
#      breaks = 10,
#      col="darkmagenta",
# )
# text(hist_age$mids,hist_age$counts,labels=hist_age$counts, adj=c(0.5, -0.5))
# vessel_zip_geo_complete_slim <- vessel_zip_geo_complete %>% select(Year:Fuel.Capacity,Engine:Diving.Hand.Picking)
# vessel_zip_geo_complete_slim <- vessel_zip_geo_complete_slim %>% distinct(.keep_all = TRUE)
# Vessel_summary <- vessel_zip_geo_complete_slim %>% group_by(ADFG.Number) %>% summarise (built_year = min(Year.Built), 
#                                                                            operational_years=n(), 
#                                                                            min_year=min(Year), 
#                                                                            max_year=max(Year)) %>% mutate(age_boat=max_year-min_year+1) %>% mutate(any_inactive=(age_boat==operational_years))
# lag_data_limit <- vessel_zip_geo_complete_slim %>% group_by(ADFG.Number) %>% mutate(lag_year=lag(Year) ) %>% mutate(year_diff=Year-lag_year ) %>% select(ADFG.Number, Year,lag_year, year_diff, Year.Built) 





rm(list = ls())
packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap', 
           'chron', 'zipcodeR', 'stringr','RColorBrewer', 'cowplot', 'ggcorrplot', "Hmisc")
lapply(packs, require, character.only = T)



setwd("C:/Users/josep/Box Sync/Thesis/data/clean_data/")
setwd("C:/Users/Joseph Raymond/Box Sync/Thesis/data/clean_data/")
vessel_zip_geo_complete <- read.csv("vessels_clean_1978_2022.csv")

lag_data <- vessel_zip_geo_complete %>% group_by(ADFG.Number) %>% mutate(lag_year=lag(Year)) %>% mutate(year_diff=Year-lag_year ) %>% mutate(across(c(Year.Built:Fuel.Capacity), lag, .names = "lag.{.col}")) %>% mutate(across(c(Hull.Type:Diving.Hand.Picking), lag, .names = "lag.{.col}"))
vessel.var.names <- c(colnames(lag_data))
vessel.gear <- vessel.var.names[which(vessel.var.names %in% "Hull.Type"):which(vessel.var.names %in% "Diving.Hand.Picking")]

repeat_data <- lag_data %>% filter(year_diff==0)#repeat data set
nadata <- lag_data %>% filter(is.na(year_diff))
lag_data <- lag_data %>% mutate(new_reg_flag=is.na(year_diff))#this flag only works because non-registration years are not in the data. We only see a boat in the data if it is registered that year. Thus, other than the first year of data, all na in the year_diff represent the boat being newly registered

unique(vessel_zip_geo_complete$Year)#no missing or weird values for the Year variable

#in the command below, "filter(year_diff!=0)" takes out repeat entries. That is, if there is a repeat entry in the same year, the later entry is used and the first entry is discarded.
test <- lag_data %>% filter(year_diff!=0  | is.na(year_diff)) %>% select(Year,Length, ADFG.Number,Zip.Code, lag_year, Hull.Type:lag.Diving.Hand.Picking, starts_with("lag")) %>% group_by(ADFG.Number) %>% mutate(change=if_else(
  (Hull.Type!=lag.Hull.Type & !is.na(Hull.Type) & !is.na(lag.Hull.Type)) |
  (Refrigeration!=lag.Refrigeration & !is.na(Refrigeration) & !is.na(lag.Refrigeration)) |
  (Salmon.Registration.Area!=lag.Salmon.Registration.Area & !is.na(Salmon.Registration.Area) & !is.na(lag.Salmon.Registration.Area)) |
  (Freezer.Canner!=lag.Freezer.Canner & !is.na(Freezer.Canner) & !is.na(lag.Freezer.Canner)) |
  (Tender.Packer!=lag.Tender.Packer & !is.na(Tender.Packer) & !is.na(lag.Tender.Packer)) |
  (Charter!=lag.Charter & !is.na(Charter) & !is.na(lag.Charter)) |
  (Fishing!=lag.Fishing & !is.na(Fishing) & !is.na(lag.Fishing)) |
  (Purse.Seine!=lag.Purse.Seine & !is.na(Purse.Seine) & !is.na(lag.Purse.Seine)) |
  (Beach.Seine!=lag.Beach.Seine & !is.na(Beach.Seine) & !is.na(lag.Beach.Seine)) |
  (Drift.Gill.Net!=lag.Drift.Gill.Net & !is.na(Drift.Gill.Net) & !is.na(lag.Drift.Gill.Net)) |
  (Set.Gill.Net!=lag.Set.Gill.Net & !is.na(Set.Gill.Net) & !is.na(lag.Set.Gill.Net)) |
  (Hand.Troll!=lag.Hand.Troll & !is.na(Hand.Troll) & !is.na(lag.Hand.Troll)) |
  (Long.Line!=lag.Long.Line & !is.na(Long.Line) & !is.na(lag.Long.Line)) |
  (Otter.Trawl!=lag.Otter.Trawl & !is.na(Otter.Trawl) & !is.na(lag.Otter.Trawl)) |
  (Fish.Wheel!=lag.Fish.Wheel & !is.na(Fish.Wheel) & !is.na(lag.Fish.Wheel)) |
  (Pots!=lag.Pots & !is.na(Pots) & !is.na(lag.Pots)) |
  (Power.Troll!=lag.Power.Troll & !is.na(Power.Troll) & !is.na(lag.Power.Troll)) |
  (Beam.Trawl!=lag.Beam.Trawl & !is.na(Beam.Trawl) & !is.na(lag.Beam.Trawl)) |
  (Scallop.Dredge!=lag.Scallop.Dredge & !is.na(Scallop.Dredge) & !is.na(lag.Scallop.Dredge)) |
  (Mechanical.Jig!=lag.Mechanical.Jig & !is.na(Mechanical.Jig) & !is.na(lag.Mechanical.Jig)) |
  (Double.Otter.Trawl!=lag.Double.Otter.Trawl & !is.na(Double.Otter.Trawl) & !is.na(lag.Double.Otter.Trawl)) |
  (Herring.Gill.Net!=lag.Herring.Gill.Net & !is.na(Herring.Gill.Net) & !is.na(lag.Herring.Gill.Net)) |
  (Pair.Trawl!=lag.Pair.Trawl & !is.na(Pair.Trawl) & !is.na(lag.Pair.Trawl)) |
  (Diving.Hand.Picking!=lag.Diving.Hand.Picking & !is.na(Diving.Hand.Picking) & !is.na(lag.Diving.Hand.Picking)), 1, 0))

myfun <- function(i){
  return(if_else((i=="Yes" & !is.na(i)), 1, 0))}

lag_data2 <- test %>% filter(year_diff!=0 | is.na(year_diff)) %>% select(Year:Diving.Hand.Picking, change) %>%
  mutate(across(c(Refrigeration, Freezer.Canner:Diving.Hand.Picking), myfun, .names = "{.col}"))
level_cor <- lag_data2 %>% ungroup() %>%
  select(Freezer.Canner:change, Length) %>% 
  as.matrix() %>%
  rcorr(type = "spearman")

ggcorrplot(level_cor$r, hc.order = TRUE, outline.col = "white",
           ggtheme = ggplot2::theme_gray)
           + labs(title = "Correlation Matrix", subtitle="All variables depict existence in individual gear specifcation except for Length, Fishing, and Charter")




change_data <- test %>% filter(year_diff!=0 | is.na(year_diff)) %>% 
  mutate(Hull.Type = if_else((Hull.Type!=lag.Hull.Type & !is.na(Hull.Type) & !is.na(lag.Hull.Type)), 1, 0)) %>%
  mutate(Refrigeration = if_else((Refrigeration!=lag.Refrigeration & !is.na(Refrigeration) & !is.na(lag.Refrigeration)), 1, 0)) %>%
  mutate(Salmon.Registration.Area = if_else((Salmon.Registration.Area!=lag.Salmon.Registration.Area & !is.na(Salmon.Registration.Area) & !is.na(lag.Salmon.Registration.Area)), 1, 0)) %>%
  mutate(Freezer.Canner = if_else((Freezer.Canner!=lag.Freezer.Canner & !is.na(Freezer.Canner) & !is.na(lag.Freezer.Canner)), 1, 0)) %>%
  mutate(Tender.Packer = if_else((Tender.Packer!=lag.Tender.Packer & !is.na(Tender.Packer) & !is.na(lag.Tender.Packer)), 1, 0)) %>%
  mutate(Charter = if_else((Charter=="Yes" & !is.na(Charter)), 1, 0)) %>% #not change variables!!!!!!
  mutate(Fishing = if_else((Fishing=="Yes" & !is.na(Fishing)), 1, 0)) %>%
  mutate(Purse.Seine = if_else((Purse.Seine!=lag.Purse.Seine & !is.na(Purse.Seine) & !is.na(lag.Purse.Seine)), 1, 0)) %>%
  mutate(Beach.Seine = if_else((Beach.Seine!=lag.Beach.Seine & !is.na(Beach.Seine) & !is.na(lag.Beach.Seine)), 1, 0)) %>%
  mutate(Drift.Gill.Net = if_else((Drift.Gill.Net!=lag.Drift.Gill.Net & !is.na(Drift.Gill.Net) & !is.na(lag.Drift.Gill.Net)), 1, 0)) %>%
  mutate(Set.Gill.Net = if_else((Set.Gill.Net!=lag.Set.Gill.Net & !is.na(Set.Gill.Net) & !is.na(lag.Set.Gill.Net)), 1, 0)) %>%
  mutate(Hand.Troll = if_else((Hand.Troll!=lag.Hand.Troll & !is.na(Hand.Troll) & !is.na(lag.Hand.Troll)), 1, 0)) %>%
  mutate(Long.Line = if_else((Long.Line!=lag.Long.Line & !is.na(Long.Line) & !is.na(lag.Long.Line)), 1, 0)) %>%
  mutate(Otter.Trawl = if_else((Otter.Trawl!=lag.Otter.Trawl & !is.na(Otter.Trawl) & !is.na(lag.Otter.Trawl)), 1, 0)) %>%
  mutate(Fish.Wheel = if_else((Fish.Wheel!=lag.Fish.Wheel & !is.na(Fish.Wheel) & !is.na(lag.Fish.Wheel)), 1, 0)) %>%
  mutate(Pots = if_else((Pots!=lag.Pots & !is.na(Pots) & !is.na(lag.Pots)), 1, 0)) %>%
  mutate(Power.Troll = if_else((Power.Troll!=lag.Power.Troll & !is.na(Power.Troll) & !is.na(lag.Power.Troll)), 1, 0)) %>%
  mutate(Beam.Trawl = if_else((Beam.Trawl!=lag.Beam.Trawl & !is.na(Beam.Trawl) & !is.na(lag.Beam.Trawl)), 1, 0)) %>%
  mutate(Scallop.Dredge = if_else((Scallop.Dredge!=lag.Scallop.Dredge & !is.na(Scallop.Dredge) & !is.na(lag.Scallop.Dredge)), 1, 0)) %>%
  mutate(Mechanical.Jig = if_else((Mechanical.Jig!=lag.Mechanical.Jig & !is.na(Mechanical.Jig) & !is.na(lag.Mechanical.Jig)), 1, 0)) %>%
  mutate(Double.Otter.Trawl = if_else((Double.Otter.Trawl!=lag.Double.Otter.Trawl & !is.na(Double.Otter.Trawl) & !is.na(lag.Double.Otter.Trawl)), 1, 0)) %>%
  mutate(Herring.Gill.Net = if_else((Herring.Gill.Net!=lag.Herring.Gill.Net & !is.na(Herring.Gill.Net) & !is.na(lag.Herring.Gill.Net)), 1, 0)) %>%
  mutate(Pair.Trawl = if_else((Pair.Trawl!=lag.Pair.Trawl & !is.na(Pair.Trawl) & !is.na(lag.Pair.Trawl)), 1, 0)) %>%
  mutate(Diving.Hand.Picking = if_else((Diving.Hand.Picking!=lag.Diving.Hand.Picking & !is.na(Diving.Hand.Picking) & !is.na(lag.Diving.Hand.Picking)), 1, 0))

change_cor <- change_data %>% ungroup() %>%
  select(Hull.Type:Diving.Hand.Picking, Length, change) %>% filter(change==1) %>%
  as.matrix() %>%
  rcorr(type = "spearman")

ggcorrplot(change_cor$r, hc.order = TRUE, outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))+ labs(title = "Correlation Matrix", subtitle="All variables depict changes in individual gear specifcation except for Length, Fishing, and Charter")


test <- test %>% ungroup() %>% group_by(Year) %>% add_count()
test <- test %>% mutate(change_ratio=change/n)
p <- test %>% filter(change==1) %>%
  ggplot(aes(x=Year, fill=change)) +
  geom_bar(fill = "seagreen", show.legend = FALSE)+
  labs(title = "Gear Changes Over Time",
     subtitle = "Count of Confirmed Gear Changes by Year"
     #,caption = "Note: A confirmed gear change consists of a a boat changing any one of 24 specified gear specfications compared to set of gear on the boat's last registration. That is, suppose a boat was registered in 2000, not registered for ten years, and then came back in 2010 with an entirely different set of gear. This would count as one gear change in 2010"
     )
p

p2 <- test %>% ungroup() %>% group_by(Year) %>%
  summarize(year_change_ratio=sum(change_ratio)) %>%
  ggplot(aes(x=Year, y=year_change_ratio)) +
  geom_col(fill = "lightcoral", show.legend = FALSE)+
  labs(title = "Gear Changes Over Time",
       subtitle = "Proportion of Boats with Confirmed Gear Changes by Year"
       #,caption = "Note: A confirmed gear change consists of a a boat changing any one of 24 specified gear specfications compared to set of gear on the boat's last registration. That is, suppose a boat was registered in 2000, not registered for ten years, and then came back in 2010 with an entirely different set of gear. This would count as one gear change in 2010"
  )
p2

p3 <- test %>% ungroup() %>% group_by(Year) %>%
  summarize(TotalBoats=mean(n)) %>%
  ggplot(aes(x=Year, y=TotalBoats)) +
  geom_line(fill = "seagreen", show.legend = FALSE)+
  labs(title = "Number of Registered Boats"
       #,caption = "Note: A confirmed gear change consists of a a boat changing any one of 24 specified gear specfications compared to set of gear on the boat's last registration. That is, suppose a boat was registered in 2000, not registered for ten years, and then came back in 2010 with an entirely different set of gear. This would count as one gear change in 2010"
  )
p3

{
outlier_length <- 200
boat_size_distr1 <- lag_data %>% ungroup() %>% filter(Year==1981) %>%
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+
  scale_y_continuous(breaks = seq(0, 8000, 2000)) + 
  coord_cartesian(ylim = c(0, 8000)) + 
  labs(title = "Distribution of Boat Length (1981)")
boat_size_distr1

boat_size_distr2 <- lag_data %>% ungroup() %>% filter(Year==1986) %>% 
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+   scale_y_continuous(breaks = seq(0, 8000, 2000)) +
  coord_cartesian(ylim = c(0, 8000)) +
  labs(title = "Distribution of Boat Length (1986)",
       
  )
boat_size_distr2

boat_size_distr3 <- lag_data %>% ungroup() %>% filter(Year==1991) %>% 
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+   scale_y_continuous(breaks = seq(0, 8000, 2000)) +    coord_cartesian(ylim = c(0, 8000)) +
  labs(title = "Distribution of Boat Length (1991)",
       
  )
boat_size_distr3

boat_size_distr4 <- lag_data %>% ungroup() %>% filter(Year==1996) %>% 
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+   scale_y_continuous(breaks = seq(0, 8000, 2000)) +    coord_cartesian(ylim = c(0, 8000)) +
  labs(title = "Distribution of Boat Length (1996)",
       
  )
boat_size_distr4

boat_size_distr5 <- lag_data %>% ungroup() %>% filter(Year==2001) %>% 
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+   scale_y_continuous(breaks = seq(0, 8000, 2000)) +    coord_cartesian(ylim = c(0, 8000)) +
  labs(title = "Distribution of Boat Length (2001)",
       
  )
boat_size_distr5

boat_size_distr6 <- lag_data %>% ungroup() %>% filter(Year==2006) %>% 
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+   scale_y_continuous(breaks = seq(0, 8000, 2000)) +    coord_cartesian(ylim = c(0, 8000)) +
  labs(title = "Distribution of Boat Length (2006)",
       
  )
boat_size_distr6

boat_size_distr7 <- lag_data %>% ungroup() %>% filter(Year==2011) %>% 
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+   scale_y_continuous(breaks = seq(0, 8000, 2000)) +    coord_cartesian(ylim = c(0, 8000)) +
  labs(title = "Distribution of Boat Length (2011)",
       
  )
boat_size_distr7

boat_size_distr8 <- lag_data %>% ungroup() %>% filter(Year==2016) %>% 
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+   scale_y_continuous(breaks = seq(0, 8000, 2000)) +    coord_cartesian(ylim = c(0, 8000)) +
  labs(title = "Distribution of Boat Length (2016)",
       
  )
boat_size_distr8

boat_size_distr9 <- lag_data %>% ungroup() %>% filter(Year==2021) %>% 
  mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
  ggplot(aes(x=length_new)) +
  geom_histogram(bins = 20)+   scale_y_continuous(breaks = seq(0, 8000, 2000)) +    coord_cartesian(ylim = c(0, 8000)) +
  labs(title = "Distribution of Boat Length (2021)",
       
  )
boat_size_distr9

distr_length <- plot_grid(boat_size_distr1,boat_size_distr2,boat_size_distr3,boat_size_distr4,boat_size_distr5,boat_size_distr6,boat_size_distr7,boat_size_distr8,boat_size_distr9, ncol = 3)
}
distr_length
{
  boat_size_distr1 <- test %>% ungroup() %>% filter(between(Year, 1981,1985)) %>% filter(change==1) %>% 
    mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
    ggplot(aes(x=length_new)) +
    geom_histogram(binwidth = 10)+
    scale_y_continuous(breaks = seq(0, 7000, 1000)) + 
    scale_x_continuous(breaks = seq(0, 200, 25)) +
    coord_cartesian(ylim = c(0, 7000)) + 
    labs(title = "Distribution of Boat Length (1981-1985)")

  boat_size_distr2 <- test %>% ungroup() %>% filter(between(Year, 1986,1990)) %>% filter(change==1) %>% 
    mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
    ggplot(aes(x=length_new)) +
    geom_histogram(binwidth = 10)+ 
    scale_y_continuous(breaks = seq(0, 7000, 1000)) +
    scale_x_continuous(breaks = seq(0, 200, 25)) +
    coord_cartesian(ylim = c(0, 7000)) +
    labs(title = "Distribution of Boat Length (1986-1990)",
         
    )
  
  boat_size_distr3 <- test %>% ungroup() %>% filter(between(Year, 1991,1995)) %>% filter(change==1) %>% 
    mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
    ggplot(aes(x=length_new)) +
    geom_histogram(binwidth = 10)+
    scale_y_continuous(breaks = seq(0, 7000, 1000)) + 
    scale_x_continuous(breaks = seq(0, 200, 25)) +
    coord_cartesian(ylim = c(0, 7000)) +
    labs(title = "Distribution of Boat Length (1991-1995)",
         
    )
  
  boat_size_distr4 <- test %>% ungroup() %>% filter(between(Year, 1996,2000)) %>% filter(change==1) %>% 
    mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
    ggplot(aes(x=length_new)) +
    geom_histogram(binwidth = 10)+
    scale_x_continuous(breaks = seq(0, 200, 25)) +
    scale_y_continuous(breaks = seq(0, 7000, 1000)) +    coord_cartesian(ylim = c(0, 7000)) +
    labs(title = "Distribution of Boat Length (1996-2000)",
         
    )
  
  boat_size_distr5 <- test %>% ungroup() %>% filter(between(Year, 2001,2005)) %>% filter(change==1) %>% 
    mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
    ggplot(aes(x=length_new)) +
    geom_histogram(binwidth = 10)+
    scale_x_continuous(breaks = seq(0, 200, 25)) +
    scale_y_continuous(breaks = seq(0, 7000, 1000)) +    coord_cartesian(ylim = c(0, 7000)) +
    labs(title = "Distribution of Boat Length (2001-2005)",
         
    )
  
  boat_size_distr6 <- test %>% ungroup() %>% filter(between(Year, 2006,2010)) %>% filter(change==1) %>% 
    mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
    ggplot(aes(x=length_new)) +
    geom_histogram(binwidth = 10)+
    scale_x_continuous(breaks = seq(0, 200, 25)) +
    scale_y_continuous(breaks = seq(0, 7000, 1000)) +    coord_cartesian(ylim = c(0, 7000)) +
    labs(title = "Distribution of Boat Length (2006-2010)",
         
    )
  
  boat_size_distr7 <- test %>% ungroup() %>% filter(between(Year, 2011,2015)) %>% filter(change==1) %>% 
    mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
    ggplot(aes(x=length_new)) +
    geom_histogram(binwidth = 10)+
    scale_x_continuous(breaks = seq(0, 200, 25)) +
    scale_y_continuous(breaks = seq(0, 7000, 1000)) +    coord_cartesian(ylim = c(0, 7000)) +
    labs(title = "Distribution of Boat Length (2011-2015)",
         
    )
  
  boat_size_distr8 <- test %>% ungroup() %>% filter(between(Year, 2016,2020)) %>% filter(change==1) %>% 
    mutate(length_new = ifelse(Length > outlier_length, outlier_length, Length)) %>% 
    ggplot(aes(x=length_new)) +
    geom_histogram(binwidth = 10)+
    scale_x_continuous(breaks = seq(0, 200, 25)) +
    scale_y_continuous(breaks = seq(0, 7000, 1000)) +    coord_cartesian(ylim = c(0, 7000)) +
    labs(title = "Distribution of Boat Length (2016-2020)",
         
    )
  
  distr_length_change <- plot_grid(boat_size_distr1,boat_size_distr2,boat_size_distr3,boat_size_distr4,boat_size_distr5,boat_size_distr6,boat_size_distr7,boat_size_distr8, ncol = 3)
}
distr_length_change


cor_mat <- test %>% 
  do(data.frame(Cor=t(cor(.[,8:28], .[,8]))))


