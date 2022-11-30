
rm(list = ls())
path2home <- ""
setwd(path2home)
#install packages not installed in your environment, some are included here in anticipation of future needs
packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap',
           'chron', 'zipcodeR', 'stringr','RColorBrewer', 'cowplot', 'ggcorrplot', "Hmisc")
lapply(packs, require, character.only = T)
setwd("C:/Users/josep/Box Sync/Thesis/data/clean_data/")

for (i in 1:length(test)){
  age <- c(age, nrow(test[[i]]))
  tempfile <- commonpath +"year" +".csv"
  df <- list()
  temp <- read.csv(tempfile)
  df <- df.append()
}
