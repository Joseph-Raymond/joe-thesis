
rm(list = ls())
path2home <- ""
datapath <- "home/akfin/"
myfolder <- "jraymond"

setwd(paste(path2home, datapath, sep = ""))
dir.create(myfolder)

#install packages not installed in your environment, some are included here in anticipation of future needs
packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap',
           'chron', 'zipcodeR', 'stringr','RColorBrewer', 'cowplot', 'ggcorrplot', "Hmisc")
lapply(packs, require, character.only = T)


maxyear=1980
mminyear=2018
years <- minyear:maxyear


load_data <- function(x){
  data_list <- list()
  for (i in x){
    temp_name <- paste(c("akfin_", x[i],"_database.csv"), collapse = "")#change this for different file name structures
    temp_df <- read.csv(paste(c(path2home,datapath,temp), sep = ""))#get absolute path because the wd is one folder deeper (/home/akfin/jraymnond/)
    data_list <- list.append(data_list, temp_df)
  }
  return(data_list)
}
load_data(years)

for (i in 1:length(test)){
  age <- c(age, nrow(test[[i]]))
  tempfile <- commonpath +"year" +".csv"
  df <- list()
  temp <- read.csv(tempfile)
  df <- df.append()
}
