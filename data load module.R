
rm(list = ls())
path2home <- "test"
datapath <- "/home/akfin/"
sub_dir <- "jraymond"
main_dir <- paste(path2home, datapath, sep = "")

# check if sub directory exists and if it doesn't then create it.
if (file.exists(sub_dir)){
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
} else {
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, sub_dir))
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))
}
#loads my packages. If not installed in your environment, the missing ones gte installed
packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc")
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)


maxyear <- 2021
minyear <- 1991
years <- minyear:maxyear


load_data <- function(xx){
  data_list <- list()
  for (i in xx){
    temp_name <- paste(c(as.character(i), " fish ticket data (revised 4-14-17)",".csv"), collapse = "")#change this for different file name structures
    temp_df <- read.csv(paste(path2home,datapath,temp_name, sep = ""))#get absolute path because the wd is one folder deeper (/home/akfin/jraymnond/)
    data_list <- append(data_list, temp_df)
    print(paste(path2home,datapath,temp_name, sep = ""))
  }
  return(data_list)
}
df <- load_data(years)

rm(list = ls())
#"ssh -L localhost:8989:localhost:3389 jraymond@makena.ucdavis.edu", and then connect to "localhost:8989"