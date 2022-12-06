#creates subfolder ('jraymond') in the data folder. then loads the data from the parent data folder ('akfin') into a list of data frames. the memory is all kept on the subfolder
#test change
rm(list = ls())
path2home <- ""#my available directory stops at "home" so this is just blank
datapath <- "/home/akfin/"
sub_dir <- "jraymond"
main_dir <- paste(path2home, datapath, sep = "")

# check if sub directory exists and if it doesn't then create it.
if (file.exists(file.path(main_dir, sub_dir))){
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))} else {
  # create a new sub directory inside
  # the main path
  dir.create(file.path(main_dir, sub_dir))
  # specifying the working directory
  setwd(file.path(main_dir, sub_dir))}

filenames <- list.files(path = "../", pattern="*.csv")
filepaths <- (file.path("..",filenames))#navigate up with the ".."
filepaths# view the files to get the ones you would like

i <- 28#corresponds to 2018 data file
#i <- 28:29# 2018 : 2019
myfiles <- filepaths[i]#select 2018 files as a test
dlist <- lapply(myfiles, read.csv)


#filter / clean the data you need here
getwd()#MAKE SURE THE WD IS IN THE CORRECT PLACE!! Must be a subfolder of your data folder
save(dlist, file = "data_2018.RData")

rm(list = ls()) #clear memory

#Packages not required for the loading/saving of the data
#loads my packages. If not installed in your environment, the missing ones are installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc")
  new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(packs, require, character.only = T)}

#"ssh -L localhost:8989:localhost:3389 jraymond@makena.ucdavis.edu", and then connect to "localhost:8989"
# git reset HEAD~   #command to go back one commit on the local branch