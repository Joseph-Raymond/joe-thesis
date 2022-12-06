#creates subfolder ('jraymond') in the data folder. then loads the data from the parent data folder ('akfin') into a list of data frames. the memory is all kept on the subfolder
#
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
#loads my packages. If not installed in your environment, the missing ones gte installed
{packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc")
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}

filenames <- list.files(path = "../", pattern="*.csv")
filepaths <- (file.path("..",filenames))#navigate up with the ".."
#myfiles <- myfiles[1:2]#select the first two files as a test
df <- lapply(myfiles, read.delim)

#

rm(list = ls()) #clear memory
#"ssh -L localhost:8989:localhost:3389 jraymond@makena.ucdavis.edu", and then connect to "localhost:8989"