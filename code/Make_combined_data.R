#make combined_data
# List of required packages
packages <- c(
  'readr', 'tidyverse', 'dplyr', 'ggplot2', 'xlsx', 'data.table',
  'fixest', 'lubridate', 'corrplot', 'RColorBrewer', 'knitr',
  'cluster', 'factoextra', 'zoo', 'purrr', 'ggtext'
)

# Install missing packages
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) install.packages(to_install)

# Load all packages quietly
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
wd_dir <- "/home/akfin/jraymond/Rprojects/joe-thesis"
setwd(wd_dir)
source("code/myfunctions.R")

{
  data_dir <- "./../../../"
  #want to load data filter it by salmon boats then store that year of data in a list
  datalist <- list.files(data_dir, pattern = "*.csv")
  
  # Get CSV files
  csv_files <- list.files(data_dir, pattern = "*.csv", full.names = TRUE)
  
  # Process all files
  process_csv_fast <- function(file_path) {
    tryCatch({
      fread(file_path) %>% 
        as.data.frame() %>% 
        select('Batch Year', contains("Home"), contains("Owner"), contains("Area"), contains("area"), contains("CFEC Permit Holder"), 'Pre-print Ticket', 'Ticket Type', "Vessel ADFG Number", "Date Landed", "Date Fishing Began", "AKR Vessel Length", "Port Code", "Port Name", "Port State", "Council Port", "Species Code", "Species Common Name", "CFEC Species Code", "CFEC PACFIN Species Code", "CFEC Permit Year":"Permit Serial Number", "BLEND Target Group":"CFEC Whole Pounds (Detail)") %>%
        mutate(
          across(c("Vessel ADFG Number", "Permit Serial Number", "Permit Year Sequence"), ~ as.numeric(.x)),
          across(c("Pre-print Ticket", "CFEC Vessel Owner Zip", "CFEC Vessel Owner Historical Zip", "CFEC Vessel Owner Historical Zip", "CFEC Permit Holder Zip", "Statistical Area"), ~ as.character(.x))
        )
      
    }, error = function(e) {
      warning("Failed to process: ", basename(file_path))
      return(NULL)
    })
  }
  
  # Now the processing becomes much simpler:
  catch_data <- map(csv_files, process_csv_fast) %>%
    set_names(basename(csv_files)) %>%
    compact()
  
  # Combine all data frames into a single data frame
  combined_catch_data <- bind_rows(catch_data, .id = "source_file") %>% select(-source_file)
  intermediate_dir <- file.path(wd_dir, "intermediate data")
  # Save as RDS with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(intermediate_dir, paste0("combined_catch_data_", timestamp, ".rds"))
  
  # Print summary
  cat("Successfully processed", length(catch_data), "files\n")
  cat("Combined data has", nrow(combined_catch_data), "rows and", ncol(combined_catch_data), "columns\n")
  cat("Saved to:", output_file, "\n")
  cat("File size:", round(file.size(output_file) / 1024^2, 2), "MB\n")

  colnames(combined_catch_data) <- gsub("[()]", "", 
                                  gsub(" ", ".", 
                                       gsub("%", "percent", 
                                            gsub("#", "num",
                                                 colnames(combined_catch_data)))))
  
  combined_catch_data$Vessel.ADFG.Number[which(combined_catch_data$Vessel.ADFG.Number==62.39)] <- 62339#fix a typo in the data
  combined_catch_data <- combined_catch_data %>% filter(Vessel.ADFG.Number!=0) #8 missing values with vessel numbers == 0
  combined_catch_data <- combined_catch_data %>% mutate(Vessel.ADFG.Number = as.integer(Vessel.ADFG.Number))
  combined_catch_data <- combined_catch_data %>% mutate(CFEC.Value.Detail = if_else(is.na(CFEC.Value.Detail),0,CFEC.Value.Detail))
  #filter out boats that made landings but did not report the permit/fishery in which they made the landing
  combined_catch_data <- combined_catch_data %>% group_by(Vessel.ADFG.Number, Batch.Year) %>% filter(!any(is.na(Permit.Serial.Number)) ) %>% ungroup()
  output_file <- file.path(intermediate_dir, paste0("combined_data_", timestamp, ".rds"))
  saveRDS(combined_catch_data, file = output_file, compress = "gzip")
}
