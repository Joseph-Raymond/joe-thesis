loadpacks <- function(){packs <- c('readr', 'tidyverse', 'lubridate', 'leaflet', 'dplyr', 'gtools', 'ggplot2', 'sf', 'scales', 'ggmap','chron', 'zipcodeR', 'stringr','RColorBrewer','cowplot', 'ggcorrplot', "Hmisc", 'patchwork', 'xlsx', 'fastDummies')
new.packages <- packs[!(packs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packs, require, character.only = T)}
getwd()

#using catch data, get the list of vessels that have fished using a salmon permit as well as a permit for some other fish species
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

get_onlysalmon <- function(path){
  df <- read.csv(file.path(path)) %>% group_by(Vessel.ADFG.Number) %>% filter(any(substr(CFEC.Permit.Fishery, 1, 1) == "S")) %>% arrange(by_group = TRUE) %>% ungroup()#filter VESSELS (not permits) that have active salmon permits in 2018
  {df$Date.Fishing.Began <- as.Date(as.character(df$Date.Fishing.Began),format = "%Y%m%d")
    df$Date.Landed <- as.Date(as.character(df$Date.Landed),format = "%Y%m%d")
    df$Week.Ending.Date <- as.Date(df$Week.Ending.Date,format = "%Y-%m-%d")
    df <- df %>% mutate(week = as.Date(cut(Date.Landed, "week")))}#format time variables
  df <- df %>% mutate(permit_species = substr(CFEC.Permit.Fishery, 1, 1))#create species letter from the permit
  df <- df  %>%  group_by(Vessel.ADFG.Number) %>% 
    filter(!any(permit_species!="S")) %>% #filter out landings that were done with a boat that fished anything that is not salmon
    ungroup()
  boatlist <- df %>% distinct(Vessel.ADFG.Number) %>% select(Vessel.ADFG.Number)
  return(boatlist)
}

second_max <- function(x){#function that returns the second largest value in a vector. need for the next function
  if(length(x)==1){return(NA)}
    else{return(sort(x)[length(x) - 1])}
}


get.firstpermit <- function(df){ #gets the first permit for each vessel-year; picking the permit for each vessel that has the most revenue in a given year
  df["CFEC.Value..Detail."][is.na(df["CFEC.Value..Detail."])] <- 0#fill the na's with 0
  df_test <- df %>% ungroup() %>%  group_by(Batch.Year, Vessel.ADFG.Number, CFEC.Permit.Fishery) %>% summarise(tot.revenue = sum(CFEC.Value..Detail.), s_specialist = first(s_specialist))%>% ungroup() %>% filter(CFEC.Permit.Fishery!="")
  #reduce on the permit variable based on the value of the summed revenue from the previous step
  df_test <- df_test %>% ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% summarise(first.permit = CFEC.Permit.Fishery[first(which(tot.revenue==max(tot.revenue)))], second.permit = ifelse(length(unique(CFEC.Permit.Fishery))==1, NA, CFEC.Permit.Fishery[first(which(tot.revenue==second_max(tot.revenue)))]), s_specialist = first(s_specialist), permit.fished.max = length(unique(CFEC.Permit.Fishery)))
  #df <- df %>% left_join(df_test, join_by(Batch.Year,Vessel.ADFG.Number)) #use if want to output the input data with the join. change the return data if you do this
  return(df_test)
}

get.firstpermit.alltime <- function(df){ #gets the first permit for each vessel across all time
  #"major" permit ranking function picking the permit for each vessel that has the most revenue in a given year
  df["CFEC.Value..Detail."][is.na(df["CFEC.Value..Detail."])] <- 0#fill the na's with 0
  df_test <- df %>% ungroup() %>%   group_by(Vessel.ADFG.Number, CFEC.Permit.Fishery) %>% summarise(tot.revenue = sum(CFEC.Value..Detail.), s_specialist = first(s_specialist))%>% ungroup() %>% filter(CFEC.Permit.Fishery!="")
  #reduce on the permit variable based on the value of the summed revenue from the previous step
  df_test <- df_test %>% ungroup() %>% group_by(Vessel.ADFG.Number) %>% summarise(first.permit = CFEC.Permit.Fishery[first(which(tot.revenue==max(tot.revenue)))], second.permit = ifelse(length(unique(CFEC.Permit.Fishery))==1, NA, CFEC.Permit.Fishery[first(which(tot.revenue==second_max(tot.revenue)))]), s_specialist = first(s_specialist), permit.fished.max = length(unique(CFEC.Permit.Fishery)))
  return(df_test)
}
# get.primeport <- function(df){
#   df <- df %>%
#     group_by(Vessel.ADFG.Number, Port.Code, Batch.Year) %>%
#     summarise(port.revenue = sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id)) %>%
#     mutate(revenue.per.trip = port.revenue/num.trips)
#   #reduce on the permit variable based on the value of the summed revenue from the previous step
#   df_test <- df %>%
#     ungroup() %>%
#     group_by(Vessel.ADFG.Number) %>%
#     summarise(prime.port = Port.Code[which(port.revenue==max(port.revenue))])
#   df <- df %>% left_join(df_test, by = join_by(Vessel.ADFG.Number)) %>% mutate(is.primeport = (Port.Code==prime.port))
#   return(df)
# }

get.primeport <- function(df){
  df <- df %>% 
    group_by(Vessel.ADFG.Number, Council.Port, Batch.Year) %>% 
    summarise(port.revenue = sum(CFEC.Value..Detail.), num.trips = n_distinct(trip.id), port.prod = sum(trip.prod)) %>% 
    mutate(revenue.per.trip = port.revenue/num.trips)
  #reduce on the permit variable based on the value of the summed revenue from the previous step
  df_test <- df %>% ungroup() %>% group_by(Batch.Year, Vessel.ADFG.Number) %>% #boat-year
    summarise(max.rev = max(port.revenue), Council.Port = Council.Port[first(which(port.revenue==max(port.revenue)))]) %>% 
    filter(max.rev>0) %>% 
    rename(prime.port=Council.Port)
  #summarise(max.rev = max(port.revenue))
  df <- df %>% ungroup() %>% left_join(df_test, by = join_by(Batch.Year, Vessel.ADFG.Number)) %>% mutate(is.primeport = (Council.Port==prime.port))
  return(df)
}

get.trip <- function(df){#requires following vairables: (Date.Fishing.Began, Date.Landed, Week.Ending.Date, Vessel.ADFG.Number)
  #df <- df %>% mutate(trip.id = group_indices(Vessel.ADFG.Number,Date.Landed))
  catch_data_temp <- catch_data_temp %>% mutate(CFEC.Value..Detail. = if_else(is.na(CFEC.Value..Detail.),0,CFEC.Value..Detail.))
  df <- df %>% ungroup() %>% group_by(Vessel.ADFG.Number,Date.Landed) %>% mutate(trip.id = cur_group_id()) %>% ungroup()
  Date.Fishing.Began <- as.Date(as.character(df$Date.Fishing.Began),format = "%Y%m%d")
  Date.Landed <- as.Date(as.character(df$Date.Landed),format = "%Y%m%d")
  #Week.Ending.Date <- as.Date(df$Week.Ending.Date,format = "%Y-%m-%d")
  tlength <- difftime(Date.Landed, Date.Fishing.Began, units="days")
  df <- df %>% mutate(trip.length = tlength)#format time variables
  return(df)
}

get.trip.new <- function(df){#requires following vairables: (Date.Fishing.Began, Date.Landed, Week.Ending.Date, Vessel.ADFG.Number)
  df <- df %>% mutate(CFEC.Value.Detail = if_else(is.na(CFEC.Value.Detail),0,CFEC.Value.Detail)) %>% 
    group_by(Vessel.ADFG.Number,Date.Landed) %>% mutate(day.vessel.id = cur_group_id()) %>% 
    ungroup() %>% 
    mutate(Date.Fishing.Began = as.Date(as.character(Date.Fishing.Began),format = "%Y%m%d"), Date.Landed = as.Date(as.character(Date.Landed),format = "%Y%m%d"), trip.length = difftime(Date.Landed, Date.Fishing.Began, units="days")) %>% 
  return(df)
}


split_permit <- function(df){
  df <- df %>% separate(Permit.Fishery, 
             into = c("Permit.Species", "Permit.Gear.Region"), 
             sep = "(?<=[A-Za-z])(?=[0-9])", remove = FALSE)
  df <- df %>% separate(Permit.Gear.Region, 
                        into = c("Permit.Gear", "Permit.Region"), 
                        sep = "(?<=[0-9])(?=[A-Za-z])")
  return(df)
}

get_permit_cities <- function(path){
  data_dir <- path
  datalist <- list.files(data_dir, pattern = "*.csv")
  citylist <- data.frame('Home.Port.City'=character(),'Home.Port.State'=character(), check.names = FALSE)
  for(i in datalist){
    df <- read.csv(file.path(paste0(data_dir, i))) %>% select(Home.Port.City, Home.Port.State) %>% distinct()
    citylist <- bind_rows(citylist, df) %>% distinct()
    #print(i)
  }
  return(citylist)
}

which.median = function(x) {
  if (length(x) %% 2 != 0) {
    which(x == median(x))
  } else if (length(x) %% 2 == 0) {
    a = sort(x)[c(length(x)/2, length(x)/2+1)]
    c(which(x == a[1]), which(x == a[2]))
  }
}

mode <- function(codes){
  which.max(tabulate(codes))
}


get.effort.markers <- function(df){
  df <- df %>%
    mutate(
      week.number = as.integer(strftime(Date.Landed, "%V"))
    )
  df.trip.duration <- df %>% 
    mutate(days.in.year = case_when(
      is.na(Date.Landed) ~ NA_integer_,
      leap_year(Date.Landed) ~ 366L,
      TRUE ~ 365L
    )) %>% 
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery, day.vessel.id) %>% 
    summarise(trip.duration = first(trip.duration), week.number = first(week.number), days.in.year = first(days.in.year)) %>% 
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number) %>% 
    mutate(annual.vessel.active.days = n_distinct(day.vessel.id)) %>% 
    group_by(Batch.Year, CFEC.Vessel.Owner.Filing.Number, Permit.Fishery) %>%
    mutate(active.days.ratio = sum(trip.duration,na.rm = TRUE)/annual.vessel.active.days, 
           total.days.ratio = sum(trip.duration,na.rm = TRUE)/days.in.year, 
           active.weeks = n_distinct(week.number))
  return(df.trip.duration)
}#calculates the number of the week when fishing started and also ended
#outputs the share of total and active weeks for which the permit owner was active in each fishery
#fishing_communities <- read.csv("~/JoeData/PortNames/fishing_communities.csv")
#fishing_communities <- fishing_communities %>% group_by(COMMUNITY) %>% summarise(lat = first(LATITUDE), long = first(LONGITUDE))
#test <- get_permit_cities(path = "~/JoeData/vessels/")
#test <- test %>% distinct()
#test <- test %>% left_join()
cvar_lower <- function(x, q = 0.25) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  thr <- as.numeric(quantile(x, probs = q, type = 7, na.rm = TRUE))
  mean(x[x <= thr], na.rm = TRUE)
}

# Decompose the avg-annual-HHI effect into L (HHI^LR) and P (Phi; sum of variance of shares) contributions (within-FE FWL)
decompose_avgH_effect <- function(df, yvar,
                                  Lvar = "lr.hhi", Pvar = "rot_var",
                                  fe_owner = "CFEC.Vessel.Owner.Filing.Number",
                                  fe_period = "period") {
  
  # 1) Build the same sample you use in your regressions
  df <- df %>%
    filter(is.finite(.data[[yvar]]),
           is.finite(.data[[Lvar]]),
           is.finite(.data[[Pvar]])) %>%
    mutate(
      y = log(.data[[yvar]]),
      L = .data[[Lvar]],
      P = .data[[Pvar]],
      z = L + P
    ) %>%
    filter(is.finite(y), is.finite(z))
  
  # 2) Two-way demean (owner + period) to match FE identification
  fe_list <- list(df[[fe_owner]], df[[fe_period]])
  yw <- fixest::demean(df$y, fe_list)
  Lw <- fixest::demean(df$L, fe_list)
  Pw <- fixest::demean(df$P, fe_list)
  zw <- Lw + Pw
  
  # guard against zero variance in z
  vz <- var(zw, na.rm = TRUE)
  if (!is.finite(vz) || vz <= 0) {
    return(tibble(yvar = yvar, beta_barH = NA_real_,
                  beta_L = NA_real_, beta_P = NA_real_,
                  pct_L = NA_real_, pct_P = NA_real_))
  }
  
  # 3) Within simple slope of y on z = L+P
  beta_barH <- cov(yw, zw, use = "complete.obs") / vz
  
  # 4) Within partial slopes on L and P (OLS on demeaned vars)
  ok <- is.finite(yw) & is.finite(Lw) & is.finite(Pw)
  X  <- cbind(L = Lw[ok], P = Pw[ok])
  yv <- yw[ok]
  b  <- as.numeric(solve(crossprod(X), crossprod(X, yv)))
  names(b) <- c("beta_L","beta_P")
  
  # 5) Projection weights of L and P onto z within-FE
  pi_L <- cov(Lw, zw, use = "complete.obs") / vz
  pi_P <- cov(Pw, zw, use = "complete.obs") / vz
  
  # 6) Contributions and percentage shares (sum to ~100% up to rounding)
  contrib_L <- b["beta_L"] * pi_L
  contrib_P <- b["beta_P"] * pi_P
  share_L   <- contrib_L / beta_barH
  share_P   <- contrib_P / beta_barH
  
  tibble(
    yvar      = yvar,
    beta_barH = beta_barH,
    beta_L    = b["beta_L"],
    beta_P    = b["beta_P"],
    pct_L     = 100 * share_L,
    pct_P     = 100 * share_P
  )
}

