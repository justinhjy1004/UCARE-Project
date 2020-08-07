library(tidyverse)
library(lubridate)

# # From URL
# jolts <- read_csv(url('https://unl.box.com/shared/static/npyjhxlfhaif6wbja14m3dfr2ljxc8mz.csv'))
# jolts <- jolts[-1]
# jolts_industry <- read_csv(url('https://unl.box.com/shared/static/sgu028wja5eack5yqnbmt11lanc66bt5.csv'))
# jolts_industry <- jolts_industry[-1]
# 
# wr_training <- read_csv(url('https://unl.box.com/shared/static/d2led7ztj2qiakosdbox9o5r8pehjyoc.csv'))


# From Computer
jolts <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/JOLTS.csv')
jolts <- jolts[-1]
jolts_industry <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/jolts_industry.csv')
jolts_industry <- jolts_industry[-1]

wr_training <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/WRandTraining.csv')
wr_training <- wr_training %>%
  select(-X1)

# Subset JOLTS dataframe function
subset_jolts <- function(NAICS, data_element, rate_level = 'Rate'){
  t <- jolts[jolts$NAICS == NAICS &
             jolts$data_element == data_element &
             jolts$rate_level == rate_level,]
  
  upper_date <- parse_date_time('2001', 'y')
  lower_date <- parse_date_time('2020', 'y')
  
  t <- t[t$period >= upper_date &
         t$period < lower_date,]
  
  return(t)
}

# Generate Worker Reallocation Rate (Monthly) 
# Input NAICS code from jolts_industry
worker_realloc_rate <- function(NAICS){
  # Obtain hires df
  hires <- subset_jolts(NAICS, "Hires") %>%
    select(period, value, industry, NAICS) %>%
    rename(hires = value)
  
  # Obtian total separations df
  separations <- subset_jolts(NAICS, "Total separations") %>%
    select(period, value, industry, NAICS) %>%
    rename(sep = value)
  
  # Join hires and separations then calculate Worker Reallocation Rate
  realloc <- inner_join(hires, separations, by = c('period', 'industry', 'NAICS')) %>%
    mutate(realloc_rate = hires + sep)
  
  return(realloc)
}

# Annualize Worker Reallocation Rate based on NAICS (jolt_industry)
# Worker Reallocation Rate is not adjusted!!!
annualized_worker_realloc <- function(NAICS){
  # Obtain hires dataframe
  hires <- subset_jolts(NAICS, "Hires") %>%
    select(-series_id, -data_element, -rate_level) %>%
    rename(hires = value)
  
  # Extract year from period column
  hires$period <- substr(as.character(hires$period), 1,4)
  
  # Add up total hires rate over the year
  hires <- hires %>%
    group_by(period, industry, NAICS) %>%
    summarize(hires = sum(hires))
  
  # Obtain separations dataframe
  separations <- subset_jolts(NAICS, "Total separations") %>%
    select(-series_id, -data_element, -rate_level) %>%
    rename(sep = value)
  
  # Extract year from period column
  separations$period <- substr(as.character(separations$period), 1,4)
  
  # Add up total separations rate over the year
  separations <- separations %>%
    group_by(period, industry, NAICS) %>%
    summarize(sep = sum(sep))
  
  # Join hires and separations and calucalate worker reallocation rate
  df <- inner_join(hires, separations, by = c("NAICS", 'period', 'industry')) %>%
    mutate(realloc_rate = sep + hires)
  
  # Parse NAICS and period columns into numeric and datetime object respectively
  df$NAICS <- as.numeric(df$NAICS)
  df$period <- parse_date_time(df$period, 'y')
  
  return(df)
}


# Add quits column on wr_training
# TODO: Data annualized not divided
annualized_quits <- function(){
  data_element <- 'Quits'
  
  t <- jolts[jolts$data_element == data_element &
               jolts$rate_level == 'Rate',] %>%
    select(period, value, industry, NAICS) %>%
    mutate(period = substr(as.character(period),1,4)) %>%
    group_by(period, industry, NAICS) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    mutate(period = parse_date_time(period, 'y')) %>%
    mutate(NAICS = as.numeric(NAICS)) %>%
    rename(quits = value)
  
  wr_training$period <- parse_date_time(wr_training$period, 'ymd')
  
  p <- inner_join(wr_training, t, by = c("industry", 'NAICS', 'period')) %>%
    select(NAICS, period, `Scale ID`, industry, training_index, quits)
  
  return(p)
}

# Add layoffs to column on wr_training
# TODO: Data annualized not divided
annualized_layoffs <- function(){
  data_element <- 'Layoffs and discharges'
  
  t <- jolts[jolts$data_element == data_element &
               jolts$rate_level == 'Rate',] %>%
    select(period, value, industry, NAICS) %>%
    mutate(period = substr(as.character(period),1,4)) %>%
    group_by(period, industry, NAICS) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    mutate(period = parse_date_time(period, 'y')) %>%
    mutate(NAICS = as.numeric(NAICS)) %>%
    rename(layoffs = value)
  
  wr_training$period <- parse_date_time(wr_training$period, 'ymd')
  
  p <- inner_join(wr_training, t, by = c("industry", 'NAICS', 'period')) %>%
    select(NAICS, period, `Scale ID`, industry, training_index, layoffs)
  
  return(p)
}


# Add Other Discharges to column on wr_training
# TODO: Data annualized not divided
annualized_other <- function(){
  data_element <- 'Other separations'
  
  t <- jolts[jolts$data_element == data_element &
               jolts$rate_level == 'Rate',] %>%
    select(period, value, industry, NAICS) %>%
    mutate(period = substr(as.character(period),1,4)) %>%
    group_by(period, industry, NAICS) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    mutate(period = parse_date_time(period, 'y')) %>%
    mutate(NAICS = as.numeric(NAICS)) %>%
    rename(other = value)
  
  wr_training$period <- parse_date_time(wr_training$period, 'ymd')
  
  p <- inner_join(wr_training, t, by = c("industry", 'NAICS', 'period')) %>%
    select(NAICS, period, `Scale ID`, industry, training_index, other)
  
  return(p)
}
