library(tidyverse)
library(lubridate)

# # From URL
# bed <- read_csv(url('https://unl.box.com/shared/static/h6ixr5gli5pcew2a5egmmybu89h0jrh7.csv'))
# bed <- bed[-1]
# bed$NAICS <- as.numeric(bed$NAICS)
# bed_industry <- read_csv(url('https://unl.box.com/shared/static/dneof6j9qk8087fr5nvhi0vtn58mpcnw.csv'))
# jr_training <- read_csv(url('https://unl.box.com/shared/static/n9sc07385n072dkwtjd3c3tn1kgmvgqj.csv'))

# From Computer
bed <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/bed.csv')
bed <- bed[-1]
bed$NAICS <- as.numeric(bed$NAICS)
bed_industry <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/bed_industry.csv')
jr_training <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/JRandTraining.csv')

# Subset function for BED dataframe
subset_bed <- function(state = 'U.S. totals',
                       NAICS,
                       dataclass,
                       dataelement = 'Employment',
                       ratelevel = 'Rate'){
  
  t <- bed[bed$state == state &
           bed$NAICS == NAICS &
           bed$dataclass == dataclass &
           bed$dataelement == dataelement &
           bed$rate_level == ratelevel,]
  
  return(t)
}

# # Generate job reallocation rate dataframe given NAICS (bed_industry)
# # Subsectors of industry included (without following bed_industry NAICS)
# job_realloc_rate <- function(NAICS){
#   
#   # Gross Job Gains Rate
#   gain <- subset_bed('U.S. totals', NAICS, "Gross Job Gains", 'Employment', 'Rate')
#   
#   # Gross Job Losses Rate
#   loss <- subset_bed('U.S. totals', NAICS, "Gross Job Losses", 'Employment', 'Rate')
#   
#   # Select specific columns for Gains and Losses
#   gain <- gain %>%
#     select(period, value, industry, NAICS)
#   loss <- loss %>%
#     select(period, value, industry, NAICS)
#   
#   # Join gains and losses and calculate the reallocation rate
#   realloc <- inner_join(gain, loss, by = c('period', 'industry', 'NAICS')) %>%
#     rename(gain = value.x, loss = value.y) %>%
#     mutate(realloc_rate = gain + loss)
#   
#   return(realloc)
# }

# Adjust the job reallocation rate by including all subsectors of larger industry (bed_industry)
# Takes in industry code NAICS and dataclass
# Returns Rate and Level of dataframe
rate_adjuster <- function(NAICS, dataclass){
  
  # Subset BED dataframe by Rate 
  rate <- subset_bed('U.S. totals', NAICS, dataclass, 'Employment', 'Rate') %>%
    rename(rate = value) %>%
    select(-rate_level,-series_id)
  
  # Subset BED dataframe by Level
  lvl <- subset_bed('U.S. totals', NAICS, dataclass, 'Employment', 'Level') %>%
    rename(level = value) %>%
    select(-rate_level,-series_id)
  
  # Inner join rate and level
  df <- inner_join(lvl, rate, by = c('period', 'state', "industry", "dataclass", "dataelement", "NAICS")) %>%
    # Using BED Employment Formula to calculate rate to obtain estimated employment
    mutate(est_employment = (level/rate)*100) %>%
    group_by(period) %>%
    # Estimate total industry employment by summing subsectors of larger industry
    mutate(industry_emp = sum(est_employment)) %>%
    # Create weights for each subsector 
    mutate(weight = est_employment/industry_emp) %>%
    # Adjust rate to weighted
    mutate(weighted_rate = weight * rate) %>%
    # Sum the adjusted rate of industry for total industry rate
    group_by(period, dataclass, NAICS) %>%
    summarize(adj_rate = sum(weighted_rate))
  
  return(df)
}

# Return dataframe with adjusted job reallocation rate
adj_job_realloc <- function(NAICS){
  # Obtain job gains
  gains <- rate_adjuster(NAICS, 'Gross Job Gains') %>%
    rename(gain = adj_rate) %>%
    ungroup() %>%
    select(-dataclass)
  
  # Obtain job losses
  loss <- rate_adjuster(NAICS, 'Gross Job Losses') %>%
    rename(loss = adj_rate) %>%
    ungroup() %>%
    select(-dataclass)
  
  # Join gains and losses and calculate adjusted job reallocation rate
  t <- inner_join(gains, loss, by = c("period", "NAICS")) %>%
    mutate(adj_realloc = gain + loss)
  
  return(t)
}

# plot job reallocation rate (Quarterly)
plot_job_realloc <- function(NAICS){
  df <- adj_job_realloc(NAICS)
  
  p <- ggplot(data = df) +
    geom_point(mapping = aes(x = period, y= adj_realloc), colour = 'red') +
    geom_smooth(method = 'lm', mapping = aes(x = period, y= adj_realloc)) + 
    ggtitle(paste('Job Reallocation Rate of', bed_industry[bed_industry$x == NAICS,]$industry_name)) +
    ylab('Job Reallocation Rate') +
    theme_minimal()
  
  return(p)
}

# Annualize job reallocation rate 
# TODO: CHECK FOR CALCULATION, CURRENTLY DIVIDE THE ANNUALIZED RATE BY 4
annualize_job_realloc <- function(NAICS){
  
  # Obtain adjusted job reallocation rate
  df <- adj_job_realloc(NAICS)
  
  # Extract the year from date and treat as character
  df$period <- as.character(df$period)
  df$period <- substr(df$period,1,4)
  
  # Add up the gains, losses and adjusted reallocation rate by Year and NAICS
  df <- df %>%
    group_by(period, NAICS) %>%
    summarize(gain = sum(gain)/4.0, loss = sum(loss)/4.0, adj_realloc = sum(adj_realloc)/4.0)
  
  # Convert period into data time object
  df$period <- parse_date_time(df$period, 'y')
  
  return(df)
}
