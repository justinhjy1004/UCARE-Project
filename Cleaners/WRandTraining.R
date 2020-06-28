# Import util files
source("C:/Users/JHo99/Box/UCARE/Code/Util/oes_util.R")
source("C:/Users/JHo99/Box/UCARE/Code/Util/jolts_util.R")
source("C:/Users/JHo99/Box/UCARE/Code/Util/bed_util.R")
source("C:/Users/JHo99/Box/UCARE/Code/Util/sd_income_util.R")

# Create Columns of the dataframe
NAICS <- NULL
period <- NULL
`Scale ID` <- NULL
training_index <- NULL
gain <- NULL
loss <- NULL
adj_realloc <- NULL

# Create empty dataframe of wr_training
df <- tibble(NAICS, period, `Scale ID`, training_index, gain,loss,adj_realloc)

for(naics in jolts_industry$industry_code){
  
  # Extract Time Series data for Training Indices
  
  pt <- pt_training(naics) %>%
    rename(period = year) %>%
    ungroup() %>%
    select(NAICS, period, `Scale ID`, training_index)
  
  oj <- oj_training(naics) %>%
    rename(period = year) %>%
    ungroup() %>%
    select(NAICS, period, `Scale ID`, training_index)
  
  edu <- education_level(naics) %>%
    rename(period = year) %>%
    ungroup() %>%
    select(NAICS, period, `Scale ID`, training_index)
  
  exp <- work_experience(naics) %>%
    rename(period = year) %>%
    ungroup() %>%
    select(NAICS, period, `Scale ID`, training_index)
  
  # Row bind all four training indices 
  combined_train <- rbind(pt,oj,edu,exp)
  
  # Parse period column into date time
  combined_train$period <- parse_date_time(combined_train$period, 'y')
  
  # Annualize worker reallocation rate  
  ann_realloc <- annualized_worker_realloc(naics)
  
  # Inner Join training indices to worker reallocation rate
  t <- inner_join(combined_train, ann_realloc, by = c('NAICS',"period"))
  
  # Row bind each industry to overall dataframe
  df <- rbind(df,t)
}

# Remove Financial Activities Rows
df <- df[df$industry != 'Financial activities',]

write.csv(df, file = 'C:/Users/JHo99/Box/UCARE/Data/Clean Data/WRandTraining.csv')
