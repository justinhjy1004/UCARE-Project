source("C:/Users/JHo99/Box/UCARE/Code/Util/oes_util.R")
source("C:/Users/JHo99/Box/UCARE/Code/Util/jolts_util.R")
source("C:/Users/JHo99/Box/UCARE/Code/Util/bed_util.R")
source("C:/Users/JHo99/Box/UCARE/Code/Util/sd_income_util.R")

NAICS <- NULL
period <- NULL
`Scale ID` <- NULL
training_index <- NULL
gain <- NULL
loss <- NULL
adj_realloc <- NULL

df <- tibble(NAICS, period, `Scale ID`, training_index, gain,loss,adj_realloc)

for(naics in bed_industry$x){
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
  
  combined_train <- rbind(pt,oj,edu,exp)
  
  combined_train$period <- parse_date_time(combined_train$period, 'y')
  
  ann_realloc <- annualize_job_realloc(naics)
  
  t <- inner_join(combined_train, ann_realloc, by = c('NAICS',"period"))
  df <- rbind(df,t)
}

write.csv(df, file = 'JRandTraining.csv')