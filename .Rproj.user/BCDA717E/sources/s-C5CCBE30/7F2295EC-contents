library(tidyverse)
library(ggpubr)
library(lubridate)

oes <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/Training_Index.csv')
oes_industry <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/oes.industry.csv')

# Subset OES datafram according to industry using NAICS Code
# input -> NAICS code (in numeric form)
# output -> subset of oes with NAICS value
subset_oes <- function(naics){
  t <- oes[oes$NAICS == naics &
           oes$year != '2019',]
  return(t)
}

# Given NAICS code, returns a time series dataframe
# of the Training indices of the idustry
id_training <- function(naics){
  # Obtain a subset of OES dataframe according to NAICS industry code
  t <- subset_oes(naics)
  
  # Based on NAICS Code, title, year, Element Name and Scale ID
  # Calculate training index of the Entire industry
  t <- t %>%
    group_by(NAICS, NAICS_TITLE,year,`Element Name`, `Scale ID`) %>%
    mutate(training_index = training_index*PCT_TOTAL*0.01) %>%
    summarise(training_index = sum(training_index))
  
  return(t)
}


# Scales each training index to [0,1], then aggregates the training indices
aggregate_training <- function(naics){
  
  t <- id_training(naics)
  
  # Add scale based on type of Training Idices
  t$scale <- 9
  t[t$`Scale ID` == 'PT',]$scale <- 9
  t[t$`Scale ID` == 'RL',]$scale <- 12
  t[t$`Scale ID` == 'RW',]$scale <- 11
  
  # Normalize training indices and then aggregate the values
  t <- t %>%
    mutate(training_index = training_index/scale) %>%
    group_by(NAICS, NAICS_TITLE,year) %>%
    summarise(training_index = sum(training_index))
  
  return(t)
}


# Returns on-the-job training index according to industry
oj_training <- function(naics){
  
  t <- id_training(naics)
  
  t <- t[t$`Scale ID` == 'OJ',]
  
  return(t)
}

# Returns required education level index according to industry
education_level <- function(naics){
  
  t <- id_training(naics)
  
  t <- t[t$`Scale ID` == 'RL',]
  
  return(t)
}

# Returns required on-site or in-plant training according to industry
pt_training <- function(naics){
  
  t <- id_training(naics)
  
  t <- t[t$`Scale ID` == 'PT',]
  
  return(t)
}

# Returns required relevant work experiences according to industry
work_experience <- function(naics){
  
  t <- id_training(naics)
  
  t <- t[t$`Scale ID` == 'RW',]
  
  return(t)
}

# Returns subset of dataframe that has training index 
# greater or equal to the mean training index
geq_training <- function(naics){
  
  j <- subset_oes(naics)
  
  # Calculate the Mean Training Index for each scale in the industry
  mean_index <- j %>%
    group_by(`Scale ID`) %>%
    summarise(mean_ti = mean(training_index))
  
  # Initialize column with FALSE
  j$geq_mean <- FALSE
  
  # Categorize the Training Index if Greater or Equal than mean Index 
  j[j$`Scale ID` == 'OJ' & 
      j$training_index >= mean_index[mean_index$`Scale ID` == 'OJ',]$mean_ti,]$geq_mean <- TRUE
  j[j$`Scale ID` == 'PT' & 
      j$training_index >= mean_index[mean_index$`Scale ID` == 'PT',]$mean_ti,]$geq_mean <- TRUE
  j[j$`Scale ID` == 'RW' & 
      j$training_index >= mean_index[mean_index$`Scale ID` == 'RW',]$mean_ti,]$geq_mean <- TRUE
  j[j$`Scale ID` == 'RL' & 
      j$training_index >= mean_index[mean_index$`Scale ID` == 'RL',]$mean_ti,]$geq_mean <- TRUE
  
  # Subset Jobs within OES GEQ than mean index
  j_true <- j[j$geq_mean == TRUE,]
  
  # Calculate Percentage total of employment for GEQ than mean index
  t <- j_true %>%
    group_by(year, `Scale ID`,`Element Name`) %>%
    summarise(PCT_EMP = sum(PCT_TOTAL))
  
  return(t)
}

# Returns a plot according to scale and industry of percentage employees
# with greater of equal amount of mean training index
visual_emp_pct_scale <- function(naics,scale){
  
  t <- geq_training(naics)
  
  g <- t[t$`Scale ID` == scale,]
  
  p <- ggplot(data = g) +
    geom_point(mapping = aes(x=parse_date_time(g$year,'y'), y=PCT_EMP)) +
    geom_line(mapping = aes(x=parse_date_time(g$year,'y'), y=PCT_EMP)) +
    xlab("Period") +
    ylab('Percentage Employment') +
    ggtitle(oes_industry[oes_industry$industry_code == naics,]$industry_name, subtitle = g$`Element Name`) +
    theme_minimal()
  
  return(p)
}

# Returns plots with Percentage Employment Greater 
# or Equal to Mean Training Index according to industry
visual_emp_pct <- function(naics){
  
  p1 <- visual_emp_pct_scale(naics,'PT')
  p2 <- visual_emp_pct_scale(naics,'OJ')
  p3 <- visual_emp_pct_scale(naics, "RW")
  p4 <- visual_emp_pct_scale(naics, "RL")
  
  p <- ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2) 
  p <- annotate_figure(p, top =  text_grob('Percentage Employment Greater or Equal to Mean Training Index', color = "black", face = "bold", size = 14))
  return(p)
  
}

# Returns individual plots for training index by industry and scale ID
visual_index_indv <- function(naics,j){
  
  p <- ggplot(data = j) +
    geom_point(mapping = aes(x=parse_date_time(j$year,'y'), y=training_index)) +
    geom_line(mapping = aes(x=parse_date_time(j$year,'y'), y=training_index)) +
    ggtitle(oes_industry[oes_industry$industry_code == naics,]$industry_name, subtitle = j$`Element Name`) +
    xlab('Period') +
    ylab('Training Index') +
    theme_minimal()
  
  return(p)
}

# Returns an aggregate plot with training indices across time
visual_indices <- function(naics){
  
  p1 <- visual_index_indv(naics,oj_training(naics))
  p2 <- visual_index_indv(naics,pt_training(naics))
  p3 <- visual_index_indv(naics,work_experience(naics))
  p4 <- visual_index_indv(naics,education_level(naics))
  
  p <- ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2) 
  p <- annotate_figure(p, top =  text_grob('Training Indices Across Time', color = "black", face = "bold", size = 14))
  return(p)
  
}

# Generate plots between INDIVIDUAL training index to income
# Annual Median income is used
visual_training_income_indv <- function(naics,scale){
  
  t <- subset_oes(naics)
  t <- t[t$`Scale ID` == scale,]
  
  p <- ggplot(data = t) +
    geom_point(mapping = aes(x=training_index,y=A_MEDIAN,colour = year)) +
    geom_smooth(method = 'lm',mapping = aes(x=training_index,y=A_MEDIAN), colour = 'red') +
    ggtitle(oes_industry[oes_industry$industry_code == naics,]$industry_name, t$`Element Name`) +
    xlab('Training Index') +
    ylab('Median Annual Income') +
    theme_minimal()
  
  return(p)
}

# Include all plots between training indices and Annual Median Income
visual_training_income <- function(naics){
  p1 <- visual_training_income_indv(naics, 'PT')
  p2 <- visual_training_income_indv(naics, 'OJ')
  p3 <- visual_training_income_indv(naics, 'RL')
  p4 <- visual_training_income_indv(naics, 'RW')
  
  p <- ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2) 
  p <- annotate_figure(p, top =  text_grob('Training Index and Median Annual Income', color = "black", face = "bold", size = 14))
  return(p)
}

# Generate Collinearity graphs between two different training indices
visual_collinear <- function(naics, id_1, id_2){
  
  collinear <- subset_oes(naics) %>%
    select(OCC_CODE, `Element Name`, `Scale ID`, training_index)
  
  # Subset the two indices to be compared
  t1 <- collinear[collinear$`Scale ID` == id_1,]
  t2 <- collinear[collinear$`Scale ID` == id_2,]
  
  # Join both dataframe by Occupation Code
  t <- distinct(inner_join(t1,t2, by = "OCC_CODE"))
  
  p <- ggplot(data = t) +
    geom_point(mapping = aes(x = training_index.x, y = training_index.y), colour = 'red') +
    geom_smooth(method = 'lm', mapping = aes(x = training_index.x, y = training_index.y)) +
    xlab(t$`Element Name.x`[1]) +
    ylab(t$`Element Name.y`[1]) +
    theme_minimal()
  
  return(p)
}


# Aggregate collinearity plots of each pair of training indices
collinear_combined <- function(naics){
  
  p1 <- visual_collinear(naics,'PT','OJ')
  p2 <- visual_collinear(naics,'PT','RL')
  p3 <- visual_collinear(naics,'PT','RW')
  p4 <- visual_collinear(naics,'OJ','RL')
  p5 <- visual_collinear(naics,'OJ','RW')
  p6 <- visual_collinear(naics,'RW','RL')
  
  p <- ggarrange(p1,p2,p3,p4,p5,p6, ncol = 3, nrow = 2) 
  p <- annotate_figure(p, top =  text_grob(paste('Collinearity of Training Indices in',oes_industry[oes_industry$industry_code == naics,]$industry_name), color = "black", face = "bold", size = 14))
  return(p)
  
}
