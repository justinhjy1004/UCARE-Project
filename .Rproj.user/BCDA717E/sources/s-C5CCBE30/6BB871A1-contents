library(tidyverse)
library(ggpubr)

sd_income <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/income_sdestimate.csv')
oes_industry <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/oes.industry.csv')

# Subset SD Income Dataframe
subset_sd_income <- function(naics, id){
  
  t <- sd_income[sd_income$NAICS == naics & sd_income$`Scale ID` == id,]
  return(t)
  
}

# Visualize individual Training Index to the SD of income
visual_sd_income <- function(naics, id){
  
  t <- subset_sd_income(naics,id)
  
  p <- ggplot(data = t) +
    geom_point(mapping = aes(x=scale_training, y=sd_estimate, colour = year)) +
    geom_smooth(method = 'lm',mapping = aes(x=scale_training, y=sd_estimate), colour = 'red') +
    xlab('Training Index') +
    ylab('Estimate Standard Deviations of Income') +
    ggtitle(t$`Element Name`[1], subtitle = oes_industry[oes_industry$industry_code == naics,]$industry_name) +
    theme_minimal()
  
  return(p)
  
}

# Aggregate Plots of visuals of Training Indices
sd_income_combined <- function(naics){
  p1 <- visual_sd_income(naics, "OJ")
  p2 <- visual_sd_income(naics, "PT")
  p3 <- visual_sd_income(naics, "RW")
  p4 <- visual_sd_income(naics, "RL")
  
  p <- ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2) 
  p <- annotate_figure(p, top =  text_grob('Training Index and Estimated Standard Deviation of Annual Income', color = "black", face = "bold", size = 14))
  return(p)
}

# Visualize individual Training Index to the Normalized SD of income
visual_sd_norm <- function(naics, id){
  
  t <- subset_sd_income(naics,id)
  
  p <- ggplot(data = t) +
    geom_point(mapping = aes(x=scale_training, y=sd_norm, colour = year)) +
    geom_smooth(method = 'lm',mapping = aes(x=scale_training, y=sd_norm), colour = 'red') +
    xlab('Training Index') +
    ylab('Normalized Standard Deviations of Income') +
    ggtitle(t$`Element Name`[1], subtitle = oes_industry[oes_industry$industry_code == naics,]$industry_name) +
    theme_minimal()
  
  return(p)
  
}

# Aggregate Plots of visuals of Normalized Training Indices
sd_norm_combined <- function(naics){
  p1 <- visual_sd_norm(naics, "OJ")
  p2 <- visual_sd_norm(naics, "PT")
  p3 <- visual_sd_norm(naics, "RW")
  p4 <- visual_sd_norm(naics, "RL")
  
  p <- ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2) 
  p <- annotate_figure(p, top =  text_grob('Training Index and Estimated Normalized Standard Deviation of Annual Income', color = "black", face = "bold", size = 14))
  return(p)
}

