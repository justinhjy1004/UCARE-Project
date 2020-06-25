library(tidyverse)
library(lubridate)

for (index in 3:21){
  
  industry <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/oes.industry.csv')
  input <- paste(industry$industry_code[index], industry$industry_name[index], 'OES_ONET.csv', sep = ' ')
  oes.manufacturing <- read_csv(paste('C:/Users/JHo99/Box/UCARE/Data/Clean Data/ONET-OES/',input, sep = ''))
  
  df <- oes.manufacturing[-1]
  df <- df[df$OCC_CODE %% 10 != 0,]
  
  #training index
  p <- df %>% 
    group_by(NAICS, NAICS_TITLE, OCC_CODE, OCC_TITLE, TOT_EMP, PCT_TOTAL, A_MEAN,
             A_MEDIAN, year, `Element Name`, `Scale ID`) %>%
    mutate(training_index = as.numeric(Category)*`Data Value`*0.01) %>%
    summarise(training_index = sum(training_index))
  
  #adjust to employment percentage
  adj_emp <- p %>%
    group_by(year, `Scale ID`) %>%
    summarise(industry_employment = sum(TOT_EMP))
  adj_emp <- adj_emp[adj_emp$`Scale ID` != 'IM',]
  
  #empty dataframe
  t <- p[p$year == '1999',]
  
  #dataframe with adjusted employment
  for(i in 1:length(adj_emp$year)){
    temp <- p[p$year == adj_emp$year[i] & p$`Scale ID` == adj_emp$`Scale ID`[i],]
    temp$PCT_TOTAL <- (temp$TOT_EMP/adj_emp$industry_employment[i])*100
    t <- rbind(t,temp)
  }
  t$A_MEAN <- as.numeric(t$A_MEAN)
  t$A_MEDIAN <- as.character(t$A_MEDIAN)
  df.total <- rbind(t,df.total)
  
}

write.csv(df.total, file = 'Training_Index.csv')

index <- 2
industry <- read_csv('C:/Users/JHo99/Box/UCARE/Data/Clean Data/oes.industry.csv')
input <- paste(industry$industry_code[index], industry$industry_name[index], 'OES_ONET.csv', sep = ' ')
oes.manufacturing <- read_csv(paste('C:/Users/JHo99/Box/UCARE/Data/Clean Data/ONET-OES/',input, sep = ''))

df <- oes.manufacturing[-1]
df <- df[df$OCC_CODE %% 10 != 0,]

#training index
p <- df %>% 
  group_by(NAICS, NAICS_TITLE, OCC_CODE, OCC_TITLE, TOT_EMP, PCT_TOTAL, A_MEAN,
           A_MEDIAN, year, `Element Name`, `Scale ID`) %>%
  mutate(training_index = as.numeric(Category)*`Data Value`*0.01) %>%
  summarise(training_index = sum(training_index))

#adjust to employment percentage
adj_emp <- p %>%
  group_by(year, `Scale ID`) %>%
  summarise(industry_employment = sum(TOT_EMP))
adj_emp <- adj_emp[adj_emp$`Scale ID` != 'IM',]

#empty dataframe
t <- p[p$year == '1999',]

#dataframe with adjusted employment
for(i in 1:length(adj_emp$year)){
  temp <- p[p$year == adj_emp$year[i] & p$`Scale ID` == adj_emp$`Scale ID`[i],]
  temp$PCT_TOTAL <- (temp$TOT_EMP/adj_emp$industry_employment[i])*100
  t <- rbind(t,temp)
}
t$A_MEAN <- as.numeric(t$A_MEAN)
t$A_MEDIAN <- as.character(t$A_MEDIAN)
df.total <- rbind(t,df.total)





