library(tidyverse)
library(lubridate)

df.industry <- read_csv("oes.industry.csv")

i <- 4

industry_code <- df.industry$industry_code[i]
industry_name <- df.industry$industry_name[i]

file.input <- paste(industry_code,industry_name,"OES_ONET.csv", sep = " ")

df <- read_csv(file.input)
df <- df[-1]
df <- df[df$OCC_CODE %% 10 != 0,]

#calculate industry contribution of training for each job
temp <- df %>% 
          group_by(NAICS, NAICS_TITLE, OCC_CODE, OCC_TITLE, TOT_EMP,     
          PCT_TOTAL, A_MEAN,, A_MEDIAN, year,  `Element Name`,
          `Scale ID`) %>%
          summarise(weighted_val = sum(as.numeric(Category)*`Data Value`*0.01)) %>%
          mutate(industry_contribution = weighted_val*PCT_TOTAL*0.01) 

#Recorded employment
p <- temp %>%
    group_by(year, `Scale ID`) %>%
    summarize(emp_recorded = sum(PCT_TOTAL))

p <- p[p$`Scale ID` == "RL",]
year <- p$year
emp_rec <- p$emp_recorded

#median level of training
q <- temp %>%
  group_by(year,`Scale ID`) %>%
  summarize(median_training = median(weighted_val))

#mean level of training
r <- temp %>%
  group_by(year,`Scale ID`) %>%
  summarize(mean_training = mean(weighted_val))

#mean for each group
mean.id <- temp %>%
  group_by(`Scale ID`) %>%
  summarise(mean_training = mean(weighted_val))

t.mean <- temp[temp$weighted_val > 3.978616 & temp$`Scale ID` == "OJ",]

z <- t.mean[t.mean$year == "2016",]
sum(z$PCT_TOTAL)

#training index by year
t <- temp %>% 
          group_by(NAICS, NAICS_TITLE, year,`Element Name`,
                   `Scale ID`) %>%
          summarise(training_index = sum(industry_contribution))

#adjust for employment record
t$emp_rec <- 0
for(j in 1:length(year)){
  t[t$year == year[j],]$emp_rec <- emp_rec[j]
}

t <- t %>%
  mutate(emp_adj = 100/emp_rec) %>%
  mutate(adj_training = training_index*emp_adj)

t$year <- parse_date_time(t$year, "y")

ggplot(data = t) +
  geom_point(mapping = aes(x = year, y = adj_training, colour = `Scale ID`))

