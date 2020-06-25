library(tidyverse)

df <- read_tsv('jt.data.1.AllItems.txt')
industry <- read_tsv('jt.industry.txt')
industry_code <- industry$industry_code
industry_name <- industry$industry_text

data_elem <- read_tsv('jt.dataelement.txt')
elem_code <- data_elem$dataelement_code
elem_name <- data_elem$dataelement_text

df <- df[-5]

df <- df[df$period != 'M13',]

df <- df[substr(df$series_id,3,3) == 'S',]

df <- df[substr(df$series_id,10,11) == '00',]

temp <- df[df$year == '1999',]
temp$industry <- ''

for(i in 1:28){
  t <- df[substr(df$series_id,4,9) == industry_code[i],]
  t$industry <- industry_name[i]
  temp <- rbind(temp, t)
}

df <- temp

temp$data_element <- ''

for(i in 1:8){
  t <- df[substr(df$series_id,12,13) == elem_code[i],]
  t$data_element <- elem_name[i]
  temp <- rbind(temp, t)
}

df <- temp

df.rate <- df[substr(df$series_id,14,14) == "R",]
df.rate$rate_level <- 'Rate'

df.level <- df[substr(df$series_id,14,14) == "L",]
df.level$rate_level <- 'Level'

df <- rbind(df.level,df.rate)

df$period <- substr(df$period,2,3)

df <- df %>% mutate(period = paste(year,period,'01',sep = '-'))

df <- df[-2]

write.csv(df, file = 'JOLTS.csv')
