library(tidyverse)
library(hash)

df <- read_tsv("bd.data.0.Current.txt")
df.county <- read_tsv("bd.county.txt")
df.dataclass <- read_tsv("bd.dataclass.txt")
df.dataelement <- read_tsv("bd.dataelement.txt")
df.industry <- read_tsv("bd.industry.txt")
df.msa <- read_tsv("bd.msa.txt")
df.ownership <- read_tsv("bd.ownership.txt")
df.periodicity <- read_tsv("bd.periodicity.txt")
df.ratelevel <- read_tsv("bd.ratelevel.txt")
df.seasonal <- read_tsv("bd.seasonal.txt")
df.sizeclass <- read_tsv("bd.sizeclass.txt")
df.state <- read_tsv("bd.state.txt")
df.unitanalysis <- read_tsv("bd.unitanalysis.txt")

df <- subset(df,substr(df$series_id,3,3) == "S")
df <- subset(df,substr(df$series_id,22,23) == "00")

#ratelevel and dataElement
df <- subset(df,substr(df$series_id,21,21) == "2")
df <- subset(df,substr(df$series_id,26,26) == "R")

filename <- "bd.establishment.rate.csv"

#state column
state_code <- df.state$state_code
state_name <- df.state$state_name

state_map <- hash()

for(i in 1:length(state_code)){
  state_map[[state_code[i]]] <- state_name[i]
}

for(i in 1: length(df$series_id)){
  df$state[i] <- values(state_map, substr(df$series_id[i],9,10))[[1]]
}

#industry column
industry_code <- df.industry$industry_code
industry_name <- df.industry$industry_name

industry_map <- hash()

for(i in 1:length(industry_code)){
  industry_map[[industry_code[i]]] <- industry_name[i]
}

for(i in 1: length(df$series_id)){
  df$industry[i] <- values(industry_map, substr(df$series_id[i],14,19))[[1]]
}

write.csv(df, file = filename)
