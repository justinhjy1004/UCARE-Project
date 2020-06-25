library(tidyverse)
library(hash)

oes <- read_csv("oes.csv")
onet <- read_csv("oNET.csv")
industry <- read_csv("oes.industry.csv")

industry_map <- hash()
industry_code <- as.character(industry$industry_code)
industry_name <- industry$industry_name

for(i in 1:length(industry_code)){
  industry_map[[industry_code[i]]] <- industry_name[i]
}

for(i in 1:length(industry_code)){
  sector_code <- industry_code[i]
  sector_name <- industry_name[i]
  
  file.output <- paste(sector_code,sector_name,"OES_ONET.csv", sep = " ")
  
  df <- oes[oes$NAICS == sector_code,]
  
  df <- inner_join(df,onet, by = "OCC_CODE")
  
  write.csv(df, file = file.output)
}

