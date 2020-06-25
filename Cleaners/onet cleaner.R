library(tidyverse)

df <- read_csv("ONET.csv")

occ_code <- unique(df$OCC_CODE)

scale_id <- unique(df$`Scale ID`)

df.output <- df[df$OCC_CODE == "df",]

for(i in 1:length(occ_code)){
  for(j in 1:length((scale_id))){
    t <- df[df$OCC_CODE == occ_code[i] & df$`Scale ID` == scale_id[j],]
    date.keep <- unique(t$Date)[1]
    t <- t[t$Date == date.keep,]
    df.output <- rbind(df.output, t)
  }
}

df.output <- distinct(df.output)

for(i in 1:length(occ_code)){
  a <- df.output[df.output$OCC_CODE == occ_code[i],]
  if(!(length(a$OCC_CODE) == 41 | length(a$OCC_CODE) == 43)){
   print(a$OCC_CODE) 
  }
}

write.csv(df.output,file = "ONET.csv")
