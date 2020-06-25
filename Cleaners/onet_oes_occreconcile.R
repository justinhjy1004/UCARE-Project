library(tidyverse)
library(MALDIquant)

oes <- read_csv("C:/Users/JHo99/Box/UCARE/Data/Clean Data/oes.csv")
onet <- read_csv("C:/Users/JHo99/Box/UCARE/Data/Clean Data/ONET.csv") 

#remove for all occupation code, and unique onet occupation not in oes
oes <- oes[oes$OCC_CODE != "0",]
onet <- onet[onet$OCC_CODE != 453021,]

#as numerical
oes$OCC_CODE <- as.numeric(oes$OCC_CODE)
onet$OCC_CODE <- as.numeric(onet$OCC_CODE)

diff.occ <- setdiff(oes$OCC_CODE,onet$OCC_CODE)
onet.occ <- unique(onet$OCC_CODE)

for(i in diff.occ){
  closestLoc = match.closest(i, onet.occ)
  closestVal <- onet.occ[closestLoc]
  
  onet.duplicate <- onet[onet$OCC_CODE == closestVal,]
  onet.duplicate$OCC_CODE <- i
  
  onet <- rbind(onet, onet.duplicate)
}

write.csv(onet,)

    