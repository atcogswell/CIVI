#Install packages and libraries ####
install.packages("xlsx")
library(xlsx)

#Set working directory ####
setwd("C:/Users/CogswellA/Documents/AZMP/Coastal Vulnerability Index/2017")

#Import degree of protection data from excel and put into 1 file and export
dp<-NULL

for (i in 3:7){
  
  dptemp<-read.xlsx("CIVI Degree of Protection All.xlsx", sheetIndex=i, colIndex=c(1:4))
  dptemp<-na.omit(dptemp)
  dp<-rbind(dp,dptemp)
}