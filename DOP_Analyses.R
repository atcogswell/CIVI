#Install packages and libraries ####
install.packages("xlsx")
library(xlsx)

#Set working directory ####
setwd("C:/Users/CogswellA/Documents/AZMP/Coastal Vulnerability Index/2017")


#Import degree of protection data from excel, rename the fields and put into 1 file and export ####
dp<-NULL


#Revised spreadsheet for Degree of Protection for SCH provided by Emily Bird on November 30, 2016
#I've not included the .xlsx data in the project because of restrictions on these data
#Noted that there is no PEI data nor completed Quebec data

#Combine all spreadsheets into one data frame and omit na data
for (i in 3:7){
  
  dptemp<-read.xlsx("CIVI Degree of Protection All.xlsx", sheetIndex=i, colIndex=c(1:4))
  dptemp<-na.omit(dptemp)
  dp<-rbind(dp,dptemp)
}

#rename the fields
names(dp)<-c("Harbour_Name", "Province", "Harbour_Code", "Degree_of_Protection") 

# create another Province field to house the shorted code for province for anlaysis
dp$Province_short<-dp$Province 

#new varialbes for long and short province names
plev<-levels(dp$Province_short)
pabrv<-c("BC", "AB", "MB", "NT", "NU", "ON", "SK", "QC", "NB", "NS", "NL")

#rename the short province field
for (i in 1:length(pabrv)){
  
  levels(dp$Province_short)[levels(dp$Province_short)==plev[i]] <- pabrv[i]
  
}

#export these combined data to csv if you choose ####
#write.csv(dp, "CIVIDegreeofProtectionAll_161201.csv", row.names=F)


#select only west and east coast data and provide summary stats and pie charts ####
dpsel<-subset(dp, dp$Province_short=="BC"|dp$Province_short=="NB"|dp$Province_short=="NS"|dp$Province_short=="QC"|dp$Province_short=="NL")

#remove zero degree of protection values associated with buoys
dpsel<-subset(dpsel,dpsel$Degree_of_Protection>0)
dpsel$Coast<-ifelse(dpsel$Province_short=="BC","West","East")

# provide data summaries by selected provinces and by coast
by(dpsel$Degree_of_Protection,dpsel$Province_short,summary)
by(dpsel$Degree_of_Protection,dpsel$Coast,summary)


# Pie Chart from data frame with Appended Sample Sizes and percentages for the east and west coasts ####
westtab<-table(subset(dpsel$Degree_of_Protection,dpsel$Coast=="West"))
pct <- round(westtab/sum(westtab)*100)
wlbls <- paste("DOP = ",names(westtab), "\n", "Count = ",westtab," or ", pct, "%", sep="")
pie(westtab, labels = wlbls, main=paste("Pie Chart of West Coast Degree of Protection\n (with sample sizes and percentages)","\n","n = ", sum(westtab),sep=""))

easttab<-table(subset(dp$Degree_of_Protection,dpsel$Coast=="East"))
pct <- round(easttab/sum(easttab)*100)
elbls <- paste("DOP = ",names(easttab), "\n", "Count = ",easttab," or ", pct, "%", sep="")
pie(easttab, labels = elbls, main=paste("Pie Chart of East Coast Degree of Protection\n (with sample sizes and percentages)","\n","n = ", sum(easttab),sep=""))



# Pie Chart from data frame with Appended Sample Sizes and percentages for NL and NS, NB and QC combined ####
NLtab<-table(subset(dpsel$Degree_of_Protection,dpsel$Province_short=="NL"))
pct <- round(NLtab/sum(NLtab)*100)
NLlbls <- paste("DOP = ",names(NLtab), "\n", "Count = ",NLtab," or ", pct, "%", sep="")
pie(NLtab, labels = NLlbls, main=paste("Pie Chart of NL Degree of Protection\n (with sample sizes and percentages)","\n","n = ", sum(NLtab),sep=""))

martab<-table(subset(dp$Degree_of_Protection,dpsel$Province_short=="NS"|dpsel$Province_short=="NB"|dpsel$Province_short=="QC"))
pct <- round(martab/sum(martab)*100)
marlbls <- paste("DOP = ",names(martab), "\n", "Count = ",martab," or ", pct, "%", sep="")
pie(martab, labels = marlbls, main=paste("Pie Chart of Maritimes Degree of Protection\n (with sample sizes and percentages)","\n","n = ", sum(martab),sep=""))
