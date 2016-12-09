library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(foreign)
library(dplyr)
library(Rcpp)
library(matrixStats)

wd <- setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Final Pilot Report/Shapefiles")

# This section reads in a list of possible shapefiles from the working directory
list<-list.files(pattern="*^.*.csv$")

#data for sites (as) that includes harbour condition and facility replacement values that are not NA for either category or 0 for costs

as_tot<-read.csv("HarbourFacitlityandInfrastructureConditionSummaryPRGCoordinates_151204_orig.csv") # you'll need to check this as it will change as more csv files are added



as_atl<-read.csv("HarbourFacitlityandInfrastructureConditionSummaryPRGCoordinates_151204_orig_atl_clip.csv") # you'll need to check this as it will change as more csv files are added
as_atl<-as_atl[,c(2,3,8,7,5,6)]
names(as_atl)<-c("Har_Name","Har_Code","Lon", "Lat", "HC","TRC")
as_atl<-dplyr::arrange(as_atl,TRC)


#################### Replacement costs Atlantic#############################
#plot(as_atl$TRC, xlab="SCH Assets - Ordered Lowest to Highest Replacement Costs", ylab="Total SCH Replacement Costs - $", main="Atlantic SCH Replacement Costs Distribution")
#abline(v=1e+07, lty=2, col="blue")
hist(as_atl$TRC,xlab="Total SCH Replacement Costs - $", ylab="Frequency of SCH at $10 million Increments", main="Atlantic SCH Replacement Costs Distribution")
abline(v=1e+07, lty=2, col="blue")

as_atl_g<-subset(as_atl,as_atl$TRC>=1e+07) #locations valued more than 10 million
dim(as_atl_g)
as_atl_g_perc<-(sum(as_atl_g$TRC)/sum(as_atl$TRC))*100 #% of total replacement costs represented by properties less than $10 million dollars
as_atl_l<-subset(as_atl,as_atl$TRC<1e+07) #locations values less than 10 million
dim(as_atl_l)
as_atl_l_perc<-(sum(as_atl_l$TRC)/sum(as_atl$TRC))*100
trc_ratio_atl<-(nrow(as_atl_g)/nrow(as_atl_l))*100

as_atl_0<-subset(as_atl,as_atl$TRC==0)


as_atl$root6TRC<-(as_atl$TRC)^(1/6) #transformation roughly normalizes data and spreads out the dataset so low values can be included
hist(as_atl$root6TRC)

     
as_atl$root6TRC_cut<-cut(as_atl$root6TRC, breaks = 5)
as_atl$root6TRC_cut_ord<-as.integer(cut(as_atl$root6TRC, breaks = 5, labels=1:5))

png(paste("root6TRC_cut_atl",'.png',sep=""), height=800,width=1600) #starts the process of creating a 2 panel png that includes histogram of original data and cut data
par(mfrow=c(1,3), cex=1.3)
hist(as_atl$TRC, main="Atlantic SCH Replacement Costs",xlab="Dollars")
abline(v=c(0,7.06e+03,4.49e+05,5.05e+06,2.87e+07,1.10e+08),lty=2, col="blue")
hist(as_atl$root6TRC, main="1/6th Root of Atlantic SCH Replacement Cost",xlab="To the power of 1/6 Dollars")
abline(v=c(0,4.38,8.75,13.1,17.5,21.9),lty=2, col="blue")
temp<-hist(as_atl$root6TRC_cut_ord, main="Recoded Transformed SCH Replacement Cost",xlab="Bins - Cut power 1/6 $",xaxt="n", breaks=0:5)
axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
dev.off()


#################### Harbour Condition Atlantic #############################
# Harbour condition contains some values below 1 and zeros.  To have it conform, values were rated 1 through 5 using cut function
# Harbour condition is currently assessed from SCH as 0 = poor condition and 5 = good condition.  This is the opposite scale used for the other variables where 5 is more vulnerable and 1 is less vulnerable.
# For this I flipped the classification values for Harbour code to match the other codes where 1 = good conition and 5 = bad.
test<-as.character(as_atl$HC)
as_atl$HC[as_atl$HC=="#N/A"]<-NA
as_atl$HC<-as.character(as_atl$HC)
as_atl$HC<-as.numeric(as_atl$HC)
hist(as_atl$HC)
as_atl_HC_0<-subset(as_atl,as_atl$HC==0)
dim(as_atl_HC_0)
as_atl<-dplyr::arrange(as_atl,HC)
as_atl$HC_trans<-as_atl$HC^1.6
as_atl$HC_cut_ord<-as.integer(cut(as_atl$HC_trans, breaks = 5, labels=1:5)) #transform the data because slightly left skewed, apply power function 1.5
as_atl$HC_cut<-cut(as_atl$HC_trans, breaks = 5)

hist(as_atl$HC_cut_ord)
as_atl$HC_cut_ord_rc<-as_atl$HC_cut_ord
as_atl <- transform(as_atl,HC_cut_ord_rc=ifelse(as_atl$HC_cut_ord == 1, 5, HC_cut_ord_rc))
as_atl <- transform(as_atl,HC_cut_ord_rc=ifelse(as_atl$HC_cut_ord == 2, 4, HC_cut_ord_rc))
as_atl <- transform(as_atl,HC_cut_ord_rc=ifelse(as_atl$HC_cut_ord == 3, 3, HC_cut_ord_rc))
as_atl <- transform(as_atl,HC_cut_ord_rc=ifelse(as_atl$HC_cut_ord == 4, 2, HC_cut_ord_rc))
as_atl <- transform(as_atl,HC_cut_ord_rc=ifelse(as_atl$HC_cut_ord == 5, 1, HC_cut_ord_rc))
hist(as_atl$HC_cut_ord_rc)

write.csv(as_atl,"RECODED_ATLANTIC.csv")


#low value means good condition and high value is poor condition in recoded version
png(paste("HC_Cut_Atl",'.png',sep=""), height=1200,width=1200) #starts the process of creating a 2 panel png that includes histogram of original data and cut data
par(mfrow=c(2,2), cex=1.2)
hist(as_atl$HC, main="Atlantic Extent SCH Harbour Condition",xlab="Harbour Condition")
abline(v=c(0,1.83,2.82,3.63,4.34,5),lty=2,col="blue")
hist(as_atl$HC_trans,main="Atlantic Extent Transformed (^1.6)", xlab="Transformed Harbour Condition")
abline(v=c(0,2.63,5.25,7.88,10.5,13.1),lty=2,col="blue")
temp<-hist(as_atl$HC_cut_ord, main="Atlantic Extent Recoded 1-5",xlab="Recoded Harbour Condition",xaxt="n", breaks=0:5)
axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
temp2<-hist(as_atl$HC_cut_ord_rc, main="Atlantic Extent Recoded & Flipped 1-5",xlab="Recoded/Flipped Harbour Condition",xaxt="n", breaks=0:5)
axis(side=1,at=temp2$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
dev.off()

#subset of entire database provided by SCH - names were slightly different than the original list provided by SCH for pilot sites
as_atl_sub<-subset(as_atl,grepl('AULDS CO',as_atl$Har_Name)|grepl('CENTREVILLE ',as_atl$Har_Name)|grepl('INGALLS HEAD',as_atl$Har_Name)|
                 grepl('MACHONS POINT',as_atl$Har_Name)|grepl('METEGHAN',as_atl$Har_Name)|grepl('NORTH HEAD',as_atl$Har_Name)|
                 grepl('PINKNEY',as_atl$Har_Name)|grepl('SAINTE-MARIE-SUR-MER',as_atl$Har_Name)|grepl('MONT-LOUIS OUEST',as_atl$Har_Name)|
                 grepl('SEAL COVE-FISHERMEN',as_atl$Har_Name)|grepl('BARTLETTS HARBOUR',as_atl$Har_Name)|Har_Name=="BAULINE"|
                 grepl('BAMFIELD WEST',as_atl$Har_Name)|grepl('COWICHAN BAY',as_atl$Har_Name)|grepl('ETANG-DU',as_atl$Har_Name)|
                 grepl('LES ESCOUM',as_atl$Har_Name)|grepl('POOL',as_atl$Har_Name)|grepl('TOURELLES',as_atl$Har_Name)|grepl('TWILLIN',as_atl$Har_Name))
as_atl_sub<-dplyr::arrange(as_atl_sub,Har_Name) 
as_atl_sub$ID=as.numeric(rownames(as_atl_sub))

#include section in here to bind the larger dataset with the small pilot site dataset using file: PilotSite_2015_ExposureSub_Index_forGM_new.csv

#################### Degree of Facility Protection #############################
ps<-read.csv("CIVI Pilot Sites Infrastucture Summary_inc_dop.csv") #brings in dataset with exposure sub-index scores
ps[6,2]<-2565 #code for Ingalls Head is incorrect and needs to be changed
psa<-subset(ps,ps$Region=="M&G"|ps$Region=="NFLD"|ps$Region=="QC") #subset of pilot data that only includes the Atlantic data
psa$Harbour<-toupper(psa$Harbour)
psa<-dplyr::arrange(psa,Harbour) 
psa$ID=as.numeric(rownames(psa))
psa$DFP_rc<-psa$Degree.of.Facility.Protection
psa <- transform(psa,DFP_rc=ifelse(psa$Degree.of.Facility.Protection == 1, 5, DFP_rc))
psa <- transform(psa,DFP_rc=ifelse(psa$Degree.of.Facility.Protection == 2, 4, DFP_rc))
psa <- transform(psa,DFP_rc=ifelse(psa$Degree.of.Facility.Protection == 4, 2, DFP_rc))
psa <- transform(psa,DFP_rc=ifelse(psa$Degree.of.Facility.Protection == 5, 1, DFP_rc))

psa_mrg<-merge(as_atl_sub,psa,by.x="Har_Code",by.y="Harbour.Code") #merge the subset of the full SCH dataset (as_atl_sub) with the Pilot Sites file that contains the Degree of Protection
names(psa_mrg)
psa_mrg2<-select(psa_mrg,c(2,1,16:19,3:5,10,12,11,13,6:9,24,26))
names(psa_mrg2)<-c("HARBOUR_NAME","HARBOUR_CODE","HARBOUR_TYPE","PROVINCE","REGION","AUTHORITY_NAME","LONGITUDE",
                  "LATITUDE","HARBOUR_CONDITION","HC_TRANS","HC_CUT","HC_CUT_ORD","HC_CUT_ORD_RC",
                  "TOTAL_REPLACEMENT_COST","ROOT_6_TRC","ROOT_6_TRC_CUT","ROOT_6_TRC_CUT_ORD","DEGREE_OF_FACILITY_PROTECTION",
                  "DFP_RC")
psa_mrg2<-dplyr::arrange(psa_mrg2,HARBOUR_NAME)

#################### Infrastructure Sub-Index Calculation #############################

#psa_mrg2$ISI_SUM<-rowSums(psa_mrg2[,c(13,17,19)]) #sum of variables
#psa_mrg2$ISI_MEAN<-psa_mrg2$ISI_SUM/3 #mean of variables
iprod=(psa_mrg2[,13]*psa_mrg2[,17]*psa_mrg2[,19]) #product of variables
ivar=3
psa_mrg2$ISI_GM=iprod^(1/ivar) #geometric mean of variables
#psa_mrg2$ISI_CVI=sqrt(iprod/ivar)  #Gornitz calculation for CVI
psa_mrg2<-dplyr::arrange(psa_mrg2,desc(ISI_GM))
counts<-psa_mrg2$ISI_GM
psa_names<-psa_mrg2$HARBOUR_NAME
b<-barplot(counts, xaxt='n',ylim=c(0,5),xlab="SCH Location",ylab="Infrastructure Sub-Index: Geometric Mean")
axis(1,at=b,labels=F)
text(b,c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,.9),psa_names,srt=90,cex=.6)

write.csv(psa_mrg2,"PSISI_ATLANTIC.csv")

#################### Exposure Sub-Index Calculation #############################
#The exposure data (untransformed and transformed with cuts and levels) were linked to the pilot sites by 50 km NTS sheets
#The file we import here is a modified version of PSISI_ATLANTIC (above) but with exposrue subindex added.
wd <- setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Final Pilot Report/Shapefiles")
fname<-"PSISI_Atlantic_NTS_EI.shp" #creates the file name variable that is then used to read in the shape file via readOGR
shape <- readOGR(dsn=wd,layer=strsplit(fname, "\\.")[[1]][1])
names(shape@data)<-c("HARBOUR_NA","HARBOUR_CO","HARBOUR_TY","PROVINCE","REGION","AUTHORITY_","LONGITUDE","LATITUDE",
                     "HC","HC_TRA","HC_CUT_RC","HC_RC_FLIP","TRC","TRA_TRC","TRA_TRC_RC","DP","DP_RC","ISI_GM","NTS","SLC_2100",
                     "ABS_SLC_2100","TRA_SLC_2100", "SLC_2100_RC","MWH","MWH_RC","MWS_MAX","MWS_MAX_RC","MSIC","MSIC_TRA","MSIC_RC",
                     "CM_SCORE")
#shape@data$ESI_MEAN<-(shape@data$SLC_2100_RC+shape@data$MWS_MAX_RC+shape@data$MWH_RC+shape@data$CM_SCORE+shape@data$MSIC_RC)/5
shape@data$ESI_GM<-(shape@data$SLC_2100_RC*shape@data$MWS_MAX_RC*shape@data$MWH_RC*shape@data$CM_SCORE*shape@data$MSIC_RC)^(1/5)
#shape@data$ESI_CVI<-((shape@data$SLC_2100_RC*shape@data$MWS_MAX_RC*shape@data$MWH_RC*shape@data$CM_SCORE*shape@data$MSIC_RC)/5)^(1/2)

shape@data<-dplyr::arrange(shape@data,desc(ESI_GM))
counts<-shape@data$ESI_GM
har_names<-shape@data$HARBOUR_NA
c<-barplot(counts, xaxt='n',ylim=c(0,3.5),xlab="SCH Location",ylab="Exposure Sub-Index: Geometric Mean")
axis(1,at=c,labels=F)
text(c,c(1.5,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,0.90,0.75),har_names,srt=90,cex=.6)
x=shape@data
write.csv(x,paste((strsplit(fname, "\\.")[[1]][1]),"_calc.csv"))
