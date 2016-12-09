library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(foreign)
library(dplyr)
library(Rcpp)
library(matrixStats)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(stats)

wd <- setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Final Pilot Report/Shapefiles")

se<-read.csv("MECTS-#3467158-v5A-SCH_CIVI_Pilot_Study_SE_Dataset.csv")
se$X.2<-NULL
se$X.1<-NULL
se$X<-NULL
se$A<-NULL
se$B<-NULL
se$C<-NULL
names(se)<-c("PROVINCE","SCH CODE","HN","NUMBER","NAME","ALV","ALQ","CTLQ","NAVPL",
             "NAVPL_09_13","AFI","ATITF","AFIAEI","TP")
se<-dplyr::arrange(se,HN)

se[731,2]<-2603 #North Head Grand Manan missing code
#HN=="HARBOUR NAME"
#ALV=="AVERAGE LANDED VALUE 2009-2013 CDN"
#ALQ=="AVERAGE LANDED QUANTITY 2009-2013 KG"
#CTLQ=="CHANGE IN TOTAL LANDED QUANTITY 2004 AND 2013 %"
#NAVPL=="NUMBER OF ACTIVE VESSELS PORT OF LANDING 2013"
#NAVPL_09_13=="NUMBER OF ACTIVE VESSELS PORT OF LANDING 2009 - 2013
#AFI=="AVERAGE TOTAL FISHING INCOME 2009-2012"
#ATITF=="AVERAGE TOTAL INCOME WITH TAX FILERS 2009-2012
#AFIAEI=="AVERAGE FISHING INCOME TO AVERAGE EMPLOYMENT INCOME(2009-2012) % BASED ON 2006 csd BOUNARIES
#TP=="TOTAL POPULAITON FOR CSD (2011)

se$CTLQ<-se$CTLQ*100
se$AFIAEI<-se$AFIAEI*100
se <- se[se[,1]!="",] # remove any NA fields with no harbour names
se$ID<-as.numeric(rownames(se))

#####Note from Wayne Fan on December 12, 2015 concerning NA values that affected output
#The data related to landed quantity, landed value and vessels can be assumed to be zero when it is noted to be N/A. For the most part, 
#the data set is simply a record of what has been reported. Any dataset which is not available can be assumed to be zero in reported value, 
#reported quantity and reported vessel activity. The important part is to explicitly mention in the methodology that those socio-economic 
#indicators are restricted to what is reported.

#Out of the initial SCH sites decided for the pilot, I have reviewed the ones with missing socio-economic data 
#(excluding Pacific and Central/Arctic regions).  Most of them do not have suppressed data which means that their income values can be assumed 
#to be zero. Of course, we will need to be careful about writing out the caveats behind this assumption later. I have not had time to review all 
#the datasets with NA for income and population, but from what I can see the small sample, it seems that most of the data are not suppressed. 
#I will need until next week to verify the rest. 
se$NAVPL_09_13[is.na(se$NAVPL_09_13)] <- 0 #changes NAVPL_2009_2013 to zero
se$NAVPL_09_13[se$ALQ>=0 & se$NAVPL_09_13==0]<-1 #replaces any zeros in NAVPL_2009_2013 with 1 when ALV or ALQ is >=0.
se$NAVPL_09_13[abs(se$CTLQ)>=0 & se$NAVPL_09_13==0]<-1 #anytime there is a CTLQ then there was likely a vessel at the SCH so NAVPL_09_13 was changed to 1 
se$NAVPL_09_13[se$NAVPL>=0 & se$NAVPL_09_13==0]<-1 #If there was 1 or more ship in NAVPL then I increased the NAVPL_09_13 to at least 1

se$ALV[is.na(se$ALV)]<-0
se$ALQ[is.na(se$ALQ)]<-0
se$CTLQ<-NULL
se$NAVPL[is.na(se$NAVPL)]<-0
#se$NAVPL_09_13[is.na(se$NAVPL_09_13)]<-0
se$AFI[is.na(se$AFI)]<-0
se$ATITF[is.na(se$ATITF)]<-0
se$AFIAEI[is.na(se$AFIAEI)]<-0
se$TP[is.na(se$TP)]<-0

for (i in 6:13){
  
  hist(se[,i],main=paste("Histogram of ",names(se[i]),sep=""))
  
}

#Correlation analysis for SE variables

#install.packages("ggcorrplot")
library(ggcorrplot)

cormatrix<-rcorr(as.matrix(se[,6:13]),type=c("spearman"))

cormatrix_P <- cormatrix$P

cormatrix_P[is.na(cormatrix_P)] <- 0.000000e+00

ggcorrplot(cormatrix$r, hc.order = TRUE,type = "lower", p.mat = cormatrix_P, lab=T,sig.level=0.01)


se2<-se[,c(1:5,7,9,12,13,14)] #removes ALV,NAVPL,AFI,ATITF

####Total Population####
#se2$TP<-replace(se2$TP, is.infinite(se2$TP),0)
#se2$TP<-replace(se2$TP, is.na(se2$TP),0)
se2tp<-dplyr::filter(se2,TP>0)
se2tp$TP_tra<-log(se2tp$TP) #gives roughly normal distribution but the many (>130) harbours have a surrounding population of zero or near zero
se2tp$TP_tra_rc<-as.integer(cut(as.vector(t(se2tp$TP_tra)), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
#install.packages("car")
library(car)
se2tp$TP_tra_rc2=recode(se2tp$TP_tra_rc,'1=5;2=4;3=3;4=2;5=1')
se2tp$TP_tra_level<-cut(as.vector(t(se2tp$TP_tra)), breaks = 5) #includes new field that specifies the range of values represented by a bin


se2<-left_join(se2,se2tp[,10:14],by="ID")
se2$TP_tra_rc2[is.na(se2$TP_tra_rc2)] <- 1

lev<-se2$TP_tra_level
lev<-as.data.frame(lev)
names(lev)<-"test"
lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
library(stringr)
lev3<-as.data.frame(str_split_fixed(lev2,",",2))
lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
alt1<-c(min(se2$TP_tra,na.rm=T),lev4[1:4],max(se2$TP_tra,na.rm=T))
aln1<-exp(alt1)
png("TP.png", height=800,width=1600) #starts the process of creating a 3 panel png that includes histogram of original data and cut data
par(mfrow=c(1,3),cex=1.3)
hist(se2$TP, main="Histogram of Original Data",xlab="Total Population")
abline(v=aln1,lty=2,col="blue")
hist(se2$TP_tra, main="Histogram of Transformed Data",xlab="Total Population (log)",xlim=c(0,13))
abline(v=alt1,lty=2,col="blue")
temp<-hist(se2$TP_tra_rc2, main="Histogram of Recoded Data",sub="Lowest number is most populous", breaks=0:5,xlab="Total Population Recoded and Flipped",xaxt="n") #lowest number represents most populous areas
axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
dev.off()

####AFIAEI####
#se2$AFIAEI<-replace(se2$AFIAEI, is.infinite(se2$AFIAEI),0)
#se2$AFIAEI<-replace(se2$AFIAEI, is.na(se2$AFIAEI),0)
se2$AFIAEI_tra<-se2$AFIAEI^(1/3) #gives roughly normal distribution but many harbours (>300) have an AFIAEI value of 0 over this period for the census subdivision (either AFI or AEI are zero or both)
se2$AFIAEI_tra_rc<-as.integer(cut(as.vector(t(se2$AFIAEI_tra)), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
se2$AFIAEI_tra_level<-cut(as.vector(t(se2$AFIAEI_tra)), breaks = 5) #includes new field that specifies the range of values represented by a bin
lev<-se2$AFIAEI_tra_level
lev<-as.data.frame(lev)
names(lev)<-"test"
lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
lev3<-as.data.frame(str_split_fixed(lev2,",",2))
lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
alt3<-c(min(se2$AFIAEI_tra,na.rm=T),lev4[1:4],max(se2$AFIAEI_tra,na.rm=T))
aln3<-alt3^3
png("AFIAEI.png", height=800,width=1600) #starts the process of creating a 3 panel png that includes histogram of original data and cut data
par(mfrow=c(1,3),cex=1.3)
hist(se2$AFIAEI, main="Histogram of Original Data",xlab="Average Fishing Income to Employment Income %")
abline(v=aln3,lty=2,col="blue")
hist(se2$AFIAEI_tra, main="Histogram of Transformed Data",xlab="Average Fishing Income to Employment Income (^1/3)",xlim=c(0,4.5))
abline(v=alt3,lty=2,col="blue")
temp<-hist(se2$AFIAEI_tra_rc, main="Histogram of Binned Data", breaks=0:5,xlab="Average Fishing Income to Employment Income Recoded",xaxt="n")
axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
dev.off()

####Combine ALQ and NAVPL_09_13######
se2$ALQVAPL=se2$ALQ/se2$NAVPL_09_13
se2$ALQVAPL <- replace(se2$ALQVAPL, is.na(se2$ALQVAPL), 0)
#se2$ALQVAPL<-replace(se2$ALQVAPL, is.infinite(se2$ALQVAPL),0)
#se2$ALQVAPL<-replace(se2$ALQVAPL, is.na(se2$ALQVAPL),0)
se2$ALQVAPL_tra<-se2$ALQVAPL^(1/10) #gives roughly normal distribution but ~180 harbours either have an average of ~0 vessels over this time period (average vessels over 5 year period <1) or no landed quantity
se2$ALQVAPL_tra_rc<-as.integer(cut(as.vector(t(se2$ALQVAPL_tra)), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
se2$ALQVAPL_tra_level<-cut(as.vector(t(se2$ALQVAPL_tra)), breaks = 5) #includes new field that specifies the range of values represented by a bin
lev<-se2$ALQVAPL_tra_level
lev<-as.data.frame(lev)
names(lev)<-"test"
lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
lev3<-as.data.frame(str_split_fixed(lev2,",",2))
lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
alt2<-c(min(se2$ALQVAPL_tra,na.rm=T),lev4[1:4],max(se2$ALQVAPL_tra,na.rm=T))
aln2<-alt2^10
png("ALQVAPL.png", height=800,width=1600) #starts the process of creating a 3 panel png that includes histogram of original data and cut data
par(mfrow=c(1,3),cex=1.3)
hist(se2$ALQVAPL, main="Histogram of Original Data",xlab="Landed Quantity/Vessel (KG)")
abline(v=aln2,lty=2,col="blue")
#axis(side=1,at=temp$mids,labels=seq(0.0e+00,1.2e+07)) #replaces xlab with nicely centred bin labels for the histogram
hist(se2$ALQVAPL_tra, main="Histogram of Transformed Data",xlab="Landed Quantity/Vessel ^(1/10)",xlim=c(0,5.5))
abline(v=alt2,lty=2,col="blue")
temp<-hist(se2$ALQVAPL_tra_rc, main="Histogram of Recoded Data", breaks=0:5,xlab="Landed Quantity/Vessel Recoded",xaxt="n")
axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
dev.off()




#combine all cut off points used in ablines for non-transformed data
aln_all<-cbind(round(aln1,2),round(aln2,2),round(aln3,2))
alt_all<-cbind(round(alt1,2),round(alt2,2),round(alt3,2))
aln_all<-as.data.frame(aln_all)
alt_all<-as.data.frame(alt_all)
cuts<-cbind(aln_all,alt_all)
cuts<-cuts[,c(2,5,1,4,3,6)]
names(cuts)<-c("ALQVAPL","ALQVAPL_TRA","TP","TP_TRA","AFIAEI","AFIAEI_TRA")
write.csv(cuts,"SE_cuts_all.csv")

#################### Socio-Economic Sub-Index Calculation #############################
#The exposure data (untransformed and transformed with cuts and levels) were linked to the pilot sites by 50 km NTS sheets
#The file we import here is a modified version of PSISI_ATLANTIC (above) but with exposrue subindex added.
wd <- setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Final Pilot Report/Shapefiles")
fname<-"PSISI_Atlantic_NTS_EI_calc.shp" #creates the file name variable that is then used to read in the shape file via readOGR
shape <- readOGR(dsn=wd,layer=strsplit(fname, "\\.")[[1]][1])
allsi<-merge(shape@data,se2,by.x="HARBOUR_CO",by.y="SCH CODE")
allsi$SEI_GM<-(allsi$TP_tra_rc2*allsi$ALQVAPL_tra_rc*allsi$AFIAEI_tra_rc)^(1/3)
#allsi$MSE<-(allsi$TP_tra_rc2+allsi$ALQVAPL_tra_rc+allsi$AFIAEI_tra_rc)/3
#allsi$SECVI<-(allsi$TP_tra_rc2*allsi$ALQVAPL_tra_rc*allsi$AFIAEI_tra_rc/3)^(1/2)
allsi$CIVI<- (allsi$ISI_GM*allsi$ESI_GM*allsi$SEI_GM)^(1/3)
psa_mrg<-dplyr::arrange(allsi,desc(SEI_GM))
counts<-psa_mrg$SEI_GM
psa_names<-psa_mrg$HN
psa_names[17]<-"SEAL COVE"
b<-barplot(counts, xaxt='n',ylim=c(0,5),xlab="SCH Location",ylab="Socio-Economic Sub-Index: Geometric Mean")
axis(1,at=b,labels=F)
text(b,c(1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1,1,1,1,1,1,0.8,0.8),psa_names,srt=90,cex=.5)



psa_mrg<-dplyr::arrange(allsi,desc(CIVI))
counts<-psa_mrg$CIVI
psa_names<-psa_mrg$HN
psa_names[17]<-"SEAL COVE"
b<-barplot(counts, xaxt='n',ylim=c(0,5),xlab="SCH Location",ylab="SCH CIVI: Geometric Mean 3 Sub-Indicies")
axis(1,at=b,labels=F)
text(b,c(1.4,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1.2,1,1,1,1,1,1,1,1),psa_names,srt=90,cex=.56)


write.csv(allsi,"PilotSites_Atlantic_allsi.csv")
data1=allsi[,c(8,9)]
data2=allsi[,1:length(allsi)]
data3=SpatialPointsDataFrame(data1, data2, coords.nrs = numeric(0),proj4string = CRS("+proj=longlat +datum=WGS84"), match.ID = TRUE, bbox = NULL)
writeOGR(obj=data3, dsn=wd, layer=paste((strsplit(fname, "\\.")[[1]][1]),"_CIVI",sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=T)


