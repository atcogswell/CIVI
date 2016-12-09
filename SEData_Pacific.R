library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(foreign)
library(dplyr)
library(Rcpp)
library(matrixStats)

setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Shapefiles/Revised Layers 2015")

se<-read.csv("MECTS-#3467158-v5A-SCH_CIVI_Pilot_Study_SE_Dataset.csv")
se$X.2<-NULL
se$X.1<-NULL
se$X<-NULL
se$A<-NULL
se$B<-NULL
se$C<-NULL
names(se)<-c("PROVINCE","SCH CODE","HN","NUMBER","NAME","ALV","ALQ","CTLQ","NAVPL",
             "NAVPL_09_13","AFI","AFITF","AFIAEI","TP")
se<-dplyr::arrange(se,HN)
se[731,2]<-2603 #North Head Grand Manan missing code
#HN=="HARBOUR NAME"
#ALV=="AVERAGE LANDED VALUE 2009-2013 CDN"
#ALQ=="AVERAGE LANDED QUANTITY 2009-2013 KG"
#CTLQ=="CHANGE IN TOTAL LANDED QUANTITY 2004 AND 2013 %"
#NAVPL=="NUMBER OF ACTIVE VESSELS PORT OF LANDING 2013"
#NAVPL_09_13=="NUMBER OF ACTIVE VESSELS PORT OF LANDING 2009 - 2013
#AFI=="AVERAGE TOTAL FISHING INCOME 2009-2012"
#AFITF=="AVERAGE TOTAL INCOME WITH TAX FILERS 2009-2012
#AFIAEI=="AVERAGE FISHING INCOME TO AVERAGE EMPLOYMENT INCOME(2009-2012) % BASED ON 2006 csd BOUNARIES
#TP=="TOTAL POPULAITON FOR CSD (2011)

se$CTLQ<-se$CTLQ*100
se$AFIAEI<-se$AFIAEI*100
se <- se[se[,1]!="",] # remove any NA fields with now harbour names

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

se$ALV[is.na(se$ALV)]<-0
se$ALQ[is.na(se$ALQ)]<-0
se$CTLQ[is.na(se$CTLQ)]<-0
se$NAVPL[is.na(se$NAVPL)]<-0
se$NAVPL_09_13[is.na(se$NAVPL_09_13)]<-0
se$AFI[is.na(se$AFI)]<-0
se$AFITF[is.na(se$AFITF)]<-0
se$AFIAEI[is.na(se$AFIAEI)]<-0
se$TP[is.na(se$TP)]<-0

for (i in 6:14){
  
  hist(se[,i],main=paste("Histogram of ",names(se[i]),sep=""))
  
}

#Correlation analysis for SE variables
secor<-cor(se[,6:14],method="spearman",use="complete") #spearman correlations because these data are definitely not normal and contain a slew of extreme values

#ALV and ALQ correlation
#They are highly correlated - should use one or the other
cor.test(se$ALV,se$ALQ) #highly significant correlation p value 2.22e-16 and correlation value of 0.89
plot(se$ALV,se$ALQ, xlab="Average Landed Value ($ CDN)", ylab="Average Landed Quantity (KG)")
fit<-lm(se$ALQ~se$ALV)
abline(fit)
fit
summary(fit)

#NAVPL_09_13 and NAVPL correlation
test<-cor.test(se$NAVPL,se$NAVPL_09_13,method="spearman") #highly significant correlation p value 2.22e-16 and correlation value of 0.89
plot(se$NAVPL,se$NAVPL_09_13, xlab="Number of Active Vessels Port of Landing 2013", ylab="Number of Active Vessels Port of Landing 2009-2013")
fit<-lm(se$NAVPL~se$NAVPL_09_13)
abline(fit)
fit
summary(fit)


#Correlation between NAVPL_09_13 and ALQ - strong significant correlation 
fit<-lm(log(se$ALQ+1)~se$NAVPL_09_13)
cor.test(log(se$ALQ+1),se$NAVPL_09_13)
plot(log(se$NAVPL_09_13+1),log(se$ALQ+1),xlab="NAVPL 2009-2013", ylab="ALQ KG")
plot(se$NAVPL_09_13,se$ALQ, xlab="NAVPL 2009-2013",ylab="ALQ KG")
abline(fit)

#Correlation between AFI and AFITF - strong significant correlation (rho=0.76)
plot(log(se$AFI),log(se$AFITF), xlab="log AFL",ylab="log AFITF")
fit<-lm(log(se$AFITF)~log(se$AFI))
abline(fit)
x<-cor.test(log(se$AFITF),log(se$AFI))
x<-cor.test(se$AFITF,se$AFI,method="spearman")

x<-cor.test(se$AFIAEI,se$TP,method="spearman")

#significant correlation between AFIAEI and both AFITF and AFI - Chosing AFIAEI as it represents the relative contribution of fishing to income in the area


####Total Population####
se$TP<-replace(se$TP, is.infinite(se$TP),0)
se$TP<-replace(se$TP, is.na(se$TP),0)
se$TP_tra<-log(se$TP+1) #gives roughly normal distribution but the many (>130) harbours have a surrounding population of zero or near zero
se$TP_tra_rc<-as.integer(cut(as.vector(t(se$TP_tra)), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
#install.packages("car")
library(car)
se$TP_tra_rc2=recode(se$TP_tra_rc,'1=5;2=4;3=3;4=2;5=1')
se$TP_tra_level<-cut(as.vector(t(se$TP_tra)), breaks = 5) #includes new field that specifies the range of values represented by a bin
lev<-se$TP_tra_level
lev<-as.data.frame(lev)
names(lev)<-"test"
lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
library(stringr)
lev3<-as.data.frame(str_split_fixed(lev2,",",2))
lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
alt1<-c(min(se$TP_tra,na.rm=T),lev4[1:4],max(se$TP_tra,na.rm=T))
aln1<-exp(alt1)-1
png("TP.png", height=800,width=1600) #starts the process of creating a 3 panel png that includes histogram of original data and cut data
par(mfrow=c(1,3),cex=1.3)
hist(se$TP, main="Histogram of Original Data: TP ",xlab="Total Population 2011")
abline(v=aln1,lty=2,col="blue")
hist(se$TP_tra, main="Histogram of Transformed Data: log TP + 1",xlab="Bin",xlim=c(0,13))
abline(v=alt1,lty=2,col="blue")
temp<-hist(se$TP_tra_rc2, main="Histogram of Binned Data: TP Recoded and flipped",sub="Lowest number is most populous", breaks=0:5,xlab="Bin",xaxt="n") #lowest number represents most populous areas
axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
dev.off()

####Combine ALQ and NAVPL_09_13######
se$ALQVAPL=se$ALQ/se$NAVPL_09_13
se$ALQVAPL<-replace(se$ALQVAPL, is.infinite(se$ALQVAPL),0)
se$ALQVAPL<-replace(se$ALQVAPL, is.na(se$ALQVAPL),0)
se$ALQVAPL_tra<-log(se$ALQVAPL+1) #gives roughly normal distribution but ~180 harbours either have an average of ~0 vessels over this time period (average vessels over 5 year period <1) or no landed quantity
se$ALQVAPL_tra_rc<-as.integer(cut(as.vector(t(se$ALQVAPL_tra)), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
se$ALQVAPL_tra_level<-cut(as.vector(t(se$ALQVAPL_tra)), breaks = 5) #includes new field that specifies the range of values represented by a bin
lev<-se$ALQVAPL_tra_level
lev<-as.data.frame(lev)
names(lev)<-"test"
lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
lev3<-as.data.frame(str_split_fixed(lev2,",",2))
lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
alt2<-c(min(se$ALQVAPL_tra,na.rm=T),lev4[1:4],max(se$ALQVAPL_tra,na.rm=T))
aln2<-exp(alt2)-1
png("ALQVAPL.png", height=800,width=1600) #starts the process of creating a 3 panel png that includes histogram of original data and cut data
par(mfrow=c(1,3),cex=1.3)
hist(se$ALQVAPL, main="Histogram of Original Data: ALQVAPL ",xlab="Quantity/Vessel KG 2009-2013")
abline(v=aln2,lty=2,col="blue")
#axis(side=1,at=temp$mids,labels=seq(0.0e+00,1.2e+07)) #replaces xlab with nicely centred bin labels for the histogram
hist(se$ALQVAPL_tra, main="Histogram of Transformed Data: log ALQVAPL +1",xlab="Bin",xlim=c(0,17))
abline(v=alt2,lty=2,col="blue")
temp<-hist(se$ALQVAPL_tra_rc, main="Histogram of Binned Data: ALQVAPL Recoded", breaks=0:5,xlab="Bin",xaxt="n")
axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
dev.off()


####AFIAEI####
se$AFIAEI<-replace(se$AFIAEI, is.infinite(se$AFIAEI),0)
se$AFIAEI<-replace(se$AFIAEI, is.na(se$AFIAEI),0)
se$AFIAEI_tra<-se$AFIAEI^(1/3) #gives roughly normal distribution but many harbours (>300) have an AFIAEI value of 0 over this period for the census subdivision (either AFI or AEI are zero or both)
se$AFIAEI_tra_rc<-as.integer(cut(as.vector(t(se$AFIAEI_tra)), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
se$AFIAEI_tra_level<-cut(as.vector(t(se$AFIAEI_tra)), breaks = 5) #includes new field that specifies the range of values represented by a bin
lev<-se$AFIAEI_tra_level
lev<-as.data.frame(lev)
names(lev)<-"test"
lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
lev3<-as.data.frame(str_split_fixed(lev2,",",2))
lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
alt3<-c(min(se$AFIAEI_tra,na.rm=T),lev4[1:4],max(se$AFIAEI_tra,na.rm=T))
aln3<-alt3^3
png("AFIAEI.png", height=800,width=1600) #starts the process of creating a 3 panel png that includes histogram of original data and cut data
par(mfrow=c(1,3),cex=1.3)
hist(se$AFIAEI, main="Histogram of Original Data: AFIAEI ",xlab="FISHING INCOME TO EMPLOYMENT INCOME(2009-2012) %")
abline(v=aln3,lty=2,col="blue")
hist(se$AFIAEI_tra, main="Histogram of Transformed Data: AFIAEI (^1/3) ",xlab="Bin",xlim=c(0,4.5))
abline(v=alt3,lty=2,col="blue")
temp<-hist(se$AFIAEI_tra_rc, main="Histogram of Binned Data: AFIAEI Recoded", breaks=0:5,xlab="Bin",xaxt="n")
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
wd<-setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Shapefiles/Atlantic Zone Layers 2015/")
fname<-"PSISI_Atlantic_NTS_EI_calc.shp" #creates the file name variable that is then used to read in the shape file via readOGR
shape <- readOGR(dsn=wd,layer=strsplit(fname, "\\.")[[1]][1])
allsi<-merge(shape@data,se,by.x="HARBOUR_C",by.y="SCH CODE")
allsi$SEGM<-(allsi$TP_tra_rc2*allsi$ALQVAPL_tra_rc*allsi$AFIAEI_tra_rc)^(1/3)
allsi$MSE<-(allsi$TP_tra_rc2+allsi$ALQVAPL_tra_rc+allsi$AFIAEI_tra_rc)/3
allsi$SECVI<-(allsi$TP_tra_rc2*allsi$ALQVAPL_tra_rc*allsi$AFIAEI_tra_rc/3)^(1/2)
allsi$AGM<- (allsi$ISI_GM*allsi$ESI_GM*allsi$SEGM)^(1/3)


data1=allsi[,c(13,12)]
data2=allsi[,1:length(allsi)]
data3=SpatialPointsDataFrame(data1, data2, coords.nrs = numeric(0),proj4string = CRS("+proj=longlat +datum=WGS84"), match.ID = TRUE, bbox = NULL)
write.csv(allsi,"PilotSites_Atlantic_allsi.csv")
writeOGR(obj=data3, dsn=wd, layer=paste((strsplit(fname, "\\.")[[1]][1]),"_CIVI",sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=T)
View(se)

