library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(foreign)
library(stringr)

wd<-setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Final Pilot Report/Shapefiles")

# This section reads in a list of possible shapefiles from the working directory

fname<-"SeaIce_Line20160224_atl_clip.shp" #creates the file name variable that is then used to read in the shape file via readOGR
shape <- readOGR(dsn=wd,layer=strsplit(fname, "\\.")[[1]][1])

# This is the difference between 1970 and 2010 based on >50% coverage in weeks (1970 - 2010).  A negative value means more weeks of coverage in 2010 and a positive number means less weeks of coverage in 2010.

####Mean Sea Ice difference (weeks) greater than 50% coverage (1970 - 2010)####
hist(abs(shape@data$MEAN_RASTE))
hist((log(abs(shape@data$MEAN_RASTE)+1)))
shape@data$MEAN_TRA<-((log(abs(shape@data$MEAN_RASTE)+1)))
shape@data$MEAN_TRA_RC<-as.integer(cut(as.vector(t(shape@data$MEAN_TRA)), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
shape@data$MEAN_LEVELS<-cut(as.vector(t(shape@data$MEAN_TRA)), breaks = 5) #includes new field that specifies the range of values represented by a bin

#calculated cut points for raster
lev<-shape@data[length(shape@data)]
names(lev)<-"test"
lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
lev3<-as.data.frame(str_split_fixed(lev2,",",2))
lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
alt1<-c(0,lev4)
aln1<-exp(alt1)-1  
  
png("SI_MEAN.png", height=800,width=1600) #starts the process of creating a 3 panel png that includes histogram of original data and cut data
par(mfrow=c(2,2),cex=1.3)
hist(shape@data$MEAN_RASTE, main="Histogram of Original Data",xlab="Difference in Sea Ice Coverage >50% between 1970 and 2010 (weeks)")
hist(abs(shape@data$MEAN_RASTE), main="Histogram of Absolute of Data",xlab="Absolute Difference ")
abline(v=aln1,lty=2,col="blue")
hist(shape@data$MEAN_TRA, main="Histogram of Transformed Data (logx + 1)",xlab="Transformed Absolute Difference")
abline(v=alt1,lty=2,col="blue")
temp<-hist(shape@data$MEAN_TRA_RC, main="Histogram of Recoded Data", breaks=0:5,xlab="Recoded Sea Ice Coverage Difference",xaxt="n")
axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
dev.off()

SI_al_all<-as.data.frame(cbind(round(aln1,2),round(alt1,2)))
names(SI_al_all)<-c("ABS_MEAN_SI_50","MEAN_SI_50_TRA")
write.csv(SI_al_all,"SeaIce_cut_summary.csv")

# The data are then exported to a new shape file with the extension _cut.shp that includes all of the new calculated fields.
writeOGR(obj=shape, dsn=wd, layer=paste((strsplit(fname, "\\.")[[1]][1]),"_recoded",sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=T)
rm(list = ls())
