library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(foreign)

wd <- setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Final Pilot Report/Shapefiles")

# This section reads in a list of possible shapefiles from the working directory

fname<-"Windspeed_Line_atl_clip.shp" #creates the file name variable that is then used to read in the shape file via readOGR
shape <- readOGR(dsn=wd,layer=strsplit(fname, "\\.")[[1]][1])
l=length(shape@data) #number of columns of import data required for the loop below

lim<-NULL
for (i in 3:4){
  
  shape@data[length(shape@data)+1]<-as.integer(cut(as.vector(t(shape@data[i])), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
  names(shape@data)[length(shape@data)]<- c(paste("cut_",substr(names(shape@data[i]),1,3),sep=""))#changes the name of the new field to include cut_ and the decade
  shape@data[length(shape@data)+1]<-cut(as.vector(t(shape@data[i])), breaks = 5) #includes new field that specifies the range of values represented by a bin
  names(shape@data)[length(shape@data)]<- c(paste("level_",substr(names(shape@data[i]),1,3),sep=""))
  lev<-shape@data[length(shape@data)]
  names(lev)<-"test"
  lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
  lev3<-as.data.frame(str_split_fixed(lev2,",",2))
  lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
  aln<-c(min(shape@data[i]),lev4)
  lim<-as.data.frame(cbind(lim,aln))
  png(paste((strsplit(fname, "\\.")[[1]][1]),"_",names(shape@data[i]),'.png',sep=""), height=800,width=1600) #starts the process of creating a 2 panel png that includes histogram of original data and cut data
  par(mfrow=c(1,2),cex=1.5)
  hist(shape@data[,i], main="Histogram of Original Data",xlab="Mean Maximum Wind Speed (m/s)", xlim=c(15,36))
  abline(v=aln,lty=2,col="blue")
  temp<-hist(shape@data[,length(shape@data)-1], main="Histogram of Recoded Wind Speed", breaks=0:5,xlab="Recoded Mean Maximum Wind Speed ",xaxt="n")
  axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
  dev.off()
}

names(lim)<-c("MAX_WSMAX","MEAN_WSMAX")
write.csv(lim,"WS_LINE_ATLANTIC_CLIP_CUTS.csv")

# The data are then exported to a new shape file with the extension _cut.shp that includes all of the new calculated fields.
writeOGR(obj=shape, dsn=wd, layer=paste((strsplit(fname, "\\.")[[1]][1]),"_recoded",sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=T)
rm(list = ls())
