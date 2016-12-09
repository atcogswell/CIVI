library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(foreign)
library(stringr)

wd <- setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Final Pilot Report/Shapefiles")


# This section reads in a list of possible shapefiles from the working directory
fname<-"SLC_Line_atl_clip.shp" #creates the file name variable that is then used to read in the shape file via readOGR
shape <- readOGR(dsn=wd,layer=strsplit(fname, "\\.")[[1]][1])
l=length(shape@data) #number of columns of import data required for the loop below

lim<-NULL
#####Start Here - I've already started with a transformed column in a loop, just have to tidy this up and check all of the fields to make sure the same transformation applies
for (i in 2:(l-1)){
  shape@data[length(shape@data)+1]<-abs(shape@data[i]) #transform data to reduce left skew
  names(shape@data)[length(shape@data)]<- c(paste("abs_",substr(names(shape@data[i]),7,10),sep="")) #name the transformed field
  shape@data[length(shape@data)+1]<-as.numeric((shape@data[length(shape@data)]^1.6)) #transform data to reduce left skew
  names(shape@data)[length(shape@data)]<- c(paste("tra_",substr(names(shape@data[i]),7,10),sep="")) #name the transformed field
  shape@data[length(shape@data)+1]<-as.integer(cut(as.vector(t(shape@data[length(shape@data)])), breaks = 5, labels=1:5))#cuts the orginal data and creates new field binned 1:5
  names(shape@data)[length(shape@data)]<- c(paste("cut_",substr(names(shape@data[i]),7,10),sep=""))#changes the name of the new field to include cut_ and the decade
  shape@data[length(shape@data)+1]<-cut(as.vector(t(shape@data[length(shape@data)-1])), breaks = 5) #includes new field that specifies the range of values represented by a bin
  names(shape@data)[length(shape@data)]<- c(paste("level_",substr(names(shape@data[i]),7,10),sep=""))#names the field with the levels listed
  lev<-shape@data[length(shape@data)]
  names(lev)<-"test"
  lev2<-levels(lev$test) #calculates the levels of the split and puts them into their own dataframe
  lev3<-as.data.frame(str_split_fixed(lev2,",",2))
  lev4<-as.numeric(gsub("\\(|\\]", "", lev3$V2)) #this step takes out the ( and ] from the upper end of each cut which is then used to calculate the transformed and untransformed vertical ablines
  alt<-c(0,lev4)
  aln<-(alt^(1/1.6))
  lim<-as.data.frame(cbind(lim,aln,alt))
  png(paste((strsplit(fname, "\\.")[[1]][1]),"_",names(shape@data[i]),'.png',sep=""), height=800,width=1600) #starts the process of creating a 2 panel png that includes histogram of original data and cut data
  par(mfrow=c(1,3),cex=1.5)
  hist(shape@data[,length(shape@data)-3], main="Histogram of Absolute Mean SLC", xlab="Absolute Sea-level Distribution (m)")
  abline(v=aln,lty=2,col="blue")
  hist(shape@data[,length(shape@data)-2],main="Histogram of Transformed Data",xlab="Sea-level Distribution (absolute original value ^1.6)")
  abline(v=alt,lty=2,col="blue")
  temp<-hist(shape@data[,length(shape@data)-1], main="Histogram of Recoded Data", breaks=0:5,xlab="Recoded Sea-level",xaxt="n")
  axis(side=1,at=temp$mids,labels=seq(1,5)) #replaces xlab with nicely centred bin labels for the histogram
  dev.off()
}

names(lim)<-c("cut_2020","cut_tra_2020","cut_2030","cut_tra_2030","cut_2040","cut_tra_2040","cut_2050","cut_tra_2050","cut_2060","cut_tra_2060","cut_2070","cut_tra_2070","cut_2080","cut_tra_2080","cut_2090","cut_tra_2090","cut_2100","cut_tra_2100")
write.csv(lim,paste((strsplit(fname, "\\.")[[1]][1]),"_cuts.csv",sep=""))

# The data are then exported to a new shape file with the extension _cut.shp that includes all of the new calculated fields.
writeOGR(obj=shape, dsn=wd, layer=paste((strsplit(fname, "\\.")[[1]][1]),"_recoded",sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=T)
rm(list = ls())
