library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(foreign)

wd <- setwd("C:/Users/cogswella/Documents/AZMP/Coastal Vulnerability Index/Final Pilot Report/Shapefiles")

# This section reads in a list of possible shapefiles from the working directory
fname<-"CoastalMaterials_Line_pac_clip.shp" #creates the file name variable that is then used to read in the shape file via readOGR
shape <- readOGR(dsn=wd,layer=strsplit(fname, "\\.")[[1]][1])
l=length(shape@data) #number of columns of import data required for the loop below

shape@data[shape@data==0]<-NA

png(paste((strsplit(fname, "\\.")[[1]][1]),'.png',sep=""), height=800,width=800) #starts the process of creating a 2 panel png that includes histogram of original data and cut data
hist(shape@data$Score, breaks=0:5,xlab="Coastal Materials Classification",xaxt="n", main="Histogram of Coastal Materials")
axis(side=1,at=temp$mids,labels=seq(1,5))
dev.off()
  


# The data are then exported to a new shape file with the extension _cut.shp that includes all of the new calculated fields.
writeOGR(obj=shape, dsn=wd, layer=paste((strsplit(fname, "\\.")[[1]][1]),"_recoded",sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE,verbose=T)
rm(list = ls())
