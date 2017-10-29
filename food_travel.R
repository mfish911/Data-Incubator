setwd("~/Documents/DataIncubator")
data<-read.csv("cfs_2012_pumf.csv")

data$ORIG_STATE<-as.factor(data$ORIG_STATE)
levels(data$ORIG_STATE)<-c("Origin Suppressed","AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

data$DEST_STATE<-as.factor(data$DEST_STATE)
levels(data$DEST_STATE)<-c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

data$ORIG_MA[data$ORIG_MA==99999]<-NA
data$DEST_MA[data$DEST_MA==99999]<-NA

levels(data$SCTG)[46]<-"01-05"
levels(data$SCTG)[51]<-"06-09"
levels(data$SCTG)[8]<-"10-14"

SCTG_labels<-read.csv("SCTG_Meta.csv")
levels(SCTG_labels$SCTG)[46]<-"01-05"
levels(SCTG_labels$SCTG)[51]<-"06-09"
levels(SCTG_labels$SCTG)[8]<-"10-14"


plot(data$SCTG,data$SHIPMT_DIST_ROUTED-data$SHIPMT_DIST_GC)
plot(data$SHIPMT_DIST_GC,data$SHIPMT_DIST_ROUTED)
plot(data$SHIPMT_DIST_GC,data$SHIPMT_DIST_ROUTED-data$SHIPMT_DIST_GC)

library(tidyverse)
library(maps)
library(geosphere)

MA_loc<-read.csv("MA_loc.csv")

plot_my_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}

"%contain%" <- function(values,x) {
  tx <- table(x)
  tv <- table(values)
  z <- tv[names(tx)] - tx
  all(z >= 0 & !is.na(z))
}

dist<-as.data.frame(matrix(NA,nrow=length(data[,1]),ncol=6))
for(i in seq(1:length(data[,1]))){
  if(MA_loc$ORIG_CFS_AREA %contain% data$ORIG_CFS_AREA[i]){
  dist[i,1]<-data$ORIG_CFS_AREA[i]
  dist[i,2]<-MA_loc$Longitude[MA_loc$ORIG_CFS_AREA==data$ORIG_CFS_AREA[i]]
  dist[i,3]<-MA_loc$Latitude[MA_loc$ORIG_CFS_AREA==data$ORIG_CFS_AREA[i]]
  dist[i,4]<-data$DEST_CFS_AREA[i]
  dist[i,5]<-MA_loc$Longitude[MA_loc$ORIG_CFS_AREA==data$DEST_CFS_AREA[i]]
  dist[i,6]<-MA_loc$Latitude[MA_loc$ORIG_CFS_AREA==data$DEST_CFS_AREA[i]]
  } else dist[i,]<-rep(NA,6)
}
names(dist)<-c("Orig","long1","lat1","Dest","long2","lat2")

par(mar=c(0,0,0,0))
map('usa',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80) )
for(i in 1:nrow(dist)){
  plot_my_connection(dist$long1[i], dist$lat1[i], dist$long2[i], dist$lat2[i], col="skyblue", lwd=1)
}
points(x=MA_loc$Longitude,y=MA_loc$Latitude,col="slateblue", cex=3, pch=20)
