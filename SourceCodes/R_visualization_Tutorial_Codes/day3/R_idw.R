setwd("C:/Users/USER/Dropbox/R-work/R_vis/day3")

data<-read.csv("data/ASOS_sample.csv") 
station<-read.csv("data/stnInfo_20171018091530.csv")

t.data <- merge(data[,1:4], station[c(1,6,7,8)], by="지점")
head(t.data)
names(t.data)<-c("지점","date","avg","min","lat","lon","alt")

library(plyr)
library(sp)
t.data2<-ddply(t.data, ~지점, summarize, tmean=mean(avg, na.rm=T))

dotchart(t.data2$tmean[1:10], labels=t.data2$지점[1:10])
rep(1:2,c(3,7))
dotchart(t.data2$tmean[1:10], labels=t.data2$지점[1:10], groups=rep(1:2,c(3,7)), col=rep(1:2,c(3,7)), xlim=c(0,20))

tc.data2<-t.data[t.data$date=="2017-01-01",]
coordinates(tc.data2) <-c("lon","lat")

grd <- as.data.frame(spsample(tc.data2, "regular", n=50000))
names(grd)<- c("lon","lat")
coordinates(grd) <- c("lon","lat")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

tc.idw <- gstat::idw(avg ~ 1, tc.data2, newdata=grd, idp=2.0)

library(raster)
r <- raster(tc.idw)
plot(r)

library(mapdata)
map(database = "worldHires", region='South Korea', add=TRUE)
contour(r, add=T, lty=2)

