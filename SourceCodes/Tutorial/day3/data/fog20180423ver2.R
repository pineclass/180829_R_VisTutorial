require(maptools)
require(rgdal)
require(ggmap)
require("plotrix")
require("plyr")
require("colorRamps")
library(XML)
library(plyr)
library(sp)
library(stringr)
library(RCurl)
library(spTimer)
library(maps)
library(gstat)
library("RColorBrewer")
#setwd("S:/Users/TaeYong/안개")

#####안개

express<-readShapeLines("S:/Users/TaeYong/안개/express/express_WGS84.shp")

load(file="S:/Users/TaeYong/안개/data2(forti_exp).rda")
road.link<-(unique(data2$group))

id.n<-unique(data2$id)

link.gis<- matrix(rep(NA,9392*7),ncol=7)

for (i in 1:9392){
  
  id.length <-length(data2[data2$id==id.n[i],1])
  temp<- data2[data2$id==id.n[i],]
  link.gis[i,2]<-temp[(id.length+1)/2,1]
  link.gis[i,3]<-temp[(id.length+1)/2,2]
  link.gis[i,1]<-paste(express$LINK_ID[i])
}

link.gis <- as.data.frame(link.gis)[,-7]
names(link.gis)<-c("LINK_ID","경도","위도","vis","speed","si")
link.gis$경도 <- as.numeric(paste(link.gis$경도))
link.gis$위도 <- as.numeric(paste(link.gis$위도))

########################################################################
url2<-iconv(getURL("http://www.weather.go.kr/weather/observation/currentweather.jsp?type=t99&mode=0&stn=0&auto_man=a",
                   .encoding="euc-kr"),from="euc-kr",to='UTF-8')
tables<-as.data.frame(readHTMLTable(url2,encoding='UTF-8'))

names(tables)<-
  c("id","current","vis","cloud","l.cloud","Tcurrent","dew","sensible","prec","rh","dir","ws","hpa")

tables<-tables[tables$id!="지점",]

tables1<-tables[,c(1,3)]

stations<-read.csv("S:/Users/TaeYong/안개/위도.csv")
stations1 <- stations[,c(4,6,7)]

test1<-merge(tables1,stations1,by.x='id',by.y='지점명')

test1$vis<-paste(test1$vis)

test1$vis[test1$vis=="20 이상"]="20"
test1$vis<-as.numeric(test1$vis)

test2<-test1[complete.cases(test1),]
########################################################################################

vec.dist <-rep(NA,dim(test2)[1])
for (i in 1:dim(test2)[1]){
  vec.dist[i]<-min(spT.geo.dist(as.numeric(test2[i,c("경도","위도")]),as.data.frame(link.gis)))
}

map('world', 'South Korea', fill=TRUE, col="lightgrey", xlim=c(125,130.4), ylim=c(34,39))
plot(express, col='white', xlim=c(125,130.4), ylim=c(34,39), add=TRUE, lwd=2)
points(test1[vec.dist<2,c("경도","위도")], pch=16, col="red")
points(test1[vec.dist>2,c("경도","위도")], pch=16, col="blue")
map.axes()

coordinates(test2) <-c("경도","위도")
link.gis2<-link.gis

coordinates(link.gis2) <-c("경도","위도")
gis2.idw<-gstat::idw(vis~ 1, test2, newdata=link.gis2, idp=2.0)

link.gis$vis<-gis2.idw$var1.pred

cuts <-seq(0,20,length.out=12)
pred.level<-cut(link.gis$vis,cuts,brewer.pal(n = 11, name = "RdYlGn"))
map('world', 'South Korea', fill=TRUE, col="lightgrey", xlim=c(125,130.4), ylim=c(34,39))
points(link.gis$경도, link.gis$위도, col=paste(pred.level), cex=0.3, pch=15)
map.axes()

express2<-express
express2@data$vis<-link.gis$vis
express2@data$viscol<-link.gis$pred.level

#####속도

url = "http://data.ex.co.kr/openapi/odtraffic/trafficAmountByRealtime?key=3314135116&type=xml"
raw.data <- xmlParse(url)
real.data<-ldply(xmlToList(raw.data), function(x) { data.frame(x[!names(x)=="author"]) } )

##################################################################################
load(file="S:/Users/TaeYong/안개/final_vds.rda")#vds4

#######################################################################################
new.vds<-vds4[,3:5]
names(new.vds)<-c("vdsId","경도","위도")

v.data<-real.data[,c("speed","vdsId")]
new.v.data<-v.data[which(as.numeric(paste(v.data$speed))>0),]

v.vds<-merge(new.v.data, new.vds, by="vdsId")

coordinates(v.vds) <-c("경도","위도")
gis2.idw2<-gstat::idw(speed~ 1, v.vds, newdata=link.gis2, idp=2.0)
link.gis$speed<-gis2.idw2$var1.pred
# summary(link.gis$speed)
# rainbow(n = 24, start=0, end=4/6)
cuts <-seq(0,110,length.out=25)
pred.level<-cut(link.gis$speed,cuts,rainbow(n = 24, start=0, end=2/6))
table(pred.level)
cuts
map('world', 'South Korea', fill=TRUE, col="lightgrey", xlim=c(125,130.4), ylim=c(34,39))
points(link.gis$경도, link.gis$위도, col=paste(pred.level), cex=0.3, pch=15)
map.axes()

express2@data$speed<-link.gis$speed
express2@data$sd <- 0.694*link.gis$speed+link.gis$speed^2/(254*0.63)
express2@data$rwi <- express2@data$sd/express2@data$vis

###############################################################################

library(RgoogleMaps)
library(plotGoogleMaps)
express3<-express2
express3@proj4string =CRS('+proj=longlat +datum=WGS84')
m1=plotGoogleMaps(express3,zcol=31,colPalette=rainbow(n = 24, start=0, end=2/6),control.width='50%',control.height='50%',
                  api="https://maps.googleapis.com/maps/api/js?key=AIzaSyCkuAgeN7WipKLaNSUAJTeoTRceEFYKOKc&callback=initMap")
