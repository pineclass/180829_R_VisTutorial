reposUrl = "http://cran.us.r-project.org"

importLib <- function( libName ){
  if( !require( libName ,character.only=TRUE ) ){
    install.packages( libName,repos="http://cran.us.r-project.org")
    require( libName,character.only=TRUE )
  } 
}

librarys <- c("spatstat", "ggplot2", "ggmap", "sp", "maptools", "raster", "rgdal", "gridGraphics")

for ( libName in librarys ){
  importLib( libName )  
}


if(!require(spatstat)) install.packages("spatstat",repos=reposUrl); library("spatstat")
if(!require(ggplot2)) install.packages("ggplot2",repos=reposUrl); library("ggplot2")
if(!require(ggmap)) install.packages("ggmap",repos=reposUrl); library("ggmap")
if(!require(sp)) install.packages("sp",repos=reposUrl); library("sp")
if(!require(maptools)) install.packages("maptools",repos=reposUrl); library("maptools")
if(!require(raster)) install.packages("raster", repos=reposUrl); library("raster")
if(!require(rgdal)) install.packages("rgdal",repos=reposUrl); library("rgdal")
if(!require(gridGraphics)) install.packages("gridGraphics",repos=reposUrl); library("gridGraphics")

set_wd <- function() {
  importLib("rstudioapi") # make sure you have it installed
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}
set_wd()
seoul.link <- readOGR("../data/seoul1_a.shp")

border<-readOGR("../data/행정경계_구_WGS84.shp")

data2 <-fortify(seoul.link)
data3<-fortify(border)

a1 <- get_map(location='Seoul', zoom=11, maptype='roadmap')

ggmap(a1)+geom_polygon(aes(x=long, y=lat, group=group), data=data3, colour="black", fill="black", alpha=0.3)

ggmap(a1)+
   geom_polygon(aes(x=long, y=lat, group=group), data=data3, colour="black", fill="black", alpha=0.05)+
   geom_path(aes(x = long, y = lat, group=group), data=data2, colour="#FFFFFF", size=0.5, lineend="butt",
              linejoin="round", linemitre=1, alpha=0.5) 


###################################################################
data12=readOGR("../data/TL_SCCO_EMD_2015_W.shp",encoding='UTF-8')

l=read.csv("../data/b.csv") #sprandom을 이용한 화재사고 위치 (생성 코드 필요)
o=read.csv("../data/a.csv") # 소방서의 위경도
names(o) <- c("lon","lat")

shp1owin <- as.owin(data12)
class(shp1owin)

pts <- coordinates(l)
head(pts)

p <- ppp(pts[,1], pts[,2], window=shp1owin)
class(p)
p
plot(p)

ds <- density(p)

plot(ds)
r <-raster(ds)
plot(r, main='서울특별시 화재건수의 공간분포')

df <- rasterToPoints(r)
df[,3] <- df[,3]/10000
df <- as.data.frame(df)

ggmap(a1)+
  geom_polygon(aes(x=long, y=lat, group=group), data=data3, colour="black", fill="black", alpha=0.3)+
  geom_point(data=o, aes(x=lon, y=lat), size=3, shape=21,  fill="blue", color="white")

ggmap(a1)+
  geom_point(data=as.data.frame(df), aes(x=x,y=y, color=layer, size=1), alpha=0.06 )+ scale_color_gradient(low="green", high="red")+
  # geom_path(aes(x = long, y = lat, group=group), data=data2, colour="#FFFFFF", size=0.5, lineend="butt",
  #       linejoin="round", linemitre=1, alpha=0.5) +
  geom_polygon(aes(x=long, y=lat, group=group), data=data3, colour="black", fill="black", alpha=0.05) +
  geom_point(data=o, aes(x=lon, y=lat), size=3, shape=21,  fill="blue", color="white")



# 최소 거리 계산하기

head(df[,1:2])
head(o)

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}


dist.data = rep(NA,8849)
temp <- rep(NA,22)

for (i in 1:8849){
  for(j in 1:22){
    temp[j]<-earth.dist(df[i,1],df[i,2],o[j,1],o[j,2])}
  dist.data[i]<- min(temp)
print(i)
  }

df$dist <-dist.data

ggmap(a1)+
  geom_point(data=as.data.frame(df), aes(x=x,y=y, color=dist, size=1), alpha=0.06 )+ scale_color_gradient(low="blue", high="yellow")+
  # geom_path(aes(x = long, y = lat, group=group), data=data2, colour="#FFFFFF", size=0.5, lineend="butt",
  #       linejoin="round", linemitre=1, alpha=0.5) +
  geom_polygon(aes(x=long, y=lat, group=group), data=data3, colour="black", fill="black", alpha=0.07) +
  geom_point(data=o, aes(x=lon, y=lat), size=3, shape=21,  fill="blue", color="white")


head(df)
df$new = df$layer*(df$dist/2.5)^3

ggmap(a1)+
  geom_point(data=as.data.frame(df), aes(x=x,y=y, color=new, size=1), alpha=0.06 )+ scale_color_gradient(low="green", high="red")+
  # geom_path(aes(x = long, y = lat, group=group), data=data2, colour="#FFFFFF", size=0.5, lineend="butt",
  #       linejoin="round", linemitre=1, alpha=0.5) +
  geom_polygon(aes(x=long, y=lat, group=group), data=data3, colour="black", fill="black", alpha=0.05) +
  geom_point(data=o, aes(x=lon, y=lat), size=3, shape=21,  fill="blue", color="white")
