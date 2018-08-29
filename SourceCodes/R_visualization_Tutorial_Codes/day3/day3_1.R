#7장 유용한 플롯

#7.2 지리정보 그래프

#7.2.1 maps, mapdata, mapproj

#7.2.1.1 maps 패키지

if (!require(maps)) {
  install.packages("maps",repos = "http://cran.us.r-project.org")
}

if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
library(ggplot2)

library(maps)

world <- map_data("world")
head(world)
nations <- unique(world$region)  # map 함수 안에 그릴 수 있는 모든 나라 목록
nations[which( grepl("Korea", nations ) )]

# 한국지도와 제주도
korea <- map("world", "South Korea")
polygon(Korea$names)
head(korea)





map("italy")
title("databse=\"italy\"")

map("italy", fill=TRUE, col=2:4)
title("fill=T, col=2:4")

map("italy", resolution=5)
title("resolution=5")

world <- map_data("world")
italy <- world[grep("Italy$", world$region),]
italy
unique(italy$subregion)

map("italy")
polygon(italy$long[italy$subregion=="Sicily"],italy$lat[italy$subregion=="Sicily"], col="red")
map("italy", add=T)

#한국지도 데이터 정보
world <- map_data("world")
korea <- world[grep("Korea$", world$region),]
unique(korea$subregion)

#한국지도를 그리고 제주도만 회색으로 그려보세요
map('world','South Korea')
polygon(korea$long[korea$subregion=="Cheju Do"],korea$lat[korea$subregion=="Cheju Do"], col="grey")


country <- map_data("state")
head(country)
unique(country$region)
NJ <- country[grep("new jersey$", country$region),]

map("state")
polygon(NJ$long, NJ$lat, col="gold")
map.text("state", add=T)

map('county', 'texas')
map.text('county', 'texas',add=T, cex=0.6)

#연습문제 7.21

map("world", "China")
map.cities(country = "China", capitals = 1)
title("capitals = 1")

map("world", "China")
map.cities(country = "China", capitals = 2)
title("capitals = 2")

map("world", "China")
map.cities(country = "China", capitals = 3, pch=16, col="red", cex=2)
title("capitals = 3")

map("world", "China")
map.cities(country = "China", capitals = 3, minpop = 3500000, maxpop = 5000000)
title("capitals=3, minpop=3500000, maxpop=5000000")

map('world','South Korea')
map.cities(capitals=1)

#연습문제 7.22

map("world", "China")
map.scale()
map.axes()


#연습문제 7.23

# 본토에서 떨어진 Alaska, Hawaii 데이터 제외
sub.usa <- subset(USArrests,!rownames(USArrests) %in% c("Alaska", "Hawaii"))
# 주이름, 폭행범 수를 갖는 데이터 프레임 생성
usa.data <- data.frame(states = rownames(sub.usa), Assault = sub.usa$Assault)

# 범례 데이터 생성
col.level <- cut(sub.usa[, 2], c(0, 100, 150, 200, 250, 300, 350))
legends <- levels(col.level)

# 주이름, 폭행범 수, 색상을 갖는 데이터 프레임 생성
levels(col.level) <- sort(heat.colors(6), decreasing = TRUE)
usa.data <- data.frame(usa.data, col.level = col.level)
# Map 데이터 시각화


map('state', region = usa.data$states, fill = TRUE, col = as.character(usa.data$col.level))
title("USA Assault map")
legend(-76, 35, legends, fill = sort(heat.colors(6), decreasing = TRUE), cex = 0.7)


#연습문제 7.24
if (!require(mapproj)) install.packages('mapproj',repos = "http://cran.us.r-project.org")
library(mapproj)
data(unemp)
data(county.fips)

head(unemp)
head(county.fips)

# define color buckets
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")

newdata <- merge(county.fips, unemp, by="fips")
head(newdata)

# draw map
map("county", col = colors[newdata$colorBuckets], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.7, projection="polyconic")
title("unemployment by county, 2009")
legend("bottomright", leg.txt, fill = colors, bty = 'n')


#7.2.1.2 mapdata 패키지

#연습문제 7.25

if (!require(mapdata)) install.packages('mapdata',repos = "http://cran.us.r-project.org")
library(mapdata)
map(database = 'world', region = c('South Korea', 'North Korea'))
title("Korea map in maps packages")

map(database = 'worldHires', region = c('South Korea', 'North Korea'))
title("Korea map in mapdata packages")


#연습문제 7.26
map('worldHires', region=c('South Korea', 'North Korea', 'Japan', 'China'))
map('worldHires', region=c('South Korea'), col = 'blue', add = TRUE, fill = TRUE)
map('worldHires', region=c('North Korea'), col = 'red', add = TRUE, fill = TRUE)
map('worldHires', region=c('Japan'), col = 'black',add = TRUE, fill = TRUE)
map('worldHires', region=c('China'), col = 'yellow',add = TRUE, fill = TRUE)
map.scale()
map.axes()

p <- data.frame(x=rep(100,6), y=35:40)
points(p, pch=15:20)
#7.2.1.3 mapproj 패키지

#연습문제 7.27

library(mapproj)
if (!require(ggmap)) install.packages('ggmap',repos = "http://cran.us.r-project.org")
library(ggmap)

a<- "독도"
a<-iconv(a, from="cp949", to="UTF-8") 
geocode(a)

m <- map("worldHires", plot = FALSE)
map('worldHires', proj = 'azequalarea', orient = c(37.24223, 131.8643, 0))
map.grid(m, col = 2)
points(mapproject(list(y = 37.24223, x = 131.8643)), col = "blue", pch = "x", cex = 2)
title("지구본에서의 독도")

map('worldHires', proj = 'gilbert', orient = c(37.24223, 131.8643, 0))
map('worldHires', proj = 'mercator', orient = c(37.24223, 131.8643, 0))

# 자세한 정보는 ?mapproject


#7.2.2 maptools 패키지

#7.2.2.1 데이터 파일 불러오기

# shape파일 필요##############################################

if(!require(maptools)) install.packages("maptools",repos = "http://cran.us.r-project.org")
library(maptools)
if(!require(foreign)) install.packages("foreign",repos = "http://cran.us.r-project.org")
library(foreign)
if (!require(rgdal)) install.packages('rgdal',repos = "http://cran.us.r-project.org")
library(rgdal)

if(!require(sp)) install.packages("sp",repos = "http://cran.us.r-project.org")
library(sp)
if(!require(stringr)) install.packages("stringr",repos = "http://cran.us.r-project.org")
library(stringr)
if(!require(raster)) install.packages("raster",repos = "http://cran.us.r-project.org")
library(raster)


setwd("./SourceCodes/R_visualization_Tutorial_Codes/day3")

census <- read.csv("data_map/101_DT_1IN1002_F_2010.csv", skip=2)
head(census)
# 행정구역 경계 파일

file.exists('data_map/CTPRVN_201703/TL_SCCO_CTPRVN.shp')
# 1. 시도단위
s <- shapefile("data_map/CTPRVN_201703/TL_SCCO_CTPRVN.shp")  #raster
hrr.shp<-readShapePoly("data_map/CTPRVN_201703/TL_SCCO_CTPRVN.shp", verbose=TRUE)
plot(hrr.shp) # plot shape file in R  
hrr.shp@data$val <- 1:17
plot(hrr.shp, col=hrr.shp@data$val)

dbf <- read.dbf("data_map/CTPRVN_201703/TL_SCCO_CTPRVN.dbf")

hrr.shp@data
head(census)

census$C행정구역별.읍면동. <- sub("^\\'", "", census$C행정구역별.읍면동.)
head(census)

# shape파일 필요##############################################


#http://www.gisdeveloper.co.kr/?p=2332

# 1. 시도단위
hrr.shp<-readShapePoly("data_map/CTPRVN_201703/TL_SCCO_CTPRVN.shp", verbose=TRUE)
plot(hrr.shp) # plot shape file in R
hrr.shp@data  # Check the data

# hrr.shp@data를 보면 hrr.shp@data$CTPRVN_CD 가 50인 경우 제주도입니다.
hrr.shp@data[hrr.shp@data$CTPRVN_CD==50,]

# 제주도에 해당하는 부분만 선택
jeju1.shp <- subset(hrr.shp, hrr.shp@data$CTPRVN_CD==50) 
plot(jeju1.shp)

writePolyShape(jeju1.shp, "data_map/jeju1.shp")

# 2. 시군구단위
sig.shp<-readShapePoly("data_map/SIG_201703/TL_SCCO_SIG.shp", verbose=TRUE)
plot(sig.shp)
tail(sig.shp@data,10)

# sig.shp@data를 보면 50110과 50130이 제주도에 해당합니다.
# 해당지역을 선택하는 코드를 아래와 같이 짤 수 있습니다.

jeju2.shp <- subset(sig.shp, sig.shp@data$SIG_CD %in% c(50110,50130))
plot(jeju2.shp)

writePolyShape(jeju2.shp, "data_map/jeju2.shp")

# 3. 읍면동단위
# http://www.gisdeveloper.co.kr/?p=2332
eup.shp<-readShapePoly("data_map/EMD_201703/TL_SCCO_EMD.shp", verbose=TRUE)
plot(eup.shp)

head(eup.shp@data)
tail(eup.shp@data,100)
# 제주도에 해당하는 읍면동을 확인하면 제주시는 5011로 시작하고 서귀포시는 5013으로 시작합니다.
# 이 숫잔느 위의 시군구단위와 동일합니다.
# substr 함수는 원하는 위치의 값만 출력할 수 있습니다.
substr(eup.shp@data$EMD_CD,1,4)

# eup.shp@data$EMC_CD가 5011 또는 5013으로 시작하는 자료만 선택하여 저장합니다.
jeju3.shp <- subset(eup.shp, substr(eup.shp@data$EMD_CD,1,4) %in% c("5011","5013"))
plot(jeju3.shp, axes=T, cex.axis=0.9, bg="#3366CC", col="white", border="#666666")

writePolyShape(jeju3.shp, "data_map/jeju3.shp")

rm(list=ls())  #모든 저장된 자료 제거

# 저장한 자료 가져오기
jeju1<- readShapePoly("data_map/jeju1.shp", verbose= TRUE)
jeju2<- readShapePoly("data_map/jeju2.shp", verbose= TRUE)
jeju3<- readShapePoly("data_map/jeju3.shp", verbose= TRUE)

# 불러온 자료 그리
plot(jeju1, axes=T, cex.axis=0.9, bg="#3366CC", col="white", border="#666666")
plot(jeju2, axes=T, cex.axis=0.9, bg="#3366CC", col="white", border="#666666")
plot(jeju3, axes=T, cex.axis=0.9, bg="#3366CC", col="white", border="#666666")




#7.2.2.3 특정 지역 표시하기

#연습문제 7.29

if(!require(ggmap)) install.packages("ggmap");library(ggmap)

mountains <- c("설악산", "태백산", "지리산", "소백산", "한라산", "내장산", "북한산")
# 명산들의 위도와 경도의 조회
xy <- geocode(mountains)


mountains<-iconv(mountains, from="cp949", to="UTF-8") 
xy <- geocode(mountains)
xy <- xy[complete.cases(xy),]

# TM 중부 좌표계 : 통계청 지도
crsTMcenter <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs")
# WGS84 경위도 좌표계 : 구글 지도
crsWGS84lonlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# WGS84 경위도 좌표계를 TM 중부좌표계로 변환
xy.new <- spTransform(SpatialPoints(xy, proj4string=crsWGS84lonlat), crsTMcenter)
# 지도를 그리고 명산의 위치를 표시하기
plot(hrr.shp, bg="lightblue", col="cornsilk")
points(xy.new@coords, pch=17, col="blue", cex=2)
text(x=xy.new@coords[, "lon"], y=xy.new@coords[, "lat"],
     col="red", labels=mountains, adj=0, pos=4, offset=0.5)

#7.2.3 mapplots 패키지

#7.2.3.1 맵을 시각화하는 함수

#7.2.3.2 맵에 플롯을 그리는 함수
# shape파일 필요##############################################

#7.2.4 ggmap 패키지

#7.2.4.1 get_map() 함수

#7.2.4.2 ggmap() 함수

#7.2.4.3 qmap() 함수

#연습문제 7.32

a<- "통계교육원"
a<-iconv(a, from="cp949", to="UTF-8") 
geocode(a)
# 구글 맵에서 해당 위치의 맵 정보 가져오기
map <- get_googlemap(iconv("대전", from="cp949", to="UTF-8"), zoom = 11, maptype = "roadmap",
                     markers = data.frame(127.3787, 36.35879))
# 맵 그리기
ggmap(map)

