setwd("C:/Users/USER/Documents/jeju-master/jeju-master")
 if (!require(sp)) install.packages("sp") ;library(sp)
 if (!require(maptools)) install.packages("maptools") ;library(maptools)
 if (!require(maps)) install.packages("maps") ;library(maps) #지도 자료


express<-readShapeLines("data/express_WGS84.shp") #한국 고속도로 자료 불러오기
map('world','South Korea', fill=T, col="gray")
plot(express, add=T, col="white", lwd=2)
# lwd 선의 두께

# 실시간 기상자료 가져오기
if (!require(XML)) install.packages("XML") ;library(XML)
if (!require(RCurl)) install.packages("RCurl") ;library(RCurl)

url2<-iconv(getURL("http://www.weather.go.kr/weather/observation/currentweather.jsp?type=t99&mode=0&stn=0&auto_man=a",
                   .encoding="euc-kr"),from="euc-kr",to='UTF-8')
tables<-as.data.frame(readHTMLTable(url2,encoding='UTF-8'))
tables # 수집된 기상자료 확

names(tables)<-
  c("id","current","vis","cloud","l.cloud","Tcurrent","dew","sensible","prec","rh","dir","ws","hpa")

tables<-tables[tables$id!="지점",] # 불필요한 행 제거

tables1<-tables[,c(1,3)]
tables1[,2]<-paste(tables1[,2])

tables1[tables1[,2]=="20 이상",2]="20" 
tables # 정제된 실시간 기상관측 정보

# 실시간 고속도로 속도 가져오기
if (!require(plyr)) install.packages("plyr") ;library(plyr)
url = "http://data.ex.co.kr/openapi/odtraffic/trafficAmountByRealtime?key=3314135116&type=xml"
raw.data <- xmlParse(url)
real.data<-ldply(xmlToList(raw.data), function(x) { data.frame(x[!names(x)=="author"]) } )
head(real.data)

# 문제점 고속도로의 표준노드링크와 고속도로 속도의 표준 단위가 상이함

load(file="data/final_vds.rda") # 고속도로 속도 단위 도로단위 vds

#######################################################################################
new.vds<-vds4[,3:5]
names(new.vds)<-c("vdsId","경도","위도")

v.data<-real.data[,c("speed","vdsId")]
new.v.data<-v.data[which(as.numeric(paste(v.data$speed))>0),]

v.vds<-merge(new.v.data, new.vds, by="vdsId")

load(file="data/link_gis.rda")
coordinates(v.vds) <-c("경도","위도")
link.gis2<-link.gis
coordinates(link.gis2) <-c("경도","위도")

#역거리 가중법을 이용하여 표준노드링크 단위 고속도로 속도 예측
gis2.idw2<-gstat::idw(speed~ 1, v.vds, newdata=link.gis2, idp=2.0) 
link.gis$speed<-gis2.idw2$var1.pred

cuts <-seq(0,110,length.out=25)
pred.level<-cut(link.gis$speed,cuts,rev(rainbow(n = 24, start=0, end=2/6))) #속도에 따른 속도 부여
table(pred.level)
cuts

map('world', 'South Korea', fill=TRUE, col="lightgrey", xlim=c(125,130.4), ylim=c(34,39))
points(link.gis$경도, link.gis$위도, col=paste(pred.level), cex=0.5, pch=15)
map.axes()


express@data$speed<-link.gis$speed #구글 지도로 그리기 위한 속도자료 입력
express@data$sd <- 0.694*link.gis$speed+link.gis$speed^2/(254*0.63) # 정지거리 입력

###############################################################################
if(!require(RgoogleMaps)) install.packages("RgoogleMaps");  library(RgoogleMaps)
if(!require(plotGoogleMaps)) install.packages("plotGoogleMaps");  library(plotGoogleMaps)
      
# 구글지도를 통해 생성된 정보 시각화
express@proj4string =CRS('+proj=longlat +datum=WGS84')
m1=plotGoogleMaps(express,zcol=29,colPalette=rainbow(n = 24, start=0, end=2/6),control.width='50%',control.height='50%',
                  api="https://maps.googleapis.com/maps/api/js?key=AIzaSyCkuAgeN7WipKLaNSUAJTeoTRceEFYKOKc&callback=initMap")
