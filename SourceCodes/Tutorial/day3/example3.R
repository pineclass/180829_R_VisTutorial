if (!require(stringr)) install.packages("stringr") ;library(stringr)

url1 <- paste0("http://www.weather.go.kr/weather/earthquake_volcano/domesticlist.jsp" )
line1 <- readLines(url1, encoding = "euc-kr")   #HTML source code 불러오기
title1 <- line1[str_detect(line1, "<tr><td>")]  #str_detect() 함수를 사용하여 <tr><td>가 나오는 문장 가져오기
title1 <- gsub("<.+?>|\t", "", title1)          #gsub() 함수를 사용하여 <  > 안에 들어가는 문자 삭제
title2 <- title1[str_detect(title1, "E")] 

pos.1<-str_locate(title2,"/")  #날짜에 해당하는 값을 가져오기 위한 위치
pos.2<-str_locate(title2,"N")  # 위도를 가져오기 위한 위치  
pos.3<-str_locate(title2,"E")  # 경도를 가져오기 위한 위치

number<-substr(title1,1,pos.1[,1]-5) #지진번호
day<-substr(title1,pos.1[,1]-4,pos.1[,1]+5)   #substr() 함수를 이용하여 날짜를 가져와 day에 저장
size<-substr(title1,pos.1[,1]+15,pos.1[,1]+17)  #substr() 함수를 이용하여 지진 규모를 size에 저장
temp<-pos.2[,1]-pos.1[,1]-18 

lat <- substr(title1, pos.2[,1]-temp, pos.2[,1]-1) #위도 저장
lat <- as.numeric(lat) 

lon <- substr(title1, pos.3[,1]-7, pos.3[,1]-1) #경도 저장
lon <- gsub("N","",lon) 
lon <- as.numeric(lon) 

data1 <- cbind(number=number, day=day, size=size, lat=lat, lon=lon) #행을 합쳐서 data.frame을 만듬
data1=data.frame(data1) 

data1$size=as.numeric(paste(data1$size)) 
data1$lon=as.numeric(paste(data1$lon)) 
data1$lat=as.numeric(paste(data1$lat)) 

data1   #최종 생산된 데이터 (HTML의 데이터를 원하는 형태로 정제)


# HTMLtable을 이용하여 쉽게 변환
if (!require(XML)) install.packages("XML") ;library(XML)
if (!require(RCurl)) install.packages("RCurl") ;library(RCurl)

url2 <-iconv(getURL("http://www.weather.go.kr/weather/earthquake_volcano/domesticlist.jsp",
                    .encoding="euc-kr"), from="euc-kr", to='UTF-8')
tables <-as.data.frame(readHTMLTable(url2, encoding="UTF-8"))
dim(tables)
head(tables)
names(tables) <- c("no","date","mag","depth","lat","lon","address","map")
head(tables)

tables$lat <- as.numeric(substr(tables$lat,1,5))
tables$lon <- as.numeric(substr(tables$lon,1,6))
####################################################################################

myfunc1<-function(url){   #함수로 처리
  
  tables <-as.data.frame(readHTMLTable(url, encoding="UTF-8"))
  dim(tables)
  head(tables)
  names(tables) <- c("no","date","mag","depth","lat","lon","address","map")
  head(tables)
  
  tables$lat <- as.numeric(substr(tables$lat,1,5))
  tables$lon <- as.numeric(substr(tables$lon,1,6))
  tables$mag <- as.numeric(paste(tables$mag))
  
  return(tables) 
} 

end.date <- as.Date("2018-05-28")    # 지진 조사 마지막일
start.date <- as.Date("2016-01-01")  # 지진 조사 시작일

k<-seq(start.date, end.date, 30)

end.date-start.date

t.data <- list()
for (i in 1:(length(k)-1)){
url <- paste0("http://www.weather.go.kr/weather/earthquake_volcano/domesticlist.jsp?startTm=",k[i],"&endTm=",k[i+1],"&startSize=2&endSize=999&startLat=&endLat=&startLon=&endLon=&lat=&lon=&dist=&keyword=&x=44&y=13" )
data1<-myfunc1(url) 
t.data<-rbind(t.data, data1)
}

dim(t.data)

# check max magnitute date and data 
t.data[which(t.data$mag==max(t.data$mag, na.rm=T)),] 
t.data$col<-NA 
t.data[which(round(t.data$mag,0)==2),"col"]="lightblue" 
t.data[which(round(t.data$mag,0)==3),"col"]="Green" 
t.data[which(round(t.data$mag,0)==4),"col"]="Orange" 
t.data[which(round(t.data$mag,0)==5),"col"]="Red" 

map('world',c('South Korea','North Korea'), xlim=c(121,132), ylim=c(31,43)) 
points(t.data$lon, t.data$lat, cex=t.data$mag/1.5, pch=16, col=t.data$col) 

axis(1)
axis(2)
box()

##################################################################
load(file="data/earthquake.rda") #1980년~2016년까지 지진자료

plot(0,0, xlim=c(124,132),ylim=c(33,43),type="n")
image(x,y,mat.freq, add=T, col = rainbow(40))
contour(x,y, mat.freq, add=TRUE, col="black", levels=seq(2,8,0.5), lty=2)
map('world','South Korea', add=T)
map('world','North Korea', add=T)

breaks1 <- seq(42, 33, length.out=30)
rect(131.2, breaks1[-length(breaks1)], 131.7, breaks1[-1], col = rainbow(40),
     border = F)
text(131.5,breaks1, round(seq(3,8,length.out=30),1), cex=0.7)
