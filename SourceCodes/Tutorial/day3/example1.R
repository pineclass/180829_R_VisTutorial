setwd("C:/Users/USER/Documents/jeju-master/jeju-master")
if(!require(ggmap)) install.packages("ggmap"); library(ggmap) #구글 지도 이용
if(!require(gstat)) install.packages("gstat"); library(gstat) # 역거리가중법
if(!require(sp)) install.packages("sp"); library(sp) # 공간자료 분석 

 #관측소,관광지 위치####

data<-read.csv("data/weather.csv", header = TRUE)   #기상관측자료 불러오기
head(data)

stat<-read.csv("data/station.csv", header = TRUE)  #기상관측소 위치정보 가져오기
stat$ID2<-paste0(stat$type,"_",stat$ID)
stat$lat<-as.numeric(paste(stat$LON))
stat$lon<-as.numeric(paste(stat$LAT))
stat$type<-as.character(stat$type)
stat$ID<-NULL
stat$ID<-stat$ID2
head(stat)

tour_loc<-read.csv("data/tripadvisor.csv", header = TRUE)  # 관광명소 자료 가져오기
tour_loc<-tour_loc[complete.cases(tour_loc),]
tour_loc$type="Tourism"
head(tour_loc)

temp1<-cbind(tour_loc$type, tour_loc$경도, tour_loc$위도) #관광지 위치
temp2<-cbind(stat$type, stat$lon, stat$lat)  #기상관측자료 위치

temp<-rbind(temp1,temp2) #기상관측소와 관광지의 위치 시각화를 위한 자료생성
temp<-data.frame(temp)

colnames(temp)<-c("type","lon","lat")
temp[,1]<-as.factor(temp[,1])
temp[,2]<-as.numeric(paste(temp[,2]))
temp[,3]<-as.numeric(paste(temp[,3]))
temp

# 관광명소 및 기상관측소 위치정보를 지도로 표현하기
hdf <- get_map("Jeju")
a<-ggmap(hdf) + geom_point(aes(x = lon, y = lat, colour = type), size=3, alpha=0.9,  data=data.frame(temp))
plot(a)

######################################################################################## 
# 역거리 가중법을 이용한 관광지의 날씨 예측

t.data<-merge(data,stat[,c("ID","lat","lon")],by = "ID", all=TRUE)

tour_loc2<-tour_loc
coordinates(tour_loc2) <- ~경도+위도

year1="2014"
month1="07"
day1="30"

cal.f<-function(year1, month1, day1){ #년도, 월, 일을 넣으면 관광명소 기상정보 예측값이 생성되는 함수

data1<-paste0(year1,"-",month1,"-",day1) #원하는 날자

d.data<-t.data[t.data$date==data1,] # AWS+ASOS 기상관측소
d.data2<-t.data[t.data$date==data1 &t.data$ID %in% c("ASOS_184","ASOS_185","ASOS_188","ASOS_189") ,]
# 상대습도, 일조량은 ASOS만 관측되므로 ASOS 자료만 선책

d.data  # 선택된 자료 확인
d.data2 # 선택된 자료 확인

d.data <-d.data[complete.cases(d.data[,3:6]),] # 결측치 제거
d.data2 <-d.data2[complete.cases(d.data2[,8:9]),] # 결측치 제거

coordinates(d.data)=~lon+lat
coordinates(d.data2)=~lon+lat

d.data@data$pcp<-as.numeric(paste(d.data@data$pcp))

idw_1<-idw(formula = temp ~ 1, d.data,  newdata = tour_loc2, idp=1)  # apply idw to temperature
idw_2<-idw(formula = maxt ~ 1, d.data,  newdata = tour_loc2, idp=1)  # apply idw to max temperature
idw_3<-idw(formula = pcp ~ 1, d.data,  newdata = tour_loc2, idp=1)  # apply idw to precipitation
idw_4<-idw(formula = ws ~ 1, d.data,  newdata = tour_loc2, idp=1)  # apply idw to wind speed
idw_5<-idw(formula = minh ~ 1, d.data2,  newdata = tour_loc2, idp=1)  # apply idw to minimum humidity
idw_6<-idw(formula = avgh ~ 1, d.data2,  newdata = tour_loc2, idp=1)  # apply idw to average humidity
idw_7<-idw(formula = light ~ 1, d.data2,  newdata = tour_loc2, idp=1)  # apply idw to light duration

cl.data71<-cbind(temp=data.frame(idw_1)[,3], maxt=data.frame(idw_2)[,3], pcp=data.frame(idw_3)[,3], 
                 ws=data.frame(idw_4)[,3], minh=data.frame(idw_5)[,3], avgh=data.frame(idw_6)[,3], light=data.frame(idw_7)[,3])

cl.data71<-data.frame(cl.data71)

cl.data71$name<-tour_loc$name
cl.data71$lat<-tour_loc$위도
cl.data71$lon<-tour_loc$경도

# 한국형 관광기후지후 생성을 위한 요인 값 생성
KTCI_S <- cl.data71$light*0.5 # 일조시간

ws_cut<-cut(cl.data71$ws,breaks=c(0,0.8,1.6,2.5,3.40,5.5,6.75,8,10.7))
ws_cut<-as.numeric(ws_cut)
KTCI_W = 5-(ws_cut-1)*0.5 # 바람세기

# indoor exhibition center does not effect on wind
KTCI_W[which(tour_loc$세분화=="미술관/박물관/전시장" & KTCI_W<5)]=4.5

pcp_2<-(cl.data71$pcp-5)/10 
pcp_2[which(pcp_2<0)]=0
pcp_2[which(pcp_2>5)]=5
KTCI_P = 5- pcp_2 # 강수량

# indoor exhibition center does not effect on rainfall
KTCI_P[which(tour_loc$세분화=="미술관/박물관/전시장" & KTCI_P<5)]=4.5


KTCI_Ca = 5-(abs((cl.data71$temp-25)/25)+abs((cl.data71$avgh-50)/50)/2)*2.5 # 쾌적지수
KTCI_Cd = 5-(abs((cl.data71$maxt-25)/25)+abs((cl.data71$minh-50)/50)/2)*2.5 # 쾌적지수

KTCI = 2*(3.07*KTCI_Cd+1.90*KTCI_Ca+3.27*KTCI_P+0.90*KTCI_W+0.86*KTCI_S) # 한국형 관광기후지수

cl.data71$KTCI<-KTCI
cl.data71$category<-NA

cl.data71$category[which(cl.data71$KTCI>=90)]="1_ideal"
cl.data71$category[which(cl.data71$KTCI>=80 & cl.data71$KTCI<90)]="2_excellent"
cl.data71$category[which(cl.data71$KTCI>=70 & cl.data71$KTCI<80)]="3_very good"
cl.data71$category[which(cl.data71$KTCI>=60 & cl.data71$KTCI<70)]="4_good"
cl.data71$category[which(cl.data71$KTCI>=50 & cl.data71$KTCI<60)]="5_acceptable"
cl.data71$category[which(cl.data71$KTCI>=40 & cl.data71$KTCI<50)]="6_marginal"
cl.data71$category[which(cl.data71$KTCI<40)]="7_unfavorable"

## reflection of extreme weather effect 
cl.data71[which(cl.data71$maxt>=30),"category"]="7_unfavorable"
cl.data71[which(cl.data71$ws>=8),"category"]="7_unfavorable"
cl.data71[which(cl.data71$pcp>=110),"category"]="7_unfavorable"

cl.data71
# write.csv(cl.data71,paste0("E:/daily_data/",year1,"_",month1,"_",day1,".csv"))
return(cl.data71)
}


cal.f("2014","07","31")
cal.f("2014","08","31")
cal.f("2015","07","31")
cal.f("2015","08","31")

data.temp<-cal.f("2014","08","03")

#hdf <- get_map("Jeju")
a<-ggmap(hdf) + geom_point(data=data.temp, aes(x = lon, y = lat, colour = category), size=3, alpha=1) 
plot(a)
# 한국형 관광기후지수를 기반으로 한 관광지 추천


#############################################################################################
# tripadvisor 정보를 활용한 관광지 추천 알고리즘 제안
head(tour_loc)

tour.review<-rep(NA,97)

for (i in 1:97)
  
  tour.review[i]<-(tour_loc[i,2]*5+tour_loc[i,3]*4+tour_loc[i,4]*3+tour_loc[i,5]*2+tour_loc[i,6]*1)/sum(tour_loc[i,2:6])

tour_loc[,8]^0.05 *tour.review^0.5 # 별점와 리뷰수에 가중치 부여

tour_loc[order(data.temp$KTCI, decreasing = T)[1:10],1]  #KTCI 기준순위

tour_loc[order(tour_loc[,8]^0.05 *tour.review^0.5, decreasing = T)[1:10],1] #제안된 알고리즘 순위