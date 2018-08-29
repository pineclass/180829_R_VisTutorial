###############################
setwd("C:/Users/USER/Dropbox/R-work/R_vis/day3")

data<-read.csv("data/ASOS_sample.csv") 
station<-read.csv("data/stnInfo_20171018091530.csv")

t.data <- merge(data[,1:4], station[c(1,6,7,8)], by="지점")
head(t.data)
names(t.data)<-c("지점","date","avg","min","lat","lon","alt")

t.data$date <-as.Date(t.data$date)
t.data$date[1]+1
t.data$mon <- format(t.data$date, "%m")

boxplot(avg~지점, data=t.data, col="gold")  #관측소별 상자그림
boxplot(avg~지점, data=t.data[1:80,],col="gold")  #관측소별 상자그림 sample

if(!require(plyr)) install.packages("plyr");library(plyr)
t.data2<-ddply(t.data, ~지점, summarize, mean=mean(avg))
t.data2<-ddply(t.data, ~지점+mon, summarize, mean=mean(avg))
t.data3<-merge(t.data2, station[c(1,6,7,8)], by="지점")

boxplot(t.data3$mean)

#####################################################################
library(maps)
map('world','South Korea', fill=TRUE, col="cornsilk", bg="lightblue", xlim=c(125,131))
range(t.data3$mean)
cuts <- c(-5,0,5,10,15)
level<-cut(t.data3$mean, cuts, labels=c('blue','green','orange','red'))
points(t.data3$경도, t.data3$위도, col=paste(level), pch=16)

legend('bottomright', pch=15,col=c('blue','green','orange','red'), legend=c(-5,0,5,10))

#####################################################################
x<-seq(126,131, by=0.5)
y<-seq(33,38.5, by=0.5)
xy<-expand.grid(x,y)
names(xy)=c('경도','위도')
map('world','South Korea', fill=TRUE, col="cornsilk", bg="lightblue", xlim=c(125,131))
points(xy, pch=3, col="red")

if(!require(gstat))install.packages("gstat");library(gstat)
if(!require(sp))install.packages("sp");library(sp)

t.data4<-t.data3
coordinates(t.data4)=c('경도','위도')
coordinates(xy)=c('경도','위도')
gs <- idw(formula=mean~1, t.data4, newdata=xy, idp=4)

cuts <- c(-5,0,5,10,15)
level<-cut(gs$var1.pred, cuts, labels=c('blue','green','orange','red'))

map('world','South Korea', fill=TRUE, col="grey", bg="lightblue")
points(xy@coords[,1], xy@coords[,2], col=paste(level), pch=16)

level<-cut(t.data3$mean, cuts, labels=c('blue','green','orange','red'))
points(t.data3$경도, t.data3$위도, col=paste(level), pch=16)
points(t.data3$경도, t.data3$위도, col=1, pch=1)

#####################################################################
if(!require(akima))install.package("akima");library(akima)

int.scp <- interp(t.data3$경도,t.data3$위도, t.data3$mean, 
                 xo = seq(min(t.data3$경도), max(t.data3$경도), length = 150),
                 yo = seq(min(t.data3$위도), max(t.data3$위도), length = 150),
                 duplicate="strip")

map(database="world", region="south korea", ylim=c(34,38.9))
image(int.scp,  add=T)
contour(int.scp, add=T)
points(t.data3$경도, t.data3$위도, pch=16, cex=0.7)

x1 <- int.scp$x
y1 <- int.scp$y
xy <- expand.grid(x1, y1)
u <- map.where(database="world", x=xy[,1], y=xy[,2])
a <- rep(0, length(xy[,1]))
a[u=="South Korea"] <- 1
z <- as.vector(int.scp$z)
z[!a] <- NA
z <- matrix(z, nrow = length(int.scp$x))
int.scp$z <- z

map(database="world", region="south korea", ylim=c(34,38.9))
image(int.scp,  add=T)
contour(int.scp, add=T)
points(t.data3$경도, t.data3$위도, pch=16, cex=0.7)
#####################################################################

breaks_M <- seq(-5, 10, length=6)
length(breaks_M)
col_M<-c("#0100FF", "#2524FF", "#4948FF", "#6D6CFF", "#9190FF", "#B5B4FF", "#FFFFFF")
image(int.scp, col=col_M,  add=T)
contour(int.scp, add=T)
breaks1 <- seq(38.5, 34.5, length.out=length(breaks_M))

rect(130.6, breaks1[-length(breaks1)], 130.95, breaks1[-1], col = col_M,
     border = TRUE)
axis(4, at=breaks1,labels=breaks_M,cex.axis=0.8)

#####################################################################
t.data7<- t.data[t.data$date=="2017-01-02",]

int.scp <- interp(t.data7$lon,t.data7$lat, t.data7$min,
                  xo = seq(min(t.data7$lon), max(t.data7$lon), length = 150),
                  yo = seq(min(t.data7$lat), max(t.data7$lat), length = 150),
                  duplicate="strip")

x1 <- int.scp$x
y1 <- int.scp$y
xy <- expand.grid(x1, y1)
u <- map.where(database="world", x=xy[,1], y=xy[,2])
a <- rep(0, length(xy[,1]))
a[u=="South Korea"] <- 1
z <- as.vector(int.scp$z)
z[!a] <- NA
z <- matrix(z, nrow = length(int.scp$x))
int.scp$z <- z

map(database="world", region="south korea", ylim=c(34,38.9))
image(int.scp,  add=T)
contour(int.scp, add=T)
points(t.data3$경도, t.data3$위도, pch=16, cex=0.7)
map.scale(129,34.5, cex=.7)

