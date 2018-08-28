# -----------------------------------
# R visualization - 소스코드
#   출판사: 도서출판 인사이트
#   저자: 유충현, 홍성학
#   챕터: 4장
#   파일명: chapter_04.R
# -----------------------------------


# ==================== 소스순번: 001 ==================== 

x <- seq(0, 1, length=10)
set.seed(1)
y <- 10 * x + rnorm(length(x))
# 1단계. 새로운 그래픽 윈도우 생성
plot.new()
# 2단계. 캔버스의 높이와 폭 정의 및 종횡비 지정
plot.window(xlim = range(x), ylim = range(y))
# 3단계. 플롯 그리기
points(x, y, col="blue", cex=1.2, pch=16)
abline(lm(y~x), col="red")
# 4단계. 플롯 꾸미기
axis(1)
axis(2)
title(main = "f(x)=10*x+e")
title(ylab = "f(x)")
title(xlab = "sequence")
box()


# ==================== 소스순번: 002 ==================== 

radius <- function () {
      par(mfrow=c(1, 1), pty='s')
      plot(c(-1, 1), c(-1, 1), type='n', axes=F, xlab=", ylab=")
      angle <- (0:(10*6))/(10*6)*2*pi

      # 내접원을 그릴 좌표의 생성 및 내접원 그리기
      x1 <- cos(angle)/2
      y1 <- sin(angle)/2
      lines(x1, y1, col="blue")
      # 삼각형을 그릴 좌표의 계산
      x <- rep(0, 3)
      y <- rep(0, 3)
      for (i in 0:3) {
          x[i+1] <- cos(angle[i*20+1])
          y[i+1] <- sin(angle[i*20+1])
      }
      # 삼각형 그리기
      lines(x, y)
      # 원점 찍기
      points(0, 0, pch=19)
      # 정삼각형의 중심과 꼭지점간의 선 그리기
      lines(c(0, x[2]), c(0, y[2]))
      # 원의 반지름 표현하기
      lines(c(0, cos(pi)/2), c(0, 0), col="red", lwd=2)
      text(cos(pi)/4, -0.05, "반지름", cex=0.7)
      # 반지름을 구성하는 각도
      text(-0.15, 0.1, labels=expression(frac(pi, 2*n)), cex=0.8)
  }
radius()


# ==================== 소스순번: 003 ==================== 

# 사용자 정의 함수 구현
circle <- function (points) {
      par(pty='s')
      plot(c(-1, 1), c(-1, 1), type='n', axes=F, xlab=", ylab=")
      angle <- (0:(10*points))/(10*points)*2*pi
      # 내접원 좌표 구하기
      x1 <- cos(2*pi/(points*2))*cos(angle)
      y1 <- cos(2*pi/(points*2))*sin(angle)
      # 외접원 좌표 구하기
      x2 <- cos(angle)
      y2 <- sin(angle)
      # 내접원과 외접원 그리기
      lines(x1, y1, col="blue")
      lines(x2, y2)

      # 다각형 좌표 구하기
      x <- rep(0, points)
      y <- rep(0, points)
      for (i in 0:points) {
          x[i+1] <- cos(angle[i*10+1])
          y[i+1] <- sin(angle[i*10+1])
      }
      # 다각형 그리기
      lines(x, y, col="red", lwd=1.7)
  }

op <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
circle(3)
circle(6)
circle(9)
circle(12)
par(op)


# ==================== 소스순번: 004 ==================== 

temp <- c( 3.9, 4.4, 6.7, 8.9, 12.2, 15.6, 17.8, 17.2, 15.0, 10.6, 6.7, 4.4)
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
op <- par(no.readonly = TRUE)
plot.new()
par(mar=rep(0.1, 4))
plot.window(xlim=c(-25, 25), ylim=c(-25, 25), asp=1)
# 회색의 다각형을 만든다.
theta <- seq(0, length=12, by=2 * pi/12)
x <- temp * cos(theta)
y <- temp * sin(theta)
polygon(x, y, col = "lightgray")
# 12개의 기준선(파선)을 그린다.
segments(0, 0, 20 * cos(theta), 20 * sin(theta), lty="dotted")
# 원을 그린다.
segments(0, 0, 20 * cos(theta), 20 * sin(theta), lty="dotted")
phi <- seq(3, 360 - 3, length = 72) * (pi/180)
for (r in c(5, 10, 15))
       lines(r * cos(phi), r * sin(phi), lty="dotted")
lines(20 * cos(phi), 20 * sin(phi))
# 12월도 Text를 출력한다.
text(24 * cos(theta), 23 * sin(theta), month)
# 기준점수 Text를 출력한다.
labs <- seq(5, 20, by=5)
text(labs, rep(0, length(labs)), labs)
par(op)


# ==================== 소스순번: 005 ==================== 

set.seed(1)
d <- Sys.time() + sample(-5000:5000, size=1000) * 60 * 2
set.seed(10)
# "년-월-일 시:분:초" 포맷으로 가상의 데이터 생성
sample.log <-
      data.frame(dtime=as.character(d),
                 cnt=sample(1:5, size=1000, replace=TRUE) + 1:1000)
head(sample.log)
# "년-월-일" 포맷으로 일자별 집계한 데이터 생성
date.format <- strftime(sample.log$dtime, format="%Y-%m-%d")
date.log <- aggregate(sample.log$cnt, list(date.format), sum)
names(date.log) <- c("date", "cnt")
date.log <- date.log[-c(1, NROW(date.log)), ]
date.log


# ==================== 소스순번: 006 ==================== 

trendPlot(sample.log$dtime, sample.log$cnt, stats="mean", trend="all",
            pch=21, col=1, bg="blue",
            main="trend by date", xlab="date", ylab="mean")


# ==================== 소스순번: 007 ==================== 

trendPlot(date.log$date, date.log$cnt, trend="all", pch=21, col=1, bg="blue", type="b",
            main="trend by date", xlab="date", ylab="count")


# ==================== 소스순번: 008 ==================== 

set.seed(7)
lineChart(rnorm(30), shadow=TRUE, main="custom line chart", fg="seashell", bg="lightcyan")


# ==================== 소스순번: 009 ==================== 

lineChart(cars$speed, cars$dist, col=3, lowess=TRUE, shadow=TRUE, main="lowess by cars data",
            fg="lightcyan", bg="peachpuff")


# ==================== 소스순번: 010 ==================== 

layout(mat = rbind(c(0,6,6,6,0),
                     c(0,0,4,0,0),
                     c(3,0,1,5,0),
                     c(0,0,0,0,0),
                     c(0,0,2,0,0)),
         height = c(lcm(2), lcm(2), 1, lcm(2),lcm(1)),
         width = c(lcm(1), lcm(2), 1, lcm(2), lcm(1)))
layout.show(6)
box("outer", lty = "dotted")


# ==================== 소스순번: 011 ==================== 

boxScatter(cars$speed, cars$dist, xlab="Speed (mph)",
             ylab="Stopping distance (ft)", main="Speed and Stopping Distances of Cars",
             col="blue", bcol="lightgray", cex=1.2)


# ==================== 소스순번: 012 ==================== 

boxScatter(USArrests$Murder, USArrests$Assault, xlab="Murder arrests (per 100,000)",
             ylab="Assault arrests (per 100,000)", main="Violent Crime Rates by US State",
             col="black", bcol="lightblue", cex=1.2)


# ==================== 소스순번: 013 ==================== 

layout(matrix(c(0,6,6,6,0,
                  0,3,0,4,0,
                  0,1,5,2,0,
                  0,7,7,7,0), nc = 5, byrow = TRUE),
         widths = c(lcm(2), 1, lcm(2), 1, lcm(2)),
         heights = c(lcm(2), lcm(1), 1, lcm(3)))
layout.show(7)
box("outer", lty = "dotted")


# ==================== 소스순번: 014 ==================== 

getDropboxRData <- function(URL) {
      require(RCurl)

      # URL로부터 바이너리 정보 다운로드
      bin <- getBinaryURL(URL, ssl.verifypeer=FALSE)
      # 파일 이름 추출하기
      strs <- strsplit(URL, "/")[[1]]
      fname <- strs[length(strs)]
      # URL로부터의 파일 정보 기술
      con <- file(fname, open="wb")
      # 이진 파일로 출력하기
      writeBin(bin, con)
      # 이진 파일 닫기
      close(con)
      # R Data 파일 불러 들이기
      load(file=fname, envir = parent.frame(), verbose=TRUE)
  }


# ==================== 소스순번: 015 ==================== 

getDropboxRData("https://dl.dropboxusercontent.com/u/46305178/datas/population.info.RData")
Loading objects:
  population.info

# 데이터 건수와 변수의 개수
dim(population.info)
# 몇 건의 데이터 조회
head(population.info)
# 데이터의 구조 파악
str(population.info)
# 1970년도 서울의 남성 인구 집계
male <- xtabs(frequency~gender+age.group, data=population.info,
                subset=gender=="male" & year=="1970" & region=="Seoul",
                drop.unused.levels=TRUE)
# 1970년도 서울의 여성 인구 집계
female <- xtabs(frequency~gender+age.group, data=population.info,
                  subset=gender=="female" & year=="1970" & region=="Seoul",
                  drop.unused.levels=TRUE)
# 1970년도 서울의 인구피라미드
plotPairBar(male, female, names(male[1, ]), main="Korea census(seoul, 1970)",
              sub="population counts")


# ==================== 소스순번: 016 ==================== 

# DescTools 패키지의 로드, 없으면 설치 후 로드
if (!require(DescTools)) {
      install.packages("DescTools")
      require(DescTools)
  }
m.pop<-c(3.2,3.5,3.6,3.6,3.5,3.5,3.9,3.7,3.9,3.5,
           3.2,2.8,2.2,1.8,1.5,1.3,0.7,0.4)
f.pop<-c(3.2,3.4,3.5,3.5,3.5,3.7,4,3.8,3.9,3.6,3.2,
           2.5,2,1.7,1.5,1.3,1,0.8)
age <- c("0-4","5-9","10-14","15-19","20-24","25-29",
           "30-34","35-39","40-44","45-49","50-54",
           "55-59","60-64","65-69","70-74","75-79","80-44","85+")
# 인구 피라미드 그리기
x <- PlotPyramid(m.pop, f.pop,
                   ylab = age, space = 0, col = c("cornflowerblue", "indianred"),
                   main="Age distribution at baseline of HELP study",
                   lxlab="male", rxlab="female" )


# ==================== 소스순번: 017 ==================== 

mapinfo <- data.frame(I_LATITUDE = c(37.51484, 37.53197, 37.51155, 37.51136, 37.53621, 37.50740),
                        I_LONGITUDE = c(127.0738, 127.0786, 127.0806, 127.0832, 127.0832, 127.0840),
                        CNT = c(309, 720, 741, 812, 2576, 526),
                        RATIO = c(.5, .29, .21, .37, .12, .05))

ScatterOnStaticMap(x=mapinfo[,c("CNT","RATIO")], lat=mapinfo$I_LATITUDE, lon=mapinfo$I_LONGITUDE, alpha=0.8,
                     maptype="roadmap", title=list(labels="Scatter plot example using Google Maps", font=1,
                     col="red", cex=1.5))


# ==================== 소스순번: 018 ==================== 

ScatterOnStaticMap(x=mapinfo[,c("CNT","RATIO")], lat=mapinfo$I_LATITUDE, lon=mapinfo$I_LONGITUDE, alpha=0.5,
                     maptype="satellite", title=list(labels="종합운동장 주변 주제도 예제", col="blue", cex=2.0))


# ==================== 소스순번: 019 ==================== 

piedata <- rbind(c(800, 400, 800), c(300, 200, 500))
colnames(piedata) <- c("자가", "월세", "전세")
piedata
lat <- c(35.18254, 35.19294)
lon <- c(129.2041, 129.2061)
PieOnStaticMap(piedata, lat, lon, alpha=0.8, maptype="satellite", legend=TRUE,
                 title=list(labels="주택 소유여부 현황", font=2, col="blue", cex=1.5))


# ==================== 소스순번: 020 ==================== 

PieOnStaticMap(piedata, lat, lon, alpha=0.8, maptype="terrain", legend=TRUE, legend.bg="gray90",
                 title=list(labels="주택 소유여부 현황", font=2, col="blue", cex=1.5))

