# -----------------------------------
# R visualization - 소스코드
#   출판사: 도서출판 인사이트
#   저자: 유충현, 홍성학
#   챕터: 9장
#   파일명: chapter_09.R
# -----------------------------------


# ==================== 소스순번: 001 ==================== 

pie.sales <- c(0.06, 0.15, 0.56, 0.09, 0.04, 0.1)
names(pie.sales) <- c("Blueberry", "Cherry", "Apple",
                        "Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales) # default colours


# ==================== 소스순번: 002 ==================== 

info.pie <- function(x, labels = names(x), col = NULL, border = NULL, etc=FALSE, ratio=FALSE,
                       lty = NULL, main = NULL, ...)
  {
      # 데이터 정렬
      x <- sort(x)
      x <- c(x[length(x)], x[-length(x)])
      # 파이 조각이 5개가 넘을 경우의 처리
      if (etc & length(x) > 5) {
          x <- sort(x, decreasing=TRUE)
          x <- c(x[1], sum(x[-c(1:4)]), x[4:2])
          names(x)[2] <- if (all(nchar(names(x), type="chars") == nchar(names(x), type="bytes")))
              "Others" else "기타"
      }
      # 백분율으 표시하는 로직
      if (ratio) names(x) <- paste(names(x), paste(prop.table(x)*100, "%", sep=""), sep="\n")
      # pie() 함수를 이용한 파이 차트 출력
      pie(x, clockwise=TRUE, col=col, border=border, lty=lty, main=main, ...)
  }


# ==================== 소스순번: 003 ==================== 

op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mai=c(1, 1, 1, 1), mar=c(1, 1, 1, 1))
info.pie(pie.sales)
info.pie(pie.sales, border="white", col="gray")
info.pie(pie.sales, border="white", col="gray", etc=TRUE)
info.pie(pie.sales, border="white", col="gray", ratio=TRUE)
par(op)


# ==================== 소스순번: 004 ==================== 

index <- c(35, 56, 39, 52, 70, 75)
names(index) <- c("2009", "2010", "2011", "2012", "2013", "2014")
barplot(index)


# ==================== 소스순번: 005 ==================== 

info.bar <- function(x, labels=names(x), col="gray", border=FALSE, main=NULL) {
      # Graphics Parameter 설정
      op <- par(no.readonly = TRUE)
      par(las=1)
      on.exit(par(op))
      # 차트의 각종 설정값 구하기
      posx <- barplot(x, border=col, col=col, axes=FALSE)
      usr <- par("usr")
      yaxp <- axTicks(side=2, par("yaxp"))
      minx <- ifelse(min(0, x)<0, min(x)-abs(min(x))/5, 0)
      # y-축의 지시선 그리기
      for (i in yaxp)
          lines(x=usr[1:2], y=c(i, i), lwd=0.8)
      # 음수 영역 표현하기
      if (minx < 0) {
          bg <- gray(level=0.7, alpha=0.3)
          rect(xleft=usr[1], ybottom=0, xright=usr[2], ytop=minx, border=bg, col=bg)
      }
      # 막대 그래프 그리기
      names(x) <- NULL
      barplot(x, border=col, col=col, axes=FALSE, main=main, add=T)
      axis(2, at=yaxp, labels=yaxp, tick=FALSE)
      # y-축이 0인 지시선 그리기
      lines(x=usr[1:2], y=c(0, 0), lwd=2.5)
  }


# ==================== 소스순번: 006 ==================== 

info.bar(index, main="연도별 지수 현황")


# ==================== 소스순번: 007 ==================== 

index <- c(-7, 6, 10, 12, 15)
names(index) <- c("2010", "2011", "2012", "2013", "2014")
info.bar(index, main="음수를 포함한 연도별 지수 현황")


# ==================== 소스순번: 008 ==================== 

data <- getStock(startDate="2014-01-01", endDate="2014-05-31")
stock <- rev(data$Close)
adj.stock <- rev(data$Adj.Close)
names(stock) <- names(adj.stock) <- rev(data$Date)
plot(stock, type="l")


# ==================== 소스순번: 009 ==================== 

info.line <- function(x, col="lightblue", lwd=3, main=NULL, ...)
  {
      # Graphics Parameter 설정
      op <- par(no.readonly = TRUE)
      par(las=1)
      on.exit(par(op))
      # 선 그래프의 그리기
      xlim <- c(-2, length(x))
      ylim <- range(x) + c(-1, 1) * diff(range(x))/4
      plot(x, col=col, xlim=xlim, ylim=ylim, type="l",
           main=main, xlab="", ylab="", lwd=lwd, axes=FALSE)
      # 차트의 각종 설정값 구하기
      usr <- par("usr")
      yaxp <- axTicks(side=2, par("yaxp"))
      xaxp <- axTicks(side=1, par("xaxp"))
      # y-축의 지시선 그리기
      for (i in yaxp) {
          lines(x=usr[1:2], y=c(i, i), lwd=ifelse(i==0, 2.5, 0.8))
      }
      # y-축 그리기
      axis(2, at=yaxp, labels=yaxp, tick=FALSE)
      # x-축 그리기
      for (i in xaxp) {
          if (i==0)
              i <- 1
          lines(c(i, i), c(yaxp[1], yaxp[1]-diff(range(yaxp))/100*length(yaxp)))
          if (yaxp[1]-usr[3] < 5) {
              mtext(side=1, at=i, names(x)[i])
          } else {
              text(i, yaxp[1]-diff(range(yaxp))/30*length(yaxp), names(x)[i])
          }
      }
  }


# ==================== 소스순번: 010 ==================== 

info.line(stock, main="Apple stock (2014-01-01 ~ 2014-05-31)")


# ==================== 소스순번: 011 ==================== 

info.line(adj.stock, main="Apple adjected stock (2014-01-01 ~ 2014-05-31)")


# ==================== 소스순번: 012 ==================== 

activity <- function (m=1, e=1, s=1, cex=1.5, lwd=58) {
      # 기본 환경값 정의
      n <- 100
      bangle <- (0:n)/n*2*pi
      fangle <- bangle[c(26:1, 100:27)]
      # 배경인 원을 표현하기 위한 속성 정의
      bband <- 4:1
      bx <-  bband %o% cos(bangle)
      by <-  bband %o% sin(bangle)
      # 전경인 링을 표현하기 위한 속성 정의
      fband <- 3.5:1.5
      fx <- fband %o% cos(fangle)
      fy <- fband %o% sin(fangle)
      # 색상과 라벨 정의
      bcols <- c("#470E1D", "#2C4202", "#133D3C", "#030303")
      fcols <- c("#FF0F5A", "#9BFE03", "#18D4DD")
      labels <-c("MOVE", "EXERCISE", "STAND")
      # 인포그래픽을 그릴 영역 정의
      par(pty='s', mar = rep(0.7, 4))
      plot(c(-5, 5),c(-5, 5), type='n', axes=F, xlab='', ylab='')
      # 배경인 원의 출력
      for (i in rev(bband)) {
          polygon(bx[i, ], by[i, ], col=bcols[i])
      }
      # 전경인 링의 출력
      index <- c(m , e, s)
      for (i in 1:3) {
          lines(fx[i, 1:(index[i]*n+1)], fy[i, 1:(index[i]*n+1)], col=fcols[i], lwd=lwd)
          text(x=fx[i, 1]-0.7, y=fy[i, 1], labels=labels[i], col=fcols[i], cex=cex, adj=1)
      }
  }


# ==================== 소스순번: 013 ==================== 

activity(0.7, 0.45, 0.25)


# ==================== 소스순번: 014 ==================== 

hist(faithful$waiting, main="간헐천 유휴 시간")


# ==================== 소스순번: 015 ==================== 

png("./korean_win.png")
hist(faithful$waiting, main = "간헐천 유휴 시간")
dev.off()


# ==================== 소스순번: 016 ==================== 

windowsFonts()  # Windows에 설치된 폰트 목록을 보여준다


# ==================== 소스순번: 017 ==================== 

nanumgothic <- windowsFont("나눔고딕")  # 나눔고딕 폰트
windowsFonts(nanumgothic=nanumgothic)


# ==================== 소스순번: 018 ==================== 

par(family="nanumgothic")
hist(faithful$waiting, main="간헐천 유휴 시간")


# ==================== 소스순번: 019 ==================== 

png("./windows_korean_1.png")
par(family="nanumgothic")
hist(faithful$waiting, main="간헐천 유휴 시간")
dev.off()


# ==================== 소스순번: 020 ==================== 

nanumgothic <- windowsFont("나눔고딕")  # 나눔고딕 폰트
windowsFonts(nanumgothic=nanumgothic)   # 이전에 실행 한적이 있다면 이 줄은 에러가 발생한다.
pdf("./windows_korean_1.pdf")
par(family="nanumgothic")
hist(faithful$waiting, main="간헐천 유휴 시간")
dev.off()


# ==================== 소스순번: 021 ==================== 

hist(faithful$waiting, main = "간헐천 유휴 시간")


# ==================== 소스순번: 022 ==================== 

install.packages("showtext")
library("showtext")


# ==================== 소스순번: 023 ==================== 

font.add("나눔고딕", "c:\\Windows\\Fonts\\nanumgothic.ttf")
pdf("./windows_korean_3.pdf")
par(family="나눔고딕")
showtext.begin()
hist(faithful$waiting, main="간헐천 유휴 시간")
showtext.end()
dev.off()


# ==================== 소스순번: 024 ==================== 

hist(faithful$waiting, main="간헐천 유휴 시간")


# ==================== 소스순번: 025 ==================== 

par(family="나눔고딕")
hist(faithful$waiting, main="간헐천 유휴 시간")


# ==================== 소스순번: 026 ==================== 

par(family="Apple SD Gothic Neo")
hist(faithful$waiting, main="간헐천 유휴 시간")


# ==================== 소스순번: 027 ==================== 

op <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))
par(family="나눔명조")
hist(faithful$waiting, main="간헐천 유휴시간 히스토그램 (나눔명조)", xlab="유휴시간", ylab="빈도")
par(family="NanumMyeongjo")
hist(faithful$waiting, main="간헐천 유휴시간 히스토그램 (NanumMyeongjo)", xlab="유휴시간", ylab="빈도")
par(family="NanumMyeongjoExtraBold")
hist(faithful$waiting, main="간헐천 유휴시간 히스토그램 (NanumMyeongjoExtraBold)", xlab="유휴시간", ylab="빈도")
par(family="나눔명조 ExtraBold")
hist(faithful$waiting, main="간헐천 유휴시간 히스토그램 (나눔명조 ExtraBold)", xlab="유휴시간", ylab="빈도")
par(op)


# ==================== 소스순번: 028 ==================== 

par(family="나눔고딕")
hist(faithful$waiting, main="간헐천 유휴 시간")


# ==================== 소스순번: 029 ==================== 

pdf()
par(family="나눔고딕")
hist(faithful$waiting, main="간헐천 유휴 시간")
dev.off()


# ==================== 소스순번: 030 ==================== 

names(postscriptFonts())  # 또는 names(pdfFonts())


# ==================== 소스순번: 031 ==================== 

pdf("./korea_default_font.pdf")
par(family='Korea1')
hist(faithful$waiting, main = "간헐천 유휴 시간")
dev.off()


# ==================== 소스순번: 032 ==================== 

cairo_pdf("./linux_korean_1.pdf")
par(family="나눔고딕")
hist(faithful$waiting, main="간헐천 유휴 시간")
dev.off()


# ==================== 소스순번: 033 ==================== 

library(showtext)
font.add("나눔손글씨 붓", "/usr/share/fonts/truetype/korean_fonts/NanumBrush.ttf")
pdf("./linux_korean_2.pdf")
par(family="나눔손글씨 붓")
showtext.begin()
hist(faithful$waiting, main="간헐천 유휴 시간")
showtext.end()
dev.off()


# ==================== 소스순번: 034 ==================== 

x <- rnorm(200)
y <- rnorm(200)
fit <- lm(y ~x)
par(mfrow=c(2, 2), mar=c(2, 1, 2, 1), pty="s")
plot(fit)
box(which="outer", lwd=2)


# ==================== 소스순번: 035 ==================== 

hist(x)
box(which="outer", lwd=2)


# ==================== 소스순번: 036 ==================== 

# 현재의 그래픽 파라미터를 op에 저장
op <- par(no.readonly = TRUE)
# 그래픽 파라미터 설정
par(mfrow=c(2, 2), mar=c(2, 1, 2, 1), pty="s")
plot(fit)
# 저장된 그래픽 파라미터가 담긴 op를 그래픽 파라미터로 설정
par(op)


# ==================== 소스순번: 037 ==================== 

my.chart <- function(x) {
    # 현재의 그래픽 파라미터를 op에 저장
    op <- par(no.readonly = TRUE)
    # 함수 종료 시 그래픽 파라미터가 담긴 op를 그래픽 파라미터로 설정
    on.exit(par(op))
    # # 그래픽 파라미터 설정
    par(mfrow=c(2, 2), mar=c(2, 1, 2, 1), pty="s")
    # 함수의 내용 기술 시작
    # 함수의 내용 기술 종료
  }


# ==================== 소스순번: 038 ==================== 

set.seed(1)
x <- sample(12)
names(x) <- month.name
barplot(x)


# ==================== 소스순번: 039 ==================== 

apropos("\\.abb$")


# ==================== 소스순번: 040 ==================== 

names(x) <- month.abb
barplot(x)


# ==================== 소스순번: 041 ==================== 

x <- c("count", "amount", "total count", "total amount", "average")
abbreviate(x, 3)


# ==================== 소스순번: 042 ==================== 

set.seed(1)
x <- sample(12) * 1000000
names(x) <- month.abb
barplot(x, xlab="월도", main="지수 형태의 축 눈금 라벨")


# ==================== 소스순번: 043 ==================== 

y <- pretty(x)
y.lab <- formatC(y, format="d")
y.lab
barplot(x, axes=FALSE, xlab="월도", main="십진수의 축 눈금 라벨")
axis(side=2, at=y, labels=y.lab)


# ==================== 소스순번: 044 ==================== 

op <- par(no.readonly=TRUE)
par(mar=c(3, 5, 3, 2))
barplot(x, axes=FALSE, xlab="월도", main="수평 방향의 축 눈금 라벨")
axis(side=2, at=y, labels=y.lab, las=1)
par(op)


# ==================== 소스순번: 045 ==================== 

new.x <- x/1000000
barplot(new.x, ylab="(단위 : 백만)", las=1, xlab="월도",
          main="백만 단위의 축 눈금 라벨")


# ==================== 소스순번: 046 ==================== 

barplot(new.x, las=1, xlab="월도", main="단위를 포함한 백만 단위의 축 눈금 라벨")
mtext("(단위 : 백만)", side=3, line=1, at=-2, adj=0)


# ==================== 소스순번: 047 ==================== 

install.packages("plot3D")
library(plot3D)


# ==================== 소스순번: 048 ==================== 

par(mar = c(1, 1, 1, 1))
scatter3D(x = mtcars$wt, y = mtcars$gear, z = mtcars$mpg, pch = 19)


# ==================== 소스순번: 049 ==================== 

par(mar = c(1, 1, 1, 1))
scatter3D(x = mtcars$wt, y = mtcars$gear, z = mtcars$mpg, pch = 19,
            main = "mtcars", xlab = "weight", ylab = "gear", zlab = "mile per galon")


# ==================== 소스순번: 050 ==================== 

par(mar = c(1, 1, 1, 1))
scatter3D(x = mtcars$wt, y = mtcars$gear, z = mtcars$mpg, pch = 19,
            main = "mtcars", xlab = "weight", ylab = "gear", zlab = "mile per galon",
            bty = "g", ticktype = "detailed", d = 3, theta = 30, phi = 20)


# ==================== 소스순번: 051 ==================== 

# 선형 회귀모형 적합
fit <- lm(mpg ~ wt + gear, data = mtcars)

wt.pred <- seq(1.5, 5.5, length.out = 30)
gear.pred <- sample(c(3, 4, 5, 6), 30, replace = TRUE)
exp.xy <- expand.grid(wt = wt.pred, gear = gear.pred)
mpg.pred <- matrix(nrow = 30, ncol = 30,
                     data = predict(fit, newdata = data.frame(exp.xy), + interval = "prediction"))
# 모형으로 예측값 구하기
fits <- predict(fit)
# 산점도 그리기
par(mar = c(1, 1, 1, 1))
scatter3D(x = mtcars$wt, y = mtcars$gear, z = mtcars$mpg, pch = 19,
            xlab = "weight", ylab = "gear", zlab = "mile per galon", main = "mtcars",
            bty = "g", ticktype = "detailed", d = 3, theta = 30, phi = 20,
            surf = list(x = wt.pred, y = gear.pred, z = mpg.pred, facets = NA,
            fit = fits))


# ==================== 소스순번: 052 ==================== 

# 데이터 전처리 (비닝)
x_breaks <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), length.out = 30)
y_breaks <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), length.out = 30)
x_values <- 0.5 * (x_breaks[-1] + x_breaks[-length(x_breaks)])
y_values <- 0.5 * (y_breaks[-1] + y_breaks[-length(y_breaks)])
z_values <- table(cut(iris$Sepal.Length, x_breaks),
                    cut(iris$Sepal.Width, y_breaks))
# 히스토그램 그리기
par(mar = c(1, 1, 1, 1))
hist3D(x = x_values, y = y_values, z = z_values, border = "black",
         xlab = "Sepal Length", ylab = "Sepal Width", zlab = "Freq", main = "Iris",
         bty = "g", ticktype = "detailed", d = 3)


# ==================== 소스순번: 053 ==================== 

par(mar = c(1, 1, 1, 1))
text3D(USArrests$Murder, USArrests$Assault, USArrests$Rape, colvar = USArrests$UrbanPop,
         labels = rownames(USArrests), cex = 0.7, main = "USA arrests",
         xlab = "Murder", ylab = "Assault", zlab = "Rape", theta = 30,
         phi = 20, clab = c("Urban", "Pop"), adj = 0.5, bty = "g", ticktype = "detailed", d=3)

scatter3D(USArrests$Murder, USArrests$Assault, USArrests$Rape - 1,
            colvar = USArrests$UrbanPop, type = "h", pch = ".", add = TRUE)


# ==================== 소스순번: 054 ==================== 

install.packages("rgl")
library(rgl)


# ==================== 소스순번: 055 ==================== 

plot3d(x = mtcars$wt, y = mtcars$gear, z = mtcars$mpg, col = mtcars$mpg,
         size = 2, type = "s", main = "mtcars", xlab = "wt", ylab = "gear",
         zlab = "mpg")


# ==================== 소스순번: 056 ==================== 

demo(rgl)


# ==================== 소스순번: 057 ==================== 

rgl.bg(col = "#ffffff")
hist3d(iris$Sepal.Length, iris$Sepal.Width, nclass = 10, scale = 40)
axes3d()
title3d("Iris", "", "Length", "Width", "Freq")

