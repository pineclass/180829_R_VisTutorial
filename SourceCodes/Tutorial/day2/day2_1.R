# 6장 ggplot2 그래픽스

# 6.1 ggplot2 소개

# 6.1.1 ggplot2 설치

if (!require(ggplot2)) {
  install.packages("ggplot2", ,repos = "http://cran.us.r-project.org")
  require(ggplot2)
}

# 6.1.4 ggplot 객체

# p385
# mtcars 데이터 프레임의 내용 확인
head(mtcars)

plot(mtcars$wt, mtcars$mpg, col=mtcars$cyl, pch=16)
legend("topright", legend=unique(mtcars$cyl), col=unique(mtcars$cyl), pch=16)

# aes()함수로 x-축과 y-축, 그리고 색상의 매핑
p <- ggplot(mtcars, aes(wt, mpg, colour=cyl))
# Geometric object로 점(point) 정의
p <- p + geom_point()
# ggplot 클래스 객체인 p의 출력
p

p <- ggplot(mtcars, aes(wt, mpg, colour=cyl))
p  # 아무 그림도 출력되지 않음

p <- ggplot(mtcars, aes(wt, mpg, colour="blue"))
p <- p + geom_point()
# ggplot 클래스 객체인 p의 출력
p #색이 파랑색이 들어가지 않음. 어떻게 하면 파랑색??

p <- ggplot(mtcars, aes(wt, mpg))
p <- p + geom_point(colour="blue")
p


# ggplot 클래스 객체의 성분들
attributes(p)
# p의 클래스 조회
class(p)
# ggplot 클래스 객체의 집계
summary(p)

# p385
# aesthetic Mappings로 축을 매핑
p <- ggplot(mtcars, aes(factor(cyl), fill=factor(cyl)))
# Geometric object 중에 막대(bar)를 정의
p <- p + geom_bar(width=.5)
p     # 실린더 수에 따른 bar chart를 그려줌

p <- p + facet_grid(. ~ gear)
# ggplot 클래스 객체인 p의 출력
p     # 기어 수에 따른 그림 생성

# p389
# aes()함수로 축을 매핑
p <- ggplot(mtcars, aes(wt, mpg))
# Geometric object인 점(point)을 정의
p <- p + geom_point()
# Geometric object인 평활(smooth)을 정의
p <- p + geom_smooth(method="loess")
# ggplot 클래스 객체인 p의 출력
p


# 6.1.6 함수 분류

# 접두어가 "geom"인 Geoms 함수군의 목록
apropos("^geom*_")

# 6.2 Plot creation 함수군

# p.391
# qplot() 함수 이용
qplot(mtcars$wt, mtcars$mpg)
# qqplot() 함수 이용
ggplot(data=mtcars, aes(x=wt, y=mpg)) # 에러 발생

# qplot() 함수의 인수들
args(qplot)
# ggplot() 함수의 인수들
args(ggplot)

qplot(mtcars$wt, mtcars$mpg, geom="point")

qplot(wt, mpg, data=mtcars, geom="point")

ggplot(data=mtcars, aes(x=wt, y=mpg)) + geom_point()



# 6.3 Geoms 함수군
# 6.3.1 geom_point() 함수

# p395
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point() 

# p396
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point(aes(x=wt, y=mpg))
print(p)

ggplot(mtcars, aes(wt, mpg)) + geom_point()

# 연습문제 6.1
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_point(colour="orange", size=6)

# p398
# 연습문제 6.2
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_point(aes(colour=cyl, size=gear))

# 6.3.2 geom_abline() 함수


# p400
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_abline()

lm(mpg~wt, data=mtcars)

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + xlim(1, 5) + ylim(10, 35)
p + geom_abline(intercept = 37, slope = -5) + geom_point()

# p401
# 연습문제 6.3
mtcars_coefs <- coef(lm(mpg ~ wt, mtcars))
mtcars_coefs
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p + geom_abline(intercept=mtcars_coefs["(Intercept)"],
                slope=mtcars_coefs["wt"], colour="red")


# p402
# 연습문제 6.4
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p + stat_smooth(method="lm", se=TRUE, colour="red")

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p + stat_smooth(method="loess", se=TRUE, colour="red")


# 6.3.3 geom_bar() 함수

# p403
p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar()

p <- ggplot(data=mtcars, aes(cyl))
p + geom_bar()

# p405
p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar(aes(fill=cyl), colour="black")

# p405
# 연습문제 6.5
p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar(aes(fill=factor(gear)), colour="black")

# p407
# 연습문제 6.6
p <- ggplot(data=mtcars, aes(factor(cyl)))
p <- p + geom_bar(aes(fill=factor(gear)), colour="black")
p + coord_flip()

# p408
p <- ggplot(data=mtcars, aes(factor(cyl)))
p <- p + geom_bar(aes(fill=factor(carb)), colour="black")
p + facet_wrap(~ gear)


# 6.3.4 geom_ribbon() 함수

# p409
huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
ggplot(data=huron, aes(x=year)) + geom_area(aes(y=level))


# p410
p <- ggplot(data=huron, aes(x=year))
p <- p + geom_area(aes(y=level))
p + coord_cartesian(ylim=c(570, 590))

# p411
p <- ggplot(data=huron, aes(x=year))
p <- p + geom_area(aes(y=level))
p + coord_cartesian(ylim = c(min(huron$level)-2, max(huron$level)+2))

p <- ggplot(huron, aes(x=year))
p + geom_ribbon(aes(ymin=min(level)-2, ymax=level+2))

# p412
p <- ggplot(huron, aes(x=year))
p + geom_ribbon(aes(ymin=level-2, ymax=level+2), colour="blue")


# 연습문제 6.7
# p413
if (!require(quantmod)) install.packages("quantmod",repos = "http://cran.us.r-project.org")
require(quantmod)
# 애플의 주가 정보 가져오기
getSymbols("AAPL", from=as.Date("2014-05-01"),to=as.Date("2014-05-31"))
# 리본 플롯과 loess 곡선 등 그리기
head(AAPL)
p <- ggplot(AAPL, aes(x=index(AAPL), y=AAPL.Close))
p <- p + geom_ribbon(aes(min=AAPL.Low, max=AAPL.High), fill="lightblue", colour="black")
p <- p + geom_point(aes(y=AAPL.Close), colour="black", size=5)
p <- p + geom_line(aes(y=AAPL.Close), colour="blue")
p <- p + stat_smooth(method="loess", se=FALSE, colour="red", lwd=1.2)
p

# 6.3.5 geom_boxplot() 함수

# p414
p <- ggplot(mtcars, aes(factor(cyl), mpg))
p + geom_boxplot()

p <- ggplot(mtcars, aes(factor(cyl), mpg))
p <- p + geom_boxplot(aes(fill=factor(carb)))
p <- p + facet_grid(~am) + scale_fill_brewer()
p


p <- ggplot(mtcars, aes(factor(cyl), mpg))
p <- p + geom_boxplot(aes(fill=factor(carb)))
p <- p + facet_grid(am~.)
p

# 6.3.6 geom_histogram() 함수

# p416
if (!require("ggplot2movies")) install.packages("ggplot2movies", repos = "http://cran.us.r-project.org")
library(ggplot2movies)

dim(movies)
p <- ggplot(data=movies, aes(x=rating))
p + geom_histogram()

# p417
# 연습문제 6.8
p <- ggplot(data=movies, aes(x=rating))
p + geom_histogram(binwidth=1)

# p418
p <- ggplot(data=movies, aes(x=rating))
p <- p + geom_histogram(binwidth=1, aes(y=..density.., fill=..count..), colour="black")
p

p <- p + geom_density(colour="red")
p + scale_fill_gradient(low="white", high="#496ff5")

# 6.3.7 geom_density() 함수
# p420
p <- ggplot(movies, aes(x = rating))
p + geom_density()

p <- ggplot(movies, aes(x = rating))
p + geom_density(aes(fill=factor(mpaa)), alpha=0.25)


# 6.3.8 geom_density2d() 함수

# p421
data(geyser, package="MASS")
head(geyser)
p <- ggplot(geyser, aes(x=duration, y=waiting))
p <- p + geom_point()
p <- p + xlim(min(geyser$duration)-0.5, max(geyser$duration)+0.5)
p <- p + ylim(min(geyser$waiting)-5, max(geyser$waiting)+5)
p + geom_density2d()


# 6.3.9 geom_countour() 함수

# p423
# 마운트 화산의 지형을 등고선 플롯
if (!require(reshape2)) {
  install.packages("reshape2",repos = "http://cran.us.r-project.org")
  require(reshape2)
}
# 2차원 행렬을 변수가 세 개인 데이터 프레임으로 변환
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
p <- ggplot(volcano3d, aes(x, y, z = z))
p + geom_contour(binwidth = 2, size = 0.5, aes(colour= ..level..))


# 6.3.10 geom_text() 함수

# p424
# 산점도의 점 위치에 mtcars 데이터 프레임의 행 이름을 플로팅
head(mtcars)
row.names(mtcars)

p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
p <- p + geom_point()
p + geom_text(aes(x=wt+0.05, colour=factor(cyl)), size=5, hjust=0)


# 6.3.11 geom_map() 함수

# p425
# 미국의 각 주별 살인범죄 현황 플롯
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
if (!require(maps)) {
  install.packages("maps",repos = "http://cran.us.r-project.org")
  require(maps)
}
# 미국 주별 지도 가져오기
states_map <- map_data("state")
head(states_map)
# 데이터 전처리
crimesm <- melt(crimes, id=1)
# 각 주별 살인범죄에 대한 주제도 그리기
p <- ggplot(crimes, aes(map_id=state))
p <- p + geom_map(aes(fill=Murder), map=states_map)
p <- p + expand_limits(x=states_map$long, y=states_map$lat)
p + coord_map()

# 6.4 Statistics 함수군

# 6.4.1 stat_bin() 함수

# p427
p <- ggplot(movies, aes(x = rating))
p + stat_bin(binwidth = 0.5, aes(fill = ..count..), colour = "black")

# 6.4.2 stat_density() 함수

p <- ggplot(diamonds, aes(x = price))
p <- p + stat_density(aes(ymax = ..density..,  ymin = -..density..),
                      fill = "blue", colour = "black", alpha = 0.50,
                      geom = "area", position = "identity")
p + facet_grid(. ~ cut)

# p428
p <- ggplot(diamonds, aes(x = price, fill=cut))
p +  stat_density(aes(ymax = ..density..,  ymin = -..density..),
                  colour = "black", alpha = 0.15,
                  geom = "area", position = "identity")

# 6.4.3 stat_hexbin() 함수

# 연습문제 6.3
if (!require(hexbin)) {
  install.packages("hexbin",repos = "http://cran.us.r-project.org")
  require(hexbin)
}
g <- ggplot(diamonds, aes(carat, price))
g + stat_binhex(bins=20)


# 6.4.4 stat_ecdf() 함수

# p430
# 정규분포의 누적분포 곡선
df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 6)), g = gl(2, 100))
head(df)
p <- ggplot(data=df, aes(x, colour = g))
p + stat_ecdf(geom="line", size=1)
p + stat_ecdf(geom="point", size=1)


# 6.4.5 stat_function() 함수

# p431
set.seed(1)
d <- data.frame(x = rnorm(100))
p <- ggplot(d, aes(x = x))
p <- p + geom_density(fill = "green", alpha = 0.15)
p + stat_function(fun = dnorm, colour = "red", fill="red", alpha=0.15, geom="area")