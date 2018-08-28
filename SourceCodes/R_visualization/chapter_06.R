# -----------------------------------
# R visualization - 소스코드
#   출판사: 도서출판 인사이트
#   저자: 유충현, 홍성학
#   챕터: 6장
#   파일명: chapter_06.R
# -----------------------------------


# ==================== 소스순번: 001 ==================== 

if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}


# ==================== 소스순번: 002 ==================== 

# mtcars 데이터 프레임의 내용 확인
head(mtcars)
# aes()함수로 x-축과 y-축, 그리고 색상의 매핑
p <- ggplot(mtcars, aes(wt, mpg, colour=cyl))
# Geometric object로 점(point) 정의
p <- p + geom_point()
# ggplot 클래스 객체인 p의 출력
p


# ==================== 소스순번: 003 ==================== 

# ggplot 클래스 객체의 성분들
attributes(p)
# p의 클래스 조회
class(p)
# ggplot 클래스 객체의 집계
summary(p)


# ==================== 소스순번: 004 ==================== 

# aesthetic Mappings로 축을 매핑
p <- ggplot(mtcars, aes(factor(cyl), fill=factor(cyl)))
# Geometric object 중에 막대(bar)를 정의
p <- p + geom_bar(width=.5)
p <- p + facet_grid(. ~ gear)
# ggplot 클래스 객체인 p의 출력
p


# ==================== 소스순번: 005 ==================== 

summary(p)


# ==================== 소스순번: 006 ==================== 

# aes()함수로 축을 매핑
p <- ggplot(mtcars, aes(wt, mpg))
# Geometric object인 점(point)을 정의
p <- p + geom_point()
# Geometric object인 평활(smooth)을 정의
p <- p + geom_smooth(method="loess")
# ggplot 클래스 객체인 p의 출력
p


# ==================== 소스순번: 007 ==================== 

summary(p)


# ==================== 소스순번: 008 ==================== 

apropos("^geom*_")


# ==================== 소스순번: 009 ==================== 

qplot(mtcars$wt, mtcars$mpg)


# ==================== 소스순번: 010 ==================== 

ggplot(data=mtcars, aes(x=wt, y=mpg))


# ==================== 소스순번: 011 ==================== 

# qplot() 함수의 인수들
args(qplot)
# ggplot() 함수의 인수들
args(ggplot)


# ==================== 소스순번: 012 ==================== 

qplot(mtcars$wt, mtcars$mpg, geom="point")


# ==================== 소스순번: 013 ==================== 

ggplot(data=mtcars, aes(x=wt, y=mpg)) + geom_point()


# ==================== 소스순번: 014 ==================== 

qplot(wt, mpg, data=mtcars, geom="point")


# ==================== 소스순번: 015 ==================== 

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()


# ==================== 소스순번: 016 ==================== 

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point(aes(x=wt, y=mpg))
print(p)


# ==================== 소스순번: 017 ==================== 

ggplot(mtcars, aes(wt, mpg)) + geom_point()


# ==================== 소스순번: 018 ==================== 

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_point(colour="orange", size=6)


# ==================== 소스순번: 019 ==================== 

summary(p)


# ==================== 소스순번: 020 ==================== 

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_point(aes(colour=cyl, size=gear))


# ==================== 소스순번: 021 ==================== 

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_abline()


# ==================== 소스순번: 022 ==================== 

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + xlim(1, 5) + ylim(10, 35)
p + geom_abline(intercept = 37, slope = -5)


# ==================== 소스순번: 023 ==================== 

mtcars_coefs <- coef(lm(mpg ~ wt, mtcars))
mtcars_coefs
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p + geom_abline(intercept=mtcars_coefs["(Intercept)"],
                  slope=mtcars_coefs["wt"], colour="red")


# ==================== 소스순번: 024 ==================== 

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p + stat_smooth(method="lm", se=FALSE, colour="red")


# ==================== 소스순번: 025 ==================== 

p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar()


# ==================== 소스순번: 026 ==================== 

p <- ggplot(data=mtcars, aes(cyl))
p + geom_bar()


# ==================== 소스순번: 027 ==================== 

p <- ggplot(data=mtcars, aes(cyl))
p + geom_bar(binwidth=1)


# ==================== 소스순번: 028 ==================== 

p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar(aes(fill=cyl), colour="black")


# ==================== 소스순번: 029 ==================== 

p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar(aes(fill=factor(gear)), colour="black")


# ==================== 소스순번: 030 ==================== 

p <- ggplot(data=mtcars, aes(factor(cyl)))
p <- p + geom_bar(aes(fill=factor(gear)), colour="black")
p + coord_flip()


# ==================== 소스순번: 031 ==================== 

p <- ggplot(data=mtcars, aes(factor(cyl)))
p <- p + geom_bar(aes(fill=factor(carb)), colour="black")
p + facet_wrap(~ gear)


# ==================== 소스순번: 032 ==================== 

huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
ggplot(data=huron, aes(x=year)) + geom_area(aes(y=level))


# ==================== 소스순번: 033 ==================== 

p <- ggplot(data=huron, aes(x=year))
p <- p + geom_area(aes(y=level))
p + coord_cartesian(ylim=c(570, 590))


# ==================== 소스순번: 034 ==================== 

p <- ggplot(data=huron, aes(x=year))
p <- p + geom_area(aes(y=level))
p + coord_cartesian(ylim = c(min(huron$level)-2, max(huron$level)+2))


# ==================== 소스순번: 035 ==================== 

p <- ggplot(huron, aes(x=year))
p + geom_ribbon(aes(ymin=min(level)-2, ymax=level+2))


# ==================== 소스순번: 036 ==================== 

p <- ggplot(huron, aes(x=year))
p + geom_ribbon(aes(ymin=level-2, ymax=level+2), colour="blue")


# ==================== 소스순번: 037 ==================== 

if (!require(quantmod)) {
     install.packages("quantmod")
     require(quantmod)
 }
# 애플의 주가 정보 가져오기
getSymbols("AAPL", from=as.Date("2014-05-01"),to=as.Date("2014-05-31"))
# 리본 플롯과 loess 곡선 등 그리기
p <- ggplot(AAPL, aes(x=index(AAPL), y=AAPL.Close))
p <- p + geom_ribbon(aes(min=AAPL.Low, max=AAPL.High), fill="lightblue", colour="black")
p <- p + geom_point(aes(y=AAPL.Close), colour="black", size=5)
p <- p + geom_line(aes(y=AAPL.Close), colour="blue")
p <- p + stat_smooth(method="loess", se=FALSE, colour="red", lwd=1.2)
p


# ==================== 소스순번: 038 ==================== 

p <- ggplot(mtcars, aes(factor(cyl), mpg))
p + geom_boxplot()


# ==================== 소스순번: 039 ==================== 

p <- ggplot(mtcars, aes(factor(cyl), mpg))
p <- p + geom_boxplot(aes(fill=factor(carb)))
p <- p + facet_grid(~am) + scale_fill_brewer()
p


# ==================== 소스순번: 040 ==================== 

dim(movies)
p <- ggplot(data=movies, aes(x=rating))
p + geom_histogram()


# ==================== 소스순번: 041 ==================== 

p <- ggplot(data=movies, aes(x=rating))
p + geom_histogram(binwidth=1)


# ==================== 소스순번: 042 ==================== 

p <- ggplot(data=movies, aes(x=rating))
p <- p + geom_histogram(binwidth=1, aes(y=..density.., fill=..count..), colour="black")
p <- p + geom_density(colour="red")
p + scale_fill_gradient(low="white", high="#496ff5")


# ==================== 소스순번: 043 ==================== 

summary(p)


# ==================== 소스순번: 044 ==================== 

p <- ggplot(movies, aes(x = rating))
p + geom_density()


# ==================== 소스순번: 045 ==================== 

p <- ggplot(movies, aes(x = rating))
p + geom_density(aes(fill=factor(mpaa)), alpha=0.25)


# ==================== 소스순번: 046 ==================== 

data(geyser, package="MASS")
p <- ggplot(geyser, aes(x=duration, y=waiting))
p <- p + geom_point()
p <- p + xlim(min(geyser$duration)-0.5, max(geyser$duration)+0.5)
p <- p + ylim(min(geyser$waiting)-5, max(geyser$waiting)+5)
p + geom_density2d()


# ==================== 소스순번: 047 ==================== 

if (!require(reshape2)) {
     install.packages("reshape2")
     require(reshape2)
 }
# 2차원 행렬을 변수가 세 개인 데이터 프레임으로 변환
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
p <- ggplot(volcano3d, aes(x, y, z = z))
p + geom_contour(binwidth = 2, size = 0.5, aes(colour= ..level..))


# ==================== 소스순번: 048 ==================== 

p <- ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars)))
p <- p + geom_point()
p + geom_text(aes(x=wt+0.05, colour=factor(cyl)), size=5, hjust=0)


# ==================== 소스순번: 049 ==================== 

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
if (!require(maps)) {
     install.packages("maps")
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


# ==================== 소스순번: 050 ==================== 

p <- ggplot(movies, aes(x = rating))
p + stat_bin(binwidth = 0.5, aes(fill = ..count..), colour = "black")


# ==================== 소스순번: 051 ==================== 

p <- ggplot(diamonds, aes(x = price))
p <- p + stat_density(aes(ymax = ..density..,  ymin = -..density..),
                      fill = "blue", colour = "black", alpha = 0.50,
                      geom = "area", position = "identity")
p + facet_grid(. ~ cut)


# ==================== 소스순번: 052 ==================== 

p <- ggplot(diamonds, aes(x = price, fill=cut))
p +  stat_density(aes(ymax = ..density..,  ymin = -..density..),
                  colour = "black", alpha = 0.15,
                  geom = "area", position = "identity")


# ==================== 소스순번: 053 ==================== 

g <- ggplot(diamonds, aes(carat, price))
g + stat_binhex(bin=20)


# ==================== 소스순번: 054 ==================== 

df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 6)), g = gl(2, 100))
p <- ggplot(df, aes(x, colour = g))
p + stat_ecdf(geom="line", size=1)


# ==================== 소스순번: 055 ==================== 

set.seed(1)
d <- data.frame(x = rnorm(100))
p <- ggplot(d, aes(x = x))
p <- p + geom_density(fill = "green", alpha = 0.15)
p + stat_function(fun = dnorm, colour = "red", fill="red", alpha=0.15, geom="area")


# ==================== 소스순번: 056 ==================== 

p <- ggplot(data=mtcars, aes(x=mpg, y=cyl, alpha=cyl))
p + geom_point(size=10)


# ==================== 소스순번: 057 ==================== 

p <- ggplot(data=mtcars, aes(x=mpg, y=cyl, alpha=cyl))
p <- p + geom_point(size=10)
p + scale_alpha(range=c(0.4, 0.8))


# ==================== 소스순번: 058 ==================== 

p <- ggplot(data=mtcars, aes(x=mpg, y=cyl, alpha=factor(cyl)))
p <- p + geom_point(size=10)
p + scale_alpha_discrete(range=c(0.4, 0.8))


# ==================== 소스순번: 059 ==================== 

library(scales)
show_col(brewer_pal(pal="RdYlBu")(9))


# ==================== 소스순번: 060 ==================== 

p <- ggplot(data=diamonds, aes(price, carat, colour=clarity))
p <- p + geom_point()
p + scale_colour_brewer(type="seq", palette=5)


# ==================== 소스순번: 061 ==================== 

p <- ggplot(data=diamonds, aes(price, carat, colour=clarity))
p <- p + geom_point()
p + scale_colour_brewer(palette="Dark2")


# ==================== 소스순번: 062 ==================== 

p <- ggplot(data=diamonds, aes(price, fill=clarity))
p <- p + geom_histogram(binwidth=500)
p + scale_fill_brewer(palette="YlGn")


# ==================== 소스순번: 063 ==================== 

dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
p <- ggplot(data=dsub, aes(x, y, colour=z))
p <- p + geom_point()
p + scale_colour_gradient(limits=c(3, 3.6), low="red", high="green")


# ==================== 소스순번: 064 ==================== 

range(dsub$z)
boxplot(dsub$z)


# ==================== 소스순번: 065 ==================== 

p <- ggplot(data=dsub, aes(x, fill=..count..))
p <- p + geom_histogram(binwidth=0.02)
p + scale_fill_continuous(low="lightblue", high="blue", limits=c(200, 700), na.value="white")


# ==================== 소스순번: 066 ==================== 

dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
p <- ggplot(data=dsub, aes(x, y, colour=diff))
p <- p + geom_point()
p + scale_colour_gradient2(low="red", high="blue", mid="white", midpoint=0.15)


# ==================== 소스순번: 067 ==================== 

dsub <- data.frame(x=letters[1:5], y=c(-3, 3, 5, 2, -2))
p <- ggplot(data=dsub, aes(x, y, fill=y))
p <- p + geom_bar(stat="identity")
p + scale_fill_gradient2()


# ==================== 소스순번: 068 ==================== 

dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
p <- ggplot(data=dsub, aes(x, y, colour=diff))
p <- p + geom_point()
p + scale_colour_gradientn(colours=rainbow(7))


# ==================== 소스순번: 069 ==================== 

p <- ggplot(data=dsub, aes(x, y, colour=diff))
p <- p + geom_point()
p + scale_colour_gradientn(colours=rainbow(7), values=seq(-0.51, 0.8, length=7))


# ==================== 소스순번: 070 ==================== 

new_mtcars <- mtcars
new_mtcars$miss <- factor(sample(c(NA, 1:5), nrow(new_mtcars), rep = TRUE))
p <- ggplot(data=new_mtcars, aes(x=mpg, y=wt, colour = miss))
p <- p + geom_point(size=3)
p + scale_colour_grey(start=0.3, end=0.8, na.value = "red")


# ==================== 소스순번: 071 ==================== 

new_mtcars <- mtcars
new_mtcars$miss <- factor(sample(c(NA, 1:5), nrow(new_mtcars), rep = TRUE))
p <- ggplot(data=new_mtcars, aes(x=mpg, y=wt, colour = miss))
p <- p + geom_point(size=5)
p + scale_colour_hue()


# ==================== 소스순번: 072 ==================== 

p <- ggplot(data=new_mtcars, aes(x=mpg, y=wt, colour = miss))
p <- p + geom_point(size=5)
p + scale_colour_hue(h=c(90, 180), l=80, c=50)


# ==================== 소스순번: 073 ==================== 

colours <- c("red", "green", "blue", "yellow", "orange")
sizes <- c(1, 2, 3, 4, 5) + 3
df <- data.frame(x=1:5, y=1:5)
p <- ggplot(data=df, aes(x, y, colour = colours, size=sizes))
p <- p + geom_point()
p <- p + scale_colour_identity()
p <- p + scale_size_identity()
p


# ==================== 소스순번: 074 ==================== 

# cyl 변수의 수준
levels(factor(mtcars$cyl))
p <- ggplot(data = mtcars, aes(x = mpg, y = wt, colour = factor(cyl)))
p <- p + geom_point(size = 3)
p + scale_colour_manual(values = c("red", "blue", "green"))


# ==================== 소스순번: 075 ==================== 

p <- ggplot(data = mtcars, aes(x=mpg, y=wt, colour=factor(cyl)))
p <- p + geom_point(size=3)
p + scale_colour_manual(values = c("8"="red", "6"="blue", "4"="green"))


# ==================== 소스순번: 076 ==================== 

library(reshape2)
library(plyr)
ecm <- melt(economics, id = "date")
rescale01 <- function(x) (x - min(x)) / diff(range(x))
ecm <- ddply(ecm, "variable", transform, value = rescale01(value))
p <- ggplot(data=ecm, aes(date, value, group=variable, linetype=variable, colour=variable))
p <- p + geom_line()
p + scale_linetype_discrete()


# ==================== 소스순번: 077 ==================== 

dsmall <- diamonds[sample(nrow(diamonds), 100), ]
p <- ggplot(data=dsmall, aes(x=carat, y=price, shape=cut))
p <- p + geom_point(size=3)
p + scale_shape_discrete(solid=FALSE)


# ==================== 소스순번: 078 ==================== 

dsmall <- diamonds[sample(nrow(diamonds), 100), ]
p <- ggplot(data=dsmall, aes(x=carat, y=price, size=cut))
p <- p + geom_point(alpha=0.3)
p + scale_size_discrete(range = c(0, 10))
# 수준의 이름
levels(dsmall$cut)
# 예상되는 점의 크기
seq(0, 10, length=5)


# ==================== 소스순번: 079 ==================== 

p <- ggplot(data=movies, aes(x=rating, y=votes))
p <- p + geom_point()
p <- p + scale_x_continuous(limits=c(2.5, 9))
p + scale_y_continuous(limits=c(0, 10000))


# ==================== 소스순번: 080 ==================== 

p <- ggplot(data=movies, aes(x=rating, y=votes))
p <- p + geom_point()
p <- p + scale_x_reverse()
p + scale_y_reverse()


# ==================== 소스순번: 081 ==================== 

p <- ggplot(data=movies, aes(x=rating, y=votes))
p <- p + geom_point()
p <- p + scale_x_log10()
p + scale_y_log10()


# ==================== 소스순번: 082 ==================== 

p <- ggplot(data=movies, aes(x=rating, y=votes))
p <- p + geom_point()
p <- p + scale_x_sqrt()
p + scale_y_sqrt()


# ==================== 소스순번: 083 ==================== 

p <- ggplot(data=subset(diamonds, carat > 1), aes(x=cut, y=clarity, colour=carat))
p <- p + geom_jitter()
p + scale_x_discrete("Cutting", labels=paste("Grade", 1:5))


# ==================== 소스순번: 084 ==================== 

p <- ggplot(data=subset(diamonds, carat > 1), aes(x=cut, y=clarity, colour=carat))
p <- p + geom_jitter()
p + scale_x_discrete("Cutting", labels=paste("Grade", 1:5), limits=c("Fair", "Good", "Very Good"))


# ==================== 소스순번: 085 ==================== 

p <- ggplot(data=economics, aes(x=date, y=psavert))
p + geom_path()


# ==================== 소스순번: 086 ==================== 

p <- ggplot(data=economics, aes(x=date, y=psavert))
p <- p + geom_path()
p + scale_x_date("21 century", limits=c(as.Date("2000-01-01"), max(economics$date)))


# ==================== 소스순번: 087 ==================== 

dat <- data.frame(x = 1:5, y = 1:5, p = 1:5, q = factor(1:5), r = factor(1:5))
p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r))
p + geom_point()


# ==================== 소스순번: 088 ==================== 

p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r))
p <- p + geom_point()
p <- p + guides(colour = guide_colorbar(), size = guide_legend(), shape = guide_legend())


# ==================== 소스순번: 089 ==================== 

p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r))
p <- p + geom_point()
p + guides(colour = guide_legend("title"), size = guide_legend("title"), shape = guide_legend("title"))


# ==================== 소스순번: 090 ==================== 

# x-축에 매핑된 데이터의 범위
range(mtcars$mpg)
# y-축에 매핑된 데이터의 범위
range(mtcars$wt)
p <- ggplot(data = mtcars, aes(x = mpg, y = wt))
p <- p + geom_point()
p + expand_limits(x=0, y=c(-1, 10))


# ==================== 소스순번: 091 ==================== 

p <- ggplot(data = mtcars, aes(x = mpg, y = wt))
p <- p + geom_point(size=5)
p <- p + xlim(15, 25)
p <- p + ylim(2, 4.5)
p


# ==================== 소스순번: 092 ==================== 

p <- ggplot(data = mtcars, aes(x = mpg, y = wt))
p <- p + geom_point()
p <- p + labs(title = "New main title")
p <- p + labs(x = "New x labels")
p + labs(y = "New y labels")


# ==================== 소스순번: 093 ==================== 

p <- ggplot(data = mtcars, aes(x = mpg, y = wt))
p <- p + geom_point()
p <- p + ggtitle("New main title")
p <- p + xlab("New x labels")
p + ylab("New y labels")


# ==================== 소스순번: 094 ==================== 

p <- qplot(mpg, wt, data = mtcars)
p <- p + xlab("New x labels")
update_labels(p, list(x = "Updated x lables"))


# ==================== 소스순번: 095 ==================== 

p <- ggplot(data=mtcars, aes(x=disp, y=wt))
p <- p + geom_smooth()
p


# ==================== 소스순번: 096 ==================== 

p + coord_cartesian(xlim=c(325, 500), ylim=c(3,6))


# ==================== 소스순번: 097 ==================== 

# 가상의 데이터 생성 (단위: 만원)
incomes <- c(500, 350, 700, 600, 400, 350, 500, 900, 700, 600)
savings <- c(10, 20, 30, 30, 20, 0, 30, 100, 50, 50)
df <- data.frame(incomes, savings)
# 수입대비 저축에 대한 산점도 그리기
p <- ggplot(data=df, aes(x=incomes, y=savings))
p <- p + geom_point(size=5)
p


# ==================== 소스순번: 098 ==================== 

p + coord_fixed(ratio = 1)


# ==================== 소스순번: 099 ==================== 

p <- ggplot(data=diamonds, aes(x=cut, y=price))
p <- p + geom_boxplot()
p + coord_flip()


# ==================== 소스순번: 100 ==================== 

p <- ggplot(data=diamonds, aes(x=carat))
p <- p + geom_histogram(binwidth=0.2)
p + coord_flip()


# ==================== 소스순번: 101 ==================== 

p <- ggplot(data=diamonds, aes(x=carat))
p <- p + geom_histogram(binwidth=0.2)
p + coord_flip() + scale_x_reverse()


# ==================== 소스순번: 102 ==================== 

# 수치지도를 가져오기 위해서 map 패키지를 사용함
require("maps")
world <- map_data("world")
# 세계지도에서 대한민국 지도가 발췌해 옴
korea <- world[grep("Korea$", world$region),]
# ggplot2에서 지도를 표현함
p <- ggplot(korea, aes(x=long, y=lat, group=group))
p <- p + geom_polygon(fill="white", colour="black")
p
p$coordinates


# ==================== 소스순번: 103 ==================== 

# 종횡비를 1로 변경함
p + coord_fixed(ratio = 1)


# ==================== 소스순번: 104 ==================== 

p <- ggplot(korea, aes(x=long, y=lat, group=group))
p <- p + geom_polygon(fill="white", colour="black")
p <- p + coord_map()
p
p$coordinates


# ==================== 소스순번: 105 ==================== 

require("mapdata")
world <- map_data("worldHires")
# mapdata 패키지의 세계지도에서 대한민국 지도가 발췌해 옴
korea <- world[grep("Korea$", world$region),]
# ggplot2에서 지도를 표현함
p <- ggplot(korea, aes(x=long, y=lat, group=group))
p <- p + geom_polygon(fill="white", colour="black")
p + coord_map()


# ==================== 소스순번: 106 ==================== 

p <- ggplot(mtcars, aes(x = factor(cyl)))
p <- p + geom_bar(width = 1, colour = "black", aes(fill=cyl))
p
p$coordinates


# ==================== 소스순번: 107 ==================== 

p <- ggplot(mtcars, aes(x = factor(cyl)))
p <- p + geom_bar(width = 1, colour = "black", aes(fill=cyl))
p <- p + coord_polar()
p
p$coordinates


# ==================== 소스순번: 108 ==================== 

p <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl)))
p <- p + geom_bar(width = 1, colour = "black")
p <- p + coord_polar(theta = "y")
p
p$coordinates


# ==================== 소스순번: 109 ==================== 

p <- ggplot(data=diamonds, aes(x=carat, y=price, colour=factor(cut)))
p <- p + geom_point()
p <- p + scale_x_log10() + scale_y_log10()
p
p$coordinates


# ==================== 소스순번: 110 ==================== 

p <- ggplot(data=diamonds, aes(x=carat, y=price, colour=factor(cut)))
p <- p + geom_point()
p <- p + coord_trans(x = "log10", y = "log10")
p
p$coordinates


# ==================== 소스순번: 111 ==================== 

p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point()
p + facet_grid(. ~ cyl)


# ==================== 소스순번: 112 ==================== 

p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point()
p + facet_grid(cyl ~ .)


# ==================== 소스순번: 113 ==================== 

p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point()
p + facet_grid(cyl ~ am)


# ==================== 소스순번: 114 ==================== 

p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point()
p + facet_grid(gear ~ am + cyl, margins=TRUE)


# ==================== 소스순번: 115 ==================== 

p + facet_null()


# ==================== 소스순번: 116 ==================== 

p <- ggplot(data = diamonds, aes(x=price))
p <- p + geom_histogram(binwidth = 1000, aes(fill=..count..))
p + facet_wrap(~ color, ncol=2)


# ==================== 소스순번: 117 ==================== 

p + facet_wrap(~ color, nrow=2)


# ==================== 소스순번: 118 ==================== 

p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ cyl)
p + theme(strip.text = element_text(size = 20, colour = "blue"))


# ==================== 소스순번: 119 ==================== 

p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ cyl, labeller="label_both")
p + theme(strip.text = element_text(size = 20, colour = "blue"))


# ==================== 소스순번: 120 ==================== 

mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha^2", "beta+2", "gamma[2]"))
p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ cyl2)
p + theme(strip.text = element_text(size = 20, colour = "blue"))


# ==================== 소스순번: 121 ==================== 

p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ cyl2, labeller = label_parsed)
p + theme(strip.text = element_text(size = 20, colour = "blue"))


# ==================== 소스순번: 122 ==================== 

p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ am, labeller = label_bquote(alpha == .(x)))
p + theme(strip.text = element_text(size = 20, colour = "blue"))


# ==================== 소스순번: 123 ==================== 

p <- ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs)))
p + geom_bar()


# ==================== 소스순번: 124 ==================== 

p <- ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs)))
p + geom_bar(position=position_dodge())


# ==================== 소스순번: 125 ==================== 

p <- ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs)))
p + geom_bar()


# ==================== 소스순번: 126 ==================== 

p <- ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs)))
p + geom_bar(position="fill")


# ==================== 소스순번: 127 ==================== 

# 가상 데이터의 생성
set.seed(2)
data.set <- data.frame(
  Time=rep(1:4, each=4),
  Type=rep(letters[1:4], times=4),
  Value=rpois(n=16, lambda=10)
)
p <- ggplot(data=data.set, aes(Time, Value, colour=Type))
p + geom_line()


# ==================== 소스순번: 128 ==================== 

p <- ggplot(data=data.set, aes(Time, Value, colour=Type, ymax=max(Value)))
p + geom_line(position=position_stack())


# ==================== 소스순번: 129 ==================== 

exam <- data.frame(
  pos.x=sample(1:5, size=500, replace=TRUE),
  pos.y=sample(1:5, size=500, replace=TRUE)
)
p <- ggplot(data = exam, aes(pos.x, pos.y))
p + geom_point()


# ==================== 소스순번: 130 ==================== 

p + geom_point(position=position_jitter(w=0.1, h=0.1))


# ==================== 소스순번: 131 ==================== 

ferrari <- mtcars[rownames(mtcars) == "Ferrari Dino",]
# 산점도 그리기
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p <- p + geom_point(data=ferrari, colour="red")
# 문자열로 부연 설명하기
p + annotate("text", x = ferrari$wt, y = ferrari$mpg,
             label=paste0("<-- ", rownames(ferrari)), hjust=-0.1, colour="red")


# ==================== 소스순번: 132 ==================== 

# 이상치의 영역 구하기
wt_bounds <- IQR(mtcars$wt) * c(-1.5, 1.5) + fivenum(mtcars$wt)[c(2, 4)]
mpg_bounds <- IQR(mtcars$mpg) * c(-1.5, 1.5) + fivenum(mtcars$mpg)[c(2, 4)]
# 사각형 영역 표현하기
p + annotate("rect",
             xmin = wt_bounds[1], xmax = wt_bounds[2],
             ymin = mpg_bounds[1], ymax = mpg_bounds[2], alpha = .2)


# ==================== 소스순번: 133 ==================== 

# 분할선 그리기
p + annotate("segment", x=2.5, xend=4, y=15, yend=25, colour="blue")


# ==================== 소스순번: 134 ==================== 

# 점과 선으로 중위수와 이상치가 아닌 영역 표현하기
p + annotate("pointrange", pch=15, cex=1.2,
                  x = median(mtcars$wt), y = median(mtcars$mpg),
                  ymin = mpg_bounds[1], ymax = mpg_bounds[2],
                  colour="red")


# ==================== 소스순번: 135 ==================== 

# gridExtra 패키지의 로드, 없으면 설치 후 로드
if (!require(gridExtra)) {
  install.packages("gridExtra")
  require(gridExtra)
}


# ==================== 소스순번: 136 ==================== 

# 테이블로 출력될 table 클래스 객체 생성하기
top10 <- head(mtcars[order(mtcars$mpg, decreasing=T), ], 10)
table_grob <- tableGrob(top10[, c("mpg", "wt")])
# 플롯 작성하기
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p <- p + expand_limits(xmax=10)
# 테이블 타이틀 출력하기
p <- p + annotate("text", x=8.2, y=31,
                  label="Best mpg Top 10 lists", hjust=0.5, colour="red")
# 테이블 출력하기
p + annotation_custom(grob=table_grob, xmin=6, xmax=Inf, ymin=-Inf, ymax=Inf)


# ==================== 소스순번: 137 ==================== 

# 테이블로 출력될 table 클래스 객체 생성하기
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p <- p + expand_limits(xmax = 8)
p <- p + annotation_custom(grob=roundrectGrob(), xmin=6, xmax=8, ymin=11, ymax=34)
p + annotate("text",
		x=rep(6.2, 10), y=seq(33, 12, length.out=10),
		label=paste0("No.", seq(1, 10), " ", rownames(top10)),
		hjust=rep(0, 10), size=3.5)


# ==================== 소스순번: 138 ==================== 

p <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length))
p <- p + geom_point()
p + annotation_logticks(sides="trbl")


# ==================== 소스순번: 139 ==================== 

library(MASS)
library(scales)


# ==================== 소스순번: 140 ==================== 

p <- ggplot(Animals, aes(x = body, y = brain))
p <- p + geom_point()
p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  labels = trans_format("log10", math_format(10^.x)))
p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  labels = trans_format("log10", math_format(10^.x)))
p + annotation_logticks()


# ==================== 소스순번: 141 ==================== 

library(maps)
usamap <- map_data("state")
seal.sub <- subset(seals, long > -130 & lat < 45 & lat > 40)
ggplot(seal.sub, aes(x = long, y = lat)) +
  annotation_map(usamap, fill = "NA", colour = "grey50") +
  geom_segment(aes(xend = long + delta_long, yend = lat + delta_lat))


# ==================== 소스순번: 142 ==================== 

rainbow.colors <- matrix(hcl(seq(0, 360, length = 10), 80, 70), nrow = 1)
rainbow.colors
qplot(mpg, wt, data = mtcars) +
  annotation_raster(rainbow.colors, -Inf, Inf, -Inf, Inf) +
  geom_point()


# ==================== 소스순번: 143 ==================== 

library(maps)
data(world.cities)
world <- map_data("world")
korea_south <- world.cities[world.cities$country == "Korea South", ]
p <- ggplot(korea_south, aes(long, lat))
p <- p + coord_map()
p <- p + borders("world", "South Korea", fill="white")
p + geom_point(aes(size = pop), colour="blue", alpha=0.8)


# ==================== 소스순번: 144 ==================== 

model <- lm(mpg ~ wt, data = mtcars)


# ==================== 소스순번: 145 ==================== 

p <- ggplot(data=model, aes(x=.fitted, y=.resid))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point()
p + geom_smooth(se=FALSE, method=loess)


# ==================== 소스순번: 146 ==================== 

names(model)
names(fortify(model))
is.data.frame(fortify(model))


# ==================== 소스순번: 147 ==================== 

p <- ggplot(data=model, aes(x=.fitted, y=.resid))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point(colour=factor(cyl))
p + geom_smooth(se=FALSE, method=loess)


# ==================== 소스순번: 148 ==================== 

p <- ggplot(data=fortify(model, mtcars), aes(x=.fitted, y=.resid))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point(aes(colour=factor(cyl)))
p + geom_smooth(se=FALSE, method=loess)


# ==================== 소스순번: 149 ==================== 

is.data.frame(fortify(model, mtcars))
names(fortify(model, mtcars))


# ==================== 소스순번: 150 ==================== 

names(ggplot2:::fortify.lm(model, mtcars))


# ==================== 소스순번: 151 ==================== 

library("multcomp")


# ==================== 소스순번: 152 ==================== 

# 이원배치 분산분석 모델 생성
aov_model <- aov(breaks ~ wool + tension, data = warpbreaks)
# 튜키의 방법에 의한 다중비교
ht <- glht(aov_model, linfct=mcp(tension="Tukey"))
# wool에 대한 다중비교시
# ht <- glht(aov_model, linfct=mcp(wool="Tukey"))
ht_ci <- confint(ht)
# 분산분석 모델의 다중비교 시각화
p <- ggplot(mapping = aes(lhs, estimate))
p <- p + geom_linerange(aes(ymin = lwr, ymax = upr), data=ht_ci)
p <- p + geom_point(aes(size=p), data=summary(ht))
p + scale_size(trans="reverse")


# ==================== 소스순번: 153 ==================== 

library(maps)
ca <- map("county", "ca", plot = FALSE, fill = TRUE)
p <- ggplot(data=ca, aes(x=long, y=lat, group=group))
p + geom_polygon(colour = I("white"))


# ==================== 소스순번: 154 ==================== 

library(sp)
library(maptools)
korea <- readShapePoly("map/KOR_adm/KOR_adm2.shp")
p <- ggplot(korea, aes(x=long, y=lat, group=group))
p + geom_polygon(colour="black", fill="white")


# ==================== 소스순번: 155 ==================== 

p <- ggplot(data=subset(diamonds, color=="J"), aes(x=carat, y=price, colour=clarity))
p <- p + geom_point()
p <- p + ggtitle("Diamond plot (color=J)")
p


# ==================== 소스순번: 156 ==================== 

p + theme(title=element_text(family="Arial", size=14, colour="blue"))


# ==================== 소스순번: 157 ==================== 

p + theme(plot.title=element_text(family="Arial", size=14, colour="blue"))


# ==================== 소스순번: 158 ==================== 

p + theme(title=element_text(family="Arial", size=14),
          plot.title=element_text(colour="blue"))


# ==================== 소스순번: 159 ==================== 

p <- ggplot(data=subset(diamonds, color=="J"), aes(x=carat, y=price, colour=clarity))
p <- p + geom_point()
p <- p + ggtitle("Diamond plot")
p <- p + theme(title=element_text(family="Arial", size=14))
p + theme(plot.title=element_text(colour="blue"))


# ==================== 소스순번: 160 ==================== 

p <- ggplot(mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", aes(fill=cyl), colour="black")
p <- p + ggtitle("Car MPG")
p <- p + xlab("Car models") + ylab("Mile per Gallon")
p <- p + scale_fill_gradient(low="#c3e9df", high="#063a2c")
base.p <- p
base.p


# ==================== 소스순번: 161 ==================== 

base.p + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


# ==================== 소스순번: 162 ==================== 

p <- base.p + ylab("Mile\nper\nGallon")
p <- p + theme(plot.title = element_text(face="bold", size=20))
p <- p + theme(axis.title.x = element_text(face="bold", colour="#333333", size=14))
p <- p + theme(axis.title.y = element_text(angle = 0, face="bold", colour="#333333", size=14))
p2 <- p + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=12))
p2


# ==================== 소스순번: 163 ==================== 

library("grid")
p <- p2 + scale_fill_gradient(low="#c3e9df", high="#063a2c", guide=guide_legend(title="Cylinder"))
p <- p + theme(legend.background = element_rect(linetype="dotted", colour="#333333", fill="#eeeeee"))
p3 <- p + theme(legend.key.size = unit(10, "mm"))
p3


# ==================== 소스순번: 164 ==================== 

p <- p3 + theme(panel.background = element_rect(fill="#eeeeff"))
p <- p + theme(plot.background = element_rect(fill="#eeeeee"))
p


# ==================== 소스순번: 165 ==================== 

p <- p3 + theme(panel.background = element_rect(fill="#eeeeff"))
p <- p + theme(plot.background = element_rect(fill="#eeeeee"))
p + theme_bw()


# ==================== 소스순번: 166 ==================== 

p <- ggplot(mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + theme_bw()
p <- p + geom_bar(stat="identity", aes(fill=cyl), colour="black")
p <- p + ggtitle("Car MPG")
p <- p + xlab("Car models") + ylab("Mile\nper\nGallon")
p <- p + scale_fill_gradient(low="#c3e9df", high="#063a2c", guide=guide_legend(title = "Cylinder"))
p <- p + theme(plot.title = element_text(face="bold", size=20))
p <- p + theme(axis.title.x = element_text(face="bold", colour="#333333", size=14))
p <- p + theme(axis.title.y = element_text(angle = 0, face="bold", colour="#333333", size=14))
p <- p + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=12))
p <- p + theme(legend.background = element_rect(linetype="dotted", colour="#333333", fill="#eeeeee"))
p <- p + theme(legend.key.size = unit(10, "mm"))
p


# ==================== 소스순번: 167 ==================== 

library(grid)
p <- ggplot(mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + theme_gray()
p <- p + geom_bar(stat="identity", aes(fill=cyl), colour="black")
p <- p + ggtitle("Car MPG")
p <- p + xlab("Car models") + ylab("Mile\nper\nGallon")
p <- p + scale_fill_gradient(low="#c3e9df", high="#063a2c", guide=guide_legend(title = "Cylinder"))
p <- p + theme(plot.title = element_text(face="bold", size=20))
p <- p + theme(axis.title.x = element_text(face="bold", colour="#333333", size=14))
p <- p + theme(axis.title.y = element_text(angle = 0, face="bold", colour="#333333", size=14))
p <- p + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=12))
p <- p + theme(legend.background = element_rect(linetype="dotted", colour="#333333", fill="#eeeeee"))
p <- p + theme(legend.key.size = unit(10, "mm"))
p


# ==================== 소스순번: 168 ==================== 

theme_mine <- function (base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
      theme(plot.title = element_text(face="bold", size=20),
            axis.title.x = element_text(face="bold", colour="#333333", size=14),
            axis.title.y = element_text(angle = 0, face="bold", colour="#333333", size=14),
            axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=12),
            legend.background = element_rect(linetype="dotted", colour="#333333", fill="#eeeeee"),
            legend.key.size = unit(10, "mm")
      )
}


# ==================== 소스순번: 169 ==================== 

p <- ggplot(mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", aes(fill=cyl), colour="black")
p <- p + ggtitle("Car MPG")
p <- p + xlab("Car models") + ylab("Mile\nper\nGallon")
p <- p + scale_fill_gradient(low="#c3e9df", high="#063a2c", guide=guide_legend(title = "Cylinder"))
p + theme_mine()


# ==================== 소스순번: 170 ==================== 

# 앞서 만들어 놓았던 p3 ggplot 객체를 시각화 함
p3
# 현재 전역 테마 설정을 조회하여 저장함
my.theme <- theme_get()
# my.theme의 객체가 무엇일까?
is(my.theme)
# my.theme의 객체가 리스트 객체일까?
is.list(my.theme)
# 테마 요소들의 이름 조회
names(my.theme)
# 테마에서 x-축의 틱 라벨의 설정 조회
my.theme$axis.text.x


# ==================== 소스순번: 171 ==================== 

# theme_bw() 테마 적용
old.theme <- theme_set(theme_bw())
# p3 ggplot 객체의 시각화
p3
# old.theme 객체가 theme 클래스 객체인가?
is(old.theme)
# old.theme 테마의 패싯(facets)의 배경색
old.theme$panel.background$fill
# 현재 테마의 패싯(facets)의 배경색
theme_get()$panel.background$fill


# ==================== 소스순번: 172 ==================== 

# theme_mine() 테마 적용
theme_set(theme_mine())
p <- ggplot(data=subset(diamonds, color=="J"),
            aes(x=carat, y=price, colour=clarity))
p <- p + geom_point()
p <- p + ggtitle("Diamond plot (color=J)")
p


# ==================== 소스순번: 173 ==================== 

# my.theme theme 객체를 테마로 적용
theme_set(my.theme)
p <- ggplot(data=subset(diamonds, color=="J"),
            aes(x=carat, y=price, colour=clarity))
p <- p + geom_point()
p <- p + ggtitle("Diamond plot (color=J)")
p


# ==================== 소스순번: 174 ==================== 

theme_set(theme_mine())
p <- ggplot(mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", aes(fill=cyl))
# 현재 테마의 panel.background 요소
theme_get()$panel.background
# 테마의 수정
theme_update(panel.background = element_rect(colour = "blue"))
#theme_update(panel.background = element_rect(colour = "blue", fill="grey90"))
# 수정된 테마의 panel.background 요소
theme_get()$panel.background
p


# ==================== 소스순번: 175 ==================== 

theme_update(axis.text.y=element_text(size=17))
theme_update(panel.background = element_rect(colour = "black"))
d <- data.frame(lt=c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F2F3F", "1F"))
p <- ggplot()
p <- p + scale_x_continuous(name="", limits=c(0,1), breaks=NULL)
p <- p + scale_y_discrete(name="")
p <- p + scale_linetype_identity()
p + geom_segment(data=d, mapping=aes(x=0, xend=1, y=lt, yend=lt, linetype=lt))


# ==================== 소스순번: 176 ==================== 

theme_set(theme_bw())
df1 <- data.frame(x=c(0,2,0), y=c(0,1,2))
df2 <- data.frame(x=c(0,1,2), l=c("round", "butt", "square"))
ggplot() +
  geom_path(data=df1, mapping=aes(x=x, y=y), size=10, lineend="round") +
  geom_path(data=df1, mapping=aes(x=x+1, y=y), size=10, lineend="butt") +
  geom_path(data=df1, mapping=aes(x=x+2, y=y), size=10, lineend="square") +
  geom_path(data=df1, mapping=aes(x=x, y=y), size=1, color="white") +
  geom_path(data=df1, mapping=aes(x=x+1, y=y), size=1, color="white") +
  geom_path(data=df1, mapping=aes(x=x+2, y=y), size=1, color="white") +
  geom_text(data=df2, mapping=aes(x=x+0.15, y=0, label=l), hjust=0, vjust=1.2)


# ==================== 소스순번: 177 ==================== 

thm <- theme_grey()
calc_element('text', thm)


# ==================== 소스순번: 178 ==================== 

thm <- theme_bw()
is.theme(thm)


# ==================== 소스순번: 179 ==================== 

theme_set(theme_gray())
p <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width))
p <- p + geom_point()
p <- p + theme(axis.title.x=element_text(size=rel(2.5)))
p + theme(axis.title.y=element_text(size=rel(2.5)))


# ==================== 소스순번: 180 ==================== 

p <- ggplot(data=iris, mapping=aes(x=Sepal.Length, y=Sepal.Width))
p + geom_point()


# ==================== 소스순번: 181 ==================== 

p <- ggplot(data=iris)
p + geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width))


# ==================== 소스순번: 182 ==================== 

p <- ggplot(data=iris)
p <- p + xlab("Length") + ylab("Width")
p <- p + geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width), colour="blue", pch=19)
p <- p + geom_point(mapping=aes(x=Petal.Length, y=Petal.Width), colour="red", pch=17)
p


# ==================== 소스순번: 183 ==================== 

p <- ggplot(data=iris)
p + geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width, colour=Species))


# ==================== 소스순번: 184 ==================== 

p <- ggplot(data=iris)
p + geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width, colour=Species, size=Petal.Length))


# ==================== 소스순번: 185 ==================== 

# 데이터 프레임 생성
df <- as.data.frame(cbind(x=mtcars$qsec, y=mtcars$mpg, label=rownames(mtcars)))
names(df)
# aes_all() 함수를 이용한 데이터 매핑
p <- ggplot(df, aes_all(c("x", "y", "label")))
p + geom_text()


# ==================== 소스순번: 186 ==================== 

df <- as.data.frame(cbind(x=mtcars$qsec, y=mtcars$mpg, label=rownames(mtcars)))
p <- ggplot(df, aes_all(names(df)))
p + geom_text()


# ==================== 소스순번: 187 ==================== 

df <- with(iris, data.frame(
  x=Sepal.Length,
  y=Sepal.Width,
  colour=Species,
  label=as.character(Species))
)
p <- ggplot(df, aes_auto(df))
p + geom_text()


# ==================== 소스순번: 188 ==================== 

p <- ggplot(mtcars, aes(x=mpg, y=qsec))
p + geom_point()


# ==================== 소스순번: 189 ==================== 

prefix <- "var"
df <- as.data.frame(cbind(mtcars$mpg, mtcars$qsec))
names(df)
names(df) <- paste0(prefix, 1:NCOL(df))
names(df)
p <- ggplot(df, aes_string(x=paste0(prefix, 1), y=paste0(prefix, 2)))
p + geom_point()


# ==================== 소스순번: 190 ==================== 

# 한글 폰트의 지정
par(family="나눔고딕", cex=1.3)
# 막대 그래프 그리기
p <- ggplot(data=mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", colour="gray", fill="blue", alpha=0.75)
p <- p + coord_flip()
p <- p + xlab("자동차 모델")
p <- p + ylab("연비(마일)")
p <- p + ggtitle("자동차 모델별 연비 현황")
p


# ==================== 소스순번: 191 ==================== 

# 막대 그래프 그리기
p <- ggplot(data=mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", colour="gray", fill="blue", alpha=0.75)
p <- p + coord_flip()
p <- p + xlab("자동차 모델")
p <- p + ylab("연비(마일)")
p <- p + ggtitle("자동차 모델별 연비 현황")
# 한글 출력을 위한 테마 설정
p <- p + theme(title=element_text(family="나눔고딕", face="bold", size=18))
p


# ==================== 소스순번: 192 ==================== 

# 막대 그래프 그리기
p <- ggplot(data=mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", colour="gray", fill="blue", alpha=0.75)
p <- p + coord_flip()
p <- p + xlab("자동차 모델")
p <- p + ylab("연비(마일)")
p <- p + ggtitle("자동차 모델별 연비 현황")
# 한글 출력을 위한 테마 설정
p <- p + theme(title=element_text(family="나눔고딕", face="bold", size=18))
p


# ==================== 소스순번: 193 ==================== 

nanumgothic <- windowsFont("나눔고딕")
windowsFonts(nanumgothic=nanumgothic)

p <- ggplot(data=mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", colour="gray", fill="blue", alpha=0.75)
p <- p + coord_flip()
p <- p + xlab("자동차 모델")
p <- p + ylab("연비(마일)")
p <- p + ggtitle("자동차 모델별 연비 현황")
# 한글 출력을 위한 테마 설정
p <- p + theme(title=element_text(family="nanumgothic", face="bold", size=18))
p

