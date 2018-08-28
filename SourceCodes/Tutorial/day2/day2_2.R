# 6.5 Scales 함수군
# 6.5.1 scale_alpha*() 함수

# p435
if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
library(ggplot2)
if(!require(ggplot2movies)) install.packages("ggplot2movies",repos = "http://cran.us.r-project.org")
library(ggplot2movies)

p <- ggplot(data=mtcars, aes(x=mpg, y=cyl, alpha=cyl))
p + geom_point(size=10)

p <- ggplot(data=mtcars, aes(x=mpg, y=cyl, alpha=cyl))
p <- p + geom_point(size=10)
p + scale_alpha(range=c(0.4, 0.8))

# p436
p <- ggplot(data=mtcars, aes(x=mpg, y=cyl, alpha=factor(cyl)))
p <- p + geom_point(size=10)
p + scale_alpha_discrete(range=c(0.4, 0.8))

# 6.5.2 scale_*_brewer() 함수들

# p438
# 연습문제 6.10

library(scales)
show_col(brewer_pal(pal="RdYlBu")(9))
show_col(brewer_pal(pal="BuGn")(9))
# http://colorbrewer2.org

p <- ggplot(data=diamonds, aes(price, carat, colour=clarity))
p <- p + geom_point()
p + scale_colour_brewer(type="seq", palette="RdYlBu")
p + scale_colour_brewer(type="seq", palette=3)


# p439
# 연습문제 6.11
p <- ggplot(data=diamonds, aes(price, carat, colour=clarity))
p <- p + geom_point()
p + scale_colour_brewer(palette="Dark2")

# p441
head(diamonds)
p <- ggplot(data=diamonds, aes(price, fill=clarity))
p <- p + geom_histogram(binwidth=500)
p + scale_fill_brewer(palette="YlGn")

# 6.5.3 sale_*_gradient() 함수들

# p442
dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
p <- ggplot(data=dsub, aes(x, y, colour=z))
p <- p + geom_point()
p + scale_colour_gradient(limits=c(3, 3.6), low="red", high="green")

# p443
range(dsub$z)
boxplot(dsub$z)

# p444
p <- ggplot(data=dsub, aes(x, fill=..count..))
p <- p + geom_histogram(binwidth=0.02)
p + scale_fill_continuous(low="lightblue", high="blue", limits=c(200, 700), na.value="white")

# 6.5.4 scale_*_gradient2() 함수

# p445
# 연습문제 6.12
dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))

p <- ggplot(data=dsub, aes(x, y, colour=diff))
p <- p + geom_point()
p + scale_colour_gradient2(low="red", high="blue", mid="white", midpoint=0.15)

# p446
dsub <- data.frame(x=letters[1:5], y=c(-3, 3, 5, 2, -2))
p <- ggplot(data=dsub, aes(x, y, fill=y))
p <- p + geom_bar(stat="identity")
p + scale_fill_gradient2()

# 6.5.5 scale_*_gradientn() 함수들

# p477
dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
p <- ggplot(data=dsub, aes(x, y, colour=diff))
p <- p + geom_point()
p + scale_colour_gradientn(colours=rainbow(7))

# p448
p <- ggplot(data=dsub, aes(x, y, colour=diff))
p <- p + geom_point()
p + scale_colour_gradientn(colours=rainbow(7), values=seq(0, 1, length=7))
p + scale_colour_gradientn(colours=rainbow(7), values=seq(-0.4, 0.6, length=7))

# 6.5.6 scale_*_grey() 함수들

# p449
new_mtcars <- mtcars
new_mtcars$miss <- factor(sample(c(NA, 1:5), nrow(new_mtcars), rep = TRUE))
new_mtcars

## 결측치 데이터 추출
summary(new_mtcars$miss)
is.na(new_mtcars$miss)
new_mtcars[is.na(new_mtcars$miss),]

which(is.na(new_mtcars$miss))   # which : 순서 index를 return
new_mtcars[which(is.na(new_mtcars$miss)),]

new_mtcars[!complete.cases(new_mtcars),]  # 결측치가 있는 자료 추
new_mtcars[complete.cases(new_mtcars),]  # 결측치가 없는 자료만 생



p <- ggplot(data=new_mtcars, aes(x=mpg, y=wt, colour = miss))
p <- p + geom_point(size=3)
p + scale_colour_grey(start=0.3, end=0.8, na.value = "red")

# 6.5.7 scale_*_hue() 함수들

# p450
new_mtcars <- mtcars
new_mtcars$miss <- factor(sample(c(NA, 1:5), nrow(new_mtcars), rep = TRUE))
p <- ggplot(data=new_mtcars, aes(x=mpg, y=wt, colour = miss))
p <- p + geom_point(size=5)
p + scale_colour_hue()

p <- ggplot(data=new_mtcars, aes(x=mpg, y=wt, colour = miss))
p <- p + geom_point(size=5)
p + scale_colour_hue(h=c(90, 180), l=80, c=50)

# # 6.5.8 scale_*_identify() 함수들
# 
# # p452
# colours <- c("red", "green", "blue", "yellow", "orange")
# sizes <- c(1, 2, 3, 4, 5) + 3
# df <- data.frame(x=1:5, y=1:5)
# p <- ggplot(data=df, aes(x, y, colour = colours, size=sizes))
# p <- p + geom_point()
# p <- p + scale_colour_identity()
# p <- p + scale_size_identity()
# p

# 6.5.9 scale_*_manual() 함수들

# p454
levels(factor(mtcars$cyl)) # cyl 변수의 수준
p <- ggplot(data = mtcars, aes(x = mpg, y = wt, colour = factor(cyl)))
p <- p + geom_point(size = 3)
p + scale_colour_manual(values = c("red", "blue", "green"))

# p455
p <- ggplot(data = mtcars, aes(x=mpg, y=wt, colour=factor(cyl)))
p <- p + geom_point(size=3)
p + scale_colour_manual(values = c("8"="red", "6"="blue", "4"="green"))


# 6.5.10 scale_*_linetype*() 함수들

# p456
library(reshape2)
library(plyr)
ecm <- melt(economics, id = "date")
rescale01 <- function(x) (x - min(x)) / diff(range(x))
ecm <- ddply(ecm, "variable", transform, value = rescale01(value))

p <- ggplot(data=ecm, aes(date, value, group=variable, linetype=variable, colour=variable))
p <- p + geom_line()
p

# 6.5.11 scale_shape*() 함수들

# p457

dsmall <- diamonds[sample(nrow(diamonds), 100), ]
p <- ggplot(data=dsmall, aes(x=carat, y=price, shape=cut))
p <- p + geom_point(size=3)
p + scale_shape_discrete(solid=FALSE)
p + scale_shape_discrete(solid=TRUE)

plot(dsmall$carat, dsmall$price, pch=as.numeric(dsmall$cut))


# 6.5.12 scale_*_size*() 함수들

# p458
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

p <- ggplot(data=dsmall, aes(x=carat, y=price, size=cut))
p <- p + geom_point(alpha=0.3)
p + scale_size_discrete(range = c(0, 10))

###########################################################

# 수준의 이름
levels(dsmall$cut)
# 예상되는 점의 크기
seq(0, 10, length=5)

# 6.5.13 scale_*_continuous() 함수들

# p459
require(ggplot2movies)
p <- ggplot(data=movies, aes(x=rating, y=votes))
p <- p + geom_point()
p <- p + scale_x_continuous(limits=c(2.5, 9))
p + scale_y_continuous(limits=c(0, 10000))

# 6.5.14 scale_*_reverse() 함수들

# p460
p <- ggplot(data=movies, aes(x=rating, y=votes))
p <- p + geom_point()
p <- p + scale_x_reverse()
p + scale_y_reverse()

# 6.5.15 scale_*_log10() 함수들

# p461
p <- ggplot(data=movies, aes(x=rating, y=votes))
p <- p + geom_point()
p <- p + scale_x_log10()
p + scale_y_log10()

# 6.5.16 scale_*_sqrt() 함수들

# p462
p <- ggplot(data=movies, aes(x=rating, y=votes))
p <- p + geom_point()
p <- p + scale_x_sqrt()
p + scale_y_sqrt()

# 6.5.17 scale_*_discrete() 함수들

p <- ggplot(data=subset(diamonds, carat > 1), aes(x=cut, y=clarity, colour=carat))
p <- p + geom_jitter(size=0.3)
p + scale_x_discrete("Cutting", labels=paste("Grade", 1:5))

## 위의 코드와 결과를 비교해 보자
p <- ggplot(data=subset(diamonds, carat > 1), aes(x=cut, y=clarity, colour=carat))
p <- p + geom_point()
p + scale_x_discrete("Cutting", labels=paste("Grade", 1:5))


# p463
# 연습문제 6.13
p <- ggplot(data=subset(diamonds, carat > 1), aes(x=cut, y=clarity, colour=carat))
p <- p + geom_jitter()
p + scale_x_discrete("Cutting", labels=paste("Grade", 1:5), limits=c("Fair", "Good", "Very Good"))

# 6.5.18 scale_*_date(), scale_*_datetime() 함수들

# p464
p <- ggplot(data=economics, aes(x=date, y=psavert))
p + geom_path()

# p465
# 연습문제 6.34
p <- ggplot(data=economics, aes(x=date, y=psavert))
p <- p + geom_path()
p + scale_x_date("21 century", limits=c(as.Date("2000-01-01"), max(economics$date)))

# 6.5.19 guides*() 함수들

# p466
dat <- data.frame(x = 1:5, y = 1:5, p = 1:5, q = factor(1:5), r = factor(1:5))
p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r))
p + geom_point()

# p467
p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r))
p <- p + geom_point()
p <- p + guides(colour = guide_colorbar(), size = guide_legend(), shape = guide_legend())

# 468
p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r))
p <- p + geom_point()
p + guides(colour = guide_legend("title"), size = guide_legend("title"), shape = guide_legend("title"))

# 6.5.20 expand_limits() 함수

########################################

# p470
# 연습문제 6.15
range(mtcars$mpg) # x-축에 매핑된 데이터의 범위
range(mtcars$wt) # y-축에 매핑된 데이터의 범위
p <- ggplot(data = mtcars, aes(x = mpg, y = wt))
p <- p + geom_point()
p + expand_limits(x=0, y=c(-1, 10))

# 6.5.21 xlim(), ylim() 함수

# p471
p <- ggplot(data = mtcars, aes(x = mpg, y = wt))
p <- p + geom_point(size=5)
p <- p + xlim(15, 25)
p <- p + ylim(2, 4.5)
p

# 6.5.22 labs(), ggtitle(), xlab(), ylab() 함수

# p472
p <- ggplot(data = mtcars, aes(x = mpg, y = wt))
p <- p + geom_point()
p <- p + labs(title = "New main title")
p <- p + labs(x = "New x labels")
p + labs(y = "New y labels")

# p473
# 연습문제 6.17
p <- ggplot(data = mtcars, aes(x = mpg, y = wt))
p <- p + geom_point()
p <- p + ggtitle("New main title")
p <- p + xlab("New x labels")
p + ylab("New y labels")


# 6.5.23 update_labels() 함수

p <- qplot(mpg, wt, data = mtcars)
p <- p + xlab("New x labels")
p

update_labels(p, list(x = "Updated x lables"))

# 6.6 Coordinate systems 함수군
# 6.6.1 coord_cartesian() 함수

# p475
p <- ggplot(data=mtcars, aes(x=disp, y=wt))
p <- p + geom_smooth()
p

p + coord_cartesian(xlim=c(325, 500), ylim=c(3,6))

# 6.6.2 coord_fixed() 함수

# p477
# 가상의 데이터 생성 (단위: 만원)
incomes <- c(500, 350, 700, 600, 400, 350, 500, 900, 700, 600)
savings <- c(10, 20, 30, 30, 20, 0, 30, 100, 50, 50)
df <- data.frame(incomes, savings)
# 수입대비 저축에 대한 산점도 그리기
p <- ggplot(data=df, aes(x=incomes, y=savings))
p <- p + geom_point(size=5)
p

p + coord_fixed(ratio = 1)


# 6.6.3 coord_flip() 함수

# p479
p <- ggplot(data=diamonds, aes(x=cut, y=price))
p <- p + geom_boxplot()
p
p + coord_flip()

# p480
p <- ggplot(data=diamonds, aes(x=carat))
p <- p + geom_histogram(binwidth=0.2)
p
p + coord_flip()

p <- ggplot(data=diamonds, aes(x=carat))
p <- p + geom_histogram(binwidth=0.2)
p
p + coord_flip() + scale_x_reverse()

# 6.6.4 coord_map() 함수

# p482
# 수치지도를 가져오기 위해서 map 패키지를 사용함
if(!require("maps")) install.packages("maps",repos = "http://cran.us.r-project.org"); library(maps)

world <- map_data("world")
# 세계지도에서 대한민국 지도가 발췌해 옴
korea <- world[grep("Korea$", world$region),]
# ggplot2에서 지도를 표현함
p <- ggplot(korea, aes(x=long, y=lat, group=group))
p <- p + geom_polygon(fill="white", colour="black")
p
p$coordinates

# p483
# 연습문제 6.18
# 종횡비를 1로 변경함
p + coord_fixed(ratio = 1)

# p484
p <- ggplot(korea, aes(x=long, y=lat, group=group))
p <- p + geom_polygon(fill="white", colour="black")
p <- p + coord_map()
p
p$coordinates

# p485
if (!require(mapdata)) {
  install.packages("mapdata",repos = "http://cran.us.r-project.org")
  require(mapdata)
}
world <- map_data("worldHires")
# mapdata 패키지의 세계지도에서 대한민국 지도가 발췌해 옴
korea <- world[grep("Korea$", world$region),]
# ggplot2에서 지도를 표현함
p <- ggplot(korea, aes(x=long, y=lat, group=group))
p <- p + geom_polygon(fill="white", colour="black")
p + coord_map()

# 6.6.5 coord_polar() 함
# p486
p <- ggplot(mtcars, aes(x = factor(cyl)))
p <- p + geom_bar(width = 1, colour = "black", aes(fill=cyl))
p
p$coordinates

# p487
# 연습문제 6.19
p <- ggplot(mtcars, aes(x = factor(cyl)))
p <- p + geom_bar(width = 1, colour = "black", aes(fill=cyl))
p <- p + coord_polar()
p
p$coordinates

# p487
# 연습문제 6.20
p <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl)))
p <- p + geom_bar(width = 1, colour = "black")
p <- p + coord_polar(theta = "y")
p
p$coordinates


# 6.6.6 coord_trans() 함수

# p489
p <- ggplot(data=diamonds, aes(x=carat, y=price, colour=factor(cut)))
p <- p + geom_point()
p <- p + scale_x_log10() + scale_y_log10()
p
p$coordinates

# p490
p <- ggplot(data=diamonds, aes(x=carat, y=price, colour=factor(cut)))
p <- p + geom_point()
p <- p + coord_trans(x = "log10", y = "log10")
p
p$coordinates