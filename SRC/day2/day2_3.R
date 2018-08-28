if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

# 6.7 Faceting 함수군

# 6.7.1 facet_grid() 함수

# p492
p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point()
p + facet_grid(. ~ cyl)

# p493
p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point()
p + facet_grid(cyl ~ .)

# p495
p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point()
p + facet_grid(cyl ~ am)

# 연습문제 6.21
p <- ggplot(mtcars, aes(mpg, wt))
p <- p + geom_point()
p + facet_grid(gear ~ am + cyl, margins=TRUE)

# 6.7.2 facet_null() 함수

# p496 
# 연습문제 6.22
p + facet_null()

# 6.7.3 facet_wrap() 함수

# p497
# 연습문제 6.23
p <- ggplot(data = diamonds, aes(x=price))
p <- p + geom_histogram(binwidth = 1000, aes(fill=..count..))
p + facet_wrap(~ color)
p + facet_wrap(~ color, ncol=2)

# 6.7.4 label_both(), label_value() 함수

# p499
p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ cyl)
p + theme(strip.text = element_text(size = 20, colour = "blue"))

# p500
# 연습문제 6.25
p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ cyl, labeller="label_both")
p + theme(strip.text = element_text(size = 20, colour = "blue"))

p <- p + facet_grid(. ~ cyl, labeller="label_value")
p + theme(strip.text = element_text(size = 20, colour = "blue"))


# 6.7.5 label_parsed() 함수

# p501
mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha^2", "beta+2", "gamma[2]"))

p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ cyl2)
p + theme(strip.text = element_text(size = 20, colour = "blue"))

# p503
p <- ggplot(data = mtcars, aes(wt, mpg))
p <- p + geom_point()
p <- p + facet_grid(. ~ cyl2, labeller = label_parsed)
p + theme(strip.text = element_text(size = 20, colour = "blue"))


# 6.8 Position adjustments 함수군
# 6.8.1 Position_dodge() 함수

# p505
p <- ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs)))
p + geom_bar()

# p506
p <- ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs)))
p + geom_bar(position=position_dodge())

# 6.8.2 position_fill() 함수

# p507
p <- ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs)))
p + geom_bar()

# 연습문제 6.26
p <- ggplot(mtcars, aes(x=factor(cyl), fill=factor(vs)))
p + geom_bar(position="fill")


# 6.8.3 position_stack() 함수

# p509
set.seed(2)
data.set <- data.frame(
  Time=rep(1:4, each=4),
  Type=rep(letters[1:4], times=4),
  Value=rpois(n=16, lambda=10)
)
head(data.set)

p <- ggplot(data=data.set, aes(Time, Value, colour=Type))
p + geom_line()

# 연습문제 6.27
p <- ggplot(data=data.set, aes(Time, Value, colour=Type, ymax=max(Value)))
p + geom_line(position=position_stack())

# 6.8.4 position_jitter() 함수

# p511
set.seed(5)
exam <- data.frame(
  pos.x=sample(1:5, size=500, replace=TRUE),
  pos.y=sample(1:5, size=500, replace=TRUE)
)
head(exam)
p <- ggplot(data = exam, aes(pos.x, pos.y))
p + geom_point()

# 연습문제 2.28
p + geom_point(position=position_jitter(w=0.1, h=0.1))
p + geom_point(position=position_jitter(w=0.4, h=0.4))

# 6.9 Annotation 함수군
# 6.9.1 annotate() 함수

# p513
ferrari <- mtcars[rownames(mtcars) == "Ferrari Dino",]
# 산점도 그리기
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p <- p + geom_point(data=ferrari, colour="red")
# 문자열로 부연 설명하기
p + annotate("text", x = ferrari$wt, y = ferrari$mpg,
             label=paste0("<-- ", rownames(ferrari)), hjust=-0.1, colour="red")

# p514
# 이상치의 영역 구하기
wt_bounds <- IQR(mtcars$wt) * c(-1.5, 1.5) + fivenum(mtcars$wt)[c(2, 4)]
mpg_bounds <- IQR(mtcars$mpg) * c(-1.5, 1.5) + fivenum(mtcars$mpg)[c(2, 4)]
# 사각형 역역 표현하기
p + annotate("rect",
             xmin = wt_bounds[1], xmax = wt_bounds[2],
             ymin = mpg_bounds[1], ymax = mpg_bounds[2], alpha = .2)

# p515
# 분할선 그리기
p + annotate("segment", x=2.5, xend=4, y=15, yend=25, colour="blue")


# 점과 선으로 중위수와 이상치가 아닌 영역 표현하기
p + annotate("pointrange", pch=15, cex=1.2,
             x = median(mtcars$wt), y = median(mtcars$mpg),
             ymin = mpg_bounds[1], ymax = mpg_bounds[2],
             colour="red")


# 6.9.2 annotation_custom() 함수

# p516
# gridExtra 패키지의 로드, 없으면 설치 후 로드
if (!require(gridExtra)) {
  install.packages("gridExtra")
  require(gridExtra)
}

# p517
# 테이블로 출력될 table 클래스 객체 생성하기
top10 <- head(mtcars[order(mtcars$mpg, decreasing=T), ], 10)
top10
table_grob <- tableGrob(top10[, c("mpg", "wt")])
head(table_grob)
# 플롯 작성하기
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p <- p + expand_limits(xmax=10)
# 테이블 타이틀 출력하기
p <- p + annotate("text", x=8.2, y=31,
                  label="Best mpg Top 10 lists", hjust=0.5, colour="red")
# 테이블 출력하기
p + annotation_custom(grob=table_grob, xmin=6, xmax=Inf, ymin=-Inf, ymax=Inf)

library(grid)
# p518
# 테이블로 출력될 table 클래스 객체 생성하기
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p <- p + expand_limits(xmax = 8)
p <- p + annotation_custom(grob=roundrectGrob(), xmin=6, xmax=8, ymin=11, ymax=34)
p + annotate("text",
             x=rep(6.2, 10), y=seq(33, 12, length.out=10),
             label=paste0("No.", seq(1, 10), " ", rownames(top10)),
             hjust=rep(0, 10), size=3.5)


# 6.9.3 annotation_logticks() 함수 

# p519
p <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length))
p <- p + geom_point()
p + annotation_logticks(sides="trbl")

# p520
library(MASS)
library(scales)

p <- ggplot(Animals, aes(x = body, y = brain))
p <- p + geom_point()
p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x)))
p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x)))
p + annotation_logticks()


# 6.9.5 annotation_raster() 함수

# p522
rainbow.colors <- matrix(hcl(seq(0, 360, length = 10), 80, 70), nrow = 1)
rainbow.colors
qplot(mpg, wt, data = mtcars) +
  annotation_raster(rainbow.colors, -Inf, Inf, -Inf, Inf) +
  geom_point()

# 6.9.6 borders 함수

# p524
library(maps)
data(world.cities)
world <- map_data("world")
korea_south <- world.cities[world.cities$country == "Korea South", ]
head(korea_south)
p <- ggplot(korea_south, aes(long, lat))
p <- p + coord_map()
p <- p + borders("world", "South Korea", fill="white")
p + geom_point(aes(size = pop), colour="blue", alpha=0.8)


# 6.10 Fortify 함수군
# 6.10.1 frtity(),fortify.lm()함수

# p526
model <- lm(mpg ~ wt, data = mtcars)

p <- ggplot(data=model, aes(x=.fitted, y=.resid))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point()
p + geom_smooth(se=FALSE, method=loess)

# p527
names(model)
names(fortify(model))
is.data.frame(fortify(model))

p <- ggplot(data=model, aes(x=.fitted, y=.resid))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point(colour=factor(mtcars$cyl))
p + geom_smooth(se=FALSE, method=loess)

p <- ggplot(data=fortify(model, mtcars), aes(x=.fitted, y=.resid))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point(aes(colour=factor(cyl)))
p + geom_smooth(se=FALSE, method=loess)


# p528
is.data.frame(fortify(model, mtcars))
names(fortify(model, mtcars))

names(ggplot2:::fortify.lm(model, mtcars))


# 6.10.2 fortity-multcomp() 함수

# p529
if (!require(multcomp)) install.packages('multcomp')
library("multcomp")

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


# 6.10.3 fortify.map() 함수

# p530
library(maps)
ca <- map("county", "ca", plot = FALSE, fill = TRUE)
p <- ggplot(data=ca, aes(x=long, y=lat, group=group))
p + geom_polygon(colour = I("white"))


# 6.10.4 fortify.sp() 함수
#지도 데이터 다운로드 : http://www.gadm.org

# p532
# library(sp)
# library(maptools)
# korea <- readShapePoly("map/KOR_adm/KOR_adm2.shp")
# p <- ggplot(korea, aes(x=long, y=lat, group=group))
# p + geom_polygon(colour="black", fill="white")


# 6.11 Themes 함수군

# 6.11.1 theme() 함수

# p536
p <- ggplot(data=subset(diamonds, color=="J"), aes(x=carat, y=price, colour=clarity))
p <- p + geom_point()
p <- p + ggtitle("Diamond plot (color=J)")
p

p + theme(title=element_text(family="Arial", size=14, colour="blue"))

# p538
p + theme(plot.title=element_text(family="Arial", size=14, colour="blue"))

p + theme(title=element_text(family="Arial", size=14),
          plot.title=element_text(colour="blue"))

# p540
p <- ggplot(data=subset(diamonds, color=="J"), aes(x=carat, y=price, colour=clarity))
p <- p + geom_point()
p <- p + ggtitle("Diamond plot")
p <- p + theme(title=element_text(family="Arial", size=14))
p + theme(plot.title=element_text(colour="blue"))

p <- ggplot(mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", aes(fill=cyl), colour="black")
p <- p + ggtitle("Car MPG")
p <- p + xlab("Car models") + ylab("Mile per Gallon")
p <- p + scale_fill_gradient(low="#c3e9df", high="#063a2c")
base.p <- p
base.p

# p541
base.p + theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

# p542
p <- base.p + ylab("Mile\nper\nGallon")
p <- p + theme(plot.title = element_text(face="bold", size=20))
p <- p + theme(axis.title.x = element_text(face="bold", colour="#333333", size=14))
p <- p + theme(axis.title.y = element_text(angle = 0, face="bold", colour="#333333", size=14))
p2 <- p + theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1, size=12))
p2

# p543
library(grid)
p <- p2 + scale_fill_gradient(low="#c3e9df", high="#063a2c", guide=guide_legend(title="Cylinder"))
p <- p + theme(legend.background = element_rect(linetype="dotted", colour="#333333", fill="#eeeeee"))
p3 <- p + theme(legend.key.size = unit(10, "mm"))
p3

# 6.11.2 theme_*() 함수들

# 6.11.2.1 theme_bw() 함수

# p544
p <- p3 + theme(panel.background = element_rect(fill="#eeeeff"))
p <- p + theme(plot.background = element_rect(fill="#eeeeee"))
p

# p545
p <- p3 + theme(panel.background = element_rect(fill="#eeeeff"))
p <- p + theme(plot.background = element_rect(fill="#eeeeee"))
p + theme_bw()


# 6.11.2.3 테마 함수 정의하기

# p548
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

# 연습문제 6.30
p <- ggplot(mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", aes(fill=cyl), colour="black")
p <- p + ggtitle("Car MPG")
p <- p + xlab("Car models") + ylab("Mile\nper\nGallon")
p <- p + scale_fill_gradient(low="#c3e9df", high="#063a2c", guide=guide_legend(title = "Cylinder"))
p + theme_mine()


# 6.11.3 테마 조작 함수

#6.11.3.1 theme_get() 함수

# p550
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


# 6.11.3.2 theme_set() 함수

# p551
# 연습문제 6.31
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

# p552
# theme_mine() 테마 적용
theme_set(theme_mine())
p <- ggplot(data=subset(diamonds, color=="J"),
            aes(x=carat, y=price, colour=clarity))
p <- p + geom_point()
p <- p + ggtitle("Diamond plot (color=J)")
p

# p553
# 연습문제 6.32
# my.theme theme 객체를 테마로 적용
theme_set(my.theme)
p <- ggplot(data=subset(diamonds, color=="J"),
            aes(x=carat, y=price, colour=clarity))
p <- p + geom_point()
p <- p + ggtitle("Diamond plot (color=J)")
p


# 6.11.3.3 theme_update() 함수

# p554
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

# 6.11.4 element_*()함수

# 6.11.4.2 element_line() 함수

# p557
theme_update(axis.text.y=element_text(size=17))
theme_update(panel.background = element_rect(colour = "black"))
d <- data.frame(lt=c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F2F3F", "1F"))
p <- ggplot()
p <- p + scale_x_continuous(name="", limits=c(0,1), breaks=NULL)
p <- p + scale_y_discrete(name="")
p <- p + scale_linetype_identity()
p + geom_segment(data=d, mapping=aes(x=0, xend=1, y=lt, yend=lt, linetype=lt))

# p558
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

# 6.11.4.5 calc_element() 함수

# p559
thm <- theme_grey()
calc_element('text', thm)

# 6.11.5 기타 theme 함수

# p560
thm <- theme_bw()
is.theme(thm)

theme_set(theme_gray())
p <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width))
p <- p + geom_point()
p <- p + theme(axis.title.x=element_text(size=rel(2.5)))
p + theme(axis.title.y=element_text(size=rel(2.5)))

# 6.12 Aesthetics 함수군

# 6.12.1 aes() 함수

# p562
p <- ggplot(data=iris, mapping=aes(x=Sepal.Length, y=Sepal.Width))
p + geom_point()

# p563
p <- ggplot(data=iris)
p + geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width))

p <- ggplot(data=iris)
p <- p + xlab("Length") + ylab("Width")
p <- p + geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width), colour="blue", pch=19)
p <- p + geom_point(mapping=aes(x=Petal.Length, y=Petal.Width), colour="red", pch=17)
p

# p564
p <- ggplot(data=iris)
p + geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width, colour=Species))

# p565
p <- ggplot(data=iris)
p + geom_point(mapping=aes(x=Sepal.Length, y=Sepal.Width, colour=Species, size=Petal.Length))

# 6.12.2 aes_all() 함수

# p566
# 데이터 프레임 생성
df <- as.data.frame(cbind(x=mtcars$qsec, y=mtcars$mpg, label=rownames(mtcars)))
names(df)
# aes_all() 함수를 이용한 데이터 매핑
p <- ggplot(df, aes_all(c("x", "y", "label")))
p + geom_text()

df <- as.data.frame(cbind(x=mtcars$qsec, y=mtcars$mpg, label=rownames(mtcars)))
p <- ggplot(df, aes_all(names(df)))
p + geom_text()

# 6.12.3 aes_auto() 함수

# p567
df <- with(iris, data.frame(
  x=Sepal.Length,
  y=Sepal.Width,
  colour=Species,
  label=as.character(Species))
)
p <- ggplot(df, aes_auto(df))
p + geom_text()

# 6.12.4 aes_string() 함수

# p568
p <- ggplot(mtcars, aes(x=mpg, y=qsec))
p + geom_point()

# p569
prefix <- "var"
df <- as.data.frame(cbind(mtcars$mpg, mtcars$qsec))
names(df)
names(df) <- paste0(prefix, 1:NCOL(df))
names(df)
p <- ggplot(df, aes_string(x=paste0(prefix, 1), y=paste0(prefix, 2)))
p + geom_point()

# 6.14 ggplot2에서 한글 사용하기

# 6.14.1 Mac/Linux

# p571
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

# p572
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

# 6.14.2 windows

# p573
# 막대 그래프 그리기
p <- ggplot(data=mtcars, aes(x=rownames(mtcars), y=mpg))
p <- p + geom_bar(stat="identity", colour="gray", fill="blue", alpha=0.75)
p <- p + coord_flip()
p <- p + xlab("자동차 모델")
p <- p + ylab("연비(마일)")
p <- p + ggtitle("자동차 모델별 연비 현황")
p
# 한글 출력을 위한 테마 설정

p <- p + theme(title=element_text(family="나눔고딕", face="bold", size=18))
p

# p574
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