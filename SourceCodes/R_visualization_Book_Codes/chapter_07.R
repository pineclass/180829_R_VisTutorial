# -----------------------------------
# R visualization - 소스코드
#   출판사: 도서출판 인사이트
#   저자: 유충현, 홍성학
#   챕터: 7장
#   파일명: chapter_07.R
# -----------------------------------


# ==================== 소스순번: 001 ==================== 

cor(USArrests)


# ==================== 소스순번: 002 ==================== 

library(corrplot)
cor.usa <- cor(USArrests)
par(mfrow=c(2, 4))
corrplot(cor.usa, tl.pos="n", cl.pos="n", title="circle", mar=c(4, 0, 4, 0))
corrplot(cor.usa, method = "square", tl.pos="n", cl.pos="n", title="square", mar=c(4, 0, 4, 0))
corrplot(cor.usa, method = "ellipse", tl.pos="n", cl.pos="n", title="ellipse", mar=c(4, 0, 4, 0))
corrplot(cor.usa, method = "number", tl.pos="n", cl.pos="n", title="number", mar=c(4, 0, 4, 0))
corrplot(cor.usa, method = "shade", addshade="positive", tl.pos="n", cl.pos="n", title="shade",
           mar=c(4, 0, 4, 0))
corrplot(cor.usa, method = "color", tl.pos="n", cl.pos="n", title="color", mar=c(4, 0, 4, 0))
corrplot(cor.usa, method = "pie", tl.pos="n", cl.pos="n", title="pie", mar=c(4, 0, 4, 0))
par(mfrow=c(1, 1))


# ==================== 소스순번: 003 ==================== 

par(mfrow=c(2, 4))
corrplot(cor.usa, type="upper", title="type=\"upper\"",
           mar=c(4, 0, 4, 1))
corrplot(cor.usa, type="lower", title="type=\"lower\"",
           mar=c(4, 0, 4, 1))
corrplot(cor.usa, tl.pos="d", cl.pos="b", title="tl.pos=\"d\", cl.pos=\"b\"",
           mar=c(4, 0, 4, 1))
corrplot(cor.usa, tl.srt=45, tl.offset=2, title="tl.srt=45, tl.offset=2",
           mar=c(4, 0, 4, 1))
corrplot(cor.usa, cl.length=5, , title="cl.length=5", mar=c(4, 0, 4, 1))
corrplot(cor.usa, tl.col="blue", tl.cex=1.5,
           title="tl.col=\"blue\", tl.cex=1.5", mar=c(4, 0, 4, 1))
corrplot(cor.usa, cl.ratio=0.5, title="cl.ratio=0.5", mar=c(4, 0, 4, 1))
corrplot(cor.usa, cl.ratio=0.5, cl.align.text="l",
           title="cl.ratio=0.5, cl.align.text=\"l\"", mar=c(4, 0, 4, 1))
par(mfrow=c(1, 1))


# ==================== 소스순번: 004 ==================== 

M <- cor(mtcars)
par(mfrow=c(2, 4))
corrplot(M, order = "original", title="order = \"original\"",
           mar=c(4, 0, 4, 1))
corrplot(M, order = "AOE", title="order = \"AOE\"",
           mar=c(4, 0, 4, 1))
corrplot(M, order = "hclust", title="order = \"hclust\"",
           mar=c(4, 0, 4, 1))
corrplot(M, order = "FPC", title="order = \"FPC\"",
           mar=c(4, 0, 4, 1))
corrplot(M, order = "alphabet", title="order = \"alphabet\"",
           mar=c(4, 0, 4, 1))
corrplot(M, order = "hclust", addrect = 2,
           title="order = \"hclust\", addrect = 2", mar=c(4, 0, 4, 1))
corrplot(M, order = "hclust", addrect = 3,
           title="order = \"hclust\", addrect = 3", mar=c(4, 0, 4, 1))
par(mfrow=c(1, 1))


# ==================== 소스순번: 005 ==================== 

cor.mtest <- function(mat, conf.level = 0.95) {
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
      diag(p.mat) <- 0
      diag(lowCI.mat) <- diag(uppCI.mat) <- 1
      for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
              tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
              p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
              lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
              uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
          }
      }
      return(list(p.mat, lowCI.mat, uppCI.mat))
  }

res1 <- cor.mtest(mtcars, 0.95)
res2 <- cor.mtest(mtcars, 0.99)
par(mfrow=c(2, 4))
corrplot(M, p.mat = res1[[1]], sig.level=0.2, title="sig.level=0.2",
           mar=c(4, 0, 4, 1))
corrplot(M, p.mat = res1[[1]], sig.level=0.05, pch.cex=2,
           title="sig.level=0.05, pch.cex=2", mar=c(4, 0, 4, 1))
corrplot(M, p.mat = res1[[1]], sig.level=0.01, pch.col="red",
           title="sig.level=0.01, pch.col=\"red\"", mar=c(4, 0, 4, 1))
corrplot(M, p.mat = res1[[1]], insig="blank", title="insig=\"blank\"",
           mar=c(4, 0, 4, 1))
corrplot(M, p.mat = res1[[1]], insig="p-value", title="insig=\"p-value\"",
           mar=c(4, 0, 4, 1))
corrplot(M, p.mat = res1[[1]], insig="p-value", sig.level=-1,
           title="insig=\"p-value\", sig.level=-1", mar=c(4, 0, 4, 1))
corrplot(M, p.mat = res1[[1]], order="hclust", insig="pch",
           title="order=\"hclust\", insig=\"pch\"", mar=c(4, 0, 4, 1))
par(mfrow=c(1, 1))


# ==================== 소스순번: 006 ==================== 

corrplot(M, p.mat = res1[[1]], method="ellipse", type="upper", order="AOE",
           tl.offset=0.5, sig.level=0.05, tl.pos="d", tl.cex=1.3, pch.col="red",
           cl.align.text = "r", cl.cex=1, title="Motor Trend Car Road Tests",
           mar=c(1, 0, 2, 0))
corrplot(M, add=TRUE, type="lower", method="number", order="AOE",
           diag=FALSE, tl.pos="n", cl.pos="n")


# ==================== 소스순번: 007 ==================== 

hc <- hclust(dist(mtcars))
dend <- as.dendrogram(hc)
plot(dend)


# ==================== 소스순번: 008 ==================== 

x <- as.matrix(mtcars)
heatmap(x)


# ==================== 소스순번: 009 ==================== 

heatmap(x, scale="column")


# ==================== 소스순번: 010 ==================== 

i <- which(names(USArrests)=="UrbanPop")
x  <- as.matrix(USArrests[, -i])
result <- heatmap(x, scale="column", Colv=NA, cexCol=1,
                    main="Violent Crime Rates by US State (1973)")
row.names(USArrests)[result$rowInd[1:10]]
row.names(USArrests)[result$rowInd[35:50]]


# ==================== 소스순번: 011 ==================== 

getStock(startDate="2014-01-01", endDate="2014-01-05")
getStock("MSFT", "2014-05-01", "2014-05-10")


# ==================== 소스순번: 012 ==================== 

if (!require(quantmod)) {
      install.packages("quantmod")
      require(quantmod)
  }

getSymbols("AAPL", src="yahoo")
getSymbols("MSFT", src="yahoo")
AAPL['2014-01-01::2014-01-05']
MSFT['2014-05-01::2014-05-10']
# xts to data.frame
apple <- data.frame(date=index(AAPL), AAPL)
row.names(apple) <- NULL
head(apple)


# ==================== 소스순번: 013 ==================== 

apple <- getStock(startDate="2010-01-01", endDate="2014-05-31")

apple$year <- as.numeric(as.POSIXlt(apple$Date)$year+1900)
apple$month <- as.numeric(as.POSIXlt(apple$Date)$mon+1)

apple$monthf <-
      factor(apple$month, levels=as.character(1:12),
             labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             ordered=TRUE)

apple$weekday <- as.POSIXlt(apple$Date)$wday
apple$weekdayf <-
      factor(apple$weekday, levels=rev(1:7),
             labels=rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
             ordered=TRUE)

apple$yearmonth<-as.yearmon(apple$Date)
apple$yearmonthf<-factor(apple$yearmonth)

apple$week <- as.numeric(format(as.Date(apple$Date),"%W"))

library(plyr)
apple <- ddply(apple,.(yearmonthf), transform, monthweek=1+week-min(week))

library(ggplot2)
ch <- ggplot(apple, aes(monthweek, weekdayf, fill = Adj.Close)) +
               geom_tile(colour = "white") + facet_grid(year~monthf) +
               scale_fill_gradient(low="red", high="yellow") +
               labs(title = "Time-Series Calendar Heatmap of Apple Stock") +
               xlab("Week of Month") + ylab("")
ch


# ==================== 소스순번: 014 ==================== 

source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
args(calendarHeat)
function (dates, values, ncolors = 99, color = "r2g", varname = "Values",
    date.form = "%Y-%m-%d", ...)
calendarHeat(apple$Date, apple$Adj.Close,
               varname="AAPL Adjusted Close")


# ==================== 소스순번: 015 ==================== 

calendarHeat(apple$Date, apple$Volume, color="r2b", varname="AAPL Volume")


# ==================== 소스순번: 016 ==================== 

if (!require(tabplot)) {
      install.packages("tabplot")
      require(tabplot)
  }

require(ggplot2)
names(diamonds)
dim(diamonds)
tableplot(diamonds)


# ==================== 소스순번: 017 ==================== 

tableplot(diamonds, select=c(carat, price, cut, color, clarity), subset=price < 2000
            & cut %in% c("Fair", "Good"), sortCol=price, decreasing=FALSE,
            title=paste("subset=price < 2000 & cut %in% c(\"Fair\", \"Good\"),",
                        "sortCol=price, decreasing=FALSE"))


# ==================== 소스순번: 018 ==================== 

tableplot(diamonds, sortCol=price, nBins=50, from=0, to=10, sample=TRUE,
            sampleBinSize=100, title=paste("sortCol=price, nBins=50, from=0,",
                                           "to=10, sample=TRUE, sampleBinSize=100"))


# ==================== 소스순번: 019 ==================== 

load(url('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.sav'))
dim(titanic3)
names(titanic3)
titanic <-
    titanic3[, c("pclass", "survived", "sex", "age", "sibsp", "parch", "fare", "embarked")]
titanic$survived <- factor(ifelse(titanic$survived, "Yes", "No"))
tableplot(titanic)


# ==================== 소스순번: 020 ==================== 

if (!require(DescTools)) {
      install.packages("DescTools")
      require(DescTools)
  }
# 데이터 조작
states <- state.abb
area <- state.area
names(area) <- states
order.area <- sort(area, decreasing=TRUE)
# 트리맵 시각화
PlotTreemap(x=order.area, labels=names(order.area))


# ==================== 소스순번: 021 ==================== 

grp <- state.region
z <- Sort(data.frame(area=area, grp=grp, states=states), c("grp","area"),
            decreasing=c(FALSE,TRUE, TRUE))
z$col <-
    SetAlpha(c("steelblue", "green", "yellow", "orangered")[z$grp],
             unlist(lapply(split(z$area, z$grp),
                           function(...) LinScale(..., newlow=0.1, newhigh=0.6))))
PlotTreemap(x=z$area, grp=z$grp, labels=z$states, col=z$col,
              text.col.grp="gray50", cex.grp=1.8)


# ==================== 소스순번: 022 ==================== 

pop <- as.vector(USArrests$UrbanPop)
z <- Sort(data.frame(area=area, grp=grp, states=states, pop=pop), c("grp","pop"),
            decreasing=c(FALSE,TRUE, TRUE))
z$col <-
    SetAlpha(c("steelblue", "green", "yellow", "orangered")[z$grp],
             unlist(lapply(split(z$pop, z$grp),
                           function(...) LinScale(..., newlow=0.1, newhigh=0.6))))
PlotTreemap(x=z$area, grp=z$grp, labels=z$states, col=z$col,
              text.col.grp="gray50", cex.grp=1.8)


# ==================== 소스순번: 023 ==================== 

if (!require(maps)) {
      install.packages("maps")
      require(maps)
  }

# maps 패키지의 데이터가 있는 디렉터리 이름 가져오기 (Mac의 경우)
pos <- grep("maps", searchpaths(), value=TRUE)
pos <- paste(pos, "data", sep="/")
pos
# 데이터 파일 이름 중에서 postfix가 'MapEnv'인 맵 데이터베이스 이름 가져오기
map.data <- grep("MapEnv", list.files(pos), value=TRUE)
map.data <- sapply(map.data, function(x) strsplit(x, "MapEnv")[[1]][1])
names(map.data) <- NULL
map.data


# ==================== 소스순번: 024 ==================== 

library(maps)
op <- par(no.readonly=TRUE)
par(mfrow=c(2, 2), mar=c(0, 0, 1, 0))
map("italy")
title("databse=\"italy\"")
map("italy", fill=TRUE, col=2:4)
title("fill=T, col=2:4")
map("italy", resolution=5)
title("resolution=5")
map("italy")
map("italy", "a", exact=FALSE, fill=TRUE, col="red", add=TRUE, lwd=3)
title("exact=F, add=T, lwd=5")
par(op)


# ==================== 소스순번: 025 ==================== 

map.text("county", "new jersey")


# ==================== 소스순번: 026 ==================== 

op <- par(no.readonly=T)
par(mfrow=c(2, 2), mar=c(0, 0, 1, 0))
map("world", "China")
map.cities(country = "China", capitals = 1)
title("capitals = 1")
map("world", "China")
map.cities(country = "China", capitals = 2)
title("capitals = 2")
map("world", "China")
map.cities(country = "China", capitals = 3)
title("capitals = 3")
map("world", "China")
map.cities(country = "China", capitals = 3, minpop = 3500000, maxpop = 5000000)
title("capitals=3, minpop=3500000, maxpop=5000000")
par(op)


# ==================== 소스순번: 027 ==================== 

map("world", "China")
map.scale()
map.axes()


# ==================== 소스순번: 028 ==================== 

library(maps)
# 본토에서 떨어진 Alaska, Hawaii 데이터 제외
sub.usa <- subset(USArrests,!rownames(USArrests) %in% c("Alaska", "Hawaii"))
# 주이름, 폭행범 수를 갖는 데이터 프레임 생성
usa.data <- data.frame(states = rownames(sub.usa), Assault = sub.usa$Assault)
# 범례 데이터 생성
col.level <- cut(sub.usa[, 2], c(0, 100, 150, 200, 250, 300, 350))
legends <- levels(col.level)
# 주이름, 폭행범 수, 색상을 갖는 데이터 프레임 생성
levels(col.level) <- sort(heat.colors(6), decreasing = TRUE)
usa.data <- data.frame(usa.data, col.level = col.level)
# Map 데이터 시각화
map('state', region = usa.data$states, fill = TRUE, col = as.character(usa.data$col.level))
title("USA Assault map")
legend(-76, 35, legends, fill = sort(heat.colors(6), decreasing = TRUE), cex = 0.7)


# ==================== 소스순번: 029 ==================== 

library(mapproj)
data(unemp)
data(county.fips)

# define color buckets
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")

cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]

# draw map
map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.7, projection="polyconic")
title("unemployment by county, 2009")
legend("bottomright", leg.txt, fill = colors, bty = 'n')


# ==================== 소스순번: 030 ==================== 

library(mapdata)
par(mfrow = c(1, 2))
map(database = 'world', region = c('South Korea', 'North Korea'))
title("Korea map in maps packages")
map(database = 'worldHires', region = c('South Korea', 'North Korea'))
title("Korea map in mapdata packages")
par(mfrow = c(1, 1))


# ==================== 소스순번: 031 ==================== 

map('worldHires', region=c('South Korea', 'North Korea', 'Japan', 'China'))
map('worldHires', region=c('South Korea'), col = 'blue', add = TRUE, fill = TRUE)
map('worldHires', region=c('North Korea'), col = 'red', add = TRUE, fill = TRUE)
map('worldHires', region=c('Japan'), col = 'black',add = TRUE, fill = TRUE)
map('worldHires', region=c('China'), col = 'yellow',add = TRUE, fill = TRUE)


# ==================== 소스순번: 032 ==================== 

library(mapproj)
library(ggmap)

geocode("독도")
m <- map("worldHires", plot = FALSE)
map('worldHires', proj = 'azequalarea', orient = c(37.24223, 131.8643, 0))
map.grid(m, col = 2)
points(mapproject(list(y = 37.24223, x = 131.8643)), col = "blue", pch = "x", cex = 2)
title("지구본에서의 독도")


# ==================== 소스순번: 033 ==================== 

library(maptools)
library(foreign)
library(rgdal)


# ==================== 소스순번: 034 ==================== 

install.packages('rgdal',repos="http://www.stats.ox.ac.uk/pub/RWin")


# ==================== 소스순번: 035 ==================== 

data.path <- "./datas"
census.file <- "101_DT_1IN1002_F_2010.csv"
# 행정구역 경계 파일
level1.shp.file <- "2010_1_0/temp"
level2.shp.file <- "2010_2_0/temp"
level3.shp.file <- "2010_3_0/temp"
# 행정구역별 통계 데이터
level1.dbf.file <- "2010_1_0/temp.dbf"
level2.dbf.file <- "2010_2_0/temp.dbf"
level3.dbf.file <- "2010_3_0/temp.dbf"


# ==================== 소스순번: 036 ==================== 

# 2010년도 인구 센서스 데이터 파일 경로와 이름
census.file <- paste(data.path, census.file, sep="/")
# Mac OS나 Linux의 경우
census.data <- read.csv(census.file, header=T, skip=2, fileEncoding="cp949")
# MS-Windows의 경우
#census.data <- read.csv(census.file, header=T, skip=2)

# 변수명 정리하기
names(census.data)
names(census.data) <- gsub("[[:punct:]]+", "_", gsub("[[:punct:]]$", "",
                             names(census.data)))
names(census.data)

# 데이터에서 ' 떼어내기
head(census.data$C행정구역별_읍면동)
census.data$C행정구역별_읍면동 <- sub("^\\'", "", census.data$C행정구역별_읍면동)
head(census.data$C행정구역별_읍면동)


# ==================== 소스순번: 037 ==================== 

# 2010년도 센서스용 행정구역경계 지도 데이터 로드
level1.shp.file <- paste(data.path, level1.shp.file, sep="/")
level2.shp.file <- paste(data.path, level2.shp.file, sep="/")
level3.shp.file <- paste(data.path, level3.shp.file, sep="/")

# TM 중부 좌표계 : 통계청 지도
crsTMcenter <- CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=500000 +ellps=bessel +units=m +no_defs")
# WGS84 경위도 좌표계 : 구글 지도
crsWGS84lonlat <- CRS("+proj=longlat +zone=52 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +lat_0=38N +lon_0=127E")

# 시도 지도 데이터
level1.shp <- readShapePoly(level1.shp.file, verbose=T, proj4string=crsTMcenter)
# 시군구 지도 데이터
level2.shp <- readShapePoly(level2.shp.file, verbose=T, proj4string=crsTMcenter)
# 읍면동 지도 데이터
level3.shp <- readShapePoly(level3.shp.file, verbose=T, proj4string=crsTMcenter)


# ==================== 소스순번: 038 ==================== 

# 2010년도 센서스용 행정구역경계 지도 메타정보 로드
level1.dbf.file <- paste(data.path, level1.dbf.file, sep="/")
level2.dbf.file <- paste(data.path, level2.dbf.file, sep="/")
level3.dbf.file <- paste(data.path, level3.dbf.file, sep="/")

level1_dbf <- read.dbf(level1.dbf.file)
level2_dbf <- read.dbf(level2.dbf.file)
level3_dbf <- read.dbf(level3.dbf.file)

# cp949 코드를 utf-8로 변경 (MS-Windows 환경에서는 필요 없음)
level1_dbf$name <- iconv(level1_dbf$name, "cp949", "utf-8")
level2_dbf$name <- iconv(level2_dbf$name, "cp949", "utf-8")
level3_dbf$name <- iconv(level3_dbf$name, "cp949", "utf-8")


# ==================== 소스순번: 039 ==================== 

library(sqldf)

data1 <- subset(census.data, nchar(C행정구역별_읍면동)==2 & C행정구역별_읍면동>10)
data2 <- subset(census.data, nchar(C행정구역별_읍면동)==5)
data3 <- subset(census.data, nchar(C행정구역별_읍면동)==7)

level1_dbf <- sqldf("select level1_dbf.*, data1.* from level1_dbf, data1
                      where level1_dbf.code = data1.C행정구역별_읍면동")
level2_dbf <- sqldf("select level2_dbf.*, data2.* from level2_dbf, data2
                      where level2_dbf.code = data2.C행정구역별_읍면동")
level3_dbf <- sqldf("select level3_dbf.*, data3.* from level3_dbf, data3
                      where level3_dbf.code = data3.C행정구역별_읍면동")


# ==================== 소스순번: 040 ==================== 

nlevel <- 5
col.level <- cut(level1_dbf$총인구_명, pretty(level1_dbf$총인구_명, n=nlevel), dig.lab=8)
legends <- levels(col.level)
levels(col.level) <- heat.colors(length(legends))
# 인구분포도 그리기 및 수행시간 계산
runtime1 <- system.time(plot(level1.shp, axes=FALSE, bty="n",
                               col=as.character(col.level), lwd=0.3, border="#000055"))
# 수행시간 보기
runtime1
title("전국 시도 인구분포도")
legend("bottomright", legends, fill=levels(col.level), cex=0.8, title="인구 (단위:명)")
box()


# ==================== 소스순번: 041 ==================== 

runtime2 <- system.time(plotShp(level1.shp, axes=FALSE, bty="n",
                                  col=as.character(col.level), lwd=0.3, border="#000055"))
runtime2
title("전국 시도 인구분포도 - 사용자 정의 함수")
legend("bottomright", legends, fill=levels(col.level), cex=0.8, title="인구 (단위:명)")
box()


# ==================== 소스순번: 042 ==================== 

seoul_dbf <- level3_dbf[grep("^11", level3_dbf$code),]
seoul_shape <- level3.shp[level3.shp$code %in% seoul_dbf$code,]

nlevel <- 5
col.level <- cut(seoul_dbf$총인구_명, pretty(seoul_dbf$총인구_명, n=nlevel), dig.lab=6)
legends <- levels(col.level)
levels(col.level) <- heat.colors(length(legends))

plot(seoul_shape, axes=FALSE, bty="n", col=as.character(col.level), lwd=0.5, border="#000055")
title("동별 서울 인구분포도")
legend("topleft", legends, fill=levels(col.level), cex=0.8, title="인구 (단위:명)")
box()


# ==================== 소스순번: 043 ==================== 

library(ggmap)
mountains <- c("설악산", "태백산", "지리산", "소백산", "한라산", "내장산", "북한산")
# 명산들의 위도와 경도의 조회
xy <- geocode(mountains)
# WGS84 경위도 좌표계를 TM 중부좌표계로 변환
xy.new <- spTransform(SpatialPoints(xy, proj4string=crsWGS84lonlat), crsTMcenter)
# 지도를 그리고 명산의 위치를 표시하기
plotShp(level1.shp, bg="lightblue", col="cornsilk")
points(xy.new, pch=17, col="blue", cex=1.5)
text(x=xy.new@coords[, "lon"], y=xy.new@coords[, "lat"],
       col="red", labels=mountains, adj=0, pos=4, offset=0.5)


# ==================== 소스순번: 044 ==================== 

library(shapefiles)

sido <- read.shapefile("./datas/2010_1_0/temp")
is(sido)
is(level1.shp)

xlim <- c(sido$shp$header$xmin, sido$shp$header$xmax)
ylim <- c(sido$shp$header$ymin, sido$shp$header$ymax)

Lon <- sapply(sido$shp$shp, function(x) mean(x$points$X))
Lat <- sapply(sido$shp$shp, function(x) mean(x$points$Y))


# ==================== 소스순번: 045 ==================== 

library(mapplots)

basemap(xlim, ylim, xlab="", ylab="", main="전국 시도 인구현황 (단위:천명)", axes=F)
draw.shape(sido, col="cornsilk")
draw.bubble(Lon, Lat, level1_dbf$총인구_명, maxradius=50000, pch=21, bg='#00FF0070')

legend.z <- round(max(level1_dbf$총인구_명)/1000,0)
legend.bubble("topright", z=legend.z, maxradius=50000, inset=0.02, bg="lightblue",
                txt.cex=0.8, pch=21, pt.bg="#00FF0050")


# ==================== 소스순번: 046 ==================== 

xlim <- level1.shp@bbox[1, ]
ylim <- level1.shp@bbox[2, ]

Lon <- sapply(level1.shp@polygons, function(x) x@labpt)[1, ]
Lat <- sapply(level1.shp@polygons, function(x) x@labpt)[2, ]

plot(level1.shp, bg="lightblue", col="cornsilk")
draw.bubble(Lon, Lat, level1_dbf$총인구_명, maxradius=50000, pch=21, bg='#00FF0070')

legend.z <- round(max(level1_dbf$총인구_명)/1000,0)
legend.bubble("topright", z=legend.z, maxradius=50000, inset=0.02, bg="lightblue",
                txt.cex=0.8, pch=21, pt.bg="#00FF0050")


# ==================== 소스순번: 047 ==================== 

library(reshape2)

household <- cbind(Lon, Lat,
                     level1_dbf[, c("code", "단독주택_호", "아파트_호", "연립주택_호", "다세대주택_호")])
names(household) <- do.call("c", sapply(names(household), strsplit, split="_호"))
household <- melt(household, id=c("Lon", "Lat", "code"), na.rm=TRUE)

xyz <- make.xyz(household$Lon, household$Lat, household$value, household$variable)
cols <- rainbow(4, alpha=0.8)

basemap(xlim, ylim, xlab="", ylab="", main="전국 시도 주거현황 (단위:가구)", axes=F)
draw.shape(sido, col="cornsilk")
draw.pie(xyz$x, xyz$y, xyz$z, radius=30000, col=cols)

labs <- c("단독주택", "아파트", "연립주택", "다세대주택")
legend.pie(550000, 100000, labels=labs, radius=30000, bty="n", col=cols, cex=0.8, label.dist=1.3)
legend.z <- round(max(rowSums(xyz$z, na.rm=TRUE))/10^3, 0)
legend.bubble(550000, 200000, z=legend.z, round=1, maxradius=30000, bty="n", txt.cex=0.6)
text(550000, 250000, "가구 수(천 가구)", cex=0.8)


# ==================== 소스순번: 048 ==================== 

library(ggmap)
# 필자의 집 위도와 경도
longitude <- 127.064471
latitude <- 37.666028
# 구글 맵에서 해당 위치의 맵 정보 가져오기
map <- get_googlemap("서울", zoom = 11, maptype = "roadmap",
                       markers = data.frame(longitude, latitude))
# 맵 그리기
ggmap(map)


# ==================== 소스순번: 049 ==================== 

# 1. 데이터 다운로드 및 R로 읽어 오기
library(repmis)
traffic <- source_DropboxData("traffic.csv", "wsjmfxx1cex5mbv", sep=",", header=TRUE)

# 2. 돌발상황 시작점 경도/위도 데이터의 범위 살펴보기
range(traffic$start.pos.x)
range(traffic$start.pos.y)

# 3. 경도/위도 데이터가 0인 데이터 필터링 하기
traffic <- traffic[!traffic$start.pos.x==0, ]
traffic <- traffic[!traffic$start.pos.y==0, ]


# ==================== 소스순번: 050 ==================== 

theme_set(theme_classic(base_family = "나눔고딕"))

map <- qmap(location = "서울", zoom = 11, maptype = "roadmap", legend = "topleft")
p <- map + geom_point(aes(x = start.pos.x, y = start.pos.y, colour = info.tit),
       size = 5, alpha = 0.7, data = traffic)
p + ggtitle("서울시 돌발상황 위치")


# ==================== 소스순번: 051 ==================== 

agg <- aggregate(list(z=traffic$rpt.id),
                   list(x=traffic$start.pos.x, y=traffic$start.pos.y,
                        class=traffic$info.tit), length)
maxz <- max(agg$z, na.rm = T)
agg$z <- sqrt(agg$z)/sqrt(maxz)

map <- qmap(location='서울', zoom=11, maptype="roadmap")
p <- map + geom_point(aes(x=x, y=y, size=z, bg=class),
       alpha=0.6, pch=21, data=agg) + scale_size(range=c(0, 20))
p + ggtitle("서울시 돌발상황 위치별 버블 차트")


# ==================== 소스순번: 052 ==================== 

loc <- geocode("서울")
loc
is(loc)
loc <- as.numeric(loc)


# ==================== 소스순번: 053 ==================== 

map <- get_map(loc, zoom=11, maptype="satellite")
p <- ggmap(map, extent="device", legend="topleft") +
       stat_bin2d(aes(x=start.pos.x, y=start.pos.y, colour=info.tit, fill=info.tit),
                 bins=30, alpha=1/2, data=traffic)
p + ggtitle("서울시 돌발상황 분포")


# ==================== 소스순번: 054 ==================== 

map <- qmap(location='서울', zoom=11, maptype="terrain", legend="topleft")
p <- map + stat_density2d(aes(x=start.pos.x, y=start.pos.y, fill=..level..), alpha=0.45,
       size=2, bins=4, data=traffic, geom="polygon")
p + ggtitle("서울시 돌발상황 분포 - 레벨 플롯")


# ==================== 소스순번: 055 ==================== 

map <- qmap(location='서울', zoom=11, maptype="roadmap", legend = "topleft")
p <- map + geom_hex(aes(x=start.pos.x, y=start.pos.y), bins=12,
       alpha=0.6, color="white", data=traffic)
p <- p + scale_fill_gradientn(colours = terrain.colors(15))
p + ggtitle("서울시 돌발상황 위치별 HexBin 플롯")


# ==================== 소스순번: 056 ==================== 

library(ROAuth)

download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

# requestURL 설정하기
requestURL <- "https://api.twitter.com/oauth/request_token"

# accessURL 설정하기
accessURL <- "https://api.twitter.com/oauth/access_token"

# authURL 설정하기
authURL <- "https://api.twitter.com/oauth/authorize"

# Application의 consumer key로 사용할 API Key 설정하기
consumerKey <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

# Application의 consumer secret로 사용할 API Secret 설정하기
consumerSecret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

# OAuth 클래스 객체 생성하기
twitCred <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret,
                               requestURL = requestURL, accessURL = accessURL, authURL = authURL)


# ==================== 소스순번: 057 ==================== 

twitCred$handshake(cainfo="cacert.pem")


# ==================== 소스순번: 058 ==================== 

library(twitteR)

registerTwitterOAuth(twitCred)
searchTwitter("Big Data", n=3)


# ==================== 소스순번: 059 ==================== 

save(list="twitCred", file="twitteR_credentials")

library(twitteR)
load("twitteR_credentials")
registerTwitterOAuth(twitCred)
searchTwitter("Visualization", n=3, lang="en")


# ==================== 소스순번: 060 ==================== 

twitter.vis <- searchTwitter("Visualization Analytics", n=100, lang="en",
                               since="2014-07-21", until="2014-07-25")


# ==================== 소스순번: 061 ==================== 

vis.en <- searchTwitter("Visualization Analytics", lang="en")
vis.ko <- searchTwitter("데이터 시각화", lang="ko")
save(vis.en, vis.ko, file="vis_twit.RData")


# ==================== 소스순번: 062 ==================== 

URL <- "https://dl.dropboxusercontent.com/u/46305178/datas/vis_twit.RData"
getDropboxRData(URL)


# ==================== 소스순번: 063 ==================== 

clean.text <- function(some_txt)
  {
      some_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
      some_txt <- gsub("@\\w+", "", some_txt)
      some_txt <- gsub("[[:punct:]]", "", some_txt)
      some_txt <- gsub("[[:digit:]]", "", some_txt)
      some_txt <- gsub("http\\w+", "", some_txt)
      some_txt <- gsub("[ \t]{2,}", "", some_txt)
      some_txt <- gsub("^\\s+|\\s+$", "", some_txt)
      some_txt <- gsub("amp", "", some_txt)
      some_txt <- gsub("\\n", "", some_txt)
      some_txt <- gsub("[\u{0093}-\u{0094}]", "", some_txt)

      try.tolower <- function(x)
      {
          y <- NA
          try_error <- tryCatch(tolower(x), error=function(e) e)
          if (!inherits(try_error, "error"))
              y <- tolower(x)
          return(y)
      }

      some_txt <- sapply(some_txt, try.tolower)
      some_txt <- some_txt[some_txt != ""]
      names(some_txt) <- NULL
      return(some_txt)
  }


# ==================== 소스순번: 064 ==================== 

vis.txt <- sapply(vis.en, function(x) x$getText())


# ==================== 소스순번: 065 ==================== 

vis.clean <- unique(clean.text(vis.txt))


# ==================== 소스순번: 066 ==================== 

library(tm)

vis.corpus <- Corpus(VectorSource(vis.clean))

tdm <- TermDocumentMatrix(
      vis.corpus,
      control = list(
          removePunctuation = TRUE,
          stopwords = c("visualization", "analytics", "data", "big", stopwords("english")),
          removeNumbers = TRUE, tolower = TRUE)
  )
tdm
m <- as.matrix(tdm)
m[11:13, 1:5]


# ==================== 소스순번: 067 ==================== 

library(wordcloud)

word_freqs <- sort(rowSums(m), decreasing=TRUE)
dm <- data.frame(word=names(word_freqs), freq=word_freqs)

wordcloud(dm$word, dm$freq, random.order=FALSE, min.freq=2, colors=brewer.pal(8, "Dark2"))


# ==================== 소스순번: 068 ==================== 

library(KoNLP)

vis.txt <- sapply(vis.ko, function(x) x$getText())
vis.clean <- unique(clean.text(vis.txt))

useSejongDic()
mergeUserDic(data.frame("편집", "ncp"))
mergeUserDic(data.frame("도구", "ncn"))
mergeUserDic(data.frame("인사이트", "ncn"))

result_nouns <- Map(extractNoun, vis.clean)

vis.table <- table(unlist(result_nouns, use.name=F))
vis.table <- vis.table[!names(vis.table) %in%
                             c("데이터", "자료", "시각화", "한", "것", "수", "곳", "적")]
wordcloud(names(vis.table), vis.table, random.order=FALSE,
            min.freq=1, colors=brewer.pal(8, "Dark2"))


# ==================== 소스순번: 069 ==================== 

mm <- m[apply(m, 1, sum) >= 3, ]

m.clus <- hclust(dist(mm, method="binary"), method="ward")

plot(m.clus)
rect.hclust(m.clus, k=3, border="red")


# ==================== 소스순번: 070 ==================== 

library(ape)

# 군집의 개수를 4으로 설정
k <- 4
clus3 <- cutree(m.clus, k=k)

# Dark2 팔레트에서 k개의 색상을 추출함
word.colors <- brewer.pal(k, "Dark2")

# 각각의 단어에 색상을 부여함
clus.colors <- word.colors[clus3]

m.phylo <- as.phylo(m.clus)

# 수형도를 그림
par(mfrow=c(2, 3))
plot.phylo(m.phylo, type="phylogram", cex=log(sqrt(rowSums(mm))), edge.color="gray50",
             tip.color=clus.colors, lab4ut="axial", main="type=\"phylogram\"")
plot.phylo(m.phylo, type="cladogram", cex=1, edge.color="black",
             tip.color="red", lab4ut="axial", main="type=\"cladogram\"")
plot(m.phylo, type="fan", cex=log(sqrt(rowSums(mm))), edge.color="blue",
       tip.color=clus.colors, lab4ut="axial", main="type=\"fan\"")
plot(m.phylo, type="radial", cex=log(sqrt(rowSums(mm))), edge.color="gray50",
       tip.color=clus.colors, lab4ut="axial", main="type=\"radial\"")
plot(m.phylo, type="unrooted", cex=log(sqrt(rowSums(mm))), edge.color="red",
       tip.color=clus.colors, main="type=\"unrooted\"")
plot(m.phylo, type="unrooted", cex=log(sqrt(rowSums(mm))), edge.color="red",
       tip.color=clus.colors, lab4ut="axial", use.edge.length=FALSE,
       main="type=\"unrooted\", use.edge.length=F")
par(mfrow=c(1, 1))


# ==================== 소스순번: 071 ==================== 

library(arulesViz)
# 트랜잭션 클래스 객체로 변환
trans <- as(t(m), "transactions")
trans
rules <- apriori(trans, parameter=list(support=0.03, confidence=0.6))
arules::inspect(rules[1:2])
# 연관규칙 플롯 그리기
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="grouped")


# ==================== 소스순번: 072 ==================== 

if (!require(DescTools)) {
      install.packages("DescTools")
      require(DescTools)
  }
# 데이터 조작
northeast.states <- USArrests[state.region=="Northeast", ]
# 안형 그래프 출력
PlotFaces(northeast.states)


# ==================== 소스순번: 073 ==================== 

PlotFaces(northeast.states, fill=TRUE)


# ==================== 소스순번: 074 ==================== 

# 주의 면적을 나타내는 Area 변수의 추가
northeast.states <- cbind(USArrests, Area=state.area)[state.region=="Northeast", ]
# 데이터 조작
stars(northeast.states, key.loc = c(9, 2), len=0.6, mar=c(3,2,2,5))


# ==================== 소스순번: 075 ==================== 

stars(northeast.states, key.loc = c(9, 2), ncol=4, len=0.6, full=FALSE)


# ==================== 소스순번: 076 ==================== 

stars(northeast.states, key.loc = c(9, 2), ncol=4, draw.segments = TRUE, len=0.6)


# ==================== 소스순번: 077 ==================== 

cols <- c("lightblue", "mistyrose", "lightcyan", "lavender", "lightyellow1")
stars(northeast.states, draw.segments = TRUE, len=0.6,
        col.segments=cols, mar=c(2, 2, 4, 4),
        full=FALSE, frame.plot=TRUE, main="segment diagrams example")
legend("topright", names(northeast.states), fill=cols)

