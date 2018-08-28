# -----------------------------------
# R visualization - 소스코드
#   출판사: 도서출판 인사이트
#   저자: 유충현, 홍성학
#   챕터: 8장
#   파일명: chapter_08.R
# -----------------------------------


# ==================== 소스순번: 001 ==================== 

lm.fit <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
lm.fit


# ==================== 소스순번: 002 ==================== 

plot(lm.fit, which = 1)


# ==================== 소스순번: 003 ==================== 

plot(lm.fit, which = 2)


# ==================== 소스순번: 004 ==================== 

plot(lm.fit, which = 3)


# ==================== 소스순번: 005 ==================== 

plot(lm.fit, which = 4)


# ==================== 소스순번: 006 ==================== 

plot(lm.fit, which = 5)


# ==================== 소스순번: 007 ==================== 

plot(lm.fit, which = 6)


# ==================== 소스순번: 008 ==================== 

par(mfrow=c(2, 2))
plot(lm.fit, ask=FALSE)
par(mfrow=c(1, 1))


# ==================== 소스순번: 009 ==================== 

if (!require(tree)) {
      install.packages("tree")
      require(tree)
  }
data(cpus, package="MASS")
cpus.ltr <- tree(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpus)
plot(cpus.ltr)
text(cpus.ltr)


# ==================== 소스순번: 010 ==================== 

iris.tr <- tree(Species ~., iris)
plot(iris.tr)
text(iris.tr)


# ==================== 소스순번: 011 ==================== 

if (!require(party)) {
      install.packages("party")
      require(party)
  }
airq <- subset(airquality, !is.na(Ozone))
airct <- ctree(Ozone ~ ., data = airq, controls = ctree_control(maxsurrogate = 3))
plot(airct)


# ==================== 소스순번: 012 ==================== 

iris.ct <- ctree(Species ~ ., data = iris)
plot(iris.ct)


# ==================== 소스순번: 013 ==================== 

if (!require(TH.data)) {
      install.packages("TH.data")
      require(TH.data)
  }
data("GlaucomaM", package = "TH.data")
glauct <- ctree(Class ~ ., data = GlaucomaM)
plot(glauct)


# ==================== 소스순번: 014 ==================== 

if (!require(survival)) {
      install.packages("survival")
      require(survival)
  }
data("GBSG2", package = "TH.data")
GBSG2ct <- ctree(Surv(time, cens) ~ .,data = GBSG2)
plot(GBSG2ct)


# ==================== 소스순번: 015 ==================== 

if (!require(ROCR)) {
      install.packages("ROCR")
      require(ROCR)
  }
# 데이터를 training set과 test set으로 70%:30%로 나누기
set.seed(3)
idx <- sample(2, size=NROW(GlaucomaM), replace=TRUE, prob=c(0.7, 0.3))
GlaucomaM.train <- GlaucomaM[idx==1, ]
GlaucomaM.test <- GlaucomaM[idx==2, ]

# tree 패키지와 party 패키지로 모델 만들기
GlaucomaM.trian.tr <- tree(Class ~ ., GlaucomaM.train)
GlaucomaM.trian.ct <- ctree(Class ~ ., GlaucomaM.train)

# tree 패키지의 모델 성능 계산
test.tr <- predict(GlaucomaM.trian.tr, newdata = GlaucomaM.test)
tr.pred <- prediction(test.tr[, 2], GlaucomaM.test$Class)
tr.perf <- performance(tr.pred, "tpr", "fpr")

# party 패키지의 모델 성능 계산
test.ct <- predict(GlaucomaM.trian.ct, type = "prob", newdata = GlaucomaM.test)
test.ct <- t(sapply(test.ct, "rbind"))
ct.pred <- prediction(test.ct[, 2], GlaucomaM.test$Class)
ct.perf <- performance(ct.pred, "tpr", "fpr")

# ROC curve 그리기
plot(tr.perf, col=2, main="ROC curve")
plot(ct.perf, col=4, add=TRUE)
lines(c(0, 1), c(0, 1), lwd=0.8, col="gray")
legend("topleft", c('tree', 'party'), lty=1, col=c(2, 4))


# ==================== 소스순번: 016 ==================== 

tr.perf <- performance(tr.pred, "prec", "rec")
ct.perf <- performance(ct.pred, "prec", "rec")

plot(tr.perf, col=2, main="precision/recall curve")
plot(ct.perf, col=4, add=TRUE)
legend("topright", c('tree', 'party'), lty=1, col=c(2, 4))


# ==================== 소스순번: 017 ==================== 

tr.perf <- performance(tr.pred, "sens", "spec")
ct.perf <- performance(ct.pred, "sens", "spec")

plot(tr.perf, col=2, main="sensitivity/specificity curve")
plot(ct.perf, col=4, add=TRUE)
lines(c(0, 1), c(1, 0))
legend("topright", c('tree', 'party'), lty=1, col=c(2, 4))


# ==================== 소스순번: 018 ==================== 

distance <- dist(USArrests)
hc <- hclust(distance, method="ward")
plot(hc)


# ==================== 소스순번: 019 ==================== 

plot(hc)
rect.hclust(hc, k=5, border="red")


# ==================== 소스순번: 020 ==================== 

wss <- (nrow(USArrests)-1) * sum(apply(USArrests, 2, var))
for (i in 2:15)
      wss[i] <- sum(kmeans(USArrests, centers=i)$withinss)
plot(wss, type="b", pch=19, xlab="Number of Clusters", ylab="Within groups sum of squares")


# ==================== 소스순번: 021 ==================== 

# 3개의 군집 구하기
fit <- kmeans(USArrests, 3)
fit$cluster
# 군집별 변수의 평균 구하기
cluster.mean <- aggregate(USArrests, by=list(fit$cluster), FUN=mean)
names(cluster.mean)[1] <- "cluster"
cluster.mean


# ==================== 소스순번: 022 ==================== 

RADAR <- function(x, fill=TRUE, col=c("blue", "red", "cyan","yellow","green"), alpha=0.2,
                    main="Cluster RADAR Chart", varname=NULL, locator=FALSE) {
      n.vars <- NCOL(x)
      n.clusters <- NROW(x)
      # min-max standardization
      std <- function(x)
          sx <- (x - min(x)) / diff(range(x)) + 0.5
      sx <- apply(x, 2, std)
      #그래프를 그리기 위한 값 설정
      max.value <- max(sx)
      limit <- 25 * max.value / 20
      # 색상 정의
      cols <- col2rgb(col)/255
      cols <- rgb(cols[1,], cols[2, ], cols[3, ], alpha=alpha)
      # 그래픽 환경 저장
      op <- par()
      on.exit(par(op))
      # 기초 그래프 출력
      plot.new()
      par(mar = c(0.5, 0.5, 1.5, 2))
      plot.window(xlim = c(-limit, limit), ylim = c(-limit, limit), asp = 1)
      # 각 변수들의 점수를 출력
      for (i in 1:n.clusters) {
          temp <- sx[i, ]
          theta <- seq(0, length = n.vars, by = 2 * pi/n.vars)
          x <- temp * cos(theta)
          y <- temp * sin(theta)
          # 변수들의 점수를 표현
          if (fill) polygon(x, y, col = cols[i])
          else polygon(x, y, col = col[i], density = 0, lwd = 2, lty = i)
      }
      segments(0, 0, max.value * cos(theta), max.value * sin(theta), lty = "dotted", lwd = 0.8)
      # 원을 그린다.
      base.score <- seq(0, max.value, length=5)[-c(1, 5)]
      # 기준원(25, 50, 75, 100)을 그린다.
      phi <- seq(3, 360 - 3, length = 72) * (pi/180)
      for (r in base.score)
          lines(r * cos(phi), r * sin(phi), lty = "dotted")
      lines(max.value * cos(phi), max.value * sin(phi))
      # 기준점수 가이드를 출력한다.
      pos <- c(base.score, max.value)
      text(pos, rep(0, length(pos)), c(25, 50, 75, 100))
      # 변수 이름 출력하기
      varname <- if (is.null(varname)) names(x) else varname
      text(25 * max.value / 20 * cos(theta), 23 * max.value / 20 * sin(theta), varname, cex=.8)
      title(main=main)
      legends <- paste("Cluster", 1:n.clusters)
      # 범례출력
      if (locator) {
          if (fill) legend(locator(1), legends, fill=cols[1:length(legends)],  cex=.8)
          else legend(locator(1), legends, lty=1:length(legends), col=col, cex=.8)
      }
      else {
          if (fill) legend("topright", legends, fill=cols[1:length(legends)],  cex=.8)
          else legend("topright", legends, lty=1:length(legends), col=col, cex=.8)
      }
  }


# ==================== 소스순번: 023 ==================== 

RADAR(cluster.mean[, -1])


# ==================== 소스순번: 024 ==================== 

if (!require(mclust)) {
      install.packages("mclust")
      require(mclust)
  }
model <- Mclust(USArrests)
summary(model, classification=TRUE)


# ==================== 소스순번: 025 ==================== 

plot(model, what="BIC")


# ==================== 소스순번: 026 ==================== 

plot(model, what="classification")


# ==================== 소스순번: 027 ==================== 

plot(model, what="uncertainty")


# ==================== 소스순번: 028 ==================== 

plot(model, what="density")


# ==================== 소스순번: 029 ==================== 

oneway <- aov(breaks ~ tension, data=warpbreaks)
oneway


# ==================== 소스순번: 030 ==================== 

boxplot(breaks ~ tension, data=warpbreaks)


# ==================== 소스순번: 031 ==================== 

if (!require(gplots)) {
      install.packages("gplots")
      require(gplots)
  }
plotmeans(breaks ~ tension, data=warpbreaks, xlab="The level of tension",
            ylab="The number of breaks", main="Mean Plot\nwith 95% CI")


# ==================== 소스순번: 032 ==================== 

twoway <- aov(breaks ~ wool + tension, data=warpbreaks)
twoway


# ==================== 소스순번: 033 ==================== 

op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
       varwidth = TRUE, subset = wool == "A", main = "Wool A")
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
       varwidth = TRUE, subset = wool == "B", main = "Wool B")
mtext("warpbreaks data", side = 3, outer = TRUE)
par(op)


# ==================== 소스순번: 034 ==================== 

with(warpbreaks, {
      interaction.plot(tension, wool, breaks, fixed = TRUE, col = 2:3, leg.bty = "o")
  })


# ==================== 소스순번: 035 ==================== 

if (!require(multcomp)) {
      install.packages("multcomp")
      require(multcomp)
  }
# set up all-pair comparisons for factor `tension'
wht <- glht(twoway, linfct = mcp(tension = "Tukey"))
wht
# 95% simultaneous confidence intervals
plot(confint(wht))


# ==================== 소스순번: 036 ==================== 

# 종속 패키지를 가져오는 사용자 정의 함수
getDepends <- function(whichs=c("Depends", "Imports", "LinkingTo", "Suggests")) {
      # arules 패키지 자원 불러오기
      if (!require(arules)) {
          install.packages("arules")
          require(arules)
      }
      # CRAN에서 사용 가능한 패키지 정보 가져오기
      pkgs <- available.packages()
      pkg.name <- pkgs[, "Package"]
      # 종속 패키지 가져 오기
      depends <- tools::package_dependencies(pkg.name, pkgs, which=whichs)
      # 종속 패키지가 없는 패키지 제거
      depends <- depends[unlist(lapply(depends, function(x) length(x)>0))]
      # 종속 패키지로 트랜잭션 데이터 생성하기
      as(depends, "transactions")
  }
# 종속 패키지 가져 오는 함수 호출
options(repos = "http://cran.r-project.org")
trans <- getDepends(whichs=c("Depends", "Imports"))


# ==================== 소스순번: 037 ==================== 

URL <- "https://dl.dropboxusercontent.com/u/46305178/datas/trans.RData"
getDropboxRData(URL)


# ==================== 소스순번: 038 ==================== 

summary(trans)


# ==================== 소스순번: 039 ==================== 

rules <- apriori(trans, parameter=list(supp=0.005, conf=0.6, target="rules"))


# ==================== 소스순번: 040 ==================== 

summary(rules)


# ==================== 소스순번: 041 ==================== 

inspect(rules[1:10])


# ==================== 소스순번: 042 ==================== 

if (!require(arulesViz)) {
      install.packages("arulesViz")
      require(arulesViz)
  }
plot(rules, measure=c("support", "confidence"), shading="lift")


# ==================== 소스순번: 043 ==================== 

plot(rules, method="grouped")


# ==================== 소스순번: 044 ==================== 

plot(rules, method="graph", control=list(type="items"))


# ==================== 소스순번: 045 ==================== 

plot(rules, method="paracoord", control=list(reorder=TRUE))


# ==================== 소스순번: 046 ==================== 

rules <- apriori(trans, parameter=list(supp=0.005, conf=0.6, maxlen=2, target="rules"))
plot(rules, method="graph", control=list(type="items"))


# ==================== 소스순번: 047 ==================== 

# Task View에 포함되어 있는 패키지 이름 가져오는 사용자 정의 함수
getTaskViewPackages <- function(topic="Graphics") {
      library(RCurl)
      library(XML)
      # Task View URL 만들기
      topic <- paste("http://cran.r-project.org/web/views/", topic, ".html", sep="")
      html <- getURL(topic, followlocation = TRUE)
      href <- getNodeSet(htmlParse(html),"//a")
      pkg.flag <- as.logical(unlist(lapply(href, function(x) length(grep("index\\.html$", xmlAttrs(x))))))
      # 패키지명만 가져오기
      href <- href[pkg.flag]
      unique(unlist(lapply(href, getChildrenStrings)))
  }
# 종속관계를 가져오는 사용자 정의 함수
getDepndsNetwork <- function(topic="all", pos=c("transmitter", "receiver", "both")[1],
                               whichs=c("Depends", "Imports", "LinkingTo", "Suggests")) {
      library(tools)
      # transmitter와 receiver 모두 가져오기
      if (pos=="both") {
          return(rbind(getDepndsNetwork(topic=topic, pos="transmitter", whichs=whichs),
                       getDepndsNetwork(topic=topic, pos="receiver", whichs=whichs)))
      }
      # CRAN에서 패키지 정보 가져오기
      pkgs <- available.packages()
      pkg.name <- if (topic=="all") pkgs[, "Package"] else getTaskViewPackages(topic)

      # transmitter와 receiver의 경우에 따른 reverse 인수 설정
      if (pos=="transmitter") reverse <- TRUE
      if (pos=="receiver") reverse <- FALSE
      # CRAN에서 패키지 의존성 정보 가져오기
      depends <- package_dependencies(pkg.name, pkgs, which=whichs, reverse=reverse, recursive=FALSE)
      depends <- depends[unlist(lapply(depends, function(x) length(x)>0))]
      # edge에 대한 변수 초기화
      edge.cnt <- sum(unlist(lapply(depends, length)))
      edge <- matrix(character(0), ncol=2, nrow=edge.cnt)
      vertex.name <- attr(depends,"names")
      # edge 구하기
      idx <- 1
      for (pkg in vertex.name) {
          vertex <- unlist(depends[pkg])
          cnt <- length(vertex)
          edge[idx:(idx+cnt-1), ] <- if (pos=="transmitter") cbind(pkg, vertex) else cbind(vertex, pkg)
          idx <- idx + cnt
      }
      colnames(edge) <- c("transmitter", "receiver")
      edge
  }


# ==================== 소스순번: 048 ==================== 

pkgs <- getTaskViewPackages(topic="Graphics")
pkgs


# ==================== 소스순번: 049 ==================== 

# transmitter 기준
options(repos = "http://cran.r-project.org")
edge.transmitter <- getDepndsNetwork(topic="Graphics", whichs=c("Depends"))
NROW(edge.transmitter)
head(edge.transmitter, n=3)
# receiver 기준
edge.receiver <- getDepndsNetwork(topic="Graphics", pos="receiver", whichs=c("Depends"))
NROW(edge.receiver)
head(edge.receiver, n=3)
# transmitter 기준 + receiver 기준
edge.both <- getDepndsNetwork(topic="Graphics", pos="both", whichs=c("Depends"))
NROW(edge.both)
head(edge.both, n=3)
tail(edge.both, n=3)


# ==================== 소스순번: 050 ==================== 

if (!require(igraph)) {
      install.packages("igraph")
      require(igraph)
  }
# igraph 그래프 객체로 변환
g <- graph.data.frame(edge.receiver)
# degree의 계산
in_degree <- igraph::degree(g, mode="in")
out_degree <- igraph::degree(g, mode="out")
# degree가 큰 상위 열개의 패키지 조회
sort(in_degree, decreasing=T)[1:10]
sort(out_degree, decreasing=T)[1:10]


# ==================== 소스순번: 051 ==================== 

if (!require("RColorBrewer")) {
      install.packages("RColorBrewer")
      library(RColorBrewer)
  }
# in_degree에 비례하는 색상 정의
cols <- colorRampPalette(brewer.pal(9,"Blues"))(15)
idx <- as.integer(cut(in_degree, pretty(in_degree, n=5), include.lowest=TRUE)) * 2 + 3
V(g)$degree <- in_degree
V(g)$label.cex <- 1 + (in_degree - min(in_degree)) / diff(range(in_degree))
V(g)$label.color <- cols[idx]
plot(g, layout=layout.kamada.kawai(g, niter=10000), vertex.size = 0, edge.arrow.size=0.1)


# ==================== 소스순번: 052 ==================== 

# out_degree에 비례하는 색상 정의
cols <- colorRampPalette(brewer.pal(9,"Blues"))(15)
idx <- as.integer(cut(out_degree, pretty(out_degree, n=5), include.lowest=TRUE)) * 2 + 3
V(g)$degree <- out_degree
V(g)$label.cex <- 1 + (out_degree - min(out_degree)) / diff(range(out_degree))
V(g)$label.color <- cols[idx]
plot(g, layout=layout.kamada.kawai(g, niter=10000), vertex.size = 0, edge.arrow.size=0.1)


# ==================== 소스순번: 053 ==================== 

# vertex와 edge의 모양 변경
V(g)$label.cex <- 0.8
V(g)$label.color <- "black"
E(g)$arrow.size <- 0.3
wc <- walktrap.community(g)
plot(wc, g)


# ==================== 소스순번: 054 ==================== 

mw <- graph.formula(1-2:3:4:5:6, 2-3:4:5:7, 3-4:6:7, 4-5:6:7,
                      5-6:7:21, 6-7, 7-8:11:14:19, 8-9:11:14, 9-10,
                      10-12:13, 11-12:14, 12-16, 13-16, 14-15, 15-16,
                      17-18:19:20, 18-20:21, 19-20:22:23, 20-21,
                      21-22:23, 22-23)
mwBlocks <- cohesive.blocks(mw)
plot(mwBlocks, mw)


# ==================== 소스순번: 055 ==================== 

if (!require("bipartite")) {
      install.packages("bipartite")
      library(bipartite)
  }
# 데이터 전처리
pivot.receiver <- xtabs(~transmitter+receiver, data=edge.receiver)
pivot.receiver <- t(pivot.receiver)
# bipartite 플롯
plotweb(pivot.receiver, col.high="lightblue", col.low="mistyrose")


# ==================== 소스순번: 056 ==================== 

plotweb(Safariland)


# ==================== 소스순번: 057 ==================== 

if (!require("DescTools")) {
      install.packages("DescTools")
      library(DescTools)
  }
# 데이터 전처리
pivot.receiver <- xtabs(~transmitter+receiver, data=edge.receiver)
# bipartite 플롯
PlotCirc(pivot.receiver)


# ==================== 소스순번: 058 ==================== 

# lattice 패키지나 stats 패키지를 사용한 패키지
subnetwork <- xtabs(~transmitter+receiver, subset=transmitter %in% c("lattice", "stats"),
                      data=edge.receiver, drop.unused.levels=TRUE)
PlotCirc(subnetwork)


# ==================== 소스순번: 059 ==================== 

if (!require(arcdiagram)) {
      install.packages("devtools")
      library(devtools)
      install_github('arcdiagram', username = 'gastonstat')
      require(arcdiagram)
  }
# 아크 다이어그램 플롯팅
arcplot(edge.receiver)


# ==================== 소스순번: 060 ==================== 

# igraph 그래프 객체로 변환
g <- graph.data.frame(edge.receiver)
# edgelist 구하기
edgelist <- get.edgelist(g)
# vertex degree 구하기
out_degree <- igraph::degree(g, mode="out")
degrees <- igraph::degree(g)
# vertex labels 구하기
vlabels <- names(degrees)
# vertex 내부 색상 지정
vfill <- ifelse(in_degree==0, "#8b91d4", "#f0c753")
# vertex 테두리 색상 지정
vborders <- ifelse(in_degree==0, "#6f74a9", "#b89e54")
# vertex 그룹 지정
vgroups <- ifelse(in_degree==0, 1, 0)
# 임시 데이터 프레임 생성
x <- data.frame(vgroups, degrees, vlabels, ind = 1:vcount(g))
# vgroups, degrees, vlabels로 순서를 정함
if (!require("plyr")) {
      install.packages("plyr")
      library(plyr)
  }
y <- arrange(x, desc(vgroups), desc(degrees))
# 'ind'로 새로운 순서를 정하기 위한 변수 생성
new_ord <- y$ind
# arc diagram 플롯팅
arcplot(edgelist, vertices=vlabels, ordering=new_ord, cex.labels=0.8, cex.nodes=log(degrees)+0.5,
          bg.nodes=vfill, col.nodes=vborders, pch.nodes=21, lwd.nodes=1.5)


# ==================== 소스순번: 061 ==================== 

values <- degrees * 1.5
arcplot(edgelist, vertices=vlabels, ordering=new_ord, cex.labels=0.8, cex.nodes=log(degrees) + 0.5,
          bg.nodes=vfill, col.nodes=vborders, pch.nodes=21, lwd.nodes=1.5,
          col.arcs=hsv(0, 0, 0.2, 0.25), lwd.arcs=values)


# ==================== 소스순번: 062 ==================== 

plot(UKDriverDeaths)


# ==================== 소스순번: 063 ==================== 

plot(Seatbelts)


# ==================== 소스순번: 064 ==================== 

plot(stl(UKDriverDeaths, s.window="periodic"))


# ==================== 소스순번: 065 ==================== 

model3 <- filter(UKDriverDeaths, filter=rep(1/3, 3))
model6 <- filter(UKDriverDeaths, filter=rep(1/6, 6))
model12 <- filter(UKDriverDeaths, filter=rep(1/12, 12))
op <- par(no.readonly = TRUE)
par(mfrow=c(4, 1), mar=c(3, 1, 3, 1))
plot(UKDriverDeaths, main="original data")
plot(model3, main="3-points moving average")
plot(model6, main="6-points original data")
plot(model12, main="12-points original data")
par(op)


# ==================== 소스순번: 066 ==================== 

m <- HoltWinters(UKDriverDeaths, gamma = FALSE, beta = FALSE)
plot(m)
legend("topright", c("original", "exponenatial smoothing"), lty=1, col=c(1,2))


# ==================== 소스순번: 067 ==================== 

acf(UKDriverDeaths)


# ==================== 소스순번: 068 ==================== 

pacf(UKDriverDeaths)


# ==================== 소스순번: 069 ==================== 

if (!require(forecast)) {
      install.packages("forecast")
      require(forecast)
  }
fit <- auto.arima(UKDriverDeaths)
plot(forecast(fit, h=24))

