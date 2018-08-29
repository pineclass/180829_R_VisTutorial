# -----------------------------------
# R visualization - 소스코드
#   출판사: 도서출판 인사이트
#   저자: 유충현, 홍성학
#   챕터: 10장
#   파일명: chapter_10.R
# -----------------------------------


# ==================== 소스순번: 001 ==================== 

n <- 50
avg <- 175
std <- 3
set.seed(2)
height <- round(rnorm(n, mean = avg, sd = std), 1)
height
hist(height, col = "lightblue", main = "신장의 분포", xlab = "신장(cm)")


# ==================== 소스순번: 002 ==================== 

den <- density(height)
den
plot(den, main = "신장의 분포", xlab = "신장(cm)")


# ==================== 소스순번: 003 ==================== 

hist(height, col = "lightblue", probability = TRUE, main = "신장의 분포", + xlab = "신장(cm)")
lines(density(height))


# ==================== 소스순번: 004 ==================== 

plot(den, main = "신장의 분포", xlab = "신장(cm)", ylim = c(0, + 0.15))
den.norm <- function(x) dnorm(x, mean = avg, sd = std)
curve(den.norm, col = "red", add = TRUE, lty = 2)
abline(v = avg, col = "blue", lty = 3)
legend("topleft", c("3학년5반 신장", "X~N(175, 3)", expression(mu == 175)),
         lty = 1:3, col = c("black", "red", "blue"))


# ==================== 소스순번: 005 ==================== 

qqnorm(height, pch = 16)
qqline(height)


# ==================== 소스순번: 006 ==================== 

before <- height
after <- append(height, 190)
mean(before)
mean(after)
median(before)
median(after)
boxplot(list(전학이전 = before, 전학이후 = after), xlab = "전학 이전 이후 여부",
          ylab = "신장(cm)", main = "3학년 5반 신장의 Box plot")


# ==================== 소스순번: 007 ==================== 

p <- 1/2
n <- 50
size <- 10
set.seed(1)
toss <- rbinom(n = n, size = size, prob = p)
toss
toss.table <- table(toss)
toss.table
plot(toss.table, main="동전 던지기의 분포 (실험수=50, 던지기회수=10)",
      xlab="10번 던지기 중 앞면이 나온 횟수", ylab="실험에서 출현 횟수")


# ==================== 소스순번: 008 ==================== 

par(mfrow = c(1, 2))
barplot(toss.table, main = "동전 던지기의 분포 \n(실험수=50, 던지기회수=10)",
          xlab = "10번 던지기 중 앞면이 나온 횟수", ylab = "실험에서 출현 횟수")
toss.fac <- factor(toss, level = min(toss):max(toss))
toss.fac
toss.tab <- table(toss.fac)
toss.tab
barplot(toss.tab, main = "동전 던지기의 분포 \n(실험수=50, 던지기회수=10)",
          xlab = "10번 던지기 중 앞면이 나온 횟수", ylab = "실험에서 출현 횟수")
par(mfrow = c(1, 1))


# ==================== 소스순번: 009 ==================== 

toss.prop <- prop.table(toss.tab)
toss.prop
x <- barplot(toss.prop, main = "동전 던지기의 분포 (실험수=50, 던지기회수=10)",
               xlab = "10번 던지기 중 앞면이 나온 횟수", ylab = "실험에서 출현 횟수")
number.head <- min(toss):max(toss)
biom.prop <- dbinom(x = number.head, size = size, prob = p)
biom.prop
x
for (i in 1:length(x)) lines(c(x[i], x[i]), c(0, biom.prop[i]), col = "blue")
points(x, biom.prop, col = "red", pch = 16, cex = 1.2)
legend("topright", "X~bin(n=10, p=0.5)", bty = "n", pch = 16, lty = 1)


# ==================== 소스순번: 010 ==================== 

set.seed(1)
fruits <- sample(x = 1:5, size = 50, replace = TRUE)
fruits <- c("사과", "포도", "딸기", "체리", "복숭아")[fruits] > fruits <- factor(fruits)
fruits
fruits.tab <- table(fruits)
fruits.tab
barplot(fruits.tab, main = "과일 선호도 조사", col = "lightgreen",
          xlab = "과일의 종류", ylab = "선호 학생 수")


# ==================== 소스순번: 011 ==================== 

fruits.prop <- prop.table(fruits.tab)
fruits.prop
barplot(fruits.prop, main = "과일 선호도 조사", col = "lightgreen",
          xlab = "과일의 종류", ylab = "선호 학생의 비율")


# ==================== 소스순번: 012 ==================== 

imageBar <- function(x, imgfiles, main=NULL, xlab=NULL, ylab=NULL) {
    library(png)

    getImgName <- function(x) {
      x <- strsplit(strsplit(x, "\\.png")[[1]], "\\/")[[1]]
      x[length(x)]
    }

    imgname <- sapply(imgfiles, getImgName)

    for (i in 1:length(imgname)) {
      assign(imgname[i], readPNG(imgfiles[[i]]))
    }

    y <- barplot(x, horiz=TRUE, col="white", las=1, main=main,
                 xlab=xlab, ylab=ylab)

    for (i in 1:length(x)) {
      for (j in 1:x[i]) {
        rasterImage(get(imgname[i]), j-1, y[i] - 0.5, j, y[i] + 0.5)
      }
      rect(0, y[i] - 0.5, x[i], y[i] + 0.5, border="black")
    }
  }

# dropbox에서 과일의 모양을 나타내는 이미지 파일을 가져오는 루틴
# qdap 패키지의 로드, 없으면 설치 후 로드

if (!require(qdap)) {
      install.packages("qdap")
      require(qdap)
  }

# qdap::url_dl() 함수로 파일을 다운로드 함
url_dl("https://dl.dropboxusercontent.com/u/46305178/images/strawberry.png")
url_dl("https://dl.dropboxusercontent.com/u/46305178/images/peach.png")
url_dl("https://dl.dropboxusercontent.com/u/46305178/images/apple.png")
url_dl("https://dl.dropboxusercontent.com/u/46305178/images/cherry.png")
url_dl("https://dl.dropboxusercontent.com/u/46305178/images/grape.png")

files <- list(strawberry = "strawberry.png",
                peach = "peach.png",
                apple = "apple.png",
                cherry = "cherry.png",
                grape = "grape.png")

imageBar(fruits.tab, imgfiles=files,
           smain="3학년 5반 과일 선호도", xlab="학생수", ylab="과일 종류")


# ==================== 소스순번: 013 ==================== 

pie(fruits.tab, main = "3학년 5반 과일 선호도")


# ==================== 소스순번: 014 ==================== 

set.seed(1)
korean <- round(rnorm(50, mean = 70, sd = 10))
korean
mu <- mean(korean)
sigma <- sd(korean)
breaks <- c(0, mu - 1.5 * sigma, mu - 0.5 * sigma, mu + 0.5 * sigma,
              mu + 1.5 * sigma, 100)
breaks
lab <- c("가", "양", "미", "우", "수")
korean.grade <- ordered(cut(korean, breaks = breaks, labels = lab))
pass.flag <- factor(ifelse(korean.grade %in% c("가", "양"), "과락", "과락아님"))
table(pass.flag)
korean.tab <- table(korean.grade)
korean.tab
barplot(korean.tab, main = "국어성적 등급 현황", ylab = "학생수", xlab = "성적등급",
          col = ifelse(names(korean.tab) %in% c("가", "양"), "orangered", "lightblue"))
legend("topleft", c("과락", "과락아님"), bty = "n", fill = c("orangered", "lightblue"))


# ==================== 소스순번: 015 ==================== 

# HistData 패키지의 로드, 없으면 설치 후 로드
if (!require(HistData)) {
    install.packages("HistData")
    require(HistData)
  }

data(Galton)
dim(Galton)
names(Galton)
head(Galton)
summary(Galton)
table(Galton)
plot(child ~ parent, data = Galton, pch = 16, main = "부모키와 자식키의 관계")


# ==================== 소스순번: 016 ==================== 

plot(child ~ parent, data = Galton, pch = 16, main = "부모키와 자식키의 관계")
lm.fit <- lm(child ~ parent, data = Galton)
abline(lm.fit, col = "red")


# ==================== 소스순번: 017 ==================== 

plot(jitter(child, 5) ~ jitter(parent, 5), data = Galton,
       main = "부모키와 자식키의 관계 (jitter plot)",
       xlab = "부모의 키", ylab = "자식의 키")


# ==================== 소스순번: 018 ==================== 

sunflowerplot(Galton, size = 1/15, main = "부모키와 자식키의 관계 (sunflower)",
                xlab = "부모의 키", ylab = "자식의 키")


# ==================== 소스순번: 019 ==================== 

plot(Galton, col = "#0000FF08", pch = 16, cex = 1.2,
       main = "부모키와 자식키의 관계 (alpha color)",
       xlab = "부모의 키", ylab = "자식의 키")


# ==================== 소스순번: 020 ==================== 

op <- par(no.readonly = TRUE)
x <- 1:3
par(mfrow = c(2, 1))
# cex 값
plot(x, xlim = c(0, 4), ylim = c(0, 4), pch = 16, cex = 1:3, main = "cex의 크기")
# 면적 기준으로 환산한 cex 값
plot(x, xlim = c(0, 4), ylim = c(0, 4), pch = 16, cex = sqrt(1:3), main = "면적의 크기")
par(op)


# ==================== 소스순번: 021 ==================== 

tab <- table(Galton)
x <- as.numeric(dimnames(tab)$parent)
y <- as.numeric(dimnames(tab)$child)
axis.grid <- expand.grid(x, y)
plot(axis.grid, cex = sqrt(tab/pi), col = "blue", pch = 19,
       main = "부모키와 자식키의 관계 (점의 크기)",
       xlab = "부모의 키", ylab = "자식의 키")


# ==================== 소스순번: 022 ==================== 

symbols(axis.grid, circles = as.vector(sqrt(tab/pi)),
          inches = 0.15,
          fg = "blue", bg = "lightblue",
          main = "부모키와 자식키의 관계 (symbols 함수)",
          xlab = "부모의 키", ylab = "자식의 키")


# ==================== 소스순번: 023 ==================== 

# hexbin 패키지의 로드, 없으면 설치 후 로드
if (!require(hexbin)) {
    install.packages("hexbin")
    require(hexbin)
}

bin <- hexbin(Galton)
plot(bin, main = "부모키와 자식키의 관계 (Hexagon Binning)",
       xlab = "부모의 키", ylab = "자식의 키")


# ==================== 소스순번: 024 ==================== 

set.seed(101)
bin <- hexbin(rnorm(10000), rnorm(10000))
plot(bin, colramp = terrain.colors)


# ==================== 소스순번: 025 ==================== 

image(x, y, tab, main = "부모키와 자식키의 관계(밀도 기반)",
        xlab = "부모의 키", ylab = "자식의 키")
contour(x, y, tab, col = "pink", add = TRUE, method = "edge")


# ==================== 소스순번: 026 ==================== 

filled.contour(x, y, tab, color = terrain.colors,
                 plot.title = title(main = "부모키와 자식키의 관계",
                 xlab = "부모의 키", ylab = "자식의 키"),
                 key.title = title(main = "신장\n(10*inch)"),
                 key.axes = axis(4, seq(0, 50, by = 5)))


# ==================== 소스순번: 027 ==================== 

# car 패키지의 로드, 없으면 설치 후 로드
if (!require(car)) {
    install.packages("car")
    require(car)
  }

plot(axis.grid, cex = tab/15, col = "blue", pch = 19, xlab = "부모의 키",
       ylab = "자식의 키", main = "부모키와 자식키의 관계 (밀도 기반 + 타원체)")
dataEllipse(Galton$parent, Galton$child, plot.points = FALSE, levels = 0.2 * 1:4,
              ellipse.label = 0.2 * 1:4, lwd = 1, lty = 2, fill = TRUE,
              fill.alpha = 0.1)


# ==================== 소스순번: 028 ==================== 

# scatterplot with marginal histograms fuction
scatter.hist <- function(x, y, xlab = "", ylab = "", pch = 16, col = "black", cex = 1, hcol = "lightblue") {
      op <- par(no.readonly = TRUE)

      zones <- matrix(c(2, 0, 1, 3), ncol = 2, byrow = TRUE)
      layout(zones, widths = c(4/5, 1/5), heights = c(1/5, 4/5))

      xhist <- hist(x, plot = FALSE)
      yhist <- hist(y, plot = FALSE)

      par(mar = c(4, 4, 1, 1))
      plot(x, y, col = col, pch = pch, cex = cex, xlab = xlab, ylab = ylab)
      par(mar = c(0, 4, 1, 1))
      barplot(xhist$counts, axes = FALSE, space = 0, col = hcol)
      par(mar = c(4, 0, 1, 1))
      barplot(yhist$counts, axes = FALSE, space = 0, col = hcol, horiz = TRUE)

      par(op)
  }

with(mtcars, scatter.hist(hp, disp, pch = 16, col = "#0000FF", cex = 1.3,
       xlab = "마력(hp)", ylab = "배기량"))


# ==================== 소스순번: 029 ==================== 

par(mfrow = c(2, 2))
barplot(apply(Titanic, c(4, 1), sum), col = c("lightblue", "mistyrose"),
          main = "Survived over Economic status (class)")
legend("topleft", fill = c("lightblue", "mistyrose"), c("Death", "Survive"))
barplot(apply(Titanic, c(4, 2), sum), col = c("lightblue", "mistyrose"),
          main = "Survived over Sex")
legend("topright", fill = c("lightblue", "mistyrose"), c("Death",
         "Survive"))
barplot(apply(Titanic, c(4, 3), sum), col = c("lightblue", "mistyrose"),
          main = "Survived over Age")
legend("topleft", fill = c("lightblue", "mistyrose"), c("Death", "Survive"))
barplot(apply(Titanic, 4, sum), col = c("lightblue", "mistyrose"),
          main = "Survived")
par(mfrow = c(1, 1))


# ==================== 소스순번: 030 ==================== 

par(mfrow = c(2, 2))
spineplot(margin.table(Titanic, c(1, 4)), col = c("lightblue", "mistyrose"),
            main = "Survived over Economic status (class)")
spineplot(margin.table(Titanic, c(2, 4)), col = c("lightblue", "mistyrose"),
            main = "Survived over Sex")
spineplot(margin.table(Titanic, c(3, 4)), col = c("lightblue", "mistyrose"),
            main = "Survived over Age")
spineplot(t(as.matrix(margin.table(Titanic, 4))), col = c("lightblue", "mistyrose"),
            main = "Survived", xlab = "All Data", ylab = "Survived", xaxlabels = "Total")
par(mfrow = c(1, 1))


# ==================== 소스순번: 031 ==================== 

par(mfrow = c(2, 2))
assocplot(margin.table(Titanic, c(1, 4)), main = "Survived over Economic status (class)")
assocplot(margin.table(Titanic, c(2, 4)), main = "Survived over Sex")
assocplot(margin.table(Titanic, c(3, 4)), main = "Survived over Age")
par(mfrow = c(1, 1))


# ==================== 소스순번: 032 ==================== 

sub.data <- apply(Titanic, c(1, 2, 4), sum)[, 2, ]
sub.data
par(mfrow = c(1, 2))
spineplot(sub.data, col=c("lightblue", "mistyrose"),
            main="Survived over Economic status\n(Female, 3rd vs Crew)")
assocplot(sub.data, col=c("red", "blue"),
            main="Survived over Economic status\n(Female, 3rd vs Crew)")
par(mfrow = c(1, 1))


# ==================== 소스순번: 033 ==================== 

library(ggplot2)
p <- ggplot(iris, aes(x = Petal.Length, fill = Species))
p <- p + geom_histogram(binwidth = 0.2, alpha = 0.5, position = "identity")
p <- p + ggtitle("Histogram of petal length")
p + theme_bw()


# ==================== 소스순번: 034 ==================== 

library(ggplot2)
p <- ggplot(iris, aes(x = Petal.Length))
p <- p + geom_histogram(binwidth = 0.2, fill = "lightblue")
p <- p + facet_grid(. ~ Species)
p <- p + ggtitle("Histogram of petal length")
p + theme_bw()


# ==================== 소스순번: 035 ==================== 

plot(iris[, !names(iris) %in% c("Species")], main = "SPOLM of iris")


# ==================== 소스순번: 036 ==================== 

plot(iris[, !names(iris) %in% c("Species")], col = c(1:3)[iris$Species],
      pch = c(16:18)[iris$Species], main = "SPOLM of iris")
mtext(levels(iris$Species), line = 1, adj = c(0.2, 0.5, 0.7), col = 1:3)


# ==================== 소스순번: 037 ==================== 

library(lattice)
splom(~iris[1:4] | Species, data = iris, layout = c(2, 2), pscales = 0,
        varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength",
        "Petal\nWidth"))


# ==================== 소스순번: 038 ==================== 

# psych 패키지의 로드, 없으면 설치 후 로드
if (!require(psych)) {
    install.packages("psych")
    require(psych)
  }

pairs.panels(iris[1:4], bg = c("red", "yellow", "blue")[iris$Species],
               pch = 21, main = "Fisher Iris data by Species")


# ==================== 소스순번: 039 ==================== 

library(lattice)
parallelplot(~iris[1:4], iris, groups = Species, auto.key = list(text = levels(iris$Species),
               space = "top", columns = 3, lines = TRUE), main = "Parallel Coordinate Chart",
               horizontal.axis = FALSE, scales = list(x = list(rot = 90)))


# ==================== 소스순번: 040 ==================== 

library(lattice)
parallelplot(~iris[1:4] | Species, iris)


# ==================== 소스순번: 041 ==================== 

data <- c(apply(Titanic[dimnames(Titanic)[[1]] != "Crew", , , ], 2:4, sum),
            Titanic[dimnames(Titanic)[[1]] == "Crew", , , ])
dim(data) <- c(2, 2, 2, 2)
dimnames(data) <- list(Sex = c("Male", "Female"), Age = c("Child", "Adult"),
           Survived = c("No", "Yes"), Class = c("Passenger", "Crew"))
mosaicplot(apply(data, c(4, 3), sum),
             main = "Titanic Mosaic Survived over Class & Sex",
             col = hcl(c(240, 120)), off = c(5, 5, 5))


# ==================== 소스순번: 042 ==================== 

mosaicplot(apply(data, c(1, 4, 3), sum),
             main = "Titanic Mosaic Survived over Class & Sex",
             col = hcl(c(240, 120)), off = c(5, 5, 5))

