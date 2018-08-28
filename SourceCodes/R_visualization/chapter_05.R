# -----------------------------------
# R visualization - 소스코드
#   출판사: 도서출판 인사이트
#   저자: 유충현, 홍성학
#   챕터: 5장
#   파일명: chapter_05.R
# -----------------------------------


# ==================== 소스순번: 001 ==================== 

trellis.par.get("background")$col         # (1)
trellis.par.get("fontsize")               # (2)

ltce.list <- trellis.par.get()            # (3)
is.list(ltce.list)                        # (4)
names(ltce.list)                          # (5)
names(ltce.list$fontsize)                 # (6)


# ==================== 소스순번: 002 ==================== 

# 방법 1
fontsize <- trellis.par.get("fontsize")
fontsize$points <- 9
trellis.par.set("fontsize", fontsize)
trellis.par.get("fontsize")

# 방법 2
trellis.par.set(list(fontsize = list(points = 10)))
trellis.par.get("fontsize")

# 방법 3
trellis.par.set(fontsize = list(points = 8))
trellis.par.get("fontsize")

trellis.par.set(theme = col.whitebg())      # (1)
trellis.par.get("background")$col
names(col.whitebg())


# ==================== 소스순번: 003 ==================== 

col.whitebg


# ==================== 소스순번: 004 ==================== 

trellis.device(theme = "col.whitebg")
show.settings()


# ==================== 소스순번: 005 ==================== 

trellis.device()
show.settings()


# ==================== 소스순번: 006 ==================== 

trellis.device(color = FALSE)
show.settings()


# ==================== 소스순번: 007 ==================== 

names(lattice.options())
names(lattice.getOption("layout.heights"))
lattice.getOption("save.object")
lattice.options(save.object = F)      # save.object 값을 변경함
lattice.getOption("save.object")      # save.object 값이 변경됨
lattice.options(save.object=T)          # save.object 값을 복원함
lattice.getOption("save.object")      # save.object 값이 변경됨


# ==================== 소스순번: 008 ==================== 

ls(pos = grep("lattice", search()), pat = "^panel\\.")


# ==================== 소스순번: 009 ==================== 

is.data.frame(ethanol)
dim(ethanol)
names(ethanol)
xyplot(NOx ~ E , data = ethanol, main = "Single Panel by xyplot ethanol")


# ==================== 소스순번: 010 ==================== 

xyplot(NOx ~ E | C , data = ethanol, main = " NOx ~ E | C (ethanol)")


# ==================== 소스순번: 011 ==================== 

table(ethanol$C)



# ==================== 소스순번: 012 ==================== 

xyplot(NOx ~ E | C , data = ethanol, subset = C > 8,
         main = "NOx ~ E | C , data = ethanol, subset = C > 8")


# ==================== 소스순번: 013 ==================== 

table(ethanol$C[ethanol$C > 8])



# ==================== 소스순번: 014 ==================== 

ii <- ethanol$C > 8
xyplot(NOx[ii] ~ E[ii] | C[ii], data = ethanol)         # 방법 1
xyplot(NOx ~ E | C, data = ethanol[ethanol$C > 8, ])    # 방법 2


# ==================== 소스순번: 015 ==================== 

inter <- co.intervals(ethanol$C, number = 3, overlap = -1)
inter
setLevel <- function (x) {
      for (i in 1:nrow(inter)) {
          if (inter[i, 1] < x && x <= inter[i, 2]) {
              return (switch(as.character(i),
                             "1" = "Low",
                             "2" = "Middle",
                             "3" = "High"))
          }
      }
  }
grade <- apply(t(ethanol$C), 2, setLevel)
grade <- ordered(grade, levels = c("Low", "Middle", "High"))
new.ethanol <- data.frame(ethanol, grade)
new.ethanol[sample(1:88, 5),]
xyplot(NOx ~ E, data = new.ethanol, groups = grade,
         auto.key = list(space = "top", columns = 3))


# ==================== 소스순번: 016 ==================== 

lattice.getOption("drop.unused.levels")

xyplot(NOx ~ E | C , data = ethanol, subset = C > 8, drop.unused.levels = FALSE,
         main= "NOx ~ E | C , data = ethanol, subset=C > 8, drop.unused.levels = F")


# ==================== 소스순번: 017 ==================== 

is.data.frame(iris)
dim(iris)
names(iris)
trellis.par.set(theme = col.whitebg())
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
         data = iris, allow.multiple = TRUE, auto.key = list(x = .6, y = .7, corner = c(0, 0)),
         main = "allow.multiple = TRUE")


# ==================== 소스순번: 018 ==================== 

xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
         data = iris, allow.multiple = TRUE, outer = TRUE,
         main = "allow.multiple = TRUE, outer = TRUE")


# ==================== 소스순번: 019 ==================== 

xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
         data = iris, allow.multiple = FALSE,
         auto.key = list(x = .6, y =.7, corner = c(0, 0)),
         main = "allow.multiple = FALSE")


# ==================== 소스순번: 020 ==================== 

xyplot(NOx ~ E | C, data = ethanol,
         panel = function(x, y) {
             panel.grid(h = -1, v = 2)
             panel.xyplot(x, y, pch = 16, col = "blue")
             panel.loess(x, y, col = "red")},
         main = "User Defined panel function")


# ==================== 소스순번: 021 ==================== 

xyplot(NOx ~ E, data = new.ethanol, groups = grade,
         panel = "panel.superpose",
         panel.groups = "panel.linejoin", horiz = FALSE,
         key = list(lines = Rows(trellis.par.get("superpose.line"), 1:3),
                    text = list(lab = as.character(levels (new.ethanol$grade))),
                    columns = 3),
         main = "panel.groups = \"panel.linejoin\"")


# ==================== 소스순번: 022 ==================== 

xyplot(NOx ~ E | C, data = ethanol, aspect = 1/2, pch=16,
         main = "aspect = 1/2(vertical size/horizontal size)")


# ==================== 소스순번: 023 ==================== 

# (1) aspect = "iso" 인 경우
xyplot(NOx ~ E | C, data = ethanol, aspect = "iso",
         main = "aspect = \"iso\"")
# (2) aspect = diff(range(ethanol$NOx))/diff(range(ethanol$E)) 인 경우
xyplot(NOx ~ E | C,
         aspect=diff(range(ethanol$NOx))/diff(range(ethanol$E)),
         data = ethanol,
         main="aspect=diff(range(ethanol$NOx))/diff(range(ethanol$E))")


# ==================== 소스순번: 024 ==================== 

attributes(sunspot.year)

plot <- xyplot(sunspot.year ~ 1700:1988, xlab = "", type = "l",
                 scales = list(x = list(alternating = 2)),
                 main = "Yearly Sunspots")
print(plot, position = c(0, .3, 1, .9), more = TRUE)
print(update(plot, aspect = "xy", main = "", xlab = "Year"),
        position = c(0, 0, 1, .3))


# ==================== 소스순번: 025 ==================== 

dotplot(variety ~ yield | year * site, data = barley, layout = c(6, 2),
          xlab = "Barley Yield (bushels/acre) ", ylab = NULL,
          main = "variety ~ yield | year*site, layout = c(6,2)")
levels(barley$year)      # 첫 번째 조건부 변수의 수준
levels(barley$site)   # 두 번째 조건부 변수의 수준


# ==================== 소스순번: 026 ==================== 

packet <- outer(levels(barley$year), levels(barley$site), paste)
dim(packet) <- c(12, 1)
packet


# ==================== 소스순번: 027 ==================== 

variety.medians <- tapply(barley$yield, barley$variety, median)
variety.medians


# ==================== 소스순번: 028 ==================== 

xyplot(NOx ~ E | C, data = ethanol, between = list(x = c(0, 1), y = 2),
         main = "between = list(x = c(0, 1), y = 2)")


# ==================== 소스순번: 029 ==================== 

xyplot(NOx ~ E | C, data = ethanol, skip = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE), layout=c(3, 2),
         main = "skip = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)")


# ==================== 소스순번: 030 ==================== 

library(grid)
xyplot(Sepal.Length ~ Petal.Length | Species,
         data = iris, scales = "free", layout = c(2, 2),
         panel = function(x, y) {
             panel.xyplot(x, y)
             panel.lmline(x, y, col="red")},
         strip = function(which.given, which.panel, var.name, factor.levels,
                          bg = trellis.par.get("strip.background")$col[which.given], ...) {
             if (which.given == 1) {
                 grid.rect(gp = gpar(fill = "lightblue", col = "black"))
                 ltext(factor.levels[which.panel[which.given]],
                       x = .24, y = .5, adj = 1)
             }
             grid.rect(x = .26, width = 0.74, just = "left",
                       gp = gpar(fill = bg, col = "black"))
             ltext(paste("Correlation :",
                         round(cor(iris$Sepal.Length[iris$Species ==
                                                         factor.levels[which.panel[which.given]]],
                                   iris$Petal.Length[iris$Species==
                                                         factor.levels[which.panel[which.given]]]), 3)),
                   x = .28, y = .5, adj = 0)
         }, par.strip.text = list(lines = 1), main = "User defined strip function")


# ==================== 소스순번: 031 ==================== 

xyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
         main="par.strip.text=list(col=\"blue\", cex=1.5, font=3, lines=2)",
         par.strip.text=list(col = "blue", cex=1.5, font=3, lines=2))


# ==================== 소스순번: 032 ==================== 

trellis.par.set(theme = col.whitebg())
panel.linepchjoin <-
      function (x, y, fun = mean, horizontal = TRUE, lwd = reference.line$lwd,
                lty = reference.line$lty, col, col.line = reference.line$col,
                pch = reference.symbol$pch, ...) {
          x <- as.numeric(x)
          y <- as.numeric(y)

          if (!missing(col)) {
              if (missing(col.line))
                  col.line <- col
          }
          if (horizontal) {
              vals <- unique(sort(y))
              yy <- seq(along = vals)
              xx <- numeric(length(yy))
              for (i in yy) xx[i] <- fun(x[y == vals[i]])
              llines(xx, vals[yy], col = col.line, lty = lty)
              lpoints(xx, vals[yy], col = col.line, pch = pch)
          }
          else {
              vals <- unique(sort(x))
              xx <- seq(along = vals)
              yy <- numeric(length(xx))
              for (i in xx) yy[i] <- fun(y[x == vals[i]])
              lpoints(vals[xx], yy, col = col.line, pch = pch)
              llines(vals[xx], yy, col = col.line, lty = lty)
          }
      }

xyplot(NOx ~ E, data=new.ethanol, groups=grade,
         panel="panel.superpose",
         panel.groups="panel.linepchjoin", horiz=FALSE,
         xlab="Equivalence ratio",
         ylab="NOx (micrograms/J)",
         key=list(lines=list(
             lty=trellis.par.get("superpose.line")$lty[1:3],
             pch=trellis.par.get("superpose.symbol")$pch[1:3],
             col=trellis.par.get("superpose.line")$col[1:3], type="o"),
             text=list(lab=as.character(levels(new.ethanol$grade))),
             columns=3, title="Compression ratio", background="lightgreen",
             space="bottom", border=TRUE, cex.title=1.2, between=2,
             between.columns=3, divide=2),
         main = "User defined key legend")


# ==================== 소스순번: 033 ==================== 

trellis.par.set(theme = col.whitebg())
xyplot(NOx ~ E, data = new.ethanol, groups = grade,
         panel = "panel.superpose",
         panel.groups = "panel.linepchjoin", horiz = FALSE,
         xlab = "Equivalence ratio",
         ylab = "NOx (micrograms/J)",
         key = list(lines = list(lty=trellis.par.get("superpose.line")$lty[1:3],
                                 pch=trellis.par.get("superpose.symbol")$pch[1:3],
                                 col=trellis.par.get("superpose.line")$col[1:2], type="o"),
                    text=list(lab=as.character(levels(new.ethanol$grade))),
                    columns=3, title="Compression ratio", background="lightgreen",
                    x=0.8, y=0.1, corner=c(1,1), border=TRUE, cex.title=1.2, between=2,
                    between.columns=3, divide=2, rep=TRUE),
         main="User defined key legend")


# ==================== 소스순번: 034 ==================== 

trellis.par.set(theme = col.whitebg())
xyplot(NOx ~ E, data = new.ethanol, groups = grade,
         panel = "panel.superpose",
         panel.groups = "panel.linepchjoin", horiz = FALSE,
         xlab = "Equivalence ratio",
         ylab = "NOx (micrograms/J)",
         auto.key = TRUE, main = "auto.key = TRUE")


# ==================== 소스순번: 035 ==================== 

xyplot(NOx ~ E, data = new.ethanol, groups = grade,
         panel = "panel.superpose",
         panel.groups = "panel.linepchjoin", horiz = FALSE,
         xlab = "Equivalence ratio", ylab = "NOx (micrograms/J)",
         legend =
           list(bottom =
             list(fun = draw.key,
               args = list(key = list(space = "bottom",
                 lines = list(lty = trellis.par.get("superpose.line")$lty[1:3],
                   col=trellis.par.get("superpose.line")$col[1:3]),
                 text = list(lab = as.character(levels (new.ethanol$grade))),
                 columns = 3, title = "Compression ratio",
                 background = "lightgreen", border = TRUE,
                 cex.title = 1.2, between = 2, between.columns = 3))),
               right = list(fun = draw.key,
                 args = list(key = list(space = "right",
                   points = list(pch = trellis.par.get("superpose.symbol")$pch[1:3],
                     col = trellis.par.get("superpose.line")$col[1:3]),
                   text = list(lab = as.character(levels (new.ethanol$grade))),
                   columns = 1, title = "Compression\nratio\n",
                   background = "lightblue", border = TRUE, cex.title = 0.9)))),
        main = "legend = list(bottom, right)")


# ==================== 소스순번: 036 ==================== 

val2col <- function(x, col) {
      rng <- range(x, na.rm = TRUE)
      ncol <- length(col)
      id <- 1 + round( ncol * (x - rng[1]) / diff(rng) )
      id[id > ncol] <- ncol
      col[id]
  }

my.key <- list(col = heat.colors(100),
                 at = seq(from = min(quakes$depth),
                          to = max(quakes$depth), length = 101),
                 space = "right")

xyplot(quakes$lat ~ quakes$long, col = val2col(quakes$depth, col = heat.colors(100)),
         main = "legend, fun=draw.colorkey",
         legend = list(right = list(fun = draw.colorkey,
                                args = list(key = my.key)
                           )))


# ==================== 소스순번: 037 ==================== 

xyplot(NOx ~ E | C, data = ethanol,
         prepanel = function(x, y) prepanel.loess(x, y),
         panel = function(x, y) {
             panel.grid(h = -1, v = 2)
             panel.xyplot(x, y, pch = 16, col = "blue")
             panel.loess(x, y, col = "red")
         },
         aspect = "xy",
         main = "prepanel = function(x, y) prepanel.loess(x, y)")


# ==================== 소스순번: 038 ==================== 

xyplot(NOx ~ E, data = ethanol, groups = C,
         main = "Used subscripts",
         panel = function(x, y, subscripts, groups)
             ltext(x, y, subscripts, cex = 0.9,
                   col = match(ethanol$C, sort(unique(ethanol$C)))),
         key = list(space = "bottom",  border = TRUE,
                    rectangles = list(col=1:5), columns = 5,
                    text = list(lab = as.character(sort(unique(ethanol$C)))),
                    title = "Compression ratio", cex.title = 1.2,
                    background = "lightblue", between = 1))


# ==================== 소스순번: 039 ==================== 

xyplot(NOx ~ E | C, data = ethanol,
         layout = c(3, 1),
         page = function(n) {
             ltext(x = 1, y = 0.95, paste("Page", n), pos = 2, col = "red")
             #winDialog("ok", "Plot next page...") # 방법 1
             #readline()                           # 방법 2
             Sys.sleep(5)                          # 방법 3
         },
         main = "page function")


# ==================== 소스순번: 040 ==================== 

unlist(trellis.par.get("par.main.text"))
unlist(trellis.par.get("par.sub.text"))
unlist(trellis.par.get("par.xlab.text"))
unlist(trellis.par.get("par.ylab.text"))


# ==================== 소스순번: 041 ==================== 

xyplot(NOx ~ E | C, data = ethanol,
         main = list("Ethanol Data Example", cex = 1.8, col = "blue", font = 4),
         sub = list("NOx ~ E | C", cex = 1.2, col = "red", font = 3),
         xlab = list("Equivalence ratio", cex = 1, col = 4, font = 1),
         ylab = list("NOx Concentration of nitrogen oxides\n(micrograms/J)",
                     cex = 1, col = 3, font = 2))


# ==================== 소스순번: 042 ==================== 

packet <- outer(levels(barley$year), levels(barley$site), paste)
as.vector(aperm(packet, 1:2)) # (1) prem.cond = 1:2인 경우의 패킷 순서
as.vector(aperm(packet, 2:1))    # (2) prem.cond = 2:1인 경우의 패킷 순서
dotplot(variety ~ yield | year * site, data = barley, layout = c(6, 2),
          xlab = "Barley Yield (bushels/acre) ", ylab = NULL,
          perm.cond = c(2, 1),
          main = "variety ~ yield | year * site, perm.cond = c(2, 1)")


# ==================== 소스순번: 043 ==================== 

levels(barley$year)
order.year <- match(sort(levels(barley$year)), levels(barley$year))
order.year
levels(barley$site)
order.site <- match(sort(levels(barley$site)), levels(barley$site))
order.site
dotplot(variety ~ yield | year * site, data = barley, layout = c(6, 2),
          xlab = "Barley Yield (bushels/acre) ", ylab = NULL,
          index.cond = list(order.year, order.site),
          main = "index.cond = list(order.year, order.site)")


# ==================== 소스순번: 044 ==================== 

xyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
         layout = c(1, 3), scales = list(y = list(relation = "free")),
         panel = function(x ,y) {
             panel.xyplot(x, y)
             panel.grid(v = -1)
         },
         xlim = c(0.8, 7), ylim = list(c(4, 6), c(4.5, 7), c(5, 8)),
         main = "xlim = c(0.8, 7), ylim = list(c(4, 6), c(4.5, 7), c(5, 8))")


# ==================== 소스순번: 045 ==================== 

p.same <- xyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
                   layout = c(3, 1), scales = list(relation = "same"),
                   main = "scales = list(relation = \"same\")")
p.free <- xyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
                   layout = c(3, 1), scales = list(relation = "free"),
                   main = "scales = list(relation = \"free\")")
p.sliced <- xyplot(Sepal.Length ~ Petal.Length | Species, data = iris,
                     layout = c(3, 1), scales = list(relation = "sliced"),
                     main = "scales = list(relation = \"sliced\")")
is(p.same)
print(p.same, position = c(0, 0.66, 1, 1), more = TRUE)
print(p.free, position = c(0, 0.33, 1, 0.65), more = TRUE)
print(p.sliced, position = c(0, 0, 1, 0.32))


# ==================== 소스순번: 046 ==================== 

p1 <- xyplot(NOx ~ E | C, data = ethanol,
               scales = list(alternating = 1), main = "scales=list(alternating = 1)")
p2 <- xyplot(abs(rnorm(12)) ~ 1:12, type= "o",
               scales = list(
                   x = list(at = 1:12, labels = month.name, abbreviate = T,
                            col = "red", rot = 45, font = 4, tck = 2, cex = 1.2),
                   y = list(tick.number = 3, log = T, axs = "i", col = "blue")),
               main = "scales parameters")
print(p1, position = c(0, 0.5, 1, 1), more = TRUE)
print(p2, position = c(0, 0, 1, 0.49))


# ==================== 소스순번: 047 ==================== 

barchart(yield ~ variety | site, data = barley,
           groups = year, layout = c(1,6),
           auto.key = list(points = FALSE, rectangles = TRUE,
                           columns = 2, space = "bottom"),
           ylab = "Barley Yield (bushels/acre)",
           main = "barchart",
           scales = list(x = list(rot = 45)))


# ==================== 소스순번: 048 ==================== 

barchart(yield ~ variety | site, data = barley,
           groups = year, layout = c(1,6), box.ratio = 1,
           auto.key = list(points = FALSE, rectangles = TRUE,
                           columns = 2, space = "bottom"),
           ylab = "Barley Yield (bushels/acre)",
           scales = list(x = list(abbreviate = TRUE,
                                  minlength = 5)),
           stack = TRUE, main = "barchart")


# ==================== 소스순번: 049 ==================== 

dim(singer)
dimnames(singer)[[2]]
levels(singer$voice.part)
bwplot(voice.part ~ height, data = singer, xlab = "Height (inches)",
         main = "bwplot(voice.part ~ height, data = singer)")


# ==================== 소스순번: 050 ==================== 

densityplot( ~ height | voice.part, data = singer,
              layout = c(2, 4), n = 10, plot.points = TRUE, ref = TRUE,
              xlab = "Height (inches)", bw = 5,
              main = "densityplot by n = 10, plot.points = T, ref = T, bw = 5")



# ==================== 소스순번: 051 ==================== 

trellis.par.set(theme = col.whitebg())
dotplot(variety ~ yield | site, data = barley, groups = year,
          key = simpleKey(levels(barley$year), space = "bottom", columns = 2),
          xlab = "Barley Yield (bushels/acre) ", ylab = "Variety",
          aspect=0.5, layout = c(2, 3),
          scales = list(alternating = FALSE,
                        y = list(abbreviate = TRUE, minlength = 5),
                        font = 4, col = "blue"),
          main = "dotplot(variety ~ yield | site, data = barley, groups = year)",
          sub = list("arguments = key, aspect, scales, etc", col = "gray")
  )


# ==================== 소스순번: 052 ==================== 

hist1 <- histogram( ~ height | voice.part, data = singer, nint = 7,
                     endpoints = c(59.5, 76.5), layout = c(2, 4), aspect = 1,
                     xlab = "Height (inches)",
                     main = "layout = c(2,4),\nnint = 7, endpoints = c(59.5, 76.5)")
hist2 <- histogram( ~ height | voice.part, data = singer,
                     breaks = do.breaks(c(59.5, 76.5), 7), layout = c(2, 4), aspect = 1,
                     xlab = "Height (inches)",
                     main = "layout = c(2,4),\nbreaks = do.breaks(c(59.5, 76.5), 7) ")
print(hist1, position = c(0, 0, 0.5, 1), more = TRUE)
print(hist2, position = c(0.5, 0, 1, 1), more = FALSE)


# ==================== 소스순번: 053 ==================== 

histogram( ~ height | voice.part, data = singer,
            xlab = "Height (inches)", type = "density",
            layout = c(2, 4), aspect = 2/3,
            panel = function(x, ...) {
                panel.histogram(x, ...)
                panel.mathdensity(dmath = dnorm, col = "blue",
                                  args = list(mean = mean(x), sd = sd(x))) },
            main = "type = \"density\" & User defined panel function" )


# ==================== 소스순번: 054 ==================== 

qqmath(~ height | voice.part, aspect = 1, data = singer,
         prepanel = prepanel.qqmathline, layout = c(2, 4),
         panel = function(x, ...) {
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
         }
  )


# ==================== 소스순번: 055 ==================== 

set.seed(1)
rnorm.20 <- rnorm(20)                                 # (1) 20개의 정규 난수 발생
q1 <- qqmath(~ rnorm.20, main = "~rnorm(20)")         # (2)
q2 <- qqmath(~ rnorm.20, f.value = ppoints,           # (3)
               main = "~rnorm(20), f.value = ppoints")
ppoints(20)                                           # (4) ppoints 사용 예시
qnorm.20 <- qnorm(ppoints(20))                        # (5) 정규분포의 분위수 생성
q3 <- qqmath(~ qnorm.20, main = "qnorm(ppoints(20))") # (6)
q4 <- qqmath(~ qnorm.20, f.value = ppoints,           # (7)
               main = "qnorm(ppoints(20)), f.value = ppoints")
print(q1, split = c(1, 1, 2, 2), more = TRUE)
print(q2, split = c(2, 1, 2, 2), more = TRUE)
print(q3, split = c(1, 2, 2, 2), more = TRUE)
print(q4, split = c(2, 2, 2, 2), more = FALSE)


# ==================== 소스순번: 056 ==================== 

strip1 <- stripplot(voice.part ~ jitter(height), data = singer,
                      jitter = FALSE, xlab = "Height (inches)",
                      main = "jitter = FALSE, factor = 0.3")
strip2 <- stripplot(voice.part ~ jitter(height), data = singer,
                      jitter = TRUE, factor = 0.7, xlab = "Height (inches)",
                      main = "jitter = TRUE, factor = 0.3")
print(strip1, position = c(0, 0.5, 1, 1), more = TRUE)
print(strip2, position = c(0, 0, 1, 0.5), more = FALSE)


# ==================== 소스순번: 057 ==================== 

qq(voice.part ~ height, aspect = 1, data = singer,
     subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"),
     main = "Quantile-Quantile Plots of Two Samples")


# ==================== 소스순번: 058 ==================== 

nx <- dim(subset(singer, voice.part == "Bass 2"))[1]
nx
ny <-dim(subset(singer, voice.part == "Tenor 1"))[1]
ny
ppoints(max(nx, ny), a = 1)

dimnames(subset(singer, voice.part == "Tenor 1"))[1]

qq(voice.part ~ height, aspect = 1, data = singer,
     subset = (voice.part == "Bass 2" | voice.part == "Tenor 1"),
     panel = function(x, y, subscripts) {
         ltext(x, y, subscripts)
         llines(c(0, 100), c(0, 100)) },
     main = "Quantile-Quantile Plots of Two Samples")


# ==================== 소스순번: 059 ==================== 

# (1) 여러 과정을 거치는 방법
x <- 10 * 1:nrow(volcano)
y <- 10 * 1:ncol(volcano)
tmp <- outer(x, y, paste)
tmp <- strsplit(tmp, " ")
tmp <- matrix(as.numeric(unlist(tmp)), ncol = 2, byrow = TRUE)
tmp <- as.data.frame(tmp)
colnames(tmp) <- c("x", "y")
z <- matrix(volcano, ncol = 1)
my.volcano <- data.frame(tmp, z)
# (2) expand.grid() 함수 사용 방법
expand.grid.volcano <- data.frame(expand.grid(x, y), matrix(volcano, ncol = 1))
names(expand.grid.volcano) <- c("x", "y", "z")
# (3) reshape2 패키지의 melt() 함수 사용 방법
library(reshape2)
dimnames(volcano) <- list(x, y)
melt.volcano <-  melt(volcano)
names(melt.volcano) <- c("x", "y", "z")
# (4) 세 방법의 결과 비교
all(my.volcano == expand.grid.volcano & expand.grid.volcano == melt.volcano)
# 레벨 플롯 그리기
levelplot(z ~ x * y, my.volcano, cuts = 50, region = TRUE,
            col.regions = terrain.colors(100),
            colorkey = list(at = seq(90, 195, by = 5),
                            tick.number = 10),
            main = "The Topography of Maunga Whau",
            xlab = "Meters North", ylab = "Meters West")


# ==================== 소스순번: 060 ==================== 

contourplot(z ~ x * y, my.volcano, xlab = "", ylab = "",
              panel = function(x, y, z, subscripts) {
                  panel.contourplot(x, y, z, subscripts,
                                    label.style = "flat", contour = TRUE,
                                    col = terrain.colors(12)[2],
                                    at = seq(90, 200, by = 10),
                                    labels = list(seq(90, 200, by = 10), col="blue", cex = 0.8),
                                    col.regions = terrain.colors(12)[8], region =T)
                  panel.grid(h = 4, v = 4, col = "lightgray", lty = 2)
              },
              main = "The Topography of Maunga Whau")


# ==================== 소스순번: 061 ==================== 

attach(environmental)
ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation,
                 span = 1, degree = 2, parametric = c("radiation", "wind"))
w.marginal <- seq(min(wind), max(wind), length = 50)
t.marginal <- seq(min(temperature), max(temperature), length = 50)
r.marginal <- seq(min(radiation), max(radiation), length = 4)
wtr.marginal <- list(wind = w.marginal, temperature = t.marginal,
                       radiation = r.marginal)
grid <- expand.grid(wtr.marginal)
grid[, "fit"] <- c(predict(ozo.m, grid))
detach(environmental)
contourplot(fit ~ wind * temperature | radiation, data = grid,
              cuts = 10, region = TRUE,
              xlab = "Wind Speed (mph)",
              ylab = "Temperature (F)",
              main = "Cube Root Ozone (cube root ppb)")


# ==================== 소스순번: 062 ==================== 

cloud(Sepal.Length ~ Petal.Length * Petal.Width | Species, data = iris,
        screen = list(x = -90, y = 70), distance = .4, zoom = .6,
        scales = list(x = list(arrows = TRUE),
                      y = list(arrows = FALSE),
                      z = list(draw = FALSE), col = "blue" , lty = 2, cex = 0.9),
        main = "cloud")


# ==================== 소스순번: 063 ==================== 

g <- expand.grid(x = 1:10, y = 5:15, gr = 1:2)
g$z <- log((g$x^g$g + g$y^2) * g$gr)
wire1 <- wireframe(z ~ x * y, data = g, groups = gr,
                     scales = list(arrows = FALSE), cuts = 5,
                     drape = TRUE, colorkey = TRUE,
                     screen = list(z = 30, x = -60))
wire2 <- wireframe(z ~ x * y, data = g, groups = gr,
                     scales = list(arrows = TRUE),
                     drape = TRUE, colorkey = FALSE, shade = TRUE,
                     screen = list(y = 20, x = -60),
                     light.source = c(10,0,10))
wire3 <- wireframe(volcano, shade = TRUE,
                     aspect = c(61/87, 0.4),
                     light.source = c(10,0,10))
print(wire1, split = c(1,2,2,2), more = TRUE)
print(wire2, split = c(2,2,2,2), more = TRUE)
print(wire3, split = c(1,1,2,2), more = FALSE)


# ==================== 소스순번: 064 ==================== 

trellis.par.set(theme = col.whitebg())
super.sym <- trellis.par.get("superpose.symbol")
splom(~iris[1:4], groups = Species, data = iris,
        panel = panel.superpose,
        pscales = list(
            list(labels = c("4.0", "5.0", "6.0", "7.0", "8.0")),
            list(at = c(2, 3, 4)),
            list(limits = c(1, 10)),
            list(at = seq(0.5, 2.5, 0.5))),
        key = list(title = "Three Varieties of Iris",
                   columns = 3,
                   points = list(pch = super.sym$pch[1:3],
                                 col = super.sym$col[1:3]),
                   text = list(c("Setosa", "Versicolor", "Virginica"))))


# ==================== 소스순번: 065 ==================== 

splom(~iris[1:3]|Species, data = iris,
        layout=c(2,2), pscales = 0,
        varnames = c("Sepal\nLength", "Sepal\nWidth", "Petal\nLength"),
        page = function(...) {
            ltext(x = seq(.6, .8, len = 4),
                  y = seq(.9, .6, len = 4),
                  lab = c("Three", "Varieties", "of", "Iris"),
                  cex = 2) },
        main = "~iris[1:3]|Species")


# ==================== 소스순번: 066 ==================== 

parallelplot(~iris[1:4] | Species, iris, between = list(x = 1, y = 0.5),
               main = "Parallel Coordinate Plots")


# ==================== 소스순번: 067 ==================== 

rfs(oneway(height ~ voice.part, data = singer, spread = 1), layout = c(1, 2),
      aspect = 1, main = "rfs(oneway(height ~ voice.part, data = singer, spread = 1)")


# ==================== 소스순번: 068 ==================== 

obj <- densityplot( ~ height | voice.part, data = singer,
                     layout = c(2, 4), n = 10, plot.points = TRUE, ref = TRUE,
                     xlab = "Height (inches)", bw = 5,
                     main = "densityplot by n = 10, plot.points = T, ref = T, bw = 5")
obj                           # (1) 래티스 플롯을 출력함
str(obj,  list.len = 10)      # (2) obj 객체의 일부 구조를 보여 준다.
names(obj)                    # (3) obj 객체의 성분들
obj$call                      # (4) obj 객체를 만든 함수 호출을 보여 줌
densityplot(~height | voice.part, data = singer, layout = c(2,
    4), n = 10, plot.points = TRUE, ref = TRUE, xlab = "Height (inches)",
    bw = 5, main = "densityplot by n = 10, plot.points = T, ref = T, bw = 5")
obj$main                      # (5) obj 객체의 메인 타이틀
obj$main <- NULL              # (6) obj 객체의 메인 타이틀을 없앤다.
names(obj)                    # (7) obj 객체의 성분들
obj                           # (8) obj 객체를 출력함


# ==================== 소스순번: 069 ==================== 

p11 <- histogram( ~ height | voice.part, data = singer, xlab="Height")
p12 <- densityplot( ~ height | voice.part, data = singer, xlab = "Height")
print(p11, split = c(1,1,1,2), more = TRUE, panel.width = list(1, "inches"))
print(p12, split = c(1,2,1,2), panel.width = list(4, "cm"))


# ==================== 소스순번: 070 ==================== 

spots <- by(sunspots, gl(235, 12, lab = 1749:1983), mean)
time1 <- xyplot(spots ~ 1749:1983, xlab = "", type = "l",
                  scales = list(x = list(alternating = 2)),
                  main = "Average Yearly Sunspots")
print(time1, split = c(1, 1, 1, 2), more =TRUE)
time2 <- update(trellis.last.object(xlab = "Year"), aspect = "xy")
print(time2, split = c(1, 2, 1, 2), more =FALSE)
lattice.getOption("save.object")

