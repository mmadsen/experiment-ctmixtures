# create data
set.seed(20130226)
n <- 200
x1 <- rnorm(n, mean = 2)
y1 <- 1.5 + 0.4 * x1 + rnorm(n)
x2 <- rnorm(n, mean = -1)
y2 <- 3.5 - 1.2 * x2 + rnorm(n)
class <- rep(c("A", "B"), each = n)
df <- data.frame(x = c(x1, x2), y = c(y1, y2), colour = class)

# get code for "stat_ellipse"
library(devtools)
library(ggplot2)
library(mmadsenr)
# scatterplot with confidence ellipses (but inner ellipse areas are not filled)
qplot(data = df, x = x, y = y, colour = class) + stat_ellipse(geom="polygon", alpha = 0.5, aes(fill = class))