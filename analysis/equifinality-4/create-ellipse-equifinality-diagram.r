#
# Create example of separable and overlapping distributions for ctmixtures paper
#
library(mixtools)
library(mvtnorm) 
library(ggplot2)
library(ggthemes)
library(mmadsenr)
library(MASS)
library(cluster)

p1 <- rmvnorm(1000, c(1000, 200), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p1_df <- as.data.frame(p1)

p4 <- rmvnorm(1000, c(3000, 100), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p4_df <- as.data.frame(p4)

p3 <- rmvnorm(1000, c(2000, 150), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p3_df <- as.data.frame(p3)


data_1_4 <- rbind(p1_df, p4_df)
data_1_4.mat <- as.matrix(data_1_4)

data_1_3 <- rbind(p1_df, p3_df)
data_1_3.mat <- as.matrix(data_1_3)


# Finding the 75% highest density / minimum volume ellipse
fit <- cov.mve(p1, quantile.used = nrow(p1) * 0.95)
ellipse_1 <- predict(ellipsoidhull(p1[fit$best, ]))

fit2 <- cov.mve(p4, quantile.used = nrow(p4) * 0.95)
ellipse_4 <- predict(ellipsoidhull(p4[fit2$best,]))

fit3 <- cov.mve(p3, quantile.used = nrow(p3) * 0.95)
ellipse_3 <- predict(ellipsoidhull(p3[fit2$best,]))

# Plotting it
plot(data_1_4, col = rgb(0, 0, 0, alpha = 0.3))
lines(ellipse_1, col="lightgreen", lwd=3)
lines(ellipse_4, col="lightblue", lwd=3)
legend("topleft", "95%", lty = 1, lwd = 3)


plot(data_1_3, col = rgb(0,0,0,alpha = 0.3))
lines(ellipse_1, col="lightgreen", lwd=3)
lines(ellipse_3, col="lightblue", lwd=3)
legend("topleft", "95%", lty = 1, lwd = 3)




