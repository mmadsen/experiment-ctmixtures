#
# Create example of separable and overlapping distributions for ctmixtures paper
#
library(mixtools)
library(mvtnorm) 
library(ggplot2)
library(ggthemes)
library(mmadsenr)

# Strongly overlapping example
p1 <- rmvnorm(500, c(2500, 200), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p1_df <- as.data.frame(p1)
p1_df$class <- "Model 1"

p4 <- rmvnorm(500, c(2750, 180), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p4_df <- as.data.frame(p4)
p4_df$class <- "Model 2"

combined_df <- rbind(p1_df, p4_df)
overlapping3 <- ggplot(combined_df, aes(x = V1, y = V2)) + ggtitle("(C)") + ylim(-100,450)
overlapping3 <- overlapping3 + geom_point(aes(color = class)) + xlab("Predictor 1") + ylab("Predictor 2") + theme_hc() + scale_colour_hc() 
overlapping3 <- overlapping3 + theme(axis.title.y = element_blank()) + labs(color = "Model") + guides(color = FALSE)


# Less overlapping example
p3 <- rmvnorm(500, c(2500, 275), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p3_df <- as.data.frame(p3)
p3_df$class <- "Model 1"

p4 <- rmvnorm(500, c(1200, 150), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p4_df <- as.data.frame(p4)
p4_df$class <- "Model 2"



combined_2_df <- rbind(p3_df, p4_df)
overlapping2 <- ggplot(combined_2_df, aes(x = V1, y = V2)) + ggtitle("(B)") + ylim(-100,450)
overlapping2 <- overlapping2 + geom_point(aes(color = class)) + xlab("Predictor 1") + theme_hc() + scale_colour_hc() + labs(color = "Model")
overlapping2 <- overlapping2 + theme(axis.title.y = element_blank()) + guides(color = FALSE)




# Separable example
p5 <- rmvnorm(500, c(6500, 200), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p5_df <- as.data.frame(p5)
p5_df$class <- "Model 1"

p6 <- rmvnorm(500, c(1500, 100), matrix(c(1000^2, 220^2, 220^2, 60^2),2,2))
p6_df <- as.data.frame(p6)
p6_df$class <- "Model 2"



combined_3_df <- rbind(p5_df, p6_df)
nonoverlapping <- ggplot(combined_3_df, aes(x = V1, y = V2)) + ggtitle("(A)") + ylim(-100,450)
nonoverlapping <- nonoverlapping + geom_point(aes(color = class)) + xlab("Predictor 1") + ylab("Predictor 2") + theme_hc() + scale_colour_hc()+ labs(color = "Model")
nonoverlapping <- nonoverlapping + guides(color = FALSE)


arrange_ggplot2(nonoverlapping, overlapping2, overlapping3)
pdf(file = "../paper/figure/generated/distributional-overlap.pdf", width = 8, height = 8 )
arrange_ggplot2(nonoverlapping, overlapping2, overlapping3)
dev.off()