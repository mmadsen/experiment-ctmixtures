#
# Create example of separable and overlapping distributions for ctmixtures paper
#
library(mixtools)
library(mvtnorm) 
library(ggplot2)
library(ggthemes)
library(mmadsenr)




# Separable example
p5 <- rmvnorm(500, c(101, 105))
p5_df <- as.data.frame(p5)
p5_df$class <- "Model 1"

p6 <- rmvnorm(500, c(100, 100))
p6_df <- as.data.frame(p6)
p6_df$class <- "Model 2"



combined_3_df <- rbind(p5_df, p6_df)
nonoverlapping <- ggplot(combined_3_df, aes(x = V1, y = V2)) + ggtitle("A")
nonoverlapping <- nonoverlapping + geom_point(aes(color = class)) + xlab("Variable 1") + ylab("Variable 2") + theme_pander() 
nonoverlapping <- nonoverlapping + guides(color = FALSE)
nonoverlapping <- nonoverlapping + ylim(95,110) + xlim(95, 110)
nonoverlapping

variable1 <- ggplot(combined_3_df, aes(x = V1)) + ggtitle("B")
variable1 <- variable1 + geom_density(aes(color = class), size = 2) + xlab("Density of Variable 1") + theme_pander() + guides(color = FALSE)
variable1

variable2 <- ggplot(combined_3_df, aes(x = V2)) + ggtitle("C")
variable2 <- variable2 + geom_density(aes(color = class), size = 2) + xlab("Density of Variable 2") + theme_pander() + guides(color = FALSE)
variable2



pdf(file = "../paper/figure/equifinality-variable-effect.pdf", width = 15, height = 10 )
arrange_ggplot2(nonoverlapping, variable1, variable2)
dev.off()
