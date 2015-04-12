#
# Create example of separable and overlapping distributions for ctmixtures paper
#
library(mixtools)
library(mvtnorm) 
library(ggplot2)
library(ggthemes)
library(mmadsenr)
library(gridExtra)




# Separable example
p5 <- rmvnorm(500, c(101, 103.5))
p5_df <- as.data.frame(p5)
p5_df$class <- "Model 1"

p6 <- rmvnorm(500, c(100, 101))
p6_df <- as.data.frame(p6)
p6_df$class <- "Model 2"



combined_3_df <- rbind(p5_df, p6_df)
nonoverlapping <- ggplot(combined_3_df, aes(x = V1, y = V2)) 
nonoverlapping <- nonoverlapping + geom_point(aes(color = class)) + xlab("Variable 1") + ylab("Variable 2") + theme_hc() + scale_colour_hc()
nonoverlapping <- nonoverlapping + guides(color = FALSE)
nonoverlapping <- nonoverlapping + ylim(95,110) + xlim(97, 105)


variable1 <- ggplot(combined_3_df, aes(x = V1)) 
variable1 <- variable1 + geom_density(aes(color = class), size = 1) + theme_hc() + scale_colour_hc()+ guides(color = FALSE) 
variable1 <- variable1 + theme(axis.text.x= element_blank(), axis.text.y = element_blank(), 
                               axis.title.x = element_blank(), axis.title.y = element_blank(),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank())


variable2 <- ggplot(combined_3_df, aes(x = V2)) 
variable2 <- variable2 + geom_density(aes(color = class), size = 1) + theme_hc() + scale_colour_hc()+ guides(color = FALSE) 
variable2 <- variable2 + theme(axis.text.x= element_blank(), axis.text.y = element_blank(), 
                               axis.title.x = element_blank(), axis.title.y = element_blank(),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank())
variable2 <- variable2 + coord_flip()



empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
       panel.background=element_blank(), 
       axis.text.x=element_blank(), axis.text.y=element_blank(),           
       axis.title.x=element_blank(), axis.title.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#grid.arrange(variable1, empty, nonoverlapping, variable2, ncol=2,nrow=2,widths=c(4, 1), heights=c(1, 4))


grid.arrange(variable1, empty, nonoverlapping, variable2, ncol=2,nrow=2,widths=c(4, 1), heights=c(1, 4))
pdf(file = "../paper/figure/generated/equifinality-variable-effect.pdf", width = 8, height = 8 )
grid.arrange(variable1, empty, nonoverlapping, variable2, ncol=2,nrow=2,widths=c(4, 1), heights=c(1, 4))
#arrange_ggplot2(nonoverlapping, variable1, variable2)
dev.off()

