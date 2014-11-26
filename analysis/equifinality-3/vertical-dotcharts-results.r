library(ggplot2)
library(ggthemes)


mcplot <- ggplot(classifier_results, aes(x = misclassification_rate, y = reorder(experiments, misclassification_rate)))
mcplot <- mcplot + geom_segment(aes(yend = experiments), xend = 0, color = "grey50")
mcplot <- mcplot + ylab("Classification Experiment")
mcplot <- mcplot + xlab("Misclassification Rate")
mcplot <- mcplot + geom_point(size = 3, aes(color = exp_group)) + labs(color = "Experiment Group")
mcplot <- mcplot + theme_pander()
mcplot <- mcplot + theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_blank(), strip.text = element_blank())
          #panel.grid.major.y = element_line(colour = "grey40", linetype = "dashed")
          #)
mcplot <- mcplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")

mcplot



kappaplot <- ggplot(classifier_results, aes(x = kappa, y = reorder(experiments, kappa)))
kappaplot <- kappaplot + geom_segment(aes(yend = experiments), xend = 0, color = "grey50")
kappaplot <- kappaplot + ylab("Classification Experiment")
kappaplot <- kappaplot + xlab("Cohen's Kappa")
kappaplot <- kappaplot + geom_point(size = 3, aes(color = exp_group)) + labs(color = "Experiment Group")
kappaplot <- kappaplot + theme_pander()
kappaplot <- kappaplot + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     strip.background = element_blank(), strip.text = element_blank())
#panel.grid.major.y = element_line(colour = "grey40", linetype = "dashed")
#)
kappaplot <- kappaplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")

kappaplot



aucplot <- ggplot(classifier_results, aes(x = auc, y = reorder(experiments, auc))) 
aucplot <- aucplot + geom_segment(aes(yend = experiments), xend = 0, color = "grey50")
aucplot <- aucplot + ylab("Classification Experiment")
aucplot <- aucplot + xlab("Area Under ROC Curve")
aucplot <- aucplot + xlim(0.5, 1.0)
aucplot <- aucplot + geom_point(size = 3, aes(color = exp_group)) + labs(color = "Experiment Group")
aucplot <- aucplot + theme_pander()
aucplot <- aucplot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank())
#panel.grid.major.y = element_line(colour = "grey40", linetype = "dashed")
#)
aucplot <- aucplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")

aucplot

fdrplot <- ggplot(classifier_results, aes(x = fdr, y = reorder(experiments, fdr))) 
fdrplot <- fdrplot + geom_segment(aes(yend = experiments), xend = 0, color = "grey50")
fdrplot <- fdrplot + ylab("Classification Experiment")
fdrplot <- fdrplot + xlab("False Discovery Rate")
fdrplot <- fdrplot + xlim(0.0, 1.0)
fdrplot <- fdrplot + geom_point(size = 3, aes(color = exp_group)) + labs(color = "Experiment Group")
fdrplot <- fdrplot + theme_pander()
fdrplot <- fdrplot + theme(panel.grid.major.x = element_blank(),
                           panel.grid.minor.x = element_blank(),
                           strip.background = element_blank(), strip.text = element_blank())
#panel.grid.major.y = element_line(colour = "grey40", linetype = "dashed")
#)
fdrplot <- fdrplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")

fdrplot


youdenplot <- ggplot(classifier_results, aes(x = youdensj, y = reorder(experiments, youdensj))) 
youdenplot <- youdenplot + geom_segment(aes(yend = experiments), xend = 0, color = "grey50")
youdenplot <- youdenplot + ylab("Classification Experiment")
youdenplot <- youdenplot + xlab("Youden's J")
youdenplot <- youdenplot + xlim(0.0, 1.0)
youdenplot <- youdenplot + geom_point(size = 3, aes(color = exp_group)) + labs(color = "Experiment Group")
youdenplot <- youdenplot + theme_pander()
youdenplot <- youdenplot + theme(panel.grid.major.x = element_blank(),
                           panel.grid.minor.x = element_blank(),
                           strip.background = element_blank(), strip.text = element_blank())
#panel.grid.major.y = element_line(colour = "grey40", linetype = "dashed")
#)
youdenplot <- youdenplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")

youdenplot


ssplot <- ggplot(classifier_results, aes(x = sensitivity, y = specificity))
ssplot <- ssplot + geom_point(size = 3, aes(color = exp_group)) + labs(color = "Experiment Group")
ssplot <- ssplot + xlab("Sensitivity (True Positive Rate)")
ssplot <- ssplot + ylab("Specificity (True Negative Rate)")
ssplot <- ssplot + xlim(0.9,1.0) + ylim(0.0,1.0)
ssplot <- ssplot + theme_pander()

ssplot






  
      