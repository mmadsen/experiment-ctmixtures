library(caret)
library(mmadsenr)
library(xtable)
library(ggthemes)
getOption('xtable.comment',FALSE)

load(get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = "classification-gbm-merged-dfonly.RData"))

###### Graphics #######

## Kappa plot for unbiased-versus-biased classification
kappaplot <- ggplot(eq5_classifier_results, aes(x = kappa, y = reorder(experiments, kappa)))
kappaplot <- kappaplot + geom_segment(aes(yend = experiments), xend = 0, size = 1, color = "grey50")
kappaplot <- kappaplot + ylab("Classification Experiment\n")
kappaplot <- kappaplot + xlab("\nCohen's Kappa")
kappaplot <- kappaplot + geom_point(size = 3, aes(color = exp_group)) + labs(color = "Data Collection Treatment")
kappaplot <- kappaplot + theme_hc() + scale_colour_hc() + xlim(0,1)
kappaplot <- kappaplot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank(),
                               legend.position = "right")
kappaplot <- kappaplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")
kappaplot
ggsave(kappaplot, file="../paper/figure/generated/eq5-allcomparisons-kappa-dotchart.pdf", width=11, height=8.5, units="in")













