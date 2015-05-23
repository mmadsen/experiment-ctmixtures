library(caret)
library(mmadsenr)
library(xtable)
library(ggthemes)
getOption('xtable.comment',FALSE)

load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "biased-gbm-merged-dfonly.RData"))
load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "classification-gbm-merged-dfonly.RData"))

###### Graphics #######

## Kappa plot for unbiased-versus-biased classification
kappaplot <- ggplot(classifier_results, aes(x = kappa, y = reorder(experiments, kappa)))
kappaplot <- kappaplot + geom_segment(aes(yend = experiments), xend = 0, size = 2, color = "grey50")
kappaplot <- kappaplot + ylab("Classification Experiment\n")
kappaplot <- kappaplot + xlab("\nCohen's Kappa")
kappaplot <- kappaplot + geom_point(size = 5, aes(color = exp_group)) + labs(color = "Data Collection Treatment")
kappaplot <- kappaplot + theme_hc() + scale_colour_hc() + xlim(0,1)
kappaplot <- kappaplot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank(),
                               legend.position = "right")
kappaplot <- kappaplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")
kappaplot
ggsave(kappaplot, file="../paper/figure/generated/unbiased-biased-kappa-dotchart.pdf", width=11, height=8.5, units="in")


## Kappa plot for neutral vs. balanced bias comparison 

balbias_neutral_df <- subset(full_bias_results, exp_group == "Mixed Bias/Neutrality Comparison")
bbn_kappaplot <- ggplot(balbias_neutral_df, aes(x = kappa, y = reorder(experiments, kappa)))
bbn_kappaplot <- bbn_kappaplot + geom_segment(aes(yend = experiments), xend = 0, size = 2, color = "grey50")
bbn_kappaplot <- bbn_kappaplot + ylab("Classification Experiment\n")
bbn_kappaplot <- bbn_kappaplot + xlab("\nCohen's Kappa")
bbn_kappaplot <- bbn_kappaplot + geom_point(size = 5, aes(color = exp_group)) + labs(color = "Data Collection Treatment")
bbn_kappaplot <- bbn_kappaplot + theme_hc() + scale_colour_hc() + xlim(0,1)
bbn_kappaplot <- bbn_kappaplot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")
bbn_kappaplot <- bbn_kappaplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")
bbn_kappaplot
ggsave(bbn_kappaplot, file="../paper/figure/generated/unbiased-balbiased-kappa-dotchart.pdf", width=8, height=4, units="in")

## Kappa plot for conformism versus anticonformism dominated comparison 

proanticonf_df <- subset(full_bias_results, exp_group == "Pro/Anti Conformism Comparison")
proanti_kappaplot <- ggplot(proanticonf_df, aes(x = kappa, y = reorder(experiments, kappa)))
proanti_kappaplot <- proanti_kappaplot + geom_segment(aes(yend = experiments), xend = 0, size = 2, color = "grey50")
proanti_kappaplot <- proanti_kappaplot + ylab("Classification Experiment\n")
proanti_kappaplot <- proanti_kappaplot + xlab("\nCohen's Kappa")
proanti_kappaplot <- proanti_kappaplot + geom_point(size = 5, aes(color = exp_group)) + labs(color = "Data Collection Treatment")
proanti_kappaplot <- proanti_kappaplot + theme_hc() + scale_colour_hc() + xlim(0,1)
proanti_kappaplot <- proanti_kappaplot + theme(panel.grid.major.x = element_blank(),
                                       panel.grid.minor.x = element_blank(),
                                       strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")
proanti_kappaplot <- proanti_kappaplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")
proanti_kappaplot
ggsave(proanti_kappaplot, file="../paper/figure/generated/proanticomparison-kappa-dotchart.pdf", width=8, height=4, units="in")











