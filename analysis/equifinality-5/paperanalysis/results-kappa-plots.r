library(caret)
library(mmadsenr)
library(xtable)
library(ggthemes)
library(dplyr)
getOption('xtable.comment',FALSE)

load(get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = "classification-gbm-merged-dfonly.RData"))

###### Graphics #######

## Kappa plot for all results, ordered by kappa and grouped by data collection treatment
combined_plot <- ggplot(eq5_classifier_results, aes(x = kappa, y = reorder(experiments, kappa)))
combined_plot <- combined_plot + geom_segment(aes(yend = experiments), xend = 0, size = 1, color = "grey50")
combined_plot <- combined_plot + ylab("Classification Experiment\n")
combined_plot <- combined_plot + xlab("\nCohen's Kappa")
combined_plot <- combined_plot + geom_point(size = 3, aes(color = exp_group)) + labs(color = "Data Collection Treatment")
combined_plot <- combined_plot + theme_hc() + scale_colour_hc() + xlim(0,1)
combined_plot <- combined_plot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank(),
                               legend.position = "right")
combined_plot <- combined_plot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")
combined_plot
ggsave(combined_plot, file="../paper/figure/generated/eq5-allcomparisons-kappa-dotchart.pdf", width=11, height=8.5, units="in")


######################## Kappa plots for subsets  #############################


## Census results
tmp1 <- dplyr::filter(eq5_classifier_results, exp_group == 'Population Census')
tmp2 <- dplyr::filter(eq5_classifier_results, exp_group == 'Population Per-Locus Census')
eq5_census_results <- rbind(tmp1, tmp2)

census_plot <- ggplot(eq5_census_results, aes(x = kappa, y = reorder(experiments, kappa)))
census_plot <- census_plot + geom_segment(aes(yend = experiments), xend = 0, size = 1, color = "grey50")
census_plot <- census_plot + ylab("Classification Experiment\n")
census_plot <- census_plot + xlab("\nCohen's Kappa")
census_plot <- census_plot + geom_point(size = 3, aes(color = positive_label)) + labs(color = "Type of Bias Compared to Unbiased Copying")
census_plot <- census_plot + theme_hc() + scale_colour_hc() + xlim(0,1)
census_plot <- census_plot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank(),
                               legend.position = "right")
census_plot <- census_plot + facet_grid(positive_label ~ ., scales = "free_y", space = "free_y")
census_plot
ggsave(census_plot, file="../paper/figure/generated/eq5-census-kappa-dotchart.pdf", width=11, height=8.5, units="in")


## Census results
eq5_sta_results <- dplyr::filter(eq5_classifier_results, exp_group == 'Sampled and Time Averaged')

sta_plot <- ggplot(eq5_sta_results, aes(x = kappa, y = reorder(experiments, kappa)))
sta_plot <- sta_plot + geom_segment(aes(yend = experiments), xend = 0, size = 1, color = "grey50")
sta_plot <- sta_plot + ylab("Classification Experiment\n")
sta_plot <- sta_plot + xlab("\nCohen's Kappa")
sta_plot <- sta_plot + geom_point(size = 3, aes(color = positive_label)) + labs(color = "Type of Bias Compared to Unbiased Copying")
sta_plot <- sta_plot + theme_hc() + scale_colour_hc() + xlim(0,1)
sta_plot <- sta_plot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank(),
                               legend.position = "right")
sta_plot <- sta_plot + facet_grid(positive_label ~ ., scales = "free_y", space = "free_y")
sta_plot
ggsave(sta_plot, file="../paper/figure/generated/eq5-sta-kappa-dotchart.pdf", width=11, height=8.5, units="in")


