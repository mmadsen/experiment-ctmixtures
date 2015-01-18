library(caret)
library(mmadsenr)
library(xtable)
library(ggthemes)
getOption('xtable.comment',FALSE)

results_files <- c(
  "balancedbias-neutral-comparison-gbm.RData",
  "bias-model-comparisons-gbm.RData",
  "classification-combined-tassize-combined_results-gbm.RData",
  "classification-population-results-gbm.RData",
  "classification-ta-sampled-results-gbm.RData",
  "per-locus-analysis-gbm.RData",
  "perlocus-tassize-results-gbm.RData",
  "classification-gbm-merged-dfonly.RData",
  "biased-gbm-merged-dfonly.RData"
  )

for(file in results_files) {
  load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = file))
}

model_objects <- c(
  bias_results_model,
  biasneutral_results_model,
  biasneutral_tassize_model,
  combined_tassize_results_model,
  results_model,
  tassize_perlocus_model,
  tassize_subset_model,
  tassize_biased_model,
  perlocus_results_model
)

cm_objects <- c(
  bias_results_cm,
  biasneutral_results_cm,
  biasneutral_tassize_cm,
  combined_tassize_results_cm,
  results_cm,
  tassize_perlocus_cm,
  tassize_subset_cm,
  tassize_biased_cm,
  perlocus_results_cm
)


## Capture Variable Importance ##

var_importance_list <- NULL
m_names <- names(model_objects)
for(name in m_names) {
  print(name)
  var_importance_list[[name]] <- get_sorted_variable_importance(model_objects[[name]])
}

# save objects from the environment
image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "varimp-merged-gbm.RData")
save(var_importance_list, file=image_file)


## Formatted Confusion Matrices ##

formatted_cm_list <- NULL
m_names <- names(cm_objects)
for(name in m_names) {
  print(name)
  formatted_cm_list[[name]] <- toLatex(xtable(cm_objects[[name]][["table"]],
                                      align="|c|c|c|"), comment=FALSE,floating=FALSE)
}

# save objects from the environment
image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "cm-formatted-gbm.RData")
save(formatted_cm_list, file=image_file)

image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "cm-merged-gbm.RData")
save(cm_objects, file=image_file)


###### Graphics #######

## Kappa plot for unbiased-versus-biased classification
kappaplot <- ggplot(classifier_results, aes(x = kappa, y = reorder(experiments, kappa)))
kappaplot <- kappaplot + geom_segment(aes(yend = experiments), xend = 0, size = 2, color = "grey50")
kappaplot <- kappaplot + ylab("Classification Experiment\n")
kappaplot <- kappaplot + xlab("\nCohen's Kappa")
kappaplot <- kappaplot + geom_point(size = 5, aes(color = exp_group)) + labs(color = "Data Collection Treatment")
kappaplot <- kappaplot + theme_pander()
kappaplot <- kappaplot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank())
kappaplot <- kappaplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")
kappaplot
ggsave(kappaplot, file="../paper/figure/unbiased-biased-kappa-dotchart.pdf", width=11, height=8.5, units="in")


## Kappa plot for neutral vs. balanced bias comparison 

balbias_neutral_df <- subset(full_bias_results, exp_group == "Mixed Bias/Neutrality Comparison")
bbn_kappaplot <- ggplot(balbias_neutral_df, aes(x = kappa, y = reorder(experiments, kappa)))
bbn_kappaplot <- bbn_kappaplot + geom_segment(aes(yend = experiments), xend = 0, size = 2, color = "grey50")
bbn_kappaplot <- bbn_kappaplot + ylab("Classification Experiment\n")
bbn_kappaplot <- bbn_kappaplot + xlab("\nCohen's Kappa")
bbn_kappaplot <- bbn_kappaplot + geom_point(size = 5, aes(color = exp_group)) + labs(color = "Data Collection Treatment")
bbn_kappaplot <- bbn_kappaplot + theme_pander()
bbn_kappaplot <- bbn_kappaplot + theme(panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")
bbn_kappaplot <- bbn_kappaplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")
bbn_kappaplot
ggsave(bbn_kappaplot, file="../paper/figure/unbiased-balbiased-kappa-dotchart.pdf", width=11, height=8.5, units="in")

## Kappa plot for conformism versus anticonformism dominated comparison 

proanticonf_df <- subset(full_bias_results, exp_group == "Pro/Anti Conformism Comparison")
proanti_kappaplot <- ggplot(proanticonf_df, aes(x = kappa, y = reorder(experiments, kappa)))
proanti_kappaplot <- proanti_kappaplot + geom_segment(aes(yend = experiments), xend = 0, size = 2, color = "grey50")
proanti_kappaplot <- proanti_kappaplot + ylab("Classification Experiment\n")
proanti_kappaplot <- proanti_kappaplot + xlab("\nCohen's Kappa")
proanti_kappaplot <- proanti_kappaplot + geom_point(size = 5, aes(color = exp_group)) + labs(color = "Data Collection Treatment")
proanti_kappaplot <- proanti_kappaplot + theme_pander()
proanti_kappaplot <- proanti_kappaplot + theme(panel.grid.major.x = element_blank(),
                                       panel.grid.minor.x = element_blank(),
                                       strip.background = element_blank(), strip.text = element_blank(), legend.position = "none")
proanti_kappaplot <- proanti_kappaplot + facet_grid(exp_group ~ ., scales = "free_y", space = "free_y")
proanti_kappaplot
ggsave(proanti_kappaplot, file="../paper/figure/proanticomparison-kappa-dotchart.pdf", width=11, height=8.5, units="in")











