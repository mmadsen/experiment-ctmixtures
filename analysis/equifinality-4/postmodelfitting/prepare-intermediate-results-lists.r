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
image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = "varimp-merged-gbm.RData")
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
image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = "cm-formatted-gbm.RData")
save(formatted_cm_list, file=image_file)

image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = "cm-merged-gbm.RData")
save(cm_objects, file=image_file)