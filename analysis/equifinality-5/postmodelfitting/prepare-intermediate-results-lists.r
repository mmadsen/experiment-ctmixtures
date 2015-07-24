library(caret)
library(mmadsenr)
library(xtable)
library(ggthemes)
getOption('xtable.comment',FALSE)

results_files <- c(
  "classification-perlocus-results-gbm.RData",
  "classification-population-results-gbm.RData",
  "classification-ta-sampled-results-gbm.RData",
  "combined-ta-sampled-results-gbm.RData",
  "perlocus-ta-sampled-results-gbm.RData"
)

for(file in results_files) {
  load(get_data_path(suffix = "experiment-ctmixtures/equifinality-5/fittedmodels", filename = file))
}

model_objects <- c(
  perlocus_neutral_anticonformist_model,
  perlocus_neutral_biased_model,
  perlocus_neutral_conformist_model,
  perlocus_pop_model,
  pop_results_model,
  tassize_neutral_biased_model,
  tassize_neutral_anticonformist_model,
  tassize_neutral_conformist_model,
  combined_neutral_anticonformist_model,
  combined_neutral_biased_model,
  combined_neutral_conformist_model)
  

cm_objects <- c(
  combined_neutral_anticonformist_cm,
  combined_neutral_biased_cm,
  combined_neutral_conformist_cm,
  perlocus_neutral_anticonformist_cm,
  perlocus_neutral_biased_cm,
  perlocus_neutral_conformist_cm,
  perlocus_pop_cm,
  pop_results_cm,
  tassize_neutral_biased_cm,
  tassize_neutral_anticonformist_cm,
  tassize_neutral_conformist_cm)

## Capture Variable Importance ##

var_importance_list <- NULL


for(model in names(model_objects)) {
  print(model)
  if('tunedmodel' %in% names(model_objects[[model]])) {
    print("tunedmodel present")
    var_importance_list[[model]] <- get_sorted_variable_importance(model_objects[[model]][['tunedmodel']])
  }
  else {
    print("no tunedmodel, using object directly")
    #var_importance_list[[name]] <- get_sorted_variable_importance(model_objects[[name]])
  }
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