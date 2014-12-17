
library(mmadsenr)

file_list <- c(
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


