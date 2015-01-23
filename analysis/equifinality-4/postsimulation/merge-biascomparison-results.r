#
# Combine the results of the two classification runs with gradient boosted models
# 
# The population and sampled classifications are in a data frame called "results"
# The tasampled classifications are in a data frame called "subsets" since they're subsets of the single large database
# The combined (non-subsetted) tasampled classification is in a data frame called "combined_results"
#
library(mmadsenr)

load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "balancedbias-neutral-comparison-gbm-dfonly.RData"))
balanced_bias_results <- data.frame(bias_results)
rm(bias_results)

load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "bias-model-comparisons-gbm-dfonly.RData"))

balanced_bias_results$exp_group <- 'Mixed Bias/Neutrality Comparison'
bias_results$exp_group <- 'Pro/Anti Conformism Comparison'


full_bias_results <- rbind(balanced_bias_results, bias_results)


###### Add two useful statistics #######


full_bias_results$fdr <- 1.0 - full_bias_results$ppv
full_bias_results$youdensj <- full_bias_results$sensitivity + full_bias_results$specificity - 1.0



############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "biased-gbm-merged-dfonly.RData")
save(full_bias_results, file=image_file)


