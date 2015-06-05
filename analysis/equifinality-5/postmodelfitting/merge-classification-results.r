#
# Combine the results of the two classification runs with gradient boosted models
# 
# The population and sampled classifications are in a data frame called "results"
# The tasampled classifications are in a data frame called "subsets" since they're subsets of the single large database
# The combined (non-subsetted) tasampled classification is in a data frame called "combined_results"
#
library(mmadsenr)
library(plyr)



results_files <- c(
"classification-perlocus-results-gbm-dfOnly.RData",
"classification-population-results-gbm-dfOnly.RData",
"classification-ta-sampled-results-dfonly.RData",
"combined-ta-sampled-results-gbm-dfonly.RData",
"perlocus-ta-sampled-results-gbm-dfonly.RData"
)




for(file in results_files) {
  load(get_data_path(suffix = "experiment-ctmixtures/equifinality-5/fittedmodels", filename = file))
}


# Step #1:  Add sample size and ta duration columns to the results data frame
pop_results$sample_size <- 0
pop_results$ta_duration <- 0  # this broadcasts 0 to the entire column
perlocus_results_df$sample_size <- 0
perlocus_results_df$ta_duration <- 0  # this broadcasts 0 to the entire column



# Add an "experiment group" to each df before merging, to be used in visually distinguishing the classes

pop_results$exp_group <- 'Population Census' 
perlocus_results_df$exp_group <- 'Population Per-Locus Census'

# And fix the problematic exp_group variables I created in the model fitting scripts, which 
# aren't sufficiently differentiating.
combined_neutral_anticonformist_results$exp_group <- 'Sampled and Time Averaged, Combined Duration'
combined_neutral_biased_results$exp_group <- 'Sampled and Time Averaged, Combined Duration'
combined_neutral_conformist_results$exp_group <- 'Sampled and Time Averaged, Combined Duration'
perlocus_neutral_anticonformist_results$exp_group <- 'Sampled and Time Averaged, Per-Locus'
perlocus_neutral_biased_results$exp_group <- 'Sampled and Time Averaged, Per-Locus'
perlocus_neutral_conformist_results$exp_group <- 'Sampled and Time Averaged, Per-Locus'
tassize_neutral_anticonformist_results$exp_group <- 'Sampled and Time Averaged'
tassize_neutral_biased_results$exp_group <- 'Sampled and Time Averaged'
tassize_neutral_conformist_results$exp_group <- 'Sampled and Time Averaged'




# now merge the two into a single dataframe
eq5_classifier_results <- rbind(
  combined_neutral_biased_results,
  combined_neutral_anticonformist_results,
  combined_neutral_conformist_results,
  perlocus_neutral_anticonformist_results,
  perlocus_neutral_biased_results,
  perlocus_neutral_conformist_results,
  perlocus_results_df,
  pop_results,
  tassize_neutral_anticonformist_results,
  tassize_neutral_biased_results,
  tassize_neutral_conformist_results
)

# Create descriptive versions of the positive_label which records which type
# of CT bias is compared to neutrality/unbiased in each classification

eq5_classifier_results$positive_label <- revalue(eq5_classifier_results$positive_label, c("biased"="Combined Conformist and Anticonformist",
                                                 "anticonformist"="Anticonformist",
                                                 "conformist"="Conformist"))


###### Add two useful statistics #######


eq5_classifier_results$fdr <- 1.0 - eq5_classifier_results$ppv
eq5_classifier_results$youdensj <- eq5_classifier_results$sensitivity + eq5_classifier_results$specificity - 1.0



############## Complete Processing and Save Results ##########3

# save objects from the environment
image_file <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = "classification-gbm-merged-dfonly.RData")
save(eq5_classifier_results, file=image_file)


