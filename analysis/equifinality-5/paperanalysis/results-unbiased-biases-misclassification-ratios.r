library(caret)
library(mmadsenr)
library(xtable)
library(ggthemes)
library(dplyr)
getOption('xtable.comment',FALSE)

load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "biased-gbm-merged-dfonly.RData"))
load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "classification-gbm-merged-dfonly.RData"))
load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "cm-merged-gbm.RData"))
     
     
# fix a couple of annoying mismatches stemming from the model fitting scripts
tmp <- cm_objects[["population_census"]]
cm_objects[["Population Census"]] <- tmp
cm_objects[["population_census"]] <- NULL

tmp <- cm_objects[["perlocus_pop"]]
cm_objects[["Per-Locus Population Census"]] <- tmp
cm_objects[["perlocus_pop"]] <- NULL

tmp <- cm_objects[["combined_tassize"]]
cm_objects[["All Sample Sizes and TA Durations"]] <- tmp
cm_objects[["combined_tassize"]] <- NULL


c_names <- classifier_results$experiments

names <- NULL
ratios <- NULL
groups <- NULL
for(name in c_names) {
  print(name)
  names <- c(names, name)
  
  experiment_row <- subset(classifier_results, experiments == name)
  group <- experiment_row$exp_group
  
  groups <- c(groups, group)
  
  num <- cm_objects[[name]][["table"]][[3]]
  den <- cm_objects[[name]][["table"]][[4]]
  r <- (num / (num + den)) * 100.0
  print(sprintf("r: %f", r))
  
  ratios <- c(ratios, r)
}

#error_ratio_neutral <- as.data.frame(cbind(names,as.numeric(ratios)))
error_ratio_neutral <- data.frame(names,as.numeric(ratios))
names(error_ratio_neutral) <- c("Data Collection Treatment", "% Misclassified")


# reorder in ascending order of ratio
error_ratio_neutral <- dplyr::arrange(error_ratio_neutral, ratios)


filename <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", 
                            filename = "misclassification-ratio-neutral-biased.RData")
save(error_ratio_neutral, file = filename)

# now make a nice looking table

print(xtable(error_ratio_neutral,
      align="|l|l|c|",digits = c(0,0,1)), include.rownames = FALSE, comment=FALSE,floating=FALSE,
      file = "../paper/misclassification-percentage-neutral-data.tex")




