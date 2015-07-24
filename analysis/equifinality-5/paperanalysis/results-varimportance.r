library(caret)
library(mmadsenr)
library(xtable)
library(ggthemes)
library(dplyr)
getOption('xtable.comment',FALSE)

#load(get_data_path(suffix = "experiment-ctmixtures/equifinality-5/results", filename = "varimp-merged-gbm.RData"))
load(get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = "predictor_variable_labels.Rdata"))

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




get_labeled_df <- function(df) {
  new_df <- merge(df, predictor_variable_labels, by.x = "Predictor", by.y = "variable") 
  new_df$Predictor <- NULL
  new_df <- arrange(new_df, desc(Importance))
  new_df
}




# loads var_importance_list, with 37 tables keyed by the name of the data collection treatment
# and the predictor variable labels, for producing a readable table



# population census - all biased
pop_varimp_labeled_biased <- get_labeled_df(get_sorted_variable_importance(pop_results_model[['population_census_biased']]))
print(xtable(pop_varimp_labeled_biased,
       align="|l|r|c|",digits = c(0,2,1)), include.rownames = FALSE, comment=FALSE,floating=FALSE,
file = "../paper/varimp_population_census_allbiased_table.tex")


# population census - anticonformist
pop_varimp_labeled_anticonf <- get_labeled_df(get_sorted_variable_importance(pop_results_model[['population_census_anticonformist']]))
print(xtable(pop_varimp_labeled_anticonf,
             align="|l|r|c|",digits = c(0,2,1)), include.rownames = FALSE, comment=FALSE,floating=FALSE,
      file = "../paper/varimp_population_census_anticonf_table.tex")

# population census - anticonformist
pop_varimp_labeled_conf <- get_labeled_df(get_sorted_variable_importance(pop_results_model[['population_census_conformist']]))
print(xtable(pop_varimp_labeled_conf,
             align="|l|r|c|",digits = c(0,2,1)), include.rownames = FALSE, comment=FALSE,floating=FALSE,
      file = "../paper/varimp_population_census_conf_table.tex")



# # population census - anticonformist
# tassize_biased_ex <- get_labeled_df(get_sorted_variable_importance(tassize_neutral_biased_model[['Neutral vs Biased: Sample Size: 10 Duration: 100']][['tunedmodel']]))
# print(xtable(tassize_biased_ex,
#              align="|l|r|c|",digits = c(0,2,1)), include.rownames = FALSE, comment=FALSE,floating=FALSE,
#       file = "../paper/varimp_tassize_biased_example_table.tex")
