library(caret)
library(mmadsenr)
library(xtable)
library(ggthemes)
library(dplyr)
getOption('xtable.comment',FALSE)

load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4/results", filename = "varimp-merged-gbm.RData"))
load(get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = "predictor_variable_labels.Rdata"))


get_labeled_df <- function(df) {
  new_df <- merge(df, predictor_variable_labels, by.x = "Predictor", by.y = "variable") 
  new_df$Predictor <- NULL
  new_df <- arrange(new_df, desc(Importance))
  new_df
}




# loads var_importance_list, with 37 tables keyed by the name of the data collection treatment
# and the predictor variable labels, for producing a readable table



# population census
pop_varimp_labeled <- get_labeled_df(var_importance_list$population_census)
print(xtable(pop_varimp_labeled,
       align="|l|r|c|",digits = c(0,2,1)), include.rownames = FALSE, comment=FALSE,floating=FALSE,
file = "../paper/varimp_population_census_table.tex")



