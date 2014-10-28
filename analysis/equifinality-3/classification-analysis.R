
## ----, echo=FALSE, results='asis', warning=FALSE, message=FALSE----------
library(dplyr)
# produce pandoc tables and output from R objects
library(pander)
library(knitr)
library(ggplot2)
library(caret)
# The following imports helper methods for random forests and data set splits
library(mmadsenr)
# load data frame, results in object "eq3_pop_df" in the workspace
load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/equifinality-3-population-data.rda")  
load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/equifinality-3-sampled-data.rda")



## ----two-model, echo=FALSE, results='asis', cache=TRUE, message=FALSE----
# create a label combining the biased models into one
eq3_pop_df$two_class_label <- factor(ifelse(eq3_pop_df$model_class_label == 'allneutral', 'neutral', 'biased'))

# exclude the original four class labels from the random forest classifier
excluded_fields <- c("simulation_run_id", "model_class_label", "innovation_rate")
two_model <- do_binary_random_forest_roc(eq3_pop_df, "two_class_label", excluded_fields, numtrees=1000)

two_fit <- two_model["fit"]
test_error <- two_model["prediction_rate"]
test_table <- two_model["test_confusion"]
roc <- two_model["roc"]



## ----two-confusion, results='asis', echo=FALSE, message=FALSE------------
pander(two_fit$fit["confusion"])


## ----two-importance, results='asis', echo=FALSE, message=FALSE-----------
pander(two_fit$fit["importance"], col.names = c('Variable', 'Mean Decrease Gini Index'))


## ----two-roc, results='asis', echo=FALSE, message=FALSE------------------
plot_roc(two_model$roc)



