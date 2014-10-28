
## ----, echo=FALSE, results='asis', warning=FALSE, message=FALSE----------
library(dplyr)
# produce pandoc tables and output from R objects
library(pander)
library(knitr)
library(ggplot2)
# The following imports helper methods for random forests and data set splits
library(mmadsenr)
# load data frame, results in object "eq3_pop_df" in the workspace
load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/equifinality-3-population-data.rda")  
load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/equifinality-3-sampled-data.rda")


## ----four-model-forest, results='asis', echo=FALSE, cache=TRUE, message=FALSE----
excluded_fields = c("simulation_run_id", "innovation_rate")

# run random forest with default metaparameters against the "model_class_label" field
four_full_model <- do_multiclass_random_forest(eq3_pop_df, "model_class_label", excluded_fields, numtrees=500)

test_error <- four_full_model["prediction_rate"]
four_fit <- four_full_model["fit"]



## ----four-model-test,  results='asis', echo=FALSE, message=FALSE---------
pander(four_fit$fit["confusion"])


## ----two-model, echo=FALSE, results='asis', cache=TRUE, message=FALSE----
# create a label combining the biased models into one
eq3_pop_df$two_class_label <- factor(ifelse(eq3_pop_df$model_class_label == 'allneutral', 'neutral', 'biased'))

# exclude the original four class labels from the random forest classifier
excluded_fields <- c("simulation_run_id", "model_class_label", "innovation_rate")
two_model <- do_binary_random_forest_roc(eq3_pop_df, "two_class_label", excluded_fields, numtrees=500)

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


## ----four-num-configs, results='asis', echo=FALSE, message=FALSE---------
ggplot(eq3_pop_df, aes(x = num_trait_configurations, y = ..density..)) + geom_density(aes(linetype=model_class_label, position="stack"))


## ----two-no-configs, results='asis', echo=FALSE, cache=TRUE, message=FALSE----

# exclude the original four class labels from the random forest classifier
excluded_fields <- c("simulation_run_id", "model_class_label", "innovation_rate", "configuration_slatkin", "num_trait_configurations")
two_locus_model <- do_binary_random_forest_roc(eq3_pop_df, "two_class_label", excluded_fields, numtrees=500)

two_locus_fit <- two_locus_model["fit"]
test_error <- two_locus_model["prediction_rate"]
test_table <- two_locus_model["test_confusion"]
roc <- two_locus_model["roc"]


## ----two-locus-confusion, results='asis', echo=FALSE, message=FALSE------
pander(two_locus_fit$fit["confusion"])


## ----two-roc-locus, results='asis', echo=FALSE, message=FALSE------------
plot_roc(two_locus_model$roc)


## ----low-innov-two-all, results='asis', echo=FALSE, message=FALSE--------
# subset of data with theta <= 1.0
low_innov_df <- filter(eq3_pop_df, innovation_rate <= 1.0)

# exclude the original four class labels from the random forest classifier
excluded_fields <- c("simulation_run_id", "model_class_label", "innovation_rate")
low_two_model <- do_binary_random_forest_roc(low_innov_df, "two_class_label", excluded_fields, numtrees=500)

low_two_class_fit <- low_two_model["fit"]
test_error <- low_two_model["prediction_rate"]
test_table <- low_two_model["test_confusion"]
roc <- low_two_model["roc"]



## ----low-two-confusion, results='asis', echo=FALSE, message=FALSE--------
pander(low_two_class_fit$fit["confusion"])


## ----two-roc-lowinnov, results='asis', echo=FALSE, message=FALSE---------
plot_roc(low_two_model$roc)


## ----four-model-sampled-forest, results='asis', echo=FALSE, cache=TRUE, message=FALSE----

eq3_sampled_10 <- filter(eq3_sampled_df, sample_size == 10)
eq3_sampled_20 <- filter(eq3_sampled_df, sample_size == 20)

excluded_fields = c("simulation_run_id", "innovation_rate", "sample_size")

# run random forest with default metaparameters against the "model_class_label" field -- sample size 10
four_sampled_10 <- do_multiclass_random_forest(eq3_sampled_10, "model_class_label", excluded_fields, numtrees=250)
four_sampled_20 <- do_multiclass_random_forest(eq3_sampled_20, "model_class_label", excluded_fields, numtrees=250)
#four_sampled_all <- do_multiclass_random_forest(eq3_sampled_df, "model_class_label", excluded_fields, numtrees=250)

test_error_10 <- four_sampled_10["prediction_rate"]
test_error_20 <- four_sampled_20["prediction_rate"]
#test_error_all <- four_sampled_all["prediction_rate"]
four_sampled_10_fit <- four_sampled_10["fit"]
four_sampled_20_fit <- four_sampled_20["fit"]
#four_sampled_all_fit <- four_sampled_all["fit"]




## ----four-sampled-confusion-10, results='asis', echo=FALSE, message=FALSE----
 pander(four_sampled_10_fit$fit["confusion"])


## ----four-sampled-confusion-20, results='asis', echo=FALSE, message=FALSE----
pander(four_sampled_20_fit$fit["confusion"])


## ----two-model-sampled, echo=FALSE, results='asis', cache=TRUE, message=FALSE----
# create a label combining the biased models into one
eq3_sampled_df$two_class_label <- factor(ifelse(eq3_sampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))

eq3_sampled_10 <- filter(eq3_sampled_df, sample_size == 10)
eq3_sampled_20 <- filter(eq3_sampled_df, sample_size == 20)

# exclude the original four class labels from the random forest classifier
excluded_fields <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size")
two_sampled_10 <- do_binary_random_forest_roc(eq3_sampled_10, "two_class_label", excluded_fields, numtrees=250)
two_sampled_20 <- do_binary_random_forest_roc(eq3_sampled_20, "two_class_label", excluded_fields, numtrees=250)

two_fit_10 <- two_sampled_10["fit"]
test_error_10 <- two_sampled_10["prediction_rate"]
test_table_10 <- two_fit_10["test_confusion"]

two_fit_20 <- two_sampled_20["fit"]
test_error_20 <- two_sampled_20["prediction_rate"]
test_table_20 <- two_fit_20["test_confusion"]



## ----two-sampled-confusion, results='asis', echo=FALSE, message=FALSE----
pander(two_fit_10$fit["confusion"])


## ----two-sampled-roc, results='asis', echo=FALSE, message=FALSE----------
plot_roc(two_sampled_10$roc)


## ----two-sampled-confusion-20, results='asis', echo=FALSE, message=FALSE----
pander(two_fit_20$fit["confusion"])


## ----two-sampled-roc-20, results='asis', echo=FALSE, message=FALSE-------
plot_roc(two_sampled_20$roc)


## ----two-sampled-locus-model, results='asis', echo=FALSE, cache=TRUE, message=FALSE----

# exclude the original four class labels from the random forest classifier
excluded_fields <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size", "config_slatkin_ssize", "num_configurations_ssize")

two_sampled_locus_model_10 <- do_binary_random_forest_roc(eq3_sampled_10, "two_class_label", excluded_fields, numtrees=250)
two_sampled_locus_model_20 <- do_binary_random_forest_roc(eq3_sampled_20, "two_class_label", excluded_fields, numtrees=250)

two_locus_fit_10 <- two_sampled_locus_model_10["fit"]
test_error_locus_10 <- two_sampled_locus_model_10["prediction_rate"]
test_table_locus_10 <- two_locus_fit_10["test_confusion"]

two_locus_fit_20 <- two_sampled_locus_model_20["fit"]
test_error_locus_20 <- two_sampled_locus_model_20["prediction_rate"]
test_table_locus_20 <- two_locus_fit_20["test_confusion"]




## ----two-sampled-locus-confusion-10, results='asis', echo=FALSE, message=FALSE----
pander(two_locus_fit_10$fit["confusion"])


## ----two-sampled-locus-roc-10, results='asis', echo=FALSE, message=FALSE----
plot_roc(two_sampled_locus_model_10$roc)


## ----two-sampled-locus-confusion-20, results='asis', echo=FALSE, message=FALSE----
pander(two_locus_fit_20$fit["confusion"])


## ----two-sampled-locus-roc-20, results='asis', echo=FALSE, message=FALSE----
plot_roc(two_sampled_locus_model_20$roc)


## ----two-sampled-locus-importance-20, results='asis', echo=FALSE, message=FALSE----
pander(two_locus_fit_20$fit["importance"], col.names = c('Variable', 'Mean Decrease Gini Index'))


## ----two-model-sampled-nominmax-model, results='asis', echo=FALSE, cache=TRUE, message=FALSE----



# exclude the original four class labels from the random forest classifier
excluded_fields <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size", "richness_locus_min", "richness_locus_max", "slatkin_locus_min", "slatkin_locus_max", "entropy_locus_min", "entropy_locus_max", "iqv_locus_min", 
                     "iqv_locus_max")

two_sampled_locus_model_10_nomaxmin <- do_binary_random_forest_roc(eq3_sampled_10, "two_class_label", excluded_fields, numtrees=250)
two_sampled_locus_model_20_nomaxmin <- do_binary_random_forest_roc(eq3_sampled_20, "two_class_label", excluded_fields, numtrees=250)

two_locus_fit_10_nomaxmin <- two_sampled_locus_model_10_nomaxmin["fit"]
test_error_locus_10_nomaxmin <- two_sampled_locus_model_10_nomaxmin["prediction_rate"]
test_table_locus_10_nomaxmin <- two_locus_fit_10_nomaxmin["test_confusion"]

two_locus_fit_20_nomaxmin <- two_sampled_locus_model_20_nomaxmin["fit"]
test_error_locus_20_nomaxmin <- two_sampled_locus_model_20_nomaxmin["prediction_rate"]
test_table_locus_20_nomaxmin <- two_locus_fit_20_nomaxmin["test_confusion"]




## ----two-sampled-locus-confusion-10_nomaxmin, results='asis', echo=FALSE, message=FALSE----
pander(two_locus_fit_10_nomaxmin$fit["confusion"])


## ----two-sampled-locus-roc-10_nomaxmin, results='asis', echo=FALSE, message=FALSE----
plot_roc(two_sampled_locus_model_10_nomaxmin$roc)


## ----two-sampled-locus-confusion-20_nomaxmin, results='asis', echo=FALSE, message=FALSE----
pander(two_locus_fit_20_nomaxmin$fit["confusion"])


## ----two-sampled-locus-roc-20_nomaxmin, results='asis', echo=FALSE, message=FALSE----
plot_roc(two_sampled_locus_model_20_nomaxmin$roc)


## ----two-sampled-locus-importance-20_nomaxmin, results='asis', echo=FALSE, message=FALSE----
pander(two_locus_fit_20_nomaxmin$fit["importance"], col.names = c('Variable', 'Mean Decrease Gini Index'))


## ----sessioninfo, results="asis", message=FALSE--------------------------
sessionInfo()


