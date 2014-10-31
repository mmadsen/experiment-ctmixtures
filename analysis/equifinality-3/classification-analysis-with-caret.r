library(randomForest)
library(dplyr)
library(ggplot2)
library(pander)
library(caret)
library(doMC)

# Train and tune random forest classifiers for each of the three data sets coming out of the experiment
# "equifinality-3", for binary analysis. 
#
# Assumes that data-preparation.r has previously loaded CSV files and created binary 
# data files, stored in a local data directory.  
#
# Reduces the four class data into a two class problem for basic classifier analysis, and it splits off
# a test data set, placing it in the environment.  
# 
# NOTE:  This analysis takes a LONG time to run, so it is kept separate from the Rmarkdown
# analysis script.  At the completion of the analysis, it saves an environment image, which
# other scripts, such as RMarkdown documents, can load, and access the fitted model results.  
#
# Only the model training and fitting is done in this script.  The test data set is not analyzed
# here, since it can be done quickly enough that I want to be working in RMarkdown to examine different
# options.  


# set up parallel processing
registerDoMC(cores = 4)


# load data frame, results in object "eq3_pop_df" in the workspace
load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/equifinality-3-population-data.rda")  
load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/equifinality-3-sampled-data.rda")

# make this repeatable - comment this out or change it to get a fresh analysis result
set.seed(23581321)

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_pop_df$two_class_label <- factor(ifelse(eq3_pop_df$model_class_label == 'allneutral', 'neutral', 'biased'))


# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude <- c("simulation_run_id", "model_class_label", "innovation_rate")
eq3_pop_drop <- eq3_pop_df[,!(names(eq3_pop_df) %in% exclude)]

nonTestIndex <- createDataPartition(eq3_pop_drop$two_class_label, p = .9,
                                  list = FALSE,
                                  times = 1)

eq3_pop_nontest <- eq3_pop_drop[ nonTestIndex,]
eq3_pop_test  <- eq3_pop_drop[-nonTestIndex,]

####### 

fit_control <- trainControl(method="repeatedcv", number=10, repeats=10)

training_fit <- train(two_class_label ~ ., data = eq3_pop_nontest,
                        method="rf",
                        verbose=TRUE,
                        trControl = fit_control)

########################### sampled data ##########################

# prepare data
# create a label combining the biased models into one
# then, split into training and test sets, with balanced samples for each of the binary classes
eq3_sampled_df$two_class_label <- factor(ifelse(eq3_sampled_df$model_class_label == 'allneutral', 'neutral', 'biased'))


## sample size 20 ##

eq3_sampled_20 <- filter(eq3_sampled_df, sample_size == 20)

# remove fields from analysis that aren't predictors, and the detailed label with 4 classes
exclude <- c("simulation_run_id", "model_class_label", "innovation_rate", "sample_size")
eq3_sampled_drop <- eq3_sampled_20[,!(names(eq3_sampled_20) %in% exclude)]

nonTestIndex <- createDataPartition(eq3_sampled_drop$two_class_label, p = .9,
                                    list = FALSE,
                                    times = 1)

eq3_sampled_nontest_20 <- eq3_sampled_drop[ nonTestIndex,]
eq3_sampled_test_20  <- eq3_sampled_drop[-nonTestIndex,]


####### 

fit_control_sampled <- trainControl(method="repeatedcv", number=10, repeats=10)

sampled_training_fit <- train(two_class_label ~ ., data = eq3_sampled_nontest_20,
                      method="rf",
                      verbose=TRUE,
                      Control = fit_control_sampled)




# save objects from the environment
save.image("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/classification-caret.RData")




