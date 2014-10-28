library(randomForest)
library(dplyr)
library(ggplot2)
library(pander)
library(caret)

# load data frame, results in object "eq3_pop_df" in the workspace
load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/equifinality-3-population-data.rda")  
load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-3/equifinality-3-sampled-data.rda")

# make this repeatable
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

# now, peel off another 10% of the training data to get a validation set for parameter tuning

tuneIndex <- createDataPartition(eq3_pop_nontest$two_class_label, p = .9, list = FALSE, times = 1)
nontest[-tuneIndex]

eq3_pop_train <- eq3_pop_nontest[ tuneIndex, ]
eq3_pop_tuning <- eq3_pop_nontest[-tuneIndex,]


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

eq3_sampled_nontest <- eq3_sampled_drop[ nonTestIndex,]
eq3_sampled_test  <- eq3_sampled_drop[-nonTestIndex,]

# now, peel off another 10% of the training data to get a validation set for parameter tuning

tuneIndex <- createDataPartition(eq3_sampled_nontest$two_class_label, p = .9, list = FALSE, times = 1)


eq3_sampled_train <- eq3_sampled_nontest[ tuneIndex, ]
eq3_sampled_tuning <- eq3_sampled_nontest[-tuneIndex,]


####### 

fit_control_sampled <- trainControl(method="repeatedcv", number=10, repeats=10)

sampled_training_fit <- train(two_class_label ~ ., data = eq3_sampled_nontest,
                      method="rf",
                      verbose=TRUE,
                      Control = fit_control_sampled)




