library(randomForest)
library(dplyr)
library(ggplot2)
library(pander)
library(caret)
library(doMC)
library(mmadsenr)

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

# set up parallel processing - use all the cores (unless it's a dev laptop under OS X) - from mmadsenr
registerDoMC(cores = get_parallel_cores_given_os())

# load data frame, results in object "eq3_pop_df" in the workspace
# for dev, source the file "dev-Rprofile" first to set up a base data directory (yours may vary!)
#
pop_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-population-data.rda")
sampled_data_file <- get_data_path(suffix = "equifinality-3", filename = "equifinality-3-sampled-data.rda")

load(pop_data_file)
load(sampled_data_file)

# make this repeatable - comment this out or change it to get a fresh analysis result
set.seed(23581321)


# tuning grid of parameters and tuning cross-validation parameters
fit_grid <- expand.grid(mtry=seq(from=2,to=12,by=2))
fit_control <- trainControl(method="repeatedcv", 
                            number=10, 
                            repeats=10, 
                            allowParallel = TRUE,
                            ## Estimate class probabilities
                            classProbs = TRUE,
                            ## Evaluate performance using 
                            ## the following function
                            summaryFunction = twoClassSummary)

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

start_time <- proc.time()[["elapsed"]]
training_fit <- train(two_class_label ~ ., data = eq3_pop_nontest,
                        method="rf",
                        verbose=TRUE,
                        trControl = fit_control,
                        tuneGrid = fit_grid)
end_time <- proc.time()[["elapsed"]]
pop_training_minutes <- (end_time - start_time) / 60

cat("Training random forest for population data - elapsed time (min): ", pop_training_minutes)


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



start_time <- proc.time()[["elapsed"]]

sampled_training_fit <- train(two_class_label ~ ., data = eq3_sampled_nontest_20,
                      method="rf",
                      verbose=TRUE,
                      trControl = fit_control,
                      tuneGrid = fit_grid)

end_time <- proc.time()[["elapsed"]]
sampled_training_minutes <- (end_time - start_time) / 60

cat("Training random forest for sampled data - elapsed time (min): ", sampled_training_minutes)

# save objects from the environment

image_file <- get_data_path(suffix = "equifinality-3", filename = "classification-caret-results.RData")
save.image(image_file)




