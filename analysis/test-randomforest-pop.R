library(randomForest)
library(miscTools)

source('data_preparation.r')

# we don't want to treat the sim run ID or innovation_rate as predictors...
drops <- c('simulation_run_id', 'innovation_rate')
dataset <- pop_df[,!(names(pop_df) %in% drops)]

# shuffle the data frame row-wise so that we can randomly sample test and training data from it
shuffled_df <- dataset[sample(nrow(dataset)),]
indexes = sample(1:nrow(shuffled_df), size=0.2*nrow(shuffled_df))
test = shuffled_df[indexes,]
train = shuffled_df[-indexes,]


fit <- randomForest(factor(model_class_label) ~ ., data=train, ntree=1000, nodesize = 1, mtry=3)
print(fit)
#varImpPlot(fit)

# evaluate on test data
table(test$model_class_label, predict(fit, test[names(shuffled_df)]))

# test error
sum(test$model_class_label==predict(fit, test[names(shuffled_df)])) / nrow(test)