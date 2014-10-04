# random forest ROC analysis
library(randomForest)
library(mmadsenr)
library(ROCR)
library(ggplot2)

# assumes equifinality-2 population data, with non-predictor columns dropped, and two classes with neutral = 1, bias = 0

test_fraction = 0.2
data <- random_split_dataset(df_dropped, test_fraction)

two.rf <- randomForest(two_class_label ~ ., data=data$train, mtry=2, ntree=1000, keep.forest=TRUE, importance=TRUE, test=data$test)

two.rf.pr = predict(two.rf,type="prob",newdata=data$test)[,2]
two.rf.pred = prediction(two.rf.pr, data$test$two_class_label)
two.rf.perf = performance(two.rf.pred, "tpr", "fpr")
plot(two.rf.perf, main="ROC curve for random forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")