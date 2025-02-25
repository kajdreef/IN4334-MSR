###########################################################################################
# Classification of the data to see if there is a correlation
#
# Tutorial: http://scg.sdsu.edu/rf_r/
# Documentation: http://www.inside-r.org/packages/cran/randomforest/docs/randomforest
# Info on importance output: https://dinsdalelab.sdsu.edu/metag.stats/code/randomforest.html
###########################################################################################

# Clear history
(rm(list=ls()))


# Import libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(ROCR)


set.seed(71)

metrics <-read.csv(file="../../lucene/dataset/lucene_metrics.csv",head=TRUE,sep=",")
#metrics <-read.csv(file="../camel/dataset/camel_metrics.csv",head=TRUE,sep=",")

metrics_selected <- metrics %>%
                            select( commit_ownership, minor_contributors, major_contributors,
                            line_ownership_added, lines_added_major_contributors, lines_added_minor_contributors,
                            line_ownership_deleted, lines_deleted_major_contributors, lines_deleted_minor_contributors,
                            implicated) %>%
                            transform(implicated = as.factor(implicated))

bugs <- metrics_selected %>%
                filter(implicated == 1)

non_bugs <- metrics_selected %>%
                filter(implicated == 0)


### Create the datasets for training and validating!
sample_data_train1 <- bugs[sample(nrow(bugs), 2000, replace=TRUE, prob=NULL), ]
sample_data_train0 <- non_bugs[sample(nrow(non_bugs), 2000, replace=TRUE, prob=NULL), ]
train <- rbind(sample_data_train0, sample_data_train1)

sample_data_validate1 <- bugs[sample(nrow(bugs), 2000, replace=TRUE, prob=NULL), ]
sample_data_validate0 <- non_bugs[sample(nrow(non_bugs), 2000, replace=TRUE, prob=NULL), ]
validate <-  rbind(sample_data_validate0, sample_data_validate1)


### Apply Random Forest
train.rf <- randomForest(implicated ~ ., data=train, importance=TRUE)
print(train.rf)
round(importance(train.rf), 2)


## Check he performance of random forrest!
train.rf.pr = predict(train.rf, type="prob", newdata=validate)[,2]
train.rf.pred = prediction(train.rf.pr, validate$implicated)
train.rf.perf <- performance(train.rf.pred, "rec","prec")
train.rf.perf <- performance(train.rf.pred,"tpr","fpr")
plot(train.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)

abline(a=0,b=1,lwd=2,lty=2,col="gray")

importance(train.rf)
varImpPlot(train.rf)
