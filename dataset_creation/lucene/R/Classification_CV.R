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

lucene_metrics <-read.csv(file="../dataset/metrics_v2/lucene_features_t_5.csv",head=TRUE,sep=",")

lucene_metrics_selected <- lucene_metrics %>%
  select( commit_ownership, minor_contributors, major_contributors,
          # line_ownership_added, lines_added_major_contributors, lines_added_minor_contributors,
          # line_ownership_deleted, lines_deleted_major_contributors, lines_deleted_minor_contributors,
          total_contributors, 
          implicated) %>%
  transform(implicated = as.factor(implicated))

lucene_bugs <- lucene_metrics_selected %>%
  filter(implicated == 1)

lucene_non_bugs <- lucene_metrics_selected %>%
  filter(implicated == 0)

### K-folds
k = 4

# create the dataset with even number of buggy/non-buggy rows
sample_data_set1 <- lucene_bugs[sample(nrow(lucene_bugs), 2000, replace=TRUE, prob=NULL), ]
sample_data_set2 <- lucene_non_bugs[sample(nrow(lucene_non_bugs), 2000, replace=TRUE, prob=NULL), ]
sample_data <- cbind(rbind(sample_data_set1, sample_data_set2), list("id"=1:4))

list <- 1:k
for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(sample_data, id %in% list[-i]) %>% select(-id)
  testset <- sample_data %>% filter(id == i) %>% select(-id)
  
  # run a random forest model
  train.rf <- randomForest(implicated ~ ., data=trainingset, importance=TRUE)
  print(train.rf)
  round(importance(train.rf), 2)
  
  
  train.rf.pr = predict(train.rf, type="prob", newdata=testset)[,2]
  train.rf.pred = prediction(train.rf.pr, testset$implicated)
  train.rf.perf <- performance(train.rf.pred,"tpr","fpr")
  plot(train.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
  
#   train.rf.perf <- performance(train.rf.pred,"prec","rec")
#   plot(train.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
  
  abline(a=0,b=1,lwd=2,lty=2,col="gray")
}

