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

# set.seed(71)

metrics <-read.csv(file="../lucene/dataset/lucene_metrics.csv",head=TRUE,sep=",")
#metrics <-read.csv(file="../camel/dataset/camel_metrics.csv",head=TRUE,sep=",")

########### All metrics ######################################################################
metrics_selected <- metrics %>%
  select( commit_ownership, minor_contributors, major_contributors,
          line_ownership_added, lines_added_major_contributors, lines_added_minor_contributors,
          line_ownership_deleted, lines_deleted_major_contributors, lines_deleted_minor_contributors,
          total_contributors, 
          line_authorship, total_authors, file_size, comment_to_code_ratio,
          implicated) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + ADDED #################################################################
metrics_selected <- metrics %>%
  select( file_size, comment_to_code_ratio,
          line_ownership_added, lines_added_major_contributors, lines_added_minor_contributors,
          implicated) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + DELETED #################################################################
metrics_selected <- metrics %>%
  select( file_size, comment_to_code_ratio,
          line_ownership_deleted, lines_deleted_major_contributors, lines_deleted_minor_contributors,
          implicated) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + ORIGINAL #################################################################
metrics_selected <- metrics %>%
  select( file_size, comment_to_code_ratio,
          commit_ownership, minor_contributors, major_contributors,
          implicated) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + line_authorship #################################################################
metrics_selected <- metrics %>%
  select( file_size, comment_to_code_ratio, line_authorship,
          implicated) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics #################################################################
metrics_selected <- metrics %>%
  select( file_size, comment_to_code_ratio,
          implicated) %>%
  transform(implicated = as.factor(implicated))

######################## START CLASSIFICATION ###############################################
bugs <- metrics_selected %>%
  filter(implicated == 1)

non_bugs <- metrics_selected %>%
  filter(implicated == 0)

### K-folds
k = 10

# create the dataset with even number of buggy/non-buggy rows
sample_data_set1 <- bugs[sample(nrow(bugs), 4000, replace=TRUE, prob=NULL), ]
sample_data_set2 <- non_bugs[sample(nrow(non_bugs), 4000, replace=TRUE, prob=NULL), ]
sample_data <- cbind(rbind(sample_data_set1, sample_data_set2), list("id"=1:k))

list <- 1:k
sum = 0
for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(sample_data, id %in% list[-i]) %>% select(-id)
  testset <- sample_data %>% filter(id == i) %>% select(-id)
  
  # run a random forest model
  train.rf <- randomForest(implicated ~ ., data=trainingset, importance=TRUE)
  round(importance(train.rf), 2)
  print( train.rf$err.rate[nrow(train.rf$err.rate)])
  sum = sum + train.rf$err.rate[nrow(train.rf$err.rate)]*100
  
  
  train.rf.pr = predict(train.rf, type="prob", newdata=testset)[,2]
  train.rf.pred = prediction(train.rf.pr, testset$implicated)
  train.rf.perf <- performance(train.rf.pred,"tpr","fpr")
  plot(train.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)

  abline(a=0,b=1,lwd=2,lty=2,col="gray")
}

cat("Performance: ",sum/k)
