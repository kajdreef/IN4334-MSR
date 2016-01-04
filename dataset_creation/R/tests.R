# Clear history
(rm(list=ls()))

library(dplyr)
library(randomForest)
library(ggplot2)

setwd("/home/eric/Documents/msr/ownership/IN4334-MSR/dataset_creation/lucene/R")


set.seed(71)

data <-read.csv(file="../dataset/metrics_v3/lucene_features_t_5.csv",head=TRUE,sep=",")

features <- data %>%
  select(commit_ownership, line_ownership_added, line_ownership_deleted, 
         total_contributors, major_contributors, minor_contributors,
         lines_added_major_contributors, lines_added_minor_contributors,
         lines_deleted_major_contributors, lines_deleted_minor_contributors,
         implicated) %>%
  transform(implicated = as.factor(implicated))

lucene_1 <- features %>%
  filter(implicated == 1)

lucene_0 <- features %>%
  filter(implicated == 0)

sample_train1 <- lucene_1[sample(nrow(lucene_1), 4000, replace=TRUE, prob=NULL), ]
sample_train0 <- lucene_0[sample(nrow(lucene_0), 4000, replace=TRUE, prob=NULL), ]
train <- rbind(sample_train0, sample_train1)

sample_test1 <- lucene_1[sample(nrow(lucene_1), 4000, replace=TRUE, prob=NULL), ]
sample_test0 <- lucene_0[sample(nrow(lucene_0), 4000, replace=TRUE, prob=NULL), ]
test <- rbind(sample_test0, sample_test1)

fit <- randomForest(implicated ~ .,  data=train, importance=TRUE, proximity=TRUE)

importance(fit)
varImpPlot(fit)

fit
#prediction <- predict(fit, test)
#result <- data.frame(id = test[,0], implicated = prediction)

#fit <- randomForest(as.factor(implicated) ~ commit_ownership + 
#                      line_ownership_added + 
#                      line_ownership_deleted + 
#                      total_contributors + 
#                      major_contributors + 
#                      minor_contributors + 
#                      lines_added_major_contributors + 
#                      lines_added_minor_contributors +
#                      lines_deleted_major_contributors + 
#                      lines_deleted_minor_contributors, 
#                    data=train, importance=TRUE, ntree=2000)

