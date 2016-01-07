# Clear history
(rm(list=ls()))

# Import libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(ROCR)

#CHANGE THESE TWO PARAMETERS
project = "maven" 
threshold = 0.05         #To distinguish minor and major
corr_cutoff = 0.75
sample_size = 2000       #Check the output of table(metrics$implicated) before setting
#SAMPLE SIZE MUST ALSO BE MULTIPLE OF K
k = 10                  ### K-folds
set.seed(65)
#____________________________________________

input = paste("../", project, "/dataset/", project, "_metrics","_",toString(threshold),".csv", sep="")

metrics <-read.csv(file=input ,head=TRUE,sep=",")

table(metrics$implicated)

#SAMPLING
# create the dataset with even number of buggy/non-buggy rows
bugs <- metrics %>%
  filter(implicated == 1)

non_bugs <- metrics %>%
  filter(implicated == 0)

sample_data_set1 <- bugs[sample(nrow(bugs), sample_size, replace=TRUE, prob=NULL), ]
sample_data_set2 <- non_bugs[sample(nrow(non_bugs), sample_size, replace=TRUE, prob=NULL), ]
sample_data <- cbind(rbind(sample_data_set1, sample_data_set2), list("id"=1:k))


#######################################################################################
####################### REMOVING HIGHLY CORRELATED FEATURES ###########################
#######################################################################################

library(corrgram)
library(caret)

add2 <- function(x) {
  x + 2
}

corrgram(sample_data[,c(-1,-2,-ncol(sample_data))])

cors = cor(sample_data[,c(-1,-2,-ncol(sample_data))])
hc = findCorrelation(cors, cutoff=corr_cutoff)
hc = sort(hc)
hc = sapply(hc, add2, simplify = "array") #To consider sha and file
non_redundant_sample_data = sample_data[,-c(hc)]

#######################################################################################
#######################            CLASSIFYING            #############################
#######################################################################################

#### Remove data that is not needed anymore
(rm(bugs, cors, non_bugs, sample_data_set1, sample_data_set2))

########### All metrics ######################################################################
metrics_all <- non_redundant_sample_data %>%
  select(-sha, -file) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + ADDED #################################################################
metrics_added <- sample_data %>%
  select( file_size, comment_to_code_ratio,
          line_ownership_added, 
          implicated,id) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + DELETED #################################################################
metrics_deleted <- sample_data %>%
  select( file_size, comment_to_code_ratio,
          line_ownership_deleted, lines_deleted_minor_contributors,
          implicated,id) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + ORIGINAL #################################################################
metrics_original <- sample_data %>%
  select( file_size, comment_to_code_ratio,
          commit_ownership, minor_contributors,
          implicated, id) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + line_authorship #################################################################
metrics_line_authorship <- sample_data %>%
  select( file_size, comment_to_code_ratio, 
          line_authorship, total_authors,
          implicated, id) %>%
  transform(implicated = as.factor(implicated))


########### classic metrics #################################################################
metrics_classic <- sample_data %>%
  select( file_size, comment_to_code_ratio,
          implicated, id) %>%
  transform(implicated = as.factor(implicated))

########### classic metrics + ALL LINE BASED FEATURES
metrics_line_based <- sample_data %>%
  select( line_authorship, total_authors,
          line_ownership_deleted, lines_deleted_minor_contributors,
          line_ownership_added, lines_deleted_minor_contributors,
          file_size, comment_to_code_ratio,
          implicated, id) %>%
  transform(implicated = as.factor(implicated))

########### FUNCTION CLASSIFICATION
classify <- function(data, k){
  
  list <- 1:k
  sum = 0
  for (i in 1:k){
    # remove rows with id i from dataframe to create training set
    # select rows with id i to create test set
    trainingset <- subset(data, id %in% list[-i]) %>% select(-id)
    testset <- data %>% filter(id == i) %>% select(-id)
    
    # run a random forest model
    train.rf <- randomForest(implicated ~ ., data=trainingset, importance=TRUE)
    round(importance(train.rf), 2)
#     print( train.rf$err.rate[nrow(train.rf$err.rate)])
    sum = sum + train.rf$err.rate[nrow(train.rf$err.rate)]*100
    
    
    train.rf.pr = predict(train.rf, type="prob", newdata=testset)[,2]
    train.rf.pred = prediction(train.rf.pr, testset$implicated)
    train.rf.perf <- performance(train.rf.pred,"tpr","fpr")
    
  }
  print(train.rf)
  
#   plot(train.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
#   abline(a=0,b=1,lwd=2,lty=2,col="gray")
  
#   varImpPlot(train.rf)
#   cat("Performance: ",sum/k)
#   return(train.rf$err.rate)
}


########### START CLASSIFICATION
print("metrics_classic")
classify(metrics_classic, k)
print("metrics_line_authorship")
classify(metrics_line_authorship, k)
print("metrics_commit")
classify(metrics_original, k)
print("metrics_added")
classify(metrics_added, k)
print("metrics_deleted")
classify(metrics_deleted, k)
print("metrics_all")
classify(metrics_all, k)
print("metrics_line_based")
classify(metrics_line_based, k)
