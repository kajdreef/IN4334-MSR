# Clear history
(rm(list=ls()))

# Import libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(ROCR)
library(corrgram)
library(caret)


#CHANGE THESE TWO PARAMETERS
project_list =  list("lucene" , "camel", "mahout", "maven", "zookeeper")
sample_size_list = list(4000, 4000, 2000, 2000, 800)
# project_list =  list("zookeeper")
# sample_size_list = list(800)
threshold_list = list(0.05, 0.10, 0.20, 0.30, 0.40) #List of thresholds to be run to distinguish minor and major

corr_cutoff = 0.75
#SAMPLE SIZE MUST ALSO BE MULTIPLE OF K
k = 10                  ### K-folds
set.seed(65)
#____________________________________________

#######################################################################################
#######################            CLASSIFYING            #############################
#######################################################################################
classify <- function(data, k){
  
  list <- 1:k
  i = 0
  listOfObs= list()
  
  for (i in 1:k){
    # remove rows with id i from dataframe to create training set
    # select rows with id i to create test set
    trainingset <- subset(data, id %in% list[-i]) %>% select(-id)
    testset <- data %>% filter(id == i) %>% select(-id)
    
    # run a random forest model
    train.rf <- randomForest(implicated ~ ., data=trainingset, importance=TRUE)
    round(importance(train.rf), 2)
    
    oob = mean(predict(train.rf) != trainingset$implicated)
    listOfObs = c(listOfObs, oob)
    
    train.rf.pr = predict(train.rf, type="prob", newdata=testset)[,2]
    train.rf.pred = prediction(train.rf.pr, testset$implicated)
    train.rf.perf <- performance(train.rf.pred,"tpr","fpr")
    
  }
  
  varImpPlot(train.rf)
  
  return(listOfObs)
}

run <- function(project_name, threshold, c){
  cat("Project ", project_name, ", threshold: ", threshold, ", sample_size: " ,sample_size_list[[c]], "\n")
  # Read file
  input = paste("../", project, "/dataset/", project_name, "_metrics","_",toString(threshold),".csv", sep="")
  metrics <-read.csv(file=input ,head=TRUE,sep=",")
  
  table(metrics$implicated)
  
  #SAMPLING
  # create the dataset with even number of buggy/non-buggy rows
  bugs <- metrics %>%
    filter(implicated == 1)
  
  non_bugs <- metrics %>%
    filter(implicated == 0)
  
  sample_data_set1 <- bugs[sample(nrow(bugs), sample_size_list[[c]], replace=TRUE, prob=NULL), ]
  sample_data_set2 <- non_bugs[sample(nrow(non_bugs), sample_size_list[[c]], replace=TRUE, prob=NULL), ]
  sample_data <- cbind(rbind(sample_data_set1, sample_data_set2), list("id"=1:k))
  
  ########### classic metrics
  metrics_threshold <- sample_data %>%
    select( minor_contributors, major_contributors,
            lines_deleted_minor_contributors, lines_deleted_major_contributors,
            lines_added_minor_contributors, lines_added_major_contributors,
            implicated, id) %>%
    transform(implicated = as.factor(implicated))
  
  print("Classify threshold")
  classify_threshold = classify(metrics_threshold, k)

  return(classify_threshold)
}

####################### LISTS WHERE RESULTS ARE PLACED IN #############################

thsd_0_5_threshold = list()
thsd_1_0_threshold = list()
thsd_2_0_threshold = list()
thsd_3_0_threshold = list()
thsd_4_0_threshold = list()

#######################################################################################
#######################            MAIN LOOP              #############################
#######################################################################################
count = 1
for(project in project_list){
  result = run(project, 0.05, count)
  thsd_0_5_threshold = c(thsd_0_5_threshold, result)
  
  result = run(project, 0.10, count)
  thsd_1_0_threshold = c(thsd_1_0_threshold, result)
  
  result = run(project, 0.20, count)
  thsd_2_0_threshold = c(thsd_2_0_threshold, result)
  
  result = run(project, 0.30, count)
  thsd_3_0_threshold = c(thsd_3_0_threshold, result)
  
  result = run(project, 0.40, count)
  thsd_4_0_threshold = c(thsd_4_0_threshold, result)
  count = count + 1
}

list_to_dataframe <- function(input_list){
  df <- transform(as.numeric(unlist(input_list))) 
  return(df)
}

df_0_5_threshold = list_to_dataframe(thsd_0_5_threshold)
df_1_0_threshold = list_to_dataframe(thsd_1_0_threshold)
df_2_0_threshold = list_to_dataframe(thsd_2_0_threshold)
df_3_0_threshold = list_to_dataframe(thsd_3_0_threshold)
df_4_0_threshold = list_to_dataframe(thsd_4_0_threshold)

df_0_5 = list_to_dataframe(df_0_5_threshold)
df_1_0 = list_to_dataframe(df_1_0_threshold)
df_2_0 = list_to_dataframe(df_2_0_threshold)
df_3_0 = list_to_dataframe(df_3_0_threshold)
df_4_0 = list_to_dataframe(df_4_0_threshold)

df <- cbind(df_0_5, df_1_0,  df_2_0, df_3_0, df_4_0)

names(df) <- c("t05", "t10", "t20", "t30", "t40")

for(num in 1:length(project_list)){
  start = (1+(num-1)*10)
  end = (num*10)
  result <- aov(
        df$t05[start:end] ~ 
          df$t10[start:end] *
          df$t20[start:end] * 
          df$t30[start:end] * 
          df$t40[start:end]
        )
  
  cat("Threshold 0.05 performance: ", mean(df$t05[start:end]) , "\n" )
  cat("Threshold 0.10 performance: ", mean(df$t10[start:end]) , "\n" )
  cat("Threshold 0.20 performance: ", mean(df$t20[start:end]) , "\n" )
  cat("Threshold 0.30 performance: ", mean(df$t30[start:end]) , "\n" )
  cat("Threshold 0.40 performance: ", mean(df$t40[start:end]) , "\n\n" )
  
  print(summary(result))
}


