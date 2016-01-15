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
project_list =  list("lucene", "camel", "mahout", "maven", "zookeeper")
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
  #   plot(train.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
  #   abline(a=0,b=1,lwd=2,lty=2,col="gray")
  #   varImpPlot(train.rf)
  
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
  
  #######################################################################################
  ####################### REMOVING HIGHLY CORRELATED FEATURES ###########################
  #######################################################################################
  
  
  add2 <- function(x) {
    x + 2
  }
  
  #corrgram(sample_data[,c(-1,-2,-ncol(sample_data))])
  
  cors = cor(sample_data[,c(-1,-2,-ncol(sample_data))])
  hc = findCorrelation(cors, cutoff=corr_cutoff)
  hc = sort(hc)
  hc = sapply(hc, add2, simplify = "array") #To consider sha and file
  non_redundant_sample_data = sample_data[,-c(hc)]
  
  
  #### Remove data that is not needed anymore
  (rm(bugs, cors, non_bugs, sample_data_set1, sample_data_set2))
  
  
  # Transform implicated as a factor so it can be used for classification
  ########### classic metrics
  metrics_all <- non_redundant_sample_data %>%
    select(-sha, -file) %>%
    transform(implicated = as.factor(implicated))
  
  
  ########### classic metrics
  metrics_classic <- sample_data %>%
    select( file_size, comment_to_code_ratio, previous_implications,
            implicated, id) %>%
    transform(implicated = as.factor(implicated))
  
  print("Classify classic")
  classify_classic = classify(metrics_classic, k)
  print("Classify all")
  classiy_all = classify(metrics_all, k)
  
  return(list(classic=classify_classic, all=classiy_all))
}

####################### LISTS WHERE RESULTS ARE PLACED IN #############################

thsd_0_5_classic = list()
thsd_0_5_all = list()

thsd_1_0_classic = list()
thsd_1_0_all = list()

thsd_2_0_classic = list()
thsd_2_0_all = list()

thsd_3_0_classic = list()
thsd_3_0_all = list()

thsd_4_0_classic = list()
thsd_4_0_all = list()

#######################################################################################
#######################            MAIN LOOP              #############################
#######################################################################################
count = 1
for(project in project_list){
  result = run(project, 0.05, count)
  thsd_0_5_classic = c(thsd_0_5_classic, result$classic)
  thsd_0_5_all = c(thsd_0_5_all, result$all)

  result = run(project, 0.10, count)
  thsd_1_0_classic = c(thsd_1_0_classic, result$classic)
  thsd_1_0_all = c(thsd_1_0_all, result$all)
  
  result = run(project, 0.20, count)
  thsd_2_0_classic = c(thsd_2_0_classic, result$classic)
  thsd_2_0_all = c(thsd_2_0_all, result$all)

  result = run(project, 0.30, count)
  thsd_3_0_classic = c(thsd_3_0_classic, result$classic)
  thsd_3_0_all = c(thsd_3_0_all, result$all)
  
  result = run(project, 0.40, count)
  thsd_4_0_classic = c(thsd_4_0_classic, result$classic)
  thsd_4_0_all = c(thsd_4_0_all, result$all)
  count = count + 1
}

list_to_dataframe <- function(input_list){
  df <- transform(as.numeric(unlist(input_list))) 
  return(df)
}

df_0_5_classic = list_to_dataframe(thsd_0_5_classic)
df_0_5_all = list_to_dataframe(thsd_0_5_all)

df_1_0_classic = list_to_dataframe(thsd_1_0_classic)
df_1_0_all = list_to_dataframe(thsd_1_0_all)

df_2_0_classic = list_to_dataframe(thsd_2_0_classic)
df_2_0_all = list_to_dataframe(thsd_2_0_all)

df_3_0_classic = list_to_dataframe(thsd_3_0_classic)
df_3_0_all = list_to_dataframe(thsd_3_0_all)

df_4_0_classic = list_to_dataframe(thsd_4_0_classic)
df_4_0_all = list_to_dataframe(thsd_4_0_all)

t.test(df_0_5_classic$X_data, df_0_5_all$X_data, pair=TRUE)$p.value
t.test(df_1_0_classic$X_data, df_1_0_all$X_data, pair=TRUE)$p.value
t.test(df_2_0_classic$X_data, df_2_0_all$X_data, pair=TRUE)$p.value
t.test(df_3_0_classic$X_data, df_3_0_all$X_data, pair=TRUE)$p.value
t.test(df_4_0_classic$X_data, df_4_0_all$X_data, pair=TRUE)$p.value

