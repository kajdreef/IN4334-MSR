# Clear history
(rm(list=ls()))

# Import libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(ROCR)

#CHANGE THESE TWO PARAMETERS
project_list =  list("lucene", "camel", "mahout", "maven", "zookeeper")
sample_size_list = list(4000, 4000, 2000, 2000, 800)

threshold_list = list(0.05, 0.10, 0.20, 0.30, 0.40) #List of thresholds to be run to distinguish minor and major

corr_cutoff = 0.75
#SAMPLE SIZE MUST ALSO BE MULTIPLE OF K
k = 10                  ### K-folds
set.seed(65)
#____________________________________________

#############################################################
###########  LISTS FOR COMPUTING statistical significance
#############################################################
list_classic = list()
list_commit_based = list()
list_deleted = list()
list_added = list()
list_line_authorship = list()
list_line_based = list()
list_all = list()

#######################################################
###########  CLASSIFICATION FUNCTION
#######################################################
classify <- function(data, k){
  listOfOOB = list()
  list <- 1:k
  recall = list()
  precision = list()
  for (i in 1:k){
    # remove rows with id i from dataframe to create training set
    # select rows with id i to create test set
    trainingset <- subset(data, id %in% list[-i]) %>% select(-id)
    testset <- data %>% filter(id == i) %>% select(-id)
    
    # run a random forest model
    train.rf <- randomForest(implicated ~ ., data=trainingset, importance=TRUE)
    round(importance(train.rf), 2)
    
    oob = mean(predict(train.rf) != trainingset$implicated)
    listOfOOB = c(listOfOOB, oob)
    
    train.rf.pr = predict(train.rf, type="prob", newdata=testset)[,2]
    train.rf.pred = prediction(train.rf.pr, testset$implicated)
    train.rf.perf <- performance(train.rf.pred,"tpr","fpr")
    
    # Get confusion matrix
    cm <- train.rf$confusion
    FP = cm[2]
    TP = cm[4]
    FN = cm[3]
    recall = c(recall,TP/(FN + TP))
    precision = c(precision, TP/(TP + FP))
  }

  cat("Precision = ", mean(as.numeric(precision)), ", and recall = ", mean(as.numeric(recall)), "\n")
  
  plot(train.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
  abline(a=0,b=1,lwd=2,lty=2,col="gray")
  
  varImpPlot(train.rf, type=1)
  
  return(listOfOOB)
}


##################################################################
###########     Main Loop
##################################################################
i = 1
for(project in project_list){
  threshold = threshold_list[[1]]
  
  input = paste("../", project, "/dataset/", project, "_metrics","_",toString(threshold),".csv", sep="")
  
  metrics <-read.csv(file=input ,head=TRUE,sep=",")
  
  table(metrics$implicated)
  
  #SAMPLING
  # create the dataset with even number of buggy/non-buggy rows
  bugs <- metrics %>%
    filter(implicated == 1)
  
  non_bugs <- metrics %>%
    filter(implicated == 0)
  
  sample_data_set1 <- bugs[sample(nrow(bugs), sample_size_list[[i]], replace=TRUE, prob=NULL), ]
  sample_data_set2 <- non_bugs[sample(nrow(non_bugs), sample_size_list[[i]], replace=TRUE, prob=NULL), ]
  sample_data <- cbind(rbind(sample_data_set1, sample_data_set2), list("id"=1:k))
  
  #######################################################################################
  ####################### REMOVING HIGHLY CORRELATED FEATURES ###########################
  #######################################################################################
  
  library(corrgram)
  library(caret)
  
  add2 <- function(x) {
    x + 2
  }
  
  #corrgram(sample_data[,c(-1,-2,-ncol(sample_data))])
  
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
    select( file_size, comment_to_code_ratio, previous_implications,
            line_ownership_added, lines_added_minor_contributors, lines_added_major_contributors,
            implicated,id) %>%
    transform(implicated = as.factor(implicated))
  
  ########### classic metrics + DELETED #################################################################
  metrics_deleted <- sample_data %>%
    select( file_size, comment_to_code_ratio, previous_implications,
            line_ownership_deleted, lines_deleted_minor_contributors, lines_deleted_major_contributors,
            implicated,id) %>%
    transform(implicated = as.factor(implicated))
  
  ########### classic metrics + ORIGINAL ######################################################
  metrics_original <- sample_data %>%
    select( file_size, comment_to_code_ratio, previous_implications,
            commit_ownership, minor_contributors, major_contributors,
            implicated, id) %>%
    transform(implicated = as.factor(implicated))
  
  ########### classic metrics + line_authorship ###############################################
  metrics_line_authorship <- sample_data %>%
    select( file_size, comment_to_code_ratio, previous_implications,
            line_authorship, total_authors,
            implicated, id) %>%
    transform(implicated = as.factor(implicated))
  
  
  ########### classic metrics #################################################################
  metrics_classic <- sample_data %>%
    select( file_size, comment_to_code_ratio, previous_implications,
            implicated, id) %>%
    transform(implicated = as.factor(implicated))
  
  ########### classic metrics + ALL LINE BASED FEATURES
  metrics_line_based <- sample_data %>%
    select( line_authorship, total_authors,
            line_ownership_deleted, lines_deleted_minor_contributors,
            line_ownership_added, lines_deleted_minor_contributors,
            file_size, comment_to_code_ratio, previous_implications,
            implicated, id) %>%
    transform(implicated = as.factor(implicated))
  
  
  ########### START CLASSIFICATION
  cat("Project: ", project, ", with threshold of: ", threshold, ", and sample_size of: ", sample_size_list[[i]], "\n")
  
  classic <- classify(metrics_classic, k)
  list_classic = c(list_classic, classic)
  cat("metrics_classic OOB: ", mean(unlist(classic)), "\n\n")
  
  original <- classify(metrics_original, k)
  list_commit_based = c(list_commit_based, original)
  cat("metrics_commit OOB: ", mean(unlist(original)), "\n\n")
  
  deleted <- classify(metrics_deleted, k)
  list_deleted = c(list_deleted, deleted)
  cat("metrics_deleted OOB: ", mean(unlist(deleted)), "\n\n")
  
  added <- classify(metrics_added, k)
  list_added = c(list_added, original)
  cat("metrics_added OOB: ", mean(unlist(added)), "\n\n")
  
  line_authorship <- classify(metrics_line_authorship, k)
  list_line_authorship = c(list_line_authorship, line_authorship)
  cat("metrics_line_authorship OOB: ", mean(unlist(line_authorship)), "\n\n")
  
  line_based <- classify(metrics_line_based, k)
  list_line_based = c(list_line_based, line_based)
  cat("metrics_line_based OOB: ", mean(unlist(line_based)), "\n\n")

  all <- classify(metrics_all, k)
  list_all = c(list_all, all)
  cat("metrics_all OOB: ", mean(unlist(all)), "\n\n\n\n")
  
  i = i + 1
}

########################################################################################
########## Transform list to dataframe
########################################################################################
df_classic <- data.frame(matrix(unlist(list_classic), nrow=k, byrow=T),stringsAsFactors=FALSE)
df_classic <- transform(data.frame(sapply(list_classic,c)))
df_classic <- transform(as.numeric(df_classic$sapply.list_classic..c.))

list_to_dataframe <- function(input_list){
  df <- transform(as.numeric(unlist(input_list))) 
  return(df)
}

df_classic <- list_to_dataframe(list_classic)
df_commit <- list_to_dataframe(list_commit_based)
df_added <- list_to_dataframe(list_added)
df_deleted <- list_to_dataframe(list_deleted)
df_line_authorship <- list_to_dataframe(list_line_authorship)
df_line_based <- list_to_dataframe(list_line_based)
df_all <- list_to_dataframe(list_all)


########################################################################################
########## ARE THE RESULTS STATISTICALLY SIGNIFICANT
########################################################################################
## Real tests
t.test(df_classic$X_data, df_commit$X_data, paired=TRUE)
t.test(df_classic$X_data, df_added$X_data, paired=TRUE)
t.test(df_classic$X_data, df_deleted$X_data, paired=TRUE)
t.test(df_classic$X_data, df_line_authorship$X_data, paired=TRUE)
t.test(df_classic$X_data, df_line_based$X_data, paired=TRUE)
t.test(df_classic$X_data, df_all$X_data, paired=TRUE)




