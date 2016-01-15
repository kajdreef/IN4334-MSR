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
# project_list =  list("lucene", "mahout", "camel", "maven", "zookeeper")
# sample_size_list = list(4000, 4000, 4000, 2000, 800)
project_list =  list("camel", "maven")
sample_size_list = list(4000, 2000)
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


#######################################################################################
#######################            MAIN LOOP              #############################
#######################################################################################
i = 1
for(project in project_list){
  cat("Project: ", project, ", with sample_size of: ", sample_size_list[[i]],  "\n")
  for(threshold in threshold_list){
    cat("Threshold: ", threshold, "\n")
    # Read file
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
    metrics_all <- non_redundant_sample_data %>%
      select(-sha, -file) %>%
      transform(implicated = as.factor(implicated))
    
    
    ########### classic metrics
    metrics_classic <- sample_data %>%
      select( file_size, comment_to_code_ratio, previous_implications,
              implicated, id) %>%
      transform(implicated = as.factor(implicated))
    
    ########### START CLASSIFICATION
    list_classic <-classify(metrics_classic, k)
    list_all <- classify(metrics_all, k)
    
    cat("Performance classic: ", mean(as.numeric(list_classic)), '\n')
    cat("Performance all: ", mean(as.numeric(list_all)),  '\n')
    
    
    df_classic <- data.frame(matrix(unlist(list_classic), nrow=k, byrow=T),stringsAsFactors=FALSE)
    df_all <- data.frame(matrix(unlist(list_all), nrow=k, byrow=T),stringsAsFactors=FALSE)
    
    df_classic <- transform(data.frame(sapply(list_classic,c)))
    df_classic <- transform(as.numeric(df_classic$sapply.list_classic..c.))
    
    df_all <- transform(data.frame(sapply(list_all,c)))
    df_all <- transform(as.numeric(df_all$sapply.list_all..c.))
    
    
    result <- t.test(as.numeric(list_classic), as.numeric(list_all),  paired=TRUE)
    cat("p-value of paired t-test: ", result$p.value, '\n\n')
  }
  i = i + 1
}