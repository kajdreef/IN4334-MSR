# Clear history
(rm(list=ls()))

# Import libraries
library(dplyr)
library(ggplot2)
library(caret)


#CHANGE THESE TWO PARAMETERS and add your project and the sample size 
project_list =  list("lucene", "camel", "mahout", "maven", "zookeeper")
sample_size_list = list(4000, 4000, 2000, 2000, 800)


threshold_list = list(0.05) #List of thresholds to be run to distinguish minor and major

corr_cutoff = 0.75
#SAMPLE SIZE MUST ALSO BE MULTIPLE OF K
k = 10                  ### K-folds
set.seed(65)
#____________________________________________

#######################################################
###########  CLASSIFICATION FUNCTION
#######################################################
classify <- function(data, k){
  list <- 1:k
  fit_list = list()
  for (i in 1:1){
    # remove rows with id i from dataframe to create training set
    # select rows with id i to create test set
    trainingset <- subset(data, id %in% list[-i]) %>% select(-id)
    testset <- data %>% filter(id == i) %>% select(-id)
    
    # run logistic regression
    mod_fit <- glm(implicated ~ ., data=trainingset, family=binomial(logit))
    
    # print(mod_fit)
    # result = exp(coef(mod_fit$finalModel))
    result = summary(mod_fit)
    print("Statistical significance of variables:")
    print(result)
    
    predresults <- predict(mod_fit, newdata=testset, type="response")
    predresults <- ifelse(predresults > 0.5,1,0)
    
    misClasificError <- mean(predresults != testset$implicated)
    print(paste('Accuracy', 1-misClasificError))
  }
  
  return(fit_list)
}


##################################################################
###########     Main Loop
##################################################################
i = 1
for(project in project_list){
  for(threshold in threshold_list[[1]]){
    
    input = paste("../", project, "/dataset/", project, "_metrics","_",toString(threshold),".csv", sep="")
    
    metrics <-read.csv(file=input ,head=TRUE,sep=",")
    
    table(metrics$implicated)
    
    # SAMPLING   ##########################################################################
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
    
    # corrgram(sample_data[,c(-1,-2,-ncol(sample_data))])
    
    #######################################################################################
    #######################            CLASSIFYING            #############################
    #######################################################################################
    
    #### Remove data that is not needed anymore
    (rm(bugs, cors, non_bugs, sample_data_set1, sample_data_set2))
    
    ########### All metrics ###############################################################
    metrics_all <- sample_data %>%
      select(-sha, -file) %>%
      transform(implicated = as.factor(implicated))
    
    
    ########### START CLASSIFICATION  #####################################################
    cat("Project: ", project, ", with threshold of: ", threshold, ", and sample_size of: ", sample_size_list[[i]], "\n")
    
    
    print("metrics_all ------------------------------------------------------------------------------------------")
    all <- classify(metrics_all, k)
    
    i = i + 1
  }
}
