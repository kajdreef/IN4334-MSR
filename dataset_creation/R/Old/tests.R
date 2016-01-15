# Clear history
(rm(list=ls()))


# Import libraries
library(dplyr)
library(ggplot2)

#CHANGE THESE TWO PARAMETERS
project = "lucene" 
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
library(caret)

add2 <- function(x) {
  x + 2
}

cors = cor(sample_data[,c(-1,-2,-ncol(sample_data))])
hc = findCorrelation(cors, cutoff=corr_cutoff)
hc = sort(hc)
hc = sapply(hc, add2, simplify = "array") #To consider sha and file
non_redundant_sample_data = sample_data[,-c(hc)]


# load the library
#library(mlbench)
# prepare training scheme
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
#model <- train(implicated~., data=non_redundant_sample_data, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
#importance <- varImp(model, scale=FALSE)
# summarize importance
#print(importance)
#plot(importance)

#data = non_redundant_sample_data[,c(-1,-2,-ncol(non_redundant_sample_data))]
data = sample_data[,c(-1,-2,-ncol(non_redundant_sample_data))]

data <- data %>%
  transform(implicated = as.factor(implicated))

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(randomForest)

# load the data
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(data[,1:(ncol(data) - 1)], 
               data[,ncol(data)], 
               c(1:(ncol(data) - 1)), 
               rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))