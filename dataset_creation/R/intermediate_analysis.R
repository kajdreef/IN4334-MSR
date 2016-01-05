# Clear history
(rm(list=ls()))


# Import libraries
library(dplyr)
library(ggplot2)

#CHANGE THESE TWO PARAMETERS
project = "lucene" 
threshold = 0.05         #To distinguish minor and major
corr_cutoff = 0.7
sample_size = 4000       #Check the output of table(metrics$implicated) before setting
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
####################### CLASSIFYING WITH CLASSIC  METRICS #############################
#######################################################################################
library(randomForest)
library(ROCR)

# SELECT THEM FROM THE sample_data DATAFRAME

#######################################################################################
####################### CLASSIFYING WITH CLASSIC  METRICS + NEW #######################
#######################################################################################

# Use directly non_redundant_sample_data without selecting, do only the transform of the implicated column