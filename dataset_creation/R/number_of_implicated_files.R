# Clear history
(rm(list=ls()))

# Import libraries
library(dplyr)
# library(ggplot2)
# library(randomForest)
# library(ROCR)

project = "camel" 

input = paste("../", project, "/dataset/", project, "_metrics","_",toString(0.05),".csv", sep="")
metrics <-read.csv(file=input ,head=TRUE,sep=",")

table(metrics$implicated)

sha <- metrics %>%
          select(authors) %>%
          group_by(authors) %>%
          summarize(len = n_distinct(authors))