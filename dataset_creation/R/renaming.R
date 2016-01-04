
library(dplyr)

setwd("/home/eric/Documents/msr/ownership/IN4334-MSR/dataset_creation/lucene/R")

# Import dataset
metrics <-read.csv(file="../lucene/dataset/lucene_dataset.csv",head=TRUE,sep=",")
#metrics <-read.csv(file="../camel/dataset/camel_dataset.csv",head=TRUE,sep=",")

metrics_renamed <- metrics %>%
  rename(author_commit = author_file_tot_added, this_commit = author_file_added_this_commit, total_commit = file_tot_added)

write.csv(metrics_renamed, file = "../lucene/dataset/lucene_dataset_renamed.csv", row.names=FALSE)
#write.csv(metrics_renamed, file = "../camel/dataset/camel_dataset_renamed.csv", row.names=FALSE)