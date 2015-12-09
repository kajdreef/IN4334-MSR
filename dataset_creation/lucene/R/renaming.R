
library(dplyr)

setwd("/home/eric/Documents/msr/ownership/IN4334-MSR/dataset_creation/lucene/R")

# Import dataset
lucene_metrics <-read.csv(file="../dataset/lucene_dataset_v1.csv",head=TRUE,sep=",")

lucene_metrics_renamed <- lucene_metrics %>%
  rename(author_commit = author_file_tot_added, this_commit = author_file_added_this_commit, total_commit = file_tot_added)

write.csv(lucene_metrics_renamed, file = "../dataset/lucene_dataset_v1_renamed.csv", row.names=FALSE)
