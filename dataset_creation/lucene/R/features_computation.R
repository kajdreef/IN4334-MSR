###########################################################################################
# Find correlation between lines added by auther over time and total lines added to file
# http://www.princeton.edu/~mattg/statar/group-by.html
# http://docs.ggplot2.org/current/geom_boxplot.html
# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
###########################################################################################

# Clear history
(rm(list=ls()))


# Import libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(corrgram)

setwd("/home/eric/Documents/msr/ownership/IN4334-MSR/dataset_creation/lucene/R")

# Import dataset
lucene_data <-read.csv(file="../dataset/lucene_dataset_v1.csv",head=TRUE,sep=",")

# Get the Interesting features for this 
lucene_select <- lucene_data %>%
  select(sha, file, author, author_file_tot_added, author_file_tot_deleted, 
        file_tot_added, file_tot_deleted, 
        author_file_commits, file_tot_commits, implicated)

# Author-ownership
lucene_select <- transform(
  lucene_select, 
  author_ownership_added = author_file_tot_added / file_tot_added,
  author_ownership_deleted = author_file_tot_deleted / file_tot_deleted,
  author_ownership = author_file_commits / file_tot_commits
)

# Set the NA values to 0 so it can be plotted.
lucene_select['author_ownership_deleted'][is.na(lucene_select["author_ownership_deleted"])] <- 0

#COMPUTE FILE FEATURES
lucene_metrics <- lucene_select %>%
  group_by(sha, file) %>%
  summarise(commit_ownership = max(author_ownership),
            line_ownership_added = max(author_ownership_added),
            line_ownership_deleted = max(author_ownership_deleted),
            total_contributors = n_distinct(author),
            total_lines_added = mean(file_tot_added), #Mean of a value that is always the same, it gives the value itself
            total_lines_deleted = mean(file_tot_deleted)
            )

threshold = 0.05 # To distinguish minor and major, it should be computed using some info
                 # about the summary of the file

# Commit major-minor
lucene_major <- lucene_select %>%
  filter(author_ownership >= threshold) %>%
  group_by(sha, file) %>%
  summarise(major_contributors = n_distinct(author))

lucene_minor <- lucene_select %>%
  filter(author_ownership < threshold) %>%
  group_by(sha, file) %>%
  summarise(minor_contributors = n_distinct(author))
  
lucene_metrics <- merge(lucene_metrics, lucene_major, by=c("sha", "file"))
lucene_metrics <- merge(lucene_metrics, lucene_minor, by=c("sha", "file"))

# Lines added major-minor
lucene_major <- lucene_select %>%
  filter(author_ownership_added >= threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_added_major_contributors = n_distinct(author))

lucene_minor <- lucene_select %>%
  filter(author_ownership_added < threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_added_minor_contributors = n_distinct(author))

lucene_metrics <- merge(lucene_metrics, lucene_major, by=c("sha", "file"))
lucene_metrics <- merge(lucene_metrics, lucene_minor, by=c("sha", "file"))


# Lines deleted major-minor
lucene_major <- lucene_select %>%
  filter(author_ownership_deleted >= threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_deleted_major_contributors = n_distinct(author))

lucene_minor <- lucene_select %>%
  filter(author_ownership_deleted < threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_deleted_minor_contributors = n_distinct(author))

lucene_metrics <- merge(lucene_metrics, lucene_major, by=c("sha", "file"))
lucene_metrics <- merge(lucene_metrics, lucene_minor, by=c("sha", "file"))

# Add last column
lucene_implicated <- lucene_select %>%
  group_by(sha, file) %>%
  summarise(implicated = mean(implicated)) #Mean of a value that is always the same, it gives the value itself

lucene_metrics <- merge(lucene_metrics, lucene_implicated, by=c("sha", "file"))

# Write in file
write.csv(lucene_metrics, file = "../dataset/lucene_features.csv", row.names=FALSE)




