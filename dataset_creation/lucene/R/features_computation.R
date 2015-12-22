##########################################################################################
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
lucene_data <-read.csv(file="../dataset/lucene_dataset_v4.csv",head=TRUE,sep=",")

# Get the Interesting features for this 
lucene_select <- lucene_data %>%
  select(sha, file, author, author_file_tot_added, author_file_tot_deleted, 
        file_tot_added, file_tot_deleted, 
        author_file_commits, file_tot_commits, 
        current_lines_authored, current_file_size, current_comment_lines, 
        max_current_author, total_current_authors, implicated)

# Author-ownership
lucene_select <- transform(
  lucene_select, 
  author_ownership_added = author_file_tot_added / file_tot_added,
  author_ownership_deleted = author_file_tot_deleted / file_tot_deleted,
  author_ownership = author_file_commits / file_tot_commits,
  current_authorship = max_current_author/current_file_size,
  comment_to_code_ratio = current_comment_lines / (current_file_size - current_comment_lines)
)

# Set the NA values to 0 so it can be plotted.
lucene_select['author_ownership_deleted'][is.na(lucene_select["author_ownership_deleted"])] <- 0
lucene_select['author_ownership_added'][is.na(lucene_select["author_ownership_added"])] <- 0
lucene_select['current_authorship'][is.na(lucene_select["current_authorship"])] <- 0
lucene_select['comment_to_code_ratio'][is.na(lucene_select["comment_to_code_ratio"])] <- 0

#Replace infinite values
lucene_select <- do.call(data.frame,lapply(lucene_select, function(x) replace(x, is.infinite(x),0)))
lucene_select['comment_to_code_ratio'][lucene_select['comment_to_code_ratio'] < 0] <- 0

#COMPUTE FILE FEATURES
lucene_metrics <- lucene_select %>%
  group_by(sha, file) %>%
  summarise(commit_ownership = max(author_ownership),
            line_ownership_added = max(author_ownership_added),
            line_ownership_deleted = max(author_ownership_deleted),
            total_contributors = n_distinct(author),
            line_authorship = mean(current_authorship),
            total_authors = mean(total_current_authors), #Mean of a value that is always the same, it gives the value itself
            file_size = mean(current_file_size),
            comment_to_code_ratio = mean(comment_to_code_ratio)
            #total_lines_added = mean(file_tot_added),
            #total_lines_deleted = mean(file_tot_deleted)
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
  
lucene_metrics <- merge(lucene_metrics, lucene_major, by=c("sha", "file"), all=TRUE)
lucene_metrics <- merge(lucene_metrics, lucene_minor, by=c("sha", "file"), all=TRUE)

lucene_metrics['major_contributors'][is.na(lucene_metrics["major_contributors"])] <- 0
lucene_metrics['minor_contributors'][is.na(lucene_metrics["minor_contributors"])] <- 0

# Lines added major-minor
lucene_major <- lucene_select %>%
  filter(author_ownership_added >= threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_added_major_contributors = n_distinct(author))

lucene_minor <- lucene_select %>%
  filter(author_ownership_added < threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_added_minor_contributors = n_distinct(author))

lucene_metrics <- merge(lucene_metrics, lucene_major, by=c("sha", "file"), all=TRUE)
lucene_metrics <- merge(lucene_metrics, lucene_minor, by=c("sha", "file"), all=TRUE)

lucene_metrics['lines_added_major_contributors'][is.na(lucene_metrics["lines_added_major_contributors"])] <- 0
lucene_metrics['lines_added_minor_contributors'][is.na(lucene_metrics["lines_added_minor_contributors"])] <- 0

# Lines deleted major-minor
lucene_major <- lucene_select %>%
  filter(author_ownership_deleted >= threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_deleted_major_contributors = n_distinct(author))

lucene_minor <- lucene_select %>%
  filter(author_ownership_deleted < threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_deleted_minor_contributors = n_distinct(author))

lucene_metrics <- merge(lucene_metrics, lucene_major, by=c("sha", "file"), all=TRUE)
lucene_metrics <- merge(lucene_metrics, lucene_minor, by=c("sha", "file"), all=TRUE)

lucene_metrics['lines_deleted_major_contributors'][is.na(lucene_metrics["lines_deleted_major_contributors"])] <- 0
lucene_metrics['lines_deleted_minor_contributors'][is.na(lucene_metrics["lines_deleted_minor_contributors"])] <- 0

# Add last column
lucene_implicated <- lucene_select %>%
  group_by(sha, file) %>%
  summarise(implicated = mean(implicated)) #Mean of a value that is always the same, it gives the value itself

lucene_metrics <- merge(lucene_metrics, lucene_implicated, by=c("sha", "file"), all=TRUE)

# Write in file
write.csv(lucene_metrics, file = "../dataset/metrics_v4/lucene_features_t_5.csv", row.names=FALSE)




