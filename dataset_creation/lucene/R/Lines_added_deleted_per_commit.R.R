# Clear history
(rm(list=ls()))

# Import libraries
library(dplyr)
library(ggplot2)

# Import dataset
lucene_data <- read.csv(file="../dataset/lucene_dataset_v1.csv",head=TRUE,sep=",")

###########################################################################
# Computes the number of lines added/deleted by a commit.
# Also the distribution is plotted and some other metrics are calculated 
# (mean lines added, SD, Max/Min).
###########################################################################
lucene_data_select <- lucene_data %>%
  select(sha, file, author_file_added_this_commit, author_file_deleted_this_commit)  %>%
  group_by(sha) %>%
  summarise(
    total_added_lines_in_commit = sum(author_file_added_this_commit),
    total_deleted_lines_in_commit = sum(author_file_deleted_this_commit)
    )
lucene_data_select 

# Calculate the Mean, SD, Max, and Min value of number of added lines in commits
commit_total_lines <- lucene_data_select %>%
  select(total_added_lines_in_commit, total_deleted_lines_in_commit) %>%
  summarise(Mean_added = mean(total_added_lines_in_commit),
            Std_added = sd(total_added_lines_in_commit),
            Max_value_added = max(total_added_lines_in_commit),
            Min_value_added = min(total_added_lines_in_commit),
            
            Mean_deleted = mean(total_deleted_lines_in_commit),
            Std_deleted = sd(total_deleted_lines_in_commit),
            Max_value_deleted = max(total_deleted_lines_in_commit),
            Min_value_deleted = min(total_deleted_lines_in_commit)
  )
commit_total_lines

# Density plot
histogramAdded <- ggplot(
  data=lucene_data_select,
  aes(lucene_data_select$total_added_lines_in_commit)
) + geom_histogram() + scale_x_log10() + labs(title="Distribution of commit size added")  + labs(x="Total number of lines added in commit", y="Frequency")
histogramAdded

histogramDeleted <- ggplot(
  data=lucene_data_select,
  aes(lucene_data_select$total_deleted_lines_in_commit)
) + geom_histogram() + scale_x_log10() + labs(title="Distribution of commit size deleted")  + labs(x="Total number of lines added in commit", y="Frequency")
histogramDeleted




