# Clear history
(rm(list=ls()))

# Import libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Import dataset
data <-read.csv(file="../lucene/dataset/lucene_dataset.csv",head=TRUE,sep=",")
#data <-read.csv(file="../camel/dataset/camel_dataset.csv",head=TRUE,sep=",")


###########################################################################
# Computes the number of lines added/deleted by a commit.
# Also the distribution is plotted and some other metrics are calculated 
# (mean lines added, SD, Max/Min).
###########################################################################
data_select <- data %>%
  select(sha, file, author_file_added_this_commit, author_file_deleted_this_commit)  %>%
  group_by(sha) %>%
  summarise(
    total_added_lines_in_commit = sum(author_file_added_this_commit),
    total_deleted_lines_in_commit = sum(author_file_deleted_this_commit)
    )
data_select 

# Calculate the Mean, SD, Max, and Min value of number of added lines in commits
commit_total_lines <- data_select %>%
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
  data=data_select,
  aes(data_select$total_added_lines_in_commit)
) + geom_histogram() + scale_x_log10() + labs(title="Distribution of lines added in commit")  + labs(x="Total number of lines added in commit", y="Frequency")


histogramDeleted <- ggplot(
  data=data_select,
  aes(data_select$total_deleted_lines_in_commit)
) + geom_histogram() + scale_x_log10() + labs(title="Distribution of lines deleted in commit")  + labs(x="Total number of lines deleted in commit", y="Frequency")


grid.arrange(histogramAdded, histogramDeleted, nrow=2)


