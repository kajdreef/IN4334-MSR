# Clear history
(rm(list=ls()))

# Import libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Import dataset
lucene_data <- read.csv(file="../dataset/lucene_dataset_v1.csv",head=TRUE,sep=",")

###########################################################################
# Computes the number of lines added/deleted to a file per commit.
# Also the distribution is plotted and some other metrics are calculated 
# (mean lines added, SD, Max/Min).
###########################################################################
lucene_data_select <- lucene_data %>%
  select(sha, file, author_file_added_this_commit, author_file_deleted_this_commit)  %>%
  group_by(sha, file) %>%
  summarise(
    lines_added_per_file_in_commit = sum(author_file_added_this_commit),
    lines_deleted_per_file_in_commit = sum(author_file_deleted_this_commit)
    )
lucene_data_select 

# Calculate the Mean, SD, Max, and Min value of number of added lines in commits
commit_total_lines <- lucene_data_select %>%
  select(sha,file,lines_added_per_file_in_commit, lines_deleted_per_file_in_commit)%>%
  group_by(sha)%>%
  summarise(Mean_added = mean(lines_added_per_file_in_commit),
            Std_added = sd(lines_added_per_file_in_commit),
            Max_value_added = max(lines_added_per_file_in_commit),
            Min_value_added = min(lines_added_per_file_in_commit),
            
            Mean_deleted = mean(lines_deleted_per_file_in_commit),
            Std_deleted = sd(lines_deleted_per_file_in_commit),
            Max_value_deleted = max(lines_deleted_per_file_in_commit),
            Min_value_deleted = min(lines_deleted_per_file_in_commit)
  )
commit_total_lines


summary(commit_total_lines$Mean_added)
summary(commit_total_lines$Mean_deleted)

commit_total_lines %>%
  select(Mean_added, Mean_deleted) %>%
  summarise(MeanOfMean_added = mean(Mean_added),
            SDOfMean_added = sd(Mean_added),
            MaxMean_added = max(Mean_added),
            MinMean_added = min(Mean_added),
            
            MeanOfMean_deleted = mean(Mean_deleted),
            SDOfMean_deleted = sd(Mean_deleted),
            MaxMean_deleted = max(Mean_deleted),
            MinMean_deleted = min(Mean_deleted)
  )

# Density plot
histogramAdded <- ggplot(
  data=lucene_data_select,
  aes(lucene_data_select$lines_added_per_file_in_commit)
) + geom_histogram() + scale_x_log10() + labs(title="Distribution of lines added to file in commit")  + labs(x="Total number of lines added to file in commit", y="Frequency")


# Density plot
histogramDeleted <- ggplot(
  data=lucene_data_select,
  aes(lucene_data_select$lines_deleted_per_file_in_commit)
) + geom_histogram() + scale_x_log10() + labs(title="Distribution of lines deleted from file in commit")  + labs(x="Total number of lines deleted from file in commit", y="Frequency")

grid.arrange(histogramAdded, histogramDeleted, nrow=2)




