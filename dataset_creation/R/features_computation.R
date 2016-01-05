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

project = "zookeeper" #change this
threshold = 0.05 # To distinguish minor and major, it should be computed using some info
                 # about the summary of the file


input = paste("../", project, "/dataset/", project, "_dataset.csv", sep="")
output = paste("../", project, "/dataset/", project, "_metrics","_",toString(threshold),".csv", sep="")

# Import dataset
data <-read.csv(file = input,head=TRUE,sep=",")

# Get the Interesting features for this 
data_select <- data %>%
  select(sha, file, author, author_file_tot_added, author_file_tot_deleted, 
        file_tot_added, file_tot_deleted, 
        author_file_commits, file_tot_commits, 
        current_lines_authored, current_file_size, current_comment_lines, 
        max_current_author, total_current_authors, implicated)

# Author-ownership
data_select <- transform(
  data_select, 
  author_ownership_added = author_file_tot_added / file_tot_added,
  author_ownership_deleted = author_file_tot_deleted / file_tot_deleted,
  author_ownership = author_file_commits / file_tot_commits,
  current_authorship = max_current_author/current_file_size,
  comment_to_code_ratio = current_comment_lines / (current_file_size - current_comment_lines)
)

# Set the NA values to 0 so it can be plotted.
data_select['author_ownership_deleted'][is.na(data_select["author_ownership_deleted"])] <- 0
data_select['author_ownership_added'][is.na(data_select["author_ownership_added"])] <- 0
data_select['current_authorship'][is.na(data_select["current_authorship"])] <- 0
data_select['comment_to_code_ratio'][is.na(data_select["comment_to_code_ratio"])] <- 0

#Replace infinite values
data_select <- do.call(data.frame,lapply(data_select, function(x) replace(x, is.infinite(x),0)))
data_select['comment_to_code_ratio'][data_select['comment_to_code_ratio'] < 0] <- 0

#COMPUTE FILE FEATURES
metrics <- data_select %>%
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

# Commit major-minor
major <- data_select %>%
  filter(author_ownership >= threshold) %>%
  group_by(sha, file) %>%
  summarise(major_contributors = n_distinct(author))

minor <- data_select %>%
  filter(author_ownership < threshold) %>%
  group_by(sha, file) %>%
  summarise(minor_contributors = n_distinct(author))
  
metrics <- merge(metrics, major, by=c("sha", "file"), all=TRUE)
metrics <- merge(metrics, minor, by=c("sha", "file"), all=TRUE)

metrics['major_contributors'][is.na(metrics["major_contributors"])] <- 0
metrics['minor_contributors'][is.na(metrics["minor_contributors"])] <- 0

# Lines added major-minor
major <- data_select %>%
  filter(author_ownership_added >= threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_added_major_contributors = n_distinct(author))

minor <- data_select %>%
  filter(author_ownership_added < threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_added_minor_contributors = n_distinct(author))

metrics <- merge(metrics, major, by=c("sha", "file"), all=TRUE)
metrics <- merge(metrics, minor, by=c("sha", "file"), all=TRUE)

metrics['lines_added_major_contributors'][is.na(metrics["lines_added_major_contributors"])] <- 0
metrics['lines_added_minor_contributors'][is.na(metrics["lines_added_minor_contributors"])] <- 0

# Lines deleted major-minor
major <- data_select %>%
  filter(author_ownership_deleted >= threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_deleted_major_contributors = n_distinct(author))

minor <- data_select %>%
  filter(author_ownership_deleted < threshold) %>%
  group_by(sha, file) %>%
  summarise(lines_deleted_minor_contributors = n_distinct(author))

metrics <- merge(metrics, major, by=c("sha", "file"), all=TRUE)
metrics <- merge(metrics, minor, by=c("sha", "file"), all=TRUE)

metrics['lines_deleted_major_contributors'][is.na(metrics["lines_deleted_major_contributors"])] <- 0
metrics['lines_deleted_minor_contributors'][is.na(metrics["lines_deleted_minor_contributors"])] <- 0

# Add last column
implicated <- data_select %>%
  group_by(sha, file) %>%
  summarise(implicated = mean(implicated)) #Mean of a value that is always the same, it gives the value itself

metrics <- merge(metrics, implicated, by=c("sha", "file"), all=TRUE)

# Write in file
write.csv(metrics, file = output, row.names=FALSE)





