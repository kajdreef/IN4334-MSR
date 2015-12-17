library(dplyr)


# Clear history
(rm(list=ls()))

fix_commit_data <-read.csv(file="../dataset/lucene_fix_commits.csv",head=TRUE,sep=",")


fix_commit_data.summary<- fix_commit_data %>% summarise(
  mean = mean(time_in_days),
  median = median(time_in_days),
  SD = sd(time_in_days)
)

test <- fix_commit_data %>% 
  select(SHA_bug, file) %>% # Select only data relevant to the bug commit and the related file
  group_by(SHA_bug, file) %>% # Group by sha_bug and file so we can count the numer of times a specific version of a file was implicated
  summarise(imp_count = n()) # COunt number of times a certain vesion was implicated

test.summary = list()
test.summary$mean <- mean(test$imp_count)
test.summary$median <- median(test$imp_count)
test.summary$SD <- sd(test$imp_count)


hist(test$imp_count, freq=TRUE, main="Number of times a certain file version is implicated")
abline(v = test.summary$mean, col = "red", lwd = 2)
abline(v = test.summary$median, col = "blue", lwd = 2)


hist(fix_commit_data$time_in_days, freq=TRUE, main="Histogram of time between implicated line and the fix",xlab="Time (days)")
abline(v = fix_commit_data.summary$mean, col = "red", lwd = 2)
abline(v = fix_commit_data.summary$median, col = "blue", lwd = 2)



