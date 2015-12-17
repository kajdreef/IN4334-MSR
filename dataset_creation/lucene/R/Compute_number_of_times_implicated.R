library(dplyr)


# Clear history
(rm(list=ls()))

fix_commit_data <-read.csv(file="../dataset/lucene_implicated_fix_time.csv",head=TRUE,sep=",")


################################## TIME BETWEEN IMPLICATED LINES AND FIX ###########################################
fix_commit_data.summary<- fix_commit_data %>% summarise(
  mean = mean(time_in_days),
  median = median(time_in_days),
  SD = sd(time_in_days)
)

hist(fix_commit_data$time_in_days, freq=TRUE,  main="Time between implicated line and the fix",xlab="Time (days)")
abline(v = fix_commit_data.summary$mean, col = "red", lwd = 2)
abline(v = fix_commit_data.summary$median, col = "blue", lwd = 2)


########################## COUNT NUMBER OF TIMES A SPECIFIC VERSION OF A FILE IS IMPLICATED ###################################

imp_count_list <- fix_commit_data %>% 
  select(SHA_bug, file) %>% # Select only data relevant to the bug commit and the related file
  group_by(SHA_bug, file) %>% # Group by sha_bug and file so we can count the numer of times a specific version of a file was implicated
  summarise(imp_count = n()) # COunt number of times a certain vesion was implicated

MoreThanOnce <- imp_count_list %>% 
  filter(imp_count > 1)


imp_count_list.summary = list()
imp_count_list.summary$PercImplMoreThanOnce <- nrow(MoreThanOnce)/nrow(imp_count_list.summary) *100
imp_count_list.summary$mean <- mean(imp_count_list$imp_count)
imp_count_list.summary$median <- median(imp_count_list$imp_count)
imp_count_list.summary$SD <- sd(imp_count_list$imp_count)


hist(imp_count_list$imp_count, xlim=c(0, 20), freq=FALSE, main="Number of times a certain file version is implicated", xlab="Number of times a file is implicated")
abline(v = imp_count_list.summary$mean, col = "red", lwd = 2)
abline(v = imp_count_list.summary$median, col = "blue", lwd = 2)






