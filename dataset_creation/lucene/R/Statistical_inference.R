# Clear history
(rm(list=ls()))


# Import libraries
library(dplyr)

dataset_csv <-read.csv(file="../dataset/metrics_v2/lucene_features_t_5.csv",head=TRUE,sep=",")


# Get all the bugs from the data set and create a new set.
bugs <- dataset_csv %>%
  filter(implicated == 1)

bugs_sampled_set <- bugs[
  sample(nrow(bugs), 2000, replace=TRUE, prob=NULL),
  ]

# Get all the non-bugs from the data set and create a new set.
non_bugs <- dataset_csv %>%
  filter(implicated == 0)

non_bugs_sampled_set <- non_bugs[
  sample(nrow(non_bugs), 2000, replace=TRUE, prob=NULL),
  ]


for(i in 3:(ncol(non_bugs_sampled_set)-1)){
  print(names(non_bugs_sampled_set[i]))
  result <- t.test(non_bugs_sampled_set[i], bugs_sampled_set[i], conf.level = 0.95)
  print(result$p.value)
}

(rm(bugs, non_bugs))

# Determine the mean and sd for every column
non_bug_stat <- sapply(non_bugs_sampled_set[,3:(ncol(non_bugs_sampled_set)-1)], function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE)))
bug_stat <- sapply(bugs_sampled_set[,3:(ncol(bugs_sampled_set)-1)], function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE)))

(rm(bugs_sampled_set, non_bugs_sampled_set))

