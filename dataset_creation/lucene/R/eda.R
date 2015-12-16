# Import libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(corrgram)

setwd("/home/eric/Documents/msr/ownership/IN4334-MSR/dataset_creation/lucene/R")

# Import dataset
lucene_metrics <-read.csv(file="../dataset/metrics_v2/lucene_features_t_30.csv",head=TRUE,sep=",")

summary(lucene_metrics)

corrgram(lucene_metrics[,-15])

#cor(lucene_metrics, use="all.obs", method="spearman") 
#nums <- sapply(lucene_metrics, is.numeric)
#numeric_features <- lucene_metrics[ , nums]
#c_matrix <- cor(numeric_features, use="all.obs", method="spearman")
#corrplot(c_matrix, method = "circle")

#http://datascience.stackexchange.com/questions/893/how-to-get-correlation-between-two-categorical-variable-and-a-categorical-variab
#cor(lucene_metrics$minor_contributors, lucene_metrics$implicated)

table(lucene_metrics$implicated)

#One-way ANOVA test
aov1 = aov(lucene_metrics$commit_ownership ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$line_ownership_added ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$line_ownership_deleted ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$total_contributors ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$total_lines_added ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$total_lines_deleted ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$minor_contributors ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$major_contributors ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$lines_added_minor_contributors ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$lines_added_major_contributors ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$lines_deleted_minor_contributors ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$lines_deleted_major_contributors ~ lucene_metrics$implicated)
summary(aov1)

# Boxplots

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), commit_ownership))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), line_ownership_added))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), line_ownership_deleted))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), total_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), minor_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), major_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), lines_added_minor_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), lines_added_major_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), lines_deleted_minor_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), lines_deleted_major_contributors))
boxplot + geom_boxplot()

# #_______________________________________________________________________
# 
# #THE NEXT PART STILL DOES NOT MAKE TOO MUCH SENSE - IT MUST BE DONE ON FILE FEATURES
# # Summary on the data ownership/implicated code.
# # CORRELATION between ownership and implicated code
# cor(lucene_select$ownership, lucene_select$implicated)
# 
# # Data on implicated code and what the mean ownership is
# implicated_ownerhips <- select(lucene_select, implicated, ownership) %>%
#   group_by(implicated) %>%
#   filter(implicated == 1) %>%
#   summarise(Mean = mean(ownership),
#             Std = sd(ownership),
#             Max_value = max(ownership),
#             Min_value = min(ownership)
#   )
# implicated_ownerhips
# 
# # Data on the NON implicated code and waht the mean ownership is.
# non_implicated_ownerhips <- select(lucene_select, implicated, ownership) %>%
#   group_by(implicated) %>%
#   filter(implicated == 0) %>%
#   summarise(Mean = mean(ownership),
#             Std = sd(ownership),
#             Max_value = max(ownership),
#             Min_value = min(ownership)
#   )
# non_implicated_ownerhips
# 
# # Box plot for the ownership for implicated code and non-implicated code
# boxplot <- ggplot(data=lucene_select, aes(factor(implicated), ownership))
# boxplot + geom_boxplot()