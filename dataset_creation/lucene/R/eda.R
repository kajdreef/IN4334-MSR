# Import libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(corrgram)

setwd("/home/eric/Documents/msr/ownership/IN4334-MSR/dataset_creation/lucene/R")

# Import dataset
lucene_metrics <-read.csv(file="../dataset/metrics_v4/lucene_features_t_30.csv",head=TRUE,sep=",")

summary(lucene_metrics)

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

#aov1 = aov(lucene_metrics$total_lines_added ~ lucene_metrics$implicated)
#summary(aov1)

#aov1 = aov(lucene_metrics$total_lines_deleted ~ lucene_metrics$implicated)
#summary(aov1)

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

aov1 = aov(lucene_metrics$line_authorship ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$total_authors ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$file_size ~ lucene_metrics$implicated)
summary(aov1)

aov1 = aov(lucene_metrics$comment_to_code_ratio ~ lucene_metrics$implicated)
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

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), line_authorship))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), total_authors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), file_size))
boxplot + geom_boxplot()

boxplot <- ggplot(data=lucene_metrics, aes(factor(implicated), comment_to_code_ratio))
boxplot + geom_boxplot()

corrgram(lucene_metrics[,-15])