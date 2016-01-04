# Import libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(corrgram)

# Import dataset
metrics <-read.csv(file="../lucene/dataset/lucene_metrics.csv",head=TRUE,sep=",")
#metrics <-read.csv(file="../camel/dataset/camel_metrics.csv",head=TRUE,sep=",")

summary(metrics)

#cor(metrics, use="all.obs", method="spearman") 
#nums <- sapply(metrics, is.numeric)
#numeric_features <- metrics[ , nums]
#c_matrix <- cor(numeric_features, use="all.obs", method="spearman")
#corrplot(c_matrix, method = "circle")

#http://datascience.stackexchange.com/questions/893/how-to-get-correlation-between-two-categorical-variable-and-a-categorical-variab
#cor(metrics$minor_contributors, metrics$implicated)

table(metrics$implicated)

#One-way ANOVA test
aov1 = aov(metrics$commit_ownership ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$line_ownership_added ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$line_ownership_deleted ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$total_contributors ~ metrics$implicated)
summary(aov1)

#aov1 = aov(metrics$total_lines_added ~ metrics$implicated)
#summary(aov1)

#aov1 = aov(metrics$total_lines_deleted ~ metrics$implicated)
#summary(aov1)

aov1 = aov(metrics$minor_contributors ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$major_contributors ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$lines_added_minor_contributors ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$lines_added_major_contributors ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$lines_deleted_minor_contributors ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$lines_deleted_major_contributors ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$line_authorship ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$total_authors ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$file_size ~ metrics$implicated)
summary(aov1)

aov1 = aov(metrics$comment_to_code_ratio ~ metrics$implicated)
summary(aov1)

# Boxplots

boxplot <- ggplot(data=metrics, aes(factor(implicated), commit_ownership))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), line_ownership_added))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), line_ownership_deleted))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), total_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), minor_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), major_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), lines_added_minor_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), lines_added_major_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), lines_deleted_minor_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), lines_deleted_major_contributors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), line_authorship))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), total_authors))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), file_size))
boxplot + geom_boxplot()

boxplot <- ggplot(data=metrics, aes(factor(implicated), comment_to_code_ratio))
boxplot + geom_boxplot()
