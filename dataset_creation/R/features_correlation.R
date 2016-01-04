# Import libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(corrgram)
library(caret)

add2 <- function(x) {
  x + 2
}

# Import dataset
metrics <-read.csv(file="../lucene/dataset/lucene_metrics.csv",head=TRUE,sep=",")
#metrics <-read.csv(file="../camel/dataset/camel_metrics.csv",head=TRUE,sep=",")

corrgram(metrics[,c(-1,-2,-ncol(metrics))])

#head(metrics[,c(-1,-2,-ncol(metrics))])

cors = cor(metrics[,c(-1,-2,-ncol(metrics))])
hc = findCorrelation(cors, cutoff=0.6)
hc = sort(hc)
hc = sapply(hc, add2, simplify = "array") #To consider sha and file
non_redundant_metrics = metrics[,-c(hc)]
head(non_redundant_metrics)
