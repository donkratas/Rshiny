library(readxl)
library(dplyr)
library(data.table)
library(ROSE)

data <- read.csv2("train_sample.csv", sep = ",")


#---------------------CLEANING MSC DATA---------------------------------

data$attributed_time=NULL
data$is_attributed=as.factor(data$is_attributed)

set.seed(1234)

ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train_msc <- data[ind == 1, ]
test_msc <- data[ind==2, ]


under_data <- ovun.sample(is_attributed~., data= train_msc, method = "under", N=894)$data 
train_msc <- under_data[ ,-c(6)]

rm(data)
rm(under_data)

#---------------------Neural networks data---------------------------------


