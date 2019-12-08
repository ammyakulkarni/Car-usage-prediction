setwd("C:/Users/Amit Kulkarni/Documents/R Programming/packages")
#install.packages("class")
#install.packages("plotrix")
library(tidyverse)
library(class)
library(dplyr)
library(caret) 
library(plotrix)
library(rattle) 
library(data.table) 
library(scales)
library(ineq) 
library(MASS)

setwd("C:/Users/Amit Kulkarni/Documents/R Programming/Machine Learning/Project 7")
knndata= read.csv("cars.csv")
str(knndata)
Transport.d = model.matrix(~Transport -1, data=knndata)
Gender.d = model.matrix(~Gender -1, data=knndata)
data1 = data.frame(knndata, Gender.d, Transport.d)
knnscaledate=data1[,-13]
knnscaledate=data1[,-2:-4,-9]
knnsclddata= knnscaledate[, -6]
normdata=scale(knnsclddata)
usuable.data=cbind(data1[,13], normdata)
usuable.data = as.data.frame(usuable.data)
library(caTools)
spl=sample.split(usuable.data$TransportCar, SplitRatio = 0.7)
knntrain=subset(usuable.data, spl==T)
knntest=subset(usuable.data, spl==F)
library(class)
pred3=knn(knntrain[,-10], knntest[,-10], knntrain[,10], k=19)
table.knn=table(knntest[,10], pred3)
sum(diag(table.knn))/sum(table.knn)



