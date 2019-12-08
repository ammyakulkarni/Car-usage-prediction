setwd("C:/Users/Amit Kulkarni/Documents/R Programming/packages")
#install.packages("ipred")
#library(ipred)
#library(rpart)
setwd("C:/Users/Amit Kulkarni/Documents/R Programming/Machine Learning/Project 7")
bagdata=read.csv("cars.csv")
Transport.d = model.matrix(~Transport -1, data=bagdata)
Gender.d = model.matrix(~Gender -1, data=bagdata)
bagdata = data.frame(bagdata, Gender.d, Transport.d)
bagdata= bagdata[,-2:-4,-9]
bagdata = bagdata[,-6]
View(bagdata)
library(caTools)
set.seed(123)
split=sample.split(bagdata$TransportCar, SplitRatio = 0.80)
bagtrain=subset(bagdata, split==T)
bagtest=subset(bagdata, split==F)
?bagging
Carbagging<- bagging(bagtrain$TransportCar~ Age+Salary+Work.Exp+license, data=bagtrain,
                     control=rpart.control(maxdepth = 5, minsplit = 15))
bagtest$pred.class<-predict(Carbagging, bagtest)
table(bagtest$TransportCar, bagtest$pred.class>0.75)
6/7

