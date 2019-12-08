setwd("C:/Users/Amit Kulkarni/Documents/R Programming/packages")
#install.packages(c("SDMTools", "pROC", "Hmisc"))
library(SDMTools)
library(pROC)
library(Hmisc)



setwd("C:/Users/Amit Kulkarni/Documents/R Programming/Machine Learning/Project 7")
data = read.csv("cars.csv")
Transport.d = model.matrix(~Transport -1, data=data)
data1 = data.frame(data, Transport.d)
Gender.d = model.matrix(~Gender -1, data=data)
data1 = data.frame(data, Gender.d, Transport.d)
#data.frame
set.seed(111)
train.logit <- sample(c(1:nrow(data1)), round(nrow(data1) * 0.7,0), replace = FALSE)
LR.Train<-data1[train.logit,]
LR.Test<-data1[-train.logit,]
#Fit the Sigmoid function
logit.1<-TransportCar~Salary
logit.plot<- glm(logit.1, data=LR.Train, family=binomial())
summary(logit.plot)
pred.plot.logit <- predict.glm(logit.plot, newdata=LR.Test, type="response")
pred.plot.logit
LR.Test$pred<-pred.plot.logit
predprob<-exp(-8.51757+0.25059*80)/(1+exp(-8.51757+0.25059*80))
predprob
qplot(Salary, pred.plot.logit, data=LR.Test,color="TransportCar")
#All Variables
LRModel = glm(TransportCar ~ Age+Salary+Distance+Work.Exp, data=LR.Train, family= binomial) 
summary(LRModel)
predTest<-predict(LRModel, newdata=LR.Test, type="response")
table(LR.Test$TransportCar, predTest>0.5)
(111+12)/nrow(na.omit(LR.Test))
library(ROCR)
ROCRpred=prediction(predTest, LR.Test$TransportCar)
as.numeric(performance(ROCRpred,"auc")@y.values)
perf= performance(ROCRpred, "tpr","fpr")
plot(perf)

