#Boositng
setwd("C:/Users/Amit Kulkarni/Documents/R Programming/Machine Learning/Project 7")
xgbdata= read.csv("cars.csv")
Transport.d = model.matrix(~Transport -1, data=xgbdata)
Gender.d = model.matrix(~Gender -1, data=xgbdata)
xgbdata = data.frame(xgbdata, Gender.d, Transport.d)
View(xgbdata)
xgbdata=xgbdata[,-2:-4,-9]
xgbdata=xgbdata[,-6]
#library(caTools)
set.seed(123)
split=sample.split(xgbdata$TransportCar, SplitRatio = 0.80)
xgbtrain=subset(xgbdata, split==T)
xgbtest=subset(xgbdata, split==F)
View(xgbtrain)
dim(xgbtest)
sum(xgbtrain$TransportCar)
sum(xgbtest$TransportCar)
#install.packages("xgboost")
#library(xgboost)
classifier = xgboost(data = as.matrix(xgbtrain[,-9]), label=xgbtrain$TransportCar, nrounds=10)
y_pred = predict(classifier, newdata = as.matrix(xgbtest[,-9]))
y_pred=(y_pred>=0.5)
cm=table(xgbtest[,9], y_pred)
cm

#applying k-fold cross Validation
#install.packages("caret")
#library(caret)
folds=createFolds(xgbtrain$TransportCar, k=10)
cv=lapply(folds, function(x) {
  trainingfold= xgbtrain[-x, ]
  test_fold = xgbtrain[x, ]
  classifier = xgboost(data=as.matrix(xgbtrain[-9]), label=xgbtrain$TransportCar, nrounds = 10)
  y_pred=predict(classifier, newdata = as.matrix(test_fold[-9]))
  y_pred = (y_pred>=0.5)
  cm=table(test_fold[,9], y_pred)
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(accuracy)
})
accuracy=mean(as.numeric(cv))
test_fold
y_pred
