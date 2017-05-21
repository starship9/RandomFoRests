library(readr)
winequality_white <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/winequality-white.csv",sep=";")
head(winequality_white)
names(winequality_white)
dim(winequality_white)

barplot(table(winequality_white$quality))
winequality_white$taste<-ifelse(winequality_white$quality<6,'bad','good')
winequality_white$taste[winequality_white$quality==6]<-'normal'
winequality_white$taste<-as.factor(winequality_white$taste)

table(winequality_white$taste)

set.seed(123)
sampleDataSet<-sample(nrow(winequality_white),nrow(winequality_white)*0.7)
trainDataSet<-winequality_white[sampleDataSet,]
test<-winequality_white[-sampleDataSet,]

library(randomForest)
model<-randomForest(taste~.-quality,data = trainDataSet)
model

pred<-predict(model,newdata = test)
table(pred,test$taste)

modelAccuracy<-(370+193+515)/nrow(test)
modelAccuracy

plot(model)
