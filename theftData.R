library(readr)
Theft_Data <-
  read_csv("C:/Users/Nishank/Desktop/SNU/RStuff/Theft Data.csv")
View(Theft_Data)
names(Theft_Data)
library(tidyverse)
Theft_Data <- select(Theft_Data, (1:6))
names(Theft_Data)
names(Theft_Data) <-
  c("name", "theft", "luggage", "pickpocketing", "total", "year")

#For reproducibility
set.seed(123)
table(Theft_Data$name)
g <-
  ggplot(Theft_Data, mapping = aes(x = name, y = total)) + geom_col() + facet_wrap( ~
                                                                                      year)
#Added this since the basic ggplot graph is a bit too congested to make stuff out
plotly::ggplotly(g)

#trying out linear regression
theftModel <-
  lm(total ~ ., data = theftTrain)
summary(theftModel)
plot(theftModel$residuals)
qqnorm(theftModel$residuals)
qqline(theftModel$residuals)
hist(theftModel$residuals)
lmPred <- predict(theftModel, newdata = theftTest)
#Linear regression plot
plot(predict(theftModel))

table(lmPred, theftTest$total)

library(randomForest)
theftForest <-
  randomForest(as.factor(name) ~ .,
               data = theftTrain)
class(Theft_Data$name)
theftForest
plot(theftForest)
summary(theftForest)
table(predict(theftForest))
barplot(table(predict(theftForest)))

newPred <- predict(theftForest, newdata = theftTest)
table(newPred, theftTest$name)


forestDF <- tbl_df(table(predict(theftForest)))
names(forestDF)
#Random forest plot
plotly::ggplotly(ggplot(forestDF, mapping = aes(x = Var1, y = n)) + geom_col())

Theft_Data <- Theft_Data[complete.cases(Theft_Data), ]
Theft_Data$name <- as.factor(Theft_Data$name)
theftTrain <- Theft_Data[1:499,-1]
theftTest <- Theft_Data[500:712, -1]
theftTrainNames <- Theft_Data[1:499, 1]
theftTestNames <- Theft_Data[500:712, 1]

library(class)
knnPredictions <-
  knn(
    train = theftTrain,
    cl = theftTrainNames,
    k = 27,
    test = theftTest
  )

library(caret)
intrain <-
  createDataPartition(y = Theft_Data$name, p = 0.7, list = FALSE)
training <- Theft_Data[intrain, ]
testing <- Theft_Data[-intrain, ]
dim(training)
dim(testing)
anyNA(Theft_Data)
summary(Theft_Data)
trctrl <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 3)
set.seed(123)
knnFit <-
  caret::train(
    name ~ .,
    data = training,
    method = "knn",
    trControl = trctrl,
    preProcess = c("center", "scale"),
    tuneLength = 10
  )
knnFit
summary(knnFit)
testPred <- predict(knnFit, newdata = testing)
testPred
confusionMatrix(testPred, testing$name)
plot(knnFit)

table(testPred)
knnPredDF <- tbl_df(table(testPred))
names(knnPredDF)
predG <-
  ggplot(knnPredDF, mapping = aes(x = testPred, y = n)) + geom_col()

#Caret::knn plot
plotly::ggplotly(predG)

dim(theftTest)
lmPred <- ceiling(lmPred)
theftTest$diff <-  abs(theftTest$total - lmPred)
plot(theftTest$diff)
hist(theftTest$diff)
plot(theftTest$total, lmPred)

#Adding a test linear model, ony using the name of the region and the year as predictors
testLMModel <- lm(total ~ name + year, data = training)
summary(testLMModel)
plot(testLMModel$residuals)
qqnorm(testLMModel$residuals)
qqline(testLMModel$residuals)
hist(testLMModel$residuals)
testLMPred <- predict(testLMModel, newdata = testing)
head(testLMPred, 5)
head(testing$total, 5)
plot(testLMPred, testing$total)
