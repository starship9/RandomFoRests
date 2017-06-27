library(readr)

#reading in the data
Theft_Data <-
  read_csv("C:/Users/Nishank/Desktop/SNU/RStuff/Theft Data.csv")

#viewing the data read in
View(Theft_Data)

#viewing the field names of the data
names(Theft_Data)

#library for plotting and data manipulation
library(tidyverse)

#selecting required columns
Theft_Data <- select(Theft_Data, (1:6))

#selecting only complete rows
Theft_Data <- Theft_Data[complete.cases(Theft_Data),]

#converting the names into factor variables
Theft_Data$name <- as.factor(Theft_Data$name)

#dividing data into training/testing sets
theftTrain <- Theft_Data[1:499, -1]
theftTest <- Theft_Data[500:712,-1]
theftTrainNames <- Theft_Data[1:499, 1]
theftTestNames <- Theft_Data[500:712, 1]

#Theft_Data$year <- as.factor(Theft_Data$year)
names(Theft_Data)
names(Theft_Data) <-
  c("name", "theft", "luggage", "pickpocketing", "total", "year")

#For reproducibility
set.seed(123)
table(Theft_Data$name)

#basic plot for number of crime instances per district across the years
g <-
  ggplot(Theft_Data, mapping = aes(x = name, y = total)) + geom_col() + facet_wrap(~
                                                                                     year)
#Added this since the basic ggplot graph is a bit too congested to make stuff out
plotly::ggplotly(g)

#trying out linear regression
theftModel <-
  lm(total ~ ., data = training)
summary(theftModel)

#plotting error per district name
plot(theftModel$residuals)

#checking if the data is normally distributed
qqnorm(theftModel$residuals)
qqline(theftModel$residuals)
hist(theftModel$residuals)

#predicting values using the testing data set and the model defined above
lmPred <- predict(theftModel, newdata = testing)
#Linear regression plot
plot(predict(theftModel))
#table(lmPred, theftTest$total)

#library for using functions pertaining to random forests
library(randomForest)

#using a random forest to predict crime 
theftForest <-
  randomForest(as.factor(name) ~ .,
               data = training)
#class(Theft_Data$name)

#viewing the results of the random forest
theftForest

#plotting the forest
plot(theftForest)
summary(theftForest)
table(predict(theftForest))

#basic plot of the results returned
barplot(table(predict(theftForest)))

#using the random forest model to predict new values using the testing data
newPred <- predict(theftForest, newdata = testing)
#table(newPred, testing$name)

#saving the results in a data frame
forestDF <- tbl_df(table(predict(theftForest)))
names(forestDF)
#Random forest plot
plotly::ggplotly(ggplot(forestDF, mapping = aes(x = Var1, y = n)) + geom_col())

# library(class)
# knnPredictions <-
#  knn(
#    train = theftTrain,
#    cl = theftTrainNames,
#    k = 27,
#    test = theftTest
#  )

#Loading caret for using the knn algorithm
library(caret)

#partitioning the data into training/testing sets with a 70/30 split
intrain <-
  createDataPartition(y = Theft_Data$name, p = 0.7, list = FALSE)
training <- Theft_Data[intrain,]
testing <- Theft_Data[-intrain,]
# training$year <- as.factor(training$year)
# testing$year <- as.factor(testing$year)
# dim(training)
# dim(testing)
# anyNA(Theft_Data)
summary(Theft_Data)
trctrl <-
  trainControl(method = "repeatedcv",
               number = 10,
               repeats = 3)

#for reproducibility
set.seed(123)

#training the knn model
knnFit <-
  caret::train(
    name ~ .,
    data = training,
    method = "knn",
    trControl = trctrl,
    preProcess = c("center", "scale"),
    tuneLength = 10
  )

#viewing the newly trained model
knnFit
summary(knnFit)

#predicting using the above model
testPred <- predict(knnFit, newdata = testing)
testPred

#viewing the confusion matrix
confusionMatrix(testPred, testing$name)

#plotting the model
plot(knnFit)

#viewing the results of the results of the above model
table(testPred)

#storing the results in a data frame
knnPredDF <- tbl_df(table(testPred))
names(knnPredDF)

#plotting the results of the above model
predG <-
  ggplot(knnPredDF, mapping = aes(x = testPred, y = n)) + geom_col()

#Caret::knn plot
plotly::ggplotly(predG)

#dim(theftTest)

#rounding off the results made by the previous linear regression model
# lmPred <- ceiling(lmPred)
# theftTest$diff <-  abs(theftTest$total - lmPred)
# plot(theftTest$diff)
# hist(theftTest$diff)
# plot(theftTest$total, lmPred)
# class(training$year)

#Adding an alternative linear model, ony using the name of the region and the year as predictors
testLMModel <- lm(total ~ name + as.numeric(year), data = training)
summary(testLMModel)
plot(testLMModel$residuals)
qqnorm(testLMModel$residuals)
qqline(testLMModel$residuals)
hist(testLMModel$residuals)

#predicting using the newly trained linear regression model on the testing data
testLMPred <- predict(testLMModel, newdata = testing)
# head(testLMPred, 5)
# head(testing$total, 5)

#plotting the actual vs predicted values
plot(testLMPred, testing$total)


#for 2017 data

#reading in a data frame with only 2017 data
theft2017 <-
  read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/Theft2017.csv",
           stringsAsFactors = FALSE)
head(theft2017)
theft2017$name <- as.factor(theft2017$name)

#using the year 2017
testing$year <- '2017'
#testing$year <- as.factor(testing$year)

#predicting using the new regression model
testLMPred <- predict(testLMModel, newdata = testing)

#storing the predictions with the 2017 data
theft2017$predictedTotal <- testLMPred
head(theft2017)

#rounding off predicted values
theft2017$predictedTotal <- ceiling(abs(theft2017$predictedTotal))
plot(theft2017$name, theft2017$predictedTotal)

#plotting the predicted values
plot2017 <-
  ggplot(theft2017, mapping = aes(x = name, y = predictedTotal)) + geom_col()
plotly::ggplotly(plot2017)

#Original lm being stored in the data frame
theft2017$lmPred <- lmPred

#plotting the results of the original linear regression model
plot2017LM <-
  ggplot(theft2017, mapping = aes(x = name, y = lmPred)) + geom_col()
plotly::ggplotly(plot2017LM)

# stepTheft <- step(theftModel)
# summary(stepTheft)
# stepPred <- predict(stepTheft, newdata = testing)
# head(stepPred)
# head(testing$total)

#storing the results of the random forest predictions into the data frame
theft2017$forestPred <- newPred

#creating a separate data frame, needed for power BI
theft2017RF <- tbl_df(table(theft2017$forestPred))
#head(theft2017RF)
#tail(theft2017RF)

# testForest <-
#   randomForest(total ~ as.factor(name) + year, data = training)
# names(training)
# class(training$name)