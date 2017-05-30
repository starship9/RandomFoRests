#Source: "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
library(readr)
winequality_white <-
  read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/winequality-white.csv",
           sep = ";")
head(winequality_white)
names(winequality_white)
dim(winequality_white)

#plotting quality distribution
barplot(table(winequality_white$quality))

#classifying taste based on the quality
winequality_white$taste <-
  ifelse(winequality_white$quality < 6, 'bad', 'good')
winequality_white$taste[winequality_white$quality == 6] <- 'normal'
winequality_white$taste <- as.factor(winequality_white$taste)

table(winequality_white$taste)

set.seed(123)

#70/30 split for the training and testing data sets
sampleDataSet <-
  sample(nrow(winequality_white), nrow(winequality_white) * 0.7)
trainDataSet <- winequality_white[sampleDataSet, ]
test <- winequality_white[-sampleDataSet, ]

library(randomForest)

#creating the model using the taste and quality attributes
model <- randomForest(taste ~ . - quality, data = trainDataSet)
model

pred <- predict(model, newdata = test)
table(pred, test$taste)

modelAccuracy <-
  (table(pred, test$taste)[1] + table(pred, test$taste)[5] + table(pred, test$taste)[9]) /
  nrow(test)
modelAccuracy

#Error vs the number of trees generated in the forest
plot(model)

#test values based off the model
table(predict(model))

#bargraph of the above values
barplot(table(predict(model)))

library(ggplot2)

#old visualisation
ggplot(winequality_white, mapping = aes(x = taste, fill = taste)) + geom_bar()
library(dplyr)

#new visualisation
newDF <- tbl_df(table(predict(model)))
ggplot(newDF, mapping = aes(x = Var1, y = n, fill = Var1)) + geom_col()

# Plotting predicted vs actual values
ggplot(tbl_df(table(pred, test$taste)), mapping = aes(x = pred, y = n, fill = pred)) + geom_col() + facet_wrap( ~
                                                                                                                  Var2)

wineLinearModel <-
  lm(
    quality ~ alcohol + pH + sulphates + fixed.acidity + volatile.acidity +
      residual.sugar,
    data = winequality_white
  )
summary(wineLinearModel)
plot(wineLinearModel$residuals)
hist(wineLinearModel$residuals)
qqnorm(wineLinearModel$residuals)
qqline(wineLinearModel$residuals)
