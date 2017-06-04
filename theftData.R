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
  ggplot(Theft_Data, mapping = aes(x = name, y = total, fill = name)) + geom_col() + facet_wrap(~
                                                                                                  year)

#Added this since the basic ggplot graph is a bit too congested to make stuff out
plotly::ggplotly(g)

theftModel <-
  lm(total ~ ., data = theftTrain)
summary(theftModel)
plot(theftModel$residuals)
qqnorm(theftModel$residuals)
qqline(theftModel$residuals)
hist(theftModel$residuals)
lmPred <- predict(theftModel, newdata = theftTest)
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
plotly::ggplotly(ggplot(forestDF, mapping = aes(x = Var1, y = n)) + geom_col())

Theft_Data <- Theft_Data[complete.cases(Theft_Data), ]
Theft_Data$name <- as.factor(Theft_Data$name)
theftTrain <- Theft_Data[1:499, ]
theftTest <- Theft_Data[500:712, ]
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
