library(readr)
Theft_Data <-
  read_csv("C:/Users/Nishank/Desktop/SNU/RStuff/Theft Data.csv")
View(Theft_Data)
names(Theft_Data)
library(tidyverse)
Theft_Data <- select(Theft_Data, (1:6))
names(Theft_Data)
table(Theft_Data$Name)
ggplot(Theft_Data, mapping = aes(x = Name, y = Total)) + geom_col() + facet_wrap(~
                                                                                   Year)

theftModel <- lm(Total ~ Year + Name, data = Theft_Data)
summary(theftModel)
plot(theftModel$residuals)
qqnorm(theftModel$residuals)
qqline(theftModel$residuals)
hist(theftModel$residuals)

library(randomForest)
Theft_Data$Name <- as.factor(Theft_Data$Name)
theftForest <-
  randomForest(Name ~ Theft + Year + Total + `Luggage Lifting` + `Pick Pocketing`,
               data = Theft_Data)
names(Theft_Data)
Theft_Data$`Luggage Lifting`
names(Theft_Data)
names(Theft_Data) <-
  c("name", "theft", "luggage", "pickpocketing", "total", "year")
theftForest <-
  randomForest(name ~ theft + year + total + luggage + pickpocketing, data = Theft_Data)
class(Theft_Data$name)
plot(theftForest)
summary(theftForest)
table(predict(theftForest))
barplot(table(predict(theftForest)))
