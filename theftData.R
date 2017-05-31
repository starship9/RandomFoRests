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

table(Theft_Data$name)
g<-ggplot(Theft_Data, mapping = aes(x = name, y = total, fill = name)) + geom_col() + facet_wrap(~year)

#Added this since the basic ggplot graph is a bit too congested to make stuff out
plotly::ggplotly(g)

theftModel <- lm(total ~ year + name + theft + luggage + pickpocketing, data = Theft_Data)
summary(theftModel)
plot(theftModel$residuals)
qqnorm(theftModel$residuals)
qqline(theftModel$residuals)
hist(theftModel$residuals)
predict(theftModel)
plot(predict(theftModel))

library(randomForest)
theftForest <-
  randomForest(as.factor(name) ~ theft + year + total + luggage + pickpocketing, data = Theft_Data)
class(Theft_Data$name)
plot(theftForest)
summary(theftForest)
table(predict(theftForest))
barplot(table(predict(theftForest)))
