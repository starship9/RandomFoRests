library(readr)
Property_stolen_recovered_Value_nature <- read_csv("C:/Users/Nishank/Desktop/SNU/RStuff/Property_stolen_recovered_Value_nature.csv")
names(Property_stolen_recovered_Value_nature)<-c("state","year","nature","stolen","recovered","stolenValue",
                                                 "recoveredValue")
names(Property_stolen_recovered_Value_nature)
