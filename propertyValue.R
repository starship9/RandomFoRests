library(readr)
Property_stolen_recovered_Value_nature <- read_csv("C:/Users/Nishank/Desktop/SNU/RStuff/Property_stolen_recovered_Value_nature.csv")
names(Property_stolen_recovered_Value_nature)<-c("state","year","nature","stolen","recovered","stolenValue",
                                                 "recoveredValue")
names(Property_stolen_recovered_Value_nature)
Property_stolen_recovered_Value_nature$nature<-as.factor(Property_stolen_recovered_Value_nature$nature)
Property_stolen_recovered_Value_nature[is.na(Property_stolen_recovered_Value_nature)]<-0
propertySample<-select(Property_stolen_recovered_Value_nature,year,stolen,recovered,stolenValue,recoveredValue)
dim(propertySample)
dim(Property_stolen_recovered_Value_nature)
propertySample<-propertySample[1:3915,]
propertyTest<-select(Property_stolen_recovered_Value_nature,year,stolen,recovered,stolenValue,recoveredValue)
propertyTest<-propertyTest[3916:5594,]
testSet<-select(Property_stolen_recovered_Value_nature,nature)
dim(testSet)
testSet<-testSet[1:3915,]
predictions<-knn(train = propertySample, test = propertyTest, cl = testSet, k = 75)
