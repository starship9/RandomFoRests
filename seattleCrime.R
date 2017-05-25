library(readr)
Seattle_Police_Department_911_Incident_Response <-
  read_csv(
    "C:/Users/Nishank/Desktop/SNU/RStuff/Seattle_Police_Department_911_Incident_Response.csv"
  )
View(Seattle_Police_Department_911_Incident_Response)
str(Seattle_Police_Department_911_Incident_Response)
View(head(Seattle_Police_Department_911_Incident_Response))
barplot(Seattle_Police_Department_911_Incident_Response$`District/Sector`)
table(Seattle_Police_Department_911_Incident_Response$`District/Sector`)
barplot(table(
  Seattle_Police_Department_911_Incident_Response$`District/Sector`
))
ggplot(Seattle_Police_Department_911_Incident_Response,
       mapping = aes(x = `District/Sector`)) + geom_bar()
ggplot(
  Seattle_Police_Department_911_Incident_Response,
  mapping = aes(x = `District/Sector`, fill = `District/Sector`)
) + geom_bar()
class(Seattle_Police_Department_911_Incident_Response$`Event Clearance Description`)
Seattle_Police_Department_911_Incident_Response$`Event Clearance Description` <-
  as.factor(Seattle_Police_Department_911_Incident_Response$`Event Clearance Description`)
table(Seattle_Police_Department_911_Incident_Response$`Event Clearance Description`)
barplot(
  table(
    Seattle_Police_Department_911_Incident_Response$`Event Clearance Description`
  )
)
class(Seattle_Police_Department_911_Incident_Response$`Event Clearance Date`)
Seattle_Police_Department_911_Incident_Response$`Event Clearance Date` <-
  as.Date(Seattle_Police_Department_911_Incident_Response$`Event Clearance Date`)
barplot(table(
  Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`
))
Seattle_Police_Department_911_Incident_Response$`Event Clearance Group` <-
  as.factor(Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`)
barplot(table(
  Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`
))
ggplot(
  Seattle_Police_Department_911_Incident_Response,
  mapping = aes(x = Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`)
) + geom_bar()
mean(table(
  Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`
))
sum(table(
  Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`
))
kmeans(Seattle_Police_Department_911_Incident_Response$`District/Sector`,
       3,
       nstart = 20)
table(Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`)
table(Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`)
groupDF <-
  tbl_df(table(
    Seattle_Police_Department_911_Incident_Response$`Event Clearance Group`
  ))
head(groupDF)
mean(groupDF$n)
groupDF <- filter(groupDF, n > 31000)
dim(groupDF)
groupDF
ggplot(groupDF, mapping = aes(x = Var1, y = n)) + geom_col()
barplot(table(
  Seattle_Police_Department_911_Incident_Response$`District/Sector`
))
table(Seattle_Police_Department_911_Incident_Response$`District/Sector`)
table(Seattle_Police_Department_911_Incident_Response$`Hundred Block Location`)
mean(table(
  Seattle_Police_Department_911_Incident_Response$`Hundred Block Location`
))
Seattle_Police_Department_911_Incident_Response$`Event Clearance Code` <-
  as.numeric(Seattle_Police_Department_911_Incident_Response$`Event Clearance Code`)
Seattle_Police_Department_911_Incident_Response <-
  select(Seattle_Police_Department_911_Incident_Response,
         `CAD CDW ID`:`Incident Location`)
codeCluster <-
  kmeans(Seattle_Police_Department_911_Incident_Response$`Event Clearance Code`,
         3,
         nstart = 20)
codeCluster <-
  kmeans(Seattle_Police_Department_911_Incident_Response$`Event Clearance Code`,
         3,
         nstart = 20)
plot(
  Seattle_Police_Department_911_Incident_Response$`Event Clearance Code`,
  col = codeCluster$cluster
)
(tbl_df(
  table(
    Seattle_Police_Department_911_Incident_Response$`Event Clearance Code`
  )
))
set.seed(100)
sampleCrimeSet <-
  sample(
    nrow(Seattle_Police_Department_911_Incident_Response),
    nrow(Seattle_Police_Department_911_Incident_Response) * 0.7
  )
trainCrimeSet <-
  Seattle_Police_Department_911_Incident_Response[sampleCrimeSet, ]
testCrimeSet <-
  Seattle_Police_Department_911_Incident_Response[-sampleCrimeSet, ]
library(randomForest)
crimeModel <-
  randomForest( ~ `Event Clearance Group`, data = trainCrimeSet)
crimeModel <-
  randomForest(`Event Clearance Group` ~ ., data = trainCrimeSet)
crimeModel <-
  randomForest(`Event Clearance Group` ~ . - `Event Clearance Code`, data = trainCrimeSet)

