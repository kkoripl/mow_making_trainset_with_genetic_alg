source("readingData.R")
source('divideForTrainAndTest.R')
source("geneticAlgorithm.R")

# Jesli nie masz pakietu rpart.plot to zainstaluj, pozniej wczytaj
if (!require("rpart.plot")) {
  install.packages("rpart.plot")
}
library(rpart)
library(rpart.plot)

#podzial na czesc uczaca i testowa
testPart = 0.2
result = divideForTrainAndTest(dataset, testPart)
test = result[[1]]
train = result[[2]]

findOptimumSubset(train, test, 50, 0.8)

