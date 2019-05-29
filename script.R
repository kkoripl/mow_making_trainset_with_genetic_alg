source("readingData.R")
source('divideForTrainAndTest.R')
source("geneticAlgorithm.R")
source("testParticularParameter.R")

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

#bestSet = findOptimumSubset(train, test, epochs=50, setProb=0.8, mutateProb=0.1,  bitsToMutate=3, population_size=3, plotIdx="_exampleIdx4")
testParticularParamInfluence(train, test, c(3,5,10,15,20), 'Population_size')
testParticularParamInfluence(train, test, c(0.1,0.27,0.5,0.8,1), 'Start_bits_set')
testEpochsInfluence(train, test, c(10,25,50,100))