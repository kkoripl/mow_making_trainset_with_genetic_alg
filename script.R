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

#zapewnienie powtarzalnosci wynik?w
set.seed(10)

#podzial na czesc uczaca i testowa
testPart = 0.2
result = divideForTrainAndTest(dataset, testPart)
test = result[[1]]
train = result[[2]]

#Kontrolne ustalenie wartości f. celu przy użyciu pełnego zbioru uczącego
allSetChromosome = !logical(nrow(train))
wholeTrainGoalFuncValue = valuateChromosome(allSetChromosome, train, test)
print(c("Goal func value with whole train set: ", wholeTrainGoalFuncValue))

testParticularParamInfluence(train, test, c(3,5,10,15,20), 'Population_size')
testParticularParamInfluence(train, test, c(TRUE, FALSE), 'Equal_examples_count')
testParticularParamInfluence(train, test, c(0.1,0.27,0.5,0.8,1), 'Start_bits_set')



