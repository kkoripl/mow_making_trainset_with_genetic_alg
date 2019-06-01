source("readingData.R")
source('divideForTrainAndTest.R')
source("geneticAlgorithm.R")
source("testParticularParameter.R")

if (!require("rpart.plot")) {
  install.packages("rpart.plot")
}

library(rpart)
library(rpart.plot)

#zapewnienie powtarzalnosci wynikow
set.seed(10)

dataset = readData('spliceDTrainKIS.dat')

#podzial na czesc uczaca i testowa
testPart = 0.2
result = divideForTrainAndTest(dataset, testPart)
test = result[[1]]
train = result[[2]]

#Kontrolne ustalenie wartości f. celu przy użyciu pełnego zbioru uczącego
allSetChromosome = !logical(nrow(train))
wholeTrainGoalFuncValue = valuateChromosome(allSetChromosome, train, test)
print(c("Goal func value with whole train set: ", wholeTrainGoalFuncValue))

testParticularParamInfluence(train, test, c(3,5,10,15), 'Population_size')
testParticularParamInfluence(train, test, c(TRUE, FALSE), 'Equal_examples_count')
testParticularParamInfluence(train, test, c(0.6,0.7,0.8,0.9), 'Start_bits_set')



