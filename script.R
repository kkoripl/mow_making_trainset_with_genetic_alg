source("readingData.R")
source("treeClassification.R")
source('divideForTrainAndTest.R')
source("calculateGoalFunctionValue.R")
source("createRpartTree.R")

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

# tClass$tree ma informacje o wygenerowanym drzewie, tClass$prediction - predykcje
labelsIdx = ncol(dataset)
tree = createRpartTree(trainData = train[,-labelsIdx], labels = train$LABELS)


prediction = predict(tree, test[,-labelsIdx],type="class")

#Klasyczna macierz pomylek - w wierszach przewidywania, w kolumnach prawda
tConfusionMatrix = table(test$LABELS, prediction)

#Wyznaczanie wartosci funkcji celu
calculateGoalFunctionValue(tConfusionMatrix)