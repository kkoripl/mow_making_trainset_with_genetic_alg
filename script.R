source("readingData.R")
source("treeClassification.R")
source('divideForTrainAndTest.R')

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
# zalozenie braku podzialu na zbior testujacy i uczacy, jakby co to mozna wyjac funkcj? createRpart na zewnatrz
tClass = treeClassification(dataset[,-ncol(dataset)], dataset$LABELS)

#Klasyczna macierz pomylek - w wierszach przewidywania, w kolumnach prawda
tConfusionMatrix = table(dataset$LABELS, tClass$prediction)