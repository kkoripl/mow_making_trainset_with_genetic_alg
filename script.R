source("readingData.R")
source("treeClassification.R")

# Jesli nie masz pakietu rpart.plot to zainstaluj, pozniej wczytaj
if (!require("rpart.plot")) {
  install.packages("rpart.plot")
}
library(rpart)
library(rpart.plot)

# tClass$tree ma informacje o wygenerowanym drzewie, tClass$prediction - predykcje
# zalozenie braku podzialu na zbior testujacy i uczacy, jakby co to mozna wyjac funkcjê createRpart na zewnatrz
tClass = treeClassification(dataset[,-ncol(dataset)], dataset$LABELS)

#Klasyczna macierz pomylek - w wierszach przewidywania, w kolumnach prawda
tConfusionMatrix = table(dataset$LABELS, tClass$prediction)