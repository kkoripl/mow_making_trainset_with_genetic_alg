#Autorzy - Konrad Frąc i Łukasz Kotlewski

source("calculateGoalFunctionValue.R")
source("createRpartTree.R")
source("crossover.R")
source("graphsSaver.R")

if (!require("gtools")) {
  install.packages("gtools")
}
library(gtools)


#' Algorytm genetyczny
#'
#' @param train Poczatkowy zbior uczacy
#' @param test Zbior testowy
#' @param epochs Liczba epok dzialania algorytmu
#' @param setProb Procent wielkosci poczatkowego zbioru uczacego, jakim ma byc rozpoczety zbior treningowy w chromosomach pierwszego pokolenia
#' @param mutateProb Prawdopodobienstwo mutacji
#' @param bitsToMutate Liczba bitow chromosomu do zmutowania
#' @param populationSize Liczebnosc populacji
#' @param equalExamplesCount Flaga czy w zbiorach treningowch pierwszego pokolenia liczebnosc klas powinna byc identyczna
#' @param plotIdx Oznaczenie tworzonych wykresow, by rozroznic je wsrod podobnych
#'
#' @return Lista 1: Najlepszy znaleziony zbior treningowy, 2 i 3: Odpowiednio dokladnosc dla klasy '1' i '0', 4: Najlepsze wartosci funkcji przystosowania na przestrzeni epok
#' @export
findOptimumSubset = function(train, test, epochs, setProb, mutateProb, bitsToMutate, populationSize, equalExamplesCount, plotIdx) {
  bestValuesInEpochs = c(epochs);
  posAccInEpochs = c(epochs);
  negAccInEpochs = c(epochs);
  bestSet = NULL
  bestValue = 0
  wholeTrainSize = nrow(train)
  currentPopulation = initializeChromosomes(wholeTrainSize, setProb, populationSize, equalExamplesCount, train$LABELS)
  allPairs = combinations(populationSize,2)
  pairsCnt = nrow(allPairs)
  
  for (i in 1:epochs) {
    allOffsprings = crossover(parentsTable=currentPopulation, 
                              pairsCnt=round(nrow(currentPopulation)/2),
                              crossPointsCnt=10)
    allOffsprings = mutateChromosomes(allOffsprings, bitsToMutate, mutateProb)
    currentPopulation = mutateChromosomes(currentPopulation, bitsToMutate, mutateProb)
    print(c("epoch nr", i))
    print("current Population:")
    currentRates = valuateChromosomes(currentPopulation, train, test)
    print(("offsprings"))
    offspringsRates = valuateChromosomes(allOffsprings, train, test)
    mergedRates = c(currentRates,offspringsRates)
    mergedPopulation = rbind(currentPopulation, allOffsprings)
    maxCurrentRate = max(mergedRates)
    print(c("max goal value in current epoch:", maxCurrentRate))
    if(bestValue < maxCurrentRate){
      bestValue = maxCurrentRate
      bestSet = mergedPopulation[which.max(mergedRates),]
      print(c("best set changed with val: ", bestValue))
    }
    bestValuesInEpochs[i] = bestValue
    accuracies = countPosAndNegAccuracies(train[bestSet, ], test)
    posAccInEpochs[i] = accuracies[[1]]
    negAccInEpochs[i] = accuracies[[2]]
    print(c("max goal value after current epoch:", bestValue))
    currentPopulation = mergedPopulation[findNBestIdxs(mergedRates, populationSize),]

  }
  
  #Utworzenie chromosomu z calego zbioru treningowego i jego ocena dla celow porownawczych
  allSetChromosome = !logical(nrow(train))
  wholeTrainGoalFuncValue = valuateChromosome(allSetChromosome, train, test)
  
  saveAccuraciesPlot(posAccInEpochs, negAccInEpochs, epochs, plotIdx, "accuracies_plot_")
  saveBestEvaluationValuesPlot(bestValuesInEpochs, wholeTrainGoalFuncValue,epochs, plotIdx, "best_evaluations_in_epochs_")
  saveTrainsetExamplesCounts(train$LABELS, train[bestSet, ]$LABELS, plotIdx, "trainset_examples_counts_")
  return(list(bestSet,posAccInEpochs,negAccInEpochs,bestValuesInEpochs))
}


#' Inicjalizacja chromosomow
#'
#' @param size Dlugosc chromosomu
#' @param setProb Procent poczatkowo ustawionych bitow
#' @param n Liczebnosc populacji
#' @param equalExamplesCount Flaga czy liczebnosc klas w nastawionych bitach winna byc identyczna
#' @param labels Etykiety klas
#'
#' @return Pierwsze pokolenie chromosomow
#' @export
initializeChromosomes = function(size, setProb, n, equalExamplesCount=FALSE, labels=NULL) {
  chromosomes =  matrix(nrow = n, ncol = size)
  for (i in 1:n) {
    chromosomes[i,]= initializeChromosome(size, setProb, equalExamplesCount, labels)
  }
  return(chromosomes)
}


#' Inicjalizacja pojedynczego chromosomu
#'
#' @param size Dlugosc chromosomu
#' @param setProb Procent poczatkowo ustawionych bitow
#' @param equalExamplesCount Flaga czy liczebnosc klas w nastawionych bitach winna byc identyczna
#' @param labels Etykiety klas
#'
#' @return Zainicjowany chromosom
#' @export
initializeChromosome = function(size, setProb, equalExamplesCount=FALSE, labels=NULL) {
  
  chromosome = logical(size)
  #tworzy wektor logiczny wypełniony wartoscia FALSE o rozmiarze rownym rozmiarowi zbioru trenujacego
  if(equalExamplesCount) {
    positiveIndices = which(labels==1)
    negativeIndices = which(labels==0)
    
    positiveSize = length(positiveIndices)
    negativeSize = length(negativeIndices)
    
    negativeSetIndicesSubindices = sample(negativeSize, round(negativeSize*setProb*positiveSize/negativeSize))
    positiveSetIndicesSubindices = sample(positiveSize, round(positiveSize*setProb))
    
    setInidices = c(positiveIndices[positiveSetIndicesSubindices], negativeIndices[negativeSetIndicesSubindices])
    
  } else {
    #losowanie indeksów, które mają zostać ustawione na jeden i ustawienie
    setInidices = sample(size, round(size*setProb))
  }

  chromosome[setInidices] = TRUE
  
  return(chromosome)
}


#' Funkcja zwracajaca oceny danych chromosomow
#'
#' @param chromosomes Chromosomy do oceny
#' @param train Maksymalny zbior trenujacy
#' @param test Zbior testujacy
#'
#' @return Oceny chromosomow
#' @export
valuateChromosomes = function(chromosomes, train, test){
  chromosomesNumber = nrow(chromosomes)
  goalFuncValues = integer(chromosomesNumber)
  for (i in 1:chromosomesNumber) {
    goalFuncValues[i]= valuateChromosome(chromosomes[i,], train, test)
    print(goalFuncValues[i])
  }
  return(goalFuncValues)
}


#' Funkcja oceniajaca pojedynczy chromosom
#'
#' @param chromosome Chromosom do oceny
#' @param train Maksymalny zbior trenujacy
#' @param test Zbior testujacy
#'
#' @return Ocena chromosomu
#' @export
valuateChromosome = function(chromosome, train, test) {
  tConfusionMatrix = buildConfusionMatrix(train[chromosome,], test)
  return(calculateGoalFunctionValue(tConfusionMatrix))
}


#' Tworzenie macierzy pomyłek przy pomocy aktualnie wybranego zbioru treningowego
#'
#' @param train Aktualnie wybrany zbior treningowy
#' @param test Zbior testowy
#'
#' @return Aktualna macierz pomylek
#' @export
buildConfusionMatrix = function(train, test){
  labelsIdx = ncol(train)
  tree = createRpartTree(trainData = train[,-labelsIdx], labels = train$LABELS)
  prediction = predict(tree, test[,-labelsIdx],type="class")
  return(table(test$LABELS, prediction))
}


#' Funkcja odnajdujaca N indeksow najlepiej przystosowanych chromosomow w pokoleniu i jego potomkach
#'
#' @param vector Pokolenie oraz jego potomkowie
#' @param n Liczba indeksow najlepszych chromosomow do znalezienia
#'
#' @return Indeksy N najlepiej przystosowanych chromosomow
#' @export
findNBestIdxs= function(vector, n) {
  vectorLength = length(vector)
  lastNotTakenIndex = vectorLength-n
  edgePoint = sort(vector, partial=lastNotTakenIndex)[lastNotTakenIndex]
  bestIdxs = which(vector > edgePoint)
  belowEdgePointCnt = length(bestIdxs)
  if(belowEdgePointCnt!=n){
    bestIdxs = c(bestIdxs, rev(which(vector == edgePoint))[1:(n-belowEdgePointCnt)])
  }
  return(bestIdxs)
}


#' Mutacja chromosomow
#'
#' @param chromosomes Chromosomy do mutacji
#' @param bitsToMutInEach Liczba bitow do zmutowania
#' @param mutationProb Prawdopodobienstwo mutacji
#'
#' @return Zmutowane chromosomy
#' @export
mutateChromosomes = function(chromosomes, bitsToMutInEach, mutationProb){
  mutationProb = mutationProb * 100
  bitsInChromosome = ncol(chromosomes)
  for(i in 1:nrow(chromosomes)){
    if(mutationProb <= sample(100, 1)){
      bitsMutated = sample(bitsInChromosome, bitsToMutInEach)
      chromosomes[i, bitsMutated] = !chromosomes[i, bitsMutated]
    }
  }
  return(chromosomes)
}


#' Wyliczanie dokladnosci obu klas zbioru danych
#'
#' @param train Aktualnie wybrany zbior treningowy
#' @param test Zbior testowy
#'
#' @return Lista: 1 i 2: Odpowiednio dokladnosc wyznaczania klasy '1' i '0'
#' @export
countPosAndNegAccuracies = function(train, test){
  conf = buildConfusionMatrix(train, test)
  return(list(calculatePositiveAccuracy(conf), calculateNegativeAccuracy(conf)))
}