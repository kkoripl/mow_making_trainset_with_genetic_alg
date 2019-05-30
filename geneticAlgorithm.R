source("calculateGoalFunctionValue.R")
source("createRpartTree.R")
source("crossover.R")
source("graphsSaver.R")

if (!require("gtools")) {
  install.packages("gtools")
}
library(gtools)

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
  saveAccuraciesPlot(posAccInEpochs, negAccInEpochs, epochs, plotIdx, "accuracies_plot_")
  saveBestEvaluationValuesPlot(bestValuesInEpochs, epochs, plotIdx, "best_evaluations_in_epochs_")
  saveTrainsetExamplesCounts(train$LABELS, train[bestSet, ]$LABELS, plotIdx, "trainset_examples_counts_")
  return(list(bestSet,posAccInEpochs,negAccInEpochs,bestValuesInEpochs))
}

initializeChromosomes = function(size, setProb, n, equalExamplesCount=FALSE, labels=NULL) {
  chromosomes =  matrix(nrow = n, ncol = size)
  for (i in 1:n) {
    chromosomes[i,]= initializeChromosome(size, setProb, equalExamplesCount, labels)
  }
  return(chromosomes)
}

initializeChromosome = function(size, setProb) {
  
  #tworzy wektor logiczny wypełniony wartoscia FALSE o rozmiarze rownym rozmiarowi zbioru trenujacego
  chromosome = logical(size)
  
  #losowanie indeksów, które mają zostać ustawione na jeden i ustawienie
  setInidices = sample(size, round(size*setProb))
  chromosome[setInidices] = TRUE
  
  return(chromosome)
}

initializeChromosome = function(size, setProb, equalExamplesCount=FALSE, labels=NULL) {
  
  chromosome = logical(size)
  #tworzy wektor logiczny wypełniony wartoscia FALSE o rozmiarze rownym rozmiarowi zbioru trenujacego
  if(equalExamplesCount) {
    positiveIndices= which(labels==1)
    negativeIndices= which(labels==0)
    
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

valuateChromosomes = function(chromosomes, train, test){
  chromosomesNumber = nrow(chromosomes)
  goalFuncValues = integer(chromosomesNumber)
  for (i in 1:chromosomesNumber) {
    goalFuncValues[i]= valuateChromosome(chromosomes[i,], train, test)
    print(goalFuncValues[i])
  }
  return(goalFuncValues)
}

valuateChromosome = function(chromosome, train, test) {
  tConfusionMatrix = buildConfusionMatrix(train[chromosome,], test)
  return(calculateGoalFunctionValue(tConfusionMatrix))
}

buildConfusionMatrix = function(train, test){
  labelsIdx = ncol(train)
  tree = createRpartTree(trainData = train[,-labelsIdx], labels = train$LABELS)
  prediction = predict(tree, test[,-labelsIdx],type="class")
  return(table(test$LABELS, prediction))
}

crossChromosomes = function(chromosome1, chromosome2) {
  size = length(chromosome1)
  crosscutPoint = round(size/2)
  offspring1 = c(chromosome1[1:crosscutPoint], chromosome2[(crosscutPoint+1):size])
  offspring2 = c(chromosome2[1:crosscutPoint], chromosome1[(crosscutPoint+1):size])
  return(list(offspring1, offspring2))
}

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

countPosAndNegAccuracies = function(train, test){
  conf = buildConfusionMatrix(train, test)
  return(list(calculatePositiveAccuracy(conf), calculateNegativeAccuracy(conf)))
}