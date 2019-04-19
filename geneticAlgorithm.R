source("calculateGoalFunctionValue.R")

initializeChromosome = function(train, setProb) {
  trainCnt = nrow(train)
  
  #tworzy wektor logiczny wypełniony wartoscia FALSE o rozmiarze rownym rozmiarowi zbioru trenujacego
  chromosome = logical(trainCnt)
  
  #losowanie indeksów, które mają zostać ustawione na jeden i ustawienie
  setInidices = sample(trainCnt, round(trainCnt*setProb))
  chromosome[setInidices] =  TRUE
  
  return(chromosome)
}

initializeChromosomes = function(train, setProb, n) {
  trainCnt = nrow(train)
  chromosomes =  matrix(nrow = n, ncol = trainCnt)
  for (i in 1:n) {
    chromosomes[i,]= initializeChromosome(train, setProb)
  }
  return(chromosomes)
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
  reducedTrain = train[chromosome,]
  labelsIdx = ncol(reducedTrain)
  tree = createRpartTree(trainData = reducedTrain[,-labelsIdx], labels = reducedTrain$LABELS)
  prediction = predict(tree, test[,-labelsIdx],type="class")
  tConfusionMatrix = table(test$LABELS, prediction)
  return(calculateGoalFunctionValue(tConfusionMatrix))
}