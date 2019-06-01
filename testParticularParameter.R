epochs = 20
setProb = 0.8 
mutateProb = 0.1  
bitsToMutate = 3 
populationSize = 10
equalExamplesCount = FALSE


testParticularParamInfluence = function(train, test, paramValues, paramName){
  
  posAccuracies = matrix(nrow=length(paramValues), ncol=epochs)
  negAccuracies = matrix(nrow=length(paramValues), ncol=epochs)
  bestValuesInEpochs = matrix(nrow=length(paramValues), ncol=epochs)
  bestSetSizes = matrix(nrow=length(paramValues), ncol=2)
  
  plotName =''
  
  for(i in 1:length(paramValues)){
    switch(paramName, 
           'Start_bits_set'={
             setProb = paramValues[i]
             print(c(" ===============> Start testing start bits set percent:", setProb))
           },
           'Population_size'={
             populationSize = paramValues[i]
             print(c(" ===============> Start testing population size:", populationSize))   
           },
           'Equal_examples_count'={
             equalExamplesCount = paramValues[i]
             print(c(" ===============> Start testing equal examples count", equalExamplesCount))   
           },
           {
             print('Testing nothing')
           }
    )
    bestSetData = findOptimumSubset(train, test, epochs, setProb, mutateProb,  bitsToMutate, populationSize, equalExamplesCount, 
                                    plotIdx=paste(paramName,"_",as.character(paramValues[i])))
    posAccuracies[i,] = bestSetData[[2]]
    negAccuracies[i,] = bestSetData[[3]]
    bestValuesInEpochs[i,] = bestSetData[[4]]
  }
  plotPosAccuracyPlot(posAccuracies, paramValues, paramName, epochs)
  plotNegAccuracyPlot(negAccuracies, paramValues, paramName, epochs)
  plotBestValuesPlot(bestValuesInEpochs, paramValues, paramName, epochs)
}



testEpochsInfluence = function(train, test, epochsParams){
  
  for(i in 1:length(epochsParams)){
    epochs = epochsParams[i]
    print(c(" ===============> Start testing epochs:", as.character(epochs)))
    bestSetData = findOptimumSubset(train, test, epochs, setProb, mutateProb, bitsToMutate, populationSize, equalExamplesCount, 
                                    plotIdx=paste('epochs',"_",as.character(epochs)))
  }
}



plotPosAccuracyPlot = function(posAccuracies, paramValues, paramName, epochs){
  saveParticularParamTestsValueInEpoch(posAccuracies, 
                                       paramValues,
                                       epochs, 
                                       paste('Dokladnosc dla pozytywnych'), 
                                       plotIdx = '', 
                                       plotName = paste('T_Positive_Accuracies_in_Epochs_by_',paramName))
}


plotNegAccuracyPlot = function(negAccuracies, paramValues, paramName, epochs){
  saveParticularParamTestsValueInEpoch(negAccuracies, 
                                       paramValues,
                                       epochs, 
                                       paste('Dokladnosc dla negatywnych'), 
                                       plotIdx = '', 
                                       plotName = paste('T_Negative_Accuracies_in_Epochs_by_',paramName))
}


plotBestValuesPlot = function(bestValuesInEpochs, paramValues, paramName, epochs){
  saveParticularParamTestsValueInEpoch(bestValuesInEpochs, 
                                       paramValues,
                                       epochs, 
                                       paste('Najlepsza wartosc funkcji kosztu'), 
                                       plotIdx = '', 
                                       plotName = paste('T_Best_eval_values_in_Epochs_by_',paramName))
} 