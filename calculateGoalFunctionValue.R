calculateGoalFunctionValue = function(binaryConfusionMatrix){
  negativeAccurracy = calculateNegativeAccuracy(binaryConfusionMatrix)
  positiveAccuracy = calculatePositiveAccuracy(binaryConfusionMatrix)
  return(sqrt(negativeAccurracy*positiveAccuracy))
}

calculateNegativeAccuracy = function(binaryConfusionMatrix){
  return(binaryConfusionMatrix[1,1]/(binaryConfusionMatrix[1,1] + binaryConfusionMatrix[2,1]))
}

calculatePositiveAccuracy = function(binaryConfusionMatrix){
  return(binaryConfusionMatrix[2,2]/(binaryConfusionMatrix[2,2] + binaryConfusionMatrix[1,2]))
}