calculateGoalFunctionValue = function(binaryConfusionMatrix){
  negativeAccurracy = binaryConfusionMatrix[1,1]/(binaryConfusionMatrix[1,1] + binaryConfusionMatrix[2,1])
  positiveAccuracy = binaryConfusionMatrix[2,2]/(binaryConfusionMatrix[2,2] + binaryConfusionMatrix[1,2])
  return(sqrt(negativeAccurracy*positiveAccuracy))
}