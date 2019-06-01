#' Wyliczanie funkcji dopasowania na podstawie Macierzy pomyłek
#'
#' @param binaryConfusionMatrix Otrzymana macierz pomyłek dla danego chromosomu
#'
#' @return Wartość funkcji dopasowania
#' @export
#'
calculateGoalFunctionValue = function(binaryConfusionMatrix){
  negativeAccurracy = calculateNegativeAccuracy(binaryConfusionMatrix)
  positiveAccuracy = calculatePositiveAccuracy(binaryConfusionMatrix)
  return(sqrt(negativeAccurracy*positiveAccuracy))
}


#' Wyliczanie dokładności klasy '1'
#'
#' @param binaryConfusionMatrix Otrzymana macierz pomyłek dla danego chromosomu
#'
#' @return Dokładność wyznaczania klasy '1'
#' @export
#'
calculateNegativeAccuracy = function(binaryConfusionMatrix){
  return(binaryConfusionMatrix[1,1]/(binaryConfusionMatrix[1,1] + binaryConfusionMatrix[2,1]))
}


#' Wyliczanie dokładności klasy '0'
#'
#' @param binaryConfusionMatrix Otrzymana macierz pomyłek dla danego chromosomu
#'
#' @return Dokładność wyznaczania klasy '0'
#' @export
#'
calculatePositiveAccuracy = function(binaryConfusionMatrix){
  return(binaryConfusionMatrix[2,2]/(binaryConfusionMatrix[2,2] + binaryConfusionMatrix[1,2]))
}
