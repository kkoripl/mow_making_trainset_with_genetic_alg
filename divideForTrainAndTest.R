#' Podział na dane testujace i trenujace
#'
#' @param set Zbior danych do podzialu
#' @param testPart Procent jaki z calego zbioru ma zajac czesc testujaca
#'
#' @return Lista zbiorow - pod 1: testujaca, 2: trenujaca
#' @export
#'
divideForTrainAndTest = function(set, testPart){
  samplesCnt = nrow(set)
  testCnt = round(samplesCnt*testPart)
  idxs = sample(samplesCnt, testCnt)
  return(list(set[idxs,], set[-idxs,]))
}
