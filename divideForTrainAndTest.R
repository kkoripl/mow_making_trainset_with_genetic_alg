divideForTrainAndTest = function(set, testPart){
  samplesCnt = nrow(set)
  testCnt = round(samplesCnt*testPart)
  idxs = sample(samplesCnt, testCnt)
  return(list(set[idxs,], set[-idxs,]))
} 