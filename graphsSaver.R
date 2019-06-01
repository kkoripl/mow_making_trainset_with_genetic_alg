directory = "./graph/"
imgType = ".png"

saveBestEvaluationValuesPlot = function(bestValues, wholeTrainGoalFuncValue, epochs, plotIdx, plotName = NULL){
  png(returnPlotnameInPath(name = plotName, defaultPlotName = "best_evaluations_in_epochs_plot", idx = plotIdx))
  matplot(seq(1, epochs, 1), cbind(bestValues, rep(wholeTrainGoalFuncValue, epochs)),
       main = "Najlepsza wartosci funkcji kosztu",
       xlab = "Epoka",
       ylab = "Wartosc",
       type = "b",
       col = c("blue","red"))
  legend("topleft",legend=c("Najlepszy pozdbior","Caly zbior uczacy"), bty = "n", fill=c("blue","red"))
  dev.off()
}

saveTrainsetExamplesCounts = function(startLabels, endLabels, plotIdx, plotName = NULL){
  startCounts = table(startLabels)
  counts = matrix(c(startCounts, table(endLabels)), nrow=2)
  rownames(counts) = names(startCounts)
  colnames(counts) = c("Przed alg.", "Po alg.")
  png(returnPlotnameInPath(name = plotName, defaultPlotName = "trainset_examples_counts", idx = plotIdx))
  bp = barplot(counts, beside = TRUE,
          main = "Liczba przykladow w zbiorze uczacym",
          xlab = "Klasa",
          ylab = "Liczba przykladow",
          col=c("blue","red"),
          legend = rownames(counts))
  text(bp, 0, round(counts, 1),cex=1,pos=3, col='white') 
  dev.off()
}

saveAccuraciesPlot = function(positiveAcc, negativeAcc, epochs, plotIdx, plotName = NULL){
  png(returnPlotnameInPath(name = plotName, defaultPlotName = "accuracies_in_epochs_plot", idx = plotIdx))
  matplot(seq(1, epochs, 1), cbind(positiveAcc, negativeAcc),
       main = "Dokladnosc",
       xlab = "Epoka",
       ylab = "Wartosc",
       type = "l",
       col = c("blue","red"))
  legend("topleft",legend=c("pozytywne","negatywne"), bty = "n", fill=c("blue","red"))
  dev.off()
}

saveParticularParamTestsValueInEpoch = function(values, paramValues, epochs, plotTitle, plotIdx, plotName = NULL){
  png(returnPlotnameInPath(name = plotName, defaultPlotName = "particular_param_tests", idx = plotIdx))
  matplot(seq(1, epochs, 1), t(values),
          main = plotTitle,
          xlab = "Epoka",
          ylab = "Wartosc",
          type = "l",
          col=c(1:(1+length(paramValues))))
  legend("topleft",legend=paramValues, col=c(1:(1+length(paramValues))),bg= ("white"),fill=c(1:(1+length(paramValues))))
  dev.off()
}

returnPlotnameInPath = function(name, defaultPlotName, idx){
  if(is.null(name)) name = paste(directory, defaultPlotName, idx, imgType)
  else name = paste(directory, name, idx, imgType)
  return(name)
}