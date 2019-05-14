directory = "./graph/"
imgType = ".png"

saveBestEvaluationValuesPlot = function(bestValues, epochs, plotIdx, plotName = NULL){
  png(returnPlotnameInPath(name = plotName, defaultPlotName = "best_evaluations_in_epochs_plot", idx = plotIdx))
  plot(seq(1, epochs, 1), bestValues,
       main = "Best evaluation values in epochs",
       xlab = "Epochs",
       ylab = "Evaluation value",
       type = "b",
       col = "blue")
  dev.off()
}

saveTrainsetExamplesCounts = function(startLabels, endLabels, plotIdx, plotName = NULL){
  startCounts = table(startLabels)
  counts = matrix(c(startCounts, table(endLabels)), nrow=2)
  rownames(counts) = names(startCounts)
  colnames(counts) = c("Before alg.", "After alg.")
  png(returnPlotnameInPath(name = plotName, defaultPlotName = "trainset_examples_counts", idx = plotIdx))
  barplot(counts,
          main = "No. of examples in trainset classes",
          xlab = "Classes",
          ylab = "Examples",
          col=c("blue","red"),
          legend = rownames(counts))
  dev.off()
}

saveAccuraciesPlot = function(positiveAcc, negativeAcc, epochs, plotIdx, plotName = NULL){
  png(returnPlotnameInPath(name = plotName, defaultPlotName = "accuracies_in_epochs_plot", idx = plotIdx))
  matplot(seq(1, epochs, 1), cbind(positiveAcc, negativeAcc),
       main = "Accuracies values in epochs",
       xlab = "Epochs",
       ylab = "Accuracy value",
       type = "l",
       col = c("blue","red"))
  legend("bottomright",legend=c("Positive","Negative"), bty = "n", fill=c("blue","red"))
  dev.off()
}

returnPlotnameInPath = function(name, defaultPlotName, idx){
  if(is.null(name)) name = paste(directory, defaultPlotName, idx, imgType)
  else name = paste(directory, name, idx, imgType)
  return(name)
}