#Autorzy - Konrad Frąc i Łukasz Kotlewski

directory = "./graph/"
imgType = ".png"


#' Funkcja zapisujaca wykres porownujacy najlepszy wybrany zbior uczacy ze zbiorem uczacym ze wszystkich dostepnych danych
#'
#' @param bestValues Funkcje przystosowania dla najlepszego znalezionego zbioru
#' @param wholeTrainGoalFuncValue Funkcje przystosowania dla maksymalnego zbioru
#' @param epochs Liczba epok uczenia
#' @param plotIdx Oznaczenie dla nazwy wykresu, by znalezc konkretny wykres przy wielu takich samych w katalogu
#' @param plotName Nazwa wykresu
#'
#' @return Wykres porownawczy najlepszyego wybranego zbioru uczacego ze zbiorem uczacym ze wszystkich dostepnych danych
#' @export
saveBestEvaluationValuesPlot = function(bestValues, wholeTrainGoalFuncValue, epochs, plotIdx, plotName = NULL){
  png(returnPlotnameInPath(name = plotName, defaultPlotName = "best_evaluations_in_epochs_plot", idx = plotIdx))
  matplot(seq(1, epochs, 1), cbind(bestValues, rep(wholeTrainGoalFuncValue, epochs)),
       main = "Najlepsza wartosci funkcji oceny",
       xlab = "Epoka",
       ylab = "Wartosc",
       type = "b",
       col = c("blue","red"))
  legend("topleft",legend=c("Najlepszy pozdbior","Caly zbior uczacy"), bty = "n", fill=c("blue","red"))
  dev.off()
}


#' Tworzenie wykresu pokazujacego ile przykladow danej klasy znajduje sie w poczatkowym zbiorze uczacym, a ile w znalezionym na koniec
#'
#' @param startLabels Etykiety klas z poczatkowego zbioru uczacego
#' @param endLabels Etykiety klas z koncowego zbioru uczacego
#' @param plotIdx Oznaczenie dla nazwy wykresu, by znalezc konkretny wykres przy wielu takich samych w katalogu
#' @param plotName Nazwa wykresu
#'
#' @return Wykres liczebnosci klas w poczatkowym i koncowym zbiorze uczacym
#' @export
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


#' Tworzenie wykresu dokladnosci obu klas dla danego zbioru uczacego
#'
#' @param positiveAcc Wektor dokladnosci klasy '1'
#' @param negativeAcc Wektor dokladnosci klasy '0'
#' @param epochs Liczba epok nauki
#' @param plotIdx Oznaczenie dla nazwy wykresu, by znalezc konkretny wykres przy wielu takich samych w katalogu
#' @param plotName Nazwa wykresu
#'
#' @return Wykres dokladnosci wyznaczania obu klas dla danego zbioru uczacego na przestrzeni epok
#' @export
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


#' Tworzenie zbiorczych wykresow zaleznosci miedzy funkcja, a wplywem na nia danego parametru i jego wartosci
#'
#' @param values Wartosci funkcji na ktora wplyw ma miec badany parametr w epokach uczenia
#' @param paramValues Wartosci badanego parametru
#' @param epochs Liczba epok uczenia
#' @param plotTitle Tytul wykresu
#' @param plotIdx Oznaczenie wykresu nazwa badanego parametru
#' @param plotName Nazwa pliku wykresu
#'
#' @return Zbiorczy wykres zaleznosci funkcji od wplywu badanych wartosci parametru
#' @export
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


#' Zwracanie sciezki do zapisywanego wykresu
#'
#' @param name Nazwa wykresu
#' @param defaultPlotName Domyslna nazwa
#' @param idx Oznaczenie wykresu, dla rozroznienia go wsrod podobnych
#'
#' @return Sciezka do nowotworzonego wykresu
#' @export
returnPlotnameInPath = function(name, defaultPlotName, idx){
  if(is.null(name)) name = paste(directory, defaultPlotName, idx, imgType)
  else name = paste(directory, name, idx, imgType)
  return(name)
}