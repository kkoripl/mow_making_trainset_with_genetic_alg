#Autorzy - Konrad Frąc i Łukasz Kotlewski

#Domyslne ustawienia do testowania algorytmu genetycznego
epochs = 20
setProb = 0.8 
mutateProb = 0.1  
bitsToMutate = 3 
populationSize = 10
equalExamplesCount = FALSE

#' Funkcja sprawdzająca wpływ poszczególnych parametrów na wynik działania algorytmu genetycznego przy wyborze
#' zbioru treningowego dla danego zadania.
#'
#' @param train Początkowy, pełny zbiór trenujący
#' @param test Zbiór testujący
#' @param paramValues Wartości testowanego parametru - wektor
#' @param paramName Nazwa testowanego parametru - wybór z: Start_bits_set - procent wielkości zbiorów trenujących z pierwszej populacji, względem całego zbioru, Population_size - wielkość populacji, Equal_examples_count - czy w pierwszym zbiorze trenującym ma być taka sama liczba przykładów z obu klas
#'
#' @return Wykresy (liczebności klas w wybranym zbiorze treningowym, funkcji przystosowania w epokach, precyzji dla obu klas w epokach) dla pojedynczych wartości parametrów oraz zgrupowane dla wszystkich (oznaczone literką T)
#' @export
#'
#' @examples testParticularParamInfluence(train, test, c(3,5,10,15), 'Population_size') - testowanie wpływu wielkości populacji dla 3,5,10 i 15 chromosomów
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



#' Testowanie wpływu liczby epok na działanie algorytmu genetycznego przy wyborze zbioru trenującego
#'
#' @param train Początkowy, pełny zbiór trenujący
#' @param test Zbiór testujący
#' @param epochsParams Testowane liczby epok
#'
#' @return Wykresy (liczebności klas w wybranym zbiorze treningowym, funkcji przystosowania w epokach, precyzji dla obu klas w epokach) dla pojedynczych liczb epok oraz zgrupowane dla wszystkich (oznaczone literką T)
#' @export
#'
#' @examples testEpochsInfluence(train, test, c(3,5,10,15)) - test działania algorytmu w 3,5,10 i 15 epokach
testEpochsInfluence = function(train, test, epochsParams){
  
  for(i in 1:length(epochsParams)){
    epochs = epochsParams[i]
    print(c(" ===============> Start testing epochs:", as.character(epochs)))
    bestSetData = findOptimumSubset(train, test, epochs, setProb, mutateProb, bitsToMutate, populationSize, equalExamplesCount, 
                                    plotIdx=paste('epochs',"_",as.character(epochs)))
  }
}



#' Zapisywanie diagramu zgrupowanych wykresów dokładności klasy '1' dla wszystkich testowanych wartości parametru
#'
#' @param posAccuracies Obliczone dokładności dla klasy '1' - tablica, gdzie dany wiersz to dokładności dla danej wartości parametru
#' @param paramValues Testowane wartości parametrów - wektor
#' @param paramName Nazwa testowanego parametru
#' @param epochs Liczba epok testów
#'
#' @return Zgrupowany wykres dokładności klasy '1' dla wszystkich testowanych wartości parametru
#' @export
#'
#' @examples plotPosAccuracyPlot(t(array(seq(0.05,1.0,0.05), c(10,2))), c(3,5), 'Population_size', 10) - generacja wykresu dla testów parametru wielkosci populacji (3,5), ktoraz pokaz dokladnosc 5%-50% dla wartosci 3 i 50%-100% dla 5
plotPosAccuracyPlot = function(posAccuracies, paramValues, paramName, epochs){
  saveParticularParamTestsValueInEpoch(posAccuracies, 
                                       paramValues,
                                       epochs, 
                                       paste('Dokladnosc dla pozytywnych'), 
                                       plotIdx = '', 
                                       plotName = paste('T_Positive_Accuracies_in_Epochs_by_',paramName))
}

#' Zapisywanie diagramu zgrupowanych wykresów dokładności klasy '0' dla wszystkich testowanych wartości parametru
#'
#' @param negAccuracies Obliczone dokładności dla klasy '0' - tablica, gdzie dany wiersz to dokładności dla danej wartości parametru
#' @param paramValues Testowane wartości parametrów - wektor
#' @param paramName Nazwa testowanego parametru
#' @param epochs Liczba epok testów
#'
#' @return Zgrupowany wykres dokładności klasy '0' dla wszystkich testowanych wartości parametru
#' @export
#'
#' @examples plotNegAccuracyPlot(t(array(seq(0.05,1.0,0.05), c(10,2))), c(3,5), 'Population_size', 10) - generacja wykresu dla testów parametru wielkosci populacji (3,5), ktoraz pokaz dokladnosc 5%-50% dla wartosci 3 i 50%-100% dla 5
plotNegAccuracyPlot = function(negAccuracies, paramValues, paramName, epochs){
  saveParticularParamTestsValueInEpoch(negAccuracies, 
                                       paramValues,
                                       epochs, 
                                       paste('Dokladnosc dla negatywnych'), 
                                       plotIdx = '', 
                                       plotName = paste('T_Negative_Accuracies_in_Epochs_by_',paramName))
}

#' Zapisywanie diagramu zgrupowanych wartosci funkcji przystosowania w epokach dla najlepszego zbioru trenujacego wybranego do danej epoki dla danego testowanego parametru
#'
#' @param bestValuesInEpochs Obliczone wartosci funkcji przystosowania dla najlepszego zbioru trenujacego wybranego do danej epoki - tablica, gdzie dany wiersz to wartosc funkcji przystosowania dla danej wartości parametru
#' @param paramValues Testowane wartości parametrów - wektor
#' @param paramName Nazwa testowanego parametru
#' @param epochs Liczba epok testów
#'
#' @return Zgrupowany wykres wartosci funkcji przystosowania w epokach dla najlepszego zbioru trenujacego wybranego do danej epoki
#' @export
#'
#' @examples plotBestValuesPlot(t(array(seq(0.05,1.0,0.05), c(10,2))), c(3,5), 'Population_size', 10) - generacja wykresu dla testów parametru wielkosci populacji (3,5), ktoraz pokaze wartosc funkcji przystosowania 0.05-0.5 dla wartosci 3 i 0.5-1 dla 5
plotBestValuesPlot = function(bestValuesInEpochs, paramValues, paramName, epochs){
  saveParticularParamTestsValueInEpoch(bestValuesInEpochs, 
                                       paramValues,
                                       epochs, 
                                       paste('Najlepsza wartosc funkcji oceny'), 
                                       plotIdx = '', 
                                       plotName = paste('T_Best_eval_values_in_Epochs_by_',paramName))
} 