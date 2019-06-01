#Autorzy - Konrad Frąc i Łukasz Kotlewski

#' Funkcja wczytujaca zbior danych z podanego pliku
#'
#' @param dataFile Nazwa pliku z danymi z folderu "data"
#'
#' @return Zbior "data frame" wczytanych danych z oznaczeniem etykiet w ostatniej kolumnie
#' @export
readData = function(dataFile){
  dataset = read.table(paste("data/",dataFile,sep=""), header=TRUE)

  #do.call wywoluje funkcje zadana w pierwszym argumencie -> rbind
  #rbind scala wektory [wiersze] w macierz, w tym wypadku sa to wiersze Adata i Ddata poddawane strsplit
  #strsplit dzieli string na czesci:
  # arg. split = '' mowi, zeby dzielic co element, czyli u nas co litere
  # fixed = TRUE, mowi, iz podzial nie jest ze wzgledu na wyrazenie regularne
  dataset = data.frame(do.call('rbind', strsplit(as.character(dataset[seq(2, nrow(dataset), 2), 1]),'',fixed = TRUE)), "LABELS" = dataset[seq(1, nrow(dataset), 2), 1])

  for(i in 1:(ncol(dataset)-1)) {
    dataset[,i] = as.character(dataset[,i])
  }

  dataset[,ncol(dataset)] = as.integer(dataset[,ncol(dataset)])
  
  # Z niewiadomych przyczyn numery klas wczytuja sie o 1 wieksze
  dataset$LABELS = dataset$LABELS-1

  #mieszamy dane
  shuffleIndexes = sample(1:nrow(dataset))
  dataset = dataset[shuffleIndexes, ]

  remove(i)
  remove(shuffleIndexes)

  return(dataset)
}
