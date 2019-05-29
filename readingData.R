dataset = read.table("data/spliceDTrainKIS.dat", header=TRUE)

#Wyjasnienie 2 linijek ponize
#do.call wywoluje funkcje zadana w pierwszym argumencie -> rbind
#rbind scala wektory [wiersze] w macierz, w tym wypadku sa to wiersze Adata i Ddata poddawane strsplit
#strsplit dzieli string na czesci: 
# arg. split = '' mowi, zeby dzielic co element, czyli u nas co litere
# fixed = TRUE, mowi, iz podzial nie jest ze wzgledu na wyrazenie regularne
#As.character, bo gowno wczytuje jako integer, a strsplit dziala na stringach

dataset = data.frame(do.call('rbind', strsplit(as.character(dataset[seq(2, nrow(dataset), 2), 1]),'',fixed = TRUE)), "LABELS" = dataset[seq(1, nrow(dataset), 2), 1])

#zmieniamy poszczegolne kolumny na stringi
#probowalem dzialac z -ncol(Adata) na argumencie po ',' ale pojawiaja sie jakies niewyjasnione zbitki cyfr
for(i in 1:(ncol(dataset)-1)) {
  dataset[,i] = as.character(dataset[,i])
}

#ostatnia kolumna na integer
dataset[,ncol(dataset)] = as.integer(dataset[,ncol(dataset)])
dataset$LABELS = dataset$LABELS-1

#mieszamy dane
shuffleIndexes = sample(1:nrow(dataset))
dataset = dataset[shuffleIndexes, ]

#wywalamy zbedne zmienne
remove(i)
remove(shuffleIndexes)