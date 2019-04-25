crossover = function(parentsTable, pairsCnt, crossPointsCnt){
  genotypeBits = ncol(parentsTable);
  parentsCnt = nrow(parentsTable);
  offsprings = matrix(nrow = (pairsCnt*2), ncol = genotypeBits);
  
  #miesza numery wierszy tabelki populacji - rodzicow - i tworzy z nich tablice par
  pairs = combinations(nrow(parentsTable),2);
  
  for(indP in 1:pairsCnt){
    firstOff = (indP)*2 - 1;
    secondOff = (indP)*2;
    
    father = parentsTable[pairs[indP, 1], ];
    mother = parentsTable[pairs[indP, 2], ];
    
    #losujemy punkty krzyzowania np. 3,5,8, a potem je powielamy (i sortujemy) by kazdy byl po 2 razy = 3,3,5,5,8,8
    #zaraz bedziemy dodawac +1 co drugi element, stad losujemy od 1 bitu, do przedostatniego mozliwe punkty krzyzowan
    crossPoints = sort(rep(sample(1:(genotypeBits-1), crossPointsCnt), each=2));
    
    #dodajemy co drugi element +1, aby zrobic rozdzielne przedzialy: 3,4,5,6,8,9
    crossPoints = crossPoints + rep(c(0,1), times=crossPointsCnt)
    
    #tworzymy tabele poczatkow i konczow fragmentow krzyzowanych, dodajac do krzyzowan pierwszy i ostatni bit
    #par poczatek - koniec, bedzie o 1 wiecej niz punktow krzyzowan (bez powielenia!)
    #dzieki powieleniu, podzial na 2 wiersze da nam np. takie fragmenty; 1-3,4-5,6-8.9-10
    #dodanie na przedzie 1ki, przesuwa ew. powtorki indeksow jedno w prawo, wiec wszelkie dodania wyladuja jako pierwszy element w parze
    #jesli dodamy i powstanie powtorka nastepnego indeksu - wziety bedzie po prostu jeden bit o tym numerze
    crossPoints = matrix(c(1, crossPoints, genotypeBits), nrow=(crossPointsCnt+1), ncol=2, byrow=TRUE);
    
    for(point in 1:nrow(crossPoints)){
      if(point%%2 != 0){
        offsprings[firstOff, crossPoints[point,1]:crossPoints[point,2]] = father[crossPoints[point,1]:crossPoints[point,2]]
        offsprings[secondOff, crossPoints[point,1]:crossPoints[point,2]] = mother[crossPoints[point,1]:crossPoints[point,2]]
      } else {
        offsprings[firstOff, crossPoints[point,1]:crossPoints[point,2]] = mother[crossPoints[point,1]:crossPoints[point,2]]
        offsprings[secondOff, crossPoints[point,1]:crossPoints[point,2]] = father[crossPoints[point,1]:crossPoints[point,2]]
      }
    }
  }

  return(offsprings)
}