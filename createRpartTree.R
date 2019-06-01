#Autorzy - Konrad Frąc i Łukasz Kotlewski

#' Funkcja tworząca drzewo Rpart na podstawie danych
#'
#' @param trainData Dane do budowy drzewa decyzyjnego
#' @param labels Etykiety danych
#' @param plot Informacja czy wyświetlić zbudowane drzewo
#'
#' @return Zbudowane drzewo Rpart
#' @export
createRpartTree = function(trainData, labels, plot = FALSE) {
  
  # Zrob drzewo, wpierw to co chcemy uzyskac, pozniej dane, na koncu sposob
  # z dokumentacji nic innego jak class nie pasuje, ale nie wiem co to
  # Falka z kropka to bodaj, by wzielo po prostu dane stamtad bez zadnych dzialan
  tree = rpart(labels~., data = trainData, method = 'class')
  
  # wyswietla drzewo, extra = 106 wg. dokumentacji to parametr pod klasy binarne, jak u nas
  # 6 - Class  models:  the  probability  of  the  second  class  only.   Useful  for  binary responses.
  # U nas to b?dzie klasa '1' - to jej pstwo, znajdzie sie w node w drugim wierszu
  # +100 - Add 100 to any of the above to also display the percentage of observations in the node.
  # Na rysunku: pierwszy wiersz - chyba klasa, drugi to pstwo klasy 1, trzeci to procent danych spelniajacych warunki do tej pory
  if(plot){
    rpart.plot(tree, extra = 106)
  }
  
  return(tree)
}
