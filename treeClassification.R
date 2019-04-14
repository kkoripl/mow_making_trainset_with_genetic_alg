source("createRpartTree.R")

treeClassification = function(data, labels){
  
  tree = createRpartTree(data, labels)
  prediction = predict(tree, data, type = 'class')
  
  returnList = list("tree" = tree, "prediction" = prediction)
  return(returnList)
}