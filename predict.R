
predict <- function(model,test,label){
  
  #Objective <- Predict the Complete testSet using the given model
  #Input <- Model, Test Set, Label
  #Output <- List of all Predicted Labels
  predicted_labels <- c()
  
  #Extracting Label & Centroid from the given Model
  label1 <- model[1][[1]][label]
  centroid1 <- model[1][[1]]
  centroid1 <- centroid1[-which(names(centroid1) %in% label)]
  #View(centroid1)
  label2 <- model[2][[1]][label]
  centroid2 <- model[2][[1]]
  centroid2 <- centroid2[-which(names(centroid2) %in% label)]
  #View(centroid2)
  
  print("Predicting Class Label...")
  for(i in 1:nrow(test)){
    #print(i)
    distance1 <- getDistance(centroid1,test[i,])
    distance2 <- getDistance(centroid2,test[i,])
    if(distance1 < distance2){
      class <- label1
      predicted_labels <- append(predicted_labels,label1)
    }else{
      class <- label2
      predicted_labels <- append(predicted_labels,label2)
    }
  }
  return(predicted_labels)
}


#Update It
confusionMatrix <- function(predicted,original){
  
  #Objective <- Create a Confusion Matrix with the given predicted and original class labels
  #Input <- Predicted Labels & Original Labels
  #Output <- List contaning the following information:
  #           a) Confustion Matrix
  #           b) Accuracy
  #           c) Error Rate
  #           d) True Positive Rate
  #           e) False Positive Rate
  #           f) Precision

  
  cmatrix <- matrix(c(0,0,0,0),nrow = 2,ncol = 2,
                    dimnames = list(c('P','N'),
                                    c('P','N')))
  for(i in 1:length(predicted)){
    if(predicted[i][1] == 1 && original[i] == 1){
      cmatrix['P','P'] <- cmatrix['P','P'] + 1
    }else if(predicted[i][1] == 0 && original[i] == 0){
      cmatrix['N','N'] <- cmatrix['N','N'] + 1
    }else if(predicted[i][1] == 1 && original[i] == 0){
      cmatrix['N','P'] <- cmatrix['N','P'] + 1
    }else if(predicted[i][1] == 0 && original[i] == 1){
      cmatrix['P','N'] <- cmatrix['P','N'] + 1
    }
  }
  accuracy <- (cmatrix['P','P'] + cmatrix['N','N'])/length(predicted)
  error <- (cmatrix['N','P'] + cmatrix['P','N'])/length(predicted)
  truePositive <- cmatrix['P','P']/length(original[original == 1])
  falsePositive <- cmatrix['N','P']/length(original[original == 0])
  precision <- cmatrix['P','P']/(cmatrix['P','P'] + cmatrix['N','P'])
  
  information <- list('Confusion Matrix' = cmatrix,
                      'Accuracy' = accuracy,
                      'Error' = error,
                      'True Positive' = truePositive,
                      'False Positive' = falsePositive,
                      'Precision' = precision)
  return(information)
}