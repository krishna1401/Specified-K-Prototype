getDistance <- function(center,point,cluster = NULL,weight = 1){
  
  #Objective <- Calculate the Similarity Distance for both Numerical & Categorical
  #             attributes.
  #Input <- Complete Cluster, Center Point and Point(to get distance from).
  #Output <- Combine Numerical & Categorical Distance
  
  numerical_distance <- 0
  categorical_distance <- 0
  for(i in 1:length(center)){
    if(is.null(levels(point[,i]))){
      #Numerical Distance
      temp <- (center[i] - point[i])^2
      numerical_distance <- numerical_distance + temp[,1]
    }else{
      #Categorical Distance
      #print(paste(typeof(center[i]),typeof(point[i])))
      if(center[i] == point[i]){
        
        if(!is.null(cluster)){
          temp <- point[i]
          #Number of Rows in Cluster with same value as the Point
          no_of_srows <- nrow(cluster[cluster[,i] == temp[,1],])
          categorical_distance <- categorical_distance + (1-(no_of_srows/nrow(cluster)))
        }
      }else{
        categorical_distance <- categorical_distance + 1 
      }
    }
  }
  distance <- sqrt(numerical_distance) + (weight*categorical_distance)
  return(distance)
}

computeCentroid <- function(cluster,label){

  #Objective <- Calculate the Centroid for the given Cluster Set
  #Input <- Cluster Set & Class Label
  #Output <- Centroid
  
  #Removing Label at time of Centroid calculation
  cluster <- cluster[,-which(names(cluster) %in% label)]
  
  point <- list()
  for(i in 1:ncol(cluster)){
    if(is.null(levels(cluster[,i]))){
      #Compute the Mean for Numeric Attribute
      point <- append(point,mean(cluster[,i]))
    }else{
      temp <- cluster[,i]
      #Find the Mode of the Categorical Attribute
      #tabulate function create a table of count of each unique value in the Attribute
      #which.max give the maximum of the above table
      maximum <- temp[which.max(tabulate(match(temp,unique(temp))))]
      
      point <- append(point,as.character(maximum))
    }
  }
  return(point)
}

clusterConfidence <- function(cluster,label){
  
  #Objective <- Return the Class Label with Highest Confidence
  #Input <- Cluster & Class Label
  #Output <- Single Valued Class Label

  maximum_confidence <- 0
  class_label <- 0
  for(i in unique(cluster[,label])){
    label_confidence <- nrow(cluster[cluster[,label] == i,])/nrow(cluster)
    if(label_confidence > maximum_confidence){
      maximum_confidence <- label_confidence
      class_label <- i
    }
  }
  return(class_label)
}

k_cluster <- function(train,label,iter.max = 100,weight = 1){
  
  #Objective <- Create a Clustering Model using K-Prototype Clustering Techinque
  #Input <- Refined trainSet, Maximum no of Iterations, Weight for Distance
  #Output <- Details of 2 defined Clusters
  
  print("Starting k-prototype Algorithm...")
  #Getting First Random Point as Cluster 1
  cluster1 <- c()
  centroid1 <- train[sample(1:nrow(train),1),]
  #Getting Second Random Point as Cluster 2
  cluster2 <- c()
  centroid2 <- train[sample(1:nrow(train),1),]
  
  for(iter in 1:iter.max){
    print(iter)
    cluster1 <- c()
    cluster2 <- c()
    for(i in 1:nrow(train)){
      print(i)
      distance1 <- getDistance(centroid1,train[i,],cluster1)
      distance2 <- getDistance(centroid2,train[i,],cluster2)
      if(distance1 < distance2){
        cluster1 <- rbind(cluster1,train[i,])
      }else{
        cluster2 <- rbind(cluster2,train[i,])
      }
    }
    #Compute New Centroids
    centroid1 <- computeCentroid(cluster1,label)
    centroid2 <- computeCentroid(cluster2,label)
  }
  print("Starting k-prototype Algorithm...")
  
  print("Calculating Cluster Class Label")
  #Converting Centroid to single train Frame
  class <- clusterConfidence(cluster1,label)
  centroid1 <- append(centroid1,class)
  names(centroid1) <- names(train)
  
  class <- clusterConfidence(cluster2,label)
  centroid2 <- append(centroid2,class)
  names(centroid2) <- names(train)
  
  result <- list('Centroid1' = centroid1,'Centroid2' = centroid2)
  return(result)
}