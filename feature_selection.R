variance <- function(data,column_name){
  
  #Objective <- Find average variance of the complete attribute, using multiple samples
  #Input <- Data, Column Name
  #Output <- Variance
  
  sample_size <- 0.1*length(data)
  total_variance <- 0
  for(i in 1:1000){
    temp <- data[sample(1:length(data),sample_size),column_name]
    mean <- sum(temp)/1000
    total_variance <- total_variance + sqrt(sum((temp-mean)^2))/(sample_size)
  }
  #Average Variance of the Data
  return(total_variance/(sample_size))
}


information_gain <- function(data,column,label){
  
  #Objective <- Find Entropy of the complete attribute
  #Input <- Data, Column Name, Label
  #Output <- Information gain
  
  no_rows <- nrow(data)
  #Labels in the Classifier Attribute
  no_levels <- unique(data[,label])
  parent_entropy <- 0
  for(i in no_levels){
    #Find Number of Rows of Paticular Labels
    temp <- length(which(data[,label] == i))/no_rows
    parent_entropy <- parent_entropy + (-1*(temp*log(temp)))
  }
  
  #Partitions in the Classifing Attribute
  no_partition <- unique(data[,column])
  child_entropy <- 0
  for(i in no_partition){
    partition <- data[data[,column] == i,]
    child_rows <- nrow(partition)
    temp_entropy <- 0
    
    #Calculating Entropy of Childrens
    for(j in no_levels){
      temp <- length(which(partition[,label] == j))
      temp <- temp/child_rows
      if(temp != 0)
        temp_entropy <- temp_entropy + (-1*(temp*log(temp)))
    }
    
    #Combined Entropy of Each Child
    child_entropy <- child_entropy + ((child_rows/no_rows)*temp_entropy)
  }
  return(parent_entropy - child_entropy)
}

normalize <- function(x){(x-min(x))/(max(x)-min(x))}

selection <- function(data,label){
  
  #Objective <- Select Features from data on basis of defined Criteria
  #Input <- Data & Label
  #Output <- Data with only selected features and Label
  
  info <- list()
  possible_columns <- c()
  
  print("Evaluating Feature Selection Value...")
  for(i in names(data)){
    if(i != label){
      if(is.null(levels(data[,i]))){
        #print(paste(i,'Numeric'))
        temp <- variance(data,i)
        if(temp != 0){
          possible_columns <- append(possible_columns,i)
        }
        info[[i]] <- temp
      }else{
        #print(paste(i,'categorical'))
        temp <- information_gain(data,i,label)
        if(temp != 0){
          possible_columns <- append(possible_columns,i)
        }
        info[[i]] <- temp
      }
    }
  }
  #Normalize Duration Column to range 0-1
  data[,'Duration_connection'] <- normalize(data[,'Duration_connection'])
  
  info <- unlist(info)
  View(info)
  
  print("Updating DataSet...")
  possible_columns <- append(possible_columns,label)
  data <- data[,possible_columns]
  return(data)
  
}