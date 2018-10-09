
update_column <- function(data,column_name,classMargin){
  
  #Objective <- Remove detailed information of Detection Column and convert it
  #             into 0's and 1's
  #Input <- Data, Column Name
  #Output <- Updated Data
  
  new_column <- c()
  print("Updating Column...")
  for(i in 1:nrow(data)){
    if(data[i,column_name] == classMargin){
      new_column <- append(new_column,0)
    }else{
      new_column <- append(new_column,1)
    }
  }
  levels(new_column) <- c(0,1)
  data <- data[,-which(colnames(data) == column_name)]
  data <- cbind(data,new_column)
  colnames(data)[ncol(data)] <- column_name
  return(data)
}


input_data <- function(){

  #Objective <- Read the txt tab seperated file and update the columns as per the specifications
  #Input <- NULL
  #Output <- Complete Dataset
  
  print("Reading Data...")
  data <- read.csv(choose.files(),sep = "\t")
  
  print("Processing Data...")
  column <- c("Duration_connection",
              "Service",
              "Source_bytes",
              "Destination_bytes",
              "Count_same_IP",
              "Same_server_rate",
              "Serror_rate",
              "Srv serror rate",
              "Dst_host_count",
              "Dst_host_srv_count",
              "Dst_host_port_rate",
              "Dst_host_serror_rate",
              "Dst_host_srv_serror_rate",
              "Flag",
              "IDS_detection",
              "Malware_detection",
              "Ashula_detection",
              "Label",
              "Src_ip_address",
              "Src_port_number",
              "Dst_ip_address",
              "Dst_port_number",
              "Start_time",
              "Duration_session")
  
  colnames(data) <- column
  reluctant_column <- c("Src_ip_address",
                        "Src_port_number",
                        "Dst_ip_address",
                        "Dst_port_number",
                        "Count_same_IP",
                        "Start_time")
  
  data <- data[,!(column %in% reluctant_column)]
  data <- update_column(data,'Label',1)
  
  #Converting Multi-Categorical to only 2 possible Values
  length_factors <- length(levels(data[,'IDS_detection']))
  levels(data[,'IDS_detection']) <- c(0,rep(1,length_factors-1))
  
  length_factors <- length(levels(data[,'Ashula_detection']))
  levels(data[,'Ashula_detection']) <- c(0,rep(1,length_factors-1))
  
  length_factors <- length(levels(data[,'Malware_detection']))
  levels(data[,'Malware_detection']) <- c(0,rep(1,length_factors-1))
  
  return(data)
}
