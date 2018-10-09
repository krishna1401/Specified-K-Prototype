setwd('~/R/Msc/IS/A2_Krishna_14/Question1')
source('input.R')
source('feature_selection.R')
source('k_prototype.R')
source('predict.R')

class_label = 'Malware_detection'
#Input Data
data <- input_data()
rows <- nrow(data)

#Feature Selection
data <- selection(data,label = class_label)

trainRows <- sample(1:rows,0.7*rows)
trainData <- data[trainRows,]
testData <- data[-trainRows,]

#Running k-prototype
model <- k_cluster(trainData,label = class_label,iter.max = 5)
prediction <- predict(model,testData,label = class_label)
model_details <- confusionMatrix(prediction,testData[,class_label])

View(model_details)