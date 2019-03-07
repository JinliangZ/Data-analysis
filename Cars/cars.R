# read data
data <- cars

# Split data into training and testing
sample <- sample.int(n=nrow(data), size = floor(0.2*nrow(data)))
train <- data[-sample,]
test <- data[sample,]

#Linear regression
modelLR <- lm(dist  ~ speed, train)
modelLR

# make a prediction for each temperature
predictedLR <- predict(modelLR, test)
#original data
plot(train) 
points(test, col = "black", pch=18)
#abline(modelLR)
points(test$speed, predictedLR, col = "red", pch=16)
rmseLR <- sqrt(mean((test$dist - predictedLR)^2)) 

#SVM
require (e1071)
modelSVR <- svm(dist ~ speed , train, type= "eps-regression")
modelSVR
predictedSVR <- predict(modelSVR, test)
rmseSVR <- sqrt(mean((test$dist - predictedSVR)^2)) 
points(test$speed, predictedSVR, col = "blue", pch=8)

#Neural Network
require (nnet)
modelNN <- nnet(dist~ speed, train,size = 20 ,decay =2e-4 ,maxit = 100 ,linout=T,trace=F)  
modelNN
predictedNN <- predict(modelNN,test)
rmseNN <- sqrt(mean((test$dist-predictedNN)^2)) 
points(test$speed, predictedNN, col = "magenta", pch=14)

