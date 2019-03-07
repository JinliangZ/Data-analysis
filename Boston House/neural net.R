# read data
set.seed(500)
require(MASS)
data<- Boston

apply(data,2,function(x) sum(is.na(x)))

# Split data into training and testing
sample <- sample.int(n=nrow(data), size = floor(0.8*nrow(data)))

#Standardzation
maxs <- apply(data,2,max) 
mins <- apply(data,2,min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train <- scaled[sample,]
test <- scaled[-sample,]
test.r <- (test$medv)*(max(data$medv)-min(data$medv))
p <- (max(data$medv)-min(data$medv))

#Linear regression
modelLR <- lm(medv  ~ ., train)
modelLR
a <- test$medv
predictedLR <- predict(modelLR, test)
rmseLR <- sqrt(mean((test.r - predictedLR*p)^2))

#SVM
require (e1071)
modelSVR <- svm(medv ~ . , train, type= "eps-regression")
modelSVR

predictedSVR <- predict(modelSVR, test)
rmseSVR <- sqrt(mean((test.r - predictedSVR*p)^2))

#MLP(feed-forward neural network)
library(neuralnet)
n <- names(train)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(45,40,30,20,10,5), threshold = 0.01, learningrate= 0.01, err.fct = "sse", act.fct = "logistic",stepmax=1000,linear.output= F)
#plot(nn)
pr.nn <- compute(nn,test[,1:13])
pr.nn_ <- pr.nn$net.result*p
rmseFFNN <- sqrt(mean((test.r - pr.nn_)^2))

