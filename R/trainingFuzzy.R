

#read file
patients <- read.csv2(file="./clevelanddata.csv", sep=",", dec=".", header=TRUE)
patients[, 1:13] <- sapply(patients[, 1:13], as.numeric)
patients$sick <- as.factor(patients$sick)
patients$sick <- unclass(patients$sick)


#feature selection

library(FSelector)
#information gain method
weights1 <- information.gain(sick ~., patients)
print(weights1)

# chi squared
library(mlbench)
weights2 <- chi.squared(sick~., patients)
print(weights2)

#removing some vectors, if necesairy
#patients$Age <- NULL
#patients$Sex <- NULL
#patients$Blood_pressure <- NULL
#patients$Cholesterol <- NULL
#patients$Blood_sugar <- NULL
#patients$ECG <- NULL


control <- list(num.labels = 5, type.mf = "GAUSSIAN", type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH")  


range_data_input <- matrix(apply(patients[, 1:13], 2, range), nrow = 2)
#randomize order
random <- patients[sample(nrow(patients)),]
require(plyr)

#create folds
createFolds <- function(x,k){
  n <- nrow(x)
  x$folds <- rep(1:k,length.out = n)[sample(n,n)]
  x
}


library(frbs)

method.type <- "FRBCS.w"



# 10fold cv
final <- vector(mode="numeric", length=0)
folds <-createFolds(random,10)
for (j in 1:10) {
  trainSet <- folds[folds$folds != j,]
  testSet <- folds[folds$folds == j,]
  trainSet$folds <- NULL
  testSet$folds <- NULL
  
  fuzzymodel <- frbs.learn(trainSet,method.type = method.type, range.data = range_data_input,control = control)
  expectedresults <- as.numeric(testSet$sick)
  testSet$sick <- NULL
  
  testresults <- predict(fuzzymodel,testSet)
  roundresults <- round(testresults)
  
  results = data.frame(expectedresults,testresults,roundresults)
  total <- 0
  
  for(i in 1:nrow(results)) {
    if(identical(results[i,1],results[i,3]))
      total <- total + 1
  }
  accuracy <- as.numeric(total/i)
  final[j] <-accuracy
  print(accuracy)
}

#get final accuracy
mean(final)


summary(fuzzymodel)
plotMF(fuzzymodel)
