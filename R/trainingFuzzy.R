#This file reads the database and constructs a Fuzzy Logic System by using instance based learning.
#It then performs a 10fold cross validation repeated 3times to make sure its results are accurate.
#The last two lines allow you to show a summary of the FLS which shows its membership functions and Rules,
#and it allows you to plot the membership functions


#read file
patients <- read.csv2(file="./clevelanddata.csv", sep=",", dec=".", header=TRUE)
fpatients[, 1:13] <- sapply(patients[, 1:13], as.numeric)
patients$sick <- as.factor(patients$sick)
patients$sick <- unclass(patients$sick)


#removing some vectors, if necesairy
patients$Age <- NULL
patients$Blood_pressure <- NULL
patients$Cholesterol <- NULL



require(plyr)
#create folds
createFolds <- function(x,k){
  n <- nrow(x)
  x$folds <- rep(1:k,length.out = n)[sample(n,n)]
  x
}


library(frbs)

method.type <- "ANFIS"
control <- list(num.labels = 7, type.mf = "GAUSSIAN", type.tnorm = "MIN", type.snorm = "MAX", type.implication.func = "ZADEH")  

# repeat 3 times
repeatScore <-vector(mode="numeric", length=0)
for (k in 1:3) {
  
  #randomize order
  random <- patients[sample(nrow(patients)),]
  
  # 10 fold cv
  final <- vector(mode="numeric", length=0)
  folds <-createFolds(random,10)
  for (j in 1:10) {
    trainSet <- folds[folds$folds == j,]
    testSet <- folds[folds$folds != j,]
    trainSet$folds <- NULL
    testSet$folds <- NULL
    
    fuzzymodel <- frbs.learn(trainSet,method.type = method.type, control = control)
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
  print(paste0("mean of fold = ", mean(final)))
  repeatScore[k] <-mean(final)
  
}
print(paste0("final accuracy : ", mean(repeatScore)))

summary(fuzzymodel)
plotMF(fuzzymodel)
