library(FSelector)
library(caret)
library(frbs)

patients <- read.csv2(file="./clevelanddata.csv", sep=",", dec=".", header=TRUE)
patients$Sex <- as.factor(patients$Sex)
patients$Chest_pain <- as.factor(patients$Chest_pain)
patients$Blood_sugar <- as.factor(patients$Blood_sugar)
patients$ECG <- as.factor(patients$ECG)
patients$Exercize <- as.factor(patients$Exercize)
patients$x1 <- as.factor(patients$x1)
patients$x2 <- as.factor(patients$x2)
patients$Thallium_scan <- as.factor((patients$Thallium_scan))
patients$sick <- as.factor(patients$sick)


#linear regression
model <- lm(sick ~ ., data = patients) 
anova(model)

# FeatureSelction
#information gain method
weights1 <- information.gain(sick ~., patients)
print(weights1)

# chi squared
library(mlbench)
weights2 <- chi.squared(sick~., patients)
print(weights2)

# use ml to classify the data. 94% accuracy with PART (pruned c4.5)
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# program + genre + block_position
model <- caret::train(sick ~ ., data = patients, trControl=train_control, method="rpart")
# summarize results
print(model)
summary(model, correlation = TRUE)
anova(model$finalModel)

# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
