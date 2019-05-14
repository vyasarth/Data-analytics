dataset <- read.csv("train.csv")
trainData <- dataset[order(runif(50000)), ]
trainData <- as.data.frame(trainData)
write.csv(trainData, "C:/Mitul/Mitul Docs/Advance Data Mining/Data1.csv")
str(trainData)

#Missing Values
install.packages('mice')
library(mice)
sum(is.na(trainData))
md.pattern(trainData)

#splitting data
n <- nrow(trainData)
trainIndex <- sample(1:n, size = round(0.7*n), replace = FALSE)
train <- trainData[trainIndex ,]
test <- trainData[-trainIndex ,]
nrow(train)
nrow(test)

#boxplot
str(trainData)
boxplot(trainData$winPlacePerc, trainData$walkDistance, trainData$boosts, trainData$killPlace, trainData$killStreaks, 
        names=c("WinPlacePerc","walkDistance","Boosts","KillPlace","KillStreaks"), main= "Outliers")

#Correlation
library(corrplot)
trainData <- cor(trainData)
corrplot(trainData)

###boruta
install.packages("Boruta")
library(Boruta)
borutaTrain <- Boruta(winPlacePerc~.-Id -groupId -matchId, data = trainData, doTrace = 2)

###Rank  Feature
set.seed(7)
# load the library
library(caret)
library(lattice)
library(ggplot2)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(winPlacePerc~., data=trainData, method="lm", preProcess="scale", trControl=control)
warning()
model
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

set.seed(7)
# load the library
library(caret)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(trainData[,4:25], trainData[,26], sizes=c(4:25), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


### MLR  
set.seed(123)
regressor = lm(formula = winPlacePerc ~ walkDistance + boosts + killPlace + killStreaks,
               data = train)
regressor

summary(regressor) ## summary of model 
confint(regressor) ###confidence interval of the model

y_predMLR = predict(regressor, newdata = test)
predMLR <- as.data.frame(y_predMLR) 

test$predictedValueMLR <- predMLR$y_predMLR

actuals_predsMLR <- data.frame(cbind(actuals=test$winPlacePerc, predicteds=test$predictedValueMLR))  # make actuals_predicteds dataframe
correlation_accuracyMLR <- cor(actuals_predsMLR)
summary(correlation_accuracyMLR)
head(actuals_predsMLR)
min_max_accuracyMLR <- mean(apply(actuals_predsMLR, 1, min) / apply(actuals_predsMLR, 1, max))  
min_max_accuracyMLR ### 

#Plot
act <- actuals_predsMLR[order(runif(50000)), ]
library(ggplot2)
ggplot(act, aes(x = act$actuals , y = act$predicteds)) +
  ggtitle("Multiple Linear Regression")+
  xlab("Actual Value") + ylab("Predicted Value") +
  geom_segment(aes(xend = actuals, yend = predicteds)) +
  geom_point()+
  geom_point(aes(y = act$predicteds), shape = 1, colour = 'red')


#RandomForest
#model with mtry = 2
library(randomForest)
classifier1 = randomForest(winPlacePerc  ~ walkDistance + boosts + killPlace + killStreaks, 
                           data = train, mtry = 2, ntree = 100)
summary(classifier1)
#Predicting the test set results
y_pred2 = predict(classifier1, newdata = test)
y_pred2  
pred4 <- as.data.frame(y_pred2)

#Accurancy
test$predictedValueRF <- pred4$y_pred2
actuals_predsRF <- data.frame(cbind(actualsRF=test$winPlacePerc, predictedsRF=test$predictedValueRF))  # make actuals_predicteds dataframe.
actuals_predsRF <- as.data.frame(actuals_predsRF)
correlation_accuracyRF <- cor(actuals_predsRF)
summary(correlation_accuracyRF)
head(actuals_predsRF)
min_max_accuracyRF <- mean(apply(actuals_predsRF, 1, min) / apply(actuals_predsRF, 1, max))  
min_max_accuracyRF

actRF <- actuals_predsRF[order(runif(50000)), ]

#plot
library(ggplot2)
ggplot(actRF, aes(x = actRF$actualsRF , y = actRF$predictedsRF)) +
  #geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + 
  ggtitle("Random Forest")+
  xlab("Actual Value") + ylab("Predicted Value") +
  geom_segment(aes(xend = actualsRF, yend = predictedsRF)) +
  geom_point()+
  geom_point(aes(y = actRF$predictedsRF), shape = 1, colour = 'red')

### SVR
set.seed(123)
library(e1071)
classifier = svm(formula = winPlacePerc ~ walkDistance + boosts +killPlace + killStreaks,
                 
                 data = train,
                 
                 type = 'eps-regression',
                 kernel = 'linear')
classifier
summary(classifier)

y_predSVR = predict(classifier, newdata = test)
View(y_pred)
predSVR <- as.data.frame(y_predSVR)
test$predictedValueSVR <- predSVR$y_predSVR

actuals_predsSVR <- data.frame(cbind(actualsSVR=test$winPlacePerc, predictedsSVR=test$predictedValueSVR))  # make actuals_predicteds dataframe.
correlation_accuracySVR <- cor(actuals_predsSVR)
summary(correlation_accuracySVR)
head(actuals_predsSVR)
min_max_accuracySVR <- mean(apply(actuals_predsSVR, 1, min) / apply(actuals_predsSVR, 1, max))  
min_max_accuracySVR 

actSVR <- actuals_predsSVR[order(runif(50000)), ]

#plot
library(ggplot2)
ggplot(actSVR, aes(x = actSVR$actualsSVR , y = actSVR$predictedsSVR)) +
  ggtitle("Support Vector Regression")+
  xlab("Actual Value") + ylab("Predicted Value") +
  geom_segment(aes(xend = actualsSVR, yend = predictedsSVR)) +
  geom_point()+ 
  geom_point(aes(y = actSVR$predictedsSVR), shape = 1, colour = 'red')


### KFOLD
l5 <- trainControl(method = "cv", number = 10, search = "random")
l6 <- trainControl(method = "repeatedcv", number = 2, search = "random")

install.packages('e1071')
library(e1071)
library(caret)
set.seed(123)
ModelBoot <- train(winPlacePerc  ~ walkDistance + boosts + killPlace + killStreaks, data = train, method = "rf", ntree = 100, preProcess = "scale",
                   trControl = l6)
ModelBoot

set.seed(123)
ModelBoot1 <- train(winPlacePerc  ~ walkDistance + boosts + killPlace + killStreaks, data = train, method = "rf", ntree = 100, preProcess = "scale",
                    trControl = l5)
ModelBoot1


#kfold LM
ModelBoot2 <- train(winPlacePerc  ~ walkDistance + boosts + killPlace + killStreaks, data = train, method = "lm", ntree = 100, preProcess = "scale",
                    trControl = l6)
ModelBoot2

set.seed(123)
ModelBoot3 <- train(winPlacePerc  ~ walkDistance + boosts + killPlace + killStreaks, data = train, method = "lm", ntree = 100, preProcess = "scale",
                    trControl = l5)
ModelBoot3

ModelBoot4 <- train(winPlacePerc  ~ walkDistance + boosts + killPlace + killStreaks, data = train, method = "svmLinear", ntree = 100, preProcess = "scale",
                    trControl = l6)
ModelBoot4

set.seed(123)
ModelBoot5 <- train(winPlacePerc  ~ walkDistance + boosts + killPlace + killStreaks, data = train, method = "svmLinear", ntree = 100, preProcess = "scale",
                    trControl = l5)
ModelBoot5


