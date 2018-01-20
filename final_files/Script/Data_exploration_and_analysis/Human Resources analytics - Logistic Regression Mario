# Trains a logistic regression model for given dataset and formula
### @ds <- dataset for training
### @f <- formula for training
### @model -> trained logistic regression model
logTraining <- function(ds, f, verbose = FALSE) {
  model <- glm(f, family=binomial(link='logit'), data=ds)
  nullModel <- glm(left ~1, family=binomial(link='logit'), data=ds)
  r2 <- 1-logLik(model)/logLik(nullModel)
  
  if(verbose == TRUE) {
    print(summary(model))
    print(exp(coef(model)))
    writeLines("---------\nMCFadden R-Squared log likelihood")
    print(r2)
    writeLines("---------\n")
  }
  
  return(model)
}

# Predicts over a dataset given a logistic regression model
### @model <- logistic regression model
### @ds <- dataset to predict
### @pred -> logistic probabilities array, values ranging from 0 to 1
logPrediction <- function(model, ds) {
  probs <- predict(model, ds)
  pred <- ifelse(probs > 0.5, 1, 0)
  accuracy <- mean(pred == ds$left)
  
  return(list("probs" = probs, "pred" = pred, "accuracy" = accuracy))
}

# Computes and plots learning curves
learningCurves <- function(train, cv) {
  error_cv = c()
  error_train = c()
  train.probs = c()
  test.probs = c()
  
  sequence <- head(seq(from = 10, to = nrow(train), by = 10), -1)
  for (i in sequence) {
    ##Create model from first i training examples
    model <- glm(left ~., family=binomial(link='logit'), data=train[1:i,])
    
    ##Check accuracy for test and train sets
    cv.results <- predict(model,newdata=cv,type='response')
    cv.probs <- cv.results
    cv.results <- ifelse(cv.results > 0.5,1,0)
    error_cv[i] <- mean(cv.results != cv$left)
    
    
    train.results <- predict(model,newdata=train[1:i,],type='response')
    train.probs <- train.results
    train.results <- ifelse(train.results > 0.5,1,0)
    error_train[i] <- mean(train.results != train[1:i,]$left)
  }

  ## Save raw probability of survival for visualization purposes
  #combined_probs = c(train.probs, cv.probs)
  #df_probs <- data.frame(1:(dim(train)[1] + dim(cv)[1]),combined_probs)
  #names(df_probs) <- c("PassengerId", "Survival probability")
  #write.csv(df_probs, file="survival_probabilities_lr.csv", row.names = FALSE)

  ## Plot Results
  plot(1:dim(error_cv*10), error_cv, type='l', col='green', ylim=c(0,0.5), xlab='Training examples', ylab='error %', main='Learning curve')
  lines(1:dim(error_cv*10), error_train, col='red')
  legend("topright", inset=.05, title="Data set", c("Train", "Test"), fill=c('red', 'green'), horiz=TRUE)
}

##### LIBRARIES
library(readr)
library(dplyr)
library(caret)

## CODE #####
# 1 - LOAD THE DATA
data <- read_csv("final_files/databases/HR_comma_sep_v2.csv")

# Data preparation
data$salary <- as.numeric(as.factor(data$salary))
data$department <- as.numeric(as.factor(data$department))

# Split the dataset into test, cv and train
n <- nrow(data)
idx <- sample(n, n * .80) # so that the split is 4/5 of the data

trainAndCV <- data[idx, ]
test_ds <- data[-idx, ]

n <- nrow(trainAndCV)
idx <- sample(n, n * .80)

train_ds <- trainAndCV[idx, ]
cv_ds <- trainAndCV[-idx, ]


# 2 - Model training
model <- logTraining(train_ds, formula(left ~ .), TRUE)
print(anova(model, test="Chisq"))

prediction <- logPrediction(model, cv_ds)
writeLines("---------\nAccuracy for cv_ds")
print(prediction$accuracy)

prediction <- logPrediction(model, test_ds)
writeLines("\nAccuracy for test_ds")
print(prediction$accuracy)

print(confusionMatrix(test_ds$left, prediction$pred))
#https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package

### Second degree polynomials
model2 <- logTraining(train_ds, formula(left ~ .^2), TRUE)
prediction2 <- logPrediction(model2, cv_ds)
writeLines("---------\nAccuracy for cv_ds")
print(prediction2$accuracy)

prediction2 <- logPrediction(model2, test_ds)
writeLines("\nAccuracy for test_ds")
print(prediction2$accuracy)

print(confusionMatrix(test_ds$left, prediction2$pred))
writeLines("\nComparing to First degree polynomials")
print(confusionMatrix(test_ds$left, prediction$pred))
# Learning curves
#learningCurves(train_ds, cv_ds) #Cant make this work

# Fit of the regression model
library(ROCR)

p <- predict(model2, newdata=test_ds, type="response")
pr <- prediction(p, test_ds$left)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)
# 0.9653 means an extremely powerful predictor for a TPR standpoint

## PCR ##########################################################
# Principal Component Analysis
pcrdata <- (select(data, -c(left)))
pcrdata_comps <- prcomp(pcrdata, center=TRUE, scale.=TRUE)
print(summary(pcrdata_comps))

# Project database on new space and perform regression on it to obtain a prediction model
library(pls)
pcr_model <- pcr(left~., data = train_ds, scale = TRUE, validation = "CV")
summary(pcr_model)

# Predict results using PCA model
pcr_pred <- predict(pcr_model, train_ds, ncomp = 7)
mean((pcr_pred - test_ds$left))
