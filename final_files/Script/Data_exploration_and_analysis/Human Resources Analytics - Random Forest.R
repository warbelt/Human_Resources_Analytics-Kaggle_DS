# HUMAN RESOURCES ANALYTICS KAGGLE DATASET - PREDICTING EMPLOYEE TURNOVER WITH RANDOM FORESTS

## AREAS OF IMPROVEMENT #################################################################################################

# 1 - Understand better the plot of the variables importance
# 2 - (insert area of improvement)

## CONTENT ##############################################################################################################

# 1 - LOAD THE DATA
# 2 - LOGISTIC REGRESSION
# 3 - ASSESSING THE PREDICTIVE ABILITY OF THE RANDOM FOREST MODEL

## CODE ##############################################################################################################

# 1 - LOAD THE DATA
data <- read.csv("./HR_comma_sep_v2.csv") # Remeber to set the working directory for this line of code to work

# Split the dataset into test and train
n <- nrow(data)
idx <- sample(n, n * .66) # so that the split is 2/3 of the data

train <- data[idx, ]
test <- data[-idx, ]

dim(test)
dim(train)

# 2- RANDOM FOREST

set.seed(2017)

# The below code will install the rpart package if it doesn't exist, and then load it
if (!require(rpart)) install.packages("rpart")
library(rpart)

attach(train)

# Load the randomForest library
# The below code will install the randomForest package if it doesn't exist, and then load it
if (!require(randomForest)) install.packages("randomForest")
library(randomForest)

# Running the Random Forest model

fit <- randomForest(as.factor(left)~.,
                    data=train,
                    importance=TRUE,
                    ntree=2000)

# Visualise what variables are important

varImpPlot(fit)

# There's two types of importance measures shown above.
# 1 - The accuracy one tests to see how worse the model performs without each variable, so a high decrease in
# accuracy would be expected for very predictive variables.
# 2 - The Gini one digs into the mathematics behind decision trees, but essentially measures how pure the
# nodes are at the end of the tree. Again it tests to see the result if each variable is taken out and a high score
# means the variable was important.

# 3 - ASSESSING THE PREDICTIVE ABILITY OF THE RANDOM FOREST MODEL

#Generate the prediction of the random forest on the test dataset using the ???t model generated using the train_cleaned database

#Prediction on the test dataset
Prediction <- predict(fit, test)

# Assess the accuracy of the random forest model using the test dataset
misClasificError <- mean(Prediction !=test$left)
print(paste('Accuracy',1-misClasificError))
