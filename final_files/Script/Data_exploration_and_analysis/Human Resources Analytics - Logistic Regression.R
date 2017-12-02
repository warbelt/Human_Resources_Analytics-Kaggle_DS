# HUMAN RESOURCES ANALYTICS KAGGLE DATASET - PREDICTING EMPLOYEE TURNOVER WITH LOGISTIC REGRESSION

## AREAS OF IMPROVEMENT #################################################################################################

# 1 - Add a ROC plot
# 2 - Manage to export predicted probabilities using the cross-validated model
# 3 - Add a confusion matrix with proportions

## CONTENT ##############################################################################################################

# 1 - LOAD THE DATA
# 2 - LOGISTIC REGRESSION
# 3 - ASSESSING THE PREDICTIVE ABILITY OF THE MODEL
# 4 - APPLY CROSS-VALIDATION TO THE LOGISTIC REGRESSION MODEL

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

# Scatterplot matrix

# The below code will install the caTools package if it doesn't exist, and then load it
if (!require(GGally)) install.packages("GGally")
library(GGally)

##'smooth' adds linear fitted line to each scatter plot
ggpairs(train,lower = list(continuous="smooth"))

# 2- APPLY CROSS-VALIDATION TO THE LOGISTIC REGRESSION MODEL

# 10 Fold Cross Validation for Logistic Regression
# To evaluate and critique the models, we need to train the model using part of the data and hold out a portion
# to test on. We will divide the data into 10 parts- using 9 parts as training data and 1 part as testing data,
# and alternate which was the 9 and the 1, so that each of the 10 parts gets to be training data 9 times and
# testing data once. This is called ten-fold cross-validation.

# The below code will install the caret package if it doesn't exist, and then load it
if (!require(caret)) install.packages("caret")
library(caret)

#Install the below package if you get the following message: Error in requireNamespaceQuietStop("e1071") : package e1071 is required
#install.packages('e1071', dependencies=TRUE)

# Define training control
train_control <- trainControl(method="cv", number=10)
train$left<-as.factor(train$left)

# Fix the parameters of the algorithm
grid <- expand.grid()

# 3- LOGISTIC REGRESSION MODEL

# Train the model
logit_model <- train(left~., data=train, trControl=train_control, method="glm",family=binomial())

#Install the below package if you get the following message: Error in requireNamespaceQuietStop("e1071") : package e1071 is required
#install.packages('e1071', dependencies=TRUE)

# Summarize results of the cross-validation
print(logit_model) 

summary(logit_model)

# McFadden R2 index can be used to assess the model fit

# values from 0.2-0.4 indicate (in McFadden's words) excellent model fit (https://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation)

library(pscl)
pR2(logit_model)

# Tailored summary of the logistic regression model
exp(cbind(Coefficient = coef(logit_model), confint(logit_model)))
Variable_Coefficient<- exp(cbind(Coefficient = coef(logit_model)))
Variable_Significance<- coef(summary(logit_model))[,4]
Numerical_insight<-1/Variable_Coefficient
Log_regression_summary_Andres<-cbind(Variable_Significance,Variable_Coefficient,Numerical_insight)
Log_regression_summary_Andres
formattable(Log_regression_summary_Andres)

# Insights
print("For every increment of 1 unit in the performance score, the employee is 28,11% more likely to leave")
print("For every increment of 1 year in tenure, the employee is 22,68% more likely to leave")

# 4 - ASSESSING THE PREDICTIVE ABILITY OF THE MODEL

# Make a prediction and compare results against the test dataset

# EXPAND CODE - Cannot use predict with the model generated with the cross validation, need to find a way to fix that
logit_model<- glm(left~.,data=data,binomial())

newdata <- select(test,-left) ## In newdata you have to include a selection of the columns that are part of your model (your independent variables)
fitted.results <- predict(logit_model,newdata,type='response')

## Save raw probability of survival for visualization purposes
write.csv(fitted.results,file="leaver_probabilities_HRA.csv",row.names=TRUE)

write.csv(test$left,file="test$left_HRA.csv",row.names=TRUE)

# Confusion matrix
predict <- predict(logit_model, newdata,type = 'response')
table(test$left,predict>0.5)

# TO EXPAND CODE Generate the confusion matrix showing proportions

# Calculate accuracy
fitted.results <- ifelse(fitted.results >0.5,1,0)
## the output of predict is a value in range 0 to 1. If the probability is greater than 0.5 then we give it a 1, otherwise we assign 0 to that value
misClasificError <- mean(fitted.results !=test$left)
print(paste('Accuracy',1-misClasificError))

library(ggplot2)
rocplot(logit_model)


