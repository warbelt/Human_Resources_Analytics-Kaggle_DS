# HUMAN RESOURCES ANALYTICS KAGGLE DATASET - PREDICTING EMPLOYEE TURNOVER WITH DECISION TREES

## AREAS OF IMPROVEMENT #################################################################################################

# 1 - (insert area of improvement)
# 2 - (insert area of improvement)

## CONTENT ##############################################################################################################

# 0 - AN EXAMPLE OF FORMATTABLE
# 1 - LOAD THE DATA
# 2 - DATA PREPARATION
# 3 - RUN THE DECISION TREE MODEL
# 4 - MAKING PREDICTIONS AND ASSESSING ACCURACY OF THE DECISION TREE MODEL

## CODE ##############################################################################################################


# 0 - AN EXAMPLE OF FORMATTABLE

# The below code will install the formattable package if it doesn't exist, and then load it
if (!require(formattable)) install.packages("formattable")
library(formattable)

data %>% 
  count(salary) %>% 
  formattable(align = 'l')

# 1 - LOAD THE DATA
data <- read.csv("./HR_comma_sep_v2.csv") # Remeber to set the working directory for this line of code to work

# 2 - DATA PREPARATION
n <- nrow(data)
idx <- sample(n, n * .66) # so that the split is 2/3 of the data

# The below code will install the dplyr package if it doesn't exist, and then load it
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Make a few modications
data %>% 
  mutate(
    left = factor(left, labels = c("Remain", "Left")),
    salary = ordered(salary, c("low", "medium", "high"))
  ) -> d

# Split the dataset into test and train
train <- d[idx, ]
test <- d[-idx, ]

# 3 - RUN THE DECISION TREE MODEL

tree <- rpart(left ~ ., data = train)

res <- predict(tree, test)

# AUC Score

# The below code will install the Metrics package if it doesn't exist, and then load it
if (!require(Metrics)) install.packages("Metrics")
library(Metrics)

auc(as.numeric(test$left) - 1, res[, 2])

# Visualise the tree

rpart.plot(tree, type = 2, fallen.leaves = F, cex = 1, extra = 2)

# Interpreting the decision tree
# 1 - The most important variable is satisfaction level. If its higher than 0.46 you are much more likely to stay.
# 2 - If you have high satisfaction, having worked less than 4.5 years make you more likely to remain.
# 3 - If you have low satisfaction, the number of projects becomes important - more than 2.5 projects make you more likely to stay.

# 4 - MAKING PREDICTIONS AND ASSESSING ACCURACY OF THE DECISION TREE MODEL

# Prediction on the test dataset

Prediction <- predict(tree, test,type ="class")

# Assess the accuracy of the decision tree model using the test dataset

misClasificError <- mean(Prediction !=test$left)
print(paste('Accuracy',1-misClasificError))

# Create a file with the decision tree predictions

#The satisfaction level should be replaced by the index or employee id
submit <- data.frame(SatisfactionLevel=test$satisfaction_level,Leaver=Prediction)
write.csv(submit,file="Human Resources Analytics - Decision Tree Predictions.csv",row.names=FALSE)
