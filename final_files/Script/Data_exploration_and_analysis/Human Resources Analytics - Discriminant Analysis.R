
#####DISCRIMINANT ANALYSIS#####

###LOAD DATABASE###

library(readr)
HR_comma_sep_v2 <- read_csv("~/Human_Resources_Analytics-Kaggle_DS/final_files/databases/HR_comma_sep_v2.csv")


###SPLIT TRAIN AND TEST DATABASE###

#First, we split our database in train and test databases.

# load the libraries

library(caret)
library(klaR)


# define an 80%/20% train/test split of the dataset

split<-0.80
trainIndex <- createDataPartition(HR_comma_sep_v2$left, p=split, list=FALSE)
train <- HR_comma_sep_v2[ trainIndex,]
test <- HR_comma_sep_v2[-trainIndex,]



###DISCRIMINANT ANALYSIS###

We need to load the "MASS" package.

require(MASS)

##Training model##

#First we do the model to our training data.

lda <- lda(left ~.,data=train)
lda

#We can see the means of each variable and the general probability to belong to the leavers and the non-leavers.

###PREDICTION###

Now we apply this model to our test data. 

plda <- predict(object = lda, newdata = test)
prcla<-plda$class
table<-table(test$left, prcla, dnn = c('Actual Group','Predicted Group'))
accuracy<-sum(diag(prop.table(table)))
accuracy
table

#We can see that the accuracy is an 81% (worse than the logistic regression) and below we can see the table of the prediction and real data.

##Plot the LDA##

require(ggplot2)
require(scales)
require(gridExtra)

prop.lda <- lda$svd^2/sum(lda$svd^2)
test$left<-factor(test$left)
dataset<-data.frame(left = test[,"left"], lda = plda$x)
p1 <- ggplot(dataset) + geom_point(aes(plda$x, plda$x, colour = left, shape = left), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))
p1



#Here, we can see that there are more mistakes in our model, because one can see more mixed colours.



###CROSS VALIDATION###

#Now we are going to do a cross validation of our model, and see the accuracy and the table of it.

ldacv<-lda(left ~.,data=HR_comma_sep_v2, CV=TRUE)
probstay<-ldacv$posterior[,1]
predictioncv<-ifelse(probstay<0.5, 1, 0)
reavalues<-HR_comma_sep_v2$left
table<-table(HR_comma_sep_v2$left, predictioncv, dnn = c('Actual Group','Predicted Group'))
accuracy<-sum(diag(prop.table(table)))
accuracy
table

#The accuracy is sightly better when applied the cross validation. 
