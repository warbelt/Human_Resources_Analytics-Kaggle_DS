#BASIC EXPLORATORY SCRIPT

library(readr)
HR_comma_sep_v2 <- read_csv("~/Kaggle/2. Human Resources/Raw Data/HR_comma_sep_v2.csv")

require(ggplot2)
attach(HR_comma_sep_v2)

summary(HR_comma_sep_v2)

#QUICKPLOTS (HISTOGRAMS)

quickplot(satisfaction_level)
#Most of the people are happy with the company.
quickplot(last_evaluation)
#Most of the people has an average performance, but there are only a few that has a below-average performance.
quickplot(number_project)

quickplot(average_montly_hours)

quickplot(time_spend_company)
#Most of the people are relatively new to the company.
quickplot(Work_accident)
quickplot(left)
#A 25% of the total individuals have left the company.
quickplot(promotion_last_5years)
#Only a few people have been promoted.
quickplot(department)
#Most of the people are in the sales department.
quickplot(salary)
#The salary in this company are, in the mos cases low or medium.
quickplot(projects_per_year)


require(plyr)

count(left)

#BOXPLOTS

###Now we are going to do some boxplots of the cuantitative variables, to have a "more accurate" plot.

##Single Boxplots

boxplot(satisfaction_level,data=HR_comma_sep_v2, main="Satisfaction Level")
boxplot(last_evaluation,data=HR_comma_sep_v2, main="last evaluation")
boxplot(number_project,data=HR_comma_sep_v2, main="number project")
boxplot(average_montly_hours,data=HR_comma_sep_v2, main="average_montly_hours")
boxplot(time_spend_company,data=HR_comma_sep_v2, main="time_spend_company")
boxplot(projects_per_year,data=HR_comma_sep_v2, main="projects_per_year")

##Relation boxplots 

###Satisfaction level
#The people that had an accident, has a better satisfaction level. Maybe they are less burned.
boxplot(satisfaction_level~Work_accident,data=HR_comma_sep_v2, main="Satisfaction Level", 
        xlab="Work Accident", ylab="Satisfaction Level")
#The people that left were less happy.
boxplot(satisfaction_level~left,data=HR_comma_sep_v2, main="Satisfaction Level", 
        xlab="left", ylab="Satisfaction Level")
#The people that were recently promoted is slightly happier.
boxplot(satisfaction_level~promotion_last_5years,data=HR_comma_sep_v2, main="Satisfaction Level", 
        xlab="promotion in the last 5 years", ylab="Satisfaction Level")
#The people with low salary is less happy in the work.
boxplot(satisfaction_level~salary,data=HR_comma_sep_v2, main="Satisfaction Level", 
        xlab="Salary", ylab="Satisfaction Level")
boxplot(satisfaction_level~department,data=HR_comma_sep_v2, main="Satisfaction Level", 
        xlab="Department", ylab="Satisfaction Level", cex.axis=0.45)
boxplot(satisfaction_level~number_project,data=HR_comma_sep_v2, main="Satisfaction Level", 
        xlab="number_project", ylab="Satisfaction Level")
boxplot(satisfaction_level~time_spend_company,data=HR_comma_sep_v2, main="Satisfaction Level", 
        xlab="time_spend_company", ylab="Satisfaction Level")
###Last evaluation
#The people with the best performance is the people that it's going from the company.
boxplot(last_evaluation~left,data=HR_comma_sep_v2, main="Last evaluation", 
        xlab="left", ylab="last evaluation")
boxplot(last_evaluation~promotion_last_5years,data=HR_comma_sep_v2, main="Last evaluation", 
        xlab="promotion in the last 5 years", ylab="last evaluation")

boxplot(last_evaluation~salary,data=HR_comma_sep_v2, main="Last evaluation", 
        xlab="Salary", ylab="Last evaluation")
boxplot(last_evaluation~department,data=HR_comma_sep_v2, main="Last evaluation", 
        xlab="Department", ylab="Last evaluation", cex.axis=0.45)
boxplot(last_evaluation~number_project,data=HR_comma_sep_v2, main="Last evaluation", 
        xlab="number_project", ylab="Last evaluation")
boxplot(last_evaluation~time_spend_company,data=HR_comma_sep_v2, main="Last evaluation", 
        xlab="time_spend_company", ylab="Last evaluation")

###Number of projects
#CURIOSO TAMBIEN
boxplot(number_project~promotion_last_5years,data=HR_comma_sep_v2, main="Number of projects", 
        xlab="promotion in the last 5 years", ylab="Number of projects")
boxplot(number_project~salary,data=HR_comma_sep_v2, main="Number of projects", 
        xlab="Salary", ylab="Number of projects")
boxplot(number_project~department,data=HR_comma_sep_v2, main="Number of projects", 
        xlab="Department", ylab="Number of projects", cex.axis=0.45)
boxplot(number_project~time_spend_company,data=HR_comma_sep_v2, main="Last evaluation", 
        xlab="time_spend_company", ylab="Last evaluation")
###Monthly hours

#The people with more montly hours, is probably burned, and for that leaves the company.
boxplot(average_montly_hours~left,data=HR_comma_sep_v2, main="Average Montly Hours", 
        xlab="left", ylab="Average Montly Hours")
#When they work more hours, they got promoted.Not very significant.
boxplot(average_montly_hours~promotion_last_5years,data=HR_comma_sep_v2, main="Average Montly Hours", 
        xlab="promotion in the last 5 years", ylab="Average Montly Hours")
boxplot(average_montly_hours~salary,data=HR_comma_sep_v2, main="Average Montly Hours", 
        xlab="Salary", ylab="Average Montly Hours")
boxplot(average_montly_hours~department,data=HR_comma_sep_v2, main="Average Montly Hours", 
        xlab="Department", ylab="Average Montly Hours", cex.axis=0.45)
boxplot(average_montly_hours~number_project,data=HR_comma_sep_v2, main="Average Montly Hours", 
        xlab="number_project", ylab="Average Montly Hours", cex.axis=0.45)
boxplot(average_montly_hours~time_spend_company,data=HR_comma_sep_v2, main="Average Montly Hours", 
        xlab="time_spend_company", ylab="Average Montly Hours", cex.axis=0.45)

###Time in the company
#The people who were more time in the company were probable more burned and left.
boxplot(time_spend_company~left,data=HR_comma_sep_v2, main="time_spend_company", 
        xlab="left", ylab="time_spend_company")
boxplot(time_spend_company~promotion_last_5years,data=HR_comma_sep_v2, main="time_spend_company", 
        xlab="promotion in the last 5 years", ylab="time_spend_company")
boxplot(time_spend_company~salary,data=HR_comma_sep_v2, main="time_spend_company", 
        xlab="Salary", ylab="time_spend_company")
boxplot(time_spend_company~department,data=HR_comma_sep_v2, main="time_spend_company", 
        xlab="Department", ylab="time_spend_company", cex.axis=0.45)


###Projects per year
#what
boxplot(projects_per_year~left,data=HR_comma_sep_v2, main="projects_per_year", 
        xlab="left", ylab="projects_per_year")
boxplot(projects_per_year~promotion_last_5years,data=HR_comma_sep_v2, main="projects_per_year", 
        xlab="promotion in the last 5 years", ylab="projects_per_year")
#They give more salary if you have mor projects per year.
boxplot(projects_per_year~salary,data=HR_comma_sep_v2, main="projects_per_year", 
        xlab="Salary", ylab="projects_per_year")
boxplot(satisfaction_level~department,data=HR_comma_sep_v2, main="projects_per_year", 
        xlab="Department", ylab="projects_per_year", cex.axis=0.45)
boxplot(projects_per_year~number_project,data=HR_comma_sep_v2, main="projects_per_year", 
        xlab="number_project", ylab="projects_per_year")
boxplot(satisfaction_level~time_spend_company,data=HR_comma_sep_v2, main="projects_per_year", 
        xlab="time_spend_company", ylab="projects_per_year", cex.axis=0.45)


#FREQUENCY TABLES (because of so little people left, the chi-square method is inefficient)

##Left

tabla<-xtabs(~left+Work_accident)
tabla
summary(tabla)

vector<-c(9428, 2000, 3402, 169)
names(vector)<-c("Not left", "Not left", "Left", "Left")
barplot(vector, main="Left", ylim=c(0,10000), xlab="Left", ylab="Frecuencia", col=c("green", "red", "green", "red"))
legend("topright", legend=c("Not accident", "Accident"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla

tabla<-xtabs(~left+promotion_last_5years)
tabla
summary(tabla)

vector<-c(11128, 300, 3552, 19)
names(vector)<-c("Not left", "Not left", "Left", "Left")
barplot(vector, main="Left", ylim=c(0,11500), xlab="Left", ylab="Frecuencia", col=c("green", "red", "green", "red"))
legend("topright", legend=c("Not promotion", "Promotion"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla

tabla<-xtabs(~left+department)
tabla
summary(tabla)

vector<-c(563, 204, 524, 215, 954, 273, 539, 91, 655, 203, 704, 198, 666, 121, 3126, 1014, 1674, 555, 2023, 697)
names(vector)<-c(rep("acc", 2), rep("hr", 2), rep("IT", 2), rep("management", 2), rep("marketing", 2), rep("product_mng", 2), rep("RandD", 2), rep("sales", 2), rep("support", 2), rep("technical", 2))
barplot(vector, main="Left", ylim=c(0,3200), xlab="Department", ylab="Frecuencia", cex.names=0.5, col=rep(c("green", "red"), 10))
legend("topright", legend=c("Not left", "Left"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla

#In the management and RandD department the people stays more.

tabla<-xtabs(~left+salary)
tabla
summary(tabla)

vector<-c(5144, 2172, 5129, 1317, 1155, 82)
names(vector)<-c("Low", "Low", "Medium", "Medium", "High", "High")
barplot(vector, main="Left", ylim=c(0,5200), xlab="Salary", ylab="Frecuencia", col=c("green", "red", "green", "red", "green", "red"))
legend("topright", legend=c("Not left", "Left"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla
#When the people has more salary, there are less probability to left the company.

tabla<-xtabs(~left+number_project)
tabla
summary(tabla)

vector<-c(821, 1567, 3983, 72, 3956, 409, 2149, 612, 519, 655, 0, 256)
names(vector)<-rep(c("2", "3", "4", "5", "6", "7"), 2)
barplot(vector, main="Left", ylim=c(0,4000), xlab="Number of projects", ylab="Frecuencia", col=rep(c("green", "red"), 6))
legend("topright", legend=c("Not left", "Left"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla
#Unless the people with 2 projects, when the people has more projects the probability of leaving increases.

tabla<-xtabs(~left+time_spend_company)
tabla
summary(tabla)

vector<-c(3191, 53, 4857, 1586, 1667, 890, 640, 833, 509, 209, 188, 0, 162, 0, 214, 0)
names(vector)<-rep(c("2", "3", "4", "5", "6", "7", "8", "10"), 2)
barplot(vector, main="Left", ylim=c(0,5000), xlab="Years in company", ylab="Frecuencia", col=rep(c("green", "red"), 8))
legend("topright", legend=c("Not left", "Left"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla

##Salary

tabla<-xtabs(~salary+department)
tabla
summary(tabla)

vector<-c(358, 335, 74, 335, 359, 45, 609, 535, 83, 180, 225, 225, 402, 376, 80, 451, 383, 68, 364, 372, 51, 2099, 1772, 269, 1146, 942, 141, 1372, 1147, 201)
names(vector)<-c(rep("acc", 3), rep("hr", 3), rep("IT", 3), rep("management", 3), rep("marketing", 3), rep("product_mng", 3), rep("RandD", 3), rep("sales", 3), rep("support", 3), rep("technical", 3))
barplot(vector, main="Left", ylim=c(0,2100), xlab="Department", ylab="Frecuencia", cex.names= 0.4, col=rep(c("red", "green", "blue"), 10))
legend("topright", legend=c("Low", "Medium", "High"), fill=c("red", "green", "blue"))

ptabla<-prop.table(tabla, 2)
ptabla

tabla<-xtabs(~salary+number_project)
tabla
summary(tabla)

vector<-c(11128, 300, 3552, 19)
names(vector)<-c("Not left", "Not left", "Left", "Left")
barplot(vector, main="Left", ylim=c(0,11500), xlab="Left", ylab="Frecuencia", col=c("green", "red", "green", "red"))
legend("topright", legend=c("Not promotion", "Promotion"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla

tabla<-xtabs(~salary+time_spend_company)
tabla
summary(tabla)

vector<-c(11128, 300, 3552, 19)
names(vector)<-c("Not left", "Not left", "Left", "Left")
barplot(vector, main="Left", ylim=c(0,11500), xlab="Left", ylab="Frecuencia", col=c("green", "red", "green", "red"))
legend("topright", legend=c("Not promotion", "Promotion"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla

tabla<-xtabs(~salary+promotion_last_5years)
tabla
summary(tabla)

vector<-c(11128, 300, 3552, 19)
names(vector)<-c("Not left", "Not left", "Left", "Left")
barplot(vector, main="Left", ylim=c(0,11500), xlab="Left", ylab="Frecuencia", col=c("green", "red", "green", "red"))
legend("topright", legend=c("Not promotion", "Promotion"), fill=c("green", "red"))

ptabla<-prop.table(tabla, 2)
ptabla

#With the promotion comes a better salary.


#CORRPLOT

##Transform to numeric
require(car)
numeric<-HR_comma_sep_v2
numeric$salary<-recode(numeric$salary, "'low'=1")
numeric$salary<-recode(numeric$salary, "'medium'=2")
numeric$salary<-recode(numeric$salary, "'high'=3")
numeric$salary<-as.numeric(numeric$salary)

require(dplyr)
numeric<-select(numeric, -department)
numeric<-as.numeric(numeric)

library(corrplot)
corTRAIN <- cor(numeric)
corrplot(corTRAIN, method = "circle", tl.cex = 0.5)
corrplot.mixed(corTRAIN, tl.cex = 0.5)
#We can see that the satisfaction level is correlated with the people, if they left or not.
#There is a negative correlation between the time in the company and the projects per year.
#There is positive correlation between the performance and the number of projects and the monthly hours.


#Scatter-Plots (the scatter-plots aren´t working well)
attach(HR_comma_sep_v2)

plot(satisfaction_level,last_evaluation)
plot(satisfaction_level, average_montly_hours)
plot(satisfaction_level, time_spend_company)
plot(satisfaction_level, projects_per_year)


plot(last_evaluation, average_montly_hours)
plot(last_evaluation, projects_per_year)

plot(number_project, average_montly_hours)
plot(number_project, time_spend_company)

plot(average_montly_hours, time_spend_company)
plot(average_montly_hours, projects_per_year)

plot(time_spend_company, projects_per_year)

#OPTIONAL (NOT RECOMMENDED)
HR_comma_sep_v2$sales<-ifelse(HR_comma_sep_v2$department=="sales", 1, 0)
HR_comma_sep_v2$accounting<-ifelse(HR_comma_sep_v2$department=="accounting", 1, 0)
HR_comma_sep_v2$hr<-ifelse(HR_comma_sep_v2$department=="hr", 1, 0)
HR_comma_sep_v2$technical<-ifelse(HR_comma_sep_v2$department=="technical", 1, 0)
HR_comma_sep_v2$support<-ifelse(HR_comma_sep_v2$department=="support", 1, 0)
HR_comma_sep_v2$management<-ifelse(HR_comma_sep_v2$department=="management", 1, 0)
HR_comma_sep_v2$IT<-ifelse(HR_comma_sep_v2$department=="IT", 1, 0)
HR_comma_sep_v2$product_mng<-ifelse(HR_comma_sep_v2$department=="product_mng", 1, 0)
HR_comma_sep_v2$marketing<-ifelse(HR_comma_sep_v2$department=="marketing", 1, 0)
HR_comma_sep_v2$RandD<-ifelse(HR_comma_sep_v2$department=="RandD", 1, 0)
