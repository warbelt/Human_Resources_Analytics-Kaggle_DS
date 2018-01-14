###BIPLOT###


##Load database##
library(readr)
HR_comma_sep_v2 <- read_csv("~/Human_Resources_Analytics-Kaggle_DS/final_files/databases/HR_comma_sep_v2.csv")

##Install package##

#To do a biplot analysis, first, we need the package "MultBiplotR".

install.packages("http://biplot.usal.es/classicalbiplot/multbiplot-in-r/multbiplotrtar.gz", repos = NULL, type="source")
require(MultBiplotR)

#We need all our variables to be cuantitative to be able to do a biplot. 

require(dplyr)
cuanti<-select(HR_comma_sep_v2, -c(department, salary))

###RECODE SATISFACTION LEVEL###

#We are going to recode the variable satisfaction level to a qualitative variable see the different levels if different colours in our plot. We are going to do the same in our plot with the salary.

require(car)
cuanti$satisnom<-recode(cuanti$satisfaction_level, "0.01:0.25='VL'")
cuanti$satisnom<-recode(cuanti$satisnom, "0.26:0.50='L'")
cuanti$satisnom<-recode(cuanti$satisnom, "0.51:0.75='H'")
cuanti$satisnom<-recode(cuanti$satisnom, "0.76:1='VH'")
cuanti$satisnom<-as.factor(cuanti$satisnom)


#BIPLOT

#We use command "scale" to standarize our data.

biplot<-PCA.Biplot(scale(cuanti[1:9]))
plot(biplot)

#In this plot we can barely do some interpretation (besides the relationship between variables), so we are going to delete the Labels and put some colour.

biplot=AddCluster2Biplot(biplot, ClusterType="us", Groups=cuanti$satisnom)
plot(biplot, LabelInd = FALSE, xlim=c(-4,4), ylim=c(-4,4), CexInd=0, PlotClus = TRUE, TypeClus = "ch", ClustCenters = T)

#We can see here the relationship between variables. We can see that the "montly hours"", the "last evaluation"" and the "number of projects"", are very correlated between them, but those three are incorrelated to the "left"" varaible, the "time spent in the company"", and the "satisfaction level". The latter are negatively correlated with the first two variables. Lastly, we can see that the "work accident"" variable and the "promotion"" variable are not well represented in the first two dimensions. 

#We can see that the clusters are all in the same place, although a little displaced having in mind the satisfaction level vector (which is obvious, because the colours are the levels of the satisfaction level). 

plot(biplot, LabelInd = FALSE, CexInd=0.5, ColorInd=cuanti$satisnom)
legend(x="topright", c("VL", "L", "H", "VH"), pch=1, col=c("Purple", "Green", "Red", "Skyblue"))

#This is the plot with all the individuals.

##Salary##

#Now we are going to do the same with salary, to separate the different levels of salary with colours.


HR_comma_sep_v2$salary<-as.factor(HR_comma_sep_v2$salary)

biplot=AddCluster2Biplot(biplot, ClusterType="us", Groups=HR_comma_sep_v2$salary)

#The clusters are all in the same place, so the individuals cannot be interpretated too.

plot(biplot, LabelInd = FALSE, CexInd=0.5, ColorInd=HR_comma_sep_v2$salary)
legend(x="topright", c("LOW", "MEDIUM", "HIGH"), pch=1, col=c("Green", "Blue", "Red"))

##HJ BIPLOT##

#An HJ-Biplot has a good representation on both variables and individuals, but it is supposed that it doesn?t represent well the original data.

hjbiplot<-HJ.Biplot(scale(cuanti[1:9]))
plot(hjbiplot)

#We can see that the vectors of the variables are too long compared to the dispersion of the individuals, so we have to zoom to see the situation of them. Doing that, we cannot see which vector belongs to which variable, so we have to remember the first plot. The relationship between variables is the same that we saw in the PCA Biplot.

plot(hjbiplot, xlim=c(-4,4), ylim=c(-4,4))

hjbiplot=AddCluster2Biplot(hjbiplot, ClusterType="us", Groups=cuanti$satisnom)
plot(hjbiplot, LabelInd = FALSE, xlim=c(-4,4), ylim=c(-4,4), CexInd=0, PlotClus = TRUE, TypeClus = "ch", ClustCenters = T)

plot(hjbiplot, LabelInd = FALSE, CexInd=0.5, xlim=c(-4,4), ylim=c(-4,4), ColorInd=cuanti$satisnom)
legend(x="topright", c("VL", "L", "H", "VH"), pch=1, col=c("Purple", "Green", "Red", "Skyblue"))

#In these plots we cannot interpretate more information than the PCA-Biplot.

##Salary##


hjbiplot=AddCluster2Biplot(hjbiplot, ClusterType="us", Groups=HR_comma_sep_v2$salary)
plot(hjbiplot, LabelInd = FALSE, xlim=c(-4,4), ylim=c(-4,4), CexInd=0, PlotClus = TRUE, TypeClus = "ch", ClustCenters = T)


plot(hjbiplot, LabelInd = FALSE, CexInd=0.5, xlim=c(-4,4), ylim=c(-4,4), ColorInd=HR_comma_sep_v2$salary)
legend(x="topright", c("LOW", "MEDIUM", "HIGH"), pch=1, col=c("Green", "Blue", "Red"))

