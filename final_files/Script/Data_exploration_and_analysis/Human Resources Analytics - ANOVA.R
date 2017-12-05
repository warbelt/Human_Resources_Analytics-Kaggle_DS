# HUMAN RESOURCES ANALYTICS KAGGLE DATASET - ANOVA ANALYSIS ON TURNOVER

## AREAS OF IMPROVEMENT #################################################################################################

# 1 - Add some plots
# 2 - Interpret well the result of the statistically significant interaction

## CONTENT ##############################################################################################################

# 1 - LOAD THE DATA
# 2 - LOAD/INSTALL LIBRARIES
# 3 - FIND THE CATEGORICAL VARIABLES
# 4 - ANOVA

## CODE ##############################################################################################################


# 1 - LOAD THE DATA
data <- read.csv("./HR_comma_sep_v2.csv") # Remeber to set the working directory for this line of code to work

# 2 - LOAD/INSTALL LIBRARIES
library(ggplot2)

# The below code will install the RcmdrMisc package if it doesn't exist, and then load it
if (!require(RcmdrMisc)) install.packages("RcmdrMisc")
library(RcmdrMisc)

# 3 - FIND THE CATEGORICAL VARIABLES

# Check which variables in our dataset are categorical/factor, as we will apply ANOVA to those
sapply(data, class)

# In this dataset the variables that are factors are department and salary

# 4 - ANOVA

# ANOVA: TURNOVER VS SALARY LEVEL

# Run the ANOVA
AnovaModel.salary <- (lm(data$left ~ salary, data=data))
Anova(AnovaModel.salary)

with(data, (tapply(data$left, list(salary),
                           mean, na.rm=TRUE)))

# Interpret the ANOVA

# The test shows that the turnover rate is statistically signi???cantly di???erent between salary levels (Pr(>F) < .05)

# ANOVA: TURNOVER VS DEPARTMENT

# Run the ANOVA
AnovaModel.department <- (lm(data$left ~ department, data=data))
Anova(AnovaModel.department)

with(data, (tapply(data$left, list(department),
                   mean, na.rm=TRUE)))

# Interpret the ANOVA

# The test shows that the turnover rate is statistically signi???cantly di???erent between dapartments (Pr(>F) < .05)

# ANOVA: TURNOVER VS INTERACTION (SALARY LEVEL & DEPARTMENT)

# Run the ANOVA
AnovaModel.salary_and_department <- (lm(data$left ~ salary*department, data=data))
Anova(AnovaModel.salary_and_department)

with(data, (tapply(data$left, list(salary,department),
                   mean, na.rm=TRUE)))

# Interpret the ANOVA

# The test shows that the interaction salary*department is statistically signi???cant (Pr(>F) < .05)
