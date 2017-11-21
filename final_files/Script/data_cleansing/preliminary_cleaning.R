# Create new column: average number of prejects per year
# Rename column sales to department

library(dplyr)
library(readr)

ds <- read_csv(file.choose())

ds <- mutate(ds, projects_per_year = number_project/time_spend_company)
ds <- rename(ds, department = sales)

write_csv(ds, file.choose(new = TRUE))
