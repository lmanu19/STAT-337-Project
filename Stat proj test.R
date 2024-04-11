# set working directory
setwd("C:/Users/laure/OneDrive/Documents/GitHub/STAT-337-Project/")
install.packages("foreign")
library(foreign)
data <- read.arff("C:/Users/laure/OneDrive/Documents/GitHub/STAT-337-Project/bone+marrow+transplant+children/bone-marrow.arff")


# View the first few rows of the data
head(data)

# Get summary statistics of the data
summary(data)

# Perform data manipulation or analysis tasks
# For example:
# Compute mean of a column, ex- donorage
mean(data$Donorage)
