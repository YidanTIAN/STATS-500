# STATS 500 Homework 1 Problem 1
install.packages('faraway')
library(faraway)
data(teengamb)

# Assign labels to variable sex
# Note: labels correctly correspond to the numerical codes in your data
teengamb$sex <- factor(teengamb$sex, labels = c('male', 'female'))

# Numerical summary
summary(teengamb)

# Graphical summary
par(mfrow = c(2,3))
hist(teengamb$income, main = 'Income distribution', xlab = 'Income')
hist(teengamb$verbal, main = 'Verbal score distribution', xlab = 'Verbal score')
hist(teengamb$gamble, main = 'Gambling expenditure distribution', xlab = 'Expenditure')
boxplot(income~sex, data = teengamb, main = 'Income by sex', xlab = 'Sex', ylab = 'Income')
boxplot(verbal~sex, data = teengamb, main = 'Verbal score by sex', xlab = 'Sex', ylab = 'Verbal score')
boxplot(gamble~sex, data = teengamb, main = 'Gambling expenditure by sex', xlab = 'Sex', ylab = 'Expenditure')

# Means and medians of "income" and "gamble"
mean_income <- mean(teengamb$income)
median_income <- median(teengamb$income)
mean_gamble <- mean(teengamb$gamble)
median_gamble <- median(teengamb$gamble)

mean_income
median_income
mean_gamble
median_gamble

# Unique values for "verbal"
unique_verbal_values <- unique(teengamb$verbal)
num_unique_verbal_values <- length(unique_verbal_values)

num_unique_verbal_values
unique_verbal_values

# Summary by 'sex'
tapply(teengamb$income, teengamb$sex, summary)
tapply(teengamb$verbal, teengamb$sex, summary)
tapply(teengamb$gamble, teengamb$sex, summary)

