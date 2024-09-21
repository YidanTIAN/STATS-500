## STATS 500 Homework 2 Problem 1
library(ggplot2)
library(faraway)
data(uswages)
uswages_clean <- subset(uswages, exper >= 0)
summary(uswages_clean)

# Fit the regression model
model <- lm(wage ~ educ + exper, data = uswages_clean)
summary(model)

r_squared <- summary(model)$r.squared
cat("R-squared: ", r_squared * 100, "%\n")

# Identify the observation with the largest residual
residuals <- resid(model)
largest_residual_case <- which.max(residuals)
cat("Case with largest positive residual: ", largest_residual_case, "\n")

# Compute the mean and median of the residuals
mean_residual <- mean(residuals)
median_residual <- median(residuals)
cat("Mean of residuals: ", mean_residual, "\n")
cat("Median of residuals: ", median_residual, "\n")

# Calculate the difference in predicted weekly wages
coef_experience <- coef(model)["exper"]
cat("Difference in predicted weekly wages for 1 year difference in experience: ", coef_experience, "\n")

# Compute the correlation of the residuals
fitted_values <- fitted(model)
cor_residuals_fitted <- cor(residuals, fitted_values)
cat("Correlation of residuals with fitted values: ", cor_residuals_fitted, "\n")

# Plot residuals vs fitted values
ggplot(data.frame(Fitted = fitted_values, Residuals = residuals), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals")


## STATS 500 Homework 2 Problem 3
# Generate data
set.seed(123)
x <- 1:20
y <- x + rnorm(20)

# Fit polynomial models
for (degree in 1:20) {
  formula <- as.formula(paste("y ~ ", paste0("I(x^", 1:degree, ")", collapse = " + ")))
  model <- lm(formula, data = data.frame(x, y))
  cat("Polynomial degree:", degree, "\n")
  print(coef(model))  # Print coefficients from lm()
}

# Direct calculation of beta using OLS formula
ols_direct <- function(x, y, degree) {
  X <- as.matrix(cbind(1, sapply(1:degree, function(d) x^d)))
  beta_direct <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta_direct)
}

# Calculate beta for degrees
for (degree in 1:20) {
  beta_direct <- ols_direct(x, y, degree)
  cat("Direct OLS calculation for degree", degree, ":\n")
  print(beta_direct)
}


