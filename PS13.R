# Under MLR1–MLR6, (beta_hat_j - beta_j) / sd(beta_hat_j) has a N(0, 1) 
# distribution, but (beta_hat_j - beta_j) / se(beta_hat_j) has a t distribution. 
#   ANS: The standard error replaces the unknown sigma with the estimate sigma_hat, 
# which varies from sample to sample.

# A multiple regression with K = 5 independent variables is estimated on n = 99 observations. 
# How many degrees of freedom does the t-statistic for beta_hat_j have?
# Hint: df = n - K - 1: subtract one for each slope and one for the intercept.
n <- 99
K <- 5
df <- n - K - 1
df

# An estimate is beta_hat_j = -2.34 with se(beta_hat_j) = 0.52. 
# Find the t-statistic for H_0: beta_j = 0. (round to the hundredths)
# Hint: t = beta_hat_j / se(beta_hat_j). It has the same sign as beta_hat_j.
beta_hat_j <- -2.34
se_beta_hat_j <- 0.52
t_stat1 <- round(beta_hat_j / se_beta_hat_j, 2)
t_stat1

# You test H_0: beta_j = 0 against H_1: beta_j > 0 at the 5% level with df = 25, 
# so c = 1.71. The t-statistic is t = 1.52. What do you conclude?
# Hint: against H_1: beta_j > 0, reject H_0 only if t > c.
c <- 1.71
t_stat <- 1.52
if (t_stat > c) {
  conclusion <- "Reject H_0, because t > c."
} else {
  conclusion <- "Fail to reject H_0, because t < c."
}
cat("Conclusion:", conclusion, "\n")
# ANS: Fail to reject H_0, because t < c.

# Data: bwght (bwght = birth weight in ounces; cigs = cigarettes the mother smoked 
# per day while pregnant; faminc = family income in thousands of dollars). 
# Run reg <- lm(bwght ~ cigs + faminc, data = bwght). 
# You want to test H_0: beta_cigs = 0 against H_1: beta_cigs < 0 at the 1% level. 
# Find the t-statistic for cigs. (round to the hundredths)
library(wooldridge)
data("bwght")
reg <- lm(bwght ~ cigs + faminc, data = bwght)
t_stat2 <- round(summary(reg)$coefficients["cigs", "t value"], 2)
t_stat2

# A regression of the number of hours a student studies per week on the student's 
# financial-aid share, measured in percent (so 40 means 40%), has slope 0.247. 
# If the aid share were measured as a decimal instead (so 0.40 means 40%), 
# what would the slope be? (round to the hundredths)
slope_percent <- 0.247
slope_decimal <- round(slope_percent * 100, 2)
slope_decimal

# Data: hprice1 (price = house price in thousands of dollars; sqrft = size of the house 
# in square feet; bdrms = number of bedrooms). 
# Run reg <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1). 
# What is the predicted price of a 2,000-square-foot house with 3 bedrooms 
# (in thousands of dollars)? (round to the hundredths)
library(wooldridge)
data("hprice1")
reg <- lm(log(price) ~ log(sqrft) + bdrms, data = hprice1)
new_house <- data.frame(sqrft = 2000, bdrms = 3)
log_pred <- predict(reg, newdata = new_house)
pred_price <- round(exp(log_pred), 2)
pred_price
