# A regression gives log(y)_hat = 1.5 + 0.045*x1 - 0.017*x2. If x1 rises by 4 
# and x2 rises by 4, by about what percent does predicted y change? 
# Hint: log-level MLR: % Delta y approx 100 * (beta_hat_1 * Delta x1 + beta_hat_2 * Delta x2). Here beta_hat_2 is negative.
beta_1 <- 0.045
beta_2 <- -0.017
delta_x1 <- 4
delta_x2 <- 4
q1_ans <- 100 * (beta_1 * delta_x1 + beta_2 * delta_x2)
q1_ans

# Data: wage1 (wage = average hourly earnings in dollars; educ = years of 
# education; exper = years of experience; tenure = years with the current 
# employer). Run reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1) . 
# By about what percent does predicted wage change when experience rises by 2 
# years and tenure rises by 1 year, holding education fixed? 
# Hint: % Delta wage_hat approx 100 * (beta_hat_exper * 2 + beta_hat_tenure * 1). 
# With b <- reg$coefficients : 100 * (b[3]*2 + b[4]*1).
library(wooldridge)
data(wage1)
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
q2_ans <- 100 * (b["exper"] * 2 + b["tenure"] * 1)
q2_ans

# Data: wage1 (wage = average hourly earnings in dollars; educ = years of 
# education; exper = years of experience; tenure = years with the current 
# employer). Using the same regression of log(wage) on educ, exper and tenure, 
# what is the predicted hourly wage, in dollars, of a worker with 12 years of 
# education, 10 years of experience and 3 years of tenure? (round to the hundredths)
# Hint: predict log(wage) first, then undo the log with exp() : 
# exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3) .

library(wooldridge)
data(wage1)
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b <- reg$coefficients
pred_log_wage <- b["(Intercept)"] + (b["educ"] * 12) + (b["exper"] * 10) + (b["tenure"] * 3)
q3_ans <- exp(pred_log_wage)
q3_ans

# After an OLS multiple regression of Y on X_1, ..., X_K (with an intercept) 
# and residuals U_hat_i = Y_i - Y_hat_i, which statements are always true? 
# Select all that apply.
# Hint: the notes list three properties, the same as in simple regression. 
# Careful: the residuals are uncorrelated with the fitted values, not with Y itself.
# [x] The sample average of the residuals is 0.
# [ ] The sample covariance between Y and the residuals is 0.
# [x] The sample covariance between each X_j and the residuals is 0.
# [x] The regression passes through the point of sample means: 
#     Y_bar = beta_hat_0 + beta_hat_1 * X_bar_1 + ... + beta_hat_K * X_bar_K.
# [ ] Every residual is 0 when the sample is large enough.
# ==============================================================================

# A multiple regression gives y_hat = 8.8 + 1.6*x_1 - 0.4*x_2. The sample means of 
# the independent variables are x_bar_1 = 7.2 and x_bar_2 = 2.1. What is the sample mean y_bar?
# Hint: the OLS regression passes through the point of means: 
# y_bar = beta_hat_0 + beta_hat_1 * x_bar_1 + beta_hat_2 * x_bar_2. 
# Here beta_hat_2 is negative.
beta_0 <- 8.8
beta_1 <- 1.6
beta_2 <- -0.4
x_bar_1 <- 7.2
x_bar_2 <- 2.1
q6_ans <- beta_0 + (beta_1 * x_bar_1) + (beta_2 * x_bar_2)
q6_ans

# Which of these regressions violate MLR3 (no perfect collinearity), so that 
# OLS cannot estimate them? Select all that apply.
# [x] wage on income in dollars and income in thousands of dollars
# [ ] wage on educ and exper in a sample of 526 workers, where both educ and exper vary from worker to worker
# [x] voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB
# [ ] colGPA on ACT and hsGPA, which are positively (but not perfectly) correlated
# [x] a regression with 5 independent variables, estimated on 4 observations
# ==============================================================================
ALSO:
# a regression wtih 3 independent variables, estimated on 3 observations
# house price on size in square feet and size in square meters (computed from the square feet)
# profit on online sales, store sales and total sales, where total sales = online sales + store sales

# Which situation makes MLR4, E[U | X_1, ..., X_K] = 0, fail (so that the 
# X variables are endogenous)?
# Hint: MLR4 can fail because of omitted variables or the wrong functional form.
# [x] A variable that affects Y and is correlated with X_1 is left out of the regression.
# ==============================================================================
# Demonstration: How leaving out a relevant, correlated variable breaks MLR4
# and causes endogeneity (Cov(X, U) != 0).

# IN wage = beta0 + beta 1educ + U, ability is part of the error U. Ability raises wages and is positively correlated with education. Which assumption fails?
#   MLR4, so educ is endogenous

# Formula (3.23): tilde_beta_1 = hat_beta_1 + hat_beta_2 * delta_tilde_1. 
# The simple regression of Y on X_1 gives tilde_beta_1 = 1.15. The multiple 
# regression of Y on X_1 and X_2 gives hat_beta_2 = 0.93, and the slope from
# regressing X_2 on X_1 is delta_tilde_1 = -0.56. Find the multiple-regression slope hat_beta_1.
# Hint: solve (3.23) for hat_beta_1 : hat_beta_1 = tilde_beta_1 - hat_beta_2 * delta_tilde_1 . Watch the signs.

tilde_beta_1 <- 1.61
hat_beta_2   <- -1.24
delta_tilde_1 <- -0.56
hat_beta_1 <- tilde_beta_1 - (hat_beta_2 * delta_tilde_1)
hat_beta_1

# Data: bwght (bwght = a baby's birth weight in ounces; cigs = cigarettes the 
# mother smoked per day during pregnancy; faminc = the family's income in 
# thousands of dollars). Regress faminc (the variable left out of the simple 
# regression in problem 12) on cigs, lm(faminc ~ cigs, data = bwght), to get 
# delta_tilde_1. Compute beta_hat_2 * delta_tilde_1, the faminc coefficient from 
# the multiple regression times this slope. (round to the hundredths)
# Hint: by (3.23), beta_2 * delta_tilde_1 = tilde_beta_1 - hat_beta_1: 
# the simple-regression cigs slope minus the multiple-regression one. Use that to check your answer.

library(wooldridge)
data(bwght)
reg_mult <- lm(bwght ~ cigs + faminc, data = bwght)
beta_hat_2 <- coef(reg_mult)["faminc"]
reg_aux <- lm(faminc ~ cigs, data = bwght)
delta_tilde_1 <- coef(reg_aux)["cigs"]
q14_ans <- beta_hat_2 * delta_tilde_1
q14_ans

# An omitted variable X_2 lowers Y (beta_2 < 0), and X_2 is negatively 
# correlated with the included X_1. The simple-regression slope on X_1 is:
# Hint: Table 3.2: Bias(tilde_beta_1) = beta_2 * delta_tilde_1, and delta_tilde_1 
# has the sign of Cor(X_1, X_2).
# [ ] biased downward
# [ ] impossible to sign without the data
# [x] biased upward
# [ ] unbiased
# ==============================================================================

# A researcher regresses crop yield on fertilizer use across farms, leaving out 
# land quality. Better land raises yield, and farmers tend to use more fertilizer 
# on better land. The simple-regression fertilizer slope most likely:
# Hint: two legs: the sign of land quality's effect on yield (beta_2) and the 
# sign of its correlation with fertilizer (delta_tilde_1).
# [ ] is unbiased, because land quality is not in the regression
# [ ] understates the effect of fertilizer (downward bias)
# [ ] is unbiased, because fertilizer and land quality are different variables
# [x] overstates the effect of fertilizer (upward bias)
# ==============================================================================

# biased downward (on average below, that is more nagative than the true effect of class size)