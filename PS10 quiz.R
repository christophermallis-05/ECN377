# A regression gives test score = 12 + 8 log(income). What does the 8 mean?
#   A 1% rise in income is associated with a rise of about 0.08 points in the predicted test score.

# ==============================================================================
# Problem Set 10 - Questions 2, 3, and 4
# ==============================================================================

# --- Question 2 ---
# A log-level regression gives log(y)^ = 2.4 + 0.02 * x. 
# By about what percent does y change when x falls by 3?
beta_1_q2 <- 0.02
delta_x_q2 <- -3
percent_change_y <- 100 * beta_1_q2 * delta_x_q2
percent_change_y

# Data: bwght (bwght = birth weight; faminc = family income).
# Run the log-log regression: lm(log(bwght) ~ log(faminc), data = bwght)
# By about what percent does predicted birth weight change when family income rises by 20%?
library(wooldridge)
data("bwght")
model_q3 <- lm(log(bwght) ~ log(faminc), data = bwght)
beta_1_q3 <- coef(model_q3)["log(faminc)"]
percent_change_bwght <- beta_1_q3 * 20
percent_change_bwght

# Suppose you could draw 1,000 different random samples from the same population 
# and compute beta_hat_1 in each one. If OLS is unbiased (SLR1–SLR4 hold), which statement is correct?
#   The average of the 1,000 estimates is close to beta_1.
# IF IT IS BIASED: it means that across many repeated random samples, 
# the average (expected value) of your slope estimates ($\hat{\beta}_1$) does not equal the true population parameter ($\beta_1$).

# Under SLR1–SLR5, Var(beta_hat_1) = sigma^2 / SST_X. 
# Holding everything else fixed, which changes make Var(beta_hat_1) LARGER?
# Selected Answers:
#   [X] A smaller sample (fewer observations with X spread the same way, so a smaller SST_X)
#   [X] X values bunched more tightly together (a smaller SST_X)
#   [X] A larger error variance sigma^2

# A simple regression on n = 9 observations has sigma_hat = 6.6, 
# and the sample standard deviation of X is S_X = 0.9. 
# Find the standard error se(beta_hat_1). (round to the hundredths)
n_q7 <- 9
sigma_hat_q7 <- 6.6
s_x_q7 <- 0.9

# SST_X = (n - 1) * S_X^2
sst_x_q7 <- (n_q7 - 1) * (s_x_q7^2)

# se(beta_hat_1) = sigma_hat / sqrt(SST_X)
se_beta_1_q7 <- sigma_hat_q7 / sqrt(sst_x_q7)
se_beta_1_q7

# Under SLR1–SLR5, a regression has sd(beta_hat_1) = 1.64. 
# If the sample were 25 times as large, with the X values spread out in the same way 
# (so SST_X is also 25 times as large), what would sd(beta_hat_1) be? (round to the hundredths)
# Hint: Multiplying SST_X by k divides the sd by sqrt(k), not by k.
sd_old_q9 <- 1.64
k_q9 <- 25
sd_new_q9 <- sd_old_q9 / sqrt(k_q9)
sd_new_q9

# A multiple regression gives y^ = 9 + 2.6 * x1 + 1.6 * x2. 
# Find the predicted value y^ at x1 = 8 and x2 = 16.
beta_0_q12 <- 9
beta_1_q12 <- 2.6
beta_2_q12 <- 1.6
x1_q12 <- 8
x2_q12 <- 16
y_hat_q12 <- beta_0_q12 + beta_1_q12 * x1_q12 + beta_2_q12 * x2_q12
y_hat_q12

# A multiple regression gives y^ = 2 + 2.2 * x1 - 0.6 * x2. 
# If x1 rises by 6 and x2 rises by 4, by how much does y^ change?
beta_1_q13 <- 2.2
beta_2_q13 <- -0.6
delta_x1_q13 <- 6
delta_x2_q13 <- 4

delta_y_hat_q13 <- (beta_1_q13 * delta_x1_q13) + (beta_2_q13 * delta_x2_q13)
delta_y_hat_q13

# Data: wage1 (wage = average hourly earnings, educ = education, exper = experience, tenure = tenure).
# Run reg <- lm(wage ~ educ + exper + tenure, data = wage1)
# Predicted hourly wage for a worker with 14 years educ, 10 years exper, and 5 years tenure:
library(wooldridge)
data("wage1")
reg_wage <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- coef(reg_wage)
wage_pred <- b[1] + b[2] * 14 + b[3] * 10 + b[4] * 5
wage_pred

# Using the same regression of wage on educ, exper, and tenure, 
# by how much does predicted wage change when tenure rises by 3 years 
# (holding education and experience fixed)?
# (Note: The change is simply the tenure coefficient multiplied by the change in tenure)
library(wooldridge)
data("wage1")
beta_tenure <- coef(reg_wage)["tenure"]
wage_change <- beta_tenure * 3
wage_change




