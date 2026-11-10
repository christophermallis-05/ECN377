# In the formula se(beta_hat_j) = sigma_hat / sqrt(SST_j * (1 - R_j^2)), what is X_j?
# hint: j indexes the coefficient whose standard error you want
# Options:
# (A) The residuals from the regression.
# (B) The dependent variable Y.
# (C) Whichever independent variable has the largest coefficient.
# (D) The independent variable whose coefficient beta_hat_j we want the standard error of.
# answer_q1 <- The independent variable whose coefficient beta_hat_j we want the standard error of.

# EXAM In wage = beta_0 + beta_1*educ + beta_2*exper + beta_3*tenure + U, you want se(beta_hat_2), 
# the standard error of the exper coefficient. Which regression gives R_j^2?
# hint: R_j^2 measures how well other ind. vars explain Xj. Y is not involved
# Options:
# (A) The regression of exper on educ and tenure: lm(exper ~ educ + tenure)
# (B) The regression of wage on exper alone: lm(wage ~ exper)
# (C) The regression of wage on educ, exper and tenure (its own R^2)
# (D) The regression of exper on wage: lm(exper ~ wage)

# answer_q2 <- The regression of exper on educ and tenure: lm(exper ~ educ + tenure)

# The standard error of the regression is sigma_hat = sqrt(SSR / (n - K - 1)). 
# What does sigma_hat estimate?
# Options:
# (A) The standard deviation of Y
# (B) sigma, the standard deviation of the error U
# (C) The R^2 of the regression
# (D) The standard deviation of beta_hat_j
# answer_q3 <- sigma, the standard deviation of the error U

# EXAM A multiple regression with K = 3 independent variables, estimated on n = 30 observations, 
# has SSR = 1,588. Find the standard error of the regression, sigma_hat.
n_q4 <- 30
k_q4 <- 3
ssr_q4 <- 1588
df_q4 <- n_q4 - k_q4 - 1
sigma_hat_q4 <- sqrt(ssr_q4 / df_q4)
sigma_hat_q4

# In a sample of n = 149 observations, the independent variable X_j 
# has sample standard deviation S_{X_j} = 1.4. Find SST_j.
n_q5 <- 149
sd_xj_q5 <- 1.4
sst_j_q5 <- (n_q5 - 1) * (sd_xj_q5^2)
sst_j_q5

# EXAM A multiple regression has sigma_hat = 7.2. For the independent variable X_j, 
# SST_j = 572, and regressing X_j on the other independent variables gives R_j^2 = 0.85. 
# Find se(beta_hat_j).
sigma_hat_q6 <- 7.2
sst_j_q6 <- 572
r2_j_q6 <- 0.85
se_beta_j_q6 <- sigma_hat_q6 / sqrt(sst_j_q6 * (1 - r2_j_q6))
se_beta_j_q6

# Holding sigma_hat and SST_j fixed, what happens to se(beta_hat_j) as R_j^2 gets close to 1 
# (that is, as X_j becomes nearly a linear function of the other independent variables)?
# As R_j^2 approaches 1, (1 - R_j^2) approaches 0. 
# Dividing by a number approaching 0 causes the standard error to grow without bound.
# This phenomenon is known as multicollinearity.
# ANS It grows without bound (multicollinearity).

# Data: hprice1. (price = house price in thousands of dollars; sqrft = size of
# the house in square feet; bdrms = number of bedrooms; lotsize = size of the lot
# in square feet). Run reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1). 
# Find degrees of freedom df = n - K - 1.
library(wooldridge)
data(hprice1)
reg_q8 <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
n_q8 <- nrow(hprice1)
k_q8 <- 3  # sqrft, bdrms, lotsize
df_q8 <- n_q8 - k_q8 - 1
df_q8

# EXAM Using the same regression reg, compute the standard error of the regression, 
# sigma_hat = sqrt(SSR / df).
ssr_q9 <- sum(residuals(reg_q8)^2)
sigma_hat_q9 <- sqrt(ssr_q9 / df_q8)
sigma_hat_q9


# Data: hprice1. Compute SST_j for X_j = bdrms, the total variation of bdrms around its mean.
sst_j_q10 <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)
sst_j_q10

# Data: hprice1. Compute R_j^2 for X_j = bdrms: the R^2 from regressing bdrms on the other independent variables.
reg_aux_q11 <- lm(bdrms ~ sqrft + lotsize, data = hprice1)
r2_j_q11 <- summary(reg_aux_q11)$r.squared
r2_j_q11

# EXAM Compute se(beta_hat_j) for the bdrms coefficient using unrounded objects from problems 9-11.
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
n <- nrow(hprice1)
K <- 3
df <- n - K - 1
ssr <- sum(residuals(reg)^2)
sigma_hat <- sqrt(ssr / df)
model_data <- model.frame(reg)
sst_j <- sum((model_data$bdrms - mean(model_data$bdrms))^2)
reg_j <- lm(bdrms ~ sqrft + lotsize, data = model_data)
r2_j <- summary(reg_j)$r.squared
se_beta_bdrms <- sigma_hat / sqrt(sst_j * (1 - r2_j))
print(paste("Question 12 - Standard Error of bdrms (by hand):", round(se_beta_bdrms, 2)))

# Data: hprice1. In summary(reg)$coefficients, what is the standard error 
# of the bdrms coefficient? (round to the hundredths)
library(wooldridge)
data(hprice1)
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
bdrms_se <- summary(reg)$coefficients["bdrms", "Std. Error"]
print(round(bdrms_se, 2))
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
se_bdrms_q13 <- summary(reg)$coefficients[3, 2]
se_bdrms_q13

# Print summary(reg). Near the bottom is a line Residual standard error: ... on 84 degrees of freedom. 
# Which of your answers does it match?
# Options:
# (A) Problem 10 (SST_j)
# (B) Problem 12 (the standard error of the bdrms coefficient)
# (C) Problem 9 (sigma_hat), and its degrees of freedom match problem 8
# (D) None of them
# answer_q14 Problem 9 (sigma_hat), and its degrees of freedom match problem 8

# A warning about missing data. Some datasets have missing values (NA). lm() silently drops every row 
# with an NA in any variable of the regression. Suppose Y (or another independent variable) has missing 
# values but X_j does not, and you compute SST_j by hand from all the rows of X_j. What happens?
# Options:
# (A) R stops with an error, so you notice right away.
# (B) Nothing: missing values never change any statistic.
# (C) Only R^2 is affected, never the standard errors.
# (D) Your hand-computed standard error can differ from summary(reg), because SST_j uses rows the 
#     regression dropped. Compute every piece on the same rows the regression used.
# answer_q15 Your hand-computed standard error can differ from summary(reg), because SST_j uses rows the regression dropped. Compute every piece on the same rows the regression used.

#_________________PROBLEM SET 8___________________
# In test score b0+b1 class size +U, family income is part of U. Richer school
# districts tend to have smaller classes.
#   The zero conditional mean assumption fails, because the average of U changes with classs size.

# E[X] = 3 and E[X^2] = 25. Find sd of 5X-2
E_X <- 3
E_X2 <- 25
Var_X <- E_X2 - (E_X)^2
sd_X <- sqrt(Var_X)
ans_3 <- abs(5) * sd_X
ans_3

# Var(X) = 8, Vay(Y) = 9, and Cov(X,Y) = -5.8 Find Var(3X-3Y)
Var_X_q4 <- 8
Var_Y_q4 <- 9
Cov_XY_q4 <- -5.8
a <- 3
b <- -3
ans_4 <- (a^2) * Var_X_q4 + (b^2) * Var_Y_q4 + 2 * a * b * Cov_XY_q4
ans_4

# X and Y have joint distributino. Find Cor(X,Y)
x <- c(3, 4, 6, 7)
y <- c(7, 1, 9, 3)
p <- c(0.2, 0.3, 0.4, 0.1)

E_X_q5 <- sum(x * p)
E_Y_q5 <- sum(y * p)
E_XY <- sum(x * y * p)

Cov_XY_q5 <- E_XY - (E_X_q5 * E_Y_q5)
Var_X_q5 <- sum((x^2) * p) - (E_X_q5^2)
Var_Y_q5 <- sum((y^2) * p) - (E_Y_q5^2)

sd_X_q5 <- sqrt(Var_X_q5)
sd_Y_q5 <- sqrt(Var_Y_q5)

Cor_XY <- Cov_XY_q5 / (sd_X_q5 * sd_Y_q5)
Cor_XY

# For the sample...regress y on x by OLS. Find residual of the third observation (4,12)
x_q6 <- c(2, 3, 4, 7, 9)
y_q6 <- c(5, 11, 12, 3, 3)
reg_q6 <- lm(y_q6 ~ x_q6)
ans_6 <- resid(reg_q6)[3]
ans_6

# An OLS regression on sex observations...what is the sixth residual?
residuals_known <- c(2.1, 0.4, 1.1, 2.1, 0.3)
# OLS residuals sum to zero: sum(u_i) = 0
ans_7 <- -sum(residuals_known)
ans_7

# For the sample...regress y on x by OLS
x_q8 <- c(1, 4, 5, 6, 9, 10)
y_q8 <- c(9, 14, 15, 3, 6, 8)
reg_q8 <- lm(y_q8 ~ x_q8)
# Algebraic property: sum(x_i * u_hat_i) = 0 for OLS when intercept is included
ans_8 <- sum(x_q8 * resid(reg_q8))
ans_8

# A regression has explained sum of squares...Find sum of squared residuals SSR
SSE <- 199
R2 <- 0.64
SST <- SSE / R2
SSR <- SST - SSE
SSR

# A sample of n-47 observations has sample variance s2y=13. Find total sum of squares SST
n <- 47
s_y2 <- 13
SST_q10 <- s_y2 * (n - 1)
SST_q10

# 
library(wooldridge)
data("wage1")
reg_q11 <- lm(wage ~ tenure, data = wage1)
ans_11 <- resid(reg_q11)[12]
ans_11

# An OLS regression line has intercept b0-1 and slope b1=2.4. Sample mean of x is 4. What is the sample mean of y?
beta0 <- 1
beta1 <- 2.4
x_bar <- 4
y_bar <- beta0 + beta1 * x_bar
y_bar

#From class:
# A1. Find SD of 3X-7
EX = 2
EX2 = 20
varx = EX2-EX^2
varx
sqrt(3^2*varx)


#B2. Find E[3Y-2|X=x] and Var(Y|X=x)
y = c(4,10,16)
p = c(0.25,0.5,0.25)
EY = sum(y*p)
EY
3*EY-2

#C1. Regress y on x by OLS. Find residual of fourth observation (7,9)
x = c(2,3,5,7,8)
y = c(3,6,5,9,12)
cov(x,y)
varx=var(x)
b1 = covxy/varx
ybar = mean(y)
xbar = mean(x)
b0 = ybar-b1*xbar
u_hat = b0+b1*7
9-u-hat

#D2. Sample of n=31 has sampel variance 9. Find SST and R2 if SSR = 189
sst  =(31-1)*9
ssr = 189
R2 = 1-ssr/sst
R2

#E2. Regress salary on roe. Find SSE and percentage of variation in salary explained by roe
library(wooldridge)
data("ceosal1")
reg2 = lm(salary~roe,data=ceosal1)
ssr = sum(reg2$residuals^2)
sst = sum((ceosal1$salary)-mean(ceosal1$salary))^2)
sse = sst-ssr
r2 = summary(reg2)$r2
r2*100

#G2. suppose y=2+1.5x1-0.8x2. If x1 rises by 4 and x2 falls by 3, how much does y change
1.5*4-0.8*-3


library(wooldridge)
data("ceosal1")
reg = lm(log(salary)~log(sales),data=ceosal1)
b0 = reg$coefficients[1]
b0
b1 = reg$coefficients[2]
b1
b1*50

#_________________PROBLEM SET 9___________________
# PS9
-
  # A log-log regression of CEO salary on firm sales gives log(salary) = beta_0 + beta_1 * log(sales), 
  # with salary in thousands of dollars. Salary is then measured in dollars instead. What happens?
  #   beta_1 does not change; only beta_0 changes.
  # R demonstration:
  sales <- c(1000, 2000, 5000)
salary_thousands <- c(200, 400, 900)
salary_dollars <- salary_thousands * 1000

mod_thousands <- lm(log(salary_thousands) ~ log(sales))
mod_dollars   <- lm(log(salary_dollars) ~ log(sales))

coef(mod_thousands)['log(sales)']
coef(mod_dollars)['log(sales)'] # Notice the slope coefficients are identical!

# log(y)_hat = 3.2 + 0.48 * log(x). By about what percent does y change when x falls by 5%?
beta_1_q5 <- 0.48
pct_change_x <- -5  # falling by 5%
pct_change_y <- beta_1_q5 * pct_change_x
pct_change_y

# Run lm(log(salary) ~ roe, data = ceosal1). 
# By about what percent does predicted salary change when return on equity rises by 5 points?
library(wooldridge)
data(ceosal1)
mod_q6 <- lm(log(salary) ~ roe, data = ceosal1)
beta_roe <- coef(mod_q6)['roe']

delta_roe <- 5
pct_change_salary <- 100 * beta_roe * delta_roe
pct_change_salary

# In wage = beta_0 + beta_1 * educ + U, what contributes omitted-variable bias to the OLS slope on educ?
#   The worker's ability, which raises wages and tends to be higher for people with more education.

# Holding everything else fixed, which change makes Var(beta_1) smaller?
#   More spread-out values of X (a larger SST_X).

# Under SLR1-5, the error stanard deviation is sigma = 6, A sample has the five X values
# X = (3, 4, 7, 8, 9). Find sd(beta_1).
sigma_q14 <- 6
x_vals <- c(3, 4, 7, 8, 9)

sst_x_q14 <- sum((x_vals - mean(x_vals))^2)
sd_beta1_q14 <- sigma_q14 / sqrt(sst_x_q14)
sd_beta1_q14

# A simple regression on n = 40 observations has SSR = 205. 
# Find the estimate of error variance, sigma^2.
n_q15 <- 40
ssr_q15 <- 205
sigma_sq_q15 <- ssr_q15 / (n_q15 - 2)
sigma_sq_q15

# A simple regresion on n = 9 observations has SSR = 422, and SST_X = 22. 
# Find the standard error se(beta_1).
n_q16 <- 9
ssr_q16 <- 422
sst_x_q16 <- 22

sigma_hat_sq_q16 <- ssr_q16 / (n_q16 - 2)
sigma_hat_q16 <- sqrt(sigma_hat_sq_q16)
se_beta1_q16 <- sigma_hat_q16 / sqrt(sst_x_q16)
se_beta1_q16

# PS8
-
  # A log-level regression gives log(y)_hat = 1.8 + 0.04 * x. 
  # By about what percent does y change when x rises by 3?
  beta_1_q17 <- 0.04
delta_x_q17 <- 3
# Formula: % delta y approx 100 * beta_1 * delta x
pct_change_y_q17 <- 100 * beta_1_q17 * delta_x_q17
pct_change_y_q17

# A level-log regression gives y_hat = 55 + 52.8 * log(x). 
# By about how many units does y change when x rises by 7%?
beta_1_q19 <- 52.8
pct_change_x_q19 <- 7
# Formula: delta y approx (beta_1 / 100) * (% delta x)
delta_y_q19 <- (beta_1_q19 / 100) * pct_change_x_q19
delta_y_q19

# In a regression of y on x, the OLS slope is beta_1 = 0.03. 
# Every value of x is then divided by 100. What is the new OLS slope?
beta_1 <- 0.03
divisor <- 100
new_beta_1 <- beta_1 * divisor
new_beta_1


# PS9 remaining questions

# --- Question 2 ---
beta1_q2 <- 0.0069
delta_x_q2 <- 12 # 1 year = 12 months
pct_change_q2 <- 100 * beta1_q2 * delta_x_q2
pct_change_q2

# --- Question 3 ---
beta0_q3 <- 2.21
log_1000 <- log(1000) # 6.907755... or 6.91 as hinted
new_intercept <- beta0_q3 + 6.91
new_intercept

# --- Question 7 ---
library(wooldridge)
data("ceosal1")
mod_q7 <- lm(salary ~ log(sales), data = ceosal1)
beta1_q7 <- coef(mod_q7)["log(sales)"]
delta_y_q7 <- (beta1_q7 / 100) * 10
delta_y_q7

# --- Question 11 ---
sigma_sq_q11 <- 27
sst_x_q11 <- 77
var_beta1_q11 <- sigma_sq_q11 / sst_x_q11
var_beta1_q11

# --- Question 12 ---
sigma_q12 <- 18
n_q12 <- 7
s_x_sq_q12 <- 3
sst_x_q12 <- (n_q12 - 1) * s_x_sq_q12
sd_beta1_q12 <- sigma_q12 / sqrt(sst_x_q12)
sd_beta1_q12

# --- Question 18 ---
library(wooldridge)
data("wage1")
reg_q18 <- lm(wage ~ educ, data = wage1)
sigma_hat_q18 <- summary(reg_q18)$sigma
cat("Q18 Answer:", round(sigma_hat_q18, 2), "\n")

# --- Question 19 ---
sst_x_wage1 <- sum((wage1$educ - mean(wage1$educ))^2)
cat("Q19 Answer:", round(sst_x_wage1, 2), "\n")

#_________________PROBLEM SET 10___________________
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

#_________________PROBLEM SET 11___________________
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

# Which situation makes MLR4, E[U | X_1, ..., X_K] = 0, fail (so that the 
# X variables are endogenous)?
# Hint: MLR4 can fail because of omitted variables or the wrong functional form.
# [x] A variable that affects Y and is correlated with X_1 is left out of the regression.
# ==============================================================================
# Demonstration: How leaving out a relevant, correlated variable breaks MLR4
# and causes endogeneity (Cov(X, U) != 0).

# Formula (3.23): tilde_beta_1 = hat_beta_1 + hat_beta_2 * delta_tilde_1. 
# The simple regression of Y on X_1 gives tilde_beta_1 = 1.15. The multiple 
# regression of Y on X_1 and X_2 gives hat_beta_2 = 0.93, and the slope from
# regressing X_2 on X_1 is delta_tilde_1 = -0.56. Find the multiple-regression slope hat_beta_1.
# Hint: solve (3.23) for hat_beta_1 : hat_beta_1 = tilde_beta_1 - hat_beta_2 * delta_tilde_1 . Watch the signs.

tilde_beta_1 <- 1.15
hat_beta_2   <- 0.93
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

# please write out question X and solve it in an r script
# solve question X in an r script
# solve these questions