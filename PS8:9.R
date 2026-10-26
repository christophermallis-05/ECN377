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
