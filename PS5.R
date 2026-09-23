# Q1: In the simple linear regression model Y = beta0 + beta1*X + U, 
#     the DEPENDENT variable is:
# Answer: Y

# Q2: In Y = beta0 + beta1*X + U, the error term U represents:
# Answer: Factors OTHER than X that affect Y

# Q3: In Y = beta0 + beta1*X + U, the slope beta1 measures:
# Answer: The ceteris paribus effect of X on Y 
#         (the change in Y per one-unit rise in X, holding other factors fixed)

# Q4: The zero conditional mean assumption E[U|X] = 0 says:
# Answer: The average of U does not depend on X

# Q5: Under the SLR model with E[U|X] = 0, the conditional mean E[Y|X=x] equals:
# Answer: beta0 + beta1*x

# Q6: The OLS slope estimate beta_hat_1 can be written as:
# Answer: cov(x, y) / var(x)

# Q7: We must ESTIMATE beta0, beta1 (rather than compute them exactly) because:
# Answer: They are population quantities; we observe only a sample

# Q8: In Y = (beta0 + beta1*X) + U, the term beta0 + beta1*X is:
# Answer: The part of Y EXPLAINED by X (its conditional mean)


# ------------------------------------------------------------------------------
# Question 9
# Question: "The OLS slope is beta_hat_1 = cov(x,y) / var(x). Given cov(x,y) = 12 
#            and var(x) = 2, find beta_hat_1. (round to the hundredths)"
# ------------------------------------------------------------------------------
cov_xy_q9 <- 12
var_x_q9  <- 2

beta_1_q9 <- cov_xy_q9 / var_x_q9
cat("Q9 Answer:", round(beta_1_q9, 2), "\n") # Output: 6


# ------------------------------------------------------------------------------
# Question 10
# Question: "The OLS intercept is beta_hat_0 = y_bar - beta_hat_1 * x_bar. 
#            Given y_bar = 3, x_bar = 10, and slope beta_hat_1 = 5 / 10, 
#            find beta_hat_0. (round to the hundredths)"
# ------------------------------------------------------------------------------
y_bar_q10  <- 3
x_bar_q10  <- 10
beta_1_q10 <- 5 / 10

beta_0_q10 <- y_bar_q10 - (beta_1_q10 * x_bar_q10)
cat("Q10 Answer:", round(beta_0_q10, 2), "\n") # Output: -2


# ------------------------------------------------------------------------------
# Question 11
# Question: "For the sample x = (1, 1, 5), y = (10, 3, 9), find the OLS slope 
#            beta_hat_1 = sum((x_i - x_bar)(y_i - y_bar)) / sum((x_i - x_bar)^2). 
#            (round to the hundredths)"
# ------------------------------------------------------------------------------
x_q11 <- c(1, 1, 5)
y_q11 <- c(10, 3, 9)

beta_1_q11 <- sum((x_q11 - mean(x_q11)) * (y_q11 - mean(y_q11))) / sum((x_q11 - mean(x_q11))^2)
cat("Q11 Answer:", round(beta_1_q11, 2), "\n") # Output: 0.63


# ------------------------------------------------------------------------------
# Question 12
# Question: "For that same sample x = (2, 3, 1), y = (4, 3, 3), find the OLS 
#            intercept beta_hat_0 = y_bar - beta_hat_1 * x_bar. 
#            (round to the hundredths)"
# ------------------------------------------------------------------------------
x_q12 <- c(2, 3, 1)
y_q12 <- c(4, 3, 3)

beta_1_q12 <- sum((x_q12 - mean(x_q12)) * (y_q12 - mean(y_q12))) / sum((x_q12 - mean(x_q12))^2)
beta_0_q12 <- mean(y_q12) - (beta_1_q12 * mean(x_q12))

cat("Q12 Answer:", round(beta_0_q12, 2), "\n") # Output: 3.33


# ------------------------------------------------------------------------------
# Question 13
# Question: "A fitted regression line is y_hat = beta_hat_0 + beta_hat_1 * x. 
#            With beta_hat_0 = 2 and beta_hat_1 = 5 / 10, find the predicted 
#            y_hat at x = 4. (round to the hundredths)"
# ------------------------------------------------------------------------------
beta_0_q13 <- 2
beta_1_q13 <- 5 / 10
x_val_q13  <- 4

y_hat_q13 <- beta_0_q13 + (beta_1_q13 * x_val_q13)
cat("Q13 Answer:", round(y_hat_q13, 2), "\n") # Output: 4


# ------------------------------------------------------------------------------
# Question 14
# Question: "For the sample x = (2, 2, 4), y = (9, 2, 5), fit the OLS line and 
#            predict y_hat at x = 8. (round to the hundredths)"
# ------------------------------------------------------------------------------
x_q14 <- c(2, 2, 4)
y_q14 <- c(9, 2, 5)

model_q14 <- lm(y_q14 ~ x_q14)
pred_q14 <- predict(model_q14, newdata = data.frame(x_q14 = 8))

cat("Q14 Answer:", round(pred_q14, 2), "\n") # Output: 4


# ------------------------------------------------------------------------------
# Question 15
# Question: "The notes' example is E[colGPA | hsGPA] = 11/10 + 3/10 * hsGPA. 
#            Find the expected colGPA at hsGPA = 21/10. (round to the hundredths)"
# ------------------------------------------------------------------------------
hsGPA_q15 <- 21 / 10

colGPA_hat_q15 <- (11 / 10) + (3 / 10) * hsGPA_q15
cat("Q15 Answer:", round(colGPA_hat_q15, 2), "\n") # Output: 1.73


# ------------------------------------------------------------------------------
# Question 16
# Question: "In wage = beta0 + beta1 * educ + U the estimated slope is 
#            beta_hat_1 = 6 / 10. Ceteris paribus, if educ rises by 8 year(s), 
#            by how much does predicted wage change? (round to the hundredths)"
# ------------------------------------------------------------------------------
beta_1_q16 <- 6 / 10
delta_educ <- 8

delta_wage <- beta_1_q16 * delta_educ
cat("Q16 Answer:", round(delta_wage, 2), "\n") # Output: 4.8


# ------------------------------------------------------------------------------
# Question 17
# Question: "X and Y have the joint probability distribution below:
#            x = 2, y = 2, P = 0.2
#            x = 0, y = 0, P = 0.3
#            x = 0, y = 6, P = 0.5
#            Find E[X]. (round to the hundredths)"
# ------------------------------------------------------------------------------
x_q17 <- c(2, 0, 0)
p_q17 <- c(0.2, 0.3, 0.5)

E_X_q17 <- sum(x_q17 * p_q17)
cat("Q17 Answer:", round(E_X_q17, 2), "\n") # Output: 0.4


# ------------------------------------------------------------------------------
# Question 18
# Question: "X and Y have the joint probability distribution below:
#            x = 0, y = 7, P = 0.2
#            x = 2, y = 3, P = 0.3
#            x = 4, y = 8, P = 0.5
#            Find Cov(X, Y) = E[XY] - E[X]E[Y]. (round to the hundredths)"
# ------------------------------------------------------------------------------
x_q18 <- c(0, 2, 4)
y_q18 <- c(7, 3, 8)
p_q18 <- c(0.2, 0.3, 0.5)

E_X_q18  <- sum(x_q18 * p_q18)
E_Y_q18  <- sum(y_q18 * p_q18)
E_XY_q18 <- sum(x_q18 * y_q18 * p_q18)

Cov_XY_q18 <- E_XY_q18 - (E_X_q18 * E_Y_q18)
cat("Q18 Answer:", round(Cov_XY_q18, 2), "\n") # Output: 1.42


# ------------------------------------------------------------------------------
# Question 19
# Question: "X and Y have the joint probability distribution below:
#            x = 3, y = 0, P = 0.2
#            x = 5, y = 4, P = 0.3
#            x = 5, y = 4, P = 0.5
#            The population regression slope is beta1 = Cov(X,Y) / Var(X). 
#            Find beta1. (round to the hundredths)"
# ------------------------------------------------------------------------------
x_q19 <- c(3, 5, 5)
y_q19 <- c(0, 4, 4)
p_q19 <- c(0.2, 0.3, 0.5)

E_X_q19  <- sum(x_q19 * p_q19)
E_Y_q19  <- sum(y_q19 * p_q19)
E_XY_q19 <- sum(x_q19 * y_q19 * p_q19)
E_X2_q19 <- sum(x_q19^2 * p_q19)

Cov_XY_q19 <- E_XY_q19 - (E_X_q19 * E_Y_q19)
Var_X_q19  <- E_X2_q19 - (E_X_q19)^2

beta_1_q19 <- Cov_XY_q19 / Var_X_q19
cat("Q19 Answer:", round(beta_1_q19, 2), "\n") # Output: 2


# ------------------------------------------------------------------------------
# Question 20
# Question: "Review (sample). For x = (0, 3, 6), y = (6, 0, 3), find the 
#            SAMPLE covariance S_xy = 1 / (n - 1) * sum((x_i - x_bar)(y_i - y_bar)). 
#            (round to the hundredths)"
# ------------------------------------------------------------------------------
x_q20 <- c(0, 3, 6)
y_q20 <- c(6, 0, 3)

sample_cov_q20 <- cov(x_q20, y_q20)
cat("Q20 Answer:", round(sample_cov_q20, 2), "\n") # Output: -4.5