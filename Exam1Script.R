# Econometrics combines data, economics, and statistics
# Ice-cream sales and drownings both rise in the summer, so they are positively correlated. What is the best explanation?
  #   A confounding variable (summer) drives both

# In economics, "ceteris paribus" means: Holding all other factors fixed
# FALSE: A strong correlation between education and wages proves that more education causes higher wages

# What type of data is this: the U.S. unemployment rate recorded each year from 2000 to 2024
  #   Time series
# What type of data is this: wages and years of education for 526 workers, all measured in 1976?
  #   Cross-section
# What type of data is this: GDP for all 50 states, recorded every year from 2015 to 2020?
  #   Panel

# The whole group we care about, but usually cannot fully observe, is called the POPULATION
# The part of that group we actually collect data on is called the SAMPLE

# TRUE: we use the sample we have in order to learn about the larger population we care about.
# TRUE: In R, mean(x) gives the same result as sum(x)/length(x)
# In the R expression wage1$wage, what does the $ do?
  #   Pulls the variable "wage" out of the dataset "wage1"

# summary() gives a quick snapshot (minimum, mean, maximum) of every variable in a dataset
# Course's rule for using AI: "AI writes, you verify."
# TRUE: Correlation means two variables move together, causation means changing one changes the other
# In R, which set of commands descibes a variable's center, spread, and shape (in that order)?
  #   mean(), sd(), hist()


------------PS2
# For y = beta_0 + beta_1 * X_1 + beta_2 * X_2, the coefficient beta_1 is:
cat("Answer: The effect of X_1 on y, holding X_2 fixed")

cat("==============================================================================\n")
cat("                       PROBLEM SET 2: QUESTIONS & ANSWERS                     \n")
cat("==============================================================================\n\n")

# For y = beta_0 + beta_1 * X_1 + beta_2 * X_2, the coefficient beta_1 is:
cat("Answer: The effect of X_1 on y, holding X_2 fixed")

# An interest rate rises from 4% to 6%. Which statement is correct?
cat("Answer: A 2 percentage-point increase, which is also a 50% increase")

# When we compute the SAMPLE variance, we divide the sum of squared deviations from the mean by:
cat("Answer: n - 1")

# If two variables tend to rise and fall together, their sample covariance S_xy is:
cat("Answer: Positive")

# Ice-cream sales and drownings both rise in the summer, so they are positively correlated. What is the best explanation?
cat("Answer: A confounding variable (summer) drives both")

# True or False: in y = beta_0 + beta_1 * x, the intercept beta_0 affects the change Delta y when x changes.
cat("Answer: False")

# Percent change is % Delta x = 100 * (x_new - x_old) / x_old. Find it when x goes from 50 to 99.
x_old16 <- 50
x_new16 <- 99
ans16 <- (100 * (x_new16 - x_old16) / x_old16)
ans16

# A rate goes from 1% to 10%. What is the change in percentage POINTS?
p_old17 = 1
p_new17 = 10
ans17 <- p_new17 - p_old17
ans17

# Sample variance is S_x^2 = (1 / (n - 1)) * sum (x_i - x_bar)^2. Compute it for x = (6, 1, 4).
q18 = c(6,1,4)
var(q18)
# Sample sd is S_x = sqrt(S_x^2). Compute it for x = (4, 0, 3). standard deviation
q19 = c(4,0,3)
sd(q19)
# Sample covariance is S_xy = (1 / (n - 1)) * sum (x_i - x_bar) * (y_i - y_bar).
# Compute it for x = (5, 2, 0) and y = (6, 0, 3).
q20x = c(5,2,0)
q20y = c(6,0,3)
cov(q20x,q20y)

# The SAMPLE variance divides the sum of squared deviations by n - 1.

# var(x) computes the sample variance.

# cor(x, y) computes the correlation between numeric vectors x and y (sample correlation).

# The correlation coefficient always lies between -1 and 1.

# For a discrete random varibale X, the expected value E[X] equals the sum of each value times its probability: sum(x * P(X = x)).

# For a discrete random variable X, E[X^2] equals: the sum of each SQUARED value times its probability: sum((x^2) * P(X = x)).

# True: sd(x) gives the SAMPLE standard deviation (dividing by n - 1).

q8x = c(7,4,8)
mean(q8x)

q9x = c(5,8,0)
var(q9x)

q10x = c(2,4,5)
sd(q10x)

q11x = c(0,1,2)
q11y = c(1,0,0)
cov(q11x,q11y)


# Sample mean
x8 <- c(7, 4, 8)
mean(x8)

# Sample variance
x9 <- c(5, 8, 0)
var(x9)

# Sample standard deviation
x10 <- c(2, 4, 5)
sd(x10)

# Sample covariance
x11 <- c(0, 1, 2)
y11 <- c(1, 0, 0)
cov(x11,y11)

# Find E[X] - Fair coin (1 on heads, 3 on tails)
x14 = c(1,3)
p14 = c(1/2,1/2)
sum(x14*p14)

# Find E[X^2] - Fair coin (8 on heads, 0 on tails)
x15 = c(8,0)
p15 = c(1/2,1/2)
sum(x15^2*p15)

# Find E[X] - Discrete RV (6, 5, 0 with equal prob)
x16 = c(6,5,0)
p16 = c(1/3,1/3,1/3)
sum(x16*p16)

# Find E[X^2] - Discrete RV (0, 3, 3 with equal prob)
x17 = c(0,3,3)
p17 = c(1/3,1/3,1/3)
sum(x17^2*p17)

# Find E[X] - Discrete RV (0 w/ prob 0.6, 5 w/ prob 0.4)
x18 = c(0,5)
p18 = c(6/10,(10-6)/10)
sum(x18*p18)

# Find E[X^2] - Discrete RV (10 w/ prob 0.1, 10 w/ prob 0.9)
x19 = c(10,10)
p19 = c(1/10,(10-1)/10)
sum(x19^2*p19)

# Find E[X] - Discrete RV (1, 9, 1 with prob 0.2, 0.5, 0.3)
x20 = c(1,9,1)
p20 = c(0.2,0.5,0.3)
sum(x20*p20)

# Find E[X^2] - Discrete RV (8, 5, 7 with prob 0.2, 0.5, 0.3)
x21 = c(8,5,7)
p21 = c(0.2,0.5,0.3)
sum(x21^2*p21)

# For any constant c, the expected value E[c] equals: c
q1_ans <- "c"

# If X and Y are independent, Cov(X, Y) = 0
q2_ans <- 0

# Cor(X, Y) always lies between -1 and 1
q3_ans <- "Between -1 and 1"

# THe variance of a random variable X equals: E[X^2] - (E[X])^2
q4_ans <- "E[X^2] - (E[X])^2"

# Compared with covariance, correlation is: unitless and always between -1 and 1
q5_ans <- "Unitless and always between -1 and 1"

# E[y|X=x] is best described as the average of Y among observations with X = x
q6_ans <- "The average of Y among observations with X = x"

# If X and Y are independent, then Var(X + Y) equals Var(X) + Var(Y)
q7_ans <- "Var(X) + Var(Y)"


# Given: E[X] = 3, E[Y] = 6, a = 3, b = 2
# Property: Find E[aX + bY]
E_X <- 3
E_Y <- 6
a <- 3
b <- 2
a * E_X + b * E_Y
q4_ans <- a * E_X + b * E_Y

# Given: E[X] = 1, E[X^2] = 64 Find Var(X)
# Formula: Var(X) = E[X^2] - (E[X])^2
E_X <- 1
E_X2 <- 64
E_X2-E_X

# Question 6
# Given: Var(X) = 5, a = 2, Find Var(aX+b)
# Property: Var(aX + b) = a^2 * Var(X) (constant b drops out)
Var_X <- 5
a <- 2
(a^2) * Var_X

# Given: E[XY] = 24, E[X] = 3, E[Y] = 1
# Formula: Cov(X, Y) = E[XY] - E[X] * E[Y]
E_XY <- 24
E_X <- 3
E_Y <- 1
E_XY - (E_X * E_Y)

# Given paired values: (3, 4), (3, 4), (3, 5)
X <- c(3, 3, 3)
Y <- c(4, 4, 5)
cov(X,Y)
# Since X is constant (variance is 0), Cov(X, Y) must be 0

# Given: Cov(X, Y) = -2, sd(X) = 3, sd(Y) = 5
# Formula: Cor(X, Y) = Cov(X, Y) / (sd(X) * sd(Y))
Cov_XY <- -2
sd_X <- 3
sd_Y <- 5
Cov_XY / (sd_X * sd_Y)

# Given: Var(X) = 2, Var(Y) = 4, Cov(X, Y) = -1, a = 1, b = 3
# Formula: Var(aX + bY) = a^2 * Var(X) + b^2 * Var(Y) + 2 * a * b * Cov(X, Y)
Var_X <- 2
Var_Y <- 4
Cov_XY <- -1
a <- 1
b <- 3
(a^2 * Var_X) + (b^2 * Var_Y) + (2 * a * b * Cov_XY)

# Given: Y takes values c(5, 8, 4) with probabilities c(0.2, 0.3, 0.5)
# Formula: E[Y | X = x] = sum(y_i * P(Y = y_i))
y_vals <- c(5, 8, 4)
probs  <- c(0.2, 0.3, 0.5)
sum(y_vals * probs)


# Question 8: Linear expectation E[aX + b] = a*E[X] + b
# E[X] = 3, a = 3, b = 5
E_X <- 3
a <- 3
b <- 5
q8_ans <- a * E_X + b

# Question 9: Expectation of sum E[aX + bY] = a*E[X] + b*E[Y]
# E[X] = 7, E[Y] = 1, a = 1, b = 3
E_X <- 7
E_Y <- 1
a <- 1
b <- 3
q9_ans <- a * E_X + b * E_Y

# Question 10: E[aX + bY - c] = a*E[X] + b*E[Y] - c
# E[X] = 8, E[Y] = 7, a = 4, b = 1, c = 3
E_X <- 8
E_Y <- 7
a <- 4
b <- 1
c <- 3
q10_ans <- a * E_X + b * E_Y - c

# Question 11: Var(X) = E[X^2] - (E[X])^2
# E[X] = 1, E[X^2] = 57
E_X <- 1
E_X2 <- 57
q11_ans <- E_X2 - (E_X^2)

# Question 12: Var(aX + b) = a^2 * Var(X)
# Var(X) = 4, a = 2
Var_X <- 4
a <- 2
q12_ans <- (a^2) * Var_X

# Question 13: Standard Deviation sd(X) = sqrt(Var(X))
# Var(X) = 3
Var_X <- 3
q13_ans <- sqrt(Var_X)

# Question 14: Independent Var(X + Y) = Var(X) + Var(Y)
# Var(X) = 7, Var(Y) = 4
Var_X <- 7
Var_Y <- 4
q14_ans <- Var_X + Var_Y

# Question 15: Cov(X, Y) = E[XY] - E[X]*E[Y]
# E[XY] = 27, E[X] = 5, E[Y] = 6
E_XY <- 27
E_X <- 5
E_Y <- 6
q15_ans <- E_XY - (E_X * E_Y)

# Question 16: Discrete Covariance from paired observations
# Paired values: (2,2), (3,1), (1,3), each prob = 1/3
X <- c(2, 3, 1)
Y <- c(2, 1, 3)
E_XY <- mean(X * Y)
E_X <- mean(X)
E_Y <- mean(Y)
q16_ans <- E_XY - (E_X * E_Y)

# Question 17: Cov(a1*X + b1, a2*Y + b2) = a1*a2*Cov(X, Y)
# Cov(X, Y) = 0, a1 = 3, a2 = 5
Cov_XY <- 0
a1 <- 3
a2 <- 5
q17_ans <- a1 * a2 * Cov_XY

# Question 18: Cor(X, Y) = Cov(X, Y) / (sd(X) * sd(Y))
# Cov(X, Y) = 0, sd(X) = 5, sd(Y) = 2
Cov_XY <- 0
sd_X <- 5
sd_Y <- 2
q18_ans <- Cov_XY / (sd_X * sd_Y)

# Question 19: Var(aX + bY) = a^2*Var(X) + b^2*Var(Y) + 2*a*b*Cov(X, Y)
# Var(X) = 2, Var(Y) = 3, Cov(X, Y) = 3, a = 3, b = 2
Var_X <- 2
Var_Y <- 3
Cov_XY <- 3
a <- 3
b <- 2
q19_ans <- (a^2) * Var_X + (b^2) * Var_Y + 2 * a * b * Cov_XY

# Question 20: Subgroup average E[Y | X = x]
# Y values: 5, 6, 10, 8
Y_subgroup <- c(5, 6, 10, 8)
q20_ans <- mean(Y_subgroup)

# Question 21: Conditional Expectation discrete distribution
# Y values: 2, 9, 4 with probabilities 0.2, 0.3, 0.5
Y_vals <- c(2, 9, 4)
probs <- c(0.2, 0.3, 0.5)
q21_ans <- sum(Y_vals * probs)

# Question 22: Linear conditional expectation E[aY + b | X = x] = a*E[Y | X = x] + b
# E[Y | X = x] = 1, a = 3, b = 7
E_Y_cond <- 1
a <- 3
b <- 7
q22_ans <- a * E_Y_cond + b


# ==============================================================================

# In the simple linear regression model Y = beta0 + beta1 * X + U
# The DEPENDENT variable is Y

# In  Y = beta0 + beta1 * X + U, the error term U represents
cat("U represents factors other than X that affect Y")

# In  Y = beta0 + beta1 * X + U, the slope beta1 measures
cat("the ceteris paribus effect of X on Y (the change in Y per one-unit rise in X, holding other factors fixed")

# Assumption: The zero conditional mean assumption E[U|X] = 0 says
cat("The average of U does not depend on X")

# Under the SLR model with E[U|X]=0, the conditional mean E[Y|X=x] equals
cat("beta0 + beta1 * x")

# The OLS slope estimate beta1 can be written as:
cat("cov(x, y) / var(x)")

# We must ESTIMATE beta0,beta1 (rather than compute them exactly) because:
cat("They are population quantities; we observe only a sample")

# In Y = (beta0+beta1*X)+U, the term beta0beta1*X is:
cat("The part of Y EXPLAINED by X (its conditional mean)")

# Find the OLS slope Sample: x = (1, 1, 5), y = (10, 3, 9)
x11 <- c(1, 1, 5)
y11 <- c(10, 3, 9)
lm(y11~x11)
cov(x11, y11) / var(x11)

# Find the OLS intercept Sample: x = (2, 3, 1), y = (4, 3, 3)
x12 <- c(2, 3, 1)
y12 <- c(4, 3, 3)
lm(y12~x12)
Slope = cov(x12, y12) / var(x12)
Slope
Intercept = mean(y12)-Slope*mean(y12)
Intercept

# Fitted line: beta0 = 2, beta1 = 5/10, predict at x = 4
beta0_q13 <- 2
beta1_q13 <- 5 / 10
x_val_q13 <- 4
beta0_q13 + beta1_q13 * x_val_q13

# ------------------------------------------------------------------------------
# Sample: x = (2, 2, 4), y = (9, 2, 5), predict y_hat at x = 8
x14 <- c(2, 2, 4)
y14 <- c(9, 2, 5)
fit14 = lm(y14 ~ x14)
predict(fit14, newdata = data.frame(x14 = 8)) # at x=8, take 2nd line number

# Equation: E[colGPA | hsGPA] = (11/10) + (3/10) * hsGPA, at hsGPA = 21/10
hsGPA <- 21 / 10
(11 / 10) + (3 / 10) * hsGPA

# Joint distribution: x = c(2, 0, 0), y = c(2, 0, 6), probs = c(0.2, 0.3, 0.5)
# Find E[X]
x17 <- c(2, 0, 0)
p17 <- c(0.2, 0.3, 0.5)
sum(x17 * p17)

# Joint distribution: x = c(0, 2, 4), y = c(7, 3, 8), probs = c(0.2, 0.3, 0.5)
# Find Cov(X,Y)E[XY]-E[X]E[Y].
x18 <- c(0, 2, 4)
y18 <- c(7, 3, 8)
p18 <- c(0.2, 0.3, 0.5)

E_X18  <- sum(x18 * p18)
E_X18
E_Y18  <- sum(y18 * p18)
E_Y18
E_XY18 <- sum((x18 * y18) * p18)
E_XY18
E_XY18 - (E_X18 * E_Y18)

# Joint distribution: x = c(3, 5, 5), y = c(0, 4, 4), probs = c(0.2, 0.3, 0.5)
x19 <- c(3, 5, 5)
y19 <- c(0, 4, 4)
p19 <- c(0.2, 0.3, 0.5)

E_X19  <- sum(x19 * p19)
E_X19
E_Y19  <- sum(y19 * p19)
E_Y19
E_XY19 <- sum((x19 * y19) * p19)
E_XY19
E_X219 <- sum((x19^2) * p19)
E_X219
Cov_XY19 <- E_XY19 - (E_X19 * E_Y19)
Cov_XY19
Var_X19  <- E_X219 - (E_X19^2)
Var_X19
Cov_XY19 / Var_X19

# Sample covariance for x = c(0, 3, 6), y = c(6, 0, 3)
x20 <- c(0, 3, 6)
y20 <- c(6, 0, 3)
cov(x20, y20)

x = c(1,4,5,8)
y = c(10,2,6,3)
cor(x,y)


# PSNEW
# The sample covariance between annual income (in dollars) and years of education 
# is 7,800, and their sample correlation is 0.24. If income is measured in 
# thousands of dollars instead, what is the sample correlation between income 
# and education?
r_original <- 0.24
cov_original <- 7800
c <- 1 / 1000
cov_new <- c * cov_original
cov_new
r_new <- r_original
r_new

# A vector of 16 numbers has first element 25, and each of the remaining elements equals 9. 
# Find the sample mean.
x <- c(25, rep(9, 15))
mean(x)

# Suppose y=8+1.2X1-0.5X2. If X1 rises by 2 and X2 falls by 2, by how much does y change?
delta_X1 <- 2
delta_X2 <- -2
delta_y <- 1.2 * delta_X1 - 0.5 * delta_X2
delta_y

# The unemployment rate rises from 6% to 7.2%. By what percent does the unemployment rate change?
initial_rate <- 6
final_rate <- 7.2
pct_change <- ((final_rate - initial_rate) / initial_rate) * 100
pct_change

# Find the sample standard deviation of x = (3,7,7,11).
x <- c(3, 7, 7, 11)
sd(x)

# A sample has sample covariance Sxy=-14, sample standard deviation Sx=5, and sample variance S2y=49. 
# Find the sample correlation between x and y.
S_xy <- -14
S_x <- 5
S2_y <- 49
S_y <- sqrt(S2_y)
r <- S_xy / (S_x * S_y)
r

# E[X]=4 and E[X^2]=32. Find the standard deviation of 2X-5.
E_X <- 4
E_X2 <- 32
Var_X <- E_X2 - (E_X)^2
a <- 2
sd_2X_minus_5 <- abs(a) * sqrt(Var_X)
sd_2X_minus_5

# Given X=x, Y takes the values 11, 22, and 41 with probabilities 0.5, 0.3, and 0.2. 
# Find E[4Y+9|X=x].
y <- c(11, 22, 41)
p <- c(0.5, 0.3, 0.2)
E_Y_given_X <- sum(y * p)
E_4Y_plus_9 <- 4 * E_Y_given_X + 9
E_4Y_plus_9

# Var(X)=6, Var(Y)=11, and Cov(X,Y)=-3.1. Find Var(2X-2Y).
var_X <- 6
var_Y <- 11
cov_XY <- -3.1
var_2X_minus_2Y <- (2^2 * var_X) + ((-2)^2 * var_Y) + (2 * 2 * -2 * cov_XY)
var_2X_minus_2Y

# X and Y have the joint distribution below. Find Cor(X,Y).
# x = 1,4,5,8.....y = 10,2,6,3.....P(X=x,Y=y) = 0.1,0.4,0.3,0.2
x <- c(1, 4, 5, 8)
y <- c(10, 2, 6, 3)
p <- c(0.1, 0.4, 0.3, 0.2)
E_X <- sum(x * p)
E_Y <- sum(y * p)
E_XY <- sum(x * y * p)
Var_X <- sum((x - E_X)^2 * p)
Var_Y <- sum((y - E_Y)^2 * p)
Cov_XY <- E_XY - (E_X * E_Y)
Cor_XY <- Cov_XY / sqrt(Var_X * Var_Y)
Cor_XY

# For the sample...find the OLS slope beta1 from regressing y on x.
x = c(1,4,6,8,9)
y = c(4,11,2,7,12)
lm(y~x)

# For the sample x=(1,2,7,8,9), y=(5,9,11,5,10), regress y on x by OLS. 
# Find the OLS residual of the third observation, (x3,y3)=(7,11).
x <- c(1, 2, 7, 8, 9)
y <- c(5, 9, 11, 5, 10)
model <- lm(y ~ x)
residuals(model)[3]

# A sample of n=51 observations has sample variance S2y=28. Find the total sum of squares, SST.
n <- 51
S2_y <- 28
SST <- (n - 1) * S2_y
SST

# A regression has explained sum of squares SSE=112 and R^2=0.88. 
# Find the sum of squared residuals, SSR.
SSE <- 112
R_squared <- 0.88
SST <- SSE / R_squared
SSR <- SST - SSE
SSR

# An OLS regression line has intercept betahat0=2 and slope betahat1=3.4. The sample mean of x is 7. 
# What is the sample mean of y?
beta_hat_0 <- 2
beta_hat_1 <- 3.4
x_bar <- 7
y_bar <- beta_hat_0 + (beta_hat_1 * x_bar)
y_bar

# An OLS regression on five observations has residuals -1.8,1.1,2.2,-1.4, and one more. 
# What is the fifth residual?
resids <- c(-1.8, 1.1, 2.2, -1.4)
fifth_resid <- -sum(resids)
fifth_resid

# For the sample x=(1,4,9,10,12,14), y=(15,14,7,7,13,13), regress y on x by OLS. 
# Compute sum of xi uhati where uhati are the OLS residuals.
x <- c(1, 4, 9, 10, 12, 14)
y <- c(15, 14, 7, 7, 13, 13)
model <- lm(y ~ x)
u_hat <- residuals(model)
sum(x * u_hat)

# THIS IS 0
# For the sample x=(1,4,9,10,12,14), y=(15,14,7,7,13,13), regress y on x by OLS. 
# Compute sum from i=1 to 6 of xi uhati where uhati are the OLS residuals.
x <- c(1, 4, 9, 10, 12, 14)
y <- c(15, 14, 7, 7, 13, 13)
model <- lm(y ~ x)
u_hat <- residuals(model)
round(sum(x * u_hat), 10)

# Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the 
# firm's return on equity, in percent). How many CEOs are in the sample?

library(wooldridge)
data("ceosal1")
View(ceosal1)
209

# What is the sample mean of return on equity?
mean(ceosal1$roe)

# What is the sample correlation between salary and return on equity?
cor(ceosal1$salary,ceosal1$roe)
# Data: (wage1=average hourly earnings in dollars; tenure=years with the current employer). 
# Run the OLS regression of wage on tenure. What is the OLS slope?
model <- lm(wage ~ tenure, data = wage1)
coef(model)["tenure"]

# Using the OLS regression of wage on tenure, what is the predicted hourly wage
# of a worker with 2 years of tenure?
model <- lm(wage ~ tenure, data = wage1)
predict(model, newdata = data.frame(tenure = 2))

# Using the OLS regression of wage on tenure, by how much does the predicted
# hourly wage change when tenure rises from 0 to 9 years?
model <- lm(wage ~ tenure, data = wage1)
coef(model)["tenure"] * (9 - 0)

# What percentage of the sample variation in wage is explained by tenure?
model <- lm(wage ~ tenure, data = wage1)
summary(model)$r.squared * 100

# Using the OLS regression of wage on tenure, 
# what is the OLS residual of the worker in the second row of wage1?
model <- lm(wage ~ tenure, data = wage1)
residuals(model)[2]

# Data: bwght (bwght = a baby's birth weight in ounces; female = the family's income in thousands of dollars)
# Run the OLS regression of birth weight on family income. What is the explained sum of squares, SSE
model <- lm(bwght ~ faminc, data = bwght)
sum((fitted(model) - mean(bwght$bwght))^2)

# Using the OLS regression of birth weight on family income, by how many ounces
# does predicted birth weight change when family income rises by 6, that is, by $6,000?
model <- lm(bwght ~ faminc, data = bwght)
coef(model)["faminc"] * 6

