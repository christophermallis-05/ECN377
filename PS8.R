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