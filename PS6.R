# Question 1
cat("Q1: In OLS, the fitted value y_hat is:\n")
cat("Answer: The model's PREDICTION of Y at X_i\n\n")

# Question 2
cat("Q2: The OLS residual u_hat equals:\n")
cat("Answer: Y_i - y_hat (actual minus fitted)\n\n")

# Question 3
cat("Q3: A POSITIVE residual (u_hat > 0) means OLS:\n")
cat("Answer: Under-predicted Y_i (actual > fitted)\n\n")

# Question 4
cat("Q4: R^2 is:\n")
cat("Answer: The fraction of the sample variation in Y explained by X\n\n")

# Question 5
cat("Q5: The total sum of squares decomposes as SST =\n")
cat("Answer: SSE + SSR\n\n")

# Question 6
cat("Q6: The sum of the OLS residuals sum(u_hat) is always:\n")
cat("Answer: 0\n\n")

# Question 7
cat("Q7: An R^2 close to 0 means:\n")
cat("Answer: The OLS line explains little of the variation in Y --- common, and not necessarily a bad model\n\n")

# Question 8
cat("Q8: The point (X_bar, Y_bar):\n")
cat("Answer: Always lies on the OLS regression line\n")

#   Find OLS slope (whatever value x is after final command)
x = c(6,6,3)
y = c(4,5,9)
model = lm(y~x)
coef(model)

#   Find OLS intercept
q10x = c(7,4,6)
q10y = c(6,4,0)
model10 = lm(q10y~q10x)
coef(model10)["(Intercept)"]

#   Fit the OLS line
q11x = c(1,8,6)
q11y = c(12,4,0)
model11 = lm(q11y~q11x)
predict(model11,newdata=data.frame(q11x=4))

#   Find fitted value at x = 6
#   Plug in the x number (6) to the equation. Print the equation
q12y = 7 +(8/10)*6
q12y

#   Find the residual (plug in the numbers into the equation)
q13yhat = 2+(6/10)*5
q13yhat
14-5

#   Find ssr given three pairs and yhat
#   # Question 15:
# For the fitted line y_hat = 3 + (7/10)*x and the three points
# (2, 1), (10, 9), (7, 4), find SSR = sum((Y_i - Y_hat_i)^2).
q15x = c(2,10,7)
q15y = c(1,9,4)
q15yhat = 3+(7/10)*q15x
q15yhat
residuals = q15y-q15yhat
ssr = sum(residuals^2)
ssr

#   Find sse (sst-ssr)
#   Additonal formulas
#   sst = sse+ssr
#   ssr = sst-sse
q16sst = 209
q16ssr = 171
q16sse = q16sst-q16ssr
q16sse

#   Find R^2 (1-ssr/sst)
#   R^2 = sse/sst OR 1-ssr/sst
q17sst = 227
q17ssr = 26
q17R2 = 1-q17ssr/q17sst
q17R2

#   Find R^2 (sse/sst)
q18sse = 44
q18sst = 314
q18R2 = q18sse/q18sst
q18R2

#   Find R^2 (1-ssr/sst), then subtract 1 from R^2
q19sst = 239
q19ssr = 168
q19R2 = 1-q19ssr/q19sst
q19R2
1-q19R2

q20sst = 326
q20R2 = 78/100
q20ssr = q20sst*(1-q20R2)
q20ssr
