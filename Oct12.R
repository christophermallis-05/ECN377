# A1.
EX = 2
EX2 = 20
varx = EX2-EX^2
varx
sqrt(3^2*varx)


#B2.
y = c(4,10,16)
p = c(0.25,0.5,0.25)
EY = sum(y*p)
EY
3*EY-2

#C1.
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

#D2.
sst  =(31-1)*9
ssr = 189
R2 = 1-ssr/sst
R2

#E2.
library(wooldridge)
data("ceosal1")
reg2 = lm(salary~roe,data=ceosal1)
ssr = sum(reg2$residuals^2)
sst = sum((ceosal1$salary)-mean(ceosal1$salary))^2)
sse = sst-ssr
r2 = summary(reg2)$r2
r2*100

#G2.
1.5*4-0.8*-3


library(wooldridge)
data("ceosal1")
reg = lm(log(salary)~log(sales),data=ceosal1)
b0 = reg$coefficients[1]
b0
b1 = reg$coefficients[2]
b1
b1*50
