library(wooldridge)
library(ggplot2)
data(wage2)

## Simple regression model
slr = lm(wage ~ educ, data=wage2)
summary(slr)

## Scatter Plots
qplot(y=wage, x=educ, data=wage2)
qplot(y=wage, x=IQ, data=wage2)
qplot(y=educ, x=IQ, data=wage2)

## Multiple Linear regression
mlr = lm(wage ~ educ + IQ, data=wage2)
summary(mlr)

##########################
##########################

##Calculate OVB (Omitted Variable Bias)

# Extract Coefficients
beta1tilde = coefficients(slr)["educ"]
beta2hat = coefficients(mlr)["IQ"]
beta1hat = coefficients(mlr)["educ"]

#delta1 
delta1hat = (beta1tilde - beta1hat)/beta2hat

##OVB:
delta1hat*beta2hat

## Calculate delta1hat by OLS
x2regression = lm(IQ ~ educ, data=wage2)
summary(x2regression)

mlr2 = lm(wage ~ educ + KWW, data=wage2)
summary(mlr2)
beta2hat2 = coefficients(mlr2)["KWW"]
beta1hat2 = coefficients(mlr2)["KWW"]
delta1hat2 = (beta1tilde - beta1hat2)/beta2hat2
delta1hat2*beta2hat2

summary(tenure, data=wage2)
wage2$tenure_new = ifelse(wage2$tenure > wage2$exper, wage2$exper, wage2$tenure)
mean(wage2$tenure_new)
