library(wooldridge)
library(car)
data("k401ksubs")
data(cps91)
data(card)
data("lawsch85")



############################Problem 1
############################

length(k401ksubs)
summary(k401ksubs)
table(k401ksubs$pira,k401ksubs$male)
Q5ira = length(k401ksubs$pira)
Q5male = length(k401ksubs$male)
table(k401ksubs$male)

############################Problem 2
############################

slr2 = lm(lwage ~ educ, data=cps91)
summary(slr2)

###########################Problem3
#########################

slr3 = lm(lwage ~ educ, data=card)
summary(slr3)

############################Problem 4
###########################

slr4 = lm(lwage ~ educ, data=cps91)
summary(slr4)

#############################Problem 5
#############################

slr5 = lm(lwage ~ educ, data=card)
summary(slr5)

############################Problem 6 & 7 & 8
###########################

mlr6 = lm(lsalary ~ LSAT + GPA + lcost + llibvol, data=lawsch85)
summary(mlr6)
tq11 <- (coefficients(mlr6)[3]-1)/.108528

#########################Problem 9
########################

########################Problem 10
#######################

linearHypothesis(mlr6,c("LSAT=GPA"))
