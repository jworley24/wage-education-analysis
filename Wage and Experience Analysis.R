df <- read.csv(url('https://osuecampus.github.io/econ-424/data/1qezA.csv'))

summary(df)

##########
########## 7 & 8 & 11
wageover12 = subset(df, df$educ>12)
mean(wageover12$wage)
wageunder12 = subset(df, df$educ<=12)
var(wageover12$wage)
var(wageunder12$wage)
mean(df$lwage)

###############Q6
###############Q6
slr6 = lm(wage ~ exper, data=df)
mlr6 = lm(wage ~ exper + IQ, data=df)
summary(slr6)
summary(mlr6)
coefficients(slr6)[2]-coefficients(mlr6)[2]

##########Q9
##########Q9
leduc = log(df$educ)
lwage = log(df$wage)
Q9 = lm(lwage ~ leduc, data=df)
summary(Q9)

###########Q28
###########Q28
mlr28 = lm((educ) ~ sibs + meduc + feduc, data=df)
summary(mlr28)
Educhat = 9.23009 + (2*coefficients(mlr28)["sibs"]) + (12*coefficients(mlr28)["meduc"]) + (12*(mlr28)["feduc"])

##########Q12-17
###########Q12-17
mlr13 = lm(log(wage) ~ educ + exper + feduc, data=df)
summary(mlr13)
(coefficients(mlr13)["feduc"])/.010671
(coefficients(mlr13)["feduc"]-.05)/.010671

#########Q21
#########Q21
slr21 = lm(lwage ~ educ, data=df)
summary(slr21)

#########Q20
#########Q20
Q20 = lm(lwage ~ educ, data=df)
summary(Q20)
