library(wooldridge)
library(stargazer)
data(wage2)

wage2$educ_squ = wage2$educ^2
wage2$log_educ = log(wage2$educ)
wage2$log10_educ = log10(wage2$educ)

model1 = lm(lwage ~ educ + educ_squ, data=wage2)

model2 = lm(lwage ~ log_educ, data=wage2 )

model3 = lm(lwage ~ log10_educ, data=wage2)

model4 = lm(lwage ~ asinh_exper, data=wage2)

stargazer(model1, model2, model3, type="text")
