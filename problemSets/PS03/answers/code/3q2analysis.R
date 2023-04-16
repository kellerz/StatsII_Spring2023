#################
#QUESTION 2
#################
mexico <- read_csv("data/MexicoMuniData.csv")

# Conduct poisson regression
mexico_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data=mexico, family="poisson")
summary(mexico_poisson)
stargazer(mexico_poisson, type="latex")

#Pseudo R Squared
1 - (mexico_poisson$deviance/mexico_poisson$null.deviance)

#estimate
lambda <- exp(-3.810 - 0.081 - 0.312)
lambda


#zeroinfl_mex <- zeroinfl(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data=mexico, dist = "poisson")
#summary(zeroinfl_mex)
?polr
