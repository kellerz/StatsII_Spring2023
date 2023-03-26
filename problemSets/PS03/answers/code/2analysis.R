###################
#Question 1
###################

#Loading in data
data <- read_csv("data/gdpChange.csv")
data_copy <- data
#data wrangling
head(data_copy)

data_copy$GDPWdiff <- cut(data_copy$GDPWdiff, 
                          breaks = c(-Inf,-.9, .9, Inf),
                          labels = c("NEG", "NONE", "POS"),
                          right = FALSE,
                          ordered_result = FALSE)

?cut

# set a reference level for the outcome
data_copy$GDPWdiff <- relevel(data_copy$GDPWdiff, ref = "NONE")


#Run unordered multinomial model
multinom_model1 <- multinom(GDPWdiff ~ REG + OIL, data = data_copy)
summary(multinom_model1)
exp(coef(multinom_model1))

stargazer(multinom_model1, type = "latex", summary = TRUE)
stargazer(exp(coef(multinom_model1)), type = "latex", summary = FALSE)


# how do we interpret these coefficients?
predicted_data <- with(data_copy, expand.grid(REG = unique(REG),
                                              OIL = unique(OIL)
))

predicted_values <- cbind(predicted_data, predict(multinom_model1, 
                                                  newdata = predicted_data,
                                                  type = "probs",
                                                  se = TRUE))

#prediction accuracy
addmargins(table(data_copy$GDPWdiff, predict(multinom_model1, type = "class")))


###################
#Part B
###################

#POLR
ord.log <- polr(GDPWdiff ~ REG + OIL, data = data_copy, Hess = TRUE)
summary(ord.log)
exp(cbind(OR = coef(ord.log), confint(ord.log)))

stargazer(ord.log, type = "latex", summary = TRUE)
stargazer(exp(cbind(OR = coef(ord.log), confint(ord.log))), type = "latex", summary = FALSE)

#Checking parallel lines assumption
for(i in 1:length(unique(data_copy$GDPWdiff))){
  assign(paste("logit_model", i, sep=""), glm(ifelse(GDPWdiff==unique(data_copy$GDPWdiff)[i], 1, 0) ~ REG + OIL, data = data_copy), envir=globalenv())
}

stargazer(logit_model1, logit_model2, logit_model3, type = "latex")

#predicting
plot_data <- melt(cbind(predicted_data, predict(ord.log, predicted_data, type="probs")), id.vars=c("REG", "OIL"), variable.name="Level", value.name="Probability")
stargazer(plot_data, type="latex", summary=FALSE)
