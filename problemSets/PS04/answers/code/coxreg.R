# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

#load in data
data(child)

#carrying out coxph regression
coxmod <- coxph(Surv(enter, exit, event) ~ sex + m.age, data = child)
summary(coxmod)
stargazer(coxmod, type = "latex")
drop1(coxmod, test = "Chisq")


stargazer(drop1(coxmod, test = "Chisq"), type = "latex", summary = FALSE)
#plot
cox_fit <- survfit(coxmod)
autoplot(cox_fit)

plot_cox <- coxreg(Surv(enter, exit, event) ~ sex + m.age, data = child)
plot(plot_cox, xlab = "Duration in Years", ylab = "Cumulative Probability")
# Adding an interaction
cox.int <- coxph(Surv(enter, exit, event) ~ sex * m.age, data = child)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")
