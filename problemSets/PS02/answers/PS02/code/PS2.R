#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

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

lapply(c("tidyverse", "stargazer", "car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))


# changing factors to normal factors rather than ordered factors
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)

# Regression
reg1 <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial(link = "logit"), )
reg1_null <- glm(choice ~ 1, data = climateSupport, family = binomial(link = "logit"))
anova(reg1_null, reg1, test="LRT")

#tables
summary(reg1)
stargazer(reg1, type = "latex", title = "Difflog and Presvote")

#Creating predicted data table
predicted_data <- with(climateSupport, expand.grid(countries = unique(countries),
                                               sanctions = unique(sanctions)
                                               ))

predicted_data <- cbind(predicted_data, predict(reg1, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))
predicted_data <- within(predicted_data,
                         {PredictedProb <- plogis(fit)
                         LL <- plogis(fit - (1.96 * se.fit))
                         UL <- plogis(fit + (1.96 * se.fit))
                         })

#table for predicted data
stargazer(predicted_data, summary = FALSE, type = "latex", title= "Predicted Data")


#testing interaction model
reg2 <- glm(choice ~ countries * sanctions, data = climateSupport, family = binomial(link = "logit"), )
summary(reg2)
anova(reg2, reg1, test = "LRT")

#misc - going to show in class
-.27266 + .64835 + .19186
climateSupport[9,2:3]
predict(reg1, newdata = climateSupport[9,2:3], type = "response")
pl
?predict()
