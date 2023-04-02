#loading packages
library("effects")
library("foreign")
library("gmodels")
library("gplots")
library("nlme")
library("nnet")
library("stargazer")
library("tidyverse")
library(MASS)
library(rockchalk)
library(pscl)
library(car)
library(DescTools)
library(psych)


#Reading in data/ setting reference categories
d1 = read.dta("genpop replication.dta")
d1$race=relevel(d1$race, ref="White")
d1$dideo=relevel(d1$dideo, ref="Moderate")
d1$PartyID=relevel(d1$PartyID, ref="Independent")
d1$educ=relevel(d1$educ, ref="Some College")
d1$Age=Recode(d1$Age, "-936='18'")
d1$Age=as.numeric(d1$Age)
d1$Age=d1$Age+17

#ordered logit models they used for paper - study1 TABLE 1
mod.50 = polr(Q23_1~Extra+Open+consc+Agree+emotstab+educ+income+race+dideo+PartyID+gender+Age, data=d1)
mod.150 = polr(Q23_1~Extra+Open+consc+Agree+emotstab, data=d1)
stargazer(mod.150,mod.50, type='html', style='ajps', out='tab1.html')

summary(mod.50)
summary(mod.150)
PseudoR2(mod.50)
PseudoR2(mod.150)
#obtaining probabilities for graph
effagree = as.data.frame(effect(c('Agree'), mod=mod.50))
agree=effagree[,c(1,2,3,4)]
colnames(agree) <- c("score", "prob1", "prob2", "prob3")
agree$group <- "Agreeableness"

effcons = as.data.frame(effect(c('consc'), mod=mod.50))
cons=effcons[,c(1,2,3,4)]
colnames(cons) <- c("score", "prob1", "prob2", "prob3")
cons$group <- "Conscientiousness"

effextra = as.data.frame(effect(c('Extra'), mod=mod.50))
extra=effextra[,c(1,2,3,4)]
colnames(extra) <- c("score", "prob1", "prob2", "prob3")
extra$group <- "Extraversion"

effopen = as.data.frame(effect(c('Open'), mod=mod.50))
open=effopen[,c(1,2,3,4)]
colnames(open) <- c("score", "prob1", "prob2", "prob3")
open$group <- "Openness"

effneuro = as.data.frame(effect(c('emotstab'), mod=mod.50))
neuro=effneuro[,c(1,2,3,4)]
colnames(neuro) <- c("score", "prob1", "prob2", "prob3")
neuro$group <- "Emotional Stability"

#creating plot
scores <- rbind(agree, cons, extra, open, neuro)
scores$group <- factor(scores$group)
graph1 <- ggplot(scores, aes(x=score, y=prob2, group=group, col=group, fill=group)) +
  geom_point() +
  geom_path(size=1) +
  labs(title = "Open to the Possibility of Seeking Elective Office\n", x = "Big 5 Personality Score", y = "Probability(Open to Possibility)")
graph1

# Recreating study 2
levels(d2$run)
# Importing data
d2 = read.dta("AMOS replication.dta")
d2$gender = factor(d2$gender)
d2$runhigher = factor(d2$progamb_runhigher)
d2$progamb=as.numeric(d2$runhigher)
d2$progamb_winlegis_1[d2$progamb_winlegis_1==-99] <- NA

# Models for study 2 - TABLE 2
mod.71=polr(runhigher~extra4pt+ open4pt+agree4pt+ consc4pt+ stable4pt, data=d2)
mod.73=polr(runhigher~extra4pt+ open4pt+agree4pt+ consc4pt+ stable4pt+ closevote+ tenure+ progamb_current+termlimits+ partisanelect+ progamb_similar_1+ progamb_winlegis_1+gender, data=d2)
stargazer(mod.71, mod.73, type='html', style='ajps', out="tab2.html")

summary(mod.71)
summary(mod.73)
PseudoR2(mod.71)
PseudoR2(mod.73)
#obtaining probabilities for graph
effagree_tab2 = as.data.frame(effect(c('agree4pt'), mod=mod.73))
agree_tab2=effagree_tab2[,c(1,2,3,4,5)]
colnames(agree_tab2) <- c("score", "prob1", "prob2", "prob3", "prob4")
agree_tab2$group <- "Agreeableness"

effcons_tab2 = as.data.frame(effect(c('consc4pt'), mod=mod.73))
cons_tab2=effcons_tab2[,c(1,2,3,4,5)]
colnames(cons_tab2) <- c("score", "prob1", "prob2", "prob3", "prob4")
cons_tab2$group <- "Conscientiousness"

effextra_tab2 = as.data.frame(effect(c('extra4pt'), mod=mod.73))
extra_tab2=effextra_tab2[,c(1,2,3,4,5)]
colnames(extra_tab2) <- c("score", "prob1", "prob2", "prob3", "prob4")
extra_tab2$group <- "Extraversion"

effopen_tab2 = as.data.frame(effect(c('open4pt'), mod=mod.73))
open_tab2=effopen_tab2[,c(1,2,3,4,5)]
colnames(open_tab2) <- c("score", "prob1", "prob2", "prob3", "prob4")
open_tab2$group <- "Openness"

effneuro_tab2 = as.data.frame(effect(c('stable4pt'), mod=mod.73))
neuro_tab2=effneuro_tab2[,c(1,2,3,4,5)]
colnames(neuro_tab2) <- c("score", "prob1", "prob2", "prob3", "prob4")
neuro_tab2$group <- "Emotional Stability"

#creating plot
scores_tab2 <- rbind(agree_tab2, cons_tab2, extra_tab2, open_tab2, neuro_tab2)
scores_tab2$group <- factor(scores_tab2$group)
graph2 <- ggplot(scores_tab2, aes(x=score, y=prob3, group=group, col=group, fill=group)) +
  geom_point() +
  geom_path(size=1) +
  labs(x = "Big 5 Personality Score", y = "Probability(If the opportunity presented itself)")
graph2


# Models for study 2 - TABLE 3
mod.100 = polr(office~extra4pt+ open4pt+agree4pt+ consc4pt+ stable4pt, data=d2)
mod.101 = polr(office~extra4pt+ open4pt+agree4pt+ consc4pt+ stable4pt+ closevote+ tenure+ progamb_current+termlimits+ partisanelect+ progamb_similar_1+ progamb_winlegis_1+gender, data=d2)
stargazer(mod.100,mod.101, type='html', style='ajps', out="tab3_2.html")

summary(mod.100)
summary(mod.101)
PseudoR2(mod.100)
PseudoR2(mod.101)
#Figure 3
mod.fig3 = multinom(progamb_runhigher~extra4pt+ open4pt+agree4pt+ consc4pt+ stable4pt+ closevote+ tenure+ progamb_current+termlimits+ partisanelect+ tenure+ progamb_similar_1+ progamb_winlegis_1+gender + progamb_winlegis_1*agree4pt, data=d2)
plot(effect('agree4pt:progamb_winlegis_1', mod.fig3, xlevels=list(agree4pt=c(0,2,2.75,3))))

#recreating figure 3
fig3_tables <- as.data.frame(effect('agree4pt:progamb_winlegis_1', mod.fig3, xlevels=list(agree4pt=c(0,2,2.75,3))))

graph3 <- ggplot(fig3_tables, aes(x=progamb_winlegis_1, y=prob.X3, group=factor(agree4pt), col=factor(agree4pt), fill=factor(agree4pt))) +
  geom_point() +
  geom_path(size=1) +
  labs(x = "Perceived Probability of Victory", y = "Probability(If the opportunity presented itself)")
graph3

# parallel lines assumption for study 1
d4 <- na.omit(d1)
for(i in 1:length(unique(d4$Q23_1))) {
  assign(paste("logit_model", i, sep=""), 
         glm(ifelse(Q23_1==unique(d4$Q23_1)[i], 1, 0) ~ Extra+Open+consc+Agree+emotstab, data=d4, family = "binomial"), 
         envir = globalenv())
}

stargazer(logit_model1,logit_model2,logit_model3, type="html", out="assumption2.html")

# parallel lines assumption for study 2
d3 <- na.omit(d2)
for(i in 1:length(unique(d3$runhigher))) {
  assign(paste("logit_model", i, sep=""), 
  glm(ifelse(runhigher==unique(d3$runhigher)[i], 1, 0) ~ extra4pt+ open4pt+agree4pt+ consc4pt+ stable4pt, data=d3, family = "binomial"), 
  envir = globalenv())
}

stargazer(logit_model1,logit_model2,logit_model3,logit_model4, type="html", out="assumption.html")



