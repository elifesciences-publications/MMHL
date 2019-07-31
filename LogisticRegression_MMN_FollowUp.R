# load packages
require(nlme)
require(pastecs)
require(lme4)
require(lsmeans)
require(MuMIn)
require(rcompanion)
require(car)
require(stats)
require(ggplot2)
require(readxl)

# clear workspace
rm(list = ls(all = TRUE))

######################################
# WITHIN-SUBJECT ANALYSIS
######################################
### load data file EXP 2
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code/GitHub")
d<-read_xls("MMN_Presence_FollowUp_forR.xls")
d$Age_band<-factor(d$Age_band, level=c("Younger", "Older", "YOLD"))
d<-subset(d, Age_band == "Younger" |Age_band == "YOLD")

# Progressive model fitting
SaturatedModel<-glm(d$Presence_numerical ~ d$Age_band
               +d$Condition
               +d$Age_band:d$Condition
               +(1|Participant),
               data = d, family = binomial(), na.action=na.exclude)
m1<-glm(d$Presence_numerical ~ d$Age_band
        +d$Condition,
        data = d, family = binomial(), na.action=na.exclude)
m2<-glm(d$Presence_numerical ~ d$Age_band,
        data = d, family = binomial(), na.action=na.exclude)
anova(SaturatedModel, m1, m2, test = "Chisq")
AIC(SaturatedModel, m1, m2)
anova(m2, test = "Chisq")

# Model chi square
modelChi<-m2$null.deviance - m2$deviance
modelChi
chidf<-m2$df.null - m2$df.residual
chidf
chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

# Compute odds ratio
exp(m2$coefficients)

# QUICK PLOT
p1<-ggplot(data = d, aes(x = Age_band, y = Presence_numerical))+ 
  geom_jitter(height = 0.05, size = 2, alpha = 0.6)+ 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, size = 1)
p1

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m2)
R2_AgeBAnd <- R2_BestFit[1]

######################################
# BETWEEN-SUBJECT ANALYSIS
######################################
### load data file EXP 2
rm(list = ls(all = TRUE))
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code")
d<-read_xls("MMN_Presence_FollowUp_forR.xls")
d$Age_band<-factor(d$Age_band, level=c("Younger", "Older", "YOLD"))
d<-subset(d, all_groups != "Young_MM")

# Progressive model fitting
SaturatedModel<-glm(d$Presence_numerical ~ d$all_groups
                    +d$Condition
                    +d$all_groups:d$Condition,
                    data = d, family = binomial(), na.action=na.exclude)
m1<-glm(d$Presence_numerical ~ d$all_groups
        +d$Condition,
        data = d, family = binomial(), na.action=na.exclude)
m2<-glm(d$Presence_numerical ~ d$all_groups,
        data = d, family = binomial(), na.action=na.exclude)
anova(SaturatedModel, m1, m2, test = "Chisq")
AIC(SaturatedModel, m1, m2)
anova(m2, test = "Chisq")

# Model chi square
modelChi<-m2$null.deviance - m2$deviance
modelChi
chidf<-m2$df.null - m2$df.residual
chidf
chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

# Compute odds ratio
exp(m2$coefficients)

d1<-subset(d, all_groups == "FollowUP_MM" | all_groups == "Older_CA")
d2<-subset(d, all_groups == "FollowUP_MM" | all_groups == "Older_MM")

d1$Age_band<-factor(d1$Age_band, level=c("Older", "YOLD"))
d2$Age_band<-factor(d2$Age_band, level=c("Older", "YOLD"))

m3<-glm(d1$Presence_numerical ~d1$all_groups, data = d1, family = binomial(), na.action = na.exclude)
m4<-glm(d2$Presence_numerical ~d2$all_groups, data = d2, family = binomial(), na.action = na.exclude)
anova(m3, test = "Chisq")
anova(m4, test = "Chisq")

# Compute odds ratio
exp(m3$coefficients)
exp(m4$coefficients)

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m2)
R2_AgeBAnd <- R2_BestFit[1]
