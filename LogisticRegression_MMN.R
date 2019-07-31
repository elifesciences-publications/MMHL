# load packages
require(nlme)
require(pastecs)
require(lme4)
require(lsmeans)
require(MuMIn)
require(stats)
require(readxl)

# clear workspace
rm(list = ls(all = TRUE))

### load data file EXP 1
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code/GitHub/")
d<-read_xls("MMN_Presence_forR.xls")
d$Age_band<-factor(d$Age_band, level=c("Younger", "Older"))

# Progressive model fitting
SaturatedModel<-glm(d$Presence_numerical ~ d$Age_band
               +d$Group
               +d$Condition
               +d$Age_band:d$Group
               +d$Age_band:d$Condition
               +d$Group:d$Condition
               +d$Age_band:d$Group:d$Condition,
               data = d, family = binomial(), na.action=na.exclude)
m1<-glm(d$Presence_numerical ~ d$Age_band
               +d$Group
               +d$Condition
               +d$Age_band:d$Group
               +d$Age_band:d$Condition
               +d$Group:d$Condition,
               data = d, family = binomial(), na.action=na.exclude)
m2<-glm(d$Presence_numerical ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Age_band:d$Group
        +d$Age_band:d$Condition,
        data = d, family = binomial(), na.action=na.exclude)
m3<-glm(d$Presence_numerical ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Age_band:d$Group,
        data = d, family = binomial(), na.action=na.exclude)
m4<-glm(d$Presence_numerical ~ d$Age_band
        +d$Group
        +d$Condition,
        data = d, family = binomial(), na.action=na.exclude)
m5<-glm(d$Presence_numerical ~ d$Group
        +d$Age_band,
        data = d, family = binomial(), na.action=na.exclude)
m6<-glm(d$Presence_numerical ~ d$Group,
        data = d, family = binomial(), na.action=na.exclude)

anova(SaturatedModel, m1, m2, m3, m4, m5, m6, test = "Chisq")
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6)
summary(m6)

# Model chi square
modelChi<-m6$null.deviance - m6$deviance
modelChi
chidf<-m6$df.null - m6$df.residual
chidf
chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

# Compute odds ratio
exp(m6$coefficients)

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m6)
# Use R square difference as an index of variance explained by a specific factor
R2_Group<-R2_BestFit[1]

# LOGISTIC REGRESSION IN MM ONLY - BEPTA AS PREDICTOR
d<-subset(d, Group == "MM")

# Progressive model fitting
SaturatedModel<-glm(d$Presence_numerical ~ d$Age_band
                    +d$BEPTA
                    +d$Condition
                    +d$Age_band:d$BEPTA
                    +d$Age_band:d$Condition
                    +d$BEPTA:d$Condition
                    +d$Age_band:d$BEPTA:d$Condition,
                    data = d, family = binomial(), na.action=na.exclude)
m1<-glm(d$Presence_numerical ~ d$Age_band
        +d$BEPTA
        +d$Condition
        +d$Age_band:d$BEPTA
        +d$Age_band:d$Condition
        +d$BEPTA:d$Condition,
        data = d, family = binomial(), na.action=na.exclude)
m2<-glm(d$Presence_numerical ~ d$Age_band
        +d$BEPTA
        +d$Condition
        +d$Age_band:d$BEPTA
        +d$Age_band:d$Condition,
        data = d, family = binomial(), na.action=na.exclude)
m3<-glm(d$Presence_numerical ~ d$Age_band
        +d$BEPTA
        +d$Condition
        +d$Age_band:d$BEPTA,
        data = d, family = binomial(), na.action=na.exclude)
m4<-glm(d$Presence_numerical ~ d$Age_band
        +d$BEPTA
        +d$Condition,
        data = d, family = binomial(), na.action=na.exclude)
m5<-glm(d$Presence_numerical ~ d$BEPTA
        +d$Condition,
        data = d, family = binomial(), na.action=na.exclude)
m6<-glm(d$Presence_numerical ~ d$BEPTA,
        data = d, family = binomial(), na.action=na.exclude)

anova(SaturatedModel, m1, m2, m3, m4, m5, m6, test = "Chisq")
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6)
summary(m6)
anova(m6, test = "Chisq")

# Model chi square
modelChi<-m6$null.deviance - m6$deviance
modelChi
chidf<-m6$df.null - m6$df.residual
chidf
chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

# Compute odds ratio
exp(m6$coefficients)

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m6)
# Use R square difference as an index of variance explained by a specific factor
R2_BEPTA<-R2_BestFit[1]
R2_BestFit
R2_BEPTA