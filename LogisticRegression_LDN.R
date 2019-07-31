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
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code")
d<-read_xls("LDN_Presence_forR.xls")
d<-subset(d, Condition == "Speechlike" | Condition == "Speech")
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

#################
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

m7<-glm(d$Presence_numerical ~ d$BEPTA
        + d$Age_band
        + d$BEPTA:d$Age_band, family = binomial(), na.action = na.exclude)
anova(m3, m7, test = "Chisq")
anova(m7, test = "Chisq")
summary(m7)

# Model chi square
modelChi<-m7$null.deviance - m7$deviance
modelChi
chidf<-m7$df.null - m7$df.residual
chidf
chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

# Compute odds ratio
exp(m7$coefficients)

MC_MainEffects<-glm(d$Presence_numerical ~d$BEPTA + d$Age_band, 
                    family = binomial(), na.action = na.exclude)
MC_Age<-glm(d$Presence_numerical~d$BEPTA, family = binomial(), na.action = na.exclude)
MC_BEPTA<-glm(d$Presence_numerical~d$Age_band, family = binomial(), na.action = na.exclude)
anova(m7, MC_MainEffects, test = "Chisq") # Gives interaction
anova(MC_MainEffects, MC_Age, test = "Chisq")
anova(MC_MainEffects, MC_BEPTA, test = "Chisq")

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m7)
# Use R square difference as an index of variance explained by a specific factor
Rtemp_Age <-r.squaredGLMM(MC_Age)
Rtemp_BEPTA<-r.squaredGLMM(MC_BEPTA)
Rtemp_AgeBEPTA<-r.squaredGLMM(MC_MainEffects)
R2_Age <- R2_BestFit[1] - Rtemp_Age[1]
R2_BEPTA <- R2_BestFit[1] - Rtemp_BEPTA[1]
R2_AgeBEPTA<-R2_BestFit[1] - Rtemp_AgeBEPTA[1]
R2_Age
R2_BEPTA
R2_AgeBEPTA

# DECOMPOSE THE INTERACTION
Y<-subset(d, Age_band == "Younger")
O<-subset(d, Age_band == "Older")
m8<-glm(Y$Presence_numerical ~Y$BEPTA, family = binomial(), na.action = na.exclude)
m9<-glm(O$Presence_numerical ~O$BEPTA, family = binomial(), na.action = na.exclude)
summary(m8)
summary(m9)

# What direction goes the interaction ?
p1<-ggplot(data = Y, aes(x = BEPTA, y = Presence_numerical, colour = Group))+ 
  geom_jitter(height = 0.05, size = 2, alpha = 0.6)+ 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, size = 1)
p1<-p1+theme_minimal()
p1

