# load packages
require(nlme)
require(pastecs)
require(lme4)
require(lsmeans)
require(MuMIn)
require(rcompanion)
require(car)
require(stats)

# clear workspace
rm(list = ls(all = TRUE))

### load data file EXP 1
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code")
d<-read.csv("Exo_Comp_RatersPeakPicking_forR.csv",header=T)
d<-subset(d, Condition == "Nonspeech"|Condition == "Speechlike" | Condition == "Speech")
d$Condition<-factor(d$Condition, level=c("Nonspeech", "Speechlike", "Speech"))
d$Component<-factor(d$Component, level=c("P1", "N1", "P2", "N2"))
d$Age_band<-factor(d$Age_band, level=c("Younger", "Older"))


# Progressive model fitting
FullModel<-glm(d$Presence ~ d$Age_band
               +d$Group
               +d$Condition
               +d$Component
               +d$Age_band:d$Group
               +d$Age_band:d$Condition
               +d$Age_band:d$Component
               +d$Group:d$Condition
               +d$Group:d$Component
               +d$Condition:d$Component
               +d$Age_band:d$Condition:d$Component
               +d$Age_band:d$Group:d$Condition
               +d$Age_band:d$Group:d$Component
               +d$Group:d$Condition:d$Component
               +d$Age_band:d$Group:d$Condition:d$Component, 
               data = d, family = binomial(), na.action=na.exclude)
m1<-glm(d$Presence ~ d$Age_band
               +d$Group
               +d$Condition
               +d$Component
               +d$Age_band:d$Group
               +d$Age_band:d$Condition
               +d$Age_band:d$Component
               +d$Group:d$Condition
               +d$Group:d$Component
               +d$Condition:d$Component
               +d$Age_band:d$Condition:d$Component
               +d$Age_band:d$Group:d$Condition
               +d$Age_band:d$Group:d$Component
               +d$Group:d$Condition:d$Component, 
                data = d, family = binomial(), na.action=na.exclude)
m2<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group
        +d$Age_band:d$Condition
        +d$Age_band:d$Component
        +d$Group:d$Condition
        +d$Group:d$Component
        +d$Condition:d$Component
        +d$Age_band:d$Condition:d$Component
        +d$Age_band:d$Group:d$Condition
        +d$Age_band:d$Group:d$Component, 
        data = d, family = binomial(), na.action=na.exclude)
m3<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group
        +d$Age_band:d$Condition
        +d$Age_band:d$Component
        +d$Group:d$Condition
        +d$Group:d$Component
        +d$Condition:d$Component
        +d$Age_band:d$Condition:d$Component
        +d$Age_band:d$Group:d$Condition, 
        data = d, family = binomial(), na.action=na.exclude)
m4<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group
        +d$Age_band:d$Condition
        +d$Age_band:d$Component
        +d$Group:d$Condition
        +d$Group:d$Component
        +d$Condition:d$Component
        +d$Age_band:d$Condition:d$Component,
        data = d, family = binomial(), na.action=na.exclude)
m5<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group
        +d$Age_band:d$Condition
        +d$Age_band:d$Component
        +d$Group:d$Condition
        +d$Group:d$Component
        +d$Condition:d$Component, 
        data = d, family = binomial(), na.action=na.exclude)
m6<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group
        +d$Age_band:d$Condition
        +d$Age_band:d$Component
        +d$Group:d$Condition
        +d$Group:d$Component, 
        data = d, family = binomial(), na.action=na.exclude)
m7<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group
        +d$Age_band:d$Condition
        +d$Age_band:d$Component
        +d$Group:d$Condition, 
        data = d, family = binomial(), na.action=na.exclude)
m8<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group
        +d$Age_band:d$Condition
        +d$Age_band:d$Component, 
        data = d, family = binomial(), na.action=na.exclude)
m9<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group
        +d$Age_band:d$Condition, 
        data = d, family = binomial(), na.action=na.exclude)
m10<-glm(d$Presence ~ d$Age_band
        +d$Group
        +d$Condition
        +d$Component
        +d$Age_band:d$Group, 
        data = d, family = binomial(), na.action=na.exclude)
m11<-glm(d$Presence ~ d$Age_band
         +d$Group
         +d$Condition
         +d$Component, 
         data = d, family = binomial(), na.action=na.exclude)
m12<-glm(d$Presence ~ d$Age_band
         +d$Group
         +d$Component, 
         data = d, family = binomial(), na.action=na.exclude)
m13<-glm(d$Presence ~ d$Age_band
         +d$Group, 
         data = d, family = binomial(), na.action=na.exclude)
m14<-glm(d$Presence ~ d$Group, 
         data = d, family = binomial(), na.action=na.exclude)
m15<-glm(d$Presence ~ 1, 
         data = d, family = binomial(), na.action=na.exclude)
m16<-glm(d$Presence ~d$Age_band 
         +d$Group
         +d$Component
         +d$Condition
         +d$Age_band:d$Component
         +d$Age_band:d$Condition, 
         data = d, family = binomial(), na.action = na.exclude)
m17<-glm(Presence ~Age_band 
         +Group
         +Component
         +Age_band:Component,
         data = d, family = binomial(), na.action = na.exclude)

anova(FullModel, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, test = "Chisq")
AIC(FullModel, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17)

# Analysis of variance for individual terms
summary(m17)
anova(m17, test = "Chisq")

# Model chi square
modelChi<-m17$null.deviance - m17$deviance
modelChi
chidf<-m17$df.null - m17$df.residual
chidf
chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

# Compute odds ratio
exp(m17$coefficients)

MC_MainEffects<-glm(Presence ~ Age_band + Group + Component, data = d, family = binomial(), na.action = na.exclude)
MC_GivesGroup<-glm(Presence ~ Age_band + Component, data = d, family = binomial(), na.action = na.exclude)
MC_GivesAge<-glm(Presence ~ Group+ Component, data = d, family = binomial(), na.action = na.exclude)
MC_GivesComponent<-glm(Presence ~ Group+ Age_band, data = d, family = binomial(), na.action = na.exclude)
anova(m17, MC_MainEffects, test = "Chisq")        #Gives interaction
anova(MC_MainEffects, MC_GivesGroup, test = "Chisq")
anova(MC_MainEffects, MC_GivesAge, test = "Chisq")
anova(MC_MainEffects, MC_GivesComponent, test = "Chisq")

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m17)
# Use R square difference as an index of variance explained by a specific factor
Rtemp_GivesGroup<-r.squaredGLMM(MC_GivesGroup)
Rtemp_GivesAge<-r.squaredGLMM(MC_GivesAge)
Rtemp_GivesComponent<-r.squaredGLMM(MC_GivesComponent)
Rtemp_GivesAgeComponent<-r.squaredGLMM(MC_MainEffects)
R2_Group<-R2_BestFit[1] - Rtemp_GivesGroup[1]
R2_Age<-R2_BestFit[1] - Rtemp_GivesAge[1]
R2_Component<-R2_BestFit[1] - Rtemp_GivesComponent[1]
R2_AgeComponent <- R2_BestFit[1] - Rtemp_GivesAgeComponent[1]
R2_Group
R2_Age
R2_Component
R2_AgeComponent

# post-hoc COMPONENTS & odds ratio
P1<-subset(d, d$Component == "P1")
N1<-subset(d, d$Component == "N1")
P2<-subset(d, d$Component == "P2")
N2<-subset(d, d$Component == "N2")
NewModel_P1<-glm(P1$Presence~Age_band, data = P1, family = binomial(), na.action = na.exclude)
NewModel_N1<-glm(N1$Presence~Age_band, data = N1, family = binomial(), na.action = na.exclude)
NewModel_P2<-glm(P2$Presence~Age_band, data = P2, family = binomial(), na.action = na.exclude)
NewModel_N2<-glm(N2$Presence~Age_band, data = N2, family = binomial(), na.action = na.exclude)
anova(NewModel_P1, test = "Chisq")
exp(NewModel_P1$coefficients)
anova(NewModel_N1, test = "Chisq")
exp(NewModel_N1$coefficients)
anova(NewModel_P2, test = "Chisq")
exp(NewModel_P2$coefficients)
anova(NewModel_N2, test = "Chisq")
exp(NewModel_N2$coefficients)

# Extra analyses - not 100% necessary.
P1N1<-subset(d, d$Component == "P1"| d$Component == "N1")
P1P2<-subset(d, d$Component == "P1"| d$Component == "P2")
N2N1<-subset(d, d$Component == "N2" | d$Component == "N1")
N2P2<-subset(d, d$Component == "N2" | d$Component == "P2")
P1N2<-subset(d, d$Component == "P1" | d$Component == "N2")

chisq.test(P1N1$Presence, P1P2$Component, correct = FALSE)
chisq.test(P1P2$Presence, P1P2$Component, correct = FALSE)
chisq.test(N2N1$Presence, N2N1$Component, correct = FALSE)
chisq.test(N2P2$Presence, N2P2$Component, correct = FALSE)
chisq.test(P1N2$Presence, P1N2$Component, correct = FALSE)

# post-hoc CONDITION
Nonspeech<-subset(d, d$Condition == "Nonspeech")
Speechlike<-subset(d, d$Condition == "Speechlike")
Speech<-subset(d, d$Condition == "Speech")
NewModel_NS<-glm(Nonspeech$Presence~Age, data = Nonspeech, family = binomial(), na.action = na.exclude)
NewModel_SL<-glm(Speechlike$Presence~Age, data = Speechlike, family = binomial(), na.action = na.exclude)
NewModel_Sp<-glm(Speech$Presence~Age, data = Speech, family = binomial(), na.action = na.exclude)
summary(NewModel_NS)
exp(NewModel_NS$coefficients)
summary(NewModel_SL)
exp(NewModel_SL$coefficients)
summary(NewModel_Sp)
exp(NewModel_Sp$coefficients)




################################################
# EXPERIMENT 2
################################################
# clear workspace
rm(list = ls(all = TRUE))

### load data file EXP 2
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code")
d<-read.csv("ExoComp_LogisticReg_FollowUp_forR.csv",header=T)
d<-subset(d, Condition == "Nonspeech"|Condition == "Speechlike" | Condition == "Speech")
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
d$Component<-factor(d$Component, level=c("P1", "N1", "P2", "N2"))
d$age_band<-factor(d$age_band, level=c("young", "older"))

# Set up the model
SaturatedModel<-glm(d$Presence ~ d$age_band
               +d$Condition
               +d$Component
               +d$age
               +d$age_band:d$Condition
               +d$age_band:d$Component
               +d$Condition:d$Component
               +d$age_band:d$Condition:d$Component
               + (1|d$code), data = d, family = binomial(), na.action=na.exclude)
m1<-glm(d$Presence ~ d$age_band
        +d$Condition
        +d$Component
        +d$age
        +d$age_band:d$Condition
        +d$age_band:d$Component
        +d$Condition:d$Component
        + (1|d$code), data = d, family = binomial(), na.action=na.exclude)
m2<-glm(d$Presence ~ d$age_band
       +d$Condition
       +d$Component
       +d$age
       +d$age_band:d$Component
       +d$age_band:d$Condition
       + (1|d$code), data = d, family = binomial(), na.action=na.exclude)
m3<-glm(d$Presence ~ d$age_band
       +d$Condition
       +d$Component
       +d$age
       +d$age:d$Condition
       + (1|d$code), data = d, family = binomial(), na.action=na.exclude)
m4<-glm(d$Presence ~ d$age_band
       +d$Condition
       +d$Component
       +d$age
       + (1|d$code), data = d, family = binomial(), na.action=na.exclude)
m5<-glm(d$Presence ~ d$age_band
      +d$Component
      +d$age
      + (1|d$code), data = d, family = binomial(), na.action=na.exclude)
m6<-glm(d$Presence ~ d$age_band+d$age
      + (1|d$code), data = d, family = binomial(), na.action=na.exclude)
m7<-glm(d$Presence~ age_band + Component + age_band:Component+d$age
        + (1|code), data = d, family = binomial(), na.action = na.exclude)
anova(SaturatedModel, m1, m2, m7, m3, m4, m5, m6, test = "Chisq")
AIC(SaturatedModel, m1, m2,m7, m3, m4, m5, m6)

# Analysis of variance for individual terms
anova(m7, test = "Chisq")

#Pseudo Rsquared
nagelkerke(m7)

# Model chi square
modelChi<-m7$null.deviance - m7$deviance
modelChi
chidf<-m7$df.null - m7$df.residual
chidf
chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

MC_GivesComponent<-glm(d$Presence~ d$age_band + d$age + (1|code), family = binomial(), na.action = na.exclude, data= d)
MC_GivesAgeBand<-glm(d$Presence~ d$Component + d$age+ (1|code), family = binomial(), na.action= na.exclude, data = d)
MC_MainEffects<-glm(d$Presence~d$age_band + d$Component + d$age+ (1|code), family = binomial(), na.action= na.exclude, data = d)
anova(m7, MC_MainEffects, test = "Chisq")     # Gives interaction
anova(MC_MainEffects, MC_GivesComponent, test = "Chisq")
anova(MC_MainEffects, MC_GivesAgeBand, test = "Chisq")

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m7)
# Use R square difference as an index of variance explained by a specific factor
Rtemp_GivesAge<-r.squaredGLMM(MC_GivesAgeBand)
Rtemp_GivesComponent<-r.squaredGLMM(MC_GivesComponent)
Rtemp_GivesAgeComponent<-r.squaredGLMM(MC_MainEffects)
R2_Age<-R2_BestFit[1] - Rtemp_GivesAge[1]
R2_Component<-R2_BestFit[1] - Rtemp_GivesComponent[1]
R2_AgeComponent <- R2_BestFit[1] - Rtemp_GivesAgeComponent[1]
R2_Age
R2_Component
R2_AgeComponent

# post-hoc Age effect
P1<-subset(d, d$Component == "P1")
N1<-subset(d, d$Component == "N1")
P2<-subset(d, d$Component == "P2")
N2<-subset(d, d$Component == "N2")
NewModel_P1<-glm(P1$Presence~age_band + age
                 +(1|P1$code), data = P1, family = binomial(), na.action = na.exclude)
NewModel_N1<-glm(N1$Presence~age_band + age
                 +(1|N1$code), data = N1, family = binomial(), na.action = na.exclude)
NewModel_P2<-glm(P2$Presence~age_band + age
                 +(1|P2$code), data = P2, family = binomial(), na.action = na.exclude)
NewModel_N2<-glm(N2$Presence~age_band + age
                 +(1|N2$code), data = N2, family = binomial(), na.action = na.exclude)
anova(NewModel_P1, test = "Chisq")
exp(NewModel_P1$coefficients)   # Compute odds ratio
anova(NewModel_N1, test = "Chisq")
exp(NewModel_N1$coefficients)
anova(NewModel_P2, test = "Chisq")
exp(NewModel_P2$coefficients)
anova(NewModel_N2, test = "Chisq")
exp(NewModel_N2$coefficients)



##### LOGISTIC REG ON MM ONLY - INFLUENCE OF BEPTA ?
# clear workspace
rm(list = ls(all = TRUE))

### load data file EXP 1
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code/GitHub/")
d<-read.csv("Exo_Comp_RatersPeakPicking_forR.csv",header=T)
d<-subset(d, Condition == "Nonspeech"|Condition == "Speechlike" | Condition == "Speech")
MM<-subset(d, Group== "MM")
d$Condition<-factor(d$Condition, level=c("Nonspeech", "Speechlike", "Speech"))
d$Component<-factor(d$Component, level=c("P1", "N1", "P2", "N2"))
d$Age_band<-factor(d$Age_band, level=c("Younger", "Older"))

SaturatedModel<-glm(Presence ~ BEPTA
                    + Age_band
                    +Component
                    +Condition
                    +BEPTA:Age_band
                    +BEPTA:Component
                    +BEPTA:Condition
                    +Age_band:Component
                    +Age_band:Condition
                    +Component:Condition
                    +BEPTA:Age_band:Component
                    +BEPTA:Component:Condition
                    +Age_band:Component:Condition
                    +BEPTA:Age_band:Component:Condition,
                    data = MM, family = binomial(), na.action = na.exclude)
m1<-glm(Presence~BEPTA
        +Component
        +BEPTA:Component,
        data = MM, family = binomial(), na.action = na.exclude)
anova(SaturatedModel, m1, test = "Chisq")
AIC(SaturatedModel, m1)

# Model chi square
modelChi<-m1$null.deviance - m1$deviance
modelChi
chidf<-m1$df.null - m1$df.residual
chidf
chisq.prob<- 1-pchisq(modelChi, chidf)
chisq.prob

summary(m1)
exp(m1$coefficients)

MC_GivesMainEffects<-glm(Presence~BEPTA
                         +Component,
                         data = MM, family = binomial(), na.action= na.exclude)
MC_GivesBEPTA<-glm(Presence~Component,
                   data = MM, family = binomial(), na.action= na.exclude)
MC_GivesComponent<-glm(Presence~BEPTA,
                       data = MM, family = binomial(), na.action = na.exclude)
anova(m1, MC_GivesMainEffects, test = "Chisq")      # Gives interaction
anova(MC_GivesMainEffects, MC_GivesBEPTA, test = "Chisq")
anova(MC_GivesMainEffects, MC_GivesComponent, test = "Chisq")


# Use R square as an index of goodness of fit for the selected model
R2_BestFit<-r.squaredGLMM(m1)
Rtemp_GivesComponent<-r.squaredGLMM(MC_GivesComponent)
Rtemp_GivesBEPTA<-r.squaredGLMM(MC_GivesBEPTA)
Rtemp_MainEffect<-r.squaredGLMM(MC_GivesMainEffects)
R2_Component<-Rtemp_MainEffect[1] - Rtemp_GivesComponent[1]
R2_BEPTA<-Rtemp_MainEffect[1] - Rtemp_GivesBEPTA[1]
R2_BEPTAComp<-R2_BestFit[1] - Rtemp_MainEffect[1]
R2_Component
R2_BEPTA
R2_BEPTAComp

# Posthocs
P1<-subset(d, Component == "P1")
N1<-subset(d, Component == "N1")
P2<-subset(d, Component == "P2")
N2<-subset(d, Component == "N2")
Mini_P1<-glm(Presence~BEPTA, data = P1, family = binomial(), na.action = na.exclude)
Mini_N1<-glm(Presence~BEPTA, data = N1, family = binomial(), na.action = na.exclude)
Mini_P2<-glm(Presence~BEPTA, data = P2, family = binomial(), na.action = na.exclude)
Mini_N2<-glm(Presence~BEPTA, data = N2, family = binomial(), na.action = na.exclude)
summary(Mini_P1)
summary(Mini_N1)
summary(Mini_P2)
summary(Mini_N2)
