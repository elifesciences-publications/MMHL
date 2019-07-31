# Load packages
require(nlme)
require(pastecs)
require(lme4)
require(emmeans)
require(MuMIn)

# clear workspace
rm(list = ls(all = TRUE))

# load data file
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_Code/")
d<-read.csv("Exo_Comp_RatersPeakPicking_forR.csv", header=T)
d<-subset(d, Condition == "Nonspeech" | Condition == "Speechlike" | Condition == "Speech")
d$Age_band<-as.factor(d$Age_band)
#set contrasts
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
# Check contrast
contrasts(d$Condition)

# subset the data
P1<-subset(d,Component=="P1")
N1<-subset(d,Component=="N1")
P2<-subset(d,Component=="P2")
N2<-subset(d,Component=="N2")

# mixed models LMER
# P1 AMPLITUDE
SaturatedModel<-lmer(Amplitude~Group + 
                  Age_band + 
                  Condition + 
                  Group:Age_band+
                  Group:Condition+
                  Age_band:Condition+
                  Group:Age_band:Condition
                + (1|Participant),data=P1, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Condition)
m4<-update(m3, .~. - Group:Age_band)
m5<-update(m4, .~. - Age_band)
m6<-update(m5, .~. - Group)
m7<-update(m6, .~. - Condition)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

anova(m6, m7)
R2_BestFit <-r.squaredGLMM(m7)

# Look into the interaction
NS_SL<-subset(P1, Condition == "Nonspeech" | Condition == "Speechlike")
SL_Sp<-subset(P1, Condition == "Speechlike" | Condition == "Speech")
NS_Sp<-subset(P1, Condition == "Speech" | Condition == "Nonspeech")
t.test(Amplitude ~ Condition, data  = NS_SL)
t.test(Amplitude ~ Condition, data  = SL_Sp)
t.test(Amplitude ~ Condition, data  = NS_Sp)

# P1 LATENCY
SaturatedModel<-lmer(Latency~Group + 
                       Age_band + 
                       Condition + 
                       Group:Age_band+
                       Group:Condition+
                       Age_band:Condition+
                       Group:Age_band:Condition
                     + (1|Participant),data=P1, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Condition)
m4<-update(m3, .~. - Group:Age_band)
m5<-update(m4, .~. - Age_band)
m6<-update(m5, .~. - Group)
m7<-update(m6, .~. - Condition)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

MC_Group<-lmer(Latency ~ Condition + (1|Participant), data = P1, na.action = na.exclude)
MC_Condition<-lmer(Latency ~ Group + (1|Participant), data = P1, na.action = na.exclude)
anova(m5, MC_Group)
anova(m5, MC_Condition)

by(P1$Latency, list(P1$Group), stat.desc, basic = FALSE)

# Look into the interaction
NS_SL<-subset(P1, Condition == "Nonspeech" | Condition == "Speechlike")
SL_Sp<-subset(P1, Condition == "Speechlike" | Condition == "Speech")
NS_Sp<-subset(P1, Condition == "Speech" | Condition == "Nonspeech")
t.test(Latency ~ Condition, data  = NS_SL)
t.test(Latency ~ Condition, data  = SL_Sp)
t.test(Latency ~ Condition, data  = NS_Sp)

# P2 AMPLITUDE
SaturatedModel<-lmer(Amplitude~Group + 
                       Age_band + 
                       Condition + 
                       Group:Age_band+
                       Group:Condition+
                       Age_band:Condition+
                       Group:Age_band:Condition
                     + (1|Participant),data=P2, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Condition)
m4<-update(m3, .~. - Group:Age_band)
m5<-update(m4, .~. - Group)
m6<-update(m5, .~. - Condition)
m7<-update(m6, .~. - Age_band)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

MC_AgeBand<-lmer(Amplitude ~ Condition + (1|Participant), data = P2, na.action = na.exclude)
MC_Condition<-lmer(Amplitude ~ Age_band + (1|Participant), data = P2, na.action = na.exclude)
anova(m5, MC_Condition)
anova(m5, MC_AgeBand)

# Look into the interaction
NS_SL<-subset(P2, Condition == "Nonspeech" | Condition == "Speechlike")
SL_Sp<-subset(P2, Condition == "Speechlike" | Condition == "Speech")
NS_Sp<-subset(P2, Condition == "Speech" | Condition == "Nonspeech")
t.test(Amplitude ~ Condition, data  = NS_SL)
t.test(Amplitude ~ Condition, data  = SL_Sp)
t.test(Amplitude ~ Condition, data  = NS_Sp)

# P2 LATENCY
SaturatedModel<-lmer(Latency~Group + 
                       Age_band + 
                       Condition + 
                       Group:Age_band+
                       Group:Condition+
                       Age_band:Condition+
                       Group:Age_band:Condition
                     + (1|Participant),data=P2, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Condition)
m4<-update(m3, .~. - Group:Age_band)
m5<-update(m4, .~. - Age_band)
m6<-update(m5, .~. - Group)
m7<-update(m6, .~. - Condition)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

MC_Group<-lmer(Latency ~ Condition + (1|Participant), data = P2, na.action = na.exclude)
MC_Condition<-lmer(Latency ~ Group + (1|Participant), data = P2, na.action = na.exclude)
anova(m5, MC_Group)
anova(m5, MC_Condition)

# Look into the interaction
NS_SL<-subset(P2, Condition == "Nonspeech" | Condition == "Speechlike")
SL_Sp<-subset(P2, Condition == "Speechlike" | Condition == "Speech")
NS_Sp<-subset(P2, Condition == "Speech" | Condition == "Nonspeech")
t.test(Latency ~ Condition, data  = NS_SL)
t.test(Latency ~ Condition, data  = SL_Sp)
t.test(Latency ~ Condition, data  = NS_Sp)

# mixed models LMER
# N1 AMPLITUDE
SaturatedModel<-lmer(Amplitude~Group + 
                  Age_band + 
                  Condition + 
                  Group:Age_band+
                  Group:Condition+
                  Age_band:Condition+
                  Group:Age_band:Condition
                + (1|Participant),data=N1, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Condition)
m4<-update(m3, .~. - Group:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - Group)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

m8<-update(m1, .~. - Group:Condition - Group:Age_band)
anova(m8, m1)
m9<-update(m8, .~. - Group)
anova(m8, m9)

# Model Comparisons - MC(to get p val and Rsquared)
MainEffects<-lmer(Amplitude~Condition + Age_band + (1|Participant), data = N1)
GivesAge<-lmer(Amplitude~Condition + (1|Participant), data = N1)
GivesCondition<-lmer(Amplitude~Age_band + (1|Participant), data = N1)
GivesAgeCond<-lmer(Amplitude~Age_band + Condition + (1|Participant), data = N1)
anova(MainEffects, GivesAge)
anova(MainEffects, GivesCondition)
anova(m9, GivesAgeCond)

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m8)
# Use R square difference as an index of variance explained by a specific factor
Rtemp_GivesAgeCond<-r.squaredGLMM(GivesAgeCond)
Rtemp_GivesAge<-r.squaredGLMM(GivesAge)
Rtemp_GivesCondition<-r.squaredGLMM(GivesCondition)
Rtemp_MainEffects<-r.squaredGLMM(MainEffects)
R2_GivesAgeCond<-R2_BestFit[1] - Rtemp_GivesAgeCond[1]
R2_GivesAge<-Rtemp_MainEffects[1] - Rtemp_GivesAge[1]
R2_GivesCondition<-Rtemp_MainEffects[1] - Rtemp_GivesCondition[1]
R2_GivesAgeCond
R2_GivesAge
R2_GivesCondition

# Posthoc lm
Nonspeech<-subset(N1, Condition == "Nonspeech")
Speechlike<-subset(N1, Condition == "Speechlike")
Speech<-subset(N1, Condition == "Speech")
NewModel_NS<-lm(Amplitude~Age_band, data = Nonspeech, na.action = na.exclude)
NewModel_SL<-lm(Amplitude~Age_band, data = Speechlike, na.action = na.exclude)
NewModel_Sp<-lm(Amplitude~Age_band, data = Speech, na.action = na.exclude)
summary(NewModel_NS)
summary(NewModel_SL)
summary(NewModel_Sp)
by(Nonspeech$Amplitude, list(Nonspeech$Age_band), stat.desc, basic = FALSE)
by(Speechlike$Amplitude, list(Speechlike$Age_band), stat.desc, basic = FALSE)
by(Speech$Amplitude, list(Speech$Age_band), stat.desc, basic = FALSE)

Young<-subset(N1, Age_band == "Younger")
Old<-subset(N1, Age_band == "Older")
NewModel_Young<-lm(Amplitude~Condition, data = Young, na.action = na.exclude)
NewModel_Old<-lm(Amplitude~Condition, data = Old, na.action = na.exclude)
summary(NewModel_Young)
summary(NewModel_Old)
by(Young$Amplitude, list(Young$Condition), stat.desc, basic = FALSE)
by(Old$Amplitude, list(Old$Condition), stat.desc, basic = FALSE)


# N1 LATENCY
SaturatedModel<-lmer(Latency~Group + 
                  Age_band + 
                  Condition + 
                  Group:Age_band+
                  Group:Condition+
                  Age_band:Condition+
                  Group:Age_band:Condition
                + (1|Participant),data=N1)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Condition)
m4<-update(m3, .~. - Group:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - Group)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

m8<-update(m4, .~. - Group)
anova(m4, m8)

# Model Comparisons - MC(to get p val and Rsquared)
GivesAge<-lmer(Latency~Condition + (1|Participant),data=N1)
GivesCondition<-lmer(Latency~Age_band + (1|Participant),data=N1)

anova(m8, GivesAge)
anova(m8, GivesCondition)

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m1)
# Use R square difference as an index of variance explained by a specific factor
Rtemp_Age <-r.squaredGLMM(GivesAge)
Rtemp_Condition<-r.squaredGLMM(GivesCondition)
R2_Condition <- R2_BestFit[1] - Rtemp_Condition[1]
R2_Age <- R2_BestFit[1] - Rtemp_Age[1]
R2_Age
R2_Condition[1]

by(N1$Latency, list(N1$Age_band), stat.desc, basic = FALSE)
by(N1$Latency, list(N1$Condition), stat.desc, basic = FALSE)

# Look into the interaction
NS_SL<-subset(N1, Condition == "Nonspeech" | Condition == "Speechlike")
SL_Sp<-subset(N1, Condition == "Speechlike" | Condition == "Speech")
NS_Sp<-subset(N1, Condition == "Speech" | Condition == "Nonspeech")
t.test(Latency ~ Condition, data  = NS_SL)
t.test(Latency ~ Condition, data  = SL_Sp)
t.test(Latency ~ Condition, data  = NS_Sp)


# N2 AMPLITUDE
SaturatedModel<-lmer(Amplitude~Group + 
                       Age_band + 
                       Condition + 
                       Group:Age_band+
                       Group:Condition+
                       Age_band:Condition+
                       Group:Age_band:Condition
                     + (1|Participant),data=N2, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Age_band)
m4<-update(m3, .~. - Group:Condition)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - Group)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

MC_MainEffects<-lmer(Amplitude~Group + Age_band + Condition + (1|Participant), data = N2, na.action = na.exclude)
MC_Group<-lmer(Amplitude~Age_band + Condition + (1|Participant), data = N2, na.action = na.exclude)
MC_Condition<-lmer(Amplitude~Age_band + Group + (1|Participant), data = N2, na.action = na.exclude)
MC_AgeBand<-lmer(Amplitude~Group + Condition + (1|Participant), data = N2, na.action = na.exclude)
anova(m3, MC_MainEffects)
anova(MC_MainEffects, MC_Group)
anova(MC_MainEffects, MC_Condition)
anova(MC_MainEffects, MC_AgeBand)

# Look into the interaction
NS<-subset(N2, Condition == "Nonspeech")
SL<-subset(N2, Condition == "Speechlike")
Sp<-subset(N2, Condition == "Speech")
t.test(Amplitude ~ Group, data  = NS)
t.test(Amplitude ~ Group, data  = SL)
t.test(Amplitude ~ Group, data  = Sp)

# N2 LATENCY
SaturatedModel<-lmer(Latency~Group + 
                       Age_band + 
                       Condition + 
                       Group:Age_band+
                       Group:Condition+
                       Age_band:Condition+
                       Group:Age_band:Condition
                     + (1|Participant),data=N2, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Age_band)
m4<-update(m3, .~. - Group:Condition)
m5<-update(m4, .~. - Age_band)
m6<-update(m5, .~. - Group)
m7<-update(m6, .~. - Condition)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

anova(m6, m7)

# Look into the interaction
NS_SL<-subset(N2, Condition == "Nonspeech" | Condition == "Speechlike")
SL_Sp<-subset(N2, Condition == "Speechlike" | Condition == "Speech")
NS_Sp<-subset(N2, Condition == "Speech" | Condition == "Nonspeech")
t.test(Latency ~ Condition, data  = NS_SL)
t.test(Latency ~ Condition, data  = SL_Sp)
t.test(Latency ~ Condition, data  = NS_Sp)