# Load packages
require(nlme)
require(pastecs)
require(lme4)
require(lsmeans)
require(MuMIn)
require(Rcmdr)
require(ggplot2)
require(readxl)
require(lawstat)
require(Hmisc)

# clear workspace
rm(list = ls(all = TRUE))

# load data file
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code/GitHub")
d<-read_xls("MMN_Presence_forR.xls")
by(d$Amplitude_updated, list(d$Group), stat.desc, basic = FALSE)
by(d$Amplitude_updated, list(d$Age_band), stat.desc, basic = FALSE)
Old<-subset(d, Age_band == "Older")
by(Old$Amplitude_updated, list(Old$Group), stat.desc, basic = FALSE)

#set contrasts
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
# Check contrast
contrasts(d$Condition)

# MMN AMPLITUDE - AGE AS CATEGORICAL PREDICTOR - ALL CHILDREN
SaturatedModel<-lmer(Amplitude_50ms~Group + 
                       Age_band +
                       Condition +
                       Group:Age_band+
                       Group:Condition+
                       Age_band:Condition+
                       Group:Age_band:Condition
                     + (1|Participant),data=d)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Condition)
m4<-update(m3, .~. - Group:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - Group)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

# extract coefficients
coefs <- data.frame(coef(summary(m3)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

MC_Group <-lmer(Amplitude_50ms~Condition+Age_band+(1|Participant),data=d)
MC_Age<-lmer(Amplitude_50ms~Group+Condition+(1|Participant),data=d)
MC_Condition<-lmer(Amplitude_50ms~Group+Age_band+(1|Participant), data=d)
MC_MainEffects<-lmer(Amplitude_50ms~Group+Age_band+Condition+(1|Participant), data = d)
anova(m3, MC_MainEffects)             # Gives interaction
anova(MC_Age, MC_MainEffects)
anova(MC_Group, MC_MainEffects)
anova(MC_Condition, MC_MainEffects)

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m3)
# Use R square difference as an index of variance explained by a specific factor
Rtemp_GroupAge<-r.squaredGLMM(MC_MainEffects)
Rtemp_Group<-r.squaredGLMM(MC_Group)
Rtemp_Age <-r.squaredGLMM(MC_Age)
Rtemp_Condition<-r.squaredGLMM(MC_Condition)
R2_Condition <- R2_BestFit[1] - Rtemp_Condition[1]
R2_Age <- R2_BestFit[1] - Rtemp_Age[1]
R2_Group <- R2_BestFit[1] - Rtemp_Group[1]
R2_GroupAge<-R2_BestFit[1] - Rtemp_GroupAge[1]
R2_Group
R2_Age
R2_Condition
R2_GroupAge

# Decompose the interaction
young<-subset(d, Age_band == "Younger")
old<-subset(d, Age_band == "Older")
t.test(Amplitude_50ms ~ Group, data  = young)
t.test(Amplitude_50ms ~ Group, data = old)
by(d$Amplitude_50ms, list(d$Age_band), stat.desc, basic = FALSE)

# Posthoc interaction decomposition
# By condition
NSvsSL<-subset(d, Condition=="Nonspeech" | Condition == "Speechlike")
NSvsSp<-subset(d, Condition=="Nonspeech" | Condition == "Speech")
SLvsSp<-subset(d, Condition == "Speechlike" | Condition == "Speech")
t.test(Amplitude_50ms ~ Condition, data = NSvsSL)
t.test(Amplitude_50ms ~ Condition, data = NSvsSp)
t.test(Amplitude_50ms ~ Condition, data = SLvsSp)

# MMN AMPLITUDE - AGE AS CATEGORICAL PREDICTOR - ONLY PRESENT RESPONSE
SaturatedModel<-lmer(Amplitude_updated~Group + 
                       Age_band +
                       Condition +
                       Group:Age_band+
                       Group:Condition+
                       Age_band:Condition+
                       Group:Age_band:Condition
                     + (1|Participant),data=d)
m1<-update(SaturatedModel, .~. - Group:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - Group:Condition)
m4<-update(m3, .~. - Group:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - Group)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

MC_Group <-lmer(Amplitude_updated~Condition+Age_band+(1|Participant),data=d)
MC_Age<-lmer(Amplitude_updated~Group+Condition+(1|Participant),data=d)
MC_Condition<-lmer(Amplitude_updated~Group+Age_band+(1|Participant), data=d)
MC_MainEffects<-lmer(Amplitude_updated~Group+Age_band+Condition+(1|Participant), data = d)
anova(m3, MC_MainEffects)             # Gives interaction
anova(MC_Age, MC_MainEffects)
anova(MC_Group, MC_MainEffects)
anova(MC_Condition, MC_MainEffects)

# Decompose the interaction
young<-subset(d, Age_band == "Younger")
old<-subset(d, Age_band == "Older")
t.test(Amplitude_updated ~ Group, data  = young)
t.test(Amplitude_updated ~ Group, data = old)
by(d$Amplitude_updated, list(d$Age_band), stat.desc, basic = FALSE)

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m3)
# Use R square difference as an index of variance explained by a specific factor
Rtemp_GroupAge<-r.squaredGLMM(MC_MainEffects)
Rtemp_Group<-r.squaredGLMM(MC_Group)
Rtemp_Age <-r.squaredGLMM(MC_Age)
Rtemp_Condition<-r.squaredGLMM(MC_Condition)
R2_Condition <- R2_BestFit[1] - Rtemp_Condition[1]
R2_Age <- R2_BestFit[1] - Rtemp_Age[1]
R2_Group <- R2_BestFit[1] - Rtemp_Group[1]
R2_GroupAge<-R2_BestFit[1] - Rtemp_GroupAge[1]
R2_Group
R2_Age
R2_Condition
R2_GroupAge

# Decompose the interaction
NS_SL<-subset(d, Condition == "Nonspeech" | Condition == "Speechlike")
NS_Sp<-subset(d, Condition == "Nonspeech" | Condition == "Speech")
Sp_SL<-subset(d, Condition == "Speech" | Condition == "Speechlike")
t.test(Amplitude_updated ~ Condition, data  = NS_SL)
t.test(Amplitude_updated ~ Condition, data = NS_Sp)
t.test(Amplitude_updated ~ Condition, data = Sp_SL)
by(d$Amplitude_updated, list(d$Age_band), stat.desc, basic = FALSE)
