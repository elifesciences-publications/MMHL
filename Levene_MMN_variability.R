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
require(car)

# clear workspace
rm(list = ls(all = TRUE))

# load data file
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code/")
d<-read_xls("MMN_Presence_forR.xls")
d$Group<-as.factor(d$Group)

#set contrasts
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
# Check contrast
contrasts(d$Condition)

# Levene's test
leveneTest(Onset~Group, data = d)
leveneTest(Onset~Age_band, data = d)
leveneTest(Onset~Group:Age_band, data = d)

# MMN AMPLITUDE - AGE AS CATEGORICAL PREDICTOR - ALL CHILDREN
SaturatedModel<-lmer(Onset~Group + 
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
m5<-update(m4, .~. - Age_band)
m6<-update(m5, .~. - Group)
m7<-update(m6, .~. - Condition)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)
AIC(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

R2_bestFit<-r.squaredGLMM(m4)
R2_bestFit

# t-test Condition
NS_SL<-subset(d, Condition == "Nonspeech" | Condition == "Speechlike")
NS_Sp<-subset(d, Condition == "Nonspeech" | Condition == "Speech")
SL_Sp<-subset(d, Condition == "Speechlike" | Condition == "Speech")
t.test(Onset~Condition, data = NS_SL)
t.test(Onset~Condition, data = NS_Sp)
t.test(Onset~Condition, data = SL_Sp)













