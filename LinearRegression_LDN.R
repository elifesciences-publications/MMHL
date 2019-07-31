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

# clear workspace
rm(list = ls(all = TRUE))

# load data file
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code/GitHub/")
d<-read_xls("LDN_Presence_forR.xls")
d<-subset(d, Condition == "Speechlike" | Condition == "Speech")
by(d$Amplitude_updated, list(d$Group), stat.desc, basic = FALSE)
by(d$Amplitude_updated, list(d$Age_band), stat.desc, basic = FALSE)
Old<-subset(d, Age_band == "Older")
by(Old$Amplitude_updated, list(Old$Group), stat.desc, basic = FALSE)

#set contrasts
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
# Check contrast
contrasts(d$Condition)

# LDN AMPLITUDE - AGE AS CATEGORICAL PREDICTOR - ALL CHILDREN
SaturatedModel<-lmer(Amplitude_fixed~Group + 
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
coefs <- data.frame(coef(summary(m6)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m7)

# LDN AMPLITUDE - AGE AS CATEGORICAL PREDICTOR - ONLY PRESENT LDN
SaturatedModel<-lmer(Amplitude_Naexclude~Group + 
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
coefs <- data.frame(coef(summary(m6)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

anova(m6, m7)

# Use R square as an index of goodness of fit for the selected model
R2_BestFit <-r.squaredGLMM(m7)

#############
# BEPTA
#############
# clear workspace
rm(list = ls(all = TRUE))

# load data file
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code")
d<-read_xls("LDN_Presence_forR.xls")
d<-subset(d, Condition == "Speechlike" | Condition == "Speech")
d<-subset(d, Group == "MM")

#set contrasts
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
# Check contrast
contrasts(d$Condition)

# MMN AMPLITUDE - AGE AS CATEGORICAL PREDICTOR
SaturatedModel<-lmer(Amplitude_Naexclude~Age_band +
                       Condition +
                       BEPTA + 
                       Age_band:Condition+
                       BEPTA:Age_band + 
                       BEPTA:Condition + 
                       BEPTA:Age_band:Condition + 
                       Age_band
                     + (1|Participant),data=d)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Age_band)
m4<-update(m3, .~. - BEPTA:Condition)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
baseline<-update(m6, .~. - BEPTA)
anova(baseline, m6, m5, m4, m3, m2, m1, SaturatedModel)
AIC(baseline, m6, m5, m4, m3, m2, m1, SaturatedModel)

m7<-lmer(Amplitude_Naexclude~Condition + (1|Participant), data = d)
anova(m4, m7)

MC_Condition<-lmer(Amplitude_Naexclude ~ (1|Participant), data = d)
anova(m7, MC_Condition)

R2_BestFit<-r.squaredGLMM(m7)
R2_temp_Cond<-r.squaredGLMM(MC_Condition)
R2_Condition<-R2_BestFit[1] - R2_temp_Cond[1]
R2_Condition
