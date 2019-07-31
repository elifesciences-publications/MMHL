# Load packages
require(nlme)
require(pastecs)
require(lme4)
require(emmeans)
require(MuMIn)
require(ggplot2)

# clear workspace
rm(list = ls(all = TRUE))

# load data file
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_Code/")
d<-read.csv("Exo_Comp_RatersPeakPicking_forR.csv", header=T)
d<-subset(d, Condition == "Nonspeech" | Condition == "Speechlike" | Condition == "Speech")

# turn Age_band into a factor
d$Age_band<-as.factor(d$Age_band)

# Look at MM only
MM<-subset(d, Group == "MM")

# subset the data
P1<-subset(MM,Component=="P1")
N1<-subset(MM,Component=="N1")
P2<-subset(MM,Component=="P2")
N2<-subset(MM,Component=="N2")

#set contrasts
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
# Check contrast
contrasts(d$Condition)

# mixed models LMER
# P1 AMPLITUDE
SaturatedModel<-lmer(Amplitude~BEPTA + 
                  Age_band + 
                  Condition + 
                  BEPTA:Age_band+
                  BEPTA:Condition+
                  Age_band:Condition+
                  BEPTA:Age_band:Condition
                + (1|Participant),data=P1, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Condition)
m4<-update(m3, .~. - BEPTA:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - BEPTA)
AIC(m7, m6, m5, m4, m3, m2, m1, SaturatedModel)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6)

# extract coefficients
coefs <- data.frame(coef(summary(m2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

m8<-lmer(Amplitude~BEPTA + Condition + BEPTA:Condition 
         + (1|Participant), data = P1, na.action = na.exclude)
anova(m2, m8)

MC_MainEffects<-lmer(Amplitude~BEPTA + Condition
                     +(1|Participant), data = P1, na.action = na.exclude)
MC_GivesBEPTA<-lmer(Amplitude~Condition+(1|Participant), data = P1, na.action = na.exclude)
MC_GivesCondition<-lmer(Amplitude~BEPTA + (1|Participant), data = P1, na.action = na.exclude)
anova(MC_MainEffects, MC_GivesBEPTA)
anova(MC_MainEffects, MC_GivesCondition)
anova(m8, MC_MainEffects)   # Gives interaction

R2_BestFit<-r.squaredGLMM(m8)
R2_BEPTA_temp<-r.squaredGLMM(MC_GivesBEPTA)
R2_Cond_temp<-r.squaredGLMM(MC_GivesCondition)
R2_BEPTACond_temp<-r.squaredGLMM(MC_MainEffects)
R2_BEPTA<-R2_BestFit[1] - R2_BEPTA_temp[1]
R2_Cond<-R2_BestFit[1] - R2_Cond_temp[1]
R2_BEPTACond<-R2_BestFit[1] - R2_BEPTACond_temp[1]
R2_BEPTA
R2_Cond
R2_BEPTACond

# Posthoc
Nonspeech<-subset(P1, Condition == "Nonspeech")
Speechlike<-subset(P1, Condition == "Speechlike")
Speech<-subset(P1, Condition == "Speech")
MINI_NS<-lm(Amplitude~ BEPTA, data = Nonspeech, na.action = na.exclude)
MINI_SL<-lm(Amplitude~ BEPTA, data = Speechlike, na.action = na.exclude)
MINI_Sp<-lm(Amplitude~ BEPTA, data = Speech, na.action = na.exclude)

# extract coefficients
coefs <- data.frame(coef(summary(MINI_NS)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# extract coefficients
coefs <- data.frame(coef(summary(MINI_SL)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# extract coefficients
coefs <- data.frame(coef(summary(MINI_Sp)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


# P1 LATENCY
SaturatedModel<-lmer(Latency~BEPTA + 
                       Age_band + 
                       Condition + 
                       BEPTA:Age_band+
                       BEPTA:Condition+
                       Age_band:Condition+
                       BEPTA:Age_band:Condition
                     + (1|Participant),data=P1, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Condition)
m4<-update(m3, .~. - BEPTA:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - BEPTA)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6)
m8<-lmer(Latency~BEPTA+Age_band+BEPTA:Age_band + (1|Participant), data = P1, na.action = na.exclude)
anova(m3, m8)

# extract coefficients
coefs <- data.frame(coef(summary(m8)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

MC_MainEffects<-lmer(Latency~BEPTA + Age_band
                     +(1|Participant), data = P1, na.action = na.exclude)
MC_GivesBEPTA<-lmer(Latency~Age_band+(1|Participant), data = P1, na.action = na.exclude)
MC_GivesAge<-lmer(Latency~BEPTA + (1|Participant), data = P1, na.action = na.exclude)
anova(MC_MainEffects, MC_GivesBEPTA)
anova(MC_MainEffects, MC_GivesAge)
anova(m8, MC_MainEffects)


R2_BestFit<-r.squaredGLMM(m8)
R2_BEPTA_temp<-r.squaredGLMM(MC_GivesBEPTA)
R2_Age_temp<-r.squaredGLMM(MC_GivesAge)
R2_BEPTAAge_temp<-r.squaredGLMM(MC_MainEffects)
R2_BEPTA<-R2_BestFit[1] - R2_BEPTA_temp[1]
R2_Age<-R2_BestFit[1] - R2_Age_temp[1]
R2_BEPTAAge<-R2_BestFit[1] - R2_BEPTAAge_temp[1]
R2_BEPTA
R2_Age
R2_BEPTAAge

# Posthoc
Nonspeech<-subset(P1, Condition == "Nonspeech")
Speechlike<-subset(P1, Condition == "Speechlike")
Speech<-subset(P1, Condition == "Speech")
MINI_NS<-lm(Latency~BEPTA, data = Nonspeech)
MINI_SL<-lm(Latency~BEPTA, data = Speechlike)
MINI_Sp<-lm(Latency~BEPTA, data = Speech)

# extract coefficients
coefs <- data.frame(coef(summary(MINI_NS)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# extract coefficients
coefs <- data.frame(coef(summary(MINI_SL)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# extract coefficients
coefs <- data.frame(coef(summary(MINI_Sp)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# N1 AMPLITUDE
SaturatedModel<-lmer(Amplitude~BEPTA + 
                       Age_band + 
                       Condition + 
                       BEPTA:Age_band+
                       BEPTA:Condition+
                       Age_band:Condition+
                       BEPTA:Age_band:Condition
                     + (1|Participant),data=N1, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Condition)
m4<-update(m3, .~. - BEPTA:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - BEPTA)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

# extract coefficients
coefs <- data.frame(coef(summary(m7)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

R2_BestFit<-r.squaredGLMM(m6)
anova(m6, m7)
R2_BEPTA_Temp<-r.squaredGLMM(m7)
R2_BEPTA<-R2_BestFit[1] - R2_BEPTA_Temp[1]
R2_BEPTA

# N1 LATENCY
SaturatedModel<-lmer(Latency~BEPTA + 
                       Age_band + 
                       Condition + 
                       BEPTA:Age_band+
                       BEPTA:Condition+
                       Age_band:Condition+
                       BEPTA:Age_band:Condition
                     + (1|Participant),data=N1, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Condition)
m4<-update(m3, .~. - BEPTA:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - BEPTA)
AIC(m7, m6, m5, m4, m3, m2, m1, SaturatedModel)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

# extract coefficients
coefs <- data.frame(coef(summary(m4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

m8<-lmer(Latency~BEPTA + Condition + (1|Participant), data = N1, na.action = na.exclude)
anova(m4, m8)

MC_GivesBEPTA<-update(m8, .~. - Condition)
MC_GivesCondition<-update(m8, .~. - BEPTA)
anova(m8, MC_GivesCondition)
anova(m8, MC_GivesBEPTA)

R2_BestFit<-r.squaredGLMM(m8)
R2_BEPTA_temp<-r.squaredGLMM(MC_GivesBEPTA)
R2_Cond_temp<-r.squaredGLMM(MC_GivesCondition)
R2_BEPTA<-R2_BestFit[1] - R2_BEPTA_temp[1]
R2_Cond<-R2_BestFit[1] - R2_Cond_temp[1]
R2_BEPTA
R2_Cond

p11<-ggplot(data = N1) + 
  geom_point(aes(x = BEPTA, y = Latency), position = position_dodge(1), alpha = 0.6, na.rm = TRUE)+
  geom_smooth(aes(x = BEPTA, y = Latency))
p11


# P2 AMPLITUDE
SaturatedModel<-lmer(Amplitude~BEPTA + 
                       Age_band + 
                       Condition + 
                       BEPTA:Age_band+
                       BEPTA:Condition+
                       Age_band:Condition+
                       BEPTA:Age_band:Condition
                     + (1|Participant),data=P2, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Condition)
m4<-update(m3, .~. - BEPTA:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - BEPTA)
AIC(m7, m6, m5, m4, m3, m2, m1, SaturatedModel)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

# extract coefficients
coefs <- data.frame(coef(summary(m6)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

R2_BestFit<-r.squaredGLMM(m6)
anova(m6, m7)
R2_BEPTA_Temp<-r.squaredGLMM(m7)
R2_BEPTA<-R2_BestFit[1] - R2_BEPTA_Temp[1]
R2_BEPTA

# P2 LATENCY
SaturatedModel<-lmer(Latency~BEPTA + 
                       Age_band + 
                       Condition + 
                       BEPTA:Age_band+
                       BEPTA:Condition+
                       Age_band:Condition+
                       BEPTA:Age_band:Condition
                     + (1|Participant),data=P2, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Condition)
m4<-update(m3, .~. - BEPTA:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - BEPTA)
AIC(m7, m6, m5, m4, m3, m2, m1, SaturatedModel)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

# extract coefficients
coefs <- data.frame(coef(summary(m4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

m8<-lmer(Latency~Condition + (1|Participant), data = P2, na.action = na.exclude)
anova(m4, m8)
R2_BestFit<-r.squaredGLMM(m8)
m9<-lmer(Latency~(1|Participant), data = P2, na.action = na.exclude)
anova(m8, m9)

R2_Cond_temp<-r.squaredGLMM(m9)
R2_Cond<-R2_BestFit[1] - R2_Cond_temp[1]
R2_Cond


# N2 AMPLITUDE
SaturatedModel<-lmer(Amplitude~BEPTA + 
                       Age_band + 
                       Condition + 
                       BEPTA:Age_band+
                       BEPTA:Condition+
                       Age_band:Condition+
                       BEPTA:Age_band:Condition
                     + (1|Participant),data=N2, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Condition)
m4<-update(m3, .~. - BEPTA:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - BEPTA)
AIC(m7, m6, m5, m4, m3, m2, m1, SaturatedModel)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

# extract coefficients
coefs <- data.frame(coef(summary(m4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

m8<-lmer(Amplitude~Age_band + Condition 
         + (1|Participant), data = N2, na.action = na.exclude)
anova(m4, m8)
R2_BestFit<-r.squaredGLMM(m8)

MC_GivesAge<-update(m8, .~. - Age_band)
MC_GivesCondition<-update(m8, .~. - Condition)
anova(m8, MC_GivesAge)
anova(m8, MC_GivesCondition)

R2_Age_temp<-r.squaredGLMM(MC_GivesAge)
R2_Cond_temp<-r.squaredGLMM(MC_GivesCondition)
R2_Age<-R2_BestFit[1] - R2_Age_temp[1]
R2_Cond<-R2_BestFit[1] - R2_Cond_temp[1]
R2_Age
R2_Cond

by(N2$Amplitude, list(N2$Age_band), stat.desc, basic = FALSE)
by(N2$Amplitude, list(N2$Condition), stat.desc, basic = FALSE)
NSvsSL<-subset(N2, Condition == "Nonspeech" |Condition == "Speechlike")
NSvsSp<-subset(N2, Condition == "Nonspeech" |Condition == "Speech")
SLvsSp<-subset(N2, Condition == "Speechlike" |Condition == "Speech")
t.test(Amplitude~Condition, data = NSvsSL)
t.test(Amplitude~Condition, data = NSvsSp)
t.test(Amplitude~Condition, data = SLvsSp)


# N2 LATENCY
SaturatedModel<-lmer(Latency~BEPTA + 
                       Age_band + 
                       Condition + 
                       BEPTA:Age_band+
                       BEPTA:Condition+
                       Age_band:Condition+
                       BEPTA:Age_band:Condition
                     + (1|Participant),data=N2, na.action = na.exclude)
m1<-update(SaturatedModel, .~. - BEPTA:Age_band:Condition)
m2<-update(m1, .~. - Age_band:Condition)
m3<-update(m2, .~. - BEPTA:Condition)
m4<-update(m3, .~. - BEPTA:Age_band)
m5<-update(m4, .~. - Condition)
m6<-update(m5, .~. - Age_band)
m7<-update(m6, .~. - BEPTA)
AIC(m7, m6, m5, m4, m3, m2, m1, SaturatedModel)
anova(SaturatedModel, m1, m2, m3, m4, m5, m6, m7)

# extract coefficients
coefs <- data.frame(coef(summary(m4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

m8<-lmer(Latency~BEPTA + Age_band 
         + (1|Participant), data = N2, na.action = na.exclude)
anova(m4, m8)
R2_BestFit<-r.squaredGLMM(m8)

m9<-lmer(Latency~(1|Participant), data= N2, na.action = na.exclude)
anova(m8, m9)

R2_Age_temp<-r.squaredGLMM(MC_GivesAge)
R2_Cond_temp<-r.squaredGLMM(MC_GivesCondition)
R2_Age<-R2_BestFit[1] - R2_Age_temp[1]
R2_Cond<-R2_BestFit[1] - R2_Cond_temp[1]
R2_Age
R2_Cond

by(N2$Latency, list(N2$Condition), stat.desc, basic = FALSE)
NSvsSL<-subset(N2, Condition == "Nonspeech" |Condition == "Speechlike")
NSvsSp<-subset(N2, Condition == "Nonspeech" |Condition == "Speech")
SLvsSp<-subset(N2, Condition == "Speechlike" |Condition == "Speech")
t.test(Latency~Condition, data = NSvsSL)
t.test(Latency~Condition, data = NSvsSp)
t.test(Latency~Condition, data = SLvsSp)
