# Load packages
require(nlme)
require(pastecs)
require(lme4)
require(lsmeans)
require(MuMIn)
require(ggplot2)
require(readxl)

# clear workspace
rm(list = ls(all = TRUE))

# load data file
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/UCL/EEG study 2011/stats/R_code/GitHub/")
d<-read_xls("MMN_Presence_forR.xls")
d<-subset(d, Group == "MM")

#set contrasts
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
# Check contrast
contrasts(d$Condition)

# MMN AMPLITUDE - AGE AS CATEGORICAL PREDICTOR
SaturatedModel<-lmer(Amplitude_updated~Age_band +
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

# extract coefficients
coefs <- data.frame(coef(summary(m5)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

MC_AgeBand <-lmer(Amplitude_updated~ BEPTA + (1|Participant),data=d)
MC_BEPTA<-lmer(Amplitude_updated~ Age_band + (1|Participant),data=d)
anova(m5, MC_AgeBand)
anova(m5, MC_BEPTA)

R2_BestFit<-r.squaredGLMM(m5)
R2_temp_Age<-r.squaredGLMM(MC_AgeBand)
R2_temp_BEPTA<-r.squaredGLMM(MC_BEPTA)
R2_AgeBand<-R2_BestFit[1] - R2_temp_Age[1]
R2_BEPTA<-R2_BestFit[1] - R2_temp_BEPTA[1]
R2_AgeBand
R2_BEPTA
R2_BestFit[2]

# Look into the main effect of BEPTA on MMN amplitude
P5<-ggplot(data = d)+
  geom_point(aes(x = BEPTA, y = Amplitude_50ms, shape = Condition), colour = "#E69F00", size = 4, na.rm = TRUE, stroke = 1)+
  geom_smooth(aes(x = BEPTA, y = Amplitude_50ms, linetype = Condition), colour = "#E69F00", size = 1.5, method = "lm")
P5<-P5+theme_minimal()
P5
