# Load packages
require(nlme)
require(pastecs)
require(lme4)
require(lsmeans)
require(MuMIn)
require(Rcmdr)
require(ggplot2)
require(readxl)

# clear workspace
rm(list = ls(all = TRUE))

# load data file
d<-read_xls("MMN_Presence_FollowUp_forR.xls")

#set contrasts
d$Condition<-factor(d$Condition, level=c("Nonspeech","Speechlike", "Speech"))
# Check contrast
contrasts(d$Condition)

# Subset the data per Group
FollowUpMMHL<-subset(d, all_groups=="FollowUP_MM")
YoungerMMHL<-subset(d, Group == "MMHL" & Age_band =="young")
OlderMMHL<-subset(d, Group == "MMHL" & Age_band =="older")
OlderCA<-subset(d, Group == "CA" & Age_band =="older")
withinMM<-rbind(YoungerMMHL, FollowUpMMHL)

by(d$Age, list(d$Group), stat.desc, basic = FALSE)
by(d$Amplitude_fixedLat, list(d$all_groups), stat.desc, basic = FALSE)

# Within-participants comparison
# 1. subset the data
sub_followup<-subset(d,d$all_groups=="Young_MM" |d$all_groups=="FollowUP_MM")
# 2. run the models
SaturatedModel<-lmer(Amplitude_fixedLat~ Age_band +
                  Condition +
                  Age_band:Condition
                + (1|Participant),data=sub_followup)
m1<-update(SaturatedModel, .~. - Age_band:Condition)
m2<-update(m1, .~. - Condition)
m3<-update(m2, .~. - Age_band)
anova(SaturatedModel, m1, m2, m3)
AIC(SaturatedModel, m1, m2, m3)

# extract coefficients
coefs <- data.frame(coef(summary(m2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

anova(m2, m3)

R2_BestFit <-r.squaredGLMM(m2)
R2_temp_Age<-r.squaredGLMM(m3)
R2_Age<-R2_BestFit[1] - R2_temp_Age[1]
R2_Age

# Older groups comparison
# 1. subset the data
sub_OlderGroups<-subset(d,d$all_groups=="Older_MM" |d$all_groups=="Older_CA"|d$all_groups=="FollowUP_MM")
# 2. run the models
# Updated 12/02/2018: Add age as covariate
SaturatedModel<-lmer(Amplitude_fixedLat~ all_groups +
                      Condition +
                      all_groups:Condition +
                      (1|Participant),data=sub_OlderGroups, na.action=na.exclude)
m1<-update(SaturatedModel, .~. - all_groups:Condition)
m2<-update(m1, .~. - Condition)
m3<-update(m2, .~. - all_groups)
anova(SaturatedModel, m1, m2, m3)
AIC(SaturatedModel, m1, m2, m3)
R2_BestFit <-r.squaredGLMM(m1)

# extract coefficients
coefs <- data.frame(coef(summary(m1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

MC_Group<-lmer(Amplitude_fixedLat~Condition + (1|Participant), data = sub_OlderGroups)
MC_Condition<-lmer(Amplitude_fixedLat~Group + (1|Participant), data = sub_OlderGroups)
anova(m1, MC_Group)
anova(m1, MC_Condition)

R2_temp_Group<-r.squaredGLMM(MC_Group)
R2_temp_Condition<-r.squaredGLMM(MC_Condition)
R2_Group<-R2_BestFit[1] - R2_temp_Group[1]
R2_Condition<-R2_BestFit[1] - R2_temp_Condition[1]
R2_Group
R2_Condition

sub1<-subset(d, d$all_groups=="FollowUP_MM"|d$all_groups=="Older_CA")
sub2<-subset(d, d$all_groups=="FollowUP_MM"|d$all_groups=="Older_MM")
sub3<-subset(d, d$all_groups=="Older_CA"|d$all_groups=="Older_MM")
t.test(Amplitude_fixedLat ~ all_groups, data = sub1, na.action=na.exclude)
t.test(Amplitude_fixedLat ~ all_groups, data = sub2, na.action=na.exclude)
t.test(Amplitude_fixedLat ~ all_groups, data = sub3, na.action=na.exclude)

NSvsSL<-subset(sub_OlderGroups, Condition == "Nonspeech" | Condition == "Speechlike")
NSvsSp<-subset(sub_OlderGroups, Condition == "Nonspeech" | Condition == "Speech")
SLvsSp<-subset(sub_OlderGroups, Condition == "Speechlike" | Condition == "Speech")
t.test(Amplitude_fixedLat~Condition, data = NSvsSL, na.action=na.exclude)
t.test(Amplitude_fixedLat~Condition, data = NSvsSp, na.action=na.exclude)
t.test(Amplitude_fixedLat~Condition, data = SLvsSp, na.action=na.exclude)

# Get boxplot of amplitude according to Group
d<-subset(d, all_groups!= "Young_CA")
p10<-ggplot(data = d) + 
  geom_boxplot(aes(x = all_groups, y = Amplitude_fixedLat, fill = all_groups), lwd = 1, position = position_dodge(1), na.rm = TRUE) +
  geom_point(aes(x = all_groups, y = Amplitude_fixedLat), position = position_dodge(1), size= 2, alpha = 0.6, na.rm = TRUE)
p10<-p10 + 
  scale_y_continuous(name = "Amplitude (uV)") +
  scale_x_discrete(name = "Group",limits = c("Young_MM", "FollowUP_MM", "Older_MM", "Older_CA"), labels = c("MM-Y", "MM-YO", "MM-O", "CA-O"))+
  scale_fill_manual(values = c("#E69F00","#56B4E9", "#E69F00","#E69F00"))
p10<-p10+theme_minimal()
p10<-p10 + theme(plot.title = element_text(size = 20, family = "Helvetica", face = "bold"), 
                 text = element_text(size = 20, family = "Helvetica", face = "bold"), 
                 axis.title = element_text(face = "bold"), 
                 axis.text.x = element_text(size = 20, face = "bold"), 
                 panel.border = element_blank(), legend.position ="none")
p10