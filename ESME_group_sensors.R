library(ggplot2)
library(nlme)

hrv <- read.csv("C:/Users/mehdi/Documents/ESME/results/Clean_Data/Smartwatch_HRV_min.csv")
gps <- read.csv("C:/Users/mehdi/Documents/ESME/results/Clean_Data/Location_clean.csv")
sound <- read.csv("C:/Users/mehdi/Documents/ESME/results/Clean_Data/Sound_clean.csv")
light <- read.csv("C:/Users/mehdi/Documents/ESME/results/Clean_Data/Light_clean.csv")

light_aggr<-aggregate(Light~participantid+condition+day+hour+minute,data=light,FUN=mean)

sound_aggr<-aggregate(Sound~participantid+condition+day+hour+minute,data=sound,FUN=mean)

agr_hrv_sound<-merge(hrv,sound_aggr,by=c("participantid","condition","day","hour","minute"))
agr_hrv_light<-merge(hrv,light_aggr,by=c("participantid","condition","day","hour","minute"))
agr_hrv_location<-merge(hrv,gps,by=c("participantid","condition","day","hour","minute"))
agr_sound_location<-merge(sound_aggr,gps,by=c("participantid","condition","day","hour","minute"))
agr_light_location<-merge(light_aggr,gps,by=c("participantid","condition","day","hour","minute"))

write.csv(na.omit(agr_hrv_location), "C:/Users/mehdi/Documents/ESME/results/Clean_Data/location_vs_hrv.csv")
write.csv(na.omit(agr_sound_location), "C:/Users/mehdi/Documents/ESME/results/Clean_Data/location_vs_sound.csv")
write.csv(na.omit(agr_light_location), "C:/Users/mehdi/Documents/ESME/results/Clean_Data/location_vs_light.csv")


agr_hrv_light<-subset(agr_hrv_light, Light <10000)

ctrl<- lmeControl(opt='optim')
summary(lme(rmssd ~ Sound , random = ~1|participantid,control=ctrl, data=agr_hrv_sound,na.action=na.omit, method="ML"))
summary(lme(rmssd ~ Light, random = ~1|participantid,control=ctrl, data=agr_hrv_light,na.action=na.omit, method="ML"))


ggplot(subset(agr_hrv_light, Light <10000), aes(x=Light, y=sdnn)) + 
  geom_point() +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

ggplot(agr_hrv_sound, aes(x=Sound, y=rmssd)) + 
  geom_point() +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)
