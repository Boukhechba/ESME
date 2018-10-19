library(ggplot2)
library(nlme)
library(pan)

ppg <- read.csv("C:/Users/mob3f/Documents/Python Scripts/data/ESME-clean/Smartwatch_HeartRate.csv")

ppg$day <-substr(ppg$Timestamp, 9, 10)
ppg$hour <-substr(ppg$Timestamp, 12, 13)
ppg$minute <-substr(ppg$Timestamp, 15, 16)
ppg$second <-substr(ppg$Timestamp, 18, 19)
sink()

ppg$DeviceId <- gsub("1f1160b6b06c680e", "7", ppg$DeviceId)
ppg$DeviceId <- gsub("3f24e81eb47d4cf0", "13", ppg$DeviceId)
ppg$DeviceId <- gsub("5f032c11219f031c", "5", ppg$DeviceId)
ppg$DeviceId <- gsub("6d4e6abb10a884bf", "8", ppg$DeviceId)
ppg$DeviceId <- gsub("6fb74eec82575fcf", "2", ppg$DeviceId)
ppg$DeviceId <- gsub("92fa687c4f0e7254", "6", ppg$DeviceId)
ppg$DeviceId <- gsub("b153ad6b92b13c20", "11", ppg$DeviceId)
ppg$DeviceId <- gsub("bb709ae6879f89e4", "12", ppg$DeviceId)
ppg$DeviceId <- gsub("c6b48865bc00d278", "1", ppg$DeviceId)
ppg$DeviceId <- gsub("d9075f3751fe3f60", "10", ppg$DeviceId)
ppg$DeviceId <- gsub("eadcd47e37140e83", "3", ppg$DeviceId)
ppg$DeviceId <- gsub("f48eede2f24a01ec", "4", ppg$DeviceId)
ppg$DeviceId <- gsub("f4beafed9563ddde", "9", ppg$DeviceId)

ppg$DeviceId <- as.numeric(ppg$DeviceId)
ppg$day <- as.numeric(ppg$day)

ppg <- subset(ppg, day==20 | day ==21)
ppg <- subset(ppg, DeviceId < 13)
ppg <- subset(ppg, DeviceId != 6)
ppg <- subset(ppg, DeviceId != 7)
ppg$ZHR <- ave(ppg$HR, ppg$DeviceId, FUN=scale)

ppg$expday <- ifelse(ppg$day==20,1,2)
ppg$condition<-ifelse(ppg$expday== 1,ifelse(ppg$DeviceId<=6,'grey','green'),ifelse(ppg$DeviceId<=6,'green','grey'))
ppg$groupe <-ifelse(ppg$DeviceId<=6,'groupe 1', 'groupe 2')
ppg$combin <- paste (ppg$groupe,ppg$condition)

ppg <- subset(ppg, DeviceId != 10)
ppg <- subset(ppg, DeviceId != 11)

g <- ggplot(ppg, aes(condition, ZHR))
g + geom_boxplot()  +
  theme(text = element_text(size=20))

y<-aggregate(HR~DeviceId+condition,data=ppg,FUN=length)
summary(lme(HR ~ condition , random = ~1|DeviceId,control=ctrl, data=ppg, method="ML"))

ppg <- read.csv("C:/Users/mob3f/Documents/ESME/results/Clean_Data/Smartwatch_Sound.csv")

ppg$day <-substr(ppg$Timestamp, 9, 10)
ppg$hour <-substr(ppg$Timestamp, 12, 13)
ppg$minute <-substr(ppg$Timestamp, 15, 16)
ppg$second <-substr(ppg$Timestamp, 18, 19)

ppg$DeviceId <- gsub("1f1160b6b06c680e", "7", ppg$DeviceId)
ppg$DeviceId <- gsub("3f24e81eb47d4cf0", "13", ppg$DeviceId)
ppg$DeviceId <- gsub("5f032c11219f031c", "5", ppg$DeviceId)
ppg$DeviceId <- gsub("6d4e6abb10a884bf", "8", ppg$DeviceId)
ppg$DeviceId <- gsub("6fb74eec82575fcf", "2", ppg$DeviceId)
ppg$DeviceId <- gsub("92fa687c4f0e7254", "6", ppg$DeviceId)
ppg$DeviceId <- gsub("b153ad6b92b13c20", "11", ppg$DeviceId)
ppg$DeviceId <- gsub("bb709ae6879f89e4", "12", ppg$DeviceId)
ppg$DeviceId <- gsub("c6b48865bc00d278", "1", ppg$DeviceId)
ppg$DeviceId <- gsub("d9075f3751fe3f60", "10", ppg$DeviceId)
ppg$DeviceId <- gsub("eadcd47e37140e83", "3", ppg$DeviceId)
ppg$DeviceId <- gsub("f48eede2f24a01ec", "4", ppg$DeviceId)
ppg$DeviceId <- gsub("f4beafed9563ddde", "9", ppg$DeviceId)

ppg$DeviceId <- as.numeric(ppg$DeviceId)
ppg$day <- as.numeric(ppg$day)
ppg <- subset(ppg, day==20 | day ==21)
ppg <- subset(ppg, DeviceId < 13)
ppg <- subset(ppg, DeviceId != 6)
ppg <- subset(ppg, DeviceId != 7)

ppg$expday <- ifelse(ppg$day==20,1,2)
ppg$condition<-ifelse(ppg$expday== 1,ifelse(ppg$DeviceId<=6,'grey','green'),ifelse(ppg$DeviceId<=6,'green','grey'))
ppg$groupe <-ifelse(ppg$DeviceId<=6,'groupe 1', 'groupe 2')
ppg$combin <- paste (ppg$groupe,ppg$condition)

g <- ggplot(ppg, aes(condition, Sound))
g + geom_boxplot()  +
  theme(text = element_text(size=20))

y<-aggregate(HR~DeviceId+condition,data=ppg,FUN=length)
summary(lme(Sound ~ condition , random = ~1|DeviceId,control=ctrl, data=ppg, method="ML"))

ppg <- read.csv("C:/Users/mob3f/Documents/ESME/results/Clean_Data/Smartwatch_Light.csv")

ppg$day <-substr(ppg$Timestamp, 9, 10)
ppg$hour <-substr(ppg$Timestamp, 12, 13)
ppg$minute <-substr(ppg$Timestamp, 15, 16)
ppg$second <-substr(ppg$Timestamp, 18, 19)

ppg$DeviceId <- gsub("1f1160b6b06c680e", "7", ppg$DeviceId)
ppg$DeviceId <- gsub("3f24e81eb47d4cf0", "13", ppg$DeviceId)
ppg$DeviceId <- gsub("5f032c11219f031c", "5", ppg$DeviceId)
ppg$DeviceId <- gsub("6d4e6abb10a884bf", "8", ppg$DeviceId)
ppg$DeviceId <- gsub("6fb74eec82575fcf", "2", ppg$DeviceId)
ppg$DeviceId <- gsub("92fa687c4f0e7254", "6", ppg$DeviceId)
ppg$DeviceId <- gsub("b153ad6b92b13c20", "11", ppg$DeviceId)
ppg$DeviceId <- gsub("bb709ae6879f89e4", "12", ppg$DeviceId)
ppg$DeviceId <- gsub("c6b48865bc00d278", "1", ppg$DeviceId)
ppg$DeviceId <- gsub("d9075f3751fe3f60", "10", ppg$DeviceId)
ppg$DeviceId <- gsub("eadcd47e37140e83", "3", ppg$DeviceId)
ppg$DeviceId <- gsub("f48eede2f24a01ec", "4", ppg$DeviceId)
ppg$DeviceId <- gsub("f4beafed9563ddde", "9", ppg$DeviceId)

ppg$DeviceId <- as.numeric(ppg$DeviceId)
ppg$day <- as.numeric(ppg$day)
ppg <- subset(ppg, day==20 | day ==21)
ppg <- subset(ppg, DeviceId < 13)
ppg <- subset(ppg, DeviceId != 6)
ppg <- subset(ppg, DeviceId != 7)

ppg$expday <- ifelse(ppg$day==20,1,2)
ppg$condition<-ifelse(ppg$expday== 1,ifelse(ppg$DeviceId<=6,'grey','green'),ifelse(ppg$DeviceId<=6,'green','grey'))
ppg$groupe <-ifelse(ppg$DeviceId<=6,'groupe 1', 'groupe 2')
ppg$combin <- paste (ppg$groupe,ppg$condition)


g <- ggplot(ppg, aes(condition, Light))
g + geom_boxplot()  +
  theme(text = element_text(size=20))+
  coord_cartesian(ylim = c(0, 2000))

y<-aggregate(Light~DeviceId+condition,data=ppg,FUN=length)
summary(lme(Light ~ condition , random = ~1|DeviceId,control=ctrl, data=ppg, method="ML"))

ppg <- read.csv("C:/Users/mob3f/Documents/ESME/results/Clean_Data/Smartwatch_Accelerometer.csv")

ppg$day <-substr(ppg$Timestamp, 9, 10)
ppg$hour <-substr(ppg$Timestamp, 12, 13)
ppg$minute <-substr(ppg$Timestamp, 15, 16)
ppg$second <-substr(ppg$Timestamp, 18, 19)

ppg$DeviceId <- gsub("1f1160b6b06c680e", "7", ppg$DeviceId)
ppg$DeviceId <- gsub("3f24e81eb47d4cf0", "13", ppg$DeviceId)
ppg$DeviceId <- gsub("5f032c11219f031c", "5", ppg$DeviceId)
ppg$DeviceId <- gsub("6d4e6abb10a884bf", "8", ppg$DeviceId)
ppg$DeviceId <- gsub("6fb74eec82575fcf", "2", ppg$DeviceId)
ppg$DeviceId <- gsub("92fa687c4f0e7254", "6", ppg$DeviceId)
ppg$DeviceId <- gsub("b153ad6b92b13c20", "11", ppg$DeviceId)
ppg$DeviceId <- gsub("bb709ae6879f89e4", "12", ppg$DeviceId)
ppg$DeviceId <- gsub("c6b48865bc00d278", "1", ppg$DeviceId)
ppg$DeviceId <- gsub("d9075f3751fe3f60", "10", ppg$DeviceId)
ppg$DeviceId <- gsub("eadcd47e37140e83", "3", ppg$DeviceId)
ppg$DeviceId <- gsub("f48eede2f24a01ec", "4", ppg$DeviceId)
ppg$DeviceId <- gsub("f4beafed9563ddde", "9", ppg$DeviceId)

ppg$DeviceId <- as.numeric(ppg$DeviceId)
ppg$day <- as.numeric(ppg$day)
ppg <- subset(ppg, day==20 | day ==21)
ppg <- subset(ppg, DeviceId < 13)
ppg <- subset(ppg, DeviceId != 6)
ppg <- subset(ppg, DeviceId != 7)

ppg$expday <- ifelse(ppg$day==20,1,2)
ppg$condition<-ifelse(ppg$expday== 1,ifelse(ppg$DeviceId<=6,'grey','green'),ifelse(ppg$DeviceId<=6,'green','grey'))
ppg$groupe <-ifelse(ppg$DeviceId<=6,'groupe 1', 'groupe 2')
ppg$combin <- paste (ppg$groupe,ppg$condition)

ppg$acceleration <-sqrt(ppg$X^2+ppg$Y^2+ppg$Z^2)


y<-aggregate(acceleration~DeviceId+condition+day+hour+minute,data=ppg,FUN=mean)

g <- ggplot(y, aes(condition, acceleration))
g + geom_boxplot()  +
  theme(text = element_text(size=20))+
  coord_cartesian(ylim = c(5, 15))

summary(lme(acceleration ~ condition , random = ~1|DeviceId,control=ctrl, data=y, method="ML"))


