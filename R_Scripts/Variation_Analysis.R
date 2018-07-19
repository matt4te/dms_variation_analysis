## THIS SCRIPT IS USED TO ANALYE [DMS] TEMPORAL VARIATION----


# you should begin in a folder for the project containing:
  # some type of data log (e.g., "DMS_Sampling_Log.csv")
  # dated folders (e.g., YYYY-MM-DD) containing numbered sample folders


## Set up shop ---------------

dir <- "C:/Users/mattf/Desktop/RSMAS/dms_variation_data"

# load required libraries
library("stringr")
library("ggplot2")
library("plyr")
library("dplyr")
library("quantreg")

# read your data log
log1 <- read.csv(str_c(dir,"october_2016","dms_sampling_log.csv",sep="/"),stringsAsFactors=FALSE)
log2 <- read.csv(str_c(dir,"november_2016","dms_sampling_log.csv",sep="/"),stringsAsFactors=FALSE)
log2$trial_id <- log2$trial_id + 6
log3 <- read.csv(str_c(dir,"february_2017","dms_sampling_log.csv",sep="/"),stringsAsFactors=FALSE)
colnames(log3)[1] <- "trial_id"
log4 <- read.csv(str_c(dir,"march_2017","dms_sampling_log.csv",sep="/"),stringsAsFactors=FALSE)
colnames(log4)[1] <- "trial_id"
log4 <- log4[!is.na(log4$trial_id),]

# paste it together
log <- rbind(log1,log2,log3,log4)

# some cleaning up
log$trial_id <- as.integer(log$trial_id)
log$tide <- as.factor(log$tide)
log$light <- as.factor(log$light)
log$richness <- as.numeric(log$richness)
log$abundance <- as.numeric(log$abundance)
log$datetime <- as.POSIXct(str_c(log$date," ",log$time,sep=""),format="%m/%d/%Y %H:%M")
log$dms <- log$X.dms.
log$sample <- log$sample.
log <- select(log,datetime,time,trial_id,sample,tide,light,dms,biological,richness, abundance,notes)


## Plot some stuff--------------
logS <- ddply(log, .(tide,light), summarise, N= length(dms), avg = mean(dms), sd = sd(dms), se= sd(dms)/sqrt(length(dms)))

logL <- ddply(log, .(light), summarise, N= length(dms), avg = mean(dms), sd = sd(dms), se= sd(dms)/sqrt(length(dms)))

logT <- ddply(log, .(tide), summarise, N= length(dms), avg = mean(dms), sd = sd(dms), se= sd(dms)/sqrt(length(dms)))

# all data boxplot
qplot(datetime, dms, data = log, geom = "boxplot") +
  xlab("") + ylab("[DMS] (nm)") +   theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank(),
                                          axis.ticks.x=element_blank())

# tide boxplot
ggplot(data=log, aes(y=dms, x=tide)) + 
  geom_boxplot() + ylab("[DMS] (nm)") + xlab("Tidal Cycle") +
  theme_bw()

t.test(log[log$tide=="ebb",c("dms")],log[log$tide=="flood",c("dms")])


# light boxplot
ggplot(data=log, aes(y=dms, x=light)) + 
  geom_boxplot() + ylab("[DMS] (nm)") + xlab("Time of Day") +
  theme_bw()

t.test(log[log$light=="night",c("dms")],log[log$light=="day",c("dms")])



# time points
log$time <- as.POSIXct(str_c("2016-11-01"," ",log$time))

ggplot(data=log, aes(y=dms,x=time)) +
  geom_point() + geom_smooth() 

# datetime points
ggplot(data=log, aes(y=dms,x=datetime)) +
  geom_point() + geom_smooth()

# only full cycle points
logn1 <- subset(log, trial_id==6)

ggplot(data=logn1, aes(y=dms,x=datetime)) +
  geom_point() + geom_smooth() +
  geom_vline(aes(xintercept=as.numeric(logn1$datetime[c(7)])),
             linetype=2, colour="red")





## Some statistics---------------
anova1 <- aov(data=log, dms ~ tide)
anova(anova1)





## Plot concentrations relative to richness/abundance-----------


logB <- log[log$biological == "TRUE",]


ggplot(logB, aes(x=dms,y=richness)) + geom_point() + geom_smooth(method="lm") +
  xlab("[DMS] (nm)") + ylab("Species Richness")

richM <- lm(richness~dms,data=logB)
summary(richM)



ggplot(logB, aes(x=dms,y=abundance)) + geom_point() + geom_smooth(method="lm") +
  xlab("[DMS] (nm)") + ylab("Larval Abundance") 

richA <- lm(abundance~dms+trial_id,data=logB)
summary(richA)




## Does [dms] predict 0 catches? ---------------

logB$catch <- "TRUE"
logB[logB$abundance == 0, c("catch")] <- "FALSE"
logB$catch <- as.logical(logB$catch)

nrow(logB[logB$catch=="TRUE",])
mean(logB[logB$catch=="TRUE",c("dms")])
nrow(logB[logB$catch=="FALSE",])
mean(logB[logB$catch=="FALSE",c("dms")])

logitFit <- glm(catch ~ dms+trial_id, data=logB, family="binomial")
summary(logitFit)


## Regression excluding 0 catches -------------

logC <- subset(logB, catch=="TRUE")

ggplot(logC, aes(x=dms,y=richness)) + geom_point() + geom_smooth(method="lm") +
  xlab("[DMS] (nm)") + ylab("Species Richness")

richMc <- lm(richness~dms+trial_id,data=logC)
summary(richMc)

ggplot(logC, aes(x=dms,y=abundance)) + geom_point() + geom_smooth(method="lm") +
  xlab("[DMS] (nm)") + ylab("Larval Abundance") 

richAc <- lm(abundance~dms,data=logC)
summary(richAc)


## Quantile Regression --------


# for richness

QRr20 <- rq(richness ~ dms, tau=0.2,data=logB)
summary(QRr20)

QRr80 <- rq(richness ~ dms, tau=0.8,data=logB)
summary(QRr80)

anova(QRr20,QRr80)

QRrAll <- rq(richness ~ dms, tau=seq(.3,.8,by=.2),data=logB)
summary(QRrAll, se="ker")

plot(summary(QRrAll))

ggplot(logC,aes(x=dms,y=richness)) + geom_point() +
  geom_quantile(quantiles=c(0.2),aes(color="red"),size=1) +
  geom_quantile(quantiles=c(0.4),aes(color="blue"),size=1) +
  geom_quantile(quantiles=c(0.6),aes(color="orange"),size=1) +
  geom_quantile(quantiles=c(0.8),aes(color="green"),size=1) +
  scale_color_manual(name="Quantiles",breaks=c("red","blue","orange","green"),
                     values=c("red","blue","orange","green"),
                     labels=c("20%","40%","60%","80%")) +
  xlab("[DMS] (nm)") + ylab("Larval Richness") +
  theme_bw() + coord_fixed()


# for abundance

QRa20 <- rq(abundance ~ dms, tau=0.2,data=logB)
summary(QRa20)

QRa80 <- rq(abundance ~ dms, tau=0.8,data=logB)
summary(QRa80)

anova(QRa20,QRa80)

QRaAll <- rq(abundance ~ dms, tau=seq(0.2,0.8,by=.2),data=logB)
summary(QRaAll, se="ker")

plot(summary(QRaAll))

q10 <- seq(.1,.9,by=.1)

ggplot(logC,aes(x=dms,y=abundance)) + geom_point() +
  geom_quantile(quantiles=c(0.2),aes(color="red"),size=1) +
  geom_quantile(quantiles=c(0.4),aes(color="blue"),size=1) +
  geom_quantile(quantiles=c(0.6),aes(color="orange"),size=1) +
  geom_quantile(quantiles=c(.8),aes(color="green"),size=1) +
  geom_quantile(quantiles=c(.9),aes(color="purple"),size=1) +
  scale_color_manual(name="Quantiles",breaks=c("red","blue","orange","green","purple"),
                     values=c("red","blue","orange","green","purple"),
                     labels=c("20%","40%","60%","80%","90%")) +
  xlab("[DMS] (nm)") + ylab("Larval Abundance") +
  theme_bw() + coord_fixed() +
  theme(axis.text = element_text(size=14))





