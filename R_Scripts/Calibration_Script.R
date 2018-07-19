## THIS SCRIPT IS USED TO CALIBRATE THE VG-CL DEVICE FROM STANDARD SOLUTIONS -------


# you should start in a folder that contains calibration data, including:
  # a 'calib' folder with several samples at several concentrations (from 10->1 nM)
  # see example file names below for 3 samples at 10,5, and 2 nM

# the code outputs a "Calibration_Values.csv" file, containing your calibration parameters

## Set up shop ---------

dir<- "C:/Users/mattf/Desktop/dms_variation_data"

# load libraries you need
library("stringr")
library("ggplot2")

# enter the details regarding your calibration 

month <- "march"
trial <- 2

# set working directory to folder with the calibration runs
dir2 <- str_c(dir,"/",month,"_2017/trial",trial,"/calib",sep="")

## load all of the calibration files

setwd(str_c(dir2,"/stock10"))
tenNM1 <- read.csv('1.csv',skip=7)
tenNM2 <- read.csv('2.csv',skip=7)

setwd(str_c(dir2,"/stock5"))
fiveNM1 <- read.csv('1.csv',skip=7)
fiveNM2 <- read.csv('2.csv',skip=7)

setwd(str_c(dir2,"/stock2"))
twoNM1 <- read.csv('1.csv',skip=7)
twoNM2 <- read.csv('2.csv',skip=7)


## Inspect the data visually -------------

plot(tenNM1$CHANNEL0)
plot(tenNM2$CHANNEL0)

# plot(tenNM1$CHANNEL1)
# plot(tenNM2$CHANNEL1)

plot(fiveNM1$CHANNEL0)
plot(fiveNM2$CHANNEL0)

# plot(fiveNM1$CHANNEL1)
# plot(fiveNM2$CHANNEL1)

plot(twoNM1$CHANNEL0)
plot(twoNM2$CHANNEL0)

# plot(twoNM1$CHANNEL1)
# plot(twoNM2$CHANNEL1)

# if nothing is odd, continue. start by finding baseline of each sample


## Isolate the signal from each calibration sample -----------

# First, find the signal baseline for each 

base10NM1 <- mean(tenNM1[1:25,c("CHANNEL0")])
base10NM2 <- mean(tenNM2[1:25,c("CHANNEL0")])

base5NM1 <- mean(fiveNM1[1:25,c("CHANNEL0")])
base5NM2 <- mean(fiveNM2[1:25,c("CHANNEL0")])

base2NM1 <- mean(twoNM1[1:25,c("CHANNEL0")])
base2NM2 <- mean(twoNM2[1:25,c("CHANNEL0")])



# Then, find the difference between the peak and the baseline

tenPeak1 <- max(tenNM1$CHANNEL0) - base10NM1
tenPeak2 <- max(tenNM2$CHANNEL0) - base10NM2

fivePeak1 <- max(fiveNM1$CHANNEL0) - base5NM1
fivePeak2 <- max(fiveNM2$CHANNEL0) - base5NM2

twoPeak1 <- max(twoNM1$CHANNEL0) - base2NM1
twoPeak2 <- max(twoNM2$CHANNEL0) - base2NM2


# and take the average of each replicate

tenPeak <- (tenPeak1 + tenPeak2)/2
fivePeak <- (fivePeak1 + fivePeak2)/2
twoPeak <- (twoPeak1 + twoPeak2)/2

## Extract the calibration parameters ------------

# create a matrix / data frame from your calibraiton 
mat <- matrix(data=c(10,5,2,tenPeak,fivePeak,twoPeak),nrow=3)
df <- data.frame(mat)
colnames(df) <- c("conc","volt")

# plot the calibration 

plot(df$conc ~ df$volt)

# fit the line and extract the intercept/slop

model <- lm(df$conc ~ df$volt)

yint <- model$coefficients[1]
slp <- model$coefficients[2]


# 
# ## Repeat procedure for the reference (high) channel ---------
# 
# 
# base10NM1h <- mean(tenNM1[1:25,c("CHANNEL1")])
# base10NM2h <- mean(tenNM2[1:25,c("CHANNEL1")])
# base5NM1h <- mean(fiveNM1[1:25,c("CHANNEL1")])
# base5NM2h <- mean(fiveNM2[1:25,c("CHANNEL1")])
# base2NM1h <- mean(twoNM1[1:25,c("CHANNEL1")])
# base2NM2h <- mean(twoNM2[1:25,c("CHANNEL1")])
# 
# 
# tenPeak1h <- max(tenNM1$CHANNEL1) - base10NM1h
# tenPeak2h <- max(tenNM2$CHANNEL1) - base10NM2h
# fivePeak1h <- max(fiveNM1$CHANNEL1) - base5NM1h
# fivePeak2h <- max(fiveNM2$CHANNEL1) - base5NM2h
# twoPeak1h <- max(twoNM1$CHANNEL1) - base2NM1h
# twoPeak2h <- max(twoNM2$CHANNEL1) - base2NM2h
# 
# 
# tenPeakh <- (tenPeak1h + tenPeak2h)/2
# fivePeakh <- (fivePeak1h + fivePeak2h)/2
# twoPeakh <- (twoPeak1h + twoPeak2h)/2
# 
# 
# math <- matrix(data=c(10,5,2,tenPeakh,fivePeakh,twoPeakh),nrow=3)
# dfh <- data.frame(math)
# colnames(dfh) <- c("conc","volt")
# 
# 
# plot(dfh$conc ~ dfh$volt)
# 
# 
# modelh <- lm(dfh$conc ~ dfh$volt)
# yinth <- modelh$coefficients[1]
# slph <- modelh$coefficients[2]

yinth <- NA
slph <- NA

## Save the calibration values for sample analysis ------------


calvals <- matrix(data=c(yint, yinth, slp,slph),nrow=2)
calvals <- data.frame(calvals)
colnames(calvals) <- c("intercept","slope")

setwd(dir2)
write.csv(calvals,"Calibration_Values.csv",row.names=FALSE)



## Some Nice Publication Plots ---------

# for individual peaks

plot(tenNM1$CHANNEL0) # basic version 

ggplot(data=tenNM1, aes(x=Sample.Number,y=CHANNEL0)) + geom_path(size=1) +
  theme_bw() + xlab("Time") + ylab("Voltage")


# for the calibraiton

plot(df$conc ~ df$volt) # basic

ggplot(data=df, aes(x=volt,y=conc)) + geom_point(size=4) + 
  geom_smooth(method="lm", se=FALSE, size=1) +
  theme_bw() + 
  xlab("Voltage") + ylab("DMS Concentration (nM)")



