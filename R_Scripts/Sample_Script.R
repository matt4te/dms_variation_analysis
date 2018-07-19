## THIS SCRIPT ANALYZES SAMPLE DATA FROM A CALIBRATED VG-CL DEVICE  ------

# you should start in a folder that contains all your data for the day, including:
  # a 'calib' folder with a "Calibration_Values.csv" file 
  # numbered folders containing several 'samples' from each water sample (see example names below)

# this code outputs a "concentration.csv" file which contains your sample concentration

## Set up shop -------------

dir <- "C:/Users/mattf/Desktop/dms_variation_data"

# load required libraries
library("stringr")

# enter the details regarding your trial

month <- "march"
trial <- 3

# set working directory to folder with the calibration runs
dirC <- str_c(dir,"/",month,"_2017/trial",trial,"/calib",sep="")

# load the data from the calibraiton
calibration <- read.csv(str_c(dirC,"/Calibration_Values.csv"))

## Then begin processing all the samples for the day ------------

# how many samples this day?
n <- 4

# now loop through all of the samples !

for (i in 1:n) {
  
diri <- str_c(dir,"/",month,"_2017/trial",trial,"/sample",i,sep="")

# load the sample data
setwd(diri)
s1 <- read.csv("1.csv",skip=7)
s2 <- read.csv("2.csv",skip=7)

# # if you need to inspect the samples, run:
plot(s1$CHANNEL0)
plot(s2$CHANNEL0)

## Isolate the average spike in voltage-------------

# determine the baselines for sample signals

bases1 <- mean(s1[1:25,c("CHANNEL0")])
bases2 <- mean(s2[1:25,c("CHANNEL0")])

# and the difference between peak and baselines

Peaks1 <- max(s1$CHANNEL0) - bases1
Peaks2 <- max(s2$CHANNEL0) - bases2

maxV <- (Peaks1 + Peaks2) / 2


# # and for the high channel
# bases1h <- mean(s1[1:25,c("CHANNEL1")])
# bases2h <- mean(s2[1:25,c("CHANNEL1")])
# 
# Peaks1h <- max(s1$CHANNEL1) - bases1h
# Peaks2h <- max(s2$CHANNEL1) - bases2h
# 
# maxVh <- (Peaks1h + Peaks2h) / 2


## Determine sample [DMS] from the calibration parameters -----------

conc <- (maxV * calibration[1,2]) + calibration[1,1]

# conch <- (maxVh * calibration[2,2]) + calibration[2,1]
# conch

conch <- NA

## Save the sample results ----------
concentration <- as.data.frame(rbind(conc,conch))
colnames(concentration) <- c("concentration")

write.csv(concentration,"concentration.csv")

}

## Some plots for a publication --------


ggplot(data=s1, aes(x=Sample.Number,y=CHANNEL0)) + geom_path(size=1) +
  theme_bw() + xlab("Time") + ylab("Voltage")
