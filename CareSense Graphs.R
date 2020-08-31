###This set of graphs will look at the percent of patients that rated each clinic as excellent
###for every month plotted against the attendance at the clinic. This set of graphs can be used
###to determine if there is a relationship between the attendane and how well the clinic was rated 

###Set working directory
setwd("/Users/kerryregan/Desktop/CareSense Data")
getwd()

###Read in libraries
library(readr)
library(ggplot2)

###Read in csv file for data
SSatis<-read.csv("Sandy2019Satisfaction - Sheet1-2.csv")

###Check to see if it read in properly
SSatis

###plot the data using the basic plot function. Colors can be changed at col = "blue". This code also creates
###a legend so you can tell the difference between the two lines. Make sure that if the director is color
###blind that you change the colors.
plot(SSatis$Month, SSatis$Percent.Excellent, ylab = "Percent Excellent",
     main = "Sandy Overall Satisfaction", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(SSatis$Month, SSatis$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("topright", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))


###This plot looks at if satisfaction and the  patient visit numbers correlate
plot(SSatis$Percent.Excellent, SSatis$Attendance, ylab = "Patient Visits", xlab = "Overall Satisfaction", 
     main = " Sandy Satisfaction vs. Attendance")

###abline draws a line that fits the linear model for the plot above
abline(lm(SSatis$Attendance~SSatis$Percent.Excellent))

###This allows you to see the results from the linear model
Sandy<-lm(SSatis$Attendance~SSatis$Percent.Excellent)
summary(Sandy)


###Runs same code as above for South Jordan Clinic
SJSatis<-read.csv("SouthJordan2019Satisfaction - Sheet1.csv")

plot(SJSatis$Month, SJSatis$Percent.Excellent, ylab = "Percent Excellent",
     main = "South Jordan Overall Satisfaction", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(SJSatis$Month, SJSatis$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("bottomleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

plot(SJSatis$Percent.Excellent, SJSatis$Attendance, ylab = "Attendance", xlab = "Overall Satisfaction", 
     main = " South Jordan Attendance vs. Satisfaction")

abline(lm(SJSatis$Attendance~SJSatis$Percent.Excellent))


###Runs same code as above for West Jordan clinic
WJSatis<-read.csv("WestJordan2019Satisfaction - Sheet1-2.csv")

plot(WJSatis$Month, WJSatis$Percent.Excellent, ylab = "Percent Excellent",
     main = "West Jordan Overall Satisfaction", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(WJSatis$Month, WJSatis$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("topright", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

plot(WJSatis$Percent.Excellent, WJSatis$Attendance, ylab = "Attendance", xlab = "Overall Satisfaction", 
     main = " West Jordan Attendance vs. Satisfaction")

abline(lm(WJSatis$Attendance~WJSatis$Percent.Excellent))

plot(WJSatis$Month, WJSatis$Percent.Excellent, lty=1)


attach(WJSatis)
plot(as.vector(Month[1:12]), Percent.Excellent[1:12], pch=1)

Month[2]



###This plots percent rated excellent for all clinics for each month. We did this to provide some
###information for the directors in case they suspect something was happening during those months. 
###We also wanted to see if all clinics showed any trends. 

AllPE=read.csv("PercentExcellent2019All - Sheet1.csv")
plot(subset(AllPE$Month, AllPE$Clinic=="CWH"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="CWH"), col="red", type="line", lty="dotted", lwd=4, ylim=c(70,100), main = "Percent Excellent Overall Experience 2019", ylab = "Percent Excellent", xlab = "Month")
lines(subset(AllPE$Month, AllPE$Clinic=="Draper"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="Draper"), col="blue", type="line", lwd=4, lty="solid")
lines(subset(AllPE$Month, AllPE$Clinic=="Murray"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="Murray"), col="yellow", type="line", lwd=4, lty="solid")
lines(subset(AllPE$Month, AllPE$Clinic=="Sandy"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="Sandy"), col="green", type="line", lwd=4, lty="solid")
lines(subset(AllPE$Month, AllPE$Clinic=="SS"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="SS"), col=261, type="line", lwd=4, lty="solid")
lines(subset(AllPE$Month, AllPE$Clinic=="SB"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="SB"), col="black", type="line", lwd=4, lty="solid")
lines(subset(AllPE$Month, AllPE$Clinic=="SJ"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="SJ"), col=526, type="line", lwd=4, lty="solid")
lines(subset(AllPE$Month, AllPE$Clinic=="WJ"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="WJ"), col=152, type="line", lwd=4, lty="solid")
lines(subset(AllPE$Month, AllPE$Clinic=="Riverton"), subset(AllPE$Percent.Excellent, AllPE$Clinic=="Riverton"), col="purple", type="line", lwd=4, lty="solid")
legend(1,88, legend = c("CWH", "Draper", "Murray", "Riverton", "Sandy", "SS", "SB", "SJ", "WJ"), col=c("red", "blue", "yellow", "purple", "green", 261, "black", 526, 152), lty = c("dotted", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid"), lwd=4)





#These Graphs are for individual clinics but will look at Knowledge of the therapists for Director's meeting
CWHKnow<-read.csv("CWH2019Knowledge - Sheet1.csv")

plot(CWHKnow$Month, CWHKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "CWH Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l", lty="dotted")
par(new = TRUE)
plot(CWHKnow$Month, CWHKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("bottomleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(2, 1))


DraperKnow<-read.csv("Draper2019Knowledge - Sheet1.csv")

plot(DraperKnow$Month, DraperKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "Draper Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(DraperKnow$Month, DraperKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("bottomleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

MurrayKnow<-read.csv("Murray2019Knowledge - Sheet1.csv")

plot(MurrayKnow$Month, MurrayKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "Murray Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(MurrayKnow$Month, MurrayKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("topleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

RivertonKnow<-read.csv("Riverton2019Knowledge - Sheet1.csv")

plot(RivertonKnow$Month, RivertonKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "Riverton Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(RivertonKnow$Month, RivertonKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("bottomleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

SandyKnow<-read.csv("Sandy2019Knowledge - Sheet1.csv")

plot(SandyKnow$Month, SandyKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "Sandy Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(SandyKnow$Month, SandyKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("bottomleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

SSKnow<-read.csv("SS2019Knowledge - Sheet1.csv")

plot(SSKnow$Month, SSKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "SS Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(SSKnow$Month, SSKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("topleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

SBKnow<-read.csv("SB2019Knowledge - Sheet1.csv")

plot(SBKnow$Month, SBKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "SB Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(SBKnow$Month, SBKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("bottomleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

SJKnow<-read.csv("SJ2019Knowledge - Sheet1.csv")

plot(SJKnow$Month, SJKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "SJ Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(SJKnow$Month, SJKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("bottomleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

WJKnow<-read.csv("WJ2019Knowledge - Sheet1.csv")

plot(WJKnow$Month, WJKnow$Percent.Excellent, ylab = "Percent Excellent",
     main = "WJ Knowledge of the Therapist", xlab = "Month", col = "blue", type = "l")
par(new = TRUE)
plot(WJKnow$Month, WJKnow$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1, type = "l")
axis(side = 4)
mtext("", side = 4, line = 3)
legend("bottomleft", c("Percent Excellent", "Attendance"),
       col = c("blue", "red"), lty = c(1, 1))

