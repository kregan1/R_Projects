##Working Directory
getwd()
setwd("/Users/kerryregan/Desktop/CareSense Data")
library(readr)


#Checking up on WJ Satisfaction for 2019
WJSatis<-read.csv("WestJordan2019Satisfaction - Sheet1.csv")

plot(WJSatis$Month, WJSatis$Percent.Excellent, ylab = "Percent Excellent",
     main = "West Jordan Overall Satisfaction", xlab = "Month", col = "blue")
par(new = TRUE)
plot(WJSatis$Month, WJSatis$Attendance, xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 1)
axis(side = 4)
mtext("", side = 4, line = 3)

legend("topleft", c("beaver1", "beaver2"),
       col = c("blue", "red"), lty = c(1, 2))
##################################################################



#Checking each clinic to see if they are hitting MCID within clinic average utalization
LEFSv1<-read.vsv("LEFS1vDiff - Sheet1.csv")

LEFS<-read.csv("ClinicLEFSDiff.csv", col_types(Clinic=col_character(), Visits=col_number(), Difference=col_number())

plot(subset(LEFS$Visits,LEFS$Clinic=="Sandy"), subset(LEFS$Difference,LEFS$Clinic=="Sandy"), ylim = c(-10,50), xlim = c(5,50), ylab = "Difference", xlab = "Visits"), main = "Difference in LEFS vs. Visit Number")

#Checking up on attendance VS phone - Did clinics have lower attendance when phones were unsatisfactory - clinics are not significantly different form each other
PhoneVAttendance<-read.csv("AttendancevsPhone.csv", col_types=cols(Clinic=col_character(), Attendance=col_number(), Phone=col_number()))

KnowledgeVSAttendance<-read_csv("AttendanceVsKnowledge.csv", col_types=cols(Clinic=col_character(), Attendance=col_number(), Knowledge=col_number()))

##Read in Knowledge of PT Data
KnowledgeOfPT<-read_csv("KnowledgeOfPTJanFeb - Sheet1.csv", col_types=cols(Clinic=col_character(), Rate=col_number()))
View(KnowledgeOfPT)

##Read in Knowledge of PT Data
PatientNum<-read_csv("PatientNumJanFebMean3 - Sheet1.csv", col_types=cols(Clinic=col_character(), Total=col_number(), WeeklyTherapist=col_number(), Mean=col_number()))
View(PatientNum)

PatientSatisBefore<-read_csv("PatientSatisfactionBefore - Sheet1.csv", col_types=cols(Clinic=col_character(), Rate=col_number()))

PatientSatisAfter<-read_csv("PatientSatisfactionAfter - Sheet1.csv", col_types=cols(Clinic=col_character(), Rate=col_number()))

##Read in Overall Experience##
OverallExperience<-read_csv("OverallExperience - Sheet1.csv", col_types=cols(Clinic=col_character(), Rate=col_number()))
View(OverallExperience)

##Read in Getting in by phone##
Phone<-read_csv("GettingByPhoneJanFeb - Sheet1.csv", col_types=cols(Clinic=col_character(), Rate=col_number()))
View(Phone)

##Read in Percentage vs Patient Satisfaction##
Attend<-read_csv("PercentagevsPatientSatisfaction - Sheet1.csv", col_types=cols(Clinic=col_character(), Rate=col_number()))
View(Phone)


##Set Variable
DataFile<-KnowledgeOfPT
DataFile<-OverallExperience
DataFile<-Phone
DataFile<-PatientSatisAfter

##Run ANOVA##
Init<-aov(DataFile$Rate~DataFile$Clinic)
Init
summary(Init)

##Get mean for each clinic##
CottonwoodMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="CWH"))
DraperMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="Draper"))
MurrayMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="Murray"))
RivertonMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="Riverton"))
SandyMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="Sandy"))
SaratogaMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="Saratoga"))
SBangeterMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="SB"))
SouthJMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="SJ"))
WestJMean<-mean(subset(DataFile$Rate, DataFile$Clinic=="WJ"))

##Data Vectors##

AllMeans<-c(CottonwoodMean, DraperMean, MurrayMean, RivertonMean, SandyMean, SaratogaMean, SouthJMean, WestJMean)
AllErrors<-c(CottonwoodError, DraperError, MurrayError, RivertonError, SandyError, SaratogaError, SouthJError, WestJError)
AllClinics<-c("CHW", "Draper", "Murray", "Riverton", "Sandy", "Saratoga", "SJ", "WJ")

##Get standard error for each clinic##
CottonwoodError<-sd(subset(DataFile$Rate, DataFile$Clinic=="CWH"))/sqrt(length(DataFile$Clinic=="CHW"))
DraperError<-sd(subset(DataFile$Rate, DataFile$Clinic=="Draper"))/sqrt(length(DataFile$Clinic=="Draper"))
MurrayError<-sd(subset(DataFile$Rate, DataFile$Clinic=="Murray"))/sqrt(length(DataFile$Clinic=="Murray"))
RivertonError<-sd(subset(DataFile$Rate, DataFile$Clinic=="Riverton"))/sqrt(length(DataFile$Clinic=="Riverton"))
SandyError<-sd(subset(DataFile$Rate, DataFile$Clinic=="Sandy"))/sqrt(length(DataFile$Clinic=="Sandy"))
SaratogaError<-sd(subset(DataFile$Rate, DataFile$Clinic=="Saratoga"))/sqrt(length(DataFile$Clinic=="Saratoga"))
SBangeterError<-sd(subset(DataFile$Rate, DataFile$Clinic=="SB"))/sqrt(length(DataFile$Clinic=="SB"))
SouthJError<-sd(subset(DataFile$Rate, DataFile$Clinic=="SJ"))/sqrt(length(DataFile$Clinic=="SJ"))
WestJError<-sd(subset(DataFile$Rate, DataFile$Clinic=="WJ"))/sqrt(length(DataFile$Clinic=="WJ"))

##Make a plot##

means <- c(CottonwoodMean, DraperMean, MurrayMean, RivertonMean, SandyMean, SaratogaMean, SBangeterMean, SouthJMean, WestJMean)
names <- c("CWH", "Draper", "Murray", "Riverton", "Sandy", "Saratoga","SB", "SJ", "WJ")
standardErrors <- c(CottonwoodError, DraperError, MurrayError, RivertonError, SandyError, SaratogaError, SBangeterError, SouthJError, WestJError)
plotTop <- max(means+standardErrors*2)
barCenters <- barplot(means, names.arg=names, col="gray", las=2, ylim=c(4.5, 5), main="Please Rate Ability to Get in By Phone")
segments(barCenters, means-standardErrors*2, barCenters, means+standardErrors*2, lwd=2)


means <- c(CottonwoodMean, DraperMean, MurrayMean, RivertonMean, SandyMean, SaratogaMean, SBangeterMean, SouthJMean, WestJMean)
names <- c("CWH", "Draper", "Murray", "Riverton", "Sandy", "Saratoga","SB", "SJ", "WJ")
standardErrors <- c(CottonwoodError, DraperError, MurrayError, RivertonError, SandyError, SaratogaError, SBangeterError, SouthJError, WestJError)
plotTop <- max(means+standardErrors*2)
barCenters <- barplot(means, names.arg=names, col="gray", las=2, ylim=c(4.5, 5), main="Please Rate Ability to Get in By Phone")
segments(barCenters, means-standardErrors*2, barCenters, means+standardErrors*2, lwd=2)




means <- c(CottonwoodMean, RivertonMean, SandyMean)
names <- c("CWH", "Riverton", "Sandy")
standardErrors <- c(CottonwoodError, RivertonError, SandyError) 
plotTop <- max(means+standardErrors*2)
barCenters <- barplot(means, names.arg=names, col="gray", las=2, ylim=c(3.7, 4), main="Patient Satisfaction: Jan-March")
segments(barCenters, means-standardErrors*2, barCenters, means+standardErrors*2, lwd=2)







##Read in Regression Data##

RegressionPTvOE<-read_csv("KnowPTvsOverallEx - Sheet1.csv", col_types=cols(Clinic=col_character(), Rate=col_number()))

RegressionKNOWvATT<-aov(KnowledgeVSAttendance)

RegressionAVP<-aov(PhoneVAttendance)


##Read in Dataset 1##
OverallExperience<-read_csv("OverallExperience5Weeks - Sheet1.csv", col_types=cols(Clinic=col_character()))
View(OverallExperience)


##Read in Dataset 2##
Patients<-read_csv("NumberofPatients5Weeks - Sheet1.csv", col_types=cols(Clinic=col_character()))
View(Patients)

