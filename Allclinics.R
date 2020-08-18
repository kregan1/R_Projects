###Data Entry

dataSandy<-read.csv("SandyQuickdash2019V15 - Sheet1-2.csv")
dataCWH<-read.csv("CWHQuickdash2019V15 - Sheet1.csv")
dataDraper<-read.csv("DraperQuickdash2019V15 - Sheet1.csv")
dataMurray<-read.csv("MurrayQuickdash2019V15 - Sheet1.csv")
dataRiverton<-read.csv("RivertonQuickdash2019V15 - Sheet1.csv")
dataSS<-read.csv("SSQuickdash2019V15 - Sheet1.csv")
dataSB<-read.csv("SBQuickdash2019V15 - Sheet1.csv")
dataSJ<-read.csv("SJQuickdash2019V15 - Sheet1.csv")
dataWJ<-read.csv("WJQuickdash2019V15 - Sheet1.csv")


dataSandy<-subset(dataSandy, dataSandy$Score1<1000 | dataSandy$Score15<1000)
dataCWH<-subset(dataCWH, dataCWH$Score1<1000 | dataCWH$Score15<1000)
dataDraper<-subset(dataDraper, dataDraper$Score1<1000 | dataDraper$Score15<1000)
dataMurray<-subset(dataMurray, dataMurray$Score1<1000 | dataMurray$Score15<1000)
dataRiverton<-subset(dataRiverton, dataRiverton$Score1<1000 | dataRiverton$Score15<1000)
dataSS<-subset(dataSS, dataSS$Score1<1000 | dataSS$Score15<1000)
dataSB<-subset(dataSB, dataSB$Score1<1000 | dataSB$Score15<1000)
dataSJ<-subset(dataSJ, dataSJ$Score1<1000 | dataSJ$Score15<1000)
dataWJ<-subset(dataWJ, dataWJ$Score1<1000 | dataWJ$Score15<1000)



###############################################################################################
###Sandy
Sandy15<-mean(dataSandy$Score15, na.rm=TRUE)
Sandy1<-mean(dataSandy$Score1, na.rm = TRUE) ###48.10
SandySD1<-sd(dataSandy$Score1, na.rm = TRUE) ###22.50

###Subset data
dataSandy1<-subset(dataSandy, dataSandy$Score1>0)
dataSandy2<-subset(dataSandy, dataSandy$Score15>0)

###Merge data into data.frame
hasallSandy<-merge(dataSandy1, dataSandy2, by="Name")
newdataSandy<-data.frame(hasallSandy$Name,hasallSandy[,8],hasallSandy[,5])

###This just adds the column labels
names(newdataSandy) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataSandy$name))

for (i in 1:length(newdataSandy$name)){
  differenceL1L15S[i]=newdataSandy$L1[i] - newdataSandy$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
SandyMean<-mean(differenceL1L15S) ###26.1
SandySD<-sd(differenceL1L15S) ###19.68

########################################################################################
###CWH
CWH15<-mean(dataCWH$Score15, na.rm=TRUE) ###28.57
CWH1<-mean(dataCWH$Score1, na.rm = TRUE) ###39.02
CWHSD1<-sd(dataCWH$Score1, na.rm = TRUE) ###23.26

###Subset data
dataCWH1<-subset(dataCWH, dataCWH$Score1>0)
dataCWH2<-subset(dataCWH, dataCWH$Score15>0)

###Merge data into data.frame
hasallCWH<-merge(dataCWH1, dataCWH2, by="Name")
newdataCWH<-data.frame(hasallCWH$Name,hasallCWH[,8],hasallCWH[,5])

###This just adds the column labels
names(newdataCWH) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataCWH$name))

for (i in 1:length(newdataCWH$name)){
  differenceL1L15S[i]=newdataCWH$L1[i] - newdataCWH$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
CWHMean<-mean(differenceL1L15S) ###26.1
CWHSD<-sd(differenceL1L15S) ###19.68


#########################################################################################
###Draper
Draper15<-mean(dataDraper$Score15, na.rm=TRUE)
Draper1<-mean(dataDraper$Score1, na.rm = TRUE) ###43.11
DraperSD1<-sd(dataDraper$Score1, na.rm = TRUE) ###21.98

###Subset data
dataDraper1<-subset(dataDraper, dataDraper$Score1>0)
dataDraper2<-subset(dataDraper, dataDraper$Score15>0)

###Merge data into data.frame
hasallDraper<-merge(dataDraper1, dataDraper2, by="Name")
newdataDraper<-data.frame(hasallDraper$Name,hasallDraper[,8],hasallDraper[,5])

###This just adds the column labels
names(newdataDraper) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataDraper$name))

for (i in 1:length(newdataDraper$name)){
  differenceL1L15S[i]=newdataDraper$L1[i] - newdataDraper$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
DraperMean<-mean(differenceL1L15S) ###20.71
DraperSD<-sd(differenceL1L15S) ###26.43


############################################################################################
###Murray
Murray15<-mean(dataMurray$Score15, na.rm=TRUE)
Murray1<-mean(dataMurray$Score1, na.rm = TRUE)
MurraySD1<-sd(dataMurray$Score1, na.rm = TRUE) ###21.46

###Subset data
dataMurray1<-subset(dataMurray, dataMurray$Score1>0)
dataMurray2<-subset(dataMurray, dataMurray$Score15>0)

###Merge data into data.frame
hasallMurray<-merge(dataMurray1, dataMurray2, by="Name")
newdataMurray<-data.frame(hasallMurray$Name,hasallMurray[,8],hasallMurray[,5])

###This just adds the column labels
names(newdataMurray) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataMurray$name))

for (i in 1:length(newdataMurray$name)){
  differenceL1L15S[i]=newdataMurray$L1[i] - newdataMurray$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
MurrayMean<-mean(differenceL1L15S) ###20.86
MurraySD<-sd(differenceL1L15S) ###18.01


###########################################################################################
###Riverton
Riverton15<-mean(dataRiverton$Score15, na.rm=TRUE)
Riverton1<-mean(dataRiverton$Score1, na.rm = TRUE) ###47.98
RivertonSD1<-sd(dataRiverton$Score1, na.rm = TRUE) ###22.13

###Subset data
dataRiverton1<-subset(dataRiverton, dataRiverton$Score1>0)
dataRiverton2<-subset(dataRiverton, dataRiverton$Score15>0)

###Merge data into data.frame
hasallRiverton<-merge(dataRiverton1, dataRiverton2, by="Name")
newdataRiverton<-data.frame(hasallRiverton$Name,hasallRiverton[,8],hasallRiverton[,5])

###This just adds the column labels
names(newdataRiverton) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataRiverton$name))

for (i in 1:length(newdataRiverton$name)){
  differenceL1L15S[i]=newdataRiverton$L1[i] - newdataRiverton$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
RivertonMean<-mean(differenceL1L15S) ###25.41
RivertonSD<-sd(differenceL1L15S) ###19.83

#############################################################################################
###SS
SS15<-mean(dataSS$Score15, na.rm=TRUE)
SS1<-mean(dataSS$Score1, na.rm = TRUE) ###46.42
SSSD1<-sd(dataSS$Score1, na.rm = TRUE) ###20.98

###Subset data
dataSS1<-subset(dataSS, dataSS$Score1>0)
dataSS2<-subset(dataSS, dataSS$Score15>0)

###Merge data into data.frame
hasallSS<-merge(dataSS1, dataSS2, by="Name")
newdataSS<-data.frame(hasallSS$Name,hasallSS[,8],hasallSS[,5])

###This just adds the column labels
names(newdataSS) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataSS$name))

for (i in 1:length(newdataSS$name)){
  differenceL1L15S[i]=newdataSS$L1[i] - newdataSS$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
SSMean<-mean(differenceL1L15S) ###18.33
SSSD<-sd(differenceL1L15S) ###28.57

#################################################################################
###SB
SB15<-mean(dataSB$Score15, na.rm=TRUE)
SB1<-mean(dataSB$Score1, na.rm = TRUE)
SBSD1<-sd(dataSB$Score1, na.rm = TRUE)

###Subset data
dataSB1<-subset(dataSB, dataSB$Score1>0)
dataSB2<-subset(dataSB, dataSB$Score15>0)

###Merge data into data.frame
hasallSB<-merge(dataSB1, dataSB2, by="Name")
newdataSB<-data.frame(hasallSB$Name,hasallSB[,8],hasallSB[,5])

###This just adds the column labels
names(newdataSB) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataSB$name))

for (i in 1:length(newdataSB$name)){
  differenceL1L15S[i]=newdataSB$L1[i] - newdataSB$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
SBMean<-mean(differenceL1L15S) ###23.85
SBSD<-sd(differenceL1L15S) ###23.39

#############################################################################
###SJ
SJ15<-mean(dataSJ$Score15, na.rm=TRUE)
SJ1<-mean(dataSJ$Score1, na.rm = TRUE)
SJSD1<-sd(dataSJ$Score1, na.rm = TRUE)

###Subset data
dataSJ1<-subset(dataSJ, dataSJ$Score1>0)
dataSJ2<-subset(dataSJ, dataSJ$Score15>0)

###Merge data into data.frame
hasallSJ<-merge(dataSJ1, dataSJ2, by="Name")
newdataSJ<-data.frame(hasallSJ$Name,hasallSJ[,8],hasallSJ[,5])

###This just adds the column labels
names(newdataSJ) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataSJ$name))

for (i in 1:length(newdataSJ$name)){
  differenceL1L15S[i]=newdataSJ$L1[i] - newdataSJ$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
SJMean<-mean(differenceL1L15S) ###25.99
SJSD<-sd(differenceL1L15S) ###21.47

##################################################################################
###WJ
WJ15<-mean(dataWJ$Score15, na.rm=TRUE)
WJ1<-mean(dataWJ$Score1, na.rm = TRUE)
WJSD1<-sd(dataWJ$Score1, na.rm = TRUE)

###Subset data
dataWJ1<-subset(dataWJ, dataWJ$Score1>0)
dataWJ2<-subset(dataWJ, dataWJ$Score15>0)

###Merge data into data.frame
hasallWJ<-merge(dataWJ1, dataWJ2, by="Name")
newdataWJ<-data.frame(hasallWJ$Name,hasallWJ[,8],hasallWJ[,5])

###This just adds the column labels
names(newdataWJ) = c("name", "L15", "L1")

differenceL1L15S=array(NA, length(newdataWJ$name))

for (i in 1:length(newdataWJ$name)){
  differenceL1L15S[i]=newdataWJ$L1[i] - newdataWJ$L15[i]
}

###Check to make sure it filled
differenceL1L15S

###Means and SD
WJMean<-mean(differenceL1L15S) ###23.8
WJSD<-sd(differenceL1L15S) ###18.77




diffArray<-array(NA, length(9))

diffArray<-c(CWHMean, DraperMean, MurrayMean, RivertonMean, SandyMean, SSMean, SBMean, SJMean, WJMean)


v1Array<-array(NA, length(9))

v1Array<-c(CWH1, Draper1, Murray1, Riverton1, Sandy1, SS1, SB1, SJ1, WJ1)


v1byDiff<-data.frame(v1Array, diffArray)

plot(v1Array, diffArray)

library(ggplot2)

ggplot(v1byDiff, aes(x = v1Array, y = diffArray)) +
  xlab("QuickDASH at Visit 1") + ylab("Difference between 1st and 15th visit") +
  ggtitle("QuickDASH at Visit 1 vs the Difference Between 1st and 15th Visit") +
  geom_point(shape = 20, color = "black", size = 4) +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed", color = "darkred")


v1SDArray<-array(NA, length(9))

v1SDArray<-c(CWHSD1, DraperHSD1, MurraySD1, RivertonSD1, SandySD1, SSSD1, SBSD1, SJSD1, WJSD1)

Clinics<-array(NA, length(9))
Clinics<-c("CWH", "Draper", "Murray", "Riverton", "Sandy", "SS", "SB", "SJ", "WJ" )

avgSD<-data.frame(Clinics, v1Array, v1SDArray)


ggplot(avgSD, aes(x = Clinics, y = v1Array)) +
  geom_bar(stat="identity", color = "black") + 
  geom_errorbar(aes(ymin = v1Array - v1SDArray, ymax = v1Array + v1SDArray), width = 0.2) +
  ggtitle("QuickDASH at visit 1 for each clinic") +
  ylab("QuickDASH at Visit 1")
