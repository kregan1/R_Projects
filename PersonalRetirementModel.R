currentCash = ;
currentRetiremment =; 
currentHouse =; 
savingsInvested = 0.4; 
currentAge = 33

#Economic Factors
marketGrowth = .05         #Growth rate of stock market

#Economic Recession Simulator
recessionAge = 36              #Age when a recession hits
recessionSeverity = .33       #Proportion of invested assets lost in recession

#Life Economics
annualIncome =           	  #Combined annual income after tax   
incomeGrowth = 0.005          #Growth rate of annual income per year
retirementContribution =.     #Amount contributed to retirement accounts per year
retirementAge = 65            #Age at retirement
livingExpenses =         	#Annual living expenses
retirementTransfer =    	 #Amount withdrawn from retirement accounts per year after 65

#To reproduce or not to reproduce
ageAtbirth = 				#Age when having kid
kidCost =				#Average annual cost of kid

endRM = 38      

retirementTaxrate = .25

#Big Expenses Simulator(cars, health issues, accidents...)
arrayExpenses = rep(0,120)
arrayExpenses[37]=20000
arrayExpenses[45]=18000
arrayExpenses[75]=50000

savingsInterest = .019

arrayCash = rep(0,120); arrayCash[currentAge]=currentCash
arrayRetirement = rep(0,120); arrayRetirement[currentAge]=currentRetiremment
arrayHouse = rep(0,120); arrayHouse[currentAge]=currentHouse

arrayKid = rep(0,120); arrayKid[ageAtbirth:(ageAtbirth+18)]=kidCost
arrayCrash = rep(1,120); arrayCrash[recessionAge]=1-recessionSeverity
arrayHousing = rep(0,120); arrayHousing[33:63]=1068*12

for(i in (currentAge+1):retirementAge){
  if (i<=endRM){rmrent=6000}else{rmrent=100}
  arrayCash[i] =  arrayCash[i-1] + annualIncome + (arrayCash[i-1]*savingsInvested*marketGrowth) + (arrayCash[i-1]*(1-savingsInvested)*savingsInterest) + rmrent - livingExpenses - retirementContribution - arrayKid[i] - arrayHousing[i] - arrayExpenses[i]
  arrayRetirement[i] = (arrayRetirement[i-1]*arrayCrash[i]) + retirementContribution + arrayRetirement[i-1]*marketGrowth*arrayCrash[i]
  arrayHouse[i] = arrayHouse[i-1] + arrayHouse[i-1]*marketGrowth + annualPrincipal[i]
  
  annualIncome=annualIncome + (annualIncome*incomeGrowth) #Income growth
}


for(i in (retirementAge+1):64){
  if (i<=endRM){rmrent=6000}else{rmrent=100}
  if(arrayRetirement[i-1]>0){arraytransfer[i]=retirementTransfer}else{arraytransfer[i]=0} #Amount transfered from retirement
  arrayCash[i]=   (arrayCash[i-1]+arrayCash[i-1]*savingsInvested*marketGrowth)+ (arrayCash[i-1]*(1-savingsInvested)*savingsInterest) - livingExpenses + arraytransfer[i]*(1-retirementTaxrate) - arrayHousing[i] - arrayExpenses[i]
  arrayHouse[i]=arrayHouse[i-1]+arrayHouse[i-1]*marketGrowth+annualPrincipal[i]
    if(arrayRetirement[i-1]>0){arrayRetirement[i] = (arrayRetirement[i-1]*arrayCrash[i]) + arrayRetirement[i-1]*arrayCrash[i]*marketGrowth - arraytransfer[i]}
  if(i==housingCrashage){arrayHouse[i]=arrayHouse[i]*(1 - housingCrashseverity)}
  
}


for(i in 65:120){
  if (i<=endRM){rmrent=6000}else{rmrent=100}
  if(arrayRetirement[i-1]>0){arraytransfer[i]=retirementTransfer}else{arraytransfer[i]=0} #Amount transfered from retirement
  arrayCash[i]=   (arrayCash[i-1]+arrayCash[i-1]*savingsInvested*marketGrowth)+ socialSecurity - livingExpenses + arraytransfer[i]*(1-retirementTaxrate) - arrayHousing[i] - arrayExpenses[i]
  arrayHouse[i]=arrayHouse[i-1]+arrayHouse[i-1]*marketGrowth+annualPrincipal[i]
  if(arrayRetirement[i-1]>0){arrayRetirement[i] = (arrayRetirement[i-1]*arrayCrash[i]) + arrayRetirement[i-1]*arrayCrash[i]*marketGrowth - arraytransfer[i]}
  if(i==housingCrashage){arrayHouse[i]=arrayHouse[i]*(1 - housingCrashseverity)}
  
}

top=1000000
plot(arrayRetirement,type="l",col="blue",lwd=2,ylim=c(0,min(max(arrayRetirement,arrayLiquid,arrayHomevalue),top)),yaxt="no")
lines(arrayCash,lwd=2); abline(v=retirementAge,col="red",lwd=2)
abline(v=currentAge,lwd=2,col="red")
lines(arrayHouse,lwd=2,col="orange")
lines(26:currentAge,arrayPastsavings[26:currentAge],lwd=2,col="black"); lines(26:currentAge,arrayPastsretirement[26:currentAge],lwd=2, col="blue")
abline(h=500000);abline(h=1000000);abline(h=1500000);abline(h=2000000);abline(h=2000000);
axis(side=2,at=c(500000,1000000,1500000,2000000,2500000),lab=c(0.5,1,1.5,2,2.5))
legend(1,min(max(arrayRetirement,arrayLiquid,arrayHomevalue),top),legend=c("Cash","Retirement","House"),col=c("black","blue","orange"),lty=1,lwd=3)

annualPrincipal = rep(0,120)
  annualPrincipal[33]=4856
  annualPrincipal[34]=4051
  annualPrincipal[35]=4211
  annualPrincipal[36]=4377
  annualPrincipal[37]=4550
  annualPrincipal[38]=4729
  annualPrincipal[39]=4916
  annualPrincipal[40]=5110
    annualPrincipal[41]=5311
    annualPrincipal[42]=5521
    annualPrincipal[43]=5739
    annualPrincipal[44]=5965
    annualPrincipal[45]=6201
    annualPrincipal[46]=6445
    annualPrincipal[47]=6699
    annualPrincipal[48]=6964
    annualPrincipal[49]=7238
    annualPrincipal[50]=7524
    annualPrincipal[51]=7821
    annualPrincipal[52]=8129
    annualPrincipal[53]=8450
    annualPrincipal[54]=8783
    annualPrincipal[55]=9130
    annualPrincipal[56]=9490
    annualPrincipal[57]=9864
    annualPrincipal[58]=10253
    annualPrincipal[59]=10658
    annualPrincipal[60]=11078
    annualPrincipal[61]=11515
    annualPrincipal[62]=9923
    
    