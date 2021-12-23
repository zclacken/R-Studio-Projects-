#--------------------------------------------------------------------------Tasks 
#1 Calculate annual elasticities for Tuition v. Enrollment 
#2 Calculate Tuition v. credit hour   

#-------------------------------------------------------Create vectors from data
#Create Year Vector 
Year = as.factor(seq(2000,2020))

#Undergrad Enrollment Vector
Enrollment = c(3422,	3419,	3543,	3655,	3910,	3940,	3594,	3556,	3903,	
               4220,	4058,	4134,	4124,	3674,	3202,	2775,	2587,	2637,	
               2511,	2577,	2553)

#Total LSUS Credit Hour Production Vector
Credits = c(90624,	94446,	96039,	101352,	101868,	100181,	92486,	92123,	
            94639,	101972,	98137,	98372,	93163,	85292,	87907,	91021,	
            94077,	115340,	137467,	165057,	191712)

#Total Undergrad Tuition and Fees 
Tuitionfees = c(1025,	1150,	1184,	1442,	1545,	1621,	1667,	1667,	1751,	1867,	
            2062,	2247,	2472,	2803,	3084,	3355,	3417,	3417,	3663,	3663,	3663)

#--------------------------------------------------Merge Vectors into Data Frame

LSUSdata <- data.frame(Year,Enrollment,Credits,Tuitionfees)

#-------------------------------------------------------------Elasticity Formula

# E = %change in Q/%change in P 


#--------------------------------------------------Calculate Changes in Q and P 

#Create columns showing percent change in Qs and P

#Change in Price FOr Loop 
n = seq(2,21)
DeltaP <- numeric(20)
for (i in n) {
  PriceChange <- c(LSUSdata$Tuitionfees[i] - LSUSdata$Tuitionfees[i-1])
  DeltaP[i] <- PriceChange
}   

#Insert Vector Created by Loop into a new Data Frame Column 
LSUSdata$PriceChange <- DeltaP
head(LSUSdata)

#Change in Enrollment For Loop
n = seq(2,21)
DeltaEnroll <- numeric(20)
for (i in n) {
  EnrollmentChange <- c(LSUSdata$Enrollment[i] - LSUSdata$Enrollment[i-1])
  DeltaEnroll[i] <- EnrollmentChange
}   
#Insert Vector Created by Loop into a new Data Frame Column 
LSUSdata$EnrollChange <- DeltaEnroll
head(LSUSdata)

#Change in Credits For Loop 
n = seq(2,21)
DeltaCredits <- numeric(20)
for (i in n) {
  CreditsChange <- c(LSUSdata$Credits[i] - LSUSdata$Credits[i-1])
  DeltaCredits[i] <- CreditsChange
}   
#Insert Vector Created by Loop into a new Data Frame Column 
LSUSdata$CreditsChange <- DeltaCredits
head(LSUSdata)

#------------------------------------------------------Calculate Percent Changes 

#Percent Change  = (x2-x1)/x1 

#Percent Change of Prices For Loop
n = seq(2,21)
DeltaPpercent <- numeric(20)
for (i in n) {
  PpercentChange <- c(LSUSdata$PriceChange[i]/LSUSdata$Tuitionfees[i-1])
  DeltaPpercent[i] <- PpercentChange
}   
#Insert Vector Created by Loop into a new Data Frame Column 
LSUSdata$PriceChangePercent <-  DeltaPpercent
head(LSUSdata) 

#Percent Change of Enrollment For Loop 
n = seq(2,21)
DeltaEnrollPercent <- numeric(20)
for (i in n) {
  EnrollPercentChange <- c(LSUSdata$EnrollChange[i]/LSUSdata$Enrollment[i-1])
  DeltaEnrollPercent[i] <- EnrollPercentChange
}   
LSUSdata$EnrollChangePercent <-  DeltaEnrollPercent
head(LSUSdata) 

#Percent Change of Credits FOr Loop
n = seq(2,21)
DeltaCreditsPercent <- numeric(20)
for (i in n) {
  CreditsPercentChange <- c(LSUSdata$CreditsChange[i]/LSUSdata$Credits[i-1])
  DeltaCreditsPercent[i] <- CreditsPercentChange
}   
LSUSdata$CreditsChangePercent <-  DeltaCreditsPercent
head(LSUSdata) 

#---------------------------------------------------------Calculate Elasticities

# E = %change in Q/%change in P   

P <- LSUSdata$PriceChangePercent

#Enrollment Elasticities 
LSUSdata$EnrollElasticity <- LSUSdata$EnrollChangePercent/P
head(LSUSdata)

#Credits Elasticities
LSUSdata$CreditsElasticity <- LSUSdata$CreditsChangePercent/P
head(LSUSdata)

#---------------------------------------Create Final Table:Data and Elasticities

LSUSelasticity <- data.frame(Year, Enrollment, Credits, Tuitionfees,
                             LSUSdata$EnrollElasticity, 
                             LSUSdata$CreditsElasticity)
head(LSUSelasticity)


colnames(LSUSelasticity) = c("Year","Enrollment","Credit Hours", 
                             "Tuition & Fees",
                             "Enrollment Elasticity", 
                             "Credit Hour Elasticity")
head(LSUSelasticity)

  Year Enrollment Credit Hours Tuition & Fees Enrollment Elasticity Credit Hour Elasticity
1 2000       3422        90624           1025                   NaN                    NaN
2 2001       3419        94446           1150          -0.007188778             0.34582892
3 2002       3543        96039           1184           1.226708876             0.57049406
4 2003       3655       101352           1442           0.145070419             0.25387748
5 2004       3910       101868           1545           0.976744186             0.07127634
6 2005       3940       100181           1621           0.155976578            -0.33666052
