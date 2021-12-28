#--------------------------------------------------------------Import Data Frame

#Check and Set Working Directory
getwd()
setwd("C:\\Users\\zara\\Documents\\Data Science Portfolio_Zara Clacken")
getwd()

#Import Data Frame
AvocadosRaw = read.csv("2021 Avocado Market Region Sales and Price Data.csv")

#-------------------------------------------------------------------Explore Data

#column names
colnames(AvocadosRaw)

#unique data of Geography column: I will use them as the row labels
unique(AvocadosRaw$Geography) 

#Count Rows
nrow(AvocadosRaw)

#Check Data Frame's Structure
str(AvocadosRaw) # check structure of data 


#---------------------------------------------------------Data Mining & Cleaning

#Change Geography, Period, and Current Year Week Ending to factor
AvocadosRaw$Geography <- as.factor(AvocadosRaw$Geography)
AvocadosRaw$Period <- as.factor(AvocadosRaw$Period)
AvocadosRaw$Current.Year.Week.Ending <- as.factor(AvocadosRaw$Current.Year.Week.Ending)
str(AvocadosRaw)

#Change Current Year Week Ending to Date


#Create a Vector for U.S. Regions  
Regions = c("California",  
            "Great Lakes", "Midsouth", 
            "Northeast", "Plains", 
            "South Central","Southeast", "West")

#Create a Vector for Desired Columns
columnslist = c("Geography","Period","Current.Year.Week.Ending","Units.Prior.Year" ,
            "Units.Current.Year","Dollars.Prior.Year","Dollars.Current.Year",
            "ASP.Prior.Year", 
            "ASP.Current.Year")

#Count the number of elements in the Regions vector
length(Regions) 

#Count appearances of regions in data frame
length(which(AvocadosRaw$Geography == "California")) 

#Create a sequence to represent the indices of the Regions vector
irange = seq(1,8) 

#Create a for loop to fill a vector containing the indices of each region
#Use which function to extract row index numbers  
regionindices = c()
for (i in irange){
  indices = which(AvocadosRaw$Geography == Regions[i])
  regionindices = sort(append(regionindices,indices))  
} 
regionindices

#Drop non-region rows and unwanted columns  
AvocadosRegional = AvocadosRaw[regionindices, columnslist] 

#Check data frame structure again
str(AvocadosRegional)
head(Avocadosregional,10) 
unique(AvocadosRegional$Geography)

#Export Data Frame as CSV
write.csv(AvocadosRegional,"C:\\Users\\zara\\Documents\\
          Data Science Portfolio_Zara Clacken\\Avocados_Project_Regional.csv", 
          row.names = FALSE)

#--------------------------------------------National Demand and Price Equations
colnames(AvocadosRegional)
Nationaldemandregression = lm(AvocadosNational$Units.Current.Year ~ 
                        AvocadosNational$ASP.Current.Year)
summary(Demandregression)
coef(Nationaldemandregression)

#National Demand Equation: Q = 307,138,729 - 124,661,588P

#Price Equation
1/-124661588  # Q coefficient: 1/P coefficient
-307138729/-124661588  #intercept: q intercept/p coefficient 

#National Price Equation P = -8.021717e-09Q + 2.46378

#Check if my algebra is correct 

307138729 - 124661588*1.5 #at P equals 1.5
# Q equals 120,146,347

(1/-124661588)*120146347 + (-307138729/-124661588) #at Q equals 19,511,464
#P  equals 1.5 thus my algebra is correct 

#---------------------------------------Estimated National Demand v. Actual Plot 

#P = (1/-4233335)Q + 6.109005# 

#Create a Table of only National Sales
AvocadosNational = AvocadosRaw[AvocadosRaw$Geography=="Total U.S.",]

#max ASP value
max(Avocados$ASP.Current.Year)

#Create a price vector in 1-cent increments for regression plot
Qvector = as.vector(seq(1e+08,2.5e+08,0.1e+08))
length(Qvector)
Qvector

#Create demand vector based on p range and Q equation
Pvector = c()
qrange = seq(1,16)
for (q in qrange){
  P= (1/-124661588)*Qvector[q] + (-307138729/-124661588)
  Pvector = append(Pvector,P)  
}
Pvector
length(Pvector)

#Plot Total U.S. Sales
plot(AvocadosNational$Units.Current.Year/1e+08,AvocadosNational$ASP.Current.Year,
     xlab = "Units Sold (Millions)",ylab = "Average Sales Price ($)", 
     main = "2021 U.S.A Hass Avocado Demand (National) ", cex = 1.5, pch = 19, 
     col = "blue")

#Plot Regression line onto plot 
lines(y= Pvector,x = Qvector/1e+08,type = "l", col = "Blue", lwd = 2, lty = 1) 

#Add regression equation to plot
text(x=1.79, y=1.10, "P = 2.46378 - 8.021717e-09Q ",col="Black", font=c(2,3), cex=0.8)

#-----------------------------------------------------------Regional Demand Plot
plot(AvocadosRegional$Units.Current.Year/1e+07, AvocadosRegional$ASP.Current.Year,
     xlab = "Units Sold (Millions)",ylab = "Average Sales Price ($)", 
     main = "2021 U.S.A. Hass Avocado Demand (Regional)", 
     col = factor(AvocadosRegional$Geography), 
     yaxt ="none",xlim = c(0.5,4.5),lwd = 0.5, pch = 18, cex =1.1) 
axis(2,seq(0.5,1.5,0.25))
legend("topright", legend = levels(factor(AvocadosRegional$Geography)), 
       fill = as.numeric(unique(factor(AvocadosRegional$Geography)))) 

#-------------------------------------------------------------Seasonal Demand Plot 

#Create a table for seasons 
AvocadosSeason = AvocadosRaw[,]








plot(AvocadosRegional$Units.Current.Year/1e+07, AvocadosRegional$ASP.Current.Year,
     xlab = "Units Sold (Millions)",ylab = "Average Sales Price ($)", 
     main = "2021 U.S.A. Hass Avocado Demand (4-week Period)", 
     col = factor(AvocadosRegional$Period), 
     yaxt ="none",xlim = c(0.5,4.5),lwd = 0.5, pch = 18, cex =1.1) 
axis(2,seq(0.5,1.5,0.25))
legend("topright", legend = levels(factor(AvocadosRegional$Period)), 
       fill = as.numeric(unique(factor(AvocadosRegional$Period)))) 

