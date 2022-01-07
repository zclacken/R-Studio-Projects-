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
     yaxt ="none",xlim = c(0.5,4.5),lwd = 0.5, pch = 18, cex =1.5) 
axis(2,seq(0.5,1.35,0.10))
legend("topright", legend = levels(factor(AvocadosRegional$Geography)), 
       fill = as.numeric(unique(factor(AvocadosRegional$Geography)))) 

#----------------------------------------------------------Seasonal  Demand Plot 

#Create Seasons Vectors

#Create a periods vector
Periods = seq(1,11)

#What are the unique dates corresponding to each season?  
unique(AvocadosSeasons$Current.Year.Week.Ending)

#Winter = Periods 1,2
#Spring = Periods 3,4,5
#Summer = Periods 6,7,8
#Fall = Periods 9,10,11 

#There are 8 regions, thus each period is repeated 8 times
#Thus, each season's vector should have the season repeated 8 times periods in the season

#Create a seasons vector 
Winter = rep("Winter", 16)
Spring = rep("Spring",24)
Summer = rep("Summer",24)
Fall = rep("Fall", 24)
Seasonsvector = c(Winter, Spring, Summer,Fall)

#Create a seasons data frame with a season column 
AvocadosSeasons <- AvocadosRegional
AvocadosSeasons$Season <- Seasonsvector
head(AvocadosSeasons)
unique(AvocadosSeasons$Season)

#Create a Seasons Sale Plot 
plot(AvocadosSeasons$Units.Current.Year/1e+07, AvocadosSeasons$ASP.Current.Year,
     xlab = "Units Sold (Millions)",ylab = "Average Sales Price ($)", 
     main = "2021 U.S.A. Hass Avocado Demand (Seasonal)", 
     col = factor(AvocadosSeasons$Season), 
     yaxt ="none",xlim = c(0.5,4.5),lwd = 0.5, pch = 18, cex =1.5) 
axis(2,seq(0.5,1.35,0.10))
legend("topright", legend = levels(factor(AvocadosSeasons$Season)), 
       fill = as.numeric(unique(factor(AvocadosSeasons$Season)))) 

#-----------------------------------------------------Regional and Seasonal Plot 

#Create a function to plot demand per region according to season 
RegionSeasonplot <- function(season, title){
  plot(AvocadosSeasons[AvocadosSeasons$Season == season,]$Units.Current.Year/1e+07, 
       AvocadosSeasons[AvocadosSeasons$Season == season,]$ASP.Current.Year,
       xlab = "Units Sold (Millions)",ylab = "Average Sales Price ($)", 
       main = title, 
       col = factor(AvocadosSeasons$Geography), 
       yaxt ="none",xlim = c(0.5,4.5),lwd = 0.5, pch = 18, cex =2) 
  axis(2,seq(0.5,1.5,0.10))
  legend("topright", legend = levels(factor(AvocadosSeasons$Geography)), 
         fill = as.numeric(unique(factor(AvocadosSeasons$Geography)))) 
}

#Create Regional Plot For Each Season Using Function 
RegionSeasonplot(Winter,"2021 U.S.A Hass Avocado Regional Demand (Winter)")
RegionSeasonplot(Spring,"2021 U.S.A Hass Avocado Regional Demand (Spring)")
RegionSeasonplot(Summer,"2021 U.S.A Hass Avocado Regional Demand (Summer)")
RegionSeasonplot(Fall,"2021 U.S.A Hass Avocado Regional Demand (Fall)")


#-----------------------------------------------Total Sales by Region Data Frame

#Create a vector containing the annual total for each region 
regioncount = as.vector(seq(1,8))
regiontotals <- c()
for (r in regioncount) {
  sums = sum(AvocadosSeasons[AvocadosSeasons == Regions[r],]$Units.Current.Year)
  print(sums)
  regiontotals <- c(regiontotals,sums)
}
regiontotals

#Create a table for region totals and region; sorted from highest to lowest sales 
AvocadosTotalRegionSales <- data.frame("Region" = Regions,"Total Sales" = regiontotals)
totals_in_order = order(AvocadosTotalRegionSales$Total.Sales, decreasing = TRUE)
AvocadosTotalRegionSales <- AvocadosTotalRegionSales[totals_in_order,]   
head(AvocadosTotalRegionSales)

#-------------------------------------------------Total Sales by Region Bar Plot 

#Adjust the size and margins of the plot
par(mar = c(6, 7, 4, 8) + 0.1) 
#create plot 
plt <- barplot(height = AvocadosTotalRegionSales$Total.Sales/1e+06, 
        ylab = "Units Sold (Millions)", 
        main = "2021 Total U.S.A. Hass Avocado Sales (Regional)", 
        yaxt = "none", xaxt = "none", col = factor(AvocadosTotalRegionSales$Region),
        space = 0.2)
#format y-axis
axis(2,seq(0,400,100))
#Create x-axis labels that are diagonally rotated 
text(plt, par("usr")[3]-4, srt = 60, adj = 1, xpd = TRUE,
     labels = factor(AvocadosTotalRegionSales$Region), cex = 1)

#---------------------------------------------------Total Avocado Sales Seasonal

#Create Seasonal Sales Total Data Frame
WSSF = c("Winter", "Spring", "Summer", "Fall")
seasonaltotals = c()
for (s in seq(1,4)) {
  sums2 = sum(AvocadosSeasons[AvocadosSeasons$Season == WSSF[s],"Units.Current.Year"])
  seasonaltotals = c(seasonaltotals,sums2)
}
seasonaltotals

AvocadosTotalSeasonSales <- data.frame("Season" = WSSF,"Total Sales" = seasonaltotals)
totals_in_order2 = order(AvocadosTotalSeasonSales$Total.Sales, decreasing = TRUE)
AvocadosTotalSeasonSales <- AvocadosTotalSeasonSales[totals_in_order2,]   
head(AvocadosTotalSeasonSales)

#--------------------------------------------------Total Seasonal Sales Bar Plot

#Adjust the size and margins of the plot
par(mar = c(6, 7, 4, 8) + 0.1) 
#create plot 
barplot(height = AvocadosTotalSeasonSales$Total.Sales/1e+06, 
               ylab = "Units Sold (Millions)", 
               main = "2021 Total U.S.A. Hass Avocado Sales (Seasonal)", 
               yaxt = "none", names.arg = factor(AvocadosTotalSeasonSales$Season) , 
               col = factor(AvocadosTotalSeasonSales$Season),
               space = 0.2)
#format y-axis
axis(2,seq(0,500,100))

#--------------------------------------------Create data frame for 2021 Revenues 
colnames(AvocadosSeasons)
AvocadosRevenues <- AvocadosSeasons[,c("Geography","Period","Units.Current.Year",
                                       "Dollars.Current.Year","ASP.Current.Year",
                                       "Season")]
head(AvocadosRevenues)
#--------------------------------------------------------Regional Revenue Totals 

#Create a vector containing the year totals for region 
regioncount = as.vector(seq(1,8))
regionrevenues <- c()
for (r in regioncount) {
  revsums = sum(AvocadosRevenues[AvocadosRevenues == Regions[r],]$Dollars.Current.Year)
  regionrevenues <- c(regionrevenues,revsums)
}
regionrevenues

#Create a table for region totals and region; sorted from highest to lowest sales 
AvocadosTotalRegionRevenues <- data.frame("Region" = Regions,"Total Revenue" = regionrevenues)
rev_in_order = order(AvocadosTotalRegionRevenues$Total.Revenue, decreasing = TRUE)
AvocadosTotalRegionRevenues <- AvocadosTotalRegionRevenues[rev_in_order,]   
head(AvocadosTotalRegionRevenues)  

#----------------------------------------------Regional Revenues Totals Bar Plot

#Adjust the size and margins of the plot
par(mar = c(6, 7, 4, 8) + 0.1) 
#create plot 
plt3 <- barplot(height = AvocadosTotalRegionRevenues$Total.Revenue/1e+06, 
               ylab = "Revenue (Millions $)", 
               main = "2021 Total Hass Avocado Revenue (Regional)", 
               yaxt = "none", xaxt = "none", col = factor(AvocadosTotalRegionRevenues$Region),
               space = 0.2)
#format y-axis
axis(2,seq(0,400,100))
#Create x-axis labels that are diagonally rotated 
text(plt3, par("usr")[3]-4, srt = 60, adj = 1, xpd = TRUE,
     labels = factor(AvocadosTotalRegionRevenues$Region), cex = 1)

#-------------------------------------------------------Seasonal Revenues Totals  

#Create Seasonal Sales Total Data Frame
WSSF = c("Winter", "Spring", "Summer", "Fall")
seasonalrevenues = c()
for (s in seq(1,4)) {
  revsums2 = sum(AvocadosRevenues[AvocadosRevenues$Season == WSSF[s],"Dollars.Current.Year"])
  seasonalrevenues = c(seasonalrevenues,revsums2)
}
seasonalrevenues

AvocadosTotalSeasonRevenues <- data.frame("Season" = WSSF,"Total Revenue" = seasonalrevenues)
totals_in_order3 = order(AvocadosTotalSeasonRevenues$Total.Revenue, decreasing = TRUE)
AvocadosTotalSeasonRevenues <- AvocadosTotalSeasonRevenues[totals_in_order3,]   
head(AvocadosTotalSeasonRevenues)

#-----------------------------------------------Seasonal Total Revenues Bar Plot

#Adjust the size and margins of the plot
par(mar = c(6, 7, 4, 8) + 0.1) 
#create plot 
barplot(height = AvocadosTotalSeasonRevenues$Total.Revenue/1e+06, 
                ylab = "Revenue (Millions $)", 
                main = "2021 Total U.S.A. Hass Avocado Revenue (Seasonal)", 
                yaxt = "none", names.arg = factor(AvocadosTotalSeasonRevenues$Season), 
                col = factor(AvocadosTotalSeasonRevenues$Season),
                space = 0.2)
#format y-axis
axis(2,seq(0,500,100))


#------------------------------------------------------------Regional Elasticity 

#Create Data Frame for Price Elasticity
colnames(AvocadosRevenues)

#Test filter 
AvocadosRevenues[AvocadosRevenues$Period == 1 & AvocadosRevenues$Geography== "California","Units.Current.Year"]

#Create a function for calculating Regional point elasticity 
elasticityfunc <- function(region){
  Evector = c()
  for (e in seq(1,11)) {
    DeltaQ = (AvocadosRevenues[AvocadosRevenues$Period == e & AvocadosRevenues$Geography== region,"Units.Current.Year"]
    - AvocadosRevenues[AvocadosRevenues$Period == (e-1) & AvocadosRevenues$Geography== region,"Units.Current.Year"]) 
    DeltaP = round((AvocadosRevenues[AvocadosRevenues$Period == e & AvocadosRevenues$Geography== region,"ASP.Current.Year"]
    - AvocadosRevenues[AvocadosRevenues$Period == (e-1) & AvocadosRevenues$Geography== region,"ASP.Current.Year"]),2)  
    Q = AvocadosRevenues[AvocadosRevenues$Period == e & AvocadosRevenues$Geography== region,"Units.Current.Year"]
    P = round(AvocadosRevenues[AvocadosRevenues$Period == e & AvocadosRevenues$Geography== region,"ASP.Current.Year"],2)
    E = (DeltaQ/DeltaP)*(P/Q)
    Evector = c(Evector,E)
  }
  Evector
}

unique(AvocadosRevenues$Geography)

#Create vectors 
California_elasticity = elasticityfunc("California")
Great_Lakes_elasticity =elasticityfunc("Great Lakes")
Midsouth_elasticity = elasticityfunc("Midsouth")
Northeast_elasticity = elasticityfunc("Northeast")
Plains_elasticity = elasticityfunc("Plains")
South_Central_elasticity = elasticityfunc("South Central")
Southeast_elasticity = elasticityfunc("Southeast")
West_elasticity = elasticityfunc("West")

#Create data frame of Regional Elasticities 
AvocadosElasticity = data.frame("Period" = seq(2,11),
                                "California" = California_elasticity, 
                                "Great Lakes" = Great_Lakes_elasticity,
                                "Midsouth" = Midsouth_elasticity, 
                                "Northeast" = Northeast_elasticity,
                                "Plains" = Plains_elasticity, 
                                "South Central" = South_Central_elasticity,
                                "Southeast" = Southeast_elasticity, 
                                "West" = West_elasticity)
head(AvocadosElasticity)





