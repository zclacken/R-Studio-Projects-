#--------------------------------------------------------------Import Data Frame

#Check and Set Working Directory
getwd()
setwd("C:\\Users\\zara\\Documents\\Data Science Portfolio_Zara Clacken")
getwd()

#Import Data Frame
Avocados = read.csv("2021 Avocado Market Region Sales and Price Data.csv")

#-------------------------------------------------------------------Explore Data

#column names
colnames(Avocados)

#unique data of Geography column: I will use them as the row labels
unique(Avocados$Geography) 

#Count Rows
nrow(Avocados)

#Check Data Frame's Structure
str(Avocados) # check structure of data 


#---------------------------------------------------------Data Mining & Cleaning

#Change Geography and Period to factor
Avocados$Geography <- as.factor(Avocados$Geography)
Avocados$Period <- as.factor(Avocados$Period)
str(Avocados)

#Create a Vector for U.S. Regions  
Regions = c("California",  
            "Great Lakes", "Midsouth", 
            "Northeast", "Plains", 
            "South Central","Southeast", "West")

#Create a Vector for Desired Columns
columnslist = c("Geography","Period","Units.Prior.Year" ,
            "Units.Current.Year","Dollars.Prior.Year","Dollars.Current.Year",
            "ASP.Prior.Year", 
            "ASP.Current.Year")

#Count the number of elements in the Regions vector
length(Regions) 

#Count appearances of regions in data frame
length(which(Avocados$Geography == "California")) 

#Create a sequence to represent the indices of the Regions vector
irange = seq(1,8) #88 because 8 regions by 11 reps of each 

#Create a for loop to fill a vector containing the indices of each region
#Use which function to extract row index numbers  
regionindices = c()
for (i in irange){
  indices = which(Avocados$Geography == Regions[i])
  regionindices = sort(append(regionindices,indices))  
} 
regionindices

#Drop non-region rows and unwanted columns  
Avocados = Avocados[regionindices, columnslist] 

#Check data frame structure again
str(Avocados)
head(Avocados,10) 
unique(Avocados$Geography)

