#------------------------------------------------------------Import Data Table 

#Set Working Directory
getwd()
setwd("C:\\Users\\zara\\Documents\\LSUS Files\\MBA 701 Economic Analysis for Management\\MBA 701 CSV Data Sets")  
getwd()

#Install Excel File Reading Package
install.packages("readxl") 

#Open readxl library and load excel file 
library(readxl) #load the readxl library
Crimestats <- read_excel("State Crime.xlsx")

#-------------------------------------------------------------Data Mining and Cleaning

head(Crimestats)

#Count the Number of Rows in the data table 
nrow(Crimestats)

#Drop first two rows since they contain unimportant text 
Crimestats = Crimestats[3:51,]

#Count the rows of the new data frame
nrow(Crimestats)
head(Crimestats)

#Count number of columns
ncol(Crimestats)

#Drop Column 10 because all values are NA 
Crimestats = Crimestats[,c(1:9,11:26)]
head(Crimestats)

#Set row 1 as the column names 
colnames(Crimestats) <- Crimestats[1,]
Crimestats <- Crimestats[-1,]
head(Crimestats)

#Rename the columns that have a % symbol in them 
print(colnames(Crimestats)) #view the names of all columns
colnames(Crimestats)[colnames(Crimestats) == "%Metro"] <- 'Metro(%)'
colnames(Crimestats)[colnames(Crimestats) == "%Bachelors+"] <- 'Bachelors(%)'
colnames(Crimestats)[colnames(Crimestats) == "%Unemploy"] <- 'Unemployed(%)'
colnames(Crimestats)[colnames(Crimestats) == "%Poverty"] <- 'Poverty(%)'
colnames(Crimestats)[colnames(Crimestats) == "%SameHouse"] <- 'SameHouse(%)'
colnames(Crimestats)[colnames(Crimestats) == "%young"] <- 'Young(%)'
colnames(Crimestats)[colnames(Crimestats) == "%youngmale"] <- 'YoungMale(%)'
colnames(Crimestats)[colnames(Crimestats) == "%youngfemale"] <- 'YoungFemale(%)'
print(colnames(Crimestats)) #Check if all names are changed

#Check Variable Types
str(Crimestats)

#Convert Multiple Columns Variable Types to Numeric
#This is done for column number values that were labeled as character when imported into R 
Crimestats[,2:25] <- lapply(Crimestats[,2:25], as.numeric)  

#Check to see if all variable types are correct now
str(Crimestats)

#-----------------------------------------------------------------------Create Regression Models
#Model A: Robbery = f(%Metro, %Poverty, Rain)
ModelA <- lm(Crimestats$Robbery ~ Crimestats$`Metro(%)` + Crimestats$`Poverty(%)` + Crimestats$Rain)
summary(ModelA)

#Model B: Robbery = f(Population, %SameHouse, MedianAge)
ModelB <- lm(Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + Crimestats$MedianAge)
summary(ModelB)

#Model C: Robbery = f(Population, %SameHouse, MedAgeMale)
ModelC <- lm(Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + Crimestats$MedAgeMale)
summary(ModelC)


#Model D: Robbery = f(Population, %SameHouse, MedAgeFemale)
ModelD <- lm(Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + Crimestats$MedAgeFemale)
summary(ModelD)

#Model E: Robbery = f(CarTheft, %Unemploy, %Bachelors+, Temp)
ModelE <- lm(Crimestats$Robbery ~ Crimestats$CarTheft + Crimestats$`Unemployed(%)` + Crimestats$`Bachelors+(%)`+Crimestats$Temp)
summary(ModelE)

#Model F: Robbery = f(CarTheft, %Unemploy, %Bachelors+, Sun)
ModelF <- lm(Crimestats$Robbery ~ Crimestats$CarTheft + Crimestats$`Unemployed(%)`+Crimestats$`Bachelors+(%)`+Crimestats$Sun)
summary(ModelF)

#Model G: Robbery = f(CarTheft, %Unemploy, %Bachelors+, Rain)
ModelG <- lm(Crimestats$Robbery ~ Crimestats$CarTheft+ Crimestats$`Unemployed(%)`+Crimestats$`Bachelors+(%)`+ Crimestats$Rain)
summary(ModelG)






