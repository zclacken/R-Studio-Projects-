#------------------------------------------------------#Import Data Sets 
getwd()
setwd("C:\\Users\\zara\\Documents\\Data Science Portfolio_Zara Clacken\\Data Frames I Created")
Forwards = read.csv("2021-2022 English Premier League Forward List.csv")
Salary = read.csv("2021-2022 Premier League Forward Annual Salary.csv")
Nationalities = read.csv("2021-2022 Premier League Players Nationalities.csv")
Contracts = read.csv("2021-2022 EPL Contracts.csv")
Performance = read.csv("2021-2022 EPL Player Pre-Contract Performance.csv")


#------------------------------------------------Data Cleaning & Mining Forwards

head(Forwards)
summary(Forwards)

#Convert birth dates to date variables 
Forwards$born = as.Date.character(Forwards$born,format = "%d/%m/%Y")
summary(Forwards)
str(Forwards)
head(Forwards)

#Rename birth dates from born to DOB
colnames(Forwards)[3] = "Birthdate"
head(Forwards)

#Separate height numbers from cm by space between number and cm 
library(tidyverse)
library(magrittr)
Forwards %<>% separate(Height, c("Height(cm)", "dropcm"), sep = ' ') 
head(Forwards)

#drop the cm column
Forwards$dropcm = NULL
head(Forwards)

#drop the Position column
Forwards$Position = NULL
head(Forwards)

#Convert Height column to numeric variables
Forwards$`Height(cm)`= as.numeric(Forwards$`Height(cm)`)
str(Forwards)
head(Forwards)

#Split names into first and last names
Forwards %<>% separate(Player, c("First_Name", "Last_Name"), sep = ' ') 
Forwards$Last_Name = as.factor(Forwards$Last_Name)
Forwards$First_Name = as.factor(Forwards$First_Name)
head(Forwards)
str(Forwards)

#Mononymous players' first names as last names
nrow(Forwards)
for (i in seq(1,150)) {
  if(is.na(Forwards$Last_Name[i])) {
    Forwards$Last_Name[i] = Forwards$First_Name[i]
  }
}
Forwards

#Sort data frames by last name in alphabetical order A-Z
Forwards[order(Forwards$Last_Name,decreasing = FALSE),]
head(Forwards)

#--------------------------------------------------Data Cleaning & Mining Salary

head(Salary)
str(Salary)

#Separate Player into first name and last name
Salary %<>% separate(PLAYER, c("First_Name", "Last_Name"), sep = ' ') 
head(Salary)

#Drop rows with the word Forward in them
nrow(Salary)
for (j in seq(1,228)) {
  if(Salary$First_Name[j] == "FORWARD"){
  Salary = Salary[-j,]
  }
}
head(Salary)

#Drop Team Column
Salary$TEAM = NULL
head(Salary)

#Remove the pound sign and commas from the salaries  
Salary$ANNUAL.SALARY = gsub('[£,","]', ' ', Salary$ANNUAL.SALARY)

#Remove the white spaces
Salary$ANNUAL.SALARY = gsub(' ', '', Salary$ANNUAL.SALARY)
head(Salary)

#Change Salary to Numeric type
Salary$ANNUAL.SALARY = as.numeric(Salary$ANNUAL.SALARY)
str(Salary)

#Rename ANNUAL.SALARY column
colnames(Salary)[3] = "Salary(£)"
head(Salary)

#-------------------------------------------------------Data Clean Nationalities 

head(Nationalities)

#Drop players who are not forwards
Nationalities = Nationalities[Nationalities$Position == "Forward",]   
head(Nationalities)
unique(Nationalities$Position)

#Fix Player Name Column 
head(Nationalities)
Nationalities %<>% separate(Player, 
                            c("Photodrop", "fordrop","First_Name","Namedrop","Last_Name"), 
                            sep = ' ') 
head(Nationalities)
Nationalities = Nationalities[,c("First_Name","Last_Name","Position","Nationality")]
str(Nationalities)
head(Nationalities)

#-------------------------------------------------------Create merged data frame 

ForwardProfiles = merge.data.frame(Forwards,Salary, by = "Last_Name")
ForwardProfiles = merge.data.frame(ForwardProfiles,Nationalities, by = "Last_Name")
head(ForwardProfiles)

#Drop unnecessary columns 
ForwardProfiles$First_Name.y = NULL
ForwardProfiles$First_Name = NULL
ForwardProfiles$Position =NULL
colnames(ForwardProfiles)[2]  =  "First_Name"
head(ForwardProfiles)


#Drop Players who will earn zero pounds in the 2021-2022 season 
nrow(ForwardProfiles)
min(ForwardProfiles$`Salary(£)`)
ForwardProfiles = ForwardProfiles[ForwardProfiles$`Salary(£)` != 0,]
min(ForwardProfiles$`Salary(£)`)

#Add Age Column to Merged Data Frame
Playerage = as.numeric((Sys.Date()- ForwardProfiles$Birthdate)/365) 
ForwardProfiles$"Age" = round(Playerage,1)
head(ForwardProfiles)

#Rename team to current club column
colnames(ForwardProfiles)[3] = "Club"
head(ForwardProfiles)

#Remove Duplicate Rows
nrow(ForwardProfiles)
ForwardProfiles = ForwardProfiles[!duplicated(ForwardProfiles),]
head(ForwardProfiles)
nrow(ForwardProfiles)

#-----------------------------------------------------Clean Contracts Data Frame
library(tidyverse)
library(magrittr)
Contracts %<>% separate(Player, c("First_Name", "Last_Name"), sep = ' ') 
Contracts$...1 = NULL
Contracts$Average.Annual.Value = NULL
Contracts$First_Name = NULL
Contracts$Transfer.Fee = NULL
Contracts$Year.Signed = NULL

#Remove Pound Sterling Symbol and Commas from Contract Value

Contracts$Contract.Value = gsub('[£]', '', Contracts$Contract.Value)
Contracts$Contract.Value = gsub('[,]', '', Contracts$Contract.Value)

#Rename Contract Value column to include pount sign
colnames(Contracts)[5]= "Contract.Value(£)"

head(Contracts)

#-------------------------------------------Merge Contracts with Player Profiles
ForwardProfiles = merge.data.frame(ForwardProfiles,Contracts, by = "Last_Name")
head(ForwardProfiles)

colnames(ForwardProfiles)
nrow(ForwardProfiles)


#-----------------------------Merge Forward Profiles with Performance Data Frame

Performance$First_Name = NULL
ForwardProfiles = merge.data.frame(ForwardProfiles,Performance, by = "Last_Name")
head(ForwardProfiles)
colnames(ForwardProfiles)

ForwardProfiles$X = NULL
colnames(ForwardProfiles)

#---------------------------------------------Export Forward Profiles Data Frame
getwd()
write.csv(ForwardProfiles,
          file = "C:/Users/zara/Documents/Data Science Portfolio_Zara Clacken/Data Frames I Created/2021-2022 EPL Striker Profiles.csv", 
          row.names = FALSE)

#-----------------------------------------------------Prepare Data for Regressions

#Check  Variable Types of Forward Profiles Data Frame
str(ForwardProfiles)
colnames(ForwardProfiles)
head(ForwardProfiles)

#Create a list of columns to be converted to numeric 
numericvariablelist = c(5,6,8,9,10,11,14:21)

#For loop to convert specified variables into numeric variables 
for (v in seq(1,length(numericvariablelist))){
  ForwardProfiles[,numericvariablelist[v]] = as.numeric(ForwardProfiles[,numericvariablelist[v]])
}  
str(ForwardProfiles)

#Create Dominant Foot Dummy Variable: 1: Left 0: Right 
str(ForwardProfiles$Dominant.Foot)
ForwardProfiles$Dominant.Foot = as.factor(ForwardProfiles$Dominant.Foot)
ForwardProfiles$Foot.Dummy = ifelse(ForwardProfiles$Dominant.Foot == "Right",0, 1)
ForwardProfiles$Foot.Dummy = as.numeric(ForwardProfiles$Foot.Dummy)
str(ForwardProfiles$Foot.Dummy)

#Create column for ratio of matches started to matches played
ForwardProfiles$StartsMatchesRatio = ForwardProfiles$Starts..Prior.to.Contract./ForwardProfiles$Matches..Prior.to.Contract.
ForwardProfiles$StartsMatchesRatio

#Check column names 
colnames(ForwardProfiles)

#Create a list of the independent variables 
xvariableindex = c(5,9,14,16:22)

#Use a for loop to create a vector that has column name and a plus sign between them 
xvariables = c()
for (x in 1:length(xvariableindex)){
  indvar = c(colnames(ForwardProfiles)[xvariableindex[x]],"+")
  xvariables = c(contractxvariables,indvar)
}
as.factor(xvariables)

#Print x variables list without showing index numbers to copy and paste them into regression
cat(paste(contractxvariables))

  
#-----------------------------------------------------Contract Value Regressions  
Valueregression = lm(formula = `Contract.Value(£)` ~ `Height(cm)` + Age.at.Signing + 
                          Matches..Prior.to.Contract. + Minutes..Prior.to.Contract. + 
                          Goals..Prior.to.Contract. + SoT.Ratio..Prior.to.Contract. + 
                          Goal.Shot.Ratio..Prior.to.Contract. + Progressive.Distance..Yds..Prior.to.Contract. + 
                          Opponents.Dribbled.Past..Prior.to.Contract. + Foot.Dummy, data = ForwardProfiles)
summary(Valueregression)

#At alpha = 0.1, only Goals Scored and Matches Played are significant 
Valueregression2 = lm(formula = `Contract.Value(£)` ~ Goals..Prior.to.Contract. + Matches..Prior.to.Contract., data = ForwardProfiles)
summary(Valueregression2)

#In model 2, Matches Played is insignificant: Model 3 will be goals only 
Valueregression3 = lm(formula = `Contract.Value(£)` ~ Goals..Prior.to.Contract., data = ForwardProfiles)
summary(Valueregression3)

#At alpha = 0.1, the intercept and goals scored  are significant: thus model 3 is satisfactory


#--------------------------------------------------Contract Values v. Goals Plot
colnames(ForwardProfiles)
max(ForwardProfiles$Goals..Prior.to.Contract.)

#Create a vector of predicted contract values 
coef(Valueregression3) 
#Regression Equation: y = 11021388 + 1590007x 
predictedcontractvalues = as.vector(11021388 + (1590007*0:40))
predictedcontractvalues

#Create Plot 
par(mar = c(5, 5, 3, 1.5) + 0.1) 
plot(x = ForwardProfiles$Goals..Prior.to.Contract., y = ForwardProfiles$`Contract.Value(£)`/1e+06,
     xlab = "Goals (Season Prior to Contract Start)", ylab = "Contract Value (Millions £)",
     main = "Goals v. Contract Value" , 
     ylim = c(0,100), xlim = c(0,40), col = "Black", pch = 19, cex = 1.1, lwd = 2)
lines(x = 0:40, y = predictedcontractvalues/1e+06, col = "Blue",lwd = 3)
text(x=33.9, y=90, "y = 11,021,388 + 1,590,007x  ", col="Blue", font= 4, cex=0.8)

#------------------------------------------------------Predicted vs. Actual Plot

#Create a goals vector based on actual goals for predicted value calculations
ForwardProfiles$Goals..Prior.to.Contract.
Goalslist = sort(ForwardProfiles$Goals..Prior.to.Contract.)

#Predicted values based on actual goals 
Predicted_Values_by_Actual_Goals = as.vector(11021388 + (1590007*Goalslist))
Predicted_Values_by_Actual_Goals

#Actual contract values column ordered by goals scored column
Actual_Values_Goal_Order = as.vector(ForwardProfiles[order(ForwardProfiles$Goals..Prior.to.Contract.),"Contract.Value(£)"]) 
Actual_Values_Goal_Order

PredictedvActualRegression = lm(Actual_Values_Goal_Order ~ Predicted_Values_by_Actual_Goals)
summary(PredictedvActualRegression)


plot(x = Predicted_Values_by_Actual_Goals/1e+06, y = Actual_Values_Goal_Order/1e+06,
     xlab = "Predicted Contract Values", ylab = "Actual Contract Values",
     main = "Predicted v Actual" , 
     ylim = c(0,100), xlim = c(0,70), col = "Red", pch = 19, cex = 1.1, lwd = 2)
lines(x = 0:70, y = 0:70, col = "Blue",lwd = 3)
text(x= 70, y = 75, "y=x", col="Blue", font= 4, cex=0.8)


#-----------------------------------------------------------------Residuals Plot  

#Actual Y - Predicted Y 

ValuesResidual = as.vector(Actual_Values_Goal_Order - Predicted_Values_by_Actual_Goals)
ValuesResidual
max(ValuesResidual/1e+06)
min(ValuesResidual/1e+06)

plot(x = Goalslist, y = ValuesResidual/1e+06,
     xlab = "Goals Scored", ylab = "Residuals",
     main = "Contract Values Residual Plot" , 
     ylim = c(-75,75), xlim = c(0,35), col = "Red", pch = 19, cex = 1.1, lwd = 2)
lines(x = 0:35, y = rep(0,36), col = "Blue",lwd = 3, lty =3)
