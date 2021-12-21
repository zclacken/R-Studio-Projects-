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
> #Model A: Robbery = f(%Metro, %Poverty, Rain)
> ModelA <- lm(Crimestats$Robbery ~ Crimestats$`Metro(%)` + Crimestats$`Poverty(%)` + Crimestats$Rain)
> summary(ModelA)

Call:
lm(formula = Crimestats$Robbery ~ Crimestats$`Metro(%)` + Crimestats$`Poverty(%)` + 
    Crimestats$Rain)

Residuals:
    Min      1Q  Median      3Q     Max 
-49.133  -8.861   0.046   8.559  81.578 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)             -84.9664    22.6413  -3.753 0.000509 ***
Crimestats$`Metro(%)`   128.6981    19.2060   6.701 3.14e-08 ***
Crimestats$`Poverty(%)`   4.8139     1.3490   3.568 0.000882 ***
Crimestats$Rain          -0.3085     0.2527  -1.221 0.228594    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 22.55 on 44 degrees of freedom
Multiple R-squared:  0.5367,	Adjusted R-squared:  0.5051 
F-statistic: 16.99 on 3 and 44 DF,  p-value: 1.787e-07

> 
> #Model B: Robbery = f(Population, %SameHouse, MedianAge)
> ModelB <- lm(Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + Crimestats$MedianAge)
> summary(ModelB)

Call:
lm(formula = Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + 
    Crimestats$MedianAge)

Residuals:
    Min      1Q  Median      3Q     Max 
-39.359 -16.742  -4.941  10.871  89.740 

Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)               -1.251e+02  1.747e+02  -0.716 0.477607    
Crimestats$Population      2.169e-06  5.483e-07   3.956 0.000273 ***
Crimestats$`SameHouse(%)`  3.298e+00  2.399e+00   1.375 0.176165    
Crimestats$MedianAge      -2.853e+00  1.988e+00  -1.435 0.158396    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.4 on 44 degrees of freedom
Multiple R-squared:  0.3649,	Adjusted R-squared:  0.3216 
F-statistic: 8.426 on 3 and 44 DF,  p-value: 0.0001551

> 
> #Model C: Robbery = f(Population, %SameHouse, MedAgeMale)
> ModelC <- lm(Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + Crimestats$MedAgeMale)
> summary(ModelC)

Call:
lm(formula = Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + 
    Crimestats$MedAgeMale)

Residuals:
   Min     1Q Median     3Q    Max 
-41.13 -13.90  -5.35  11.95  88.67 

Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)               -1.065e+02  1.698e+02  -0.627 0.533639    
Crimestats$Population      2.113e-06  5.402e-07   3.912 0.000314 ***
Crimestats$`SameHouse(%)`  3.347e+00  2.243e+00   1.492 0.142909    
Crimestats$MedAgeMale     -3.600e+00  1.956e+00  -1.840 0.072531 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.03 on 44 degrees of freedom
Multiple R-squared:  0.3827,	Adjusted R-squared:  0.3406 
F-statistic: 9.091 on 3 and 44 DF,  p-value: 8.482e-05

> 
> 
> #Model D: Robbery = f(Population, %SameHouse, MedAgeFemale)
> ModelD <- lm(Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + Crimestats$MedAgeFemale)
> summary(ModelD)

Call:
lm(formula = Crimestats$Robbery ~ Crimestats$Population + Crimestats$`SameHouse(%)` + 
    Crimestats$MedAgeFemale)

Residuals:
    Min      1Q  Median      3Q     Max 
-41.282 -17.875  -5.234  11.341  90.626 

Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)               -1.302e+02  1.763e+02  -0.739 0.464047    
Crimestats$Population      2.188e-06  5.482e-07   3.990 0.000246 ***
Crimestats$`SameHouse(%)`  3.259e+00  2.429e+00   1.342 0.186618    
Crimestats$MedAgeFemale   -2.586e+00  1.908e+00  -1.355 0.182212    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.46 on 44 degrees of freedom
Multiple R-squared:  0.3618,	Adjusted R-squared:  0.3183 
F-statistic: 8.315 on 3 and 44 DF,  p-value: 0.0001718

> 
> #Model E: Robbery = f(CarTheft, %Unemploy, %Bachelors+, Temp)
> ModelE <- lm(Crimestats$Robbery ~ Crimestats$CarTheft + Crimestats$`Unemployed(%)` + Crimestats$`Bachelors+(%)`+Crimestats$Temp)
> summary(ModelE)

Call:
lm(formula = Crimestats$Robbery ~ Crimestats$CarTheft + Crimestats$`Unemployed(%)` + 
    Crimestats$`Bachelors+(%)` + Crimestats$Temp)

Residuals:
    Min      1Q  Median      3Q     Max 
-31.746 -13.267  -5.458  12.660  66.981 

Coefficients:
                             Estimate Std. Error t value Pr(>|t|)    
(Intercept)                -204.88318   41.07351  -4.988 1.05e-05 ***
Crimestats$CarTheft           0.11456    0.03674   3.118 0.003241 ** 
Crimestats$`Unemployed(%)`   17.36836    4.94637   3.511 0.001061 ** 
Crimestats$`Bachelors+(%)`  263.53442   67.39320   3.910 0.000322 ***
Crimestats$Temp               1.84978    0.48045   3.850 0.000387 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 21.91 on 43 degrees of freedom
Multiple R-squared:  0.5725,	Adjusted R-squared:  0.5327 
F-statistic: 14.39 on 4 and 43 DF,  p-value: 1.55e-07

> 
> #Model F: Robbery = f(CarTheft, %Unemploy, %Bachelors+, Sun)
> ModelF <- lm(Crimestats$Robbery ~ Crimestats$CarTheft + Crimestats$`Unemployed(%)`+Crimestats$`Bachelors+(%)`+Crimestats$Sun)
> summary(ModelF)

Call:
lm(formula = Crimestats$Robbery ~ Crimestats$CarTheft + Crimestats$`Unemployed(%)` + 
    Crimestats$`Bachelors+(%)` + Crimestats$Sun)

Residuals:
    Min      1Q  Median      3Q     Max 
-44.395 -17.724  -6.372  14.278  75.472 

Coefficients:
                            Estimate Std. Error t value Pr(>|t|)    
(Intercept)                -128.5383    40.3388  -3.186 0.002682 ** 
Crimestats$CarTheft           0.1245     0.0465   2.677 0.010481 *  
Crimestats$`Unemployed(%)`   21.5975     5.5101   3.920 0.000313 ***
Crimestats$`Bachelors+(%)`  212.9705    75.1929   2.832 0.007005 ** 
Crimestats$Sun                0.1852     0.1547   1.197 0.237967    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 25 on 43 degrees of freedom
Multiple R-squared:  0.4436,	Adjusted R-squared:  0.3918 
F-statistic: 8.571 on 4 and 43 DF,  p-value: 3.536e-05

> 
> #Model G: Robbery = f(CarTheft, %Unemploy, %Bachelors+, Rain)
> ModelG <- lm(Crimestats$Robbery ~ Crimestats$CarTheft+ Crimestats$`Unemployed(%)`+Crimestats$`Bachelors+(%)`+ Crimestats$Rain)
> summary(ModelG)

Call:
lm(formula = Crimestats$Robbery ~ Crimestats$CarTheft + Crimestats$`Unemployed(%)` + 
    Crimestats$`Bachelors+(%)` + Crimestats$Rain)

Residuals:
    Min      1Q  Median      3Q     Max 
-38.761 -13.581  -2.809  14.060  71.095 

Coefficients:
                             Estimate Std. Error t value Pr(>|t|)    
(Intercept)                -142.26322   38.25547  -3.719 0.000575 ***
Crimestats$CarTheft           0.18833    0.04133   4.557 4.26e-05 ***
Crimestats$`Unemployed(%)`   19.67877    5.30440   3.710 0.000590 ***
Crimestats$`Bachelors+(%)`  222.65438   71.79501   3.101 0.003397 ** 
Crimestats$Rain               0.63848    0.26301   2.428 0.019460 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 23.83 on 43 degrees of freedom
Multiple R-squared:  0.4944,	Adjusted R-squared:  0.4473 
F-statistic: 10.51 on 4 and 43 DF,  p-value: 4.989e-06



