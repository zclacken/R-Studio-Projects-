#(Scenario: You are a Data Scientist working for a consulting firm. One of your
#colleagues from the Auditing department has asked you to help them assess the
#financial statement of organisation X.
#You have been supplied with two vectors of data: monthly revenue and monthly
#expenses for the financial year in question. Your task is to calculate the following
#financial metrics:
#  - profit for each month
#- profit after tax for each month (the tax rate is 30%)
#- profit margin for each month - equals to profit a after tax divided by revenue
#- good months - where the profit after tax was greater than the mean for the year
#- bad months - where the profit after tax was less than the mean for the year
#- the best month - where the profit after tax was max for the year
#- the worst month - where the profit after tax was min for the year)#  


#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Solution

#Profits
profit <- round(revenue - expenses)
profit

#Profits after tax
proftax <- round(profit - profit*.30) 
proftax

#Profit Margin
profmargin <- round((proftax/revenue)*100)
profmargin

#Mean of Profit after Tax
meanprofit <- mean(proftax)

#Good Months and Bad Months
pt2 <- round(profit - profit*.30) 
GoodorBadMonth<- vector(mode = 'character', length = length(pt2))
for(i in 1:12){
  if(pt2[i] > meanprofit)
  {pt2[i] <-'good month'}
  else
  {pt2[i]<-'bad month'}
  GoodorBadMonth[i] <- pt2[i]
} 
GoodorBadMonth 


#Best Month and Worst Month Profit After Tax 
pt3 <- round(profit - profit*.30) 
pt3
maxprofit <- max(proftax)
minprofit <- min(proftax)
BestandWorst <- vector(mode = 'character',length = length(pt3))
for(j in 1:12){
  if(pt3[j] == maxprofit)
  {pt3[j] <- "Best"}
  else if(pt3[j] == minprofit)
  {pt3[j] <- "Worst"}
  else
  {pt3[j] <- "X"}
   BestandWorst[j] <- pt3[j]}
BestandWorst


#Solution Vectors 
#1 Profits 
profit
#2 Profits after Tax
proftax
#3 Profit Margins
profmargin
#4Good Months and Bad Months
GoodorBadMonth
#5Best Month and Worst Month 
BestandWorst


