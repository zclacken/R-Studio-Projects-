#------------------------------------------------------------------------Tasks
#Produce a chart showing TC and TR with Q on the horizontal axis. 
#Have Q go from 0 to 10,000 units. 
#Produce a second chart showing MC and MR with Q again on the horizontal axis.
#What is the optimal level of output for your company to produce/sell? 
#What are the total revenue, total cost, and profit at optimal number of units?

#-------------------------------------------------------------------Equations
#Total Cost: TC = 31,000 + 7Q + 0.002Q^2
#Marginal Cost: MC = 7 + 0.004Q  
#P and MR= $37
#TR = $37Q

#-----------------------------------------------------------------------Vectors 

#Create Vectors for Plots
Q = seq(0,10000,1)
TC = 31000 + 7*Q + 0.002*I(Q^2)
MC = 7 + 0.004*Q
TR = 37*Q
MR = rep(37,10001) #replicate 37 eleven times 
P = rep(37,10001) #replicate 37 eleven times
Profit = TR-TC 

#--------------------------------------------------------------------Data Frame
#Create a data frame from the vectors
RevenuesandCosts = data.frame(P,Q,TC,MC, TR,MR,Profit)
head(RevenuesandCosts)

#----------------------------------------------------------------------Plots

#Create TR and TC Plot Using Matplot 
?matplot
?legend 
matplot(RevenuesandCosts[ ,c("TR","TC")], type = "l", lty = 1,lwd = 3, 
        col = 3:1, xlab = "Output (Q)",ylab = "Dollars", 
        main = "Total Revenue v. Total Cost"
        )
legend("topleft", legend = c("TR","TC"), fill = 3:1,
       border = 3:1) 

#Create MR and MC Plot Using Matplot
matplot(RevenuesandCosts[,c("MR","MC")], type = "l", lty = 1, col = 3:1, 
        lwd = 3,main = "Marginal Revenue v. Marginal Cost", ylab = "Dollars", 
        xlab = "Output(Q)" )
legend("topleft", legend = c("MR","MC"),fill = 3:1, border = 3:1) 

#-----------------------------------------------------------Find Optimal Output

#Optimal Level of Output
optimalfilter = RevenuesandCosts$MC == RevenuesandCosts$MR
print(RevenuesandCosts[optimalfilter,c("Q","TR","TC","Profit")])

#----------------------------------------------------------Profit Plot
matplot(y=RevenuesandCosts$Profit,x = RevenuesandCosts$Q, ylab = "Profit",
        xlab = "Output (Q)", main = "Profit v. Output", col = 3)

