#Import cleaned fee schedule file 
fee_schedule = 
#Count the number of columns in the fee schedule
ncol(fee_schedule)
#n is the number of plan/state columns  
HCPCS = rep(fee_schedule$Hcpcs,n)
MOD1 = rep(fee_schedule$Mod1,n)
MOD2 = rep(fee_schedule$Mod2,n)
#x is the index number of the first plan/state column; y is the index number of the final plan/state column 
Rate_Plan_Stacked = stack(fee_schedule, select = x:y)
Plan = Rate_Plan_Stacked$ind  
Rates = Rate_Plan_Stacked$values
transposed_df = data.frame(HCPCS,MOD1,MOD2,Plan,Rates)
write.csv(transposed_df,"Enteral_K0553_K0554_Fee_Schedule_Transposed.csv",row.names = FALSE)










