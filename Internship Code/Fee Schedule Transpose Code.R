fee_schedule = Enteral_K0553_K0554_Cleaned_Fee_Schedule
ncol(fee_schedule)
HCPCS = rep(fee_schedule$Hcpcs,92)
MOD1 = rep(fee_schedule$Mod1,92)
MOD2 = rep(fee_schedule$Mod2,92)
Rate_Plan_Stacked = stack(fee_schedule, select = 7:98)
Plan = Rate_Plan_Stacked$ind  
Rates = Rate_Plan_Stacked$values
transposed_df = data.frame(HCPCS,MOD1,MOD2,Plan,Rates)
write.csv(transposed_df,"Enteral_K0553_K0554_Fee_Schedule_Transposed.csv",row.names = FALSE)









