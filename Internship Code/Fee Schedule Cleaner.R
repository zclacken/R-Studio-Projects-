#import raw fee schedule and define it as fee_schedule 
fee_schedule = 
#paste list of desired columns  
utilized_columns = c(
)
#Script for cleaning  
Cleaned_Fee_Schedule = fee_schedule[,utilized_columns]
#Export as CSV 
write.csv(Cleaned_Fee_Schedule,".csv",row.names = FALSE)

