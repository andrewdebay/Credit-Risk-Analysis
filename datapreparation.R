loan_df = read.csv('/Users/andrewdebay/Documents/Semester 5/MGSC 401/Final Project/loan.csv')

attach(loan_df)
issue_d
View(loan_df)

chr_to_date_vars <- c("issue_d", "last_pymnt_d", "last_credit_pull_d","next_pymnt_d", "earliest_cr_line")

character_to_drop = c("annual_inc_joint", "dti_joint", "policy_code", "id", "member_id",
"emp_title", "url", "desc", "title", "open_acc_6m", "open_il_6m", 
"open_il_12m", "open_il_24m", "mths_since_rcnt_il", "total_bal_il", 
"il_util", "open_rv_12m", "open_rv_24m", "max_bal_bc", "all_util",
"total_rev_hi_lim", "inq_fi", "total_cu_tl", "inq_last_12m",
"verification_status_joint", "next_pymnt_d")

df_clean = loan_df[,!(names(loan_df) %in% character_to_drop)]
df_clean[["mths_since_last_delinq"]][is.na(df_clean[["mths_since_last_delinq"]])] <-0
df_clean[["mths_since_last_record"]][is.na(df_clean[["mths_since_last_record"]])] <-0
df_clean[["mths_since_last_major_derog"]][is.na(df_clean[["mths_since_last_major_derog"]])] <-0

##create a column with main_director_name counts 
df_clean = transform(df_clean, defaulted = loan_status)

##create the categorical variable with condition
df_clean$defaulted = ifelse(df_clean$loan_status == 'Default' , 'TRUE',
                                   ifelse(df_clean$loan_status ==  'Does not meet the credit policy. Status:Charged Off', 'TRUE',
                                          ifelse(df_clean$loan_status == 'In Grace Period', 'TRUE',
                                                 ifelse(df_clean$loan_status == 'Late (16-30 days)', 'TRUE',
                                                        ifelse(df_clean$loan_status == 'Late (31-120 days)', 'TRUE','FALSE')))))

table(df_clean$defaulted)/nrow(df_clean)

character_to_drop_2 = c("sub_grade", "loan_status")
df_clean = df_clean[,!(names(df_clean) %in% character_to_drop_2)]
df_clean = df_clean[complete.cases(df_clean), ]

install.packages("tidyverse")
library(ggplot2)
barplot(recoveries)
