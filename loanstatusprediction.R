attach(df)
View(df)
variables_to_drop = c('loan_status','sub_grade')

df_clean = df[,!(names(df) %in% variables_to_drop)]
df_clean = df_clean[complete.cases(df_clean), ]
attach(df_clean)
# organizing data 
df_clean$term=as.factor(df_clean$term) 
df_clean$grade=as.factor(df_clean$grade) 
df_clean$emp_length=as.factor(df_clean$emp_length) 
df_clean$home_ownership=as.factor(df_clean$home_ownership) 
df_clean$verification_status=as.factor(df_clean$verification_status) 
df_clean$pymnt_plan = as.factor(df_clean$pymnt_plan)
df_clean$purpose = as.factor(df_clean$purpose)
df_clean$addr_state = as.factor(df_clean$addr_state)
df_clean$application_type = as.factor(df_clean$application_type)
df_clean$initial_list_status = as.factor(df_clean$initial_list_status)

df_clean$default = as.factor(df_clean$default)
df_clean = na.omit(df_clean)
attach(df_clean)

table(default)

logit=glm(default ~ grade + loan_amnt, family = 'binomial')
summary(logit)

library(stargazer)
stargazer(logit, title="Logistic Regression Result", align=TRUE, type="html")

par(mfrow=c(1,1))


values = data.frame(grade = c('E','B','C'), loan_amnt = c(9,5,456789))
predict(logit, values, type="response")

# evaluating precision of this model
attach(df_clean)
#install.packages("rms")
require(rms)
mlogit2 =lrm( default ~ grade + loan_amnt)
mlogit2

# k -fold
require(caTools)
library(boot)

cv.error=rep(0,10)
for (i in 1:10) {
  logicall=glm(default ~ grade + loan_amnt, family = 'binomial')
  cv.error[i]=cv.glm(df_clean, logicall, K=10 )$delta[1]
}
cv.error
plot(cv.error, col="red")

which.min(cv.error)
min(cv.error)

# cross - val
sample=sample.split(df_clean$X1, SplitRatio=0.5)
train=subset(df_clean, sample==TRUE)
test=subset(df_clean, sample==FALSE)
mlogit2 =lrm( default ~ grade + loan_amnt, data = train)
mlogit2

mlogit2 =lrm( default ~ grade + loan_amnt, data = test)
mlogit2


#tree -base 
library(tree)
library(rpart)
library(rpart.plot)
attach(df_clean)

#tree code
myovefittedtree=rpart(default~loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + installment + grade + emp_length + home_ownership + annual_inc + verification_status  + pymnt_plan + purpose + zip_code + addr_state + dti + delinq_2yrs  + inq_last_6mths + mths_since_last_delinq + mths_since_last_record + open_acc + pub_rec + revol_bal +revol_util +total_acc + initial_list_status +out_prncp + out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + total_rec_int + total_rec_late_fee + recoveries +collection_recovery_fee  +last_pymnt_amnt  + collections_12_mths_ex_med +mths_since_last_major_derog + application_type + acc_now_delinq + tot_coll_amt +tot_cur_bal,control=rpart.control(cp=0.0001))
rpart.plot(myovefittedtree)
summary(myovefittedtree)

#getting the best cp
printcp(myovefittedtree)
plotcp(myovefittedtree)
myovefittedtree$cptable[which.min(myovefittedtree$cptable[,"xerror"]),"CP"]

# out of sample MSE
min(myovefittedtree$cptable[,'xerror'])
sqrt(min(myovefittedtree$cptable[,'xerror']))

# Random Forest 
attach(df_clean)
library(randomForest)

myforest=randomForest(default~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + installment + grade + emp_length + home_ownership + annual_inc + verification_status  + pymnt_plan + purpose  + addr_state + dti + delinq_2yrs  + inq_last_6mths + mths_since_last_delinq + mths_since_last_record + open_acc + pub_rec + revol_bal +revol_util +total_acc + initial_list_status +out_prncp + out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + total_rec_int + total_rec_late_fee + recoveries +collection_recovery_fee  +last_pymnt_amnt  + collections_12_mths_ex_med +mths_since_last_major_derog + application_type + acc_now_delinq + tot_coll_amt +tot_cur_bal  , ntree=50, data = df_clean, importance=TRUE)
(myforest)

#predicting using random forest if a new loan application will default 
predict(myforest,)


# getting the importance of variable 
importance(myforest)
varImpPlot(myforest)

#cross val 
myforest2=randomForest(default~grade+term, ntree=50, data = df_clean,do.trace =50, importance=TRUE)


#gradient boosting
library(gbm)
attach(df_clean)
set.seed (1)
boosted=gbm(default~grade+term,data=df_clean,distribution= "bernoulli",n.trees=10000, interaction.depth=4)
summary(boosted) 

gg = data.frame(grade = df_clean$grade, term = df_clean$term)

predicted_score=predict(boosted, newdata=gg, n.trees=10000)
actual_y = as.numeric(default)
mean((predicted_score - actual_y)^2) 

#PCA ANALSYIS
attach(df_clean)
loan_vars = data.frame(loan_amnt = loan_amnt,funded_amnt  = funded_amnt,funded_amnt_inv = funded_amnt_inv, int_rate = int_rate, installment = installment,annual_inc = annual_inc,dti = dti , delinq_2yrs = delinq_2yrs,inq_last_6mths = inq_last_6mths, mths_since_last_delinq = mths_since_last_delinq, mths_since_last_record = mths_since_last_record, open_acc = open_acc, pub_rec = pub_rec, revol_bal = revol_bal, revol_util = revol_util, total_acc = total_acc,out_prncp =out_prncp, out_prncp_inv = out_prncp_inv, total_pymnt = total_pymnt, total_pymnt_inv =total_pymnt_inv, total_rec_prncp = total_rec_prncp, total_rec_int = total_rec_int, total_rec_late_fee = total_rec_late_fee , recoveries = recoveries, collection_recovery_fee = collection_recovery_fee,last_pymnt_amnt = last_pymnt_amnt, collections_12_mths_ex_med = collections_12_mths_ex_med,  mths_since_last_major_derog = mths_since_last_major_derog, acc_now_delinq = acc_now_delinq, tot_coll_amt = tot_coll_amt, tot_cur_bal = tot_cur_bal)
loan_vars = data.frame(loan_amnt = loan_amnt,funded_amnt  = funded_amnt,funded_amnt_inv = funded_amnt_inv, int_rate = int_rate, installment = installment,annual_inc = annual_inc ,  msd = mths_since_last_delinq, mslr = mths_since_last_record,out_prncp =out_prncp, total_rec_prncp = total_rec_prncp, total_rec_int = total_rec_int, acc_now_delinq = acc_now_delinq)
loan_vars2 = data.frame(loan_amnt = loan_amnt, funded_amnt = funded_amnt, funded_amnt_inv = funded_amnt_inv)
pca2=prcomp(loan_vars, scale=TRUE)
pca2
autoplot(pca2, data = loan_vars, loadings = TRUE, loadings.label = TRUE )

pve=(pca2$sdev^2)/sum(pca2$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))
