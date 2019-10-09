---
  title: "R Notebook"
output: html_notebook
---
  
  This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.

```{r}

library(tidyverse)
library(lubridate)
library(ggplot2)
```


# The lcData6m.csv file contains data on 3 year loans issues in the first 6 months of 2015, which we will use for this analyses

```{r}

lcdf <- read.csv("C:/Users/LENOVO/Desktop/2018 vansh industrial/data mining/lcData6m.csv")
View(lcdf)
dim(lcdf)
```


#Data Exploration


```{r}

#How does loan status vary by loan grade
lcdf %>% group_by(loan_status, grade) %>% tally()
table(lcdf$loan_status, lcdf$grade)

#Filtering out the loans with 'Current' Status
lcdf <- lcdf %>%  filter(loan_status !="Current")

#How does loan status vary by loan sub grade
lcdf %>% group_by(loan_status, sub_grade) %>% tally()
table(lcdf$loan_status, lcdf$sub_grade)

#Default loans are the loans that are Charged off.
#Plot for Variation by Grade for Default Loans
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt))
ggplot(lcdf,aes(x = grade, fill = loan_status))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack())+ggtitle("Variation by Grade for Default Loans")+theme(plot.title = element_text(hjust = 0.5))
ggsave("Variation by Grade for Default Loans.png", width = 5, height = 5)

#Plot for Variation by Subgrade for Default Loans
lcdf %>% group_by(sub_grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt))
ggplot(lcdf,aes(x = sub_grade, fill = loan_status))+geom_bar()+ggtitle("Variation by Subgrade for Default Loans")+theme(plot.title = element_text(hjust = 0.5))
ggsave("Variation by Subgrade for Default Loans.png", width = 5, height = 5)

#Number of loans for each category of loans
lcdf %>% group_by(grade) %>% tally()
ggplot(lcdf,aes(x = grade, fill = grade))+geom_bar()+geom_text(aes(label=..count..),stat="count",position=position_stack())+ggtitle("No. of loans for each category of grade")+theme(plot.title = element_text(hjust = 0.5))
ggsave("No. of loans for each category of grade.png", width = 5, height = 5)


#Relationship between Loan Amount and Grade
lcdf %>% group_by(grade) %>% summarise(sum(loan_amnt))
lcdf %>% group_by(grade) %>% summarise(mean(loan_amnt))
ggplot(lcdf, aes(x=grade, y=loan_amnt ,fill=grade)) + geom_boxplot() + ggtitle("Variation of loan amount with grade")+theme(plot.title = element_text(hjust = 0.5))
ggsave("Variation of loan amount with grade.png", width = 5, height = 5)
ggplot(lcdf, aes( x = loan_amnt)) + geom_histogram(aes(fill=grade))
ggplot(lcdf, aes( x = loan_amnt)) + geom_histogram() + facet_wrap(~loan_status)



#Relationship between Grade and Interest Rate
lcdf %>% group_by(grade) %>% summarise(mean(int_rate))
ggplot(lcdf, aes( x = int_rate)) + geom_histogram()
ggsave("Histogram Plot for interest rate variation with grade.png", width = 5 , height = 5)


#Relationship between Subgrade and Interest Rate
lcdf %>% group_by(sub_grade) %>% summarise(mean(int_rate))


#Indentifying the purpose for which borrowers are taking the loans
#For exploration lets use the table function to get a rough estimate
table(lcdf$purpose)

#Formulating the result in a better tabulated format
lcdf %>% group_by(purpose) %>% tally()
ggplot(lcdf,aes(x = purpose, fill = loan_status))+geom_bar()+ggtitle("Variation of loans by purpose")+theme(plot.title = element_text(hjust = 0.5))
ggsave("Variation of loans by purpose.png", height = 5, width = 5)

#Relationship between Defaults and Purpose
lcdf %>% group_by(purpose) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt))

#calculate the annualized percentage return
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet))

#For loans that are paid back early - find out the actual loan term in months
#  Since last_pymnt_d is a chr variable, we need to convert it to a date variable
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")

lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)

#Then, considering this actual term, we can calculate the actual annual return
lcdf$actualReturn <-(((lcdf$total_pymnt-lcdf$funded_amnt)/lcdf$funded_amnt)/lcdf$actualTerm)*(12/36)*100

#For cost-based performance, we want to see the average interest rate, and the average of proportion of loan amount paid back, grouped by loan_status
lcdf%>% group_by(loan_status) %>% summarise(intRate=mean(int_rate), totRet=mean((total_pymnt-funded_amnt)/funded_amnt))

# Notice that the totRet on Charged Off loans as -0.366, so, for every dollar invested, there is a loss of .366 cents. The totRet seems less than what we may expected because we assume that the borrowers will use the full loan term to pay back, however some borrowers paid back much faster and much before time than what was utilised to calculate their interest rate. Thus since some loans were paid before time, their expected interest rate will differ from the total return

#summary of actualReturn by Grade
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), default_rate = (sum(loan_status=="Charged Off")/n()), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgactualRet=mean(actualReturn), stdactualRet=sd(actualReturn), minactualRet=min(actualReturn),maxactualRet=max(actualReturn))

#summary of actualReturn by Subgrade
lcdf %>% group_by(sub_grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), default_rate = (sum(loan_status=="Charged Off")/n()), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgactualRet=mean(actualReturn), stdactualRet=sd(actualReturn), minactualRet=min(actualReturn), maxactualRet=max(actualReturn))



#Tabulated from of actualTerm and actualReturn
lcdf %>% select(loan_status, loan_amnt, funded_amnt, total_pymnt, int_rate, actualTerm, actualReturn) %>% View()
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans)


#convert emp_length to factor -- can order the factors in  a meaningful way
lcdf$emp_length <- factor(lcdf$emp_length, levels=c("n/a", "< 1 year","1 year","2 years", "3 years" ,  "4 years",   "5 years",   "6 years",   "7 years" ,  "8 years", "9 years", "10+ years" ))


#Note - character variables can cause a problem with some model packages, so better to convert all of these to factors
lcdf= lcdf %>% mutate_if(is.character, as.factor)

```


#Deriving new attributes


```{r}
dim(lcdf)

#We generate some derived attributes that may be useful for predicting default
#The first derived attribute is the proportion of satisfactory bankcard accounts
lcdf$propSatisBankcardAccts <- ifelse(lcdf$num_bc_tl>0, lcdf$num_bc_sats/lcdf$num_bc_tl, 0)


#The second derived attribute is to calculate the length of borrower's history with LC
#  i.e time between earliest_cr_line and issue_d
lcdf$earliest_cr_line<-paste(lcdf$earliest_cr_line, "-01", sep = "")
lcdf$earliest_cr_line<-parse_date_time(lcdf$earliest_cr_line, "myd")
tail(lcdf$earliest_cr_line)


tail(lcdf$issue_d)
as.Date(lcdf$issue_d)
lcdf$issue_d <- as.Date(lcdf$issue_d)

#or we can use the lubridate functions to precidely handle date-times durations
lcdf$borrHistory <- as.duration(lcdf$earliest_cr_line %--% lcdf$issue_d) /365
tail(lcdf$borrHistory)

#Another new attribute: ratio of openAccounts to totalAccounts
lcdf$ratio_open_to_totalac<-c(lcdf$open_acc/lcdf$total_acc)
tail(lcdf$ratio_open_to_totalac)

# Another new attribute: proportion of funded_amnt to installments
lcdf$prop_funded_install <- amnt <- lcdf$funded_amnt/lcdf$installment
tail(lcdf$prop_funded_install)

# Another new attribute: current balance per active account
lcdf$cur_bal_open_acc <- lcdf$tot_cur_bal/lcdf$open_acc
tail(lcdf$cur_bal_open_acc)

# Another new attribute: proportion of funded_amnt to installments
lcdf$per_install_amnt <- lcdf$funded_amnt/lcdf$installment
tail(lcdf$per_install_amnt)

dim(lcdf)

colnames(lcdf)

```


#Understanding the missing values


```{r}

#Step 1: Dropping all variables wih empty values
lcdf <- lcdf %>% select_if(function(x){!all(is.na(x))})

#missing value proportions in each column
colMeans(is.na(lcdf))

#Step 2: Dropping only those columns where there are missing values
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

#Step 3: Dropping variables which have more than 60% missing values
nm<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
lcdf <- lcdf %>% select(-nm)

#Step 4: Evaluating which remaining columns have missing values
missing_value_col <- names(colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0])
missing_value_col

#Step 5: Removing two variables popping up
drop <- c("emp_title","last_credit_pull_d")
lcdf = lcdf[,!(names(lcdf) %in% drop)]
missing_value_col <- names(colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0])
length(missing_value_col)

#Step 6: Replace the remaining variables popping up

#Summarise each remaining variable and replace with suitable replacements like mean or median depending on judgement
summary(lcdf$mths_since_last_delinq)
summary(lcdf$bc_util)
summary(lcdf$revol_util)
summary(lcdf$bc_open_to_buy)
summary(lcdf$mo_sin_old_il_acct)
summary(lcdf$mths_since_recent_bc)
summary(lcdf$mths_since_recent_inq)
summary(lcdf$num_tl_120dpd_2m)
summary(lcdf$percent_bc_gt_7)


lcdf<- lcdf %>% replace_na(list(mths_since_last_delinq=250, bc_util=median(lcdf$bc_util, na.rm=TRUE), revol_util=median(lcdf$revol_util, na.rm=TRUE), bc_open_to_buy=median(lcdf$bc_open_to_buy, na.rm = TRUE), mo_sin_old_il_acct=0, mths_since_recent_bc=0, mths_since_recent_inq=0, num_tl_120dpd_2m=0, percent_bc_gt_75=50))

```

#Tackling Data Leakage

```{r}

varstoremove1 <- c("funded_amnt_inv", "term", "emp_title","issue_d", "earliest_cr_line", "application_type", "hardship_flag", "home_ownership", "pymnt_plan", "out_prncp", "out_prncp_inv", "last_credit_pull_d", "policy_code", "disbursement_method", "debt_settlement_flag", "pub_rec", "pub_rec_bankruptcies", "tax_liens", "delinq_amnt", "collections_12_mths_ex_med", "mths_since_last_major_derog", "acc_now_delinq", "total_il_high_credit_limit", "tot_cur_bal", "collection_recovery_fee", "recoveries","total_pymnt", "acc_open_past_24mths","total_bc_limit", "total_rec_prncp", "bc_open_to_buy", "bc_util", "num_bc_tl", "num_rev_tl_bal_gt_0", "num_il_tl", "num_sats", "num_tl_op_past_12m", "num_op_rev_tl","num_actv_rev_tl", "last_pymnt_d", "actualReturn", "actualTerm", "mths_since_recent_revol_delinq", "zip_code", "title", "addr_state", "total_rev_hi_lim","total_rec_int", "last_pymnt_amnt","mths_since_recent_bc_dlq","annRet", "totRet","chargeoff_within_12_mths", "total_rec_late_fee", "pct_tl_nvr_dlq" , "cur_bal_open_acc", "prop_funded_install", "ratio_open_to_totalac")

lcdf <- lcdf %>% select(-one_of(varstoremove1))
dim(lcdf)


#converting emp_length to appropriate format
lcdf$emp_length <- as.numeric(gsub("\\D", "", lcdf$emp_length))

summary(lcdf$emp_length)
#imputing missing value
lcdf <- lcdf %>% replace_na(list(emp_length=median(lcdf$emp_length, na.rm=TRUE)))
summary(lcdf$emp_length)

```


#Adding certain variables back


```{r}
library(caret)

dcorplot <- c("initial_list_status","loan_status","purpose","verification_status","sub_grade","grade")

mvcol <- names(colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]) #mths_since_recent_revol_delinq
length(mvcol)

lcdfn <- lcdf[,!(names(lcdf) %in% dcorplot)]

sapply(lcdfn, is.factor)

correlation_matrix <-  cor(lcdfn)

#cor(lcdfn[sapply(lcdfn, function(x) !is.factor(x))])


corr_var <- findCorrelation(correlation_matrix, cutoff = 0.8, verbose = TRUE, names = TRUE,  exact = ncol(correlation_matrix) < 100)

#drop_for_corplot_dataframe <- data.frame(lcdf$debt_settlement_flag,lcdf$initial_list_status)

#Adding back the dcorplot variables for model

lcdfn <- lcdfn[,!(names(lcdfn) %in% corr_var)]
lcdf1 <- data.frame(lcdfn,lcdf[, dcorplot])
colnames(lcdf)

```


#Rpart Decision Tree


```{r}

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(C50)

#It can be useful to convert the target variable, loan_status to  a factor variable. If we dont then it can give us trouble later on
lcdf1$loan_status <- factor(lcdf$loan_status, levels=c("Fully Paid", "Charged Off"))
sapply(lcdf1,class)

#Changing categorical variables into factors

lcdf1$initial_list_status  = as.factor(lcdf1$initial_list_status)
lcdf1$loan_status = as.factor(lcdf1$loan_status)
lcdf1$purpose = as.factor(lcdf1$purpose)
lcdf1$verification_status = as.factor(lcdf1$verification_status)
lcdf1$sub_grade = as.factor(lcdf1$sub_grade)
lcdf1$grade = as.factor(lcdf1$grade)

#split the data into Training = trn, Testing = tst subsets. We have taken a split of 70-30.
set.seed(6746)
nr<-nrow(lcdf1)
trnIndex<- sample(1:nr, size = round(0.7*nr), replace=FALSE)
lcdf1Trn <- lcdf1[trnIndex, ]
lcdf1Tst <- lcdf1[-trnIndex, ]


#testing dataset split
tail(lcdf1)
dim(lcdf1Tst)
tail(lcdf1Tst)
dim(lcdf1Trn)
tail(lcdf1Trn)    

#For Information gain


library(rattle)
lcDT1 <- rpart(loan_status ~., data=lcdf1Trn, method="class", parms = list(split = "information"), control = rpart.control(cp=0.0001, minsplit = 50))
fancyRpartPlot(lcDT1)
rpart.plot(lcDT1)
pdf("dtree.pdf")


#Do we want to prune the tree -- check for performance with dfferent cp levels
printcp(lcDT1)
lcDT1$cptable[which.min(lcDT1$cptable[,"xerror"]),"CP"]
plotcp(lcDT1)
lcDT1p<- prune.rpart(lcDT1, cp=0.0001)
fancyRpartPlot(lcDT1p)

predTrn=predict(lcDT1,lcdf1Trn, type='class')
table(pred = predTrn, true=lcdf1Trn$loan_status)

predTst <- predict(lcDT1,lcdf1Tst, type='class')
table(pred = predict(lcDT1,lcdf1Tst, type='class'), true=lcdf1Tst$loan_status)

#With a different classsification threshold
CTHRESH=0.6
predProbTrn=predict(lcDT1,lcdf1Trn, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table(predTrnCT , true=lcdf1Trn$loan_status)
predProbTrn<-cbind(predProbTrn, lcdf1Trn$loan_status)
predProbTrn[1:50,]

predProbTst<-cbind(predProbTst, lcdf1Tst$loan_status)
predProbTst[1:50,]

confusionMatrix(predTrn, lcdf1Trn$loan_status, positive="Charged Off")
confusionMatrix(predTst, lcdf1Tst$loan_status, positive="Charged Off")

#ROC plot
library(ROCR)
score=predict(lcDT1,lcdf1Tst, type="prob")[,"Charged Off"]
pred_2=prediction(score, lcdf1Tst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels  

#ROC curve
aucPerf <-performance(pred_2, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)


#AUC value
aucPerf=performance(pred_2, "auc")
aucPerf@y.values


#Lift curve
liftPerf <-performance(pred_2, "lift", "rpp")
plot(liftPerf)
dim(lcdf1)

#library(ROSE)

#loans.oversampled <- ovun.sample(loan_status~., data=lcdf1Trn, method = "over",  seed = 15)$data
#table(loans.oversampled$loan_status)
#tune <- data.frame(0.001)
#colnames(tune) <- "cp"
#tr_control <- trainControl(method = "cv",number = 10, verboseIter = TRUE)
#loans.rpart.oversampled <- train(loan_status ~., data = loans.oversampled, method = "rpart", trControl = tr_control, #tuneGrid = tune, control=rpart.control(minsplit=10, minbucket = 3))
#fancyRpartPlot(loans.rpart.oversampled$finalModel)
#confusionMatrix(predict(loans.rpart.oversampled, lcdf1Tst), lcdf1Tst$loan_status)
library(RColorBrewer)
library(rpart.plot)
library(rpart)

install.packages("rattle", repos="https://rattle.togaware.com", type="source")
library(rattle)



```


#C50 Decision Tree


```{r}

#Building decision tree model using c50
library(C50)


#It can be useful to convert the target variable, loan_status to  a factor variable
lcdf1Trn$loan_status <- factor(lcdf1Trn$loan_status, levels=c("Fully Paid", "Charged Off"))
lcdf1Tst$loan_status <- factor(lcdf1Tst$loan_status, levels=c("Fully Paid", "Charged Off"))
lcdf1Trn$loan_status <-as.factor(lcdf1Trn$loan_status)
lcdf1Tst$loan_status <- as.factor(lcdf1Tst$loan_status)
class(lcdf1Trn$loan_status)
colnames(lcdf1Trn)

lcdf_Trn_c50<- C5.0(x=lcdf1Trn~., data =lcdf1Trn, method="class", parms= list(split='information'))

lcdf_Trn_c50<- C5.0(x=lcdf1Trn, y=lcdf1Trn$loan_status, weights = 100)



str(lcdf_Trn_c50)

levels(lcdf_Trn_c50$)[1] = "missing"


summary(lcdf1Trn)

#Replace rpart function by C5.0 to build the tree
plot(lcdf_Trn_c50,subtree=1)

#Making Predictions
predictions=predict(lcdf_Trn_c50, data = lcdf1Trn, type='class')

#Summarizing the accuracy train
table(predictions, lcdf1Trn$loan_status)
plot(lcdf_Trn_c50)

confusionMatrix( table(predictions, lcdf1Trn$loan_status) )
table(pred = predTrn_whole, true=lcdf1Trn$lcdf1Trn~.)
confusionMatrix( table(predictions_tst, lcdf1Tst$loan_status) )

#Accuracy
mean(predictions==lcdf1Trn$lcdf1Trn~.)

library(ROCR)
#score test
lcdf1Trn$score<-predict(lcdf_Trn_c50,type='prob',lcdf1Tst)
pred <-prediction(lcdf1Trn$score[,2],lcdf1Trn$lcdf1Trn~.)
perf <- performance(pred,"tpr","fpr")
plot(perf, main="ROC Curve")

```

#Modelling Random Forest

```{r}

#Building random forest model

library('randomForest')

#for reproducible results, set a specific value for the random number seed
set.seed(900)
lcdfRF <- lcdf1
levels(lcdfRF$loan_status) <- c(0,1)

#dividing in training n testing
set.seed(1784)
nr<-nrow(lcdfRF)
trnIndex<- sample(1:nr, size = round(0.7*nr), replace=FALSE)
lcdfRF_Trn <- lcdfRF[trnIndex, ]
lcdfRF_Tst <- lcdfRF[-trnIndex, ]

#Developing Model1 with 200 Trees
rfM1 = randomForest(loan_status~., data=lcdfRF_Trn, ntree=200, importance=TRUE )
rfM1

# Predicting on Validation set
PV1 <- predict(rfM1, lcdfRF_Tst, type = "class")

# Looking for classification accuracy for Model 1
Acc1 <- mean(PV1 == lcdfRF_Tst$loan_status)                    
table(PV1,lcdfRF_Tst$loan_status)

#Developing Model2 with 200 Trees with additional condition
rfM2 = randomForest(loan_status~., data=lcdfRF_Trn, ntree=200, mtry = 6, importance=TRUE )
rfM2

# Predicting on Validation set
PV2 <- predict(rfM2, lcdfRF_Tst, type = "class")

# Looking for classification accuracy for Model 2
Acc2 <- mean(PV2 == lcdf_rf_Tst$loan_status)                    
table(PV2,lcdfRF_Tst$loan_status)

#Developing Model3 with 100 Trees with additional condition
rfM3 = randomForest(loan_status~., data=lcdfRF_Trn, ntree=100, importance=TRUE )

# Predicting on Validation set
PV3 <- predict(rfM3, lcdfRF_Tst, type = "class")

# Looking for classification accuracy for Model 3
Acc3 <- mean(PV3 == lcdfRF_Tst$loan_status)                    
table(PV3,lcdfRF_Tst$loan_status)

# To check important variables
importance(rfM1)        
varImpPlot(rfM1)

importance(rfM2)        
varImpPlot(rfM2)

importance(rfM3)        
varImpPlot(rfM3)

#Draw the ROC curve for the randomForest model
perfROC_rfTst=performance(prediction(predict(rfM2,lcdfRF_Tst, type="prob")[,2], lcdfRF_Tst$loan_status), "tpr", "fpr")
plot(perfROC_rfTst)

#Draw the lift curve fr the random forest model
perfLift_rfTst=performance(prediction(predict(rfM2,lcdfRF_Tst, type="prob")[,2], lcdfRF_Tst$loan_status), "lift", "rpp")
plot(perfLift_rfTst)

#ROC curves for the decision-tree model and the random forest model in the same plot

perfROC_dt1Tst=performance(prediction(predict(lcDT1,lcdf1Tst, type="prob")[,2], lcdf1Tst$loan_status), "tpr", "fpr")

perfRoc_rfTst=performance(prediction(predict(rfM3,lcdfRF_Tst, type="prob")[,2], lcdfRF_Tst$loan_status), "tpr", "fpr")

plot(perfROC_dt1Tst, col='red')
plot(perfRoc_rfTst, col='green', add=TRUE)
legend('bottomright', c('DecisionTree', 'RandomForest'), lty=1, col=c('red', 'green'))

```

#Modelling for GBM

```{r}
instlall.packages(gbm)
#Dividing the Training and Testing models
set.seed(8945)
head(lcdf1$loan_status)
levels(lcdf1$loan_status)

dim(lcdf1)
colnames(lcdf1)

#Variable has 1 for Fully Paid and 0 for Charged off
head(unclass(lcdf1$loan_status))
head(unclass(lcdf$loan_status)-1)
lcdfgbm <- lcdf1
nr<-nrow(lcdfgbm)
trnIndex<- sample(1:nr, size = round(0.7*nr), replace=FALSE)
lcdfgbmTrn <- lcdf1[trnIndex, ]
lcdfgbmTst <- lcdf1[-trnIndex, ]
lcdfgbmTrn <- lcdfgbmTrn[,-41]
lcdfgbmTst <- lcdfgbmTst[,-41]

lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100

lcdfgbm1<- gbm(formula = unclass(loan_status)-1 ~ .,data = subset(lcdfgbmTrn, select= -c(annRet)), distribution = "gaussian",  n.trees = 4000, interaction.depth = 1, shrinkage = 0.001, cv.folds = 5, n.cores = NULL,verbose = FALSE)
print(lcdfgbm1)
# find index for n trees with minimum CV error
min_MSE <- which.min(lcdfgbm1$cv.error)
min_MSE
#calculating minimum RMSE
sqrt(min(lcdfgbm1$cv.error))

# plot loss function as a result of n trees added to the ensemble
gbm.perf(lcdfgbm1, method = 'cv')
Bestler <- gbm.perf(lcdfgbm1, method='cv')
summary(lcdfgbm1)
summary(lcdfgbm1, method=permutation.test.gbm)
Plot(lcdfgbm1, i="int_rate")

```

```{r}

#Incorporating profits & costs
library(dplyr)
PROFITVAL=100 #profit (on $100) from accurately identifying Fully_paid loans
COSTVAL=-500  # loss (on $100) from incorrectly predicting a Charged_Off loan as Full_paid
scoreTst=predict(lcDT1,lcdf1Tst, type="prob")[,"Fully Paid"]  

#Identifying those loans wth high probability for being Fully Paid
prPerf=data.frame(scoreTst)
prPerf=cbind(prPerf, status=lcdf1Tst$loan_status)

#Sorting in descending order of  probability (fully_paid)
prPerf=prPerf[order(-scoreTst) ,]
prPerf1=prPerf%>%mutate(profit=ifelse(prPerf$status == 'Fully Paid', PROFITVAL, COSTVAL))                       

                        
cumProfit=cumsum(prPerf$profit)
                        
#Comparing against the default approach of investing in CD with 2% int (i.e. $6 profit out of $100 in 3 years)
   prPerf$cdRet <- 6
   na.omit(prPerf)
   na.omit(prPerf1)
  prPerf$cumCDRet <- cumsum(prPerf$cdRet)
                        plot(cumProfit)
                        lines(prPerf1$cumCDRet, col='purple')
                        
#Or, we really do not need to have the cdRet and cumCDRet columns, since cdRet is $6 for every row
                        plot(prPerf1$cumProfit)
                        abline(a=0, b=6)
                        
                        #Finding the score coresponding to the max profit
                        maxProfit= max(prPerf1$cumProfit)
                        maxProfit_Ind = which.max(prPerf1$cumProfit)
                        maxProfit_score = prPerf1$scoreTst[maxProfit_Ind]
                        print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))
                        
                        ```
                        
                        ```{r}
                        library(gbm)
                        #annRet<-((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100
                        #actualTerm<-((lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3))
                        #actualReturn
                        #total_pymnt
                        rfModelGp<-ranger(loan_status~., data=subset(lcdfTrn, select=-c(annRet, actualTerm, actualReturn, total_pymnt)), num.trees=200,importance='permutation')
                        
                        
                        gbm_M2 <-gbm(formula=unclass(loan_status)-1 ~., data=subset(lcdfTrn, select=-c(annRet, actualTerm, actualReturn)), distribution = "bernoulli", n.trees=2000, shrinkage=0.01, interaction.depth= 4, bag.fraction=0.5, cv.folds= 5, n.cores=16)
                        bestIter<-gbm.perf(gbm_M2, method='cv')
                        scores_gbmM2<-predict(gbm_M2, newdata=lcdfTst, n.tree= bestIter, type="response") pred_gbmM2=prediction(scores_gbmM2, lcdfTst$loan_status, label.ordering= c("Charged Off", "Fully Paid"))
                        #label.orderinghere specifies the 'negative', 'positive' class labels
                        aucPerf_gbmM2 <-performance(pred_gbmM2, "tpr", "fpr")
                        plot(aucPerf_gbmM2)
                        abline(a=0, b= 1)
                        rfModelGp<-ranger(loan_status~., data=subset(lcdfTrn, select=-c(annRet, actualTerm, actualReturn, total_pymnt)), num.trees=200, importance='permutation')
                        scoreTstRF<-predict(rfModel1,lcdfTst, type="prob")[,"Fully Paid"]
                        predRF=prediction(scoreTstRF, lcdfTst$loan_status, label.ordering= c("Charged Off", "Fully Paid"))
                        aucPerfRF<-performance(predRF, "tpr", "fpr")
                        plot(aucPref_gbmM2, col='red', add=TRUE)
                        legend('bottomright', c('RandomForest', 'gbm'), lty=1, col=c('black', 'red'))
                        
                        
                        
                        
                        ```

                        
                        