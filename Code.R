setwd='C:\\Users\\Akshay\\Desktop\\Subjects\\IDS 572- Data Mining\\Assignment\\Assignment 2'

library(tidyverse)

library(lubridate)

lcdf <- read_csv('C:\\Users\\Akshay\\Desktop\\Subjects\\IDS 572- Data Mining\\Assignment\\Assignment 2\\lcDataSample.csv')

lcdf <- lcdf %>% filter(loan_status == "Fully Paid" | loan_status == "Charged Off")
lcdf <- lcdf %>% filter(annual_inc <= 1500000)
# Identified outliers by boxplot
out_ru <- boxplot(lcdf$revol_util, plot=FALSE)$out
#Let us look at these examples
out_ru_i <-which(lcdf$revol_util %in% out_ru)
# We will remove these 9 outliers
lcdf <- lcdf [-out_ru_i, ]
# We will use the following to calculate annualized return
#annReturn = [(Total Payment  - funded amount)/funded amount]*12/36*100
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100
# Bringing them to a consistent format
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")
# Creating actual term column - If loan is charged off by default - 3 years
lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)
# We know using simple interest Total =  principle + pnr/100
# Hence r = (Total - principle)/principle * 100/n
# Then, considering this actual term, the actual annual return is
lcdf$actualReturn <- ifelse(lcdf$actualTerm>0, ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm)*100, 0)
# Arranging them since they
lcdf$emp_length <- factor(lcdf$emp_length, levels=c("n/a", "< 1 year","1 year","2 years", "3 years" ,  "4 years",   "5 years",   "6 years",   "7 years" ,  "8 years", "9 years", "10+ years" ))
lcdf$purpose <- as.character(lcdf$purpose )
lcdf$purpose  <- str_trim(lcdf$purpose )
lcdf$purpose  <- as.factor(lcdf$purpose )
lcdf$purpose <- fct_collapse(lcdf$purpose, other = c("wedding","renewable_energy", "other"),NULL = "H")




#  num_bc_tl - number  number of card
# and num_bc_sats satisfactory card


lcdf$propSatisBankcardAccts <- ifelse(lcdf$num_bc_tl>0, lcdf$num_bc_sats/lcdf$num_bc_tl, 0)
# Correcting the date format
lcdf$earliest_cr_line<-paste(lcdf$earliest_cr_line, "-01", sep = "")
lcdf$earliest_cr_line<-parse_date_time(lcdf$earliest_cr_line, "myd")
lcdf$borrHistory <- as.duration(lcdf$earliest_cr_line %--% lcdf$issue_d  ) / dyears(1)
#Another new attribute: ratio of openAccounts to totalAccounts
lcdf$openAccRatio <- ifelse(lcdf$total_acc>0, lcdf$open_acc/lcdf$total_acc, 0)
lcdf <- lcdf %>% mutate_if(is.character, as.factor)
varsToRemove = c('funded_amnt_inv', 'term', 'emp_title', 'pymnt_plan', 'earliest_cr_line', 'title', 'zip_code', 'addr_state', 'out_prncp', 'out_prncp_inv', 'total_pymnt_inv', 'total_rec_prncp', 'total_rec_int', 'total_rec_late_fee', 'recoveries', 'collection_recovery_fee', 'last_credit_pull_d', 'policy_code', 'disbursement_method', 'debt_settlement_flag',  'settlement_term', 'application_type')
lcdf <- lcdf %>% select(-all_of(varsToRemove))  
#Drop all the variables with names starting with "hardship" -- as they can cause leakage, unknown at the time when the loan was given.
# Dropping
lcdf <- lcdf %>% select(-starts_with("hardship"))

#similarly, all variable starting with "settlement", these are happening after disbursement
#Dropping them
lcdf <- lcdf %>% select(-starts_with("settlement"))
# Additional Leakage variables - based on our understanding
varsToRemove2 <- c("last_pymnt_d", "last_pymnt_amnt", "issue_d",'next_pymnt_d', 'deferral_term', 'payment_plan_start_date', 'debt_settlement_flag_date'  )
# last_pymnt_d, last_pymnt_amnt, next_pymnt_d, deferral_term, payment_plan_start_date, debt_settlement_flag_date  
lcdf <- lcdf %>% select(-all_of(varsToRemove2))
# Dropping columns with all n/a
lcdf <- lcdf %>% select_if(function(x){ ! all(is.na(x)) } ) # Dropping
# Finding names of columns which has atleast 1 missing values
options(scipen=999) # To not use scientific notation
# Finding the columns which have more than 60% missing values

nm<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
lcdf <- lcdf %>% select(-all_of(nm))
#Impute missing values for remaining variables which have missing values
# - first get the columns with missing values

#colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
nm<- names(lcdf)[colSums(is.na(lcdf))>0]

#summary(lcdf[, nm])

# Replacing values - adding median values

lcdf<- lcdf %>% replace_na(list(mths_since_last_delinq=median(lcdf$mths_since_last_delinq, na.rm=TRUE), bc_open_to_buy=median(lcdf$bc_open_to_buy, na.rm=TRUE), mo_sin_old_il_acct=median(lcdf$mo_sin_old_il_acct,na.rm=TRUE), mths_since_recent_bc=median(lcdf$mths_since_recent_bc, na.rm=TRUE), mths_since_recent_inq=5, num_tl_120dpd_2m = median(lcdf$num_tl_120dpd_2m, na.rm=TRUE),percent_bc_gt_75 = median(lcdf$percent_bc_gt_75, na.rm=TRUE), bc_util=median(lcdf$bc_util, na.rm=TRUE) ))



lcdf<- lcdf %>% mutate_if(is.numeric,  ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))

## set the seed to make your partition reproducible
set.seed(123)

TRNPROP = 0.5  #proportion of examples in the training sample

nr<-nrow(lcdf)


trnIndex<- sample(1:nr, size = round(TRNPROP * nr), replace=FALSE)

lcdfTrn <- lcdf[trnIndex, ] # Train data

lcdfTst <- lcdf[-trnIndex, ] # Test data

varsOmit <- c('actualTerm', 'actualReturn', 'annRet', 'total_pymnt')  


# Assignment 2

lcdf$loan_status[10:15]

# Let us use unclass to bring it in the required format

unclass(lcdf$loan_status)[10:15]

# fully paid 1 and charged off  2

unclass(lcdf$loan_status)[10:15]-1


## GBM Model - 1

install.packages("gbm")

library(gbm)

## Modeling

gbm_model1 <- gbm(formula=unclass(fct_rev(loan_status))-1 ~., data=lcdfTrn %>% select(-all_of(varsOmit)),
distribution = "bernoulli", n.trees=1800, shrinkage=0.01, interaction.depth = 5, bag.fraction=0.4, cv.folds = 10, n.cores=12)

gbm_model1

summary(gbm_model1)

# Getting the best iteration using performance

best_Iteration<- gbm.perf(gbm_model1,method = 'cv')

best_Iteration


# Predicting on the Test data with the best iteration, this will give probability of 1's - Charged off Loans  

scores_gbmodel1<- predict(gbm_model1, newdata=lcdfTst, n.tree= best_Iteration, type="response")


# The probability of 1's - In our case charged off loans
head(scores_gbmodel1)


install.packages("caret")

library(caret)

# Evaluation of the model we created - Label ordering 0,1 . In our case Fully Paid =0, Charged Off=1

pred_gbmodel1=prediction(scores_gbmodel1, lcdfTst$loan_status, label.ordering = c("Fully Paid","Charged Off"))

pred_gbmodel1


# ROC/AUC

install.packages("ROCR")

library(ROCR)


rocPerf_gbmodel1 <-performance(pred_gbmodel1, "tpr", "fpr")


plot(rocPerf_gbmodel1)


abline(a=0, b= 1)


##########combined plot

plot(aucPerf_1, col='blue', add=TRUE, cex=0.6, main='hi')
plot(aucPerf, col='green', add=TRUE)
plot(rocPerf_gbmodel1, col='red', add=TRUE)



####################


#AUC value
#aucPerf_gbm1=performance(rocPerf_gbmodel1, "auc")


#aucPerf_gbm1@y.values



paramter_Grid <- expand.grid(
  treeDepth = c(2, 5),
  shrinkage = c(.001, .01, .1),
  bestTree = 0,
  minError = 0
)

for(i in 1 : nrow(paramter_Grid)) {
  gbm_paramTune <- gbm(formula= unclass(loan_status)-1 ~.,
                       data=subset(lcdfTrn, select=-c(annRet, actualTerm, actualReturn, total_pymnt)),
                       distribution = "bernoulli", n.trees = 1000, interaction.depth = paramter_Grid$treeDepth[i], shrinkage = paramter_Grid$shrinkage[i],
                       train.fraction = 0.7,
                       n.cores=16 ) #use all available cores
  #add best tree and its RMSE to paramter_Grid
  paramter_Grid$bestTree[i] <- which.min(gbm_paramTune$valid.error)
  paramter_Grid$minError[i] <- min(gbm_paramTune$valid.error)}

paramter_Grid





# Model 2

gbm_model2 <- gbm(formula=unclass(fct_rev(loan_status))-1 ~., data=lcdfTrn %>% select(-all_of(varsOmit)),
                  distribution = "bernoulli", n.trees=1000, shrinkage=0.001, interaction.depth = 5, bag.fraction=0.4, cv.folds = 10, n.cores=12)
  

summary(gbm_model2)

scores_gbmodel2<- predict(gbm_model2, newdata=lcdfTst, n.tree= 1000, type="response")

pred_gbmodel2=prediction(scores_gbmodel2, lcdfTst$loan_status, label.ordering = c("Fully Paid","Charged Off"))

pred_gbmodel2


# ROC/AUC
library(ROCR)


rocPerf_gbmodel2 <-performance(pred_gbmodel2, "tpr", "fpr")

plot(rocPerf_gbmodel2)


abline(a=0, b= 1)


# Linear model Laso, alpha=1

library(glmnet)

library(dplyr)

yTrn<-factor(if_else(lcdfTrn$loan_status=="Fully Paid", '1', '0') )

xDTrn<-lcdfTrn %>% select(-loan_status, -actualTerm, -annRet, -actualReturn, -total_pymnt)


yTst<-factor(if_else(lcdfTst$loan_status=="Fully Paid", '1', '0') )

xDTst<-lcdfTst %>% select(-loan_status, -actualTerm, -annRet, -actualReturn, -total_pymnt)

install.packages("glmnet")

library(glmnet)

glmls_cv<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial", alpha=1)


plot(glmls_cv)

`##################


predsauc <- prediction(predict(glmls_cv, data.matrix(xDTst), s='lambda.min', type='response'), lcdfTst$loan_status, label.ordering = c("Charged Off", "Fully Paid"))

aucPerf <- performance(predsauc, "auc")

aucPerf@y.values

aucPerf <- performance(predsauc, "tpr","fpr")

plot(aucPerf)


glmPredls_pc=predict(glmls_cv,data.matrix(xDTst), s="lambda.min", type="class" )

confusionMatrix(factor(glmPredls_pc, levels = c(1,0)), yTst, positive = "1")

install.packages("vip", dependencies = "true")

glmPredls_pc

library(vip)

vi_model(glmls_cv) %>% View()

########################


wts = ifelse(yTrn==0, 1-sum(yTrn==0)/length(yTrn),1-sum(yTrn==1)/length(yTrn)) 

wts
# Training a model with weights

glmls_cv_wt <- cv.glmnet(data.matrix(xDTrn), yTrn, family='binomial', weights = wts)


plot(glmls_cv_wt)

predsauc <- prediction(predict(glmls_cv_wt, data.matrix(xDTst), s='lambda.min', type='response'), lcdfTst$loan_status, label.ordering = c("Charged Off", "Fully Paid"))

aucPerf <- performance(predsauc, "auc")

aucPerf@y.values

aucPerf <- performance(predsauc, "tpr","fpr")

plot(aucPerf)



####  Ridge alpha= 0

yTrn<-factor(if_else(lcdfTrn$loan_status=="Fully Paid", '1', '0') )

xDTrn<-lcdfTrn %>% select(-loan_status, -actualTerm, -annRet, -actualReturn, -total_pymnt)


yTst<-factor(if_else(lcdfTst$loan_status=="Fully Paid", '1', '0') )

xDTst<-lcdfTst %>% select(-loan_status, -actualTerm, -annRet, -actualReturn, -total_pymnt)

install.packages("glmnet")

library(glmnet)

glmls_cv<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial", alpha=0)


plot(glmls_cv)

`##################

install.packages("ROCR")


library(ROCR)

predsauc <- prediction(predict(glmls_cv, data.matrix(xDTst), s='lambda.min', type='response'), lcdfTst$loan_status, label.ordering = c("Charged Off", "Fully Paid"))

aucPerf <- performance(predsauc, "auc")

aucPerf@y.values

aucPerf <- performance(predsauc, "tpr","fpr")

plot(aucPerf)

install.packages("CARET")

library(caret)

glmPredls_pc=predict(glmls_cv,data.matrix(xDTst), s="lambda.min", type="class" )

confusionMatrix(factor(glmPredls_pc, levels = c(1,0)), yTst, positive = "1")


########################


wts = ifelse(yTrn==0, 1-sum(yTrn==0)/length(yTrn),1-sum(yTrn==1)/length(yTrn)) 

wts
# Training a model with weights

glmls_cv_wt <- cv.glmnet(data.matrix(xDTrn), yTrn, family='binomial', weights = wts, alpha=0)


plot(glmls_cv_wt)

predsauc <- prediction(predict(glmls_cv_wt, data.matrix(xDTst), s='lambda.min', type='response'), lcdfTst$loan_status, label.ordering = c("Charged Off", "Fully Paid"))

aucPerf <- performance(predsauc, "auc")

aucPerf@y.values

aucPerf <- performance(predsauc, "tpr","fpr")

plot(aucPerf)


#####################################################     new threshold set to 0.7  ###################################################

trnIndex<- sample(1:nr, size = round(0.7 * nr), replace=FALSE)

lcdfTrn <- lcdf[trnIndex, ] # Train data

lcdfTst <- lcdf[-trnIndex, ] # Test data

varsOmit <- c('actualTerm', 'actualReturn', 'annRet', 'total_pymnt')  


# Linear model Laso

yTrn<-factor(if_else(lcdfTrn$loan_status=="Fully Paid", '1', '0') )

xDTrn<-lcdfTrn %>% select(-loan_status, -actualTerm, -annRet, -actualReturn, -total_pymnt)


yTst<-factor(if_else(lcdfTst$loan_status=="Fully Paid", '1', '0') )

xDTst<-lcdfTst %>% select(-loan_status, -actualTerm, -annRet, -actualReturn, -total_pymnt)



glmls_cv<- cv.glmnet(data.matrix(xDTrn), yTrn, family="binomial", alpha=1)





plot(glmls_cv)

`##################


predsauc <- prediction(predict(glmls_cv, data.matrix(xDTst), s='lambda.min', type='response'), lcdfTst$loan_status, label.ordering = c("Charged Off", "Fully Paid"))

aucPerf <- performance(predsauc, "auc")

aucPerf@y.values

aucPerf <- performance(predsauc, "tpr","fpr")

plot(aucPerf)

abline(a=0, b= 1)

glmPredls_pc=predict(glmls_cv,data.matrix(xDTst), s="lambda.min", type="class" )

confusionMatrix(factor(glmPredls_pc, levels = c(1,0)), yTst, positive = "1")

########################



#######################question 3 ##################################################
  
TRNPROP = 0.5  #proportion of examples in the training sample

nr<-nrow(lcdf)


trnIndex<- sample(1:nr, size = round(TRNPROP * nr), replace=FALSE)

lcdfTrn <- lcdf[trnIndex, ] # Train data

lcdfTst <- lcdf[-trnIndex, ] # Test data

varsOmit <- c('actualTerm', 'total_pymnt')  




###################################  Random forest question 3

install.packages("Ranger")

library(ranger)

rfModel_Ret <- ranger(actualReturn ~., data=subset(lcdfTrn, select=-c(annRet, actualTerm, loan_status)), num.trees =220, 
                      importance='permutation')

rfPredRet_trn<- predict(rfModel_Ret, lcdfTrn)

sqrt(mean( (rfPredRet_trn$predictions - lcdfTrn$actualReturn)^2))


###test data


rfModel_Ret_tst <- ranger(actualReturn ~., data=subset(lcdfTst, select=-c(annRet, actualTerm, loan_status)), num.trees =220, 
                      importance='permutation')

rfPredRet_tst<- predict(rfModel_Ret_tst, lcdfTst)

sqrt(mean( (rfPredRet_tst$predictions - lcdfTst$actualReturn)^2))

#############################glm model

##LAso train

install.packages("glmnet")

library(glmnet)

glmRet_cv_lasso <- cv.glmnet(data.matrix(xDTrn), lcdfTrn$actualReturn, family='gaussian', alpha=1)


glmRet_cv_a5predict <- predict(glmRet_cv_lasso, data.matrix(xDTst))

sqrt(mean( (glmRet_cv_a5predict - lcdfTst$actualReturn)^2))

####Laso test

glmRet_cv_lasso_1 <- cv.glmnet(data.matrix(xDTst), lcdfTst$actualReturn, family='gaussian', alpha=1)


glmRet_cv_a5predict <- predict(glmRet_cv_lasso_1, data.matrix(xDTst))

sqrt(mean( (glmRet_cv_a5predict - lcdfTst$actualReturn)^2))

### Ridge train


glmRet_cv_ridge <- cv.glmnet(data.matrix(xDTrn), lcdfTrn$actualReturn, family='gaussian', alpha=0)

glmRet_cv_a5predict <- predict(glmRet_cv_ridge, data.matrix(xDTrn))

sqrt(mean( (glmRet_cv_a5predict - lcdfTst$actualReturn)^2))


####test

glmRet_cv_ridge <- cv.glmnet(data.matrix(xDTst), lcdfTst$actualReturn, family='gaussian', alpha=0)

glmRet_cv_a5predict <- predict(glmRet_cv_ridge, data.matrix(xDTst))

sqrt(mean( (glmRet_cv_a5predict - lcdfTst$actualReturn)^2))

################# xgboost

# train

install.packages("xgboost")
install.packages("caret")

library(xgboost)
library(caret)


lcdf_act <- subset(lcdf, select=-c(actualTerm,loan_status,actualReturn))


fdum<-dummyVars(~.,data=lcdf_act)
dxlcdf<-predict(fdum, lcdf_act) #Matrix for x (lcdf_act)
actlcdf <- lcdf$actualReturn #Matrix for y


dxlcdfTrn <- dxlcdf[trnIndex,] #Trn-x
dxlcdfTst <- dxlcdf[-trnIndex,] #Tst-x
actlcdfTrn <- actlcdf[trnIndex] #Trn-y
actlcdfTst <- actlcdf[-trnIndex] #Tst-y
eva_lcdfTrn <- lcdf[trnIndex,] #Value for evaluation
eva_lcdfTst <- lcdf[-trnIndex,] #Value for evaluation


#make data matrix
dxTrn<-xgb.DMatrix(dxlcdfTrn, label=actlcdfTrn)
dxTst<-xgb.DMatrix(dxlcdfTst, label=actlcdfTst)


xgbParamGrid <- expand.grid(max_depth= c(2,5),
                            eta = c(0.1, 0.01,0.001))


#Best Parameters

#for(i in 1:nrow(xgbParamGrid)) {
#  set.seed(1789)
#  xgb_tune <- xgb.cv(data = dxTrn,objective= "reg:squarederror",
#                     nrounds=500,
#                     nfold = 5,
#                     eta=xgbParamGrid$eta[i],
#                     max_depth=xgbParamGrid$max_depth[i],
#                     early_stopping_rounds= 10)
#  xgbParamGrid$bestTree[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$iter
# xgbParamGrid$bestPerf[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$test_rmse_mean
#}


xgb_tune$evaluation_log[xgb_tune$best_iteration]$iter
# xgbParamGrid$bestPerf[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$test_rmse_mean
#}

#view ParamGrid
#xgbParamGrid

#Select min param
best_index_ParamGrid <- which.min(xgbParamGrid$bestPerf)
best_index_ParamGrid



#xgboost Training

xgb_Mr <- xgboost( data = dxTrn,
                   nrounds=80,
                   max.depth=5,
                   eta=0.01,
                   objective="reg:squarederror")






#evaluation Training
predXgbRet_Trn <- eva_lcdfTrn %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(predXgbRet=predict(xgb_Mr,dxTrn))




#xgboost Testing

xgb_tst <- xgboost( data = dxTst,
                    nrounds=80,
                    max.depth=5 ,
                    eta=0.01,
                    objective="reg:squarederror")

#variable importance
xgb.importance(model=xgb_tst)

#evaluation Testing
predXgbRet_Tst <- eva_lcdfTst %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(predXgbRet=predict(xgb_tst,dxTst))



############### Question 5 random forest

install.packages("ranger")
library(ranger)

rf_lg_lcdfTrn_1<-lcdfTrn %>% filter(grade=='C'| grade=='D'| grade== 'E'| grade== 'F'| grade== 'G')
rf_lg_lcdfTst<-lcdfTst %>% filter(grade=='C'| grade=='D'| grade== 'E'| grade== 'F'| grade== 'G')
rf_M1_lg_rf <- ranger(loan_status ~., data=subset(rf_lg_lcdfTst, select=-c(actualTerm, actualReturn)), num.trees =1000, probability=TRUE, importance='permutation')

lg_scoreTstRF <- rf_lg_lcdfTst %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>% mutate(score=(predict(rf_M1_lg_rf,rf_lg_lcdfTst))$predictions[,1])
lg_scoreTstRF <- lg_scoreTstRF %>% mutate(tile=ntile(-score, 10))

lg_scoreTstRF%>%group_by(tile)%>% summarise(count=n(),avgSc=mean(score), numDefaults=sum(loan_status=="Charged Off"), avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), avgTer=mean(actualTerm), totA=sum(grade=="A"), totB=sum(grade=="B" ), totC=sum(grade=="C"), totD=sum(grade=="D"), totE=sum(grade=="E"), totF=sum(grade=="F") )




####################### glm


lg_lcdfTrn_1<-lcdfTrn %>% filter(grade=='C'| grade=='D'| grade== 'E'| grade== 'F'| grade== 'G')
xD<-lg_lcdfTrn_1 %>% select(-loan_status, -actualTerm, -actualReturn)
glmRet_cv<- cv.glmnet(data.matrix(xD), lg_lcdfTrn_1$actualReturn, family="gaussian")

predRet_Trn <- lg_lcdfTrn_1 %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>% mutate(predRet= predict(glmRet_cv, data.matrix(lg_lcdfTrn_1 %>% select(-loan_status, -actualTerm, -actualReturn)), s="lambda.min" ) )
predRet_Trn <- predRet_Trn %>% mutate(tile=ntile(-predRet, 10))


predRet_Trn%>%group_by(tile)%>% summarise(count=n(),avgSc=mean(predRet), numDefaults=sum(loan_status=="Charged Off"),
                                          avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), 
                                          avgTer=mean(actualTerm), totA=sum(grade=="A"), totB=sum(grade=="B" ),
                                          totC=sum(grade=="C"), totD=sum(grade=="D"), totE=sum(grade=="E"), 
                                          totF=sum(grade=="F") )

