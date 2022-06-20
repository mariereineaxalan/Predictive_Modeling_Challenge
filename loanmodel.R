setwd("C:/Users/marie/Desktop/MBAX 6330/")
loandata<-read.csv("loans.csv")

min(loandata$disbursed_amount)

##loandata$ll<-NA
##str(loandata)
##for (i in 1:length(loandata$disbursed_amount)) {
  ##if (loandata$disbursed_amount[i] > 0 ) {
     ## loandata$ll[i]<-1}
 ## else {loandata$ll[i]<-0}
 ## }

lmlike<- lm(ll ~., data = loandata)

lmlike<- lm(ll ~., data = loandata)
#clean up birth date, disbursal date
library("lubridate")
mdy <- mdy(loandata$Date.of.Birth)
dmy <- dmy(loandata$Date.of.Birth) 
mdy[is.na(mdy)] <- dmy[is.na(mdy)]
loandata$Date.of.Birth <- mdy 


mdy1 <- mdy(loandata$DisbursalDate)
dmy1 <- dmy(loandata$DisbursalDate) 
mdy1[is.na(mdy1)] <- dmy1[is.na(mdy1)]
loandata$DisbursalDate <- mdy1

str(loandata)
library("tidyr")
#credit history years
install.packages("stringr")

loandata2<-str_split_fixed(loandata$CREDIT.HISTORY.LENGTH, " ", 2)

loandata4<-cbind(loandata, loandata2)
loandata4$`1`<-as.numeric(substr(loandata4$`1`,0,1))
loandata4$`2`<-as.numeric(substr(loandata4$`2`,0,1))
loandata4$`2`<-loandata4$`2`/12
loandata4$credithist.years<-loandata4$`1`+loandata4$`2`

loandatafinal2<- subset(loandata4, select =(c(credithist.years)))
## Account age
loandata2<-str_split_fixed(loandata$AVERAGE.ACCT.AGE, " ", 2)

loandata3<-cbind(loandata, loandata2)
loandata3$`1`<-as.numeric(substr(loandata3$`1`,0,1))
loandata3$`2`<-as.numeric(substr(loandata3$`2`,0,1))
loandata3$`2`<-loandata3$`2`/12
loandata3$averageaccount.years<-loandata3$`1`+loandata3$`2`

loandatafinal<- subset(loandata3, select = -(c(`1`,`2`)))

loandatafinal3<-cbind(loandatafinal,loandatafinal2)

loandatafinal3<- subset(loandatafinal3, select = -(c(CREDIT.HISTORY.LENGTH,AVERAGE.ACCT.AGE)))






##Modeling


##loandata$llpred<- predict(lmlike, loandata)


table(loandatafinal3$PERFORM_CNS.SCORE.DESCRIPTION)

glmfit<-glm(loan_default~. -UniqueID-Current_pincode_ID-Driving_flag-branch_id - supplier_id-manufacturer_id-Employee_code_ID - Date.of.Birth
            , data = lns.train)
summary(glmfit)
lns.train$Perform.Description<-NA

table(lns.train$branch_id)

loandatafinal3$age<-trunc((loandatafinal3$Date.of.Birth %--% as.Date('2021-12-08'))/years(1))


summary(glm(loan_default~ +age *SEC.NO.OF.ACCTS, data =lns.train))

##Not significant
summary(glm(loan_default~ +credithist.years *PRI.NO.OF.ACCTS, data = lns.train))


## Longer credit history are less likely to default
summary(glm(loan_default~ +credithist.years , data = lns.train))


## Prediction 

set.seed(6330)
smp_size <- floor(0.75 * nrow(loandatafinal3))  ## Set size of training dataframe

train_ind <- sample(seq_len(nrow(loandatafinal3)), size = smp_size)  

lns.train <- loandatafinal3[train_ind, ]
lns.test <- loandatafinal3[-train_ind, ]

lns.test$pred<-predict(glmfit,lns.test, type='response')
str(lns.test)
lns.test$fitted.results <- ifelse(lns.test$pred > 0.5,1,0)

lns.test$loan_default <- as.factor(lns.test$loan_default)

install.packages("caret")
library(caret)    
cm<-confusionMatrix(data=lns.test$fitted.results, 
                    reference=lns.test$loan_default)
Accuracy<-round(cm$overall[1],2)
cm
