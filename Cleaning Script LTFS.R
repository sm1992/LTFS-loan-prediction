rm(list=ls())
library(data.table)
library(stringi)
setwd("C:/Users/saurabh/Downloads/train_aox2Jxw")
data<-fread(file = "train.csv",sep = ',')
testdata<-fread(file = "test.csv",sep = ',')

ex<-data$AVERAGE.ACCT.AGE
head(as.numeric(ex))
mon<-gsub(x = stri_extract(str = ex,regex = ".mon"),pattern = "mon",replacement = "")
yrs<-gsub(x = stri_extract(str = ex,regex = ".yrs"),pattern = "yrs",replacement = "")
act.age<-as.numeric(yrs)*365+as.numeric(mon)*30

ex<-data$CREDIT.HISTORY.LENGTH
head(as.numeric(ex))
mon<-gsub(x = stri_extract(str = ex,regex = ".mon"),pattern = "mon",replacement = "")
yrs<-gsub(x = stri_extract(str = ex,regex = ".yrs"),pattern = "yrs",replacement = "")
cred.his.len<-as.numeric(yrs)*365+as.numeric(mon)*30

data$AVERAGE.ACCT.AGE<-act.age
data$CREDIT.HISTORY.LENGTH<-cred.his.len

data2<-cbind.data.frame(disbursed_amount=data$disbursed_amount,asset_cost=data$asset_cost,ltv=data$ltv,Employment.Type=data$Employment.Type,Aadhar_flag=data$Aadhar_flag,PAN_flag=data$PAN_flag,VoterID_flag=data$VoterID_flag,Driving_flag=data$Driving_flag,Passport_flag=data$Passport_flag,PERFORM_CNS.SCORE=data$PERFORM_CNS.SCORE,PERFORM_CNS.SCORE.DESCRIPTION=data$PERFORM_CNS.SCORE.DESCRIPTION,PRI.NO.OF.ACCTS=data$PRI.NO.OF.ACCTS)
data3<-data[,23:41]
data3<-cbind(data2,data3)
data4<-data3[,c(-4,-11,-14)]
res<-data.frame(round(cor(x = data4$loan_default,y=data4[,-31]),2))
write.table(x = res,file = "Clipboard - 16000",sep = '\t')


fit1<-glm(formula =loan_default~.,data = data4)


fit2<-glm(formula =loan_default~.,data = data4,family = binomial(link="logit"))

ggplot(data4,aes(NO.OF_INQUIRIES,loan_default))+
geom_point(position=position_jitter(h=0.1, w=0.1),
           shape = 21, alpha = 0.5, size = 3) +
  
  theme_bw()


############################
ex<-testdata$AVERAGE.ACCT.AGE
head(as.numeric(ex))
mon<-gsub(x = stri_extract(str = ex,regex = ".mon"),pattern = "mon",replacement = "")
yrs<-gsub(x = stri_extract(str = ex,regex = ".yrs"),pattern = "yrs",replacement = "")
act.age<-as.numeric(yrs)*365+as.numeric(mon)*30

ex<-testdata$CREDIT.HISTORY.LENGTH
head(as.numeric(ex))
mon<-gsub(x = stri_extract(str = ex,regex = ".mon"),pattern = "mon",replacement = "")
yrs<-gsub(x = stri_extract(str = ex,regex = ".yrs"),pattern = "yrs",replacement = "")
cred.his.len<-as.numeric(yrs)*365+as.numeric(mon)*30

testdata$AVERAGE.ACCT.AGE<-act.age
testdata$CREDIT.HISTORY.LENGTH<-cred.his.len


data2<-cbind.data.frame(disbursed_amount=testdata$disbursed_amount,asset_cost=testdata$asset_cost,ltv=testdata$ltv,Employment.Type=testdata$Employment.Type,Aadhar_flag=testdata$Aadhar_flag,PAN_flag=testdata$PAN_flag,VoterID_flag=testdata$VoterID_flag,Driving_flag=testdata$Driving_flag,Passport_flag=testdata$Passport_flag,PERFORM_CNS.SCORE=testdata$PERFORM_CNS.SCORE,PERFORM_CNS.SCORE.DESCRIPTION=testdata$PERFORM_CNS.SCORE.DESCRIPTION,PRI.NO.OF.ACCTS=testdata$PRI.NO.OF.ACCTS)
data3<-testdata[,20:41]
data3<-cbind(data2,data3)
testdata4<-data3[,c(-4,-11,-14)]

library(caret)
train_control <- trainControl(method="cv", number=10,verboseIter = TRUE)

model <- train(loan_default~., data=data4, trControl=train_control, method="glm", family="binomial")

pred<-ifelse(model$finalModel$fitted.values > 0.5 , 1,0)
pred<-as.factor(pred)


###################################################start of catboost
rm(list=ls())
library(data.table)
library(stringi)
library(catboost)
library(caret)
setwd("C:/Users/saurabh/Downloads/train_aox2Jxw")
data<-fread(file = "train.csv",sep = ',')
testdata<-fread(file = "test.csv",sep = ',')

ex<-data$AVERAGE.ACCT.AGE
#head(as.numeric(ex))
mon<-gsub(x = stri_extract(str = ex,regex = ".mon"),pattern = "mon",replacement = "")
yrs<-gsub(x = stri_extract(str = ex,regex = ".yrs"),pattern = "yrs",replacement = "")
act.age<-as.numeric(yrs)*365+as.numeric(mon)*30

ex<-data$CREDIT.HISTORY.LENGTH
#head(as.numeric(ex))
mon<-gsub(x = stri_extract(str = ex,regex = ".mon"),pattern = "mon",replacement = "")
yrs<-gsub(x = stri_extract(str = ex,regex = ".yrs"),pattern = "yrs",replacement = "")
cred.his.len<-as.numeric(yrs)*365+as.numeric(mon)*30

data$AVERAGE.ACCT.AGE<-act.age
data$CREDIT.HISTORY.LENGTH<-cred.his.len

data2<-cbind.data.frame(disbursed_amount=data$disbursed_amount,asset_cost=data$asset_cost,ltv=data$ltv,Employment.Type=data$Employment.Type,Aadhar_flag=data$Aadhar_flag,PAN_flag=data$PAN_flag,VoterID_flag=data$VoterID_flag,Driving_flag=data$Driving_flag,Passport_flag=data$Passport_flag,PERFORM_CNS.SCORE=data$PERFORM_CNS.SCORE,PERFORM_CNS.SCORE.DESCRIPTION=data$PERFORM_CNS.SCORE.DESCRIPTION,PRI.NO.OF.ACCTS=data$PRI.NO.OF.ACCTS)
data3<-data[,23:41]
data3<-cbind(data2,data3)

x<-data3$PERFORM_CNS.SCORE.DESCRIPTION
x<-gsub(pattern = "No.*",replacement = "0",x = x)
x<-gsub(pattern = "-.*",replacement = "",x = x)
y<-as.factor(x)
levels(y)<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)
y<-as.numeric(gsub(pattern = '0',replacement = '#N/A',x = y))
data3$PERFORM_CNS.SCORE.DESCRIPTION<-y

l<-createDataPartition(y = 1:nrow(data3),times = 1,p = 0.8)
traindata<-data3[l[[1]],]
valdata<-data3[-l[[1]],]

pool<-catboost.load_pool(data = traindata[,-31],label = as.integer(traindata[,31]),cat_features = c(4))
?catboost.train()
  model<-catboost.cv(pool = pool,fold_count = 4)

    
  
train_control <- trainControl(method="cv", number=4,verboseIter = TRUE)
traindata[,31]<-as.factor(traindata[,31])
model <- train(y = as.factor(traindata$loan_default),x = traindata[,-31],trControl=train_control, method="glm", family="binomial")
modelcaretcat<-train(y = as.factor(traindata$loan_default),x = traindata[,-31],trControl=train_control, method=catboost.caret )

#cvmodel<-catboost.cv(pool = pool,fold_count = 10)

predcatboost<-predict.train(object = modelcaretcat,newdata = valdata[,-31],type = "raw")
predglm<-predict.train(object = model,newdata = valdata[,-31],type='raw')
res<-confusionMatrix(predcatboost,factor(valdata[,31]))
res2<-confusionMatrix(predglm,factor(valdata[,31]))

#################3may save object
###############creating new dataset for glm
val<-data.frame(sapply(X = traindata,FUN = function(x) table(is.na(x)),simplify = TRUE))
plot(traindata$PERFORM_CNS.SCORE,y = traindata$PERFORM_CNS.SCORE.DESCRIPTION)
plot(traindata$PERFORM_CNS.SCORE,x = traindata$PERFORM_CNS.SCORE.DESCRIPTION)
glmdata<-traindata[,-11]#after factoring target
model2 <- train(y = as.factor(traindata$loan_default),x = glmdata[,-30],trControl=train_control, method="glm", family="binomial")
modelcaretcat2<-train(y = as.factor(glmdata$loan_default),x = glmdata[,-30],trControl=train_control, method=catboost.caret )

valdataglm<-valdata[,-11]
predglm<-predict.train(object = model2,newdata = valdataglm[,-30],type='raw')
res2<-confusionMatrix(predglm,factor(valdataglm[,30]))
val2<-data.frame(sapply(X = valdata,FUN = function(x) table(is.na(x)),simplify = TRUE))
predcatboost2<-predict.train(object = modelcaretcat2,newdata = valdataglm[,-30],type = "raw")
res3<-confusionMatrix(predcatboost2,factor(valdataglm[,30]))

