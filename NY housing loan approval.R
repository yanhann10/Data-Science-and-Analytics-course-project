#all the code goes into this directory
#create this directory first
setwd("~/HARDLAB")

library('RODBC')
ch<-odbcConnect("hmdalab",uid="gpadmin",case="postgresql", pwd="changeme")

sqlQuery(ch,"Drop table ny_data")
sqlQuery(ch,
         "CREATE TABLE ny_data AS
         select loan_type, loan_purpose, loan_amount_ink, preapproval,
         action_type, county_code, applicant_ethnicity,
         co_applicant_ethnicity, applicant_race_1, applicant_sex,
         applicant_income_ink, rate_spread, hoepa_status,lien_status,
         minority_population_pct,
         hud_median_family_income,
         tract_to_MSAMD_income_pct,
         number_of_owner_occupied_units
         from larDB3  where state_code='36'
         and occupancy=1 and property_type='1' and action_type<=4
         ")

nydata<-data.frame(sqlFetch(ch,"ny_data"))

breaks <- c(0,1, 2, 3)
labels <- c ("Home Purchase", "Home Improvement", "Refinancing")
loanpurpose<- cut(nydata$loan_purpose, breaks, labels)
head(loanpurpose)
nydata<-cbind(nydata,loanpurpose)
head(nydata)

breaks <- c(0,1, 2, 3,4)
labels <- c ("Conventional", "FHA","VA","FSA.RHS")
loantype<- cut(nydata$loan_type, breaks, labels)
nydata<-cbind(nydata,loantype)


nydata$preapprovalcode<-ifelse(nydata$preapproval == 1,"Requested",ifelse(nydata$preapproval ==2,"Not requested","Not applicable"))
head(nydata$family_income)

#convert text as number
nydata$hud_median_family_income=as.numeric(paste(nydata$hud_median_family_income))
nydata$applicant_income_ink=as.numeric(paste(nydata$applicant_income_ink))

#remove na
nydata=nydata[!is.na(nydata$applicant_income_ink),]
nydata=nydata[!is.na(nydata$tract_to_msamd_income_pct),]
nydata=nydata[!is.na(nydata$minority_population_pct),]

action<-data.frame(sqlFetch(ch,"action"))

nydata$actiontypecode<-ifelse(nydata$action_type == 1,"Originated",
ifelse(nydata$action_type ==2,"Approved Not Accepted",
ifelse(nydata$action_type ==3,"Denied","Withdrawn")))

county<-data.frame(sqlQuery(ch,"select county_code, county_name from counties where state_code='36' order by county_code"))
nydata$county_code2=as.numeric(paste(nydata$county_code))
idx<-match(nydata$county_code2,county$county_code)
nydata$county_name=county$county_name[idx]

head(nydata)

ethnicity<-data.frame(sqlFetch(ch,"ethnicity"))
idx2<-match(nydata$applicant_ethnicity,ethnicity$code)
nydata$Applicant_Ethnicity=ethnicity$value[idx2]
idx2<-match(nydata$co_applicant_ethnicity,ethnicity$code)
nydata$Co_Applicant_Ethnicity=ethnicity$value[idx2]


race<-data.frame(sqlFetch(ch,"race"))
idx3<-match(nydata$applicant_race_1,race$code)
nydata$Applicant_Race_1=race$value[idx3]

sex<-data.frame(sqlFetch(ch,"sex"))
idx4<-match(nydata$applicant_sex,sex$code)
nydata$Applicant_Sex=sex$value[idx4]

lien<-data.frame(sqlFetch(ch,"lienstatus"))
idx5<-match(nydata$lien_status,lien$code)
nydata$Lien_Status=lien$value[idx5]

head(nydata)

colselect=c("loantype","loanpurpose","loan_amount_ink","preapprovalcode","actiontypecode","county_name",
                      "Applicant_Ethnicity","Co_Applicant_Ethnicity","Applicant_Race_1","Lien_Status","applicant_income_ink",
                      "rate_spread","hoepa_status","Applicant_Sex","minority_population_pct","hud_median_family_income","tract_to_msamd_income_pct",
                      "number_of_owner_occupied_units")


nydata$actiontypecode=as.factor(paste(nydata$actiontypecode))
nydata$preapprovalcode=as.factor(paste(nydata$preapprovalcode))
summary(nydata)

nydatatable=nydata[names(nydata)%in%colselect]

nydatatable=nydatatable[nydatatable$actiontypecode!="Withdrawn",]
nydatatable$approve=ifelse(nydatatable$actiontypecode=="Denied",
                           "Denied","Originated")
nydatatable$approve=as.factor(nydatatable$approve)
#exploration
accept=subset(nydatatable,nydatatable$actiontypecode=="Originated")
hist(accept$loan_amount_ink,breaks=100)

#modelling
#sample(seq_len(nrow(nydatatable)), size = smp_size)
trainidx=sample(1:340450,340450*0.75)
train=nydatatable[trainidx,]
test=nydatatable[-trainidx,]

#model0 with NB, no feature selection
#model<-naiveBayes(actiontypecode ~.,train)
#model
#results<-predict(model,train)
#results
#conf<-table(train$actiontypecode,results)
#accuracy<-sum(diag(conf))/sum(conf)
#model1 NB with feature selection
#model1<-naiveBayes(approve ~applicant_income_ink+loan_amount_ink,train)
#results1<-predict(model,train)
#results1raw<-predict(model,train,type="raw")
#conf1<-table(train$approve,results1)
#accuracy1<-sum(diag(conf1))/sum(conf1)
#accuracy1
#model2 NB with feature selection
#model2<-naiveBayes(approve ~applicant_income_ink+loan_amount_ink+loanpurpose,train)
#results2<-predict(model,train)

#results2raw<-predict(model,train,type="raw")
#conf2<-table(train$approve,results2)
#accuracy2<-sum(diag(conf2))/sum(conf2)
#accuracy2
#evaluate the model

#accuracy1=0.3658

#AUC
library(ROCR)


#with Logisticregression
mylogit<-glm(approve~log(applicant_income_ink)+log(loan_amount_ink)+loanpurpose+loantype+hud_median_family_income+preapprovalcode,
             data=train,
              family=binomial(link="logit"))
summary(mylogit)
pred=predict(mylogit,newdata=test,type="response")
predObj=prediction(pred,test$approve)
rocObj=performance(predObj,measure="tpr",x.measure="fpr")
aucObj=performance(predObj,measure="auc")
auc=aucObj@y.values[[1]]
auc
plot(rocObj,main=paste("Area under the curve:",auc))
#auc=0.67569

#confusion matrix
breaks=c(0,0.5,0.75,1)
labels=c("low prob","medium prob","high prob")
predbkt<-cut(pred,breaks,labels)
cm<-as.table(table(predbkt,test$approve))
prop.table(cm,1)
plot(table(predbkt,test$approve),main="Probability bucket vs Approval Status"
###     
     #predbkt          Denied Originated
     #low prob    0.5569144  0.4430856
     #medium prob 0.3399294  0.6600706
     #high prob   0.1643716  0.8356284

p1=hist(pred[test$approve=="Originated"],breaks=30)
p2=hist(pred[test$approve!="Originated"],breaks=30)
plot( p1, col=rgb(0,0,1,1/4),main="Approved in Blue, Rejected in Purple")  # first histogram
plot( p2, col=rgb(1,0,0,1/4), add=T)  # second

#choose 0.75 as cutoff
predresult=ifelse(pred<0.7,"Denied","Originated")
logitcf=table(predresult,test$approve)
     #predresult   Denied Originated
     #Denied      12762      18086
     #Originated  10199      43866
tpr=logitcf[2,2]/sum(logitcf[,2]) #0.71
fpr=logitcf[2,1]/sum(logitcf[,1]) #0.44
     accuracy=sum(diag(logitcf))/sum(logitcf)
accuracy
     #output
write.table(nydatatable, "nydatatable.txt",sep="\t")
write.table(logitcf,"truthtable.txt",sep="\t")