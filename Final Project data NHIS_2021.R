# Final Project Group : 
# Amira Elmakawy , Muhibul Islam .

attach(Household_Pulse_data)
summary(Household_Pulse_data)
restrict1 <- (Household_Pulse_data$TBIRTH_YEAR >= 1997) | (Household_Pulse_data$TBIRTH_YEAR <= 1967 )
data_new <- subset(Household_Pulse_data,restrict1)
summary(data_new)
library(tidyverse)
library(plyr)
Household_Pulse_data$HADCOVID <- factor((Household_Pulse_data$HADCOVID) , levels = c("no did not","yes doctor told had covid","not sure"))
slevels(Household_Pulse_data$HADCOVID)
levels(Household_Pulse_data$HADCOVID) <- c("no did not","yes doctor told had covid","not sure")
summary(HADCOVID)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$HADCOVID)

Household_Pulse_data$INCOME <- factor((Household_Pulse_data$INCOME) , levels = c("HH income less than $25k", "HH income $25k - $34.9k", "HH income $35k - 49.9", "HH income $50k - 74.9", "HH income $75 - 99.9", "HH income $100k - 149", "HH income $150 - 199", "HH income $200k +"))
levels(Household_Pulse_data$INCOME) <- c("HH income less than $25k", "HH income $25k - $34.9k", "HH income $35k - 49.9", "HH income $50k - 74.9", "HH income $75 - 99.9", "HH income $100k - 149", "HH income $150 - 199", "HH income $200k +")
levels(Household_Pulse_data$INCOME)
summary(Household_Pulse_data$INCOME)

table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$INCOME)
Household_Pulse_data$WRKLOSSRV <- factor((Household_Pulse_data$WRKLOSSRV) , levels = c("no recent HH job loss","yes recent HH job loss"))
levels(Household_Pulse_data$WRKLOSSRV) <- c("no recent HH job loss","yes recent HH job loss")
levels(Household_Pulse_data$WRKLOSSRV)
summary(Household_Pulse_data$WRKLOSSRV)
Household_Pulse_data$RECVDVACC <- factor((Household_Pulse_data$RECVDVACC) , levels = c("yes got vaxx","no did not get vaxx"))
levels(Household_Pulse_data$RECVDVACC) <- c("yes got vaxx","no did not get vaxx")
levels(Household_Pulse_data$RECVDVACC)
summary(Household_Pulse_data$RECVDVACC)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$RECVDVACC)
Household_Pulse_data$KINDWORK <- factor((Household_Pulse_data$KINDWORK) , levels = c("work for govt","work for private co","work for nonprofit","work in family biz","self employed"))
levels(Household_Pulse_data$KINDWORK) <- c("work for govt","work for private co","work for nonprofit","work in family biz","self employed")
summary(Household_Pulse_data$KINDWORK)
summary(KINDWORK)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$KINDWORK)
Household_Pulse_data$WORRY <- factor((Household_Pulse_data$WORRY) , levels = c("no worry over past 2 wks","several days worried over past 2 wks","nearly every day worry"))
levels(Household_Pulse_data$WORRY) <- c("no worry over past 2 wks","several days worried over past 2 wks","nearly every day worry")
summary(Household_Pulse_data$WORRY)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$WORRY)
Household_Pulse_data$ANXIOUS <- factor((Household_Pulse_data$ANXIOUS) , levels = c("no anxiety over past 2 wks","several days anxiety over past 2 wks","more than half the days anxiety over past 2 wks","nearly every day anxiety"))
levels(Household_Pulse_data$ANXIOUS) <- c("no anxiety over past 2 wks","several days anxiety over past 2 wks","more than half the days anxiety over past 2 wks","nearly every day anxiety")
summary(Household_Pulse_data$ANXIOUS)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$ANXIOUS)
Household_Pulse_data$GENID_DESCRIBE <- factor((Household_Pulse_data$GENID_DESCRIBE) , levels = c("female" , "male"))
levels(Household_Pulse_data$GENID_DESCRIBE) <- c("female" , "male")
summary(Household_Pulse_data$GENID_DESCRIBE)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$GENID_DESCRIBE)
Household_Pulse_data$Works_onsite <- factor((Household_Pulse_data$Works_onsite) , levels =c("onsite", "no"))
levels(Household_Pulse_data$Works_onsite) <- c("onsite", "no")
summary(Household_Pulse_data$Works_onsite)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$Works_onsite)
Household_Pulse_data$works_remote <- factor((Household_Pulse_data$works_remote) , levels = c("worked remotely", "no"))
summary(Household_Pulse_data$works_remote)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$works_remote)
Household_Pulse_data$RRACE <- factor((Household_Pulse_data$RRACE) , levels = c("White" , "Asian" , "Black" , "Other"))
levels(Household_Pulse_data$RRACE) <- c("White" , "Asian" , "Black" , "Other")
summary(Household_Pulse_data$RRACE)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$RRACE)
Household_Pulse_data$EEDUC <- factor((Household_Pulse_data$EEDUC) , levels = c("bach deg" , "HS diploma" , "adv deg" , "some coll" , "assoc deg"))
levels(Household_Pulse_data$EEDUC) <- c("bach deg" , "HS diploma" , "adv deg" , "some coll" , "assoc deg")
summary(Household_Pulse_data$EEDUC)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$EEDUC)
Household_Pulse_data$MS <- factor((Household_Pulse_data$MS) , levels = c("married" , "widowed" , "never" , "divorced"))
levels(Household_Pulse_data$MS) <- c("married" , "widowed" , "never" , "divorced")
summary(Household_Pulse_data$MS)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$MS)
Household_Pulse_data$RHISPANIC <- factor((Household_Pulse_data$RHISPANIC) , levels = c("Not Hispanic" , "Hispanic"))
levels(Household_Pulse_data$RHISPANIC) <- c("Not Hispanic" , "Hispanic")
summary(Household_Pulse_data$MS)
summary(Household_Pulse_data$RHISPANIC)
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$RHISPANIC)

# LINEAR REGRESSION :

model_1 <- lm(WRKLOSSRV ~ GENID_DESCRIBE + HADCOVID + ANXIOUS + WORRY + KINDWORK + works_remote  + RECVDVACC, data = data_new)

model_logit1 <- glm(WRKLOSSRV ~  WORRY + HADCOVID + ANXIOUS + RECVDVACC + KINDWORK + GENID_DESCRIBE , family = binomial, data = data_new)
summary(model_logit1)



model_logit2 <- glm(WRKLOSSRV ~ GENID_DESCRIBE + HADCOVID + ANXIOUS + WORRY + KINDWORK + works_remote + RECVDVACC, family = binomial, data = data_new)
summary(model_logit2)


require(ggplot2)
plot(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$HADCOVID)
remotebar <- table(Household_Pulse_data$works_remote)
worklossbar <- table(Household_Pulse_data$WRKLOSSRV)
vaccinedbar <- table(Household_Pulse_data$RECVDVACC)
hadcovidbar <- table(Household_Pulse_data$HADCOVID)
genidescribebar <- table(Household_Pulse_data$GENID_DESCRIBE)
worrybar <- table(Household_Pulse_data$WORRY)
anxiousbar <- table(Household_Pulse_data$ANXIOUS)
barplot(hadcovidbar)
barplot(worklossbar)
barplot(remotebar)
barplot(vaccinedbar)
barplot(genidescribebar)
barplot(worrybar)
barplot(anxiousbar)

model_logit3 <- glm(WRKLOSSRV ~ GENID_DESCRIBE + HADCOVID + ANXIOUS + WORRY + KINDWORK + EEDUC + works_remote + RECVDVACC, family = binomial, data = data_new)
summary(model_logit3)

model_logit4 <- glm(WRKLOSSRV ~ GENID_DESCRIBE + HADCOVID + ANXIOUS + WORRY + KINDWORK + EEDUC + works_remote + RRACE + RECVDVACC, family = binomial, data = data_new)
summary(model_logit4)

summary(Household_Pulse_data$WRKLOSSRV)
data_new$work_loss <- data_new$WRKLOSSRV == "yes recent HH job loss"




pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR < 2000)
dat_use1 <- subset(Household_Pulse_data, pick_use1)
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC)
model_logit5 <- glm(WRKLOSSRV ~ EEDUC + MS + RRACE +  GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit5)             

# MODEL IS TOTALLY SIGNIFICANT HERE !  except for the GENID_DESCRIBE as before .
# ALMOST ALL THE VARIABLES INCLUDING EDUCATION ARE SIGNIFICANT WITH THE WORKLOSS VARIABLE.
to_be_predicted <- data.frame(EEDUC = "some coll", MS = "divorced", RRACE = "White", GENID_DESCRIBE = "female", data = dat_use1)
to_be_predicted$yhat <- predict(model_logit5, to_be_predicted, type = "response")
summary(to_be_predicted$yhat)
# the predicted value of the divorces white females who got some college and lost their jobs is "0.1604".
to_be_predicted <- data.frame(EEDUC = "bach deg", MS = "married", RRACE = "White", GENID_DESCRIBE = "female", data = dat_use1)
to_be_predicted$yhat <- predict(model_logit5, to_be_predicted, type = "response")
summary(to_be_predicted$yhat)
 # while the predicted value for white married females who got bachelors degree and lost their jobs is "0.07952"
 
table1 <- table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$EEDUC)
table1
plot(table1, main="Work loss By Education",
     xlab="Education Levels",
     ylab="Work Loss", 
     col = c("red", "blue"))

# By doing probit :

model_probit1 <- glm(WRKLOSSRV ~ EEDUC + MS + RRACE +  GENID_DESCRIBE,
                     family = binomial (link = 'probit'), data = dat_use1)
summary(model_probit1)
to_be_predicted2<- data.frame(EEDUC = "some coll", MS = "married", RRACE = "Asian", GENID_DESCRIBE = "female" ,  data = dat_use1)
to_be_predicted2$yhat<-predict(model_probit1, to_be_predicted2, type="response")
summary(to_be_predicted2$yhat)

# predicted value of married asian females with some college is "0.1773"

# FOR MALES :
to_be_predicted3<- data.frame(EEDUC = "some coll", MS = "married", RRACE = "Asian", GENID_DESCRIBE = "male" ,  data = dat_use1)
to_be_predicted3$yhat<-predict(model_probit1, to_be_predicted2, type="response")
summary(to_be_predicted3$yhat)

# THE PREDICTED VALUE OF MARRIED ASIAN MALES WITH SOME COLLEGE IS A LITTLE BIGGER WITH "0.1776"

to_be_predicted4<- data.frame(EEDUC = "bach deg", MS = "never", RRACE = "black", GENID_DESCRIBE = "male" ,  data = dat_use1)
to_be_predicted4$yhat<-predict(model_probit1, to_be_predicted2, type="response")
summary(to_be_predicted4$yhat)
# same result

to_be_predicted5<- data.frame(EEDUC = "bach deg", MS = "never", RRACE = "black", GENID_DESCRIBE = "female" ,  data = dat_use1)
to_be_predicted5$yhat<-predict(model_probit1, to_be_predicted2, type="response")
summary(to_be_predicted5$yhat)

dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 
dat_use1$RHISPANIC <- droplevels(dat_use1$RRACE)
dat_use1$EEDUC <- droplevels(dat_use1$EEDUC)
dat_use1$MS <- droplevels(dat_use1$MS)
dat_use1$EGENID_BIRTH <- droplevels(dat_use1$GENID_DESCRIBE)
dat_use1$RRACE <- droplevels(dat_use1$RRACE)
dat_use1$WORRY <- droplevels(dat_use1$WORRY)
dat_use1$ANXIOUS <- droplevels(dat_use1$ANXIOUS)

model_logit6 <- glm(WRKLOSSRV ~ TBIRTH_YEAR + EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)

# WE ADDED THE INCOME VARIABLES TO THE MODEL IN MODEL_LOGIT 7

model_logit7 <- glm(WRKLOSSRV ~ INCOME + GENID_DESCRIBE + HADCOVID + ANXIOUS + WORRY + KINDWORK + EEDUC + works_remote + RRACE + RECVDVACC, family = binomial, data = data_new)
summary(model_logit7)



model_logit8 <- glm(WRKLOSSRV ~ INCOME + I(INCOME)^2 + I(INCOME)^3 + GENID_DESCRIBE + HADCOVID + ANXIOUS + WORRY + KINDWORK + EEDUC + works_remote + RRACE + RECVDVACC, family = binomial, data = data_new)
summary(model_logit8)




new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1996,
                                       EEDUC = factor("some coll", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("married",levels = levels(dat_use1$MS)),
                                       RRACE = factor("white",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Not Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("Female", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit6,new_data_to_be_predicted)
# i got a result of "1" here and then I get an error after searching for it , I found that There might be 
# 2 predicotrs in the regression are "perfectly correlated" . 

require(stargazer)
stargazer(model_logit5,model_probit1, type = "text")
stargazer(to_be_predicted2,to_be_predicted3, type = "text")


# while comparing logit 5 by the probit 1 : 
# They both give strongly significant model with results while their variables estimated values are different 
# and it look a little bit bigger in favor of the logit model , but still both can work while checking the work loss.
# and still also in both models the "GENIDESCRIBEmale" is the only insignificant variable.
set.seed(1)
index<-sample(x=2,size=nrow(dat_use1),replace=TRUE,prob=c(0.7,0.3))
train<-dat_use1[index==1,]
test<-dat_use1[index==2,]
dim(dat_use1)
dim(train)
dim(test)

trainmodel <- glm(WRKLOSSRV ~ GENID_DESCRIBE + HADCOVID + ANXIOUS + WORRY + KINDWORK + EEDUC + works_remote + RRACE + RECVDVACC, family = binomial, data = dat_use1)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$WRKLOSSRV,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum

# The accuracy level is "0.9151" of predicting correctly.

use_varb2 <- (Household_Pulse_data$REGION == "West")
dat_use2 <- subset(Household_Pulse_data,use_varb2) 
NNobs <- length(dat_use2$WRKLOSSRV)
set.seed(1) 
graph_obs <- (runif(NNobs) < 0.9) 
dat_graph <-subset(dat_use2,graph_obs)  
plot(WRKLOSSRV ~ TBIRTH_YEAR, pch = 16, ylim = c(0,1), data = dat_graph, main = "work loss By Birth Year", xlab = "Birth Year", ylab = "Work loss", col = c("blue","grey"))

# We see people who got born the "2000's" are the most people who lost their jobs.

table2 <- table(Household_Pulse_data$WRKLOSSRV)
table2
prop.table(table2)


# by using proportions , we see only %11 of sample recently lost their jobs compared with % 88 who haven't lost their jobs recently.


use_varb <- (Household_Pulse_data$TBIRTH_YEAR >= 1956) & (Household_Pulse_data$TBIRTH_YEAR <= 1996)
dat_use1 <- subset(Household_Pulse_data,use_varb) 
summary(dat_use1)

 # we expanded the subset up to ages 66 
d_workloss <- data.frame(model.matrix(~ dat_use1$WRKLOSSRV))
d_marstat <- data.frame(model.matrix(~ dat_use1$MS))
d_race <- data.frame(model.matrix(~ dat_use1$RRACE))
summary(d_race)
d_hispanic <- data.frame(model.matrix(~ dat_use1$RHISPANIC))
d_gender <- data.frame(model.matrix(~ dat_use1$GENID_DESCRIBE))
d_recvacc <- data.frame(model.matrix(~ dat_use1$RECVDVACC))
summary(d_recvacc)
d_genidescribe <- data.frame(model.matrix(~ dat_use1$GENID_DESCRIBE))
summary(d_genidescribe)
d_anxious <- data.frame(model.matrix(~ dat_use1$ANXIOUS))
summary(d_anxious)
d_worry <- data.frame(model.matrix(~ dat_use1$WORRY))
d_hadcovid <- data.frame(model.matrix(~ dat_use1$HADCOVID))
d_worksremote <- data.frame(model.matrix(~ dat_use1$works_remote))
d_educ <- data.frame(model.matrix(~ dat_use1$EEDUC))
d_kind_work <- data.frame(model.matrix(~ dat_use1$KINDWORK))
d_region <- data.frame(model.matrix(~ dat_use1$REGION))




dat_for_analysis_sub <- data.frame(
  d_workloss[,2],
  dat_use1$TBIRTH_YEAR[!is.na(dat_use1$WRKLOSSRV)],
  d_educ[!is.na(dat_use1$WRKLOSSRV),1:5],
  d_marstat[!is.na(dat_use1$WRKLOSSRV),1:4],
  d_race[!is.na(dat_use1$WRKLOSSRV),2:4],
  d_hispanic[!is.na(dat_use1$WRKLOSSRV),2],
  d_gender[!is.na(dat_use1$WRKLOSSRV),1:2],
  d_region[!is.na(dat_use1$WRKLOSSRV),2:4])

names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "WRKLOSSRV"
names(dat_for_analysis_sub)[2] <- "TBIRTH_YEAR"
names(dat_for_analysis_sub)[17] <- "RHISPANIC"
summary(dat_for_analysis_sub)


require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$WRKLOSSRV)
restrict_1 <- (runif(NN) < 0.1) 
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)


sobj <- standardize(WRKLOSSRV ~ TBIRTH_YEAR + EEDUCHS.diploma + EEDUCsome.coll 
                    + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg +  MS.married + MS.widowed + MS.divorced + MS.never + RRACE.Black
                    + RRACEAsian + RRACEOther+ GENID_DESCRIBEmale + GENID_DESCRIBEfemale + 
                    + ANXIOUSno.anxiety.over.past.2.wks + ANXIOUSseveral.days.anxiety.over.past.2.wks 
                    , data = dat_train, family = binomial)



summary(RSNNOWRKRV)
# WHILE DOING THE SUMMARY FOR "REASON FOR WORKLOSS" , being "sick or disabled" showed a big number of people after 
# being "retired"
table(Household_Pulse_data$WRKLOSSRV,Household_Pulse_data$RSNNOWRKRV)
# THE "laid off" results takes the top amount of people followed by " retired" and then  "am/was sick w covid or caring for sick w covid" comes after.
 
# sobj linear regression : 

model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$WRKLOSSRV)
# sobj logit  model : 

model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$WRKLOSSRV)

#Random Forest
require('randomForest')
set.seed(1)
model_randFor <- randomForest(as.factor(WRKLOSSRV) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)

# Confusion Matrix 
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$WRKLOSSRV)

# sPIKESLAB : 

require(spikeslab)
set.seed(1)
model1_spikeslab <- spikeslab(sobj$formula, data = sobj$data)
summary(model1_spikeslab)
print(model1_spikeslab)
plot(model1_spikeslab)
