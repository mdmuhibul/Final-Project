# Final Project Group : 
# Amira Elmakawy , Muhibul Islam .

attach(acs2017_ny)
acs2017_ny$MARST <- as.factor(acs2017_ny$MARST)
levels(acs2017_ny$MARST) <- c("married spouse present","married spouse absent","separated","divorced","widowed","never married")
summary(acs2017_ny$MARST)

acs2017_ny$RACE <- as.factor(acs2017_ny$RACE)
levels(acs2017_ny$RACE) <- c("White",	"Black/African American/Negro", "American Indian or Alaska Native", "Chinese", "Japanese",	"Other Asian or Pacific Islander", "Other race, nec",	"Two major races", "Three or more major races")
summary(acs2017_ny$RACE)
summary(acs2017_ny$EDUC)
# Will use Subset of people from 25 to 55 as those who are the prime-age workers.

use_varb <- (acs2017_ny$AGE >= 25) & (acs2017_ny$AGE <= 55)
dat_use <- subset(acs2017_ny,use_varb)
summary(dat_use)

# By doing Basic Summary Statistics For Labor force and Education
summary(dat_use$LABFORCE) # The mean of labor force in the sample is 1.816 .
summary(dat_use$EDUC)
table(dat_use$EDUC, dat_use$LABFORCE)  
# Majority of labor force is from the advanced education & Grade 12 workers.
# And By using proportions of labor force to the education : 

prop.test(table(as.numeric(dat_use$EDUC), dat_use$LABFORCE))
# This shows that there's a relation between education the employment levels as 
# p-value is "2.2e-16" and less than the model significance level , In this case ;
# we reject the null hypothesis of no relation. as there's a difference in probabilities and proportions.

# By Simple Regression :

model_1 <- lm(LABFORCE ~ EDUC + female + AGE +  MARST + RACE,data = dat_use)
summary(model_1)

# So when adding the Marital status variables with Race , female , Age Together with the education as independent variables , The model is still 
# Statistically Significant as p-value is "2.2e-16"  which is less than the significance level of 0.1 .

# And By adding the MARST squared to the model and doing the regression : 
model_2 <- lm(LABFORCE ~ AGE + EDUC+ female +  + I(MARST)^2 + MARST +RACE,data = dat_use)
summary(model_2)
# So by adding the marital status squared in the model , the model still significant except for some
# categories in the marital status factor such as "MARSTdivorced , MARSTmarried spouse absent ,MARSTwidowed , 
# in addition to EduGrade10 with some types of Race such as RACEAmerican Indian or Alaska Native .

# By adding the AGE squared and cubed to the model : 
model_3 <- lm(LABFORCE ~ AGE + I(AGE)^2 + I(AGE)^3 + EDUC+ female +  + I(MARST)^2 + MARST +RACE,data = dat_use)
summary(model_3)

# BY DOING SOME INTERACTIONS :

model_4 <- lm(LABFORCE ~ AGE + I(EDUC)^2 + EDUC*MARST + female + MARST + RACE,data = dat_use)
summary(model_4)

model_5 <- lm(LABFORCE ~ AGE + I(AGE^2) + EDUC+ female +  + I(MARST)^2 + MARST +RACE,data = dat_use)
summary(model_5)



# Linear Model still the same unchanged and It will be statistically significant as p-value is small and less than 0.1.
# This also may indicate that most of the model's independent variables are exogenous and important to the dependent variable which is 
# The labor force that is can stongly depend on them except for few or some of their categories "The ones mentioned above".
# So in this case we can reject the null hypothesis of no relation between Labor force participation and ( AGE , EDUCATION , RACE , MARITAL STATUS , AND BEING A FEMALE)
# LABOR FORCE PARTICIPATION dependent positively on the EDUCATION. So education has to be a main factor to be considered in this relation.

summary(dat_use$LABFORCE)
dat_use$IN_LABFORCE <- dat_use$LABFORCE ==2
summary(dat_use$IN_LABFORCE)

# While doing summary statistics for people who are in the workforce , Apparently the majority is participated in Labor force.
# There are 63,150 persons joined the labor force while only 14,232 are not in the labor force.

# NON - LINEAR MODEL :

model_logit1 <- glm(IN_LABFORCE ~  AGE + I(AGE^2) + EDUC+ female +  MARST +RACE , family = binomial, data = dat_use)
summary(model_logit1)
NNobs <- length(dat_use$IN_LABFORCE)
set.seed(1) 
graph_obs <- (runif(NNobs) < 0.1) 
dat_graph <-subset(dat_use,graph_obs)  
plot(IN_LABFORCE ~ jitter(AGE, factor = 2), pch = 16, ylim = c(0,1), data = dat_graph, main = "Labor Force Participation by Age", xlab = "Age", ylab = "Labor Force Status", col = c("brown","black"))
to_be_predicted <- data.frame(AGE = 25:55, EDUC = "4 years of college", female = 1, 
                              MARST = "married spouse present",  RACE = "Black/African American/Negro")
to_be_predicted$yhat <- predict(model_logit1, newdata = to_be_predicted, type = "response")
lines(yhat ~ AGE, data = to_be_predicted, col = "pink")
summary(to_be_predicted$yhat)
# Mean is 0.8639 for married-spouse present women.

plot(IN_LABFORCE ~ jitter(AGE, factor = 2), pch = 16, ylim = c(0,1), data = dat_graph, main = "Labor Force Participation by Age", xlab = "Age", ylab = "Labor Force Status", col = c("brown","black"))
to_be_predicted <- data.frame(AGE = 25:55, EDUC = "4 years of college", female = 1, 
                              MARST = "married spouse absent",  RACE = "Black/African American/Negro")
to_be_predicted$yhat <- predict(model_logit1, newdata = to_be_predicted, type = "response")
lines(yhat ~ AGE, data = to_be_predicted, col = "blue")
summary(to_be_predicted$yhat)
 # mean is 0.7913 for married-spouse absent women .

# FOR MALES : 
to_be_predicted2 <- data.frame(AGE = 25:55, EDUC = "4 years of college", female = 0, 
                               MARST = "married spouse present",  RACE = "Black/African American/Negro")
to_be_predicted2$yhat <- predict(model_logit1, newdata = to_be_predicted2, type = "response")
lines(yhat ~ AGE, data = to_be_predicted2, col = "black")
summary(to_be_predicted2$yhat)

# Mean is 0.92 for married male .

# COMPARING THIS TO FEMALES WHO ARE "NEVER MARRIED" :

plot(IN_LABFORCE ~ jitter(AGE, factor = 2), pch = 16, ylim = c(0,1), data = dat_graph, main = "Labor Force Participation by Age", xlab = "Age", ylab = "Labor Force Status", col = c("brown","black"))
to_be_predicted <- data.frame(AGE = 25:55, EDUC = "4 years of college", female = 1, 
                              MARST = "never married",  RACE = "Black/African American/Negro")
to_be_predicted$yhat <- predict(model_logit1, newdata = to_be_predicted, type = "response")
lines(yhat ~ AGE, data = to_be_predicted, col = "red")
summary(to_be_predicted$yhat)
# mean is 0.8157 for "never married" women .



# The mean for men participation in the labor force is always higher than the mean for women participation 
# in the labor force , as mean for men is 0.925 while the mean for married women is 0.8639 . 
# While the mens' labor force participation is also more than the NEVER MARRIED women rate participation in the LF.
# And also this is obvious in the graph that mens' line is higher than married women's and by seeing the predicted
# values of both of them , 92% mean for married men versus 86% mean for married women.
# In general , These statistics so far indicates that all the independent variables are exogenous except for
# some or few of their categories while the education variables gets almost the highest estimate which indicates 
# that Education is one factor that affects on the labor force participation.

# BY using confusion matrix to check the errors types in the logit model : 

set.seed(1)
index<-sample(x=2,size=nrow(dat_use),replace=TRUE,prob=c(0.7,0.3))
train<-dat_use[index==1,]
test<-dat_use[index==2,]
dim(dat_use)
dim(train)
dim(test)
trainmodel<-glm(IN_LABFORCE ~  AGE + I(AGE^2) + EDUC+ female +  MARST +RACE,
                family = binomial, data = dat_use)
prob<-predict(object=trainmodel,newdata=test,type="response")
pred<-cbind(test,prob)
pred<-transform(pred,predict=ifelse(prob<=0.5,0,1))
ta<-table(pred$IN_LABFORCE,pred$predict)
ta
sum_diag<-sum(diag(ta))
sum<-sum(ta)
sum_diag/sum

# There are less type II errors (287) than type I (4034) which indicates that the model is significant.

## By doing the model matrix: 

# FOR DEPENDENT VARIABLE : 
d_IN_LABFORCE <- data.frame(model.matrix(~ dat_use$IN_LABFORCE))
summary(d_IN_LABFORCE)
# EDUCATION ADVANCED DEGREE 
d_educ_advdeg <- data.frame(model.matrix(~ dat_use$educ_advdeg))
summary(d_educ_advdeg)
d_educ_college <- data.frame(model.matrix(~ dat_use$educ_college))
summary(d_educ_college)

#Marital Status
d_marst <- data.frame(model.matrix(~ dat_use$MARST))
summary(d_marst)
#Race
d_race <- data.frame(model.matrix(~ dat_use$RACE))
summary(d_race)

dat_for_analysis_sub <- data.frame(
  d_IN_LABFORCE[,2],
  d_educ_advdeg[!is.na(dat_use$IN_LABFORCE),2],
  d_marst[!is.na(dat_use$IN_LABFORCE),2:5],
  d_race[!is.na(dat_use$IN_LABFORCE),2:9])
summary(dat_for_analysis_sub)


names(dat_for_analysis_sub) <- sub("...2","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub) <- sub("..is.na.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub) <- sub("dat_use.","",names(dat_for_analysis_sub))



require("standardize")
set.seed(1)
NN <- length(dat_for_analysis_sub$d_IN_LABFORCE.)
restrict_1 <- (runif(NN) < 0.1) 
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)
sobj <- standardize(d_IN_LABFORCE. ~ d_educ_advdegIN_LABFORCE.+ MARSTmarried.spouse.absent + MARSTseparated
                    + MARSTdivorced + MARSTwidowed + RACEBlack.African.American.Negro + RACEAmerican.Indian.or.Alaska.Native 
                    + RACEChinese + RACEJapanese + RACEOther.Asian.or.Pacific.Islander+RACEOther.race..nec  + RACETwo.major.races +RACEThree.or.more.major.races, dat_train, family = binomial)
s_dat_test <- predict(sobj, dat_test)


#Linear
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
summary(pred_vals_lpm)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
pred_lm <- table(pred = pred_model_lpm1, true = dat_test$d_IN_LABFORCE.)
pred_lm
(pred_lm[1,1]+pred_lm[2,2])/sum(pred_lm)



#Type 1 errors were (42594) while type 2 errors where (1606).

model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
summary(pred_vals)
pred_model_logit1 <- (pred_vals > 0.8)
pred_logit <- table(pred = pred_model_logit1, true = dat_test$d_IN_LABFORCE.)
pred_logit
(pred_logit[1,1]+pred_logit[2,2])/sum(pred_logit)

# Huge amount of type on errors, still big amount of type 2 errors. Model logit
# still a better predictor than linear model because we have an accuracy of 0.69
# The logit model is a slightly better predictor out of the of the two models. 

# by using RANDOM FOREST METHOD :

require('randomForest')
set.seed(1)
model_randFor <- randomForest(as.factor(d_IN_LABFORCE.) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),2)
varImpPlot(model_randFor)

# confusion matrix 

pred_model1 <- predict(model_randFor,  s_dat_test)
pred_rf <- table(pred = pred_model1, true = dat_test$d_IN_LABFORCE)
pred_rf 
(pred_rf[1,1]+pred_rf[2,2])/sum(pred_rf)

# Random forest have a 81 percent accuracy when predicting train and
# test data.
#education is a good predictor for random forest.

# By using Spike and Slab: 

require(spikeslab)
set.seed(54321)
model1_spikeslab <- spikeslab(sobj$formula, data = sobj$data)
summary(model1_spikeslab)
print(model1_spikeslab)
plot(model1_spikeslab)




# Elastic Net

require(glmnet)
model1_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$d_IN_LABFORCE.) 

print(model1_elasticnet)

cvmodel1_elasticnet = cv.glmnet(data.matrix(sobj$data[,-1]),data.matrix(sobj$data$d_IN_LABFORCE)) 
cvmodel1_elasticnet$lambda.min
log(cvmodel1_elasticnet$lambda.min)
coef(cvmodel1_elasticnet, s = "lambda.min")

pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
table(pred = pred_model1_elasnet, true = dat_test$d_IN_LABFORCE)

model2_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$d_IN_LABFORCE, alpha = 0) 
print(model2_elasticnet)




# LOESS

model_loess1 <- loess(LABFORCE ~ AGE,data = dat_use)
y_loess1_pred <- predict(model_loess1, data.frame(AGE = seq(25, 55, 1)), se
                         = TRUE)
plot(seq(25, 55, 1),y_loess1_pred$fit)
