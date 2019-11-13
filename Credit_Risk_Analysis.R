

library(rpart)
View (GERMANCREDIT1_)



str(GERMANCREDIT1_)
# There are 1000 observations of 30 variables

View(GERMANCREDIT1_$RESPONSE)
sum(GERMANCREDIT1$RESPONSE)
#Proportion of good to bad cases is 700 

colSums(is.na(GERMANCREDIT1_)) # APP
#Missing values are found in variables

#Impute zeros to treat the missing values
GERMANCREDIT1_$NEW_CAR[is.na(GERMANCREDIT1_$NEW_CAR)] <- 0
GERMANCREDIT1_$USED_CAR[is.na(GERMANCREDIT1_$USED_CAR)] <- 0
GERMANCREDIT1_$FURNITURE[is.na(GERMANCREDIT1_$FURNITURE)] <- 0
GERMANCREDIT1_$RADIOTV[is.na(GERMANCREDIT1_$`RADIOTV`)] <- 0
GERMANCREDIT1_$EDUCATION[is.na(GERMANCREDIT1_$EDUCATION)] <- 0
GERMANCREDIT1_$RETRAINING[is.na(GERMANCREDIT1_$RETRAINING)] <- 0

colSums(is.na(GERMANCREDIT1_)) 

#Impute median to correct age
GERMANCREDIT1_$AGE[is.na(GERMANCREDIT1_$AGE)] <- median(GERMANCREDIT1_$AGE, na.rm= TRUE)
GERMANCREDIT1_$PERSONAL_STATUS[is.na(GERMANCREDIT1_$PERSONAL_STATUS)] <- mode(GERMANCREDIT1_$PERSONAL_STATUs, na.rm=TRUE)
as.factor(GERMANCREDIT1_$PERSONAL_STATUS)
View(GERMANCREDIT1_)

#Converting to factors
cols <- c("CHK_ACCT", "RESPONSE", "FOREIGN", "TELEPHONE", "OWN_RES", "NEW_CAR", "USED_CAR", "FURNITURE", 'RADIOTV', "SAV_ACCT"
          , "EMPLOYMENT", 'COAPPLICANT', "PRESENT_RESIDENT", "PROP_UNKN_NONE"
          , "OTHER_INSTALL", "RENT", "JOB", "HISTORY", "EDUCATION", "RETRAINING", "GUARANTOR", "REAL_ESTATE"
)
str(GERMANCREDIT1_)
GERMANCREDIT1_[cols] <- lapply(GERMANCREDIT1_[cols],factor)
sapply(GERMANCREDIT1_, class)
summary(GERMANCREDIT1_)


#Univariate Analysis

#Categorical Variables

table(GERMANCREDIT1_$CHK_ACCT)
table(GERMANCREDIT1_$HISTORY)
table(GERMANCREDIT1_$SAV_ACCT)
table(GERMANCREDIT1_$EMPLOYMENT)
table(GERMANCREDIT1_$PRESENT_RESIDENT)
table(GERMANCREDIT1_$JOB)
table(GERMANCREDIT1_$NEW_CAR)
table(GERMANCREDIT1_$USED_CAR)
table(GERMANCREDIT1_$FURNITURE)
table(GERMANCREDIT1_$RADIOTV)
table(GERMANCREDIT1_$EDUCATION)
table(GERMANCREDIT1_$RETRAINING)

library(ggplot2)
dat <- data.frame(table(GERMANCREDIT1_$FOREIGN,GERMANCREDIT1_$RESPONSE))
names(dat) <- c("FOREIGN","RESPONSE","Count")
ggplot(data=dat, aes(x=FOREIGN, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

dat <- data.frame(table(GERMANCREDIT1_$HISTORY,GERMANCREDIT1_$RESPONSE))
names(dat) <- c("HISTORY","RESPONSE","Count")
ggplot(data=dat, aes(x=HISTORY, y=Count, fill=RESPONSE)) + geom_bar(stat="identity")

#Numeric Variables
hist(GERMANCREDIT1_$DURATION, main = "Distribution of Duration")
hist(GERMANCREDIT1_$AMOUNT, main = "Distribution of Amount")
hist(GERMANCREDIT1_$INSTALL_RATE, main = "Distribution of Install Rate")
hist(GERMANCREDIT1_$AGE, main = "Distribution of Age")
hist(GERMANCREDIT1_$NUM_CREDITS, main = "Distribution of Credits")
hist(GERMANCREDIT1_$NUM_DEPENDENTS, main = "Distribution of Dependents")

par(mfrow=c(2,3))
summary(GERMANCREDIT1_$DURATION)
summary(GERMANCREDIT1_$AMOUNT)
summary(GERMANCREDIT1_$INSTALL_RATE)
summary(GERMANCREDIT1_$AGE)
summary(GERMANCREDIT1_$NUM_CREDITS)
summary(GERMANCREDIT1_$NUM_DEPENDENTS)

install.packages("pastecs")
library(pastecs)
hs0<-as.data.frame(colnames(GERMANCREDIT1_)) 
attach(hs0)
num_vars <-cbind(GERMANCREDIT1_$DURATION, GERMANCREDIT1_$AMOUNT, GERMANCREDIT1_$INSTALL_RATE, GERMANCREDIT1_$AGE, 
                 GERMANCREDIT1_$NUM_CREDITS,GERMANCREDIT1_$NUM_DEPENDENTS)
stat.desc(num_vars, basic = F)

#Correlation Matrix
corsum<-GERMANCREDIT1_ [,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)]
cormat<-cor(corsum)
cormat
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
corsum<-GERMANCREDIT1_ [,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)]
chart.Correlation(corsum, histogram=TRUE, pch=19)

#Logistic Regression 
model <- glm(formula = GERMANCREDIT1_$RESPONSE ~., family = binomial(link = "logit"), data=GERMANCREDIT1_)
summary(model)



library('rpart')
library(rpart.plot)
install.packages("rpart.plot")

#DT 1
rpModel1<-rpart(RESPONSE ~ ., data=GERMANCREDIT1_, method="class")
print(rpModel1)

plot(rpModel1, uniform=TRUE,  main="Decision Tree for German Credit Analysis")
text(rpModel1, use.n=TRUE, all=TRUE, cex=.7)


rpTree<-rpart(RESPONSE~.,data=mdTrn,method="class",parms=list(prior=c(.70,.30),loss=costMatrix,
                                                             split="information"))
#DT2
rpModel2<- rpart(RESPONSE ~., data = GERMANCREDIT1_ , method="class", maxdepth = 15, 
            minsplit = 15, xval = 10, cp=.01, parms = list(split = 'information'))
print(rpModel2)
summary(rpModel2)
par(mfrow=c(1,1))

###Lift
predTrnProb=predict(rpModel2, GERMANCREDIT1_, type='prob')
head(predTrnProb)
trnSc <- subset(GERMANCREDIT1_, select=c("RESPONSE"))
trnSc$score<-predTrnProb[, 1]
trnSc<-trnSc[order(trnSc$score, decreasing=TRUE),]
str(trnSc)
levels(trnSc$RESPONSE)
trnSc$RESPONSE<-as.numeric(as.character(trnSc$RESPONSE))
str(trnSc)
trnSc$cumDefault<-cumsum(trnSc$RESPONSE)
head(trnSc)
plot(seq(nrow(trnSc)), trnSc$cumDefault,type = "l", xlab='#cases', ylab='#default')

library(dplyr)
trnSc["bucket"]<-ntile(-trnSc[,"score"], 10)  
decGroups<-group_by(trnSc, bucket)
decLifts<-summarise(decGroups, count=n(), numDefaults=sum(RESPONSE))
decLifts<-decLifts %>% mutate(defRate=numDefaults/count, cumDefRate=cumsum(numDefaults)/cumsum(count),lift=cumDefRate/(sum(numDefaults)/sum(count)) )
plot(decLifts$bucket, decLifts$lift, xlab="deciles", ylab="Cumulative Decile Lift", type="l")
barplot(decLifts$numDefaults, main="numDefaults by decile", xlab="deciles")

rm(trnSc)
#DT3
rpModel3<- rpart(RESPONSE ~., data = GERMANCREDIT1_ , method="class", maxdepth = 15, 
                 minsplit = 40, xval = 10, parms = list(split = 'gini'))
print(rpModel3)
summary(rpModel3)



#DT4
rpModel4<- rpart(RESPONSE ~., data = GERMANCREDIT1_ , method="class", maxdepth = 5, 
                 minsplit = 50, xval = 15,cp = 0.02, parms = list(split = 'information'))
print(rpModel4)
summary(rpModel4)

#Obtain the model's predictions on the entire dataset
predTrn_whole=predict(rpModel4, data=GERMANCREDIT1_, type='class')

#Confusion table
table(pred = predTrn_whole, true=GERMANCREDIT1_$RESPONSE)
#Accuracy
mean(predTrn_whole==GERMANCREDIT1_$RESPONSE)

### Gini

### LIFT CHARTS
install.packages("dplyr")
library(dplyr)



#get the 'scores' from applying the model to the data
predProb=predict(rpModel2, GERMANCREDIT1_, type='prob')
head(predTrnProb)

### lets make a table with response and score for each observation

#we need the score and actual class (OUTCOME) values
Scores <- subset(GERMANCREDIT1_, select=c("RESPONSE"))  # selects the OUTCOME column into trnSc
Scores$score<-predProb[, 1]  #add a column named 'Score' with prob values

Scores ## first glace, does not look very accurate, lets sort
Scores<-Scores[order(Scores$score, decreasing=TRUE),]

## Accuracy....
pred <- predict(rpModel2, GERMANCREDIT1_, type='class')
A1 <- mean(pred==GERMANCREDIT1_$RESPONSE)

str(Scores)

Scores$cumDefault<-cumsum(Scores$RESPONSE)
head(Scores)

### polt curve

plot(seq(nrow(Scores)), Scores$cumDefault,type = "l", xlab='#cases', ylab='#default')

### Divide the data into 10 (for decile lift) equal groups
# this creates a new column with group number for each row
Scores["bucket"]<-ntile(-Scores[,"score"], 10)  




#split the data into training and test(validation) sets 
set.seed(123)
nr=nrow(GERMANCREDIT1_)
trnIndex = sample(1:nr, size = round(0.8*nr), replace=FALSE)
mdTrn=GERMANCREDIT1_[trnIndex,]   #training data with the randomly selected row-indices
mdTst = GERMANCREDIT1_[-trnIndex,]  #test data with the other row-indices
dim(mdTrn) 
dim(mdTst)

### Model1 TRAIN

Model1<- rpart(RESPONSE ~., data = mdTrn , method="class", maxdepth = 15, 
               minsplit = 15, cp=.01, parms = list(split = 'information'))
Model2<- rpart(RESPONSE ~., data = mdTrn , method="class", maxdepth = 10, 
               minsplit = 20, cp=.02, parms = list(split = 'information'))
Model3<- rpart(RESPONSE ~., data = mdTrn , method="class", maxdepth = 20, 
               minsplit = 50, cp=.0001, parms = list(split = 'information'))
Model4<- rpart(RESPONSE ~., data = mdTrn , method="class", maxdepth = 30, 
               minsplit = 150, cp=.01, parms = list(split = 'information'))

### Model1 Evaluation
predTrain=predict(Model4, mdTrn, type='class')
table(pred = predTrain, true=mdTrn$RESPONSE)
mean(predTrain==mdTrn$RESPONSE)

### Performance Measures 
cm <- table(pred=predict(Model2,mdTst, type="class"), true=mdTst$RESPONSE)     ##############ERROR#####
n = sum(cm) # number of instances
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 2, sum) # number of instances per class
colsums = apply(cm, 1, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
accuracy
precision = diag / colsums
precision
recall = diag / rowsums 
recall
f1 = 2 * precision * recall / (precision + recall) 
f1
library(C50)
rpTree<-C5.0(RESPONSE ~ ., data=mdTrn, method="class", parms = list( prior = c(50,.50), 
                                                                      loss = costMatrix, split = "information"))
summary(rpTree)


mdTst$score<-predict(rpTree,type='prob',mdTst)
pred<-prediction(mdTst$score[,2],mdTst$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


### c50 package
install.packages("C50")
library(C50)
set.seed(600)

tree1 <- C5.0(RESPONSE~., data=mdTrn, method="class")
summary(tree1)
# plot(tree1)
tree1pred <- predict.C5.0(tree1, newdata = mdTst[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)], type="class")
table(pred = tree1pred, true=mdTst$RESPONSE)
mean(tree1pred==mdTst$RESPONSE)


tree2 <- C5.0(RESPONSE~., data=mdTrn, method="class", subset = TRUE, winnow = TRUE, rules = TRUE)
tree2pred <- predict.C5.0(tree2, newdata = mdTst[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)], type="class")
table(pred = tree2pred, true=mdTst$RESPONSE)
mean(tree2pred==mdTst$RESPONSE)
summary(tree2)
C5imp(tree2)

tree3 <- C5.0(RESPONSE~., data=mdTrn, method="class", winnow = FALSE, rules = TRUE,  earlyStopping = TRUE, CF = .00001, miniCases = 10, seed = sample.int(4096, size = 1) -
                1L)
#summary(tree3)
tree3pred <- predict.C5.0(tree3, newdata = mdTst[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)], type="class")
table(pred = tree3pred, true=mdTst$RESPONSE)
mean(tree3pred==mdTst$RESPONSE)

C5.0Control(subset = TRUE, bands = 0, winnow = FALSE,
            noGlobalPruning = FALSE, , minCases = 2,
            fuzzyThreshold = FALSE,, seed = sample.int(4096, size = 1) -
              1L, earlyStopping = TRUE, label = "outcome")


### ROC and AUC
#score test data set
install.packages("ROCR")
library(ROCR)

mdTst$score<-predict(rpTree,type='prob',mdTst)
pred<-prediction(mdTst$score[,2],mdTst$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


### Alternate Thresholds
CTHRESH<-0.3
predProbTrn<-predict(rTree, GERMANCREDIT1_, type='prob')
#Confusion table
predTrn<-ifelse(predProbTrn[,'1'] >= CTHRESH, '1', '0')
ct<-table( pred = predTrn, true=GERMANCREDIT1_$RESPONSE)
#Accuracy
mean(predTrn==GERMANCREDIT1_$RESPONSE)

### Cost Matrix
library(rpart)
costMatrix <- matrix(c(0,1,5, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
costMatrix

rpTree<-rpart(RESPONSE ~ ., data=mdTrn, method="class", parms = list(  
                                                                       loss = costMatrix, split = "information"))
summary(rpTree)
#Obtain the model's predictions on the training data
predTrn=predict(rpTree, mdTrn, type='class')
#Confusion table
table(pred = predTrn, true=mdTrn$RESPONSE)
#Accuracy
mean(predTrn==mdTrn$RESPONSE)

#Obtain the model's predictions on the test data
predTrn=predict(rpTree, mdTst, type='class')
#Confusion table
table(pred = predTrn, true=mdTst$RESPONSE)
#Accuracy
mean(predTrn==mdTst$RESPONSE)




### ROC

install.packages("pROC")
install.packages("OptimalCutpoints")
library(pROC)
library(OptimalCutpoints)

rocobj <- plot.roc(mdTst$RESPONSE, mdTst$score[,2], percent = TRUE, main="ROC", col="#1c61b6", add=FALSE)


mdTst$RESPONSENEW <- as.character(mdTst$RESPONSE)
mdTst$Score2 <- as.numeric(mdTst$score[,2])

plot(rocobj)

optimal.cutpoint.Youden <- optimal.cutpoints(X = mdTst$Score2, status = mdTst$RESPONSENEW, tag.healthy = 0,
                                             methods = "Youden", data = mdTst, pop.prev = NULL,
                                             control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, 
                                             trace = FALSE)
summary(optimal.cutpoint.Youden)
plot(optimal.cutpoint.Youden)



#ROC & AUC curves
mdTst1 <- mdTst
library(ROCR)
#score test data set
mdTst1$score<-predict(rpTree,type='prob',mdTst1)
predTst<-prediction(mdTst1$score[,2],mdTst1$RESPONSE)
perf <- performance(predTst,"lift","rpp")
plot(perf, main="lift curve",colorize=T)

##ROC curve : 
perf3 <- performance(predTst,"tpr","fpr")
plot(perf3,colorize=T)
summary(perf3)

#AUC vaue
auc.perf = performance(predTst, measure = "auc")
auc.perf@y.values 

#optimal cutoff
ost.perf = performance(predTst, "cost")
predTst@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


#head(predTst[,1])  #this will list probs for zero
#head.........,2]   # and the ones

## and then performance 

#BUILD A MODEL ON DT2 WITH CM INCLUDED
rpModel2<- rpart(RESPONSE ~., data = GERMANCREDIT1_ , method="class", maxdepth = 15, 
            minsplit = 15, xval = 10, cp=.01, 
            parms = list(split = 'information'))

predTest=predict(rpModel2, GERMANCREDIT1_, type='class')
table(pred = predTest, true=GERMANCREDIT1_$RESPONSE)
mean(predTest==GERMANCREDIT1_$RESPONSE)

CTHRESH=0.8
predProbTrn=predict(rpModel2, GERMANCREDIT1_, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[,2] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=GERMANCREDIT1_$RESPONSE)
#Accuracy
mean(predTrn==GERMANCREDIT1_$RESPONSE)

#Calculate and apply the ‘theoretical’ threshold and assess performance – what do you
#notice, and how does this relate to the answer from (a) above. 

### ROC and AUC
#score test data set
install.packages("ROCR")
library(ROCR)
par(mfrow=c(1,4))
Cscore<-predict(Model1,type='prob',GERMANCREDIT1_)
pred<-prediction(Cscore[,2],GERMANCREDIT1_$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf)

##AUC
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#Use misclassification costs in building the tree models (rpart and C5.0) – are the trees
#here different than ones obtained earlier? Compare performance of these two new
#models with those obtained earlier (in part 3a, b above).
set.seed(123)
nr=nrow(GERMANCREDIT1_)                                                
trnIndex = sample(1:nr, size = round(0.8*nr), replace=FALSE)
mdTrn=GERMANCREDIT1_[trnIndex,]   #training data with the randomly selected row-indices
mdTst = GERMANCREDIT1_[-trnIndex,]  #test data with the other row-indices
dim(mdTrn) 
dim(mdTst)

rpModel2<- rpart(RESPONSE ~., data = mdTst , method="class", maxdepth = 15, 
            minsplit = 15, xval = 10, cp=.01, 
            parms = list(split = 'information', loss = costMatrix))


rpart.plot::prp(rpModel2, type=1, extra=1)

nodes<-as.numeric(rownames(rpModel2$frame))
count(nodes)
max(rpart:::tree.depth(nodes))
summary(rpModel2)
par(mfrow=c(1,1))

library(dplyr)


#The predicted probabilities can be used to determine how the model may be implemented. 
#We can sort the data from high to low on predicted probability of credit risk
#Then, going down the cases from high to low probabilities,
#one may be able to determine an appropriate cutoff probability values 
#above this can be considered acceptable credit risk.
#The use of cost figures given above can help in this analysis. For this, first sort the validation data on predicted probability. Then, for each validation case, calculate the actual cost/benefit of extending credit. Add a separate column for the cumulative net cost/benefit. How far into the validation data would you go to get maximum net benefit? In using this model to score future credit applicants, what cutoff value for predicted probability would you recommend? Provide appropriate performance values to back up your recommendation.

#We have plotted a graph for our model based on the above guidelines. Below is the scatter plot for the same.

library(dplyr)

PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rpModel2,mdTst, type="prob")[,2] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, mdTst$RESPONSE)
#check what is in prLifts ....head(prLifts)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`mdTst$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits)

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))



install.packages("randomForest")
library('randomForest')

set.seed(123)
train <- sample(nrow(GERMANCREDIT1_), 0.7*nrow(GERMANCREDIT1_), replace = FALSE)
TrainSet <- GERMANCREDIT1_[train,]
ValidSet <- GERMANCREDIT1_[-train,]
summary(TrainSet)
summary(ValidSet)


model<- randomForest(RESPONSE~.,data = TrainSet)
model

model1<- randomForest(RESPONSE~.,na.action =na.omit,data = TrainSet)
model1

model2<- randomForest(RESPONSE~.,mtry = 6,data = TrainSet)
model2

model3<- randomForest(RESPONSE~.,mtry = 7,na.action = na.omit,data = TrainSet)
model3

model4<- randomForest(RESPONSE~.,mtry = 8,data = TrainSet)
model4

model5<- randomForest(RESPONSE~.,mtry = 9,na.action = na.omit,data = TrainSet)
model5

model6<- randomForest(RESPONSE~.,mtry = 10,data = TrainSet)
model6

model7<- randomForest(RESPONSE~.,mtry = 16,na.action = na.omit,data = TrainSet)
model7

model8<- randomForest(RESPONSE~.,mtry = 17,na.action = na.omit,data = TrainSet)
model8

model9<- randomForest(RESPONSE~.,mtry = 18,na.action = na.omit,data = TrainSet)
model9

model10<- randomForest(RESPONSE~.,mtry = 19,na.action = na.omit,data = TrainSet)
model10

model11<- randomForest(RESPONSE~.,mtry = 20,na.action = na.omit,data = TrainSet)
model11

summary(model7)
# Predicting on train set
predTrain <- predict(model11, TrainSet, na.action = na.omit,type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$RESPONSE)  

# Predicting on Validation set
predValid <- predict(model11, ValidSet,na.action = na.omit, type = "class")
# Checking classification accuracy
mean(predValid==ValidSet$RESPONSE,na.rm=TRUE)                    
table(predValid,ValidSet$RESPONSE)


# To check important variables
importance(model3)        
varImpPlot(model3)   

#Model3 is the best fit model.


Model1<- rpart(RESPONSE ~., data = TrainSet , method="class", maxdepth = 15, 
               minsplit = 15, cp=.01, parms = list(split = 'information'))
Model2<- rpart(RESPONSE ~., data = TrainSet, method="class", maxdepth = 10, 
               minsplit = 20, cp=.02, parms = list(split = 'information'))
Model3<- rpart(RESPONSE ~., data = TrainSet , method="class", maxdepth = 20, 
               minsplit = 50, cp=.0001, parms = list(split = 'information'))
Model4<- rpart(RESPONSE ~., data = TrainSet , method="class", maxdepth = 30, 
               minsplit = 150, cp=.01, parms = list(split = 'information'))

# Predicting on train set
predTrain <- predict(Model2, TrainSet, na.action = na.omit,type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$RESPONSE)  

# Predicting on Validation set
predValid <- predict(Model2, ValidSet,na.action = na.omit, type = "class")
# Checking classification accuracy
mean(predValid==ValidSet$RESPONSE,na.rm=TRUE)                    
table(predValid,ValidSet$RESPONSE)

#ROC CURVE

library(ROCR)

Cscore<-predict(model9,type='prob',GERMANCREDIT1_)
pred<-prediction(Cscore[,2],GERMANCREDIT1_$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf, main= "ROC Curve for RF-Model")


Cscore<-predict(Model2,type='prob',GERMANCREDIT1_)
pred<-prediction(Cscore[,2],GERMANCREDIT1_$RESPONSE)
perf <- performance(pred,"tpr","fpr")
plot(perf, main= "ROC Curve for DT-Model")

par(mfrow = c(1,2))

#COST

library(dplyr)

PROFITVAL=100
COSTVAL=-500

scoreTst=predict(Model2,mdTst, type="prob")[,2] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, mdTst$RESPONSE)
#check what is in prLifts ....head(prLifts)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`mdTst$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))

plot(prLifts$cumProfits, main="DT-Model")

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))




########

