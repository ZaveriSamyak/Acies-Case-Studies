## importing data
library(readxl)
CRM <- read_excel(file.choose())
str(CRM)

#Performing Exploratory Data Analysis
head(CRM)
str(CRM)
dim(CRM)
colnames(CRM)
tail(CRM)
summary(CRM)

##change flag variables to factor variables
columns <- c("dependants", "phone", "default")
CRM[columns]=lapply(CRM[columns],factor)
str(CRM)

##missing values
colSums(is.na(CRM))

##check target variable proportion
count_check <- table(CRM$dependants)
prop.table(count_check)

##data preparation
library(caret)
set.seed(123)

trainin_index <- createDataPartition(CRM$dependants,p=0.8,list=FALSE)
train <- CRM[trainin_index,]
test <- CRM[-trainin_index,]

##checking class proportion in train data
count_train <- table(train$dependants)
prop.table(count_train)

##checking class proportion in test data
count_test <- table(test$dependants)
prop.table(count_test)

##iteration 1
regmodel1 <- glm(dependants~.,data=train,family=binomial)
summa=ry(regmodel1)

##wald test
library(aod)

##iteration 2
regmodel2 <- glm(TARGET_FLAG~.-AGE-SEX-NEW_CAR-JOB_COLOR,data=train,family=binomial)
summary(regmodel2)

##multicolinearity check
library(DAAG)
vif(regmodel2)

##iteration 3
regmodel3 <- glm(TARGET_FLAG~KIDSDRIV+HOMEKIDS+INCOME+MSTATUS+TRAVTIME+CAR_USE+BLUEBOOK+TIF+CAR_TYPE+CLM_FREQ+REVOKED+MVR_PTS+URBANCITY,data=train,family=binomial)
summary(regmodel3)

##multicolinearity check
library(DAAG)
vif(regmodel3)

##likelihood ratio test
library(lmtest)
lrtest(regmodel3)

##McFadden's pseudo R^2 
library(pscl)
pR2(regmodel3)

##Hosmer Lemeshow test
library(ResourceSelection)
hoslem.test(train$TARGET_FLAG,fitted(regmodel3))

##Somer's D in R
library(InformationValue)
somersD(train$TARGET_FLAG,fitted(regmodel3))

##preidcting values of Y for Train data
fit_train <- fitted(regmodel3,type='reponse')
head(fit_train,3)

fitclass <- ifelse(fit_train>0.5,1,0)
head(fitclass,3)

##confusion matrix in R test data
library(caret)
confusionMatrix(data=as.factor(fitclass),reference=train$TARGET_FLAG)
confusionMatrix(as.factor(fitclass),train$TARGET_FLAG)
help("confusionMatrix")


##predicting values of Y for Test data
prediction <- predict(regmodel3,test,type='response')
head(prediction,n=3)

prediction_class <- ifelse(prediction>0.5,1,0)
head(prediction_class,3)

##confusion matrix
library(caret)
confusionMatrix(data=as.factor(prediction_class),reference=test$TARGET_FLAG)

##ROC Curve in R train data
library(ROCR)
Roc_trainpredict<-prediction(fitted(regmodel3),train$TARGET_FLAG)
Roc_trainperf <- performance(Roc_trainpredict,'tpr','fpr')
plot(Roc_trainperf)
abline(0,1)
AUC <- performance(Roc_trainpredict,"auc")
AUC@y.values

##ROC Curve in R test data
library(InformationValue)
plotROC(test$TARGET_FLAG,prediction)

##Iteration 4
traincontroll<-trainControl(method="cv",number=5)
regmodel4 <-train(TARGET_FLAG~KIDSDRIV+HOMEKIDS+INCOME+MSTATUS+TRAVTIME+CAR_USE+BLUEBOOK+TIF+CAR_TYPE+CLM_FREQ+REVOKED+MVR_PTS+URBANCITY,data=train,method="glm",family=binomial,trControl=traincontroll)
regmodel4

##Predicting values of Y for test data
predicted_it4<-predict(regmodel4,test,'prob')
head(predicted_it4)

predicttt_class<-predict(regmodel4,test)
head(predicttt_class)

##Confusion matrix
library(caret)
confusionMatrix(data=as.factor(predicttt_class),reference=test$TARGET_FLAG)

##ROC curve in R test
library(InformationValue)
plotROC(test$TARGET_FLAG,predicted_it4)
