#Business Analysis Project 2
#Mingzi Tian
#04/09/2020

#######################################
#Load the packages we will use
library(ggplot2)
library(dataQualityR)
library(caret)
library(naniar)
library(dplyr)
#######################################

getwd()
setwd("/Users/tian/Desktop/Business Analytics/BA Project 2")
credit <- read.csv("application_train_s20.csv",header=TRUE, stringsAsFactors=TRUE)
str(credit)
summary(credit)

application_train <- read.csv("/Users/tian/Desktop/Business Analytics/BA Project 2/application_train_s20.csv")

vis_miss(application_train, warn_large_data=FALSE)

missing_percentage<-apply(application_train, 2, function(col)sum(is.na(col))/length(col))
missing_percentage[missing_percentage>0.2]
missing_percentage_df<-as.data.frame(missing_percentage[missing_percentage>0.2])
index<-rownames(missing_percentage_df)
application_train_2<- application_train[,!(names(application_train) %in% index)]

correlation_percentage<-apply(application_train_2, 2, function(col)chisq.test(application_train_2$TARGET,col)$p.value)
correlation_percentage

correlation_percentage[correlation_percentage>0.05]
correlation_df<-as.data.frame(correlation_percentage[correlation_percentage>0.05])
index<-rownames(correlation_df)
index_2<-c(index,"TARGET")
application_train_3<- application_train_2[,(names(application_train_2) %in% index_2)]
colnames(application_train_3)

application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR<-as.character(application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR)
application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new<-ifelse(  is.na(application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR),"NULL",application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR)
application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new<-as.factor(application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new)
table(application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new)

application_train_3$AMT_REQ_CREDIT_BUREAU_DAY<-as.character(application_train_3$AMT_REQ_CREDIT_BUREAU_DAY)
application_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new<-ifelse(  is.na(application_train_3$AMT_REQ_CREDIT_BUREAU_DAY),"NULL",application_train_3$AMT_REQ_CREDIT_BUREAU_DAY)
application_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new<-as.factor(application_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new)
table(application_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new)

application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK<-as.character(application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK)
application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new<-ifelse(  is.na(application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK),"NULL",application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK)
application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new<-as.factor(application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new)
table(application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new)

to.remove <-c("AMT_REQ_CREDIT_BUREAU_HOUR","AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK")
application_train_4 <- application_train_3[,-which(names(application_train_3) %in% to.remove)]
application_train_4  <- na.omit(application_train_4) 

##
application_train_4[,c(2,6:17)] <- lapply(application_train_4[,c(2,6:17)], as.factor)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
application_train_4[,3:5] <- lapply(application_train_4[,3:5], normalize)

lapply(application_train_4[,6:20],table)
str(application_train_4)
summary(application_train_4)
##
######################################
######################################

#model test
require(randomForest)

set.seed(2)
split <- (.75)
index <- createDataPartition(application_train_4$TARGET, p=split, list=FALSE)

train.df <- application_train_4[index,]
test.df <- application_train_4[-index,]
remove(index)

##method1 
fitControl <- trainControl(method="none")
rf.model <- train(train.df[,3:20], train.df[,2],
                  method="rf",
                  trControl=fitControl)

rfImp <- varImp(rf.model) # found option "rfimputate" in dropdown list, check later????
rfImp
plot(rfImp)

rf.predict <- predict(rf.model, test.df[,2:20], type="raw")
rf.predict
confusionMatrix(rf.predict, test.df[,2])

#method 2
rf.model2 <- randomForest(TARGET~., train.df[,2:20], ntree=250, nodesize=100)
rf.model2
rf.predict2 <- predict(rf.model2, test.df[,3:20], type="class")
confusionMatrix(rf.predict2, test.df[,2])

#method 3
str(train.df[,2:20])
summary(train.df)
summary(test.df)
glm.model <- glm(formula=TARGET~.,
                        data=train.df[,c(2:5,7:20)], family="binomial") 

#Review diagnostic measures
summary(glm.model)

#Step 3: Calculate prediction accuracy and error rates
response<- ifelse(predict(glm.model, test.df[,c(2:5,7:20)], type = "response")>.5, 2, 1)  # predict distance

actuals_preds <- data.frame(cbind(actuals=test.df$TARGET, predicted=response))  # make actuals_predicteds dataframe.
head(actuals_preds)

# simple correlation between  actuals vs predicted is an accuracy measure. 
# a higher correlation accuracy impliessimilar directional movement
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy


###############################
#model 4 
modelLookup()
require(gbm)
gbm.model <- gbm(
  TARGET~ ., 
  distribution = "adaboost", 
  data = train.df[,c(2:5,7:20)], 
  #var.monotone = NULL,
  n.trees = 200,
  interaction.depth = 4,
  n.minobsinnode = 30,
  shrinkage = 0.05,
  bag.fraction = 0.2,
  train.fraction = 0.8,
  #cv.folds=5,
  #keep.data = TRUE,
  verbose = TRUE)

predict <- predict.gbm (gbm.model, test.df[,c(2:5,7:20)], n.trees = 200, type = "response")
confusionMatrix(predict, test.df[,2])

#################################################
#no preprocessing
application_train <- read.csv("/Users/tian/Desktop/Business Analytics/BA Project 2/application_train_s20.csv"
                              ,header=TRUE, stringsAsFactors=TRUE)
application_train$TARGET <- as.factor(application_train$TARGET)
ifelse(application_train <- sapply(application_train, 
                                   function(x) length(levels(x))) == 1, "DROP", "NODROP")
str(application_train)
set.seed(3)
split <- (.75)
index <- createDataPartition(application_train$TARGET, p=split, list=FALSE)

train.df <- application_train[index,]
test.df <- application_train[-index,]
remove(index)
summary(train.df[,-1])

glm.model <- glm(formula=TARGET~.,
                 data=application_train[,-1], family="binomial") 

###########

