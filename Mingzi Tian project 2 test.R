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
library(MLmetrics)
library(ROSE)
library(pROC)
#######################################


application_train <- read.csv("/Users/tian/Desktop/Business Analytics/BA Project 2/application_train_s20.csv")

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


#########################################################
#factorlize char columns and normalize numeric columns
##########################################################  
application_train_4[,c(2,6:17)] <- lapply(application_train_4[,c(2,6:17)], as.factor)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
application_train_4[,3:5] <- lapply(application_train_4[,3:5], normalize)

lapply(application_train_4[,6:20],table)
str(application_train_4)
summary(application_train_4)


######################################
######################################
set.seed(1)
split <- (.8)
index <- createDataPartition(application_train_4$TARGET, p=split, list=FALSE)

train.df <- application_train_4[index,]
test.df <- application_train_4[-index,]
remove(index)
remove(missing_percentage_df)
remove(correlation_df)
remove(application_train_2)
remove(application_train_3)

####################  balanced the data from 8% to 40% of minority cases 
#install.packages("ROSE")
train.df.balanced<-ovun.sample(TARGET~., data = train.df, p=0.4, N= 40000)$data # this runs!
prop.table(table(train.df.balanced$TARGET))


###########################
# RF Model
###########################
fitControl <- trainControl(method = "none")

start.time <- Sys.time()
rf_1 <-train(train.df.balanced[,3:20],train.df.balanced[,2],
          method='rf',
          trControl=fitControl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# p=40% of rare cases
rf.predict<-predict(rf_1,test.df, type="raw")

#rf.predict
rf.conf <- confusionMatrix(rf.predict,test.df[,2], positive="1")
rf.conf
F1_Score(test.df[,2],rf.predict)

#plot roc line
rf.probs <- predict(rf_1, test.df, type="prob")
rf.plot<-plot(roc(test.df$TARGET,rf.probs[,2]), col="red")
legend("bottomright", legend=c("rf"), col=c("red"), lwd=2)


################################
# RF Model tuned 
################################
modelLookup(model="rf")
fitControl <- trainControl(method="repeatedcv", number=10, repeats=3)
mtry <- sqrt(ncol(train.df.balanced[,3:20]))
tunegrid <- expand.grid(.mtry=mtry)

start.time <- Sys.time()
rf_2 <-train(train.df.balanced[,3:20], 
          train.df.balanced[,2],
          method='rf',
          tuneGrid=tunegrid,
          trControl=fitControl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# p=40% of rare cases
rf.predict<-predict(rf_2, test.df,type="raw")
#rf.predict
rf.conf2 <- confusionMatrix(rf.predict,test.df[,2], positive="1")
rf.conf2
rf.conf
F1_Score(test.df[,2],rf.predict)
rf_2

#plot roc line
rf.probs_1 <- predict(rf_1,test.df,type="prob")
rf.probs_2 <- predict(rf_2,test.df,type="prob")
rf.plot<-plot(roc(test.df$TARGET,rf.probs_1[,2]), col="red")
rf.plot.tuned <-lines(roc(test.df$TARGET,rf.probs_2[,2]), col="orange")
legend("bottomright", legend=c("rf","rf tuned"), col=c("red","orange"), lwd=2)


###########################################
#Advanced GBM Tunning
####################  create pipeline, grid search and model
fitControl.gbm3 <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 5)

gbm.grid <- expand.grid(interaction.depth = c(3,4,5), 
                        n.trees = 5000, 
                        shrinkage = 0.05,
                        n.minobsinnode = 30)

gbm3.tuned<-train(train.df.balanced[,3:20],train.df.balanced[,2],
                  method='gbm',
                  trControl=fitControl.gbm3,
                  tuneGrid = gbm.grid)
#################### output the prediction and see confustion matrix and F-1
gbm.tuned.predict<-predict(gbm3.tuned,test.df[,3:20],type="raw")
#confusionMatrix accuracy
gbm.conf <- confusionMatrix(gbm.tuned.predict,test.df[,2])
#f1
F1_Score(test.df[,2],gbm.tuned.predict)

library(pROC)
rf.probs <- predict(rf,test.df[,3:20],type="prob")
gbm.probs <- predict(gbm3.tuned,test.df[,3:20],type="prob")    
 
gbm.plot<-plot(roc(test.df$TARGET,gbm.probs[,2]))
rf.plot<-lines(roc(test.df$TARGET,rf.probs[,2]), col="green")
legend("bottomright", legend=c("rf", "gbm"), col=c("blue", "black"), lwd=2)
                              

###########################################
#GBM Tuned 2 (modification of parameters)
####################                           
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

                              
########################################################
########################################################
#credit card balance merging
########################################################
########################################################

cc_balance <- read.csv("credit_card_balance.csv",header=TRUE, stringsAsFactors=FALSE)
str(cc_balance)

#check if any SK_ID_PREV is in SK_ID_CURR
ifelse(any(cc_balance$SK_ID_PREV == cc_balance$SK_ID_CURR), "TRUE","FALSE")

#To process numeric data, using mean value for variables for different id
cc_num <- cc_balance[,c(-1,-21)] %>% group_by(SK_ID_CURR) %>% summarise_all(funs(mean))
cor_coefficient <- as.data.frame(cor(cc_num[sapply(cc_num, is.numeric)], use="complete.obs"))
cor_index <- findCorrelation(cor(cor_coefficient),cutoff=0.9)
cor_index
cc_num<-subset(cc_num, select=-c(AMT_DRAWINGS_CURRENT, AMT_INST_MIN_REGULARITY,
                                 AMT_TOTAL_RECEIVABLE,AMT_RECIVABLE, AMT_RECEIVABLE_PRINCIPAL,
                                 AMT_PAYMENT_CURRENT))


#To process char data, count frequency
cc_char <- as.data.frame.matrix(table(cc_balance$SK_ID_CURR,cc_balance$NAME_CONTRACT_STATUS))
cc_char <- data.frame(row.names(cc_char), cc_char, row.names=NULL)
names(cc_char)[1]<- "SK_ID_CURR"

cc_balance_clean <- merge(cc_num, cc_char, all=TRUE, by="SK_ID_CURR")

#merge sets, append to application_train_4, from column 21
application_train_5 <- merge(application_train_4, cc_balance_clean,
                             all.x=TRUE, by="SK_ID_CURR")
str(application_train_5)

########################################################
########################################################
# Previous application merging
########################################################
########################################################

pre_application <- read.csv("/Users/tian/Desktop/Business Analytics/BA Project 2/previous_application.csv",
                            header=TRUE, stringsAsFactors=FALSE)
str(pre_application)

#check any other repetitive variables (only SK_ID_CURR)
colnames(pre_application)[which(colnames(pre_application) %in% colnames(application_train))]

#delete column with higher than 0.8 missing percentage
missing_percentage <- apply(pre_application, 2, function(col)sum(is.na(col))/length(col))
missing_percentage_df<-as.data.frame(missing_percentage[missing_percentage>0.8])
index<-rownames(missing_percentage_df)
pre_application<- pre_application[,!(names(pre_application) %in% index)]

for(i in 3:ncol(pre_application)){
    if(class(pre_application[,i]) == "character"){
      pre_application[,i] <- as.factor(pre_application[,i])}
      else {pre_application[,i] <- as.numeric(pre_application[,i])}
}

cor_coefficient <- as.data.frame(cor(pre_application[sapply(pre_application, is.numeric)], use="complete.obs"))
cor_index <- findCorrelation(cor(cor_coefficient),cutoff=0.9)
cor_index

#cut off variables with correlation > 0.9, delete the one with fewer NA values
sum(is.na(pre_application$AMT_APPLICATION))
sum(is.na(pre_application$AMT_CREDIT))
sum(is.na(pre_application$AMT_GOODS_PRICE))
pre_application_clean<-subset(pre_application, select=-AMT_GOODS_PRICE) #remove it
sum(is.na(pre_application$DAYS_TERMINATION))
sum(is.na(pre_application$DAYS_LAST_DUE))
pre_application_clean<-subset(pre_application_clean, select=-DAYS_TERMINATION) #remove it

str(pre_application_clean)
summary(pre_application_clean)

pre_factor <- sapply(pre_application_clean, is.factor)
pre_factor <- cbind("SK_ID_CURR"=pre_application_clean[,2], pre_application_clean[,pre_factor])
str(pre_factor)

dfList <- list()
for(i in 2:ncol(pre_factor)){
  x <- as.data.frame.matrix(table(pre_factor[,1], pre_factor[,i]))
  dfList[[i]] <- x
}
rm(x)

pre_count <- dfList[[2]]
for(j in 3:length(dfList)){
  pre_count <- transform(merge(pre_count, dfList[[j]], all.x=TRUE, by=0), 
                         row.names=Row.names, Row.names=NULL)
}

pre_count <- data.frame("SK_ID_CURR"=row.names(pre_count), pre_count, row.names=NULL)

pre_num <- pre_application[,sapply(pre_application, is.numeric)][,-1]
pre_num <- pre_num %>% group_by(SK_ID_CURR) %>% summarise_all(list(mean))

pre_application_clean <- merge(pre_num, pre_count, all=TRUE, by="SK_ID_CURR")

#Merge application_train_5 and previous application dataset
application_train_6 <- merge(application_train_5, pre_application_clean, all.x=TRUE, by="SK_ID_CURR")

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Removes all outliers from a data set
remove_all_outliers <- function(df){
  # We only want the numeric columns
  df[,sapply(df, is.numeric)] <- lapply(df[,sapply(df, is.numeric)], remove_outliers)
  df
}
application_train_6[,1] <- as.factor(application_train_6[,1])
application_train_6 <- remove_all_outliers(application_train_6)

#save application train 6
write.csv(application_train_6, file = "application_train_6.csv", quote = FALSE, row.names = FALSE)
