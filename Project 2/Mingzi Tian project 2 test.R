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
library(DMwR)
#######################################
application_train <- read.csv("/Users/tian/Desktop/Business Analytics/BA Project 2/application_train_S20.csv")

missing_percentage<-apply(application_train, 2, function(col)sum(is.na(col))/length(col))
missing_percentage
missing_percentage[missing_percentage>0.2]
missing_percentage_df<-as.data.frame(missing_percentage[missing_percentage>0.2])
index<-rownames(missing_percentage_df)
application_train_2<- application_train[,!(names(application_train) %in% index)]
application_train_2[,c(2,22:27,34:39,52:71)]<- lapply(application_train_2[,c(2,22:27,34:39,52:71)], as.factor)
str(application_train_2)
correlation_percentage<-apply(application_train_2[,sapply(application_train_2, is.factor)], 2, 
                              function(col)chisq.test(application_train_2$TARGET,col,simulate.p.value=TRUE)$p.value)

correlation_percentage[correlation_percentage>0.05]
correlation_df<-as.data.frame(correlation_percentage[correlation_percentage>0.05])
index<-rownames(correlation_df)
index_2<-c(index,"TARGET")

cor_coefficient <- as.data.frame(cor(application_train_2[sapply(application_train_2, is.numeric)], use="complete.obs"))
correlation_index <- findCorrelation(cor(cor_coefficient),cutoff=0.9)
correlation_index
sum(is.na(application_train_2$AMT_CREDIT)) > sum(is.na(application_train_2$AMT_GOODS_PRICE)) #FALSE, so remove right one
application_train_2<-subset(application_train_2, select=-AMT_GOODS_PRICE)


application_train_3<- cbind(application_train_2[,(names(application_train_2) %in% index_2)],
                            application_train_2[,sapply(application_train_2, is.numeric)])
str(application_train_3)

columnNumber <- which(colnames(application_train_3)=="SK_ID_CURR")
application_train_3 <- application_train_3[,c(columnNumber,1:ncol(application_train_3)-1)]
#Using knn imputation to imputate data instead of deleting (time consuming)
#application_train_4  <- knnImputation(application_train_3, k=5, meth="weighAvg")
#Using median imputation instead
Median_Imp <- function(data=data){
  for(i in 1:ncol(data)){        
    if(class(data[,i]) %in% c("numeric","integer")){
      if(sum(is.na(data[,i]))){
        data[is.na(data[,i]),i] <- 
          median(data[,i],na.rm = TRUE)
      }
    }
  }
  return(data)
}
application_train_4 <- Median_Imp(application_train_3)
summary(application_train_4)
table(application_train_3$TARGET)

rm(correlation_df, missing_percentage_df, application_train_2, index, index_2)



###############################################
#define a removing outlier function
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
cc_num <- cc_balance[,c(-21)] %>% group_by(SK_ID_CURR) %>% summarise_all(list(mean))
cc_num[,-1] <- remove_all_outliers(cc_num[,-1])
cor_coefficient <- as.data.frame(cor(cc_num[sapply(cc_num, is.numeric)], use="complete.obs"))
cor_index <- findCorrelation(cor(cor_coefficient),cutoff=0.9)
cor_index
cc_num<-subset(cc_num, select=-c(AMT_DRAWINGS_OTHER_CURRENT))

#To process char data, count frequency
cc_char <- as.data.frame.matrix(table(cc_balance$SK_ID_CURR,cc_balance$NAME_CONTRACT_STATUS))
cc_char <- data.frame(row.names(cc_char), cc_char, row.names=NULL)

names(cc_char)[1]<- "SK_ID_CURR"

cc_balance_clean <- merge(cc_num, cc_char, all=TRUE, by="SK_ID_CURR")
summary(cc_balance_clean)

missing_percentage<-apply(cc_balance_clean, 2, function(col)sum(is.na(col))/length(col))
missing_percentage
missing_percentage[missing_percentage>0.2]
missing_percentage_df<-as.data.frame(missing_percentage[missing_percentage>0.2])
index<-rownames(missing_percentage_df)
cc_balance_clean<- cc_balance_clean[,!(names(cc_balance_clean) %in% index)]
str(cc_balance_clean)

########################################################
########################################################
# Previous application merging
########################################################
########################################################

pre_application <- read.csv("/Users/tian/Desktop/Business Analytics/BA Project 2/previous_application.csv",
                            header=TRUE, stringsAsFactors=FALSE)
application_to_score <- read.csv("/Users/tian/Desktop/Business Analytics/BA Project 2/applications_to_score_S20.csv",
                            header=TRUE, stringsAsFactors=FALSE)
str(pre_application)

table(pre_application$NAME_CONTRACT_STATUS)
pre_var <- pre_application[pre_application$NAME_CONTRACT_STATUS == "Approved" | pre_application$NAME_CONTRACT_STATUS == "Refused",]
pre_var <- cbind("SK_ID_PREV"=pre_var[,1],pre_var[,colnames(pre_var) %in% colnames(application_to_score)])
str(pre_var)
pre_var <- droplevels(pre_var)


cor_coefficient <- as.data.frame(cor(pre_var[sapply(pre_var, is.numeric)], use="complete.obs"))
cor_index <- findCorrelation(cor(cor_coefficient),cutoff=0.9)
cor_index
sum(is.na(pre_var$AMT_GOODS_PRICE)) > sum(is.na(pre_var$AMT_CREDIT)) #TRUE,delete the left column
pre_var<-subset(pre_var, select=-AMT_GOODS_PRICE)

for(i in 3:ncol(pre_var)){
    if(class(pre_var[,i]) == "character"){
      pre_var[,i] <- as.factor(pre_var[,i])}
      else {pre_var[,i] <- as.numeric(pre_var[,i])}
}

pre_num <- pre_var[,sapply(pre_var, is.numeric)]
pre_num <- pre_num[,-1] %>% group_by(SK_ID_CURR) %>% summarise_all(list(mean))
str(pre_num)

pre_factor <- cbind("SK_ID_CURR"=pre_var[,2],pre_var[,sapply(pre_var, is.factor)])

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
str(pre_count)
pre_application_clean <- merge(pre_count, pre_num, all=TRUE, by="SK_ID_CURR")


############################################################
#Final merge, Merge application_train_5 and previous application dataset
application_train_5 <- merge(application_train_4, cc_balance_clean,
                             all.x=TRUE, by="SK_ID_CURR")
application_train_6 <- merge(application_train_5, pre_application_clean, 
                             all.x=TRUE, 
                             by="SK_ID_CURR",
                             row.names=NULL)
table(application_train_6$TARGET)

str(application_train_6)
summary(application_train_6)

#Optional
#for(i in 2:ncol(application_train_6)){
#  if(class(application_train_6[,i]) == "integer"){
#    application_train_6[,i] <- as.factor(application_train_6[,i])}
#  else {application_train_6[,i] <- as.numeric(application_train_6[,i])}
#}

#to use complete cases in application_train_6 to build model
#test<- application_train_6[!complete.cases(application_train_6),]

#save application train 6
write.csv(application_train_6, file = "application_train_6_update.csv", quote = FALSE, row.names = FALSE)
write.csv(pre_application_clean, file = "pre_application_clean_update.csv", quote = FALSE, row.names = FALSE)
write.csv(cc_balance_clean, file = "cc_balance_clean.csv_update", quote = FALSE, row.names = FALSE)


######################################
######################################

set.seed(1)
split <- (.7)
index <- createDataPartition(test$TARGET, p=split, list=FALSE)

train.df <- test[index,]
test.df <- test[-index,]

train.df2 <- application_train[index,]
test.df2 <- application_train[-index,]

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

remove(index)
remove(missing_percentage_df)
remove(correlation_df)
remove(application_train_2)
remove(application_train_3)

####################  balanced the data from 8% to 40% of minority cases 
#install.packages("ROSE")
train.df.balanced<-ovun.sample(TARGET~., data = train.df, p=0.40, N= 50000)$data # this runs!
prop.table(table(train.df.balanced$TARGET))

###########################
# RF Model
###########################
fitControl <- trainControl(method = "none")
str(train.df)

start.time <- Sys.time()
rf_1 <-train(train.df.balanced[,-1:-2],train.df.balanced[,2],
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
fitControl <- trainControl(method="repeatedcv", number=5, repeats=3)
mtry <- sqrt(ncol(train.df.balanced[,-1:-2]))
tunegrid <- expand.grid(.mtry=mtry)

start.time <- Sys.time()
rf_2 <-train(train.df.balanced[,-1:-2], 
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
F1_Score(test.df[,2],rf.predict)

#plot roc line
rf.probs_1 <- predict(rf_1,test.df,type="prob")
rf.probs_2 <- predict(rf_2,test.df,type="prob")
rf.plot<-plot(roc(test.df$TARGET,rf.probs_1[,2]), col="red")
rf.plot.tuned <-lines(roc(test.df$TARGET,rf.probs_2[,2]), col="orange")
legend("bottomright", legend=c("rf","rf tuned"), col=c("red","orange"), lwd=2)


###########################################
#Advanced GBM Tunning
####################  create pipeline, grid search and model
fitControl.gbm <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 3)

gbm.grid <- expand.grid(interaction.depth = c(3,4,5), 
                        n.trees = 5000, 
                        shrinkage = 0.01,
                        n.minobsinnode = 20)

gbm.tuned<-train(train.df.balanced[,-1:-2],train.df.balanced[,2],
                  method='gbm',
                  trControl=fitControl.gbm,
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
