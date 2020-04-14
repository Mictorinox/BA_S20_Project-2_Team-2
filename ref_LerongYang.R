# path for t450s
path <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
# path for x1
path <-"C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"

# path for t450s
path_train <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
path_toScore <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/applications_to_score_S20.csv"
path_cardBalance <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/credit_card_balance.csv"
path_previousApplication <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/previous_application.csv"



application_train <- read.csv(path_train)
head(application_train)

library(naniar)
# vis_miss(application_train, warn_large_data=FALSE)

missing_percentage<-apply(application_train, 2, function(col)sum(is.na(col))/length(col))
missing_percentage[missing_percentage>0.2]
missing_percentage_df<-as.data.frame(missing_percentage[missing_percentage>0.2])
index<-rownames(missing_percentage_df)
application_train_2<- application_train[,!(names(application_train) %in% index)]

correlation_percentage<-apply(application_train_2, 2, function(col)chisq.test(application_train_2$TARGET,col)$p.value)
# correlation_percentage

correlation_percentage[correlation_percentage>0.05]
correlation_df<-as.data.frame(correlation_percentage[correlation_percentage>0.05])
index<-rownames(correlation_df)
index_2<-c(index,"TARGET")
application_train_3<- application_train_2[,(names(application_train_2) %in% index_2)]
# colnames(application_train_3)

application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR<-as.character(application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR)
application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new<-ifelse(  is.na(application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR),"NULL",application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR)
application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new<-as.factor(application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new)
# table(application_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new)

application_train_3$AMT_REQ_CREDIT_BUREAU_DAY<-as.character(application_train_3$AMT_REQ_CREDIT_BUREAU_DAY)
application_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new<-ifelse(  is.na(application_train_3$AMT_REQ_CREDIT_BUREAU_DAY),"NULL",application_train_3$AMT_REQ_CREDIT_BUREAU_DAY)
application_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new<-as.factor(application_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new)
# table(application_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new)

application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK<-as.character(application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK)
application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new<-ifelse(  is.na(application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK),"NULL",application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK)
application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new<-as.factor(application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new)
# table(application_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new)


library(caret)
library(RColorBrewer)

to.remove <-c("AMT_REQ_CREDIT_BUREAU_HOUR","AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK")
application_train_4 <- application_train_3[,-which(names(application_train_3) %in% to.remove)]
application_train_4  <- na.omit(application_train_4) 
nrow(application_train_4)
### Selecting  variables for modeling.  A total of 12 variables with name updates
library(dplyr)
#names(application_train_4)
application_train_4.df <- select(application_train_4,
                                 "TARGET",
                                 "AMT_INCOME_TOTAL",
                                 "DAYS_EMPLOYED",
                                 "DAYS_REGISTRATION",
                                 "FLAG_MOBIL",
                                 "FLAG_CONT_MOBILE",
                                 "FLAG_EMAIL",
                                 "LIVE_REGION_NOT_WORK_REGION",
                                 "FLAG_DOCUMENT_4",
                                 "FLAG_DOCUMENT_5",
                                 "FLAG_DOCUMENT_7",
                                 "FLAG_DOCUMENT_10",
                                 "FLAG_DOCUMENT_12",
                                 "FLAG_DOCUMENT_17",
                                 "FLAG_DOCUMENT_19",
                                 "FLAG_DOCUMENT_20",
                                 "AMT_REQ_CREDIT_BUREAU_HOUR.new",
                                 "AMT_REQ_CREDIT_BUREAU_DAY.new",
                                 "AMT_REQ_CREDIT_BUREAU_WEEK.new"
)

# model setup
outcomeName <- 'TARGET'
predictorNames <- names(application_train_4.df)[names(application_train_4.df) != outcomeName]

application_train_4.df$TARGET<-as.factor(application_train_4.df$TARGET)

set.seed(1234)  # setting seed to reproduce results of random sampling
split<-(.70)
library (caret)
index <- createDataPartition(application_train_4.df$TARGET, p=split, list=FALSE) 

train.df <- application_train_4.df[ index,]  # model training data
test.df<- application_train_4.df[ -index,]   # test data

table(train.df$TARGET)
prop.table(table(train.df$TARGET))  #Apps is the minority class at 5.6%
#barplot(prop.table(table(train.df$TARGET)))


#install.packages("ROSE")
library(ROSE)
train.df$TARGET<-as.integer(train.df$TARGET)
train.df.balanced<-ovun.sample(TARGET~., data = train.df, p=0.4, N= 20000)$data # this runs!
table(train.df.balanced$TARGET)
prop.table(table(train.df.balanced$TARGET))
----------------------------------------------------------------------------------------------------------------
str(application_train_4)  
application_train_4[,c(2,6:17)] <- lapply(application_train_4[,c(2,6:17)], as.factor)

str(application_train_4)
summary(application_train_4)
remove(correlation_df)
remove(missing_percentage_df)
remove(application_train_2)

###############################################
#define a removing outliers function for further merge
###############################################

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

cc_balance <- read.csv(path_cardBalance,header=TRUE, stringsAsFactors=FALSE)
cc_balance[,-1:-2] <- remove_all_outliers(cc_balance[,-1:-2])
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
summary(cc_balance_clean)

#merge sets, append to application_train_4, from column 21
application_train_5 <- merge(application_train, cc_balance_clean,
                             all.x=TRUE, by="SK_ID_CURR")

str(application_train_5)

########################################################
########################################################
# Previous application merging
########################################################
########################################################

pre_application <- read.csv(path_previousApplication,
                            header=TRUE, stringsAsFactors=FALSE)
pre_application[,-1:-2] <- remove_all_outliers(pre_application[,-1:-2])                             
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
pre_count <- as.factor(pre_count)

pre_num <- pre_application[,sapply(pre_application, is.numeric)][,-1]
pre_num <- pre_num %>% group_by(SK_ID_CURR) %>% summarise_all(list(mean))
str(pre_num)
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

application_train_6[,-1:-2] <- remove_all_outliers(application_train_6[,-1:-2])
summary_6 <- as.data.frame.table(summary(application_train_6))
table(application_train_6$TARGET)
str(application_train_6)

#save application train 6
write.csv(application_train_6, file = "application_train_6.csv", quote = FALSE, row.names = FALSE)
write.csv(pre_application_clean, file = "pre_application_clean.csv", quote = FALSE, row.names = FALSE)
write.csv(cc_balance_clean, file = "cc_balance_clean.csv", quote = FALSE, row.names = FALSE)

str(application_train_6)


----------------------------------------------------------------------------------------------------------------

fitControl <- trainControl(method = "none")  

# RF Model
train.df.balanced$TARGET<-as.factor(train.df.balanced$TARGET)
rf<-train(train.df.balanced[,predictorNames],train.df.balanced[,outcomeName],
          method='rf',
          trControl=fitControl)
# p=40% of rare cases
rf.predict<-predict(rf,test.df[,predictorNames],type="raw")
#rf.predict
rf.predict<-as.factor(rf.predict)
rf.predict<-ifelse(rf.predict==2,1,0)
rf.predict<-as.factor(rf.predict)
confusionMatrix(rf.predict,test.df[,outcomeName])

library(MLmetrics)
F1_Score(test.df[,outcomeName],rf.predict)

#install.packages("ROSE")
library(ROSE)
train.df$TARGET<-as.integer(train.df$TARGET)
train.df.balanced<-ovun.sample(TARGET~., data = train.df, p=0.4, N= 20000)$data # this runs!
table(train.df.balanced$TARGET)
prop.table(table(train.df.balanced$TARGET))


# Model_2：GBM model# p=40% of rare cases
rf.predict<-predict(rf,test.df[,predictorNames],type="raw")
#rf.predict
rf.predict<-as.factor(rf.predict)
rf.predict<-ifelse(rf.predict==2,1,0)
rf.predict<-as.factor(rf.predict)
confusionMatrix(rf.predict,test.df[,outcomeName])

F1_Score(test.df[,outcomeName],rf.predict)


# Advanced GBM Tunning

####################  balanced the data from 8% to 40% of rare cases 
library(ROSE)
train.df$TARGET<-as.integer(train.df$TARGET)
train.df.balanced<-ovun.sample(TARGET~., data = train.df, p=0.4, N= 20000)$data
table(train.df.balanced$TARGET)
prop.table(table(train.df.balanced$TARGET))

####################  create pipeline, grid search and model
train.df.balanced$TARGET<-as.factor(train.df.balanced$TARGET)

fitControl.gbm3 <- trainControl(method = "repeatedcv",
                                number = 20,
                                repeats = 5)

gbm.grid <- expand.grid(interaction.depth = c(3,4,5), 
                        n.trees = (1:10)*10, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbm3.tuned<-train(train.df.balanced[,predictorNames],train.df.balanced[,outcomeName],
                  method='gbm',
                  trControl=fitControl.gbm3,
                  tuneGrid = gbm.grid)
#################### output the prediction and see confustion matrix and F-1
gbm.tuned.predict<-predict(gbm3.tuned,test.df[,predictorNames],type="raw")

prediction_test<-predict(gbm3.tuned,test.df[,predictorNames],type="prob")
hist(prediction_test$`1`)

gbm.tuned.predict<-as.factor(gbm.tuned.predict)
gbm.tuned.predict<-ifelse(gbm.tuned.predict==2,1,0)
gbm.tuned.predict<-as.factor(gbm.tuned.predict)
#confusionMatrix accuracy
confusionMatrix(gbm.tuned.predict,test.df[,outcomeName])
#f1
F1_Score(test.df[,outcomeName],gbm.tuned.predict)
#roc-auc
library(pROC)
gbm.tuned.probs <- predict(gbm3.tuned,test.df[,predictorNames],type="prob")    
auc(test.df[,outcomeName],gbm.tuned.probs[,2])

#################### see the grid search performance 
library(ggplot2)
ggplot(gbm3.tuned)

