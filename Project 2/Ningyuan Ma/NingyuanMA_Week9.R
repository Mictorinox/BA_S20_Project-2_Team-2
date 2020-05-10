Sys.setlocale("LC_ALL", "English")
library(naniar)
library(dataQualityR)
library(gbm)
library(caret)
library(pROC)
library(ROSE)
library(MLmetrics)


# path for t450s
path_train <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
path_toScore <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/applications_to_score_S20.csv"
path_cardBalance <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/credit_card_balance.csv"
path_previousApplication <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/previous_application.csv"
path_train6 <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/application_train_6 (1).csv"
path_balanceClean <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/cc_balance_clean (1).csv"
path_applicationClean <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/pre_application_clean (1).csv"
path_train_4 <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/df_train_4.csv"
path_toScore_complete <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/toScoreMerged(192variables).csv"
path_train_complete_converted <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/TrainMerged(192variables).csv"
path_output <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/output.csv"



# path for x1
path_train <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
path_toScore <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/applications_to_score_S20.csv"
path_cardBalance <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/credit_card_balance.csv"
path_previousApplication <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/previous_application.csv"
path_train6 <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/application_train_6 (1).csv"
path_balanceClean <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/cc_balance_clean (1).csv"
path_applicationClean <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/pre_application_clean (1).csv"
path_train_4 <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/df_train_4.csv"
path_train_selected <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/df_train_selected.csv"
path_toScore_selected <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/df_train_selected.csv"
 
df_train <- read.csv(path_train)
df_toScore <- read.csv(path_toScore)
df_cardBalance <- read.csv(path_cardBalance)
df_previousApplication <- read.csv(path_previousApplication)


result_yes <- c()
result_no <- c()
for (i in 1:nrow(df_train)){
  if (df_train$SK_ID_CURR[i] %in% df_previousApplication$SK_ID_CURR)
  {
    result_yes <- c(result_yes,df_toScore$SK_ID_CURR[i])
  }
  else
  {
    result_no <- c(result_no,df_toScore$SK_ID_CURR[i])
  }
}
length(df_toScore$SK_ID_CURR)
length(result_yes)
length(result_no)



# read new dataset
df_train6 <- read.csv(path_train6)
df_train_4 <- read.csv(path_train_4)
df_balanceClean <- read.csv(path_balanceClean)
df_applicationClean <- read.csv(path_applicationClean)

df_train_temp <- merge(df_train_4,df_balanceClean,all.x=T,by = "SK_ID_CURR")
df_train_complete <- merge(df_train_temp,df_applicationClean,all.x=T,by = "SK_ID_CURR")
# 


df_train_complete$AMT_REQ_CREDIT_BUREAU_HOUR<-as.numeric(df_train_complete$AMT_REQ_CREDIT_BUREAU_HOUR.new)
df_train_complete$AMT_REQ_CREDIT_BUREAU_DAY<-as.numeric(df_train_complete$AMT_REQ_CREDIT_BUREAU_DAY.new)
df_train_complete$AMT_REQ_CREDIT_BUREAU_WEEK<-as.numeric(df_train_complete$AMT_REQ_CREDIT_BUREAU_WEEK.new)
to.remove <-c("AMT_REQ_CREDIT_BUREAU_HOUR.new","AMT_REQ_CREDIT_BUREAU_DAY.new","AMT_REQ_CREDIT_BUREAU_WEEK.new")
df_train_complete_converted <- df_train_complete[,-which(names(df_train_complete) %in% to.remove)]

df_train_complete_converted <- read.csv(path_train_complete_converted)
write.csv(df_train_complete_converted,"~/TrainMerged(192variables).csv")

length(critical_variables)
df_train_complete$TARGET
critical_variables <- c("SK_ID_CURR","TARGET","FLAG_MOBIL",
                      "FLAG_CONT_MOBILE","FLAG_EMAIL","LIVE_REGION_NOT_WORK_REGION",
                      "FLAG_DOCUMENT_4","FLAG_DOCUMENT_5","FLAG_DOCUMENT_7",
                      "FLAG_DOCUMENT_10","FLAG_DOCUMENT_12","FLAG_DOCUMENT_17",
                      "FLAG_DOCUMENT_19","FLAG_DOCUMENT_20","SK_ID_CURR.1",
                      "CNT_CHILDREN","AMT_INCOME_TOTAL","AMT_CREDIT.x",
                      "AMT_ANNUITY.x","REGION_POPULATION_RELATIVE","DAYS_BIRTH",
                      "DAYS_EMPLOYED","DAYS_REGISTRATION","DAYS_ID_PUBLISH",
                      "CNT_FAM_MEMBERS","REGION_RATING_CLIENT","REGION_RATING_CLIENT_W_CITY",
                      "HOUR_APPR_PROCESS_START.x","EXT_SOURCE_2","EXT_SOURCE_3",
                      "OBS_30_CNT_SOCIAL_CIRCLE","DEF_30_CNT_SOCIAL_CIRCLE","OBS_60_CNT_SOCIAL_CIRCLE",
                      "DEF_60_CNT_SOCIAL_CIRCLE","DAYS_LAST_PHONE_CHANGE","AMT_REQ_CREDIT_BUREAU_HOUR",
                      "AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK","AMT_REQ_CREDIT_BUREAU_MON",
                      "AMT_REQ_CREDIT_BUREAU_QRT","SK_ID_PREV","MONTHS_BALANCE",
                      "AMT_BALANCE","AMT_CREDIT_LIMIT_ACTUAL","AMT_DRAWINGS_CURRENT",
                      "AMT_PAYMENT_TOTAL_CURRENT","AMT_RECEIVABLE_PRINCIPAL","AMT_RECIVABLE",
                      "AMT_TOTAL_RECEIVABLE","CNT_DRAWINGS_CURRENT","SK_DPD",
                      "SK_DPD_DEF","Active","Approved",
                      "Completed","Demand","Refused",
                      "Sent.proposal","Signed","Cash.loans",
                      "Consumer.loans","Revolving.loans","XNA",
                      "FRIDAY","MONDAY","SATURDAY",
                      "SUNDAY","THURSDAY","TUESDAY",
                      "WEDNESDAY","V1","Children",
                      "Family","Group.of.people","Other_A",
                      "Other_B","Spouse..partner","Unaccompanied",
                      "AMT_ANNUITY","AMT_CREDIT","HOUR_APPR_PROCESS_START"
                      )
#                         ,
#                         "AMT_BALANCE","Country.wide",
#                         "AMT_DRAWINGS_POS_CURRENT",
#                         "CNT_INSTALMENT_MATURE_CUM",
#                         "DAYS_REGISTRATION","RATE_DOWN_PAYMENT",
#                         "Audio.Video","AMT_APPLICATION")

df_train_selected<-df_train_complete[,c("TARGET",critical_variables)]
df_train_selected_converted<-df_train_complete_converted[,c("TARGET",critical_variables)]
str(df_train_selected_converted)

str(df_train_selected)
write.csv(df_train_selected,path_train_selected)

df_toScore_temp <- merge(df_toScore,df_balanceClean,all.x=T,by = "SK_ID_CURR")
df_toScore_complete <- merge(df_toScore_temp,df_applicationClean[,-which(colnames(df_applicationClean) %in% c("AMT_ANNUITY"))],all.x=T,by = "SK_ID_CURR")
summary(df_toScore_complete$"AMT_ANNUITY")
summary(df_applicationClean$"AMT_ANNUITY")

df_toScore_complete$
class(df_train_complete$AMT_REQ_CREDIT_BUREAU_WEEK.new)
class(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK)

df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR<-as.character(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR)
df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR.new<-ifelse(  is.na(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR),"NULL",df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR)
df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR.new<-as.factor(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR.new)
# factor(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR.new, levels = levels(df_train_complete$AMT_REQ_CREDIT_BUREAU_HOUR.new))
# table(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_HOUR.new)

df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY<-as.character(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY)
df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY.new<-ifelse(  is.na(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY),"NULL",df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY)
df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY.new<-as.factor(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY.new)
# factor(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY.new, levels = levels(df_train_complete$AMT_REQ_CREDIT_BUREAU_DAY.new))
# table(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_DAY.new)

df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK<-as.character(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK)
df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK.new<-ifelse(  is.na(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK),"NULL",df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK)
df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK.new<-as.factor(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK.new)
# factor(df_toScore_complete$AMT_REQ_CREDIT_BUREAU_WEEK.new, levels = levels(df_train_complete$AMT_REQ_CREDIT_BUREAU_WEEK.new))

df_toScore_complete <- read.csv(path_toScore_complete)
write.csv(df_toScore_complete,"~/toScore(merged).csv")

df_toScore_selected<-df_toScore_complete[,critical_variables]
write.csv(df_toScore_selected,path_toScore_selected)

length(critical_variables)
for(i in 1:length(critical_variables)){
  if (critical_variables[i] %in% names(df_toScore_complete)){
    
  }
  else{
    print(critical_variables[i])
  }
  
}

length(unique(df_toScore$SK_ID_CURR))
head(df_train)

names(df_train_complete_converted)
df_train_complete_converted_ac <- df_train_complete_converted[,-which(colnames(df_train_complete_converted) %in% c("SK_ID_CURR","X","X.1"))]
str(df_train_complete_converted_ac)


# model setup
outcomeName <- 'TARGET'
predictorNames <- names(df_train_complete_converted_ac)[names(df_train_complete_converted_ac) != outcomeName]

df_train_complete_converted_ac$TARGET<-as.factor(df_train_complete_converted_ac$TARGET)

set.seed(1234)  # setting seed to reproduce results of random sampling
split<-(.70)
# library (caret)
index <- createDataPartition(df_train_complete_converted_ac$TARGET, p=split, list=FALSE) 

df_training_1 <- df_train_complete_converted_ac[ index,]  # model training data
df_testing_1<- df_train_complete_converted_ac[ -index,]   # test data

table(df_training_1$TARGET)
prop.table(table(df_training_1$TARGET))  #Apps is the minority class at 5.6%
#barplot(prop.table(table(df_training_1$TARGET)))


#install.packages("ROSE")
# library(ROSE)
df_training_1$TARGET<-as.integer(df_training_1$TARGET)
df_training_1.balanced<-ovun.sample(TARGET~., data = df_training_1, p=0.40, N= 1000)$data # 20000 runs rather smooth
table(df_training_1.balanced$TARGET)
prop.table(table(df_training_1.balanced$TARGET))


####################  create pipeline, grid search and model
df_training_1.balanced$TARGET<-as.factor(df_training_1.balanced$TARGET)

fitControl.gbm3 <- trainControl(method = "repeatedcv",
                                number = 20,
                                repeats = 5)

gbm.grid <- expand.grid(interaction.depth = c(3,4,5), 
                        n.trees = (1:10)*10, 
                        shrinkage = 0.1,
                        n.minobsinnode = 10)

gbm3.tuned<-train(df_training_1.balanced[,predictorNames],df_training_1.balanced[,outcomeName],
                  method='gbm',
                  trControl=fitControl.gbm3,
                  tuneGrid = gbm.grid
                  )

saveRDS(gbm3.tuned,"~/gbm_model12.rds")

path_model <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/4-Models/gbm_model13.rds"
gbm3.tuned <- readRDS(path_model)
gbm_model3 <- readRDS(path_model)
summary(gbm_model3)

summary(gbm3.tuned)

#################### output the prediction and see confustion matrix and F-1
gbm.tuned.predict<-predict(gbm3.tuned,df_testing_1[,predictorNames],type="raw")

prediction_test<-predict(gbm3.tuned,df_testing_1[,predictorNames],type="prob")
hist(prediction_test$`1`)


gbm.tuned.predict<-as.factor(gbm.tuned.predict)
gbm.tuned.predict<-ifelse(gbm.tuned.predict==2,1,0)
gbm.tuned.predict<-as.factor(gbm.tuned.predict)
#confusionMatrix accuracy
confusionMatrix(gbm.tuned.predict,df_testing_1[,outcomeName])
#f1
F1_Score(df_testing_1[,outcomeName],gbm.tuned.predict)
#roc-auc
library(pROC)
gbm.tuned.probs <- predict(gbm3.tuned,df_testing_1[,predictorNames],type="prob")    
auc(df_testing_1[,outcomeName],gbm.tuned.probs[,2])

5865/(69451+5865)

#################### see the grid search performance 
library(ggplot2)
ggplot(gbm3.tuned)

prediction_toScore <- predict(gbm3.tuned,df_toScore_complete,type="prob")
prediction_toScore <- predict(gbm3.tuned,df_toScore_complete,type="raw")
prediction_toScore<-as.factor(prediction_toScore)
prediction_toScore<-ifelse(prediction_toScore==2,1,0)
prediction_toScore<-as.factor(prediction_toScore)
str(prediction_toScore)


prediction_output <- data.frame("SK_ID_CURR"=df_toScore_complete$SK_ID_CURR,"TARGET"=prediction_toScore)
write.csv(prediction_output,path_output)
write.csv(prediction_toScore$`2`,path_output)
str(prediction_output)
