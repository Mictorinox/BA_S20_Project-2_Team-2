Sys.setlocale("LC_ALL", "English")
library(naniar)
library(dataQualityR)
library(gbm)
library(caret)
library(pROC)
library(ROSE)

# path for t450s
path_train <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
path_toScore <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/applications_to_score_S20.csv"
path_cardBalance <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/credit_card_balance.csv"
path_previousApplication <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/previous_application.csv"
path_train6 <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/application_train_6 (1).csv"
path_balanceClean <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/cc_balance_clean (1).csv"
path_applicationClean <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/pre_application_clean (1).csv"
path_train_4 <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/df_train_4.csv"


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

length(critical_variables)
df_train_complete$TARGET
critical_variables <- c("DAYS_FIRST_DRAWING","Active","DAYS_FIRST_DUE",
                        "high","XNA.x.1","AMT_REQ_CREDIT_BUREAU_WEEK",
                        "Computers","AMT_CREDIT_LIMIT_ACTUAL",
                        "POS.industry.with.interest","MONTHS_BALANCE",
                        "SELLERPLACE_AREA","Regional...Local",
                        "DAYS_REGISTRATION","DAYS_EMPLOYED","AMT_ANNUITY",
                        "AMT_REQ_CREDIT_BUREAU_HOUR",
                        "AMT_PAYMENT_TOTAL_CURRENT","FRIDAY",
                        "DAYS_DECISION","RATE_DOWN_PAYMENT",
                        "Clothing.and.Accessories"
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
df_train_complete_converted_ac <- df_train_complete_converted[,-which(colnames(df_train_complete_converted) %in% c("SK_ID_CURR","X"))]
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
df_training_1.balanced<-ovun.sample(TARGET~., data = df_training_1, p=0.38, N= 1000)$data # this runs!
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
                        n.minobsinnode = 20)

gbm3.tuned<-train(df_training_1.balanced[,predictorNames],df_training_1.balanced[,outcomeName],
                  method='gbm',
                  trControl=fitControl.gbm3,
                  tuneGrid = gbm.grid
                  )

saveRDS(gbm3.tuned,"~/gbm_model8.rds")
model1 <- readRDS("~/gbm_model.rds")

summary(gbm3.tuned)
summary(model1)
#################### output the prediction and see confustion matrix and F-1
gbm.tuned.predict<-predict(gbm3.tuned,df_testing_1[,predictorNames],type="raw")

prediction_test<-predict(gbm3.tuned,df_testing_1[,predictorNames],type="prob")
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

prediction_toScore <- predict(gbm3.tuned,df_toScore_complete[,critical_variables],type="prob")
hist(prediction_toScore$`1`)



