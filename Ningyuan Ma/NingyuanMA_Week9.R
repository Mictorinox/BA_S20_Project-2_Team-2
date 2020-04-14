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
# str(df_train_temp)
# str()
# length(df_train_temp$SK_ID_CURR)
# length(unique(df_train_temp$SK_ID_CURR))

# result_yes <- c()
# result_no <- c()
# 
# for( i in 1:length(df_toScore$SK_ID_CURR))
# {
#   if (df_toScore$SK_ID_CURR[i] %in% df_cardBalance$SK_ID_CURR)
#   {
#     # print(names(df_previousApplication)[i])
#     result_yes <- c(result_yes,df_toScore$SK_ID_CURR[i])
#   }
#   else
#     result_no <- c(result_no,df_toScore$SK_ID_CURR[i])
# }
# 
# length(result_yes)
# length(result_no)


length(unique(df_toScore$SK_ID_CURR))
head(df_train)

df_train_4$SK_ID_CURR
df_train_4_ac <- df_train_4[,-which(colnames(df_train_4) %in% c("SK_ID_CURR","X"))]



names(df_train_4)

str(df_train_4)



# model setup
outcomeName <- 'TARGET'
predictorNames <- names(df_train_4_ac)[names(df_train_4_ac) != outcomeName]

df_train_4_ac$TARGET<-as.factor(df_train_4_ac$TARGET)

set.seed(1234)  # setting seed to reproduce results of random sampling
split<-(.70)
# library (caret)
index <- createDataPartition(df_train_4_ac$TARGET, p=split, list=FALSE) 

df_training_1 <- df_train_4_ac[ index,]  # model training data
df_testing_1<- df_train_4_ac[ -index,]   # test data

table(df_training_1$TARGET)
prop.table(table(df_training_1$TARGET))  #Apps is the minority class at 5.6%
#barplot(prop.table(table(df_training_1$TARGET)))


#install.packages("ROSE")
# library(ROSE)
df_training_1$TARGET<-as.integer(df_training_1$TARGET)
df_training_1.balanced<-ovun.sample(TARGET~., data = df_training_1, p=0.4, N= 20000)$data # this runs!
table(df_training_1.balanced$TARGET)
prop.table(table(df_training_1.balanced$TARGET))

fitControl <- trainControl(method = "none")  

df_training_1$TARGET<-as.integer(df_training_1$TARGET)
df_training_1.balanced<-ovun.sample(TARGET~., data = df_training_1, p=0.4, N= 20000)$data
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
                  tuneGrid = gbm.grid)
#################### output the prediction and see confustion matrix and F-1
gbm.tuned.predict<-predict(gbm3.tuned,df_testing_1[,predictorNames],type="raw")

prediction_test<-predict(gbm3.tuned,df_testing_1[,predictorNames],type="prob")
hist(prediction_test$`1`)
