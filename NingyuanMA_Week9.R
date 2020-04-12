Sys.setlocale("LC_ALL", "English")
library(naniar)
library(dataQualityR)
library(gbm)
library(caret)
library(pROC)
library(ROSE)

path_train <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
path_toScore <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/applications_to_score_S20.csv"
path_cardBalance <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/credit_card_balance.csv"
path_previousApplication <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/previous_application.csv"

  
df_train <- read.csv(path_train)
df_toScore <- read.csv(path_toScore)
df_cardBalance <- read.csv(path_cardBalance)
df_previousApplication <- read.csv(path_previousApplication)

length(unique(df_train$SK_ID_CURR))
nrow(df_train)

# not right
df_combined <- merge(df_train,df_cardBalance,all.x = T,by = "SK_ID_CURR") #
nrow(df_combined)
df_combined_2 <- merge(df_train,df_previousApplication,all.x = T)
length(unique(df_previousApplication$SK_ID_CURR))
nrow(df_previousApplication)
nrow(df_combined_2)
length(unique(df_combined_2$SK_ID_CURR))


df_combined_double <-merge(df_combined,df_previousApplication,all.x = T) 
nrow(df_cardBalance)



for( i in 1:length(names(df_previousApplication)))
{
  if (names(df_previousApplication)[i] %in% names(df_toScore))
  {
    # print(names(df_previousApplication)[i])
  }
  else
    print(names(df_previousApplication)[i])
}


head(df_train)

# Doesn't work
# vis_miss(df_train, warn_large_data=FALSE)
# 

checkDataQuality(df_train, 
                 out.file.num ="~/Downloads/dq_train_num.csv", 
                 out.file.cat= "~/Downloads/dq_train_cat.csv")
dq_num<-read.csv("~/Downloads/dq_train_num.csv")
View(dq_num)

dq_cat<-read.csv("~/Downloads/dq_train_cat.csv")
View(dq_cat)

# Removing columns with more than 20% empty values
missing_percentage<-apply(df_train, 2, function(col)sum(is.na(col))/length(col))
missing_percentage[missing_percentage>0.2]
missing_percentage_df<-as.data.frame(missing_percentage[missing_percentage>0.2])
index<-rownames(missing_percentage_df)
df_train_2<- df_train[,!(names(df_train) %in% index)]

# Removing columns with p-value lower than 0.05 in chi square test 
correlation_percentage<-apply(df_train_2, 2, function(col)chisq.test(df_train_2$TARGET,col)$p.value)
correlation_percentage

correlation_percentage[correlation_percentage>0.05]
correlation_df<-as.data.frame(correlation_percentage[correlation_percentage>0.05])
index<-rownames(correlation_df)
index_2<-c(index,"TARGET")
df_train_3<- df_train_2[,(names(df_train_2) %in% index_2)]
colnames(df_train_3)

# Reformatting some columns
df_train_3$AMT_REQ_CREDIT_BUREAU_HOUR<-as.character(df_train_3$AMT_REQ_CREDIT_BUREAU_HOUR)
df_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new<-ifelse(  is.na(df_train_3$AMT_REQ_CREDIT_BUREAU_HOUR),"NULL",df_train_3$AMT_REQ_CREDIT_BUREAU_HOUR)
df_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new<-as.factor(df_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new)
table(df_train_3$AMT_REQ_CREDIT_BUREAU_HOUR.new)

df_train_3$AMT_REQ_CREDIT_BUREAU_DAY<-as.character(df_train_3$AMT_REQ_CREDIT_BUREAU_DAY)
df_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new<-ifelse(  is.na(df_train_3$AMT_REQ_CREDIT_BUREAU_DAY),"NULL",df_train_3$AMT_REQ_CREDIT_BUREAU_DAY)
df_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new<-as.factor(df_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new)
table(df_train_3$AMT_REQ_CREDIT_BUREAU_DAY.new)

df_train_3$AMT_REQ_CREDIT_BUREAU_WEEK<-as.character(df_train_3$AMT_REQ_CREDIT_BUREAU_WEEK)
df_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new<-ifelse(  is.na(df_train_3$AMT_REQ_CREDIT_BUREAU_WEEK),"NULL",df_train_3$AMT_REQ_CREDIT_BUREAU_WEEK)
df_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new<-as.factor(df_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new)
table(df_train_3$AMT_REQ_CREDIT_BUREAU_WEEK.new)

to.remove <-c("AMT_REQ_CREDIT_BUREAU_HOUR","AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK")
df_train_4 <- df_train_3[,-which(names(df_train_3) %in% to.remove)]
df_train_4  <- na.omit(df_train_4)

write.csv(df_train_4,"D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/df_train_4.csv")
read.csv("D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/df_train_4.csv")

# balance the data
df_train_4$TARGET <- as.integer(df_train_4$TARGET)
df_train_4_balanced <- ovun.sample(TARGET ~ ., data = df_train_4, p=0.5, N= 50000)$data # 20000 runs rather smooth
df_train_4_small <- ovun.sample(TARGET ~ ., data = df_train_4, N= 20000)$data
table(df_train_4_balanced$TARGET)
prop.table(table(df_train_4_balanced$TARGET))



# training set 1: balanced, split by 0.8
split_1 <- (.8)
training_index_1 <- sample(1:nrow(df_train_4_balanced),(split_1)*nrow(df_train_4_balanced)) 
df_training_1 <- df_train_4_balanced[training_index_1,]
table(df_training_1$TARGET)

test_index_1 <- sample(1:nrow(df_train_4_balanced),(1-split_1)*nrow(df_train_4_balanced)) 
df_test_1 <- df_train_4_balanced[test_index_1,]
table(df_test_1$TARGET)


# training set 2: unbalanced, small,  split by 0.8
split_2 <- (.8)
training_index_2 <- sample(1:nrow(df_train_4_small),(split_2)*nrow(df_train_4_small)) 
df_training_2 <- df_train_4_small[training_index_2,]
table(df_training_2$TARGET)

test_index_2 <- sample(1:nrow(df_train_4_small),(1-split_2)*nrow(df_train_4_small)) 
df_test_2 <- df_train_4_small[test_index_2,]
table(df_test_2$TARGET)




# model 1: running with training set 1, gbm library
gbm_model_1 <- gbm(formula=TARGET~.,
             distribution = "bernoulli",
             data=df_training_1,
             n.trees = 1000,
             interaction.depth = 4,
             shrinkage = 0.01,
             cv.folds = 4)
gbm_model_1 <- gbm(formula=TARGET~.,
                   distribution = "bernoulli",
                   data=df_training_1)
summary(gbm_model_1)


# cv.folds must be >= 1 in order to perform early stopping


# determine the optimum number of iterations
ntree_model_1_opt_cv <- gbm.perf(gbm_model_1,method = "cv")
print(model_1_opt_cv)
ntree_model_1_opt_oob <- gbm.perf(gbm_model_1,method = "OOB")
print(model_1_opt_oob)

# OOB generally underestimates the optimal number of iterations 
# although predictive performance is reasonably competitive. 
# Using cv_folds>1 when calling gbm usually results in improved predictive performance.

p1 <- predict(gbm_model_1,df_test_1,n.trees = ntree_model_1_opt_cv,type="response")

plot(seq(-1,1,length=length(sort(p1))),sort(p1))
predicted_1 <- ifelse(p1>0.5,1,0)
predictedFactor_1 <- as.factor(predicted_1)

result_1 <- table(predicted_1,df_test_1$TARGET)
result_1
targetFactor_1 <- as.factor(df_test_1$TARGET)

confusionMatrix(predictedBinaries_1,df_test_1$TARGET)
class(df_test_1$TARGET)

showcase_1 <- data.frame(cbind(actuals=df_test_1$TARGET,predicted=predicted_1))
head(showcase_1,10)

showcase_1$correctness=1-xor(showcase_1$actuals,showcase_1$predicted)
table(showcase_1$correctness)
accuracy_1=sum(showcase_1$correctness)/nrow(showcase_1)
accuracy_1 # =0.91



# model2: running with data set 1, caret library
df_test_1$TARGET <- as.factor(df_test_1$TARGET) # didn't run this line 

fitControl_gbm_2 <- trainControl(method = "repeatedcv",
                                number = 20,
                                repeats = 5)

grid_gbm_2 <- expand.grid(interaction.depth = c(3,4,5), 
                        n.trees = (1:10)*10, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbm_model_2 <- train(df_training_1[ , -which(names(df_training_1) %in% c("TARGET","SK_ID_CURR"))],
                     df_training_1$TARGET,
                     method="gbm",
                     trControl = fitControl_gbm_2,
                     tuneGrid = grid_gbm_2,
                     train.fraction = 0.5)
# train.faction is added to solve validDeviance = nan issue
# run from here
p2 <- predict(gbm_model_2,df_test_1,type="raw")

p2 <- as.factor(p2)


p2_2 <- predict(gbm_model_2,df_training_1,type="raw")
p2_2 <- as.factor(p2_2)
p2_2 <- ifelse(p2_2==2,1,0)
p2_2 <- as.factor(p2_2)

# model 2 assessment
confusionMatrix(p2,df_test_1$TARGET)

df_training_1$TARGET <- as.factor(df_training_1$TARGET)
confusionMatrix(p2_2,df_training_1$TARGET)

F1_Score()

# model 3
# model2: running with data set 1, caret library
df_test_2$TARGET <- as.factor(df_test_2$TARGET) # didn't run this line 

fitControl_gbm_3 <- trainControl(method = "repeatedcv",
                                 number = 20,
                                 repeats = 5)

grid_gbm_3 <- expand.grid(interaction.depth = c(3,4,5), 
                          n.trees = (1:10)*10, 
                          shrinkage = 0.1,
                          n.minobsinnode = 20)

gbm_model_3 <- train(df_training_2[ , -which(names(df_training_2) %in% c("TARGET","SK_ID_CURR"))],
                     df_training_2$TARGET,
                     method="gbm",
                     trControl = fitControl_gbm_3,
                     tuneGrid = grid_gbm_3,
                     train.fraction = 0.5)

# 
p_3 <- predict(gbm_model_2,df_test_2,type="raw")
table(p_3)
p_3 <- as.factor(p_3)

p_3 <- as.factor(p_3)
confusionMatrix(p_3,df_test_2$TARGET)

df_training_2$TARGET <- as.factor(df_training_2$TARGET)
confusionMatrix(p_3,df_training_2$TARGET)

# caret, getting tuning parameters
