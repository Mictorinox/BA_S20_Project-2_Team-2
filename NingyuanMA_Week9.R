Sys.setlocale("LC_ALL", "English")
library(naniar)
library(dataQualityR)
library(gbm)
library(caret)

path_train <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
path_toScore <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/applications_to_score_S20.csv"
path_cardBalance <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/credit_card_balance.csv"
path_previousApplication <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/previous_application.csv"

  
df_train <- read.csv(path_train)
df_toScore <- read.csv(path_toScore)
df_cardBalance <- read.csv(path_cardBalance)
df_previousApplication <- read.csv(path_previousApplication)


names(df_train)
names(df_toScore)

nrow(df_train)

for( i in 1:length(names(df_cardBalance)))
{
  if (names(df_cardBalance)[i] %in% names(df_train))
  {
    print("True")
  }
  else
    print("False")
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

write.csv("~/Downloads/df_train_4.csv")
read.csv("~/Downloads/df_train_4.csv")

table(df_train_4$TARGET)

# model1:split by 0.8
split_1 <- (.8)
training_index_1 <- sample(1:nrow(df_train_4),(split_1)*nrow(df_train_4)) 
df_training_1 <- df_train_4[training_index_1,]

test_index_1 <- sample(1:nrow(df_train_4),(1-split_1)*nrow(df_train_4)) 
df_test_1 <- df_train_4[test_index_1,]

gbm_model_1 <- gbm(formula=TARGET~.,
             distribution = "bernoulli",
             data=df_training_1,
             n.trees = 1000,
             interaction.depth = 4,
             shrinkage = 0.01,
             cv.folds = 4)
summary(gbm_model_1)


# determine the optimum number of iterations
model_1_opt_cv <- gbm.perf(gbm_model_1,method = "cv")
model_1_opt_oob <- gbm.perf(gbm_model_1,method = "OOB")

# OOB generally underestimates the optimal number of iterations 
# although predictive performance is reasonably competitive. 
# Using cv_folds>1 when calling gbm usually results in improved predictive performance.

p1 <- predict(gbm_1,df_test_1,type="response")
plot(seq(-2,2,length=length(sort(p1))),sort(p1))

# caret, getting tuning parameters
modelLookup("gbm")
