path_train <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
path_toScore <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/applications_to_score_S20.csv"
path_cardBalance <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/credit_card_balance.csv"
path_preciousApplication <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/previous_application.csv"

  
df_train <- read.csv(path_train)
df_toScore <- read.csv(path_toScore)
df_cardBalance <- read.csv(path_cardBalance)
df_preciousApplication <- read.csv(path_preciousApplication) 


names(df_train)
names(df_toScore)

for( i in 1:length(names(df_cardBalance)))
{
  if (names(df_cardBalance)[i] %in% names(df_train))
  {
    print("True")
  }
  else
    print("False")
}

for( i in 1:length(names(df_cardBalance)))
{
  if (names(df_cardBalance)[i] %in% names(df_train))
  {
    print("True")
  }
  else
    print("False")
}


  
names(df_preciousApplication)

nrow(df_train)

lm(data = df_train)
names(df_cardBalance)[2]
length(names(df_cardBalance))


df_train <- read.csv("~/Downloads/df_train_S20.csv")
head(df_train)

library(naniar)
vis_miss(df_train, warn_large_data=FALSE)

missing_percentage<-apply(df_train, 2, function(col)sum(is.na(col))/length(col))
missing_percentage[missing_percentage>0.2]
missing_percentage_df<-as.data.frame(missing_percentage[missing_percentage>0.2])
index<-rownames(missing_percentage_df)
df_train_2<- df_train[,!(names(df_train) %in% index)]

correlation_percentage<-apply(df_train_2, 2, function(col)chisq.test(df_train_2$TARGET,col)$p.value)
correlation_percentage

correlation_percentage[correlation_percentage>0.05]
correlation_df<-as.data.frame(correlation_percentage[correlation_percentage>0.05])
index<-rownames(correlation_df)
index_2<-c(index,"TARGET")
df_train_3<- df_train_2[,(names(df_train_2) %in% index_2)]
colnames(df_train_3)

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
       