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
path_train6 <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/application_train_6.csv"
path_balanceClean <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/cc_balance_clean.csv"
path_applicationClean <- "D:/G-OneDrive/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/pre_application_clean.csv"

# path for x1
path_train <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/application_train_S20.csv"
path_toScore <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/applications_to_score_S20.csv"
path_cardBalance <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/credit_card_balance.csv"
path_previousApplication <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/Project 2 External -S20/previous_application.csv"
path_train6 <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/application_train_6.csv"
path_balanceClean <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/cc_balance_clean.csv"
path_applicationClean <- "C:/Users/Matyas/OneDrive/1-NYU/2-Business Analytics/2-Homework/Week 9(project 2)/3-Dataset/pre_application_clean.csv"
 
df_train <- read.csv(path_train)
df_toScore <- read.csv(path_toScore)
df_cardBalance <- read.csv(path_cardBalance)
df_previousApplication <- read.csv(path_previousApplication)

df_train6 <- read.csv(path_train6)
df_balanceClean <- read.csv(path_balanceClean)
df_applicationClean <- read.csv(path_applicationClean)

length(unique(df_train$SK_ID_CURR))
nrow(df_train)

result_yes <- c()
result_no <- c()

for( i in 1:length(df_toScore$SK_ID_CURR))
{
  if (df_toScore$SK_ID_CURR[i] %in% df_cardBalance$SK_ID_CURR)
  {
    # print(names(df_previousApplication)[i])
    result_yes <- c(result_yes,df_toScore$SK_ID_CURR[i])
  }
  else
    result_no <- c(result_no,df_toScore$SK_ID_CURR[i])
}

length(result_yes)
length(result_no)


length(unique(df_toScore$SK_ID_CURR))
head(df_train)

