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
       