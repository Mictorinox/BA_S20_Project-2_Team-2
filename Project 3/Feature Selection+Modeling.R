###Loading required libraries
library(plyr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(caret)
library(gbm)
#install.packages("")

###Defining types of extracted attributes
ctypes <- cols(fullVisitorId = col_character(),
               channelGrouping = col_character(),
               date = col_datetime(),
               device = col_character(),
               geoNetwork = col_character(),
               socialEngagementType = col_character(), 
               totals = col_character(),
               trafficSource = col_character(),
               visitId = col_integer(), 
               visitNumber = col_integer(),
               visitStartTime = col_integer(),
               hits = col_skip(),
               customDimensions = col_skip())

###Data extraction
message("Data Extraction")
tr0 <- read_csv("D:\\MingziTian\\BA\\Project 3 team 4\\train_yes_v6.csv",col_types=ctypes)
tr0_test <- read_csv("D:\\MingziTian\\BA\\Project 3 team 4\\test_yes_v6.csv",col_types=ctypes)
#tr0 <- read.csv("D:\\MingziTian\\BA\\Project 3 team 4\\combined_yes_v10.csv",header=T)
str(tr0)
#te0 <- read_csv("F:\\google_test.csv", col_types = ctypes)

#tr0$customDimensions <- gsub("\'","\"", tr0$customDimensions)

###Data parsing
flatten_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

parse <- . %>% 
  bind_cols(flatten_json(.$device)) %>%
  bind_cols(flatten_json(.$geoNetwork)) %>% 
  bind_cols(flatten_json(.$trafficSource)) %>% 
  bind_cols(flatten_json(.$totals)) %>%
  #bind_cols(flatten_json(.$customDimensions)) %>%
  select(-device, -geoNetwork, -trafficSource, -totals) #,-customDimensions,-hits)

message("Further cleaning")
tr <- parse(tr0)
tr_test <- parse(tr0_test)
tr_test <- tr_test %>% select(-IF_Customer)
tr_full <- rbind(tr, tr_test)
tr_full <- as.data.frame(tr_full)
tr_full$totalTransactionRevenue <- as.numeric(tr_full$totalTransactionRevenue)
tr_full$totalTransactionRevenue <- log(tr_full$totalTransactionRevenue)
tr_full <- tr_full %>% select(-transactionRevenue)

#replace NA in logical class with False
for (i in 1:length(tr_full)) {
  ifelse(class(tr_full[,i]) == "logical", tr_full[,i][is.na(tr_full[,i])] <- as.logical(0), tr_full[,i]) 
}

#conver logical class to factor 
for (i in 1:length(tr_full)) {
  ifelse(class(tr_full[,i]) == "logical", tr_full[,i] <- as.factor(tr_full[,i]), tr_full[,i]) 
}

#replace NA in character class with character 0
for(i in 1:length(tr_full)){
  ifelse(class(tr_full[,i])=="character",  tr_full[,i][is.na(tr_full[,i])] <- as.character(0), tr_full[,i])
}

#convert character to factor
for(i in 1:length(tr_full)){
  ifelse(class(tr_full[,i])=="character",  tr_full[,i] <- as.factor(tr_full[,i]), tr_full[,i])
}

#identify columns with 1 factor level
bad_col <- c()
for(i in 1:length(tr_full)){
  if(length(levels(tr_full[,i]))<2 && class(tr_full[,i]) == "factor"){
   bad_col <- c(bad_col, colnames(tr_full[i]))
  }
}
bad_col

#remove bad columns
tr_full <- tr_full[,!colnames(tr_full) %in% bad_col]

###date as ymd-date, part of columns as integers
tr_full$date = ymd(tr_full$date)
tr_full$visitStartTime <- as.POSIXct(tr_full$visitStartTime, tz="UTC", origin='1970-01-01')

#change specific columns to integer
for (i in c(5,29:34)){
  tr_full[,i] = as.integer(tr_full[,i])
}

tr_full$visitId <- as.factor(tr_full$visitId)
str(tr_full)

#for(i in 1:length(tr_full)){
#  ifelse(class(tr_full[,i])=="factor",  tr_full[,i] <- as.character(tr_full[,i]), tr_full[,i])
#}


#########################################
#Common models (glm,gbm)
#Split train and test data for later validation
tr_train <- tr_full[tr_full$date<="2018-4-30",]
tr_test <- tr_full[tr_full$date>"2018-4-30",]


#######################################################
# train-test split for validation (if use original train and test dataset, no need to split again)
########################################################
set.seed(1)
split<-(.9) 
trainingRowIndex <- sample(1:nrow(tr_train),(split)*nrow(tr_train)) # row indices for training data
trainingData <- tr_train[trainingRowIndex, ]  # model training data
testData  <- tr_test[-trainingRowIndex, ]   # test data

#caculate the rmse of the model for accuracy analysis
RMSE <- function(m, o){
  sqrt(mean((m - o)^2))
}

#######################################################
# building model - glm
########################################################
#tr_full_test <- sample_n(trainingData,2000)
start.time <- Sys.time()
glm_model <- glm(totalTransactionRevenue~.,tr_train[,c(-2:-4,-6)], family="gaussian")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#summary(glm_model)

#prediction for test dataset
glm_pred <- predict(glm_model, tr_test)

#caculate rmse of predicted values
RMSE(testData$totalTransactionRevenue, glm_pred) 


#######################################################
# building model - gbm
########################################################
str(tr_train)
require(gbm)
start.time <- Sys.time()
gbm_model <- gbm(totalTransactionRevenue~.,
                 distribution="gaussian",
                 data=tr_train[,c(-2:-4,-6,-17)],#,-6,-17)],    # notice -2:-4 or -2,-4
                 n.trees = 500, # number of trees
                 interaction.depth = 3,
                 n.minobsinnode = 100, # minimum number of obs needed in each node
                 shrinkage = 0.01, # learning rate
                 bag.fraction = 0.5, # subsampling
                 train.fraction = 0.5,
                 cv.folds = 10,      # do 10-fold cross-validation
                 verbose = FALSE )
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#Get the best performance ntree option
opt_iter <- gbm.perf(gbm_model, method="test")
gbm_imp <- summary(gbm_model, n.trees=opt_iter)
gbm_imp

#prediction for test dataset
gbm_pred <- data.frame(actual = tr_test$totalTransactionRevenue, 
                       predict = predict(gbm_model, tr_test, n.trees=opt_iter))

#caculate the rmse of predicted values
RMSE(tr_test$totalTransactionRevenue, gbm_pred$predict) 


#######################################################
# building model - rf       (use training data tr_train_rf)
########################################################

#########################################!!!!!!!
#########################################
#Optional (for rf model)
#remove factors with too many levels
bad_col_2 <- c()
for(i in 1:length(tr_full)){
  if(length(levels(tr_full[,i]))>53 && class(tr_full[,i]) == "factor"){
    bad_col_2 <- c(bad_col_2, colnames(tr_full[i]))
  }
}
bad_col_2

#remove bad_col_2
tr_full_rf <- tr_full[,!colnames(tr_full) %in% bad_col_2]
#prepare special datasets for rf
tr_train_rf <- tr_full_rf[tr_full_rf$date<="2018-4-30",]
tr_test_rf <- tr_full_rf[tr_full_rf$date>"2018-4-30",]
#########################################
#########################################

require(randomForest)
start.time <- Sys.time()
rf_model <- randomForest(totalTransactionRevenue~.,tr_train_rf[,c(-2,-4)],ntree=250, nodesize=100)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#prediction for tr_test_rf
rf_predict <-predict(rf_model, tr_test_rf,type="response")

#caculate the rmse of predicted values
RMSE(tr_test_rf$totalTransactionRevenue, rf_predict)


#######################################################
# building model - rf 2      (use training data tr_train_rf)
########################################################
fitControl <- trainControl(method="repeatedcv", number=5, repeats=3)
#mtry <- sqrt(ncol(trainingData[,c(-2,-4)]))
#tunegrid <- expand.grid(.mtry=mtry)

start.time <- Sys.time()
rf_model2 <-train(tr_train_rf[,c(-2,-4,-22)], 
             tr_train_rf[,22],
             method='rf',
             #tuneGrid=tunegrid,
             trControl=fitControl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

rf.predict<-predict(rf_model2, tr_test_rf,type="response")
