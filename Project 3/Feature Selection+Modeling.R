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
tr1 <- tr0
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

#Optional
#remove factors with too many levels
bad_col_2 <- c()
for(i in 1:length(tr_full)){
  if(length(levels(tr_full[,i]))>53 && class(tr_full[,i]) == "factor"){
    bad_col_2 <- c(bad_col_2, colnames(tr_full[i]))
  }
}
bad_col_2

#remove bad_col_2
tr_full_2 <- tr_full
tr_full <- tr_full[,!colnames(tr_full) %in% bad_col_2]

#######################################################
# train-test split for validation
########################################################
set.seed(1)
split<-(.9) 
trainingRowIndex <- sample(1:nrow(tr_full),(split)*nrow(tr_full)) # row indices for training data
trainingData <- tr_full[trainingRowIndex, ]  # model training data
testData  <- tr_full[-trainingRowIndex, ]   # test data

#caculate the rmse of the model for accuracy analysis
RMSE <- function(m, o){
  sqrt(mean((m - o)^2))
}

#######################################################
# building model - glm
########################################################
#tr_full_test <- sample_n(trainingData,2000)
start.time <- Sys.time()
glm_model <- glm(totalTransactionRevenue~.,trainingData[,c(-2:-4,-6)], family="gaussian")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#summary(glm_model)

#prediction for test dataset
glm_pred <- predict(glm_model, testData)
table(testData$subContinent)
#caculate rmse of predicted values
RMSE(testData$totalTransactionRevenue, gbm_pred) 


#######################################################
# building model - gbm
########################################################
str(tr_full)
require(gbm)
start.time <- Sys.time()
gbm_model <- gbm(totalTransactionRevenue~.,
                 distribution="gaussian",
                 data=trainingData[,c(-2,-4)],#,-6,-17)],    # notice -2:-4 or -2,-4
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
gbm_pred <- data.frame(actual = testData$totalTransactionRevenue, 
                       predict = predict(gbm_model, testData, n.trees=opt_iter))

#caculate the rmse of predicted values
RMSE(testData$totalTransactionRevenue, gbm_pred) 


#######################################################
# building model - rf       (factor issue?)
########################################################
require(randomForest)
start.time <- Sys.time()
rf_model <- randomForest(totalTransactionRevenue~.,trainingData[,c(-2,-4)],ntree=250, nodesize=100)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

fitControl <- trainControl(method="repeatedcv", number=5, repeats=3)
mtry <- sqrt(ncol(trainingData[,c(-2,-4)]))
tunegrid <- expand.grid(.mtry=mtry)

start.time <- Sys.time()
rf_model <-train(trainingData[,c(-2,-4,-25)], 
             trainingData[,25],
             method='rf',
             tuneGrid=tunegrid,
             trControl=fitControl)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

rf.predict<-predict(rf_2, test.df,type="raw")
#rf.predict
rf.conf2 <- confusionMatrix(rf.predict,test.df[,2], positive="1")
rf.conf2
F1_Score(test.df[,2],rf.predict)

#plot roc line
rf.probs_1 <- predict(rf_1,test.df,type="prob")
rf.probs_2 <- predict(rf_2,test.df,type="prob")
rf.plot<-plot(roc(test.df$TARGET,rf.probs_1[,2]), col="red")
rf.plot.tuned <-lines(roc(test.df$TARGET,rf.probs_2[,2]), col="orange")
legend("bottomright", legend=c("rf","rf tuned"), col=c("red","orange"), lwd=2)








######################################################
# winning solution 
#######################################################
###function includes getting:
###*dataframe of fixed time-interval of 168 days
###*revisited customers from the next fixed interval of 62 days (after 46 days from the first interval)
###*features from the first interval to predict target on the second interval
max(ifelse(is.na(tr_full$networkDomain) == TRUE, -9999, tr_full$networkDomain))
getTimeFramewithFeatures <- function(data, k)
{
  tf = data[data$date >= min(data$date)+168*(k-1) & data$date < min(data$date) + 168*k,]
  tf_fvid = unique(data[data$date >= min(data$date) + 168*k + 46 & data$date < min(data$date) + 168*k + 46 + 62,]$fullVisitorId)
  tf_returned = tf[tf$fullVisitorId %in% tf_fvid,]
  tf_tst = data[data$fullVisitorId %in% unique(tf_returned$fullVisitorId)
                & data$date >= min(data$date) + 168*k + 46 & data$date < min(data$date) + 168*k + 46 + 62,]
  
  tf_target = aggregate(totalTransactionRevenue ~ fullVisitorId, tf_tst, function(x) log(1 + sum(x)))
  tf_target$ret = 1
  colnames(tf_target)[2] = c("target")
  tf_nonret = data.frame(fullVisitorId = unique(tf[!(tf$fullVisitorId %in% tf_fvid),]$fullVisitorId), target = 0, ret = 0)
  tf_target = rbind(tf_target, tf_nonret)
  tf_maxdate = max(tf$date)
  tf_mindate = min(tf$date)
  
  tf <- tf %>% 
    group_by(fullVisitorId) %>%
    summarize(
      channelGrouping = max(ifelse(is.na(channelGrouping) == TRUE, -9999, channelGrouping)),
      first_ses_from_the_period_start = min(date) - tf_mindate,
      last_ses_from_the_period_end = tf_maxdate - max(date),
      interval_dates = max(date) - min(date),
      unique_date_num = length(unique(date)),
      maxVisitNum = max(visitNumber, na.rm = TRUE),
      browser = max(ifelse(is.na(browser) == TRUE, -9999, browser)),
      operatingSystem = max(ifelse(is.na(operatingSystem) == TRUE, -9999, operatingSystem)),
      deviceCategory = max(ifelse(is.na(deviceCategory) == TRUE, -9999, deviceCategory)),
      continent = max(ifelse(is.na(continent) == TRUE, -9999, continent)),
      subContinent = max(ifelse(is.na(subContinent) == TRUE, -9999, subContinent)),
      country = max(ifelse(is.na(country) == TRUE, -9999, country)),
      region =max(ifelse(is.na(region) == TRUE, -9999, region)),
      metro = max(ifelse(is.na(metro) == TRUE, -9999, metro)),
      city = max(ifelse(is.na(city) == TRUE, -9999, city)),
      networkDomain = max(ifelse(is.na(networkDomain) == TRUE, -9999, networkDomain)),
      source = max(ifelse(is.na(source) == TRUE, -9999, source)),
      medium = max(ifelse(is.na(medium) == TRUE, -9999, medium)),
      isVideoAd_mean = mean(ifelse(is.na(adwordsClickInfo.isVideoAd) == TRUE, 0, 1)),
      isMobile = mean(ifelse(isMobile == TRUE, 1 , 0)),
      isTrueDirect = mean(ifelse(is.na(isTrueDirect) == TRUE, 0, 1)),
      hits_sum = sum(hits),
      hits_mean = mean(hits),
      hits_min = min(hits),
      hits_max = max(hits),
      hits_median = median(hits),
      hits_sd = sd(hits),
      pageviews_sum = sum(pageviews, na.rm = TRUE),
      pageviews_mean = mean(pageviews, na.rm = TRUE),
      pageviews_min = min(pageviews, na.rm = TRUE),
      pageviews_max = max(pageviews, na.rm = TRUE),
      pageviews_median = median(pageviews, na.rm = TRUE),
      pageviews_sd = sd(pageviews, na.rm = TRUE),
      session_cnt = NROW(visitStartTime),
      totalTransactionRevenue = sum(totalTransactionRevenue),
      transactions  = sum(transactions,na.rm = TRUE)
    )
  tf = join(tf, tf_target, by = "fullVisitorId")
  return(tf)
}
str(tr_full)
###Getting parts of train-set 
message("Get 1st train part")
tr1 = getTimeFramewithFeatures(tr_full, 1)
message("Get 2nd train part")
tr2 = getTimeFramewithFeatures(tr_full, 2)
message("Get 3rd train part")
tr3 = getTimeFramewithFeatures(tr_full, 3)
message("Get 4th train part")
tr4 = getTimeFramewithFeatures(tr_full, 4)

###Costruction of the test-set (by analogy as train-set) 
message("Get test")
tr5 = tr_full[tr_full$date >=  '2018-05-01',]
tr5_maxdate = max(tr5$date)
tr5_mindate = min(tr5$date)

tr5 <- tr5 %>% 
  group_by(fullVisitorId) %>%
  summarize(
    channelGrouping = max(ifelse(is.na(channelGrouping) == TRUE, -9999, channelGrouping)),
    first_ses_from_the_period_start = min(date) - tr5_mindate,
    last_ses_from_the_period_end = tr5_maxdate - max(date),
    interval_dates = max(date) - min(date),
    unique_date_num = length(unique(date)),
    maxVisitNum = max(visitNumber, na.rm = TRUE),
    browser = max(ifelse(is.na(browser) == TRUE, -9999, browser)),
    operatingSystem = max(ifelse(is.na(operatingSystem) == TRUE, -9999, operatingSystem)),
    deviceCategory = max(ifelse(is.na(deviceCategory) == TRUE, -9999, deviceCategory)),
    continent = max(ifelse(is.na(continent) == TRUE, -9999, continent)),
    subContinent = max(ifelse(is.na(subContinent) == TRUE, -9999, subContinent)),
    country = max(ifelse(is.na(country) == TRUE, -9999, country)),
    region =max(ifelse(is.na(region) == TRUE, -9999, region)),
    metro = max(ifelse(is.na(metro) == TRUE, -9999, metro)),
    city = max(ifelse(is.na(city) == TRUE, -9999, city)),
    networkDomain = max(ifelse(is.na(networkDomain) == TRUE, -9999, networkDomain)),
    source = max(ifelse(is.na(source) == TRUE, -9999, source)),
    medium = max(ifelse(is.na(medium) == TRUE, -9999, medium)),
    isVideoAd_mean = mean(ifelse(is.na(adwordsClickInfo.isVideoAd) == TRUE, 0, 1)),
    isMobile = mean(ifelse(isMobile == TRUE, 1 , 0)),
    isTrueDirect = mean(ifelse(is.na(isTrueDirect) == TRUE, 0, 1)),
    bounce_sessions = sum(ifelse(is.na(bounces) == TRUE, 0, 1)),
    hits_sum = sum(hits),
    hits_mean = mean(hits),
    hits_min = min(hits),
    hits_max = max(hits),
    hits_median = median(hits),
    hits_sd = sd(hits),
    pageviews_sum = sum(pageviews, na.rm = TRUE),
    pageviews_mean = mean(pageviews, na.rm = TRUE),
    pageviews_min = min(pageviews, na.rm = TRUE),
    pageviews_max = max(pageviews, na.rm = TRUE),
    pageviews_median = median(pageviews, na.rm = TRUE),
    pageviews_sd = sd(pageviews, na.rm = TRUE),
    session_cnt = NROW(visitStartTime),
    transactionRevenue = sum(transactionRevenue),
    transactions  = sum(transactions, na.rm = TRUE)
  )
tr5$target = NA
tr5$ret = NA

###Combining all pieces and converting the types
train_all = rbind(tr1,tr2,tr3)
train_all$interval_dates = as.integer(train_all$interval_dates)
train_all$last_ses_from_the_period_end = as.integer(train_all$last_ses_from_the_period_end)
train_all$first_ses_from_the_period_start = as.integer(train_all$first_ses_from_the_period_start)
str(train_all)
for (i in c(2,8:19))
  train_all[,i] = as.numeric(as.factor(train_all[,i]))
train_all[1:10,]

###Filtering train and test from combined dataframe
train = train_all[is.na(train_all$target) == FALSE,]
test =  train_all[is.na(train_all$target) == TRUE,]

###Parameters of "isReturned" classificator 
param_lgb2 = list(objective = "binary",
                  max_bin = 256,
                  learning_rate = 0.01,
                  num_leaves = 15,
                  bagging_fraction = 0.9,
                  feature_fraction = 0.8,
                  min_data = 1,
                  bagging_freq = 1,
                  metric = "binary_logloss")

###Parameters of "How_Much_Returned_Will_Pay" regressor
param_lgb3= list(objective = "regression",
                 max_bin = 256,
                 learning_rate = 0.01,
                 num_leaves = 9,
                 bagging_fraction = 0.9,
                 feature_fraction = 0.8,
                 min_data = 1,
                 bagging_freq = 1,
                 metric = "rmse")

###Training and prediction of models: Averaging of 10 [Classificator*Regressor] values
dtrain_all <- lgb.Dataset(as.matrix(train[-c(1,39,40)]),label = train$ret)
dtrain_ret <- lgb.Dataset(as.matrix(train[-c(1,39,40)][train$ret == 1,]),label = train[train$ret == 1,]$target)
pr_lgb_sum = 0
message("Training and predictions")
for (i in c(1:10)) {
  message("Iteration number ", i)
  lgb_model1 = lgb.train(dtrain_all, params = param_lgb2, nrounds = 1200, bagging_seed = 13 + i, feature_fraction_seed = 42 + i)
  pr_lgb = predict(lgb_model1, as.matrix(test[c(names(train[-c(1,39,40)]))]))
  lgb_model2 = lgb.train(dtrain_ret, params = param_lgb3, nrounds = 368, bagging_seed = 42 + i, feature_fraction_seed = 13 + i)
  pr_lgb_ret = predict(lgb_model2, as.matrix(test[c(names(train[-c(1,39,40)]))]))
  pr_lgb_sum = pr_lgb_sum + pr_lgb*pr_lgb_ret
}
pr_final2 = pr_lgb_sum/10

###Writing final predictions into csv
summary(pr_final2)
newsub4 = data.frame(fullVisitorId = test$fullVisitorId, PredictedLogRevenue = pr_final2)
write.csv(newsub4, "tst4.csv", row.names = FALSE, quote = FALSE)

