###Loading required libraries
library(plyr)
library(tidyverse)
library(jsonlite)
library(magrittr)
library(caret)
library(lubridate)
library(lightgbm)
library(foreach)

###Defining types of extracted attributes
ctypes <- cols(fullVisitorId = col_character(),
               channelGrouping = col_character(),
               date = col_datetime(),
               device = col_character(),
               geoNetwork = col_character(),
               socialEngagementType = col_skip(), 
               totals = col_character(),
               trafficSource = col_character(),
               visitId = col_integer(), 
               visitNumber = col_integer(),
               visitStartTime = col_integer(),
               hits = col_skip(),
               customDimensions = col_skip())

###Data extraction
message("Data Extraction")

path_train_v2 <- ""
path_test_v2 <- ""

tr0 <- read_csv(path_train_v2, col_types = ctypes)
te0 <- read_csv(path_test_v2, col_types = ctypes)

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
  select(-device, -geoNetwork, -trafficSource, -totals)

message("Data Parsing")
tr <- parse(tr0)
te <- parse(te0)

###Defining useful columns
good_cols = c("channelGrouping","date","fullVisitorId","visitId","visitNumber","visitStartTime","browser","deviceCategory",
              "isMobile","operatingSystem","city","continent","country","metro","networkDomain","region","subContinent","bounces",                              
              "hits","newVisits","pageviews","sessionQualityDim","timeOnSite","totalTransactionRevenue","transactionRevenue",
              "transactions","adContent","adwordsClickInfo.adNetworkType", "adwordsClickInfo.gclId","adwordsClickInfo.isVideoAd",
              "adwordsClickInfo.page", "adwordsClickInfo.slot","campaign","isTrueDirect","keyword","medium","referralPath","source")

###Combining tables and convertion to dataframe
tr = rbind(tr[c(good_cols)], te[c(good_cols)])
tr = as.data.frame(tr)

###date as ymd-date, part of columns as integers
tr$date = ymd(tr$date)
for (i in c(18:26,31))
  tr[,i] = as.integer(tr[,i])

###if transactionRevenue is NA, it means 0
tr$transactionRevenue = ifelse(is.na(tr$transactionRevenue) == TRUE, 0, tr$transactionRevenue)

###function includes getting:
###*dataframe of fixed time-interval of 168 days
###*revisited customers from the next fixed interval of 62 days (after 46 days from the first interval)
###*features from the first interval to predict target on the second interval
getTimeFramewithFeatures <- function(data, k)
{
  tf = data[data$date >= min(data$date)+168*(k-1) & data$date < min(data$date) + 168*k,]
  tf_fvid = unique(data[data$date >= min(data$date) + 168*k + 46 & data$date < min(data$date) + 168*k + 46 + 62,]$fullVisitorId)
  tf_returned = tf[tf$fullVisitorId %in% tf_fvid,]
  tf_tst = data[data$fullVisitorId %in% unique(tf_returned$fullVisitorId)
               & data$date >= min(data$date) + 168*k + 46 & data$date < min(data$date) + 168*k + 46 + 62,]
  
  tf_target = aggregate(transactionRevenue ~ fullVisitorId, tf_tst, function(x) log(1 + sum(x)))
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
      transactions  = sum(transactions,na.rm = TRUE)
    )
  tf = join(tf, tf_target, by = "fullVisitorId")
  return(tf)
}

###Getting parts of train-set 
message("Get 1st train part")
tr1 = getTimeFramewithFeatures(tr, 1)
message("Get 2nd train part")
tr2 = getTimeFramewithFeatures(tr, 2)
message("Get 3rd train part")
tr3 = getTimeFramewithFeatures(tr, 3)
message("Get 4th train part")
tr4 = getTimeFramewithFeatures(tr, 4)

###Costruction of the test-set (by analogy as train-set) 
message("Get test")
tr5 = tr[tr$date >=  '2018-05-01',]
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
train_all = rbind(tr1,tr2,tr3,tr4,tr5)
train_all$interval_dates = as.integer(train_all$interval_dates)
train_all$last_ses_from_the_period_end = as.integer(train_all$last_ses_from_the_period_end)
train_all$first_ses_from_the_period_start = as.integer(train_all$first_ses_from_the_period_start)
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