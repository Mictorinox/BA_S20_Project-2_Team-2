########################
library(plyr)
library(tidyverse)
library(jsonlite)
library(lubridate)


library(plyr)
library(tidyverse)
library(jsonlite)
library(magrittr)
library(caret)
library(lubridate)
library(lightgbm)
library(foreach)

########################
Sys.setlocale("LC_ALL", "English")
'%ni%' <- function(x,y)!('%in%'(x,y))

#Defining types 

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
               hits = col_character(),
               customDimensions = col_character())

source_path <- "D:/A-Themed/0-NYU/Spring 2020/2-Business Analytics/2-Homework/Week 11(project 3)/0-Source/1-Dataset/"
test <- paste(source_path,"train_yes_v7.csv",sep = "")


train_v7_path <- paste(source_path,"train_yes_v7.csv",sep = "")
train_v6_path <- "D:/A-Themed/0-NYU/Spring 2020/2-Business Analytics/2-Homework/Week 11(project 3)/0-Source/1-Dataset/train_yes_v6.csv"
test_v6_path <- "D:/A-Themed/0-NYU/Spring 2020/2-Business Analytics/2-Homework/Week 11(project 3)/0-Source/1-Dataset/test_yes_v6.csv"
df_train_v6 <- read_csv(train_v6_path,col_types=ctypes)
names(df_train_v6)

###Data extraction
message("Data Extraction")
tr0 <- read_csv(train_v6_path,col_types=ctypes)
tr0_test <- read_csv(test_v6_path,col_types=ctypes)


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
tr_full <- parse(tr0)
tr_full <- as.data.frame(tr_full)
tr_full$totalTransactionRevenue <- as.numeric(tr_full$totalTransactionRevenue)
tr_full <- tr_full %>% select(-transactionRevenue) # transactionRevenue is deprecated



#replace NA in logical class with False
for (i in 1:length(tr_full)) {
  ifelse(class(tr_full[,i]) == "logical", tr_full[,i][is.na(tr_full[,i])] <- as.logical(0), tr_full[,i]) 
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


###Defining useful columns
good_cols = c("channelGrouping","date","fullVisitorId","visitId","visitNumber","visitStartTime","browser","deviceCategory",
              "isMobile","operatingSystem","city","continent","country","metro","networkDomain","region","subContinent",                              
              "hits","newVisits","pageviews","sessionQualityDim","timeOnSite","totalTransactionRevenue",
              "transactions","adContent","adwordsClickInfo.adNetworkType", "adwordsClickInfo.gclId","adwordsClickInfo.isVideoAd",
              "adwordsClickInfo.page", "adwordsClickInfo.slot","campaign","isTrueDirect","keyword","medium","referralPath","source")
# Removed "transactionRevenue" here,
# subset(good_cols,good_cols %ni% names(tr_full))

###Combining tables and convertion to dataframe
# removed temperarily because currently we don't have to combine training set with testing set 
# tr_full = rbind(tr_full[c(good_cols)], te[c(good_cols)])
# tr_full = as.data.frame(tr_full)
summary(tr_full)

###date as ymd-date, part of columns as integers
tr_full$date = ymd(tr_full$date)
for (i in c(18:26,31))
  tr_full[,i] = as.integer(tr_full[,i])


###if transactionRevenue is NA, it means 0
## edited into totalTransactionRevenue
tr_full$totalTransactionRevenue = ifelse(is.na(tr_full$totalTransactionRevenue) == TRUE, 0, tr_full$totalTransactionRevenue)

tr_full_re <- tr_full
tr_full_re$totalTransactionRevenue <- tr_full_re$totalTransactionRevenue/10^6

names(tr_full_re)
tr_full_re2 <-select(tr_full_re,-hits)

write.csv(tr_full_re2,file=paste(source_path,"train_yes_v9.csv",sep = ""),row.names=F,fileEncoding = "utf-8")
tr_full_2 <- read.csv(paste(source_path,"train_yes_v9.csv",sep = ""),stringsAsFactors=F,fileEncoding = "utf-8")
str(tr_full_2)



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
