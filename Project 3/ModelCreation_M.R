########################
library(plyr)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(magrittr)
library(caret)
# library(lightgbm)
library(gbm)
library(foreach)

########################
Sys.setlocale("LC_ALL", "English")
'%ni%' <- function(x,y)!('%in%'(x,y))

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
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
te0 <- read_csv(test_v6_path,col_types=ctypes)


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
te_full <- parse(te0)

tr_full <- as.data.frame(tr_full)
te_full <- as.data.frame(te_full)

tr_full$totalTransactionRevenue <- as.numeric(tr_full$totalTransactionRevenue)
te_full$totalTransactionRevenue <- as.numeric(te_full$totalTransactionRevenue)
tr_full <- tr_full %>% select(-transactionRevenue) # transactionRevenue is deprecated
te_full <- te_full %>% select(-transactionRevenue)

#replace NA in logical class with False
for (i in 1:length(tr_full)) {
  ifelse(class(tr_full[,i]) == "logical", tr_full[,i][is.na(tr_full[,i])] <- as.logical(0), tr_full[,i]) 
}
for (i in 1:length(te_full)) {
  ifelse(class(te_full[,i]) == "logical", te_full[,i][is.na(te_full[,i])] <- as.logical(0), te_full[,i]) 
}


#replace NA in character class with character 0
for(i in 1:length(tr_full)){
  ifelse(class(tr_full[,i])=="character",  tr_full[,i][is.na(tr_full[,i])] <- as.character(0), tr_full[,i])
}
for(i in 1:length(te_full)){
  ifelse(class(te_full[,i])=="character",  te_full[,i][is.na(te_full[,i])] <- as.character(0), te_full[,i])
}

#convert character to factor
for(i in 1:length(tr_full)){
  ifelse(class(tr_full[,i])=="character",  tr_full[,i] <- as.factor(tr_full[,i]), tr_full[,i])
}
for(i in 1:length(te_full)){
  ifelse(class(te_full[,i])=="character",  te_full[,i] <- as.factor(te_full[,i]), te_full[,i])
}

#identify columns with 1 factor level
bad_col <- c()
for(i in 1:length(tr_full)){
  if(length(levels(tr_full[,i]))<2 && class(tr_full[,i]) == "factor"){
    bad_col <- c(bad_col, colnames(tr_full[i]))
  }
}
length(bad_col)


#remove bad columns
tr_full <- tr_full[,!colnames(tr_full) %in% bad_col]
te_full <- te_full[,!colnames(te_full) %in% bad_col]
range(te_full$date)
###Defining useful columns
good_cols = c("channelGrouping","date","fullVisitorId","visitId","visitNumber","visitStartTime","browser","deviceCategory",
              "isMobile","operatingSystem","city","continent","country","metro","networkDomain","region","subContinent",                              
              "hits","newVisits","pageviews","sessionQualityDim","timeOnSite","totalTransactionRevenue",
              "transactions","adContent","adwordsClickInfo.adNetworkType", "adwordsClickInfo.gclId","adwordsClickInfo.isVideoAd",
              "adwordsClickInfo.page", "adwordsClickInfo.slot","campaign","isTrueDirect","keyword","medium","referralPath","source")
length(good_cols)
# Removed "transactionRevenue" here,
# subset(good_cols,good_cols %ni% names(tr_full))

###Combining tables and convertion to dataframe
# removed temperarily because currently we don't have to combine training set with testing set 
# added to combine factor levels
tr_full = rbind(tr_full[c(good_cols)], te_full[c(good_cols)])
tr_full = as.data.frame(tr_full)
str(tr_full)

###date as ymd-date, part of columns as integers
tr_full$date = ymd(tr_full$date)
tr_full$visitStartTime <- as.POSIXct(tr_full$visitStartTime, tz="UTC", origin='1970-01-01')
tr_full$fullVisitorId <- as.character(tr_full$fullVisitorId)
class(tr_full$fullVisitorId)

for (i in c(18:26,31))
  tr_full[,i] = as.integer(tr_full[,i])

tr_full$adwordsClickInfo.isVideoAd <- as.factor(tr_full$adwordsClickInfo.isVideoAd)
tr_full$isTrueDirect <- as.factor(tr_full$isTrueDirect)
tr_full$isMobile <- as.factor(tr_full$isMobile)
tr_full$adContent <- as.character(tr_full$adContent)
tr_full$adwordsClickInfo.adNetworkType <- as.character(tr_full$adwordsClickInfo.adNetworkType)
tr_full$campaign <- as.character(tr_full$campaign)

###if transactionRevenue is NA, it means 0
## edited into totalTransactionRevenue
tr_full$totalTransactionRevenue = ifelse(is.na(tr_full$totalTransactionRevenue) == TRUE, 0, tr_full$totalTransactionRevenue)
tr_full$logtotalTransactionRevenue<-ifelse(tr_full$totalTransactionRevenue==0,0,log(tr_full$totalTransactionRevenue))

tr_full <-select(tr_full,-hits)
names(tr_full)
tr_full$deviceCategory<-as.factor(tr_full$deviceCategory)
tr_full$continent<-as.factor(tr_full$continent)
tr_full$country<-as.factor(tr_full$country)
tr_full$metro<-as.factor(tr_full$metro)
tr_full$medium<-as.factor(tr_full$medium)
tr_full$source<-as.factor(tr_full$source)
tr_full$others<-as.factor(tr_full$others)
tr_full$region<-as.factor(tr_full$region)
tr_full$adwordsClickInfo.slot<-as.factor(tr_full$adwordsClickInfo.slot)
tr_full$isTrueDirect<-as.factor(tr_full$isTrueDirect)
tr_full$keyword<-as.factor(tr_full$keyword)

tr_full_re <- tr_full
tr_full_re$totalTransactionRevenue <- tr_full_re$totalTransactionRevenue/10^6

tr_full_training <- subset(tr_full,tr_full$date < as.Date("2018-05-01"))
tr_full_testing <- subset(tr_full,tr_full$date >= as.Date("2018-05-01"))


write.csv(tr_full_training,file=paste(source_path,"train_yes_v11.csv",sep = ""),row.names=F,fileEncoding = "utf-8")
write.csv(tr_full_testing,file=paste(source_path,"test_yes_v11.csv",sep = ""),row.names=F,fileEncoding = "utf-8")

write.csv(tr_full,file=paste(source_path,"combined_yes_v10_forVisual.csv",sep = ""),row.names=F,fileEncoding = "utf-8")

tr_full_training_2 <- read.csv(paste(source_path,"train_yes_v11.csv",sep = ""),stringsAsFactors=F,fileEncoding = "utf-8")
tr_full_testing_2 <- read.csv(paste(source_path,"test_yes_v11.csv",sep = ""),stringsAsFactors=F,fileEncoding = "utf-8")

str(tr_full_training_2)


set.seed(1)
split<-(.8) 
trainingRowIndex <- sample(1:nrow(tr_full_training),(split)*nrow(tr_full_training)) # row indices for training data
df_training <- tr_full[trainingRowIndex, ]  # model training data
df_testing  <- tr_full[-trainingRowIndex, ]   # test data

str(df_training)

names(df_training)


start.time <- Sys.time()
gbm_model <- gbm(logtotalTransactionRevenue ~ channelGrouping + 
                 visitNumber + browser + deviceCategory + 
                 isMobile + operatingSystem + city + continent + 
                 country + metro + region + 
                 subContinent + newVisits + pageviews + sessionQualityDim + 
                 timeOnSite + transactions + adContent + 
                 adwordsClickInfo.adNetworkType + adwordsClickInfo.gclId + adwordsClickInfo.isVideoAd + adwordsClickInfo.page + 
                 adwordsClickInfo.slot + campaign + isTrueDirect + keyword + 
                 medium + referralPath + source,
                 distribution="gaussian",
                 data=df_training,
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

RMSE <- function(m, o){
  sqrt(mean((m - o)^2))
}

