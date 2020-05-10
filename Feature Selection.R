###Loading required libraries
library(plyr)
library(tidyverse)
library(jsonlite)
library(lubridate)

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

length(unique(tr0$fullVisitorId))
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
tr_full <- parse(tr0)
tr_full <- as.data.frame(tr_full)
tr_full$totalTransactionRevenue <- as.numeric(tr_full$totalTransactionRevenue)
tr_full <- tr_full %>% select(-transactionRevenue)

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
tr_full_re <- tr_full
tr_full_re$totalTransactionRevenue <- tr_full_re$totalTransactionRevenue/10^6
write.csv(tr_full_re,file="D:\\MingziTian\\BA\\Project 3 team 4\\train_yes_v8.csv",quote=F,row.names=F)
tr_full_2 <- read.csv("D:\\MingziTian\\BA\\Project 3 team 4\\train_yes_v8.csv",header=T,stringsAsFactors=F)
#build lm model
#tr_lm <- lm(totalTransactionRevenue~.,data=tr_full)







#########################################################################################################
###Defining useful columns
good_cols = c("channelGrouping","date","fullVisitorId","visitId","visitNumber","visitStartTime","browser","deviceCategory",
              "isMobile","operatingSystem","city","continent","country","metro","networkDomain","region","subContinent",                              
              "hits","newVisits","pageviews","sessionQualityDim","timeOnSite","totalTransactionRevenue","transactionRevenue",
              "transactions","adContent","adwordsClickInfo.adNetworkType", "adwordsClickInfo.gclId","adwordsClickInfo.isVideoAd",
              "adwordsClickInfo.page", "adwordsClickInfo.slot","campaign","isTrueDirect","keyword","medium","referralPath","source")

###Combining tables and convertion to dataframe
tr_full = rbind(tr_full[c(good_cols)], te[c(good_cols)])
tr_full = as.data.frame(tr_full)

###date as ymd-date, part of columns as integers
tr_full$date = ymd(tr_full$date)
for (i in c(18:26,31))
  tr_full[,i] = as.integer(tr_full[,i])

###if transactionRevenue is NA, it means 0
tr_full$transactionRevenue = ifelse(is.na(tr_full$transactionRevenue) == TRUE, 0, tr_full$transactionRevenue)
mz_train <- tr_full[tr_full$date<="2018-08-01",]
mz_test <- tr_full[tr_full$date>"2018-08-01",]
write.csv(mz_train,file="C:\\Users\\tianm\\OneDrive\\??????\\Shortcuts\\google_train_v5.csv",quote=F,row.names=F)
write.csv(mz_test,file="C:\\Users\\tianm\\OneDrive\\??????\\Shortcuts\\google_test_v5.csv",quote=F,row.names=F)
test <- read.csv("C:\\Users\\tianm\\OneDrive\\??????\\Shortcuts\\google_train_v5.csv",quote="",header=T)
IsDate <- function(mydate, date.format="%y-%M-%D"){
  tryCatch(!is.na(as.Date(mydate,date.format)),
           error <- function(err){FALSE})
}
test <- test[test$date]



############################################################################ method 2 from Kaggle
unsnake <- . %>%
  str_replace_all(c("\\[\\]" = "[{}]", # empty element must contain dictionary
                    "^\\[|\\]$" = "", # remove initial and final brackets
                    "(\\[|\\{|, |: |', )'" = "\\1\"", # open single- to double-quote (on key or value)
                    "'(\\]|\\}|: |, )" = '\"\\1')) # close quote
parse_json <- . %>%
  str_replace_all(c("\"[A-Za-z]+\": \"not available in demo dataset\"(, )?" = "",
                    ", \\}" = "}")) %>% # if last property in list was removed
  paste(collapse = ",") %>% paste("[", ., "]") %>% # As fromJSON() isn't vectorised
  fromJSON(., flatten = TRUE)

parse2 <- . %>% 
  bind_cols(parse_json(.$device)) %>%
  bind_cols(parse_json(.$geoNetwork)) %>% 
  bind_cols(parse_json(.$trafficSource)) %>% 
  bind_cols(parse_json(.$totals)) %>%
  bind_cols(parse_json(unsnake(.$customDimensions))) %>%
  bind_cols(parse_json(unsnake(.$hits))) %>%
  select(-device, -geoNetwork, -trafficSource, -totals, -customDimensions,-hits)

message("Data Parsing")
tr_full <- parse2(tr2)
te <- parse(te0)
########################################################################################


