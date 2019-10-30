## JUST MESSING WITH TWITTER

library(twitteR)
library(wordcloud)
library(tidyverse)

creds <- read.csv("twitter.config")

setup_twitter_oauth(creds$vars[1], 
                    creds$vars[2], 
                    creds$vars[3], 
                    creds$vars[4])

dat <- searchTwitter('', geocode = '39.9878,-75.3062,2mi',
                     resultType = "recent", n = 1000)

df_dat <- data.frame()
for(tweet in 1:length(dat)){
    tmp <- as.data.frame(dat[[tweet]])
    
    df_dat <- rbind(df_dat, tmp)
}

tmp <- NULL

df_dat$clean_text <- gsub("https.*$", "", df_dat$text)
df_dat$url <- gsub("^.*(https.*$)", "\\1", df_dat$text)
df_dat$url <- ifelse(grepl("^https", df_dat$url), df_dat$url, "")

total_tweets <- length(unique(df_dat$id))

total_tweeters <- length(unique(df_dat$screenName))

top_5_tweeters <- df_dat %>% 
    group_by(screenName) %>% 
    summarise(tweets = length(unique(id)))

top_5_tweeters <- top_5_tweeters[order(top_5_tweeters$tweets, decreasing = TRUE), ]
top_5_tweeters <- top_5_tweeters[1:5, ]

top_tweet <- df_dat$clean_text[df_dat$favoriteCount == max(df_dat$favoriteCount)]
cat(top_tweet)
