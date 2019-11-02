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

earliest_tweet <- min(df_dat$created, na.rm = TRUE)
attributes(earliest_tweet)$tzone <- "America/New_York"

last_tweet <- max(df_dat$created, na.rm = TRUE)
attributes(last_tweet)$tzone <- "America/New_York"

library(wordcloud)
library(tm)

crude <- Corpus(VectorSource(paste(df_dat$clean_text, collapse = " ")))

crude <- tm_map(crude, removePunctuation)
crude <- tm_map(crude, function(x){removeWords(x, stopwords())})

tdm <- TermDocumentMatrix(crude)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(d$word, d$freq, min.freq = 2)

#A bigger cloud with a minimum frequency of 2
wordcloud(d$word,d$freq,c(8,.3),2)

#Now lets try it with frequent words plotted first
wordcloud(d$word,d$freq,c(8,.5),2,,FALSE,.1)
