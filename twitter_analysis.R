library(rtweet)

creds <- read.csv("twitter.config")

creds$vars <- as.character(creds$vars)

token <- create_token(app = "jake learns data science", 
                      consumer_key = creds$vars[1], 
                      consumer_secret = creds$vars[2], 
                      access_token = creds$vars[3], 
                      access_secret = creds$vars[4])

dat <- search_tweets("", n = 10, geocode = '39.9878,-75.3062,2mi')

dat1 <- dat %>% select_if(~!is.list(.))


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
dat <- searchTwitter('rstats', resultType = "recent", n = 25)

df_dat <- data.frame()
for(tweet in 1:length(dat)){
    tmp <- as.data.frame(dat[[tweet]])
    
    df_dat <- rbind(df_dat, tmp)
}

dat <- NULL
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

library(stringr)
library(tidytext)
library(scales)

tweets <- df_dat %>%
    unnest_tokens(clean_text, text)

tweets <- tweets[, c("status_id", "clean_text")]
sent <- get_sentiments(lexicon = "bing")
tweets <- merge(tweets, sent, by.x = "clean_text", by.y = "word", all.x = TRUE)


tweet_sent <- tweets %>% 
    group_by(status_id) %>%
    summarise(positive = length(clean_text[!is.na(sentiment) & sentiment == "positive"]),
              negative = length(clean_text[!is.na(sentiment) & sentiment == "negative"]),
              neutral = length(clean_text[is.na(sentiment)])) %>% 
    mutate(perc_pos = positive / (positive + negative + neutral),
           perc_neg = negative / (positive + negative + neutral),
           perc_neut = neutral / (positive + negative + neutral))

tweet_sent$sentiment <- case_when(
    tweet_sent$perc_pos >= tweet_sent$perc_neg  ~ "Positive",
    tweet_sent$perc_neg >= tweet_sent$perc_pos ~ "Negative",
    1 == 1 ~ "Neutral"
)

tweet_sent$sentiment_value <- case_when(
    tweet_sent$perc_pos >= tweet_sent$perc_neg  ~ tweet_sent$perc_pos,
    tweet_sent$perc_neg >= tweet_sent$perc_pos ~ tweet_sent$perc_neg * -1,
    1 == 1 ~ 0
)

df_dat <- merge(df_dat, tweet_sent, by = "id")

df_dat <- df_dat[order(df_dat$created), ]

ggplot(df_dat, aes(x = id, y = sentiment_value, group = sentiment, fill = sentiment)) + 
    geom_bar(stat = "identity") + 
    scale_y_continuous(label = percent_format()) + 
    scale_fill_manual(values = c("#FA8368", "#67ACFA")) + 
    ggtitle("Sentiment of Tweets") +
    xlab("Tweet") +
    ylab("Sentiment Value") +
    theme(panel.background = element_blank(), 
          panel.grid.major = element_line(color = "grey"),
          legend.position = "top", legend.text = element_text(size = 8),
          legend.title = element_blank(),
          axis.title = element_text(size = 10), axis.text = element_text(size = 8),
          axis.title.y = element_blank(), axis.text.x = element_blank())
