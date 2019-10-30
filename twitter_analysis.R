## JUST MESSING WITH TWITTER

library(twitteR)

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
