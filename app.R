library(shiny)
library(rtweet)
library(wordcloud)
library(tidyverse)
library(knitr)
library(wordcloud)
library(tm)
library(tidytext)
library(scales)
library(shinydashboard)
library(jsonlite)

creds <- read.csv("twitter.config")
creds$vars <- as.character(creds$vars)

token <- create_token(app = "jake learns data science", 
                      consumer_key = creds$vars[1], 
                      consumer_secret = creds$vars[2], 
                      access_token = creds$vars[3], 
                      access_secret = creds$vars[4])

header <- dashboardHeader(
    title = "Twitter Analysis", 
    
    tags$li(a(href = paste0('https://twitter.com/intent/tweet?text=Check%20out',
                            '%20this%20Twitter%20Analysis%20Shiny%20Dashboard%',
                            '20created%20by%20Jacob%20Rozran%20(@rozran00)&url=h',
                            'ttps%3a%2f%2fjakelearnsdatascience.shinyapps.io%2',
                            'ftwitter_shiny%2f'),
              icon("share-alt"),
              title = "Share this app on Twitter"),
            class = "dropdown"),
    
    tags$li(a(href = 'https://github.com/jrozra200/havertown_twitter',
              icon("github"),
              title = "Check out the code on Github"),
            class = "dropdown"),
    
    tags$li(a(href = 'https://www.jakelearnsdatascience.com',
              icon("bar-chart"),
              title = "Back to Jake Learns Data Science"),
            class = "dropdown")
)

sidebar <- dashboardSidebar(
    sidebarMenu( 
        id = "sidebar",
        menuItem("Twitter Query", tabName = "inputs", icon = icon("keyboard")),
        menuItem("Summary", tabName = "summary", icon = icon("sort-amount-down")),
        menuItem("Top Tweets", tabName = "top-tweets", icon = icon("user-tie")),
        menuItem("Sentiment", tabName = "sentiment", icon = icon("tachometer-alt")),
        menuItem("Word Cloud", tabName = "word-cloud", icon = icon("tachometer-alt"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "inputs",
            box(
                title = "Let's Search Twitter",
                
                width = 12,
                
                fluidRow(
                    column(8, textInput("search_string", "Topic to Search", "")),
                    column(4, textInput("results", "Max Number of Tweets to Get", 
                                        value = 100))
                ),
                
                helpText(paste0("Note: you cannot leave the search string (above) ",
                                "blank if you uncheck this box requesting location",
                                " details.")),
                checkboxInput("check_dist", 
                              paste0("Search a specific location? (below will be i",
                                     "gnored if unchecked)"),
                              value = TRUE),
                
                fluidRow(
                    column(4, textInput("lat", "Latitude", "39.9878")),
                    column(4, textInput("lon", "Longitude", "-75.3062")),
                    column(4, textInput("dist", "Distance (miles)", value = 2))
                )
            )
        ),
        
        tabItem(
            tabName = "summary",
            box(
                title = "Summary of Results",
                
                width = 12,
                
                fluidRow(
                    valueBoxOutput("searchString", width = 6),
                    valueBoxOutput("geocode", width = 6)
                ),
                
                fluidRow(
                    valueBoxOutput("numberTweets", width = 6),
                    valueBoxOutput("numberTweeters", width = 6)
                ),
                
                fluidRow(
                    valueBoxOutput("earliestTweet"),
                    valueBoxOutput("latestTweet"),
                    valueBoxOutput("range")
                )
            )
        ),
        
        tabItem(
            tabName = "top-tweets",
            
            box(
                title = "Most Favorited Tweet",
                
                width = 6,
                
                uiOutput("most_favorited_embed"),
                uiOutput("most_favorited_stats"),
                uiOutput("most_favorited_image")
            ),
            
            box(
                title = "Most Retweeted Tweet",
                
                width = 6,
                
                uiOutput("most_retweeted_embed"),
                uiOutput("most_retweeted_stats"),
                uiOutput("most_retweeted_image")
            ),
            
            box(
                title = "Top 10 Tweeters",
                
                width = 12,
                
                uiOutput("top_5_tweeters")
            )
        ),
        
        tabItem(
            tabName = "sentiment",
            
            box(
                title = "Sentiment Stats",
                
                width = 12,
                
                fluidRow(
                    valueBoxOutput("postive_tweets"),
                    valueBoxOutput("negative_tweets"),
                    valueBoxOutput("neutral_tweets")
                )    
            ),
            
            box(
                title = "Sentiment of Tweets",
                
                width = 12,
                
                plotOutput("sentiment", width = "100%", height = 600)
            )
        ),
        
        tabItem(
            tabName = "word-cloud",
            
            box(
                title = "Word Cloud of Tweeted Words",
                
                width = 12,
                height = 600,
                
                plotOutput("wordcloud", height = "600")
            )
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
    get_tweets <- reactive({
        if(input$check_dist == TRUE) {
            search_tweets(input$search_string, 
                          geocode = paste0(input$lat, ',', input$lon, ',', 
                                           input$dist, 'mi'),
                          type = "recent", n = as.numeric(input$results))
        } else {
            search_tweets(input$search_string, type = "recent", 
                          n = as.numeric(input$results))
        }
    })
    
    get_sentiment <- reactive({
        df_dat <- get_tweets()
        
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
            tweet_sent$perc_pos > tweet_sent$perc_neg  ~ "Positive",
            tweet_sent$perc_neg > tweet_sent$perc_pos ~ "Negative",
            1 == 1 ~ "Neutral"
        )
        
        tweet_sent$sentiment_value <- case_when(
            tweet_sent$perc_pos > tweet_sent$perc_neg  ~ tweet_sent$perc_pos,
            tweet_sent$perc_neg > tweet_sent$perc_pos ~ tweet_sent$perc_neg * -1,
            1 == 1 ~ 0
        )
        
        df_dat <- merge(df_dat, tweet_sent, by = "status_id")
        
        return(df_dat)
    })
    
    output$searchString <- renderValueBox({
        valueBox(
            ifelse(input$search_string != "", input$search_string, "<blank>"), 
            "Search String", icon = icon("search"),color = "blue"
        )
    })
    
    output$numberTweets <- renderValueBox({
        dat <- get_tweets()
        
        valueBox(
            length(unique(dat$status_id)), "Tweets", 
            icon = icon("send", lib = "glyphicon"), color = "blue"
        )
    })
    
    output$numberTweeters <- renderValueBox({
        dat <- get_tweets()
        
        valueBox(
            length(unique(dat$screen_name)), "Unique Twitter Handles", 
            icon = icon("at"), color = "blue"
        )
    })
    
    output$earliestTweet <- renderValueBox({
        dat <- get_tweets()
        
        earliest_tweet <- min(dat$created_at, na.rm = TRUE)
        attributes(earliest_tweet)$tzone <- "America/New_York"
        
        valueBox(
            earliest_tweet, "Date & Time for Oldest Tweet", 
            icon = icon("clock"), color = "blue"
        )
    })
    
    output$latestTweet <- renderValueBox({
        dat <- get_tweets()
        
        latest_tweet <- max(dat$created_at, na.rm = TRUE)
        attributes(latest_tweet)$tzone <- "America/New_York"
        
        valueBox(
            latest_tweet, "Date & Time for Newest Tweet", 
            icon = icon("clock"), color = "blue"
        )
    })
    
    output$range <- renderValueBox({
        dat <- get_tweets()
        
        earliest_tweet <- min(dat$created_at, na.rm = TRUE)
        attributes(earliest_tweet)$tzone <- "America/New_York"
        
        latest_tweet <- max(dat$created_at, na.rm = TRUE)
        attributes(latest_tweet)$tzone <- "America/New_York"
        
        dif <- latest_tweet - earliest_tweet
        
        valueBox(
            paste0(round(dif, 2), " ", attributes(dif)$units), 
            "Time Range of Tweets", 
            icon = icon("minus"), color = "blue"
        )
    })
    
    output$geocode <- renderValueBox({ 
        if(input$check_dist == TRUE){
            valueBox(
                paste0("≤ ", input$dist, " mi. of [lat: ", 
                       input$lat, ", lon: ", input$lon, "]"), 
                "Geocode", 
                icon = icon("map-marker-alt"), color = "blue"
            )
        } else {
            valueBox(
                "NA - No geocode inputted.", "Geocode", 
                icon = icon("map-marker-alt"), color = "blue"
            )
        }
    })
    
    output$most_favorited_tweet <- renderTable({
        dat <- get_tweets()
        
        tab <- as.data.frame(dat[dat$favorite_count == max(dat$favorite_count), 
                                 c("created_at", "screen_name", "text", 
                                   "favorite_count")])
        
        tab$favorite_count <- as.integer(tab$favorite_count)
        
        tab <- tab[1, ]
        
        fin <- data.frame(attr = c("Tweet Date", "Twitter Handle", "Tweet", 
                                   "Favorites"),
                          values = t(tab))
        names(fin) <- c("", "")
        
        fin
    })
    
    output$most_favorited_embed <- renderUI({
        dat <- get_tweets()
        
        twitterURL <- paste0("https://publish.twitter.com/oembed?url=", 
                             dat$status_url[dat$favorite_count == max(dat$favorite_count)][1])
        html_json <- read_json(twitterURL)
        
        HTML(html_json$html)
    })
    
    output$most_favorited_stats <- renderText({
        dat <- get_tweets()
        
        c('<p><img src="retweet.png" width = "20"> ', 
          dat$retweet_count[dat$favorite_count == max(dat$favorite_count)][[1]][1], 
          '<img src="heart.png" width = "35">',
          dat$favorite_count[dat$favorite_count == max(dat$favorite_count)][[1]][1], 
          '</p>')
    })
    
    output$most_favorited_image <- renderText({
        dat <- get_tweets()
        
        if(!is.na(dat$media_url[dat$favorite_count == max(dat$favorite_count)][[1]][1])){
            c('<img src="', 
              dat$media_url[dat$favorite_count == max(dat$favorite_count)][[1]][1], 
              '" width="100%">')    
        } else {
            "<p></p>"
        }
    })
    
    output$most_retweeted_embed <- renderUI({
        dat <- get_tweets()
        
        twitterURL <- paste0("https://publish.twitter.com/oembed?url=", 
                             dat$status_url[dat$retweet_count == max(dat$retweet_count)][1])
        html_json <- read_json(twitterURL)
        
        HTML(html_json$html)
    })
    
    output$most_retweeted_stats <- renderText({
        dat <- get_tweets()
        
        c('<p><img src="retweet.png" width = "20"> ', 
          dat$retweet_count[dat$retweet_count == max(dat$retweet_count)][[1]][1], 
          '<img src="heart.png" width = "35">',
          dat$favorite_count[dat$retweet_count == max(dat$retweet_count)][[1]][1], 
          '</p>')
    })
    
    output$most_retweeted_image <- renderText({
        dat <- get_tweets()
        
        if(!is.na(dat$media_url[dat$retweet_count == max(dat$retweet_count)][[1]][1])){
            c('<img src="', 
              dat$media_url[dat$retweet_count == max(dat$retweet_count)][[1]][1], 
              '" width="100%">')    
        } else {
            "<p></p>"
        }
    })
    
    output$top_5_tweeters <- renderText({
        dat <- get_tweets()
        
        tab <- dat %>% 
            group_by(screen_name) %>% 
            summarise(tweets = length(unique(status_id)),
                      followers = max(followers_count),
                      friends = max(friends_count),
                      account_create = max(account_created_at),
                      profile_pic = unique(profile_image_url))
        
        tab <- tab[order(tab$tweets, decreasing = TRUE), ]
        
        if(length(unique(dat$screen_name)) >= 10){
            tab <- tab[1:10, ]
        } else {
            tab <- tab[1:length(unique(dat$screen_name)), ]
        }
        
        output <- c('<html><head><style>table {border-collapse: collapse;width: 100%;}',
                    'td, th {border: 1px solid #dddddd;text-align: left;padding: 8px;}',
                    'tr:nth-child(even) {background-color: #dddddd;}</style></head>',
                    '<body><table><tr><th>Screen Name</th>',
                    '<th>Recent Tweets</th><th>Followers</th><th>Friends</th>',
                    '<th>Account Create Date</th><th>Profile Pic</th></tr>')
        
        for(i in 1:dim(tab)[1]){
            output <- c(output,
                        '<tr><td>', tab$screen_name[i], '</td>',
                        '<td>', tab$tweets[i], '</td>',
                        '<td>', tab$followers[i], '</td>',
                        '<td>', tab$friends[i], '</td>',
                        '<td>', as.character(as.Date(as.POSIXct(tab$account_create[i], origin="1970-01-01"))), '</td>',
                        '<td><img src="', tab$profile_pic[i], '" width="42"></td>',
                        '</tr>')
        }
        output <- c(output,
                    '</table></body></html>')
    })
    
    output$wordcloud <- renderPlot({
        dat <- get_tweets()
        
        wc <- Corpus(VectorSource(paste(dat$text, collapse = " ")))
        
        wc <- tm_map(wc, removePunctuation)
        wc <- tm_map(wc, function(x){removeWords(x, stopwords())})
        
        doc_mat <- TermDocumentMatrix(wc)
        mat <- as.matrix(doc_mat)
        vec <- sort(rowSums(mat), decreasing = TRUE)
        df <- data.frame(word = names(vec), freq = vec)
        
        wordcloud(df$word, df$freq, min.freq = 2, random.order = FALSE,
                  colors = "black", rot.per = 0.2, max.words = 100)
    })
    
    output$sentiment <- renderPlot({
        dat <- get_sentiment()
        
        ggplot(dat, aes(x = status_id, y = sentiment_value, group = sentiment, fill = sentiment)) + 
            geom_bar(stat = "identity") + 
            scale_y_continuous(label = percent_format()) + 
            scale_fill_manual(values = c("#FA8368", "gray", "#67ACFA")) + 
            ggtitle("Sentiment of Tweets") +
            xlab("Tweet") +
            ylab("Sentiment Value") +
            theme(panel.background = element_blank(), 
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(color = "grey"),
                  legend.position = "top", legend.text = element_text(size = 12),
                  legend.title = element_blank(), title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  axis.title = element_blank(), axis.text.x = element_blank(),
                  axis.ticks = element_blank())
    })
    
    output$postive_tweets <- renderValueBox({
        dat <- get_sentiment()
        
        valueBox(length(dat$sentiment[dat$sentiment == "Positive"]),
                 "Number of Positive Tweets",
                 icon = icon("thumbs-up"), color = "blue")
        
    })
    
    output$negative_tweets <- renderValueBox({
        dat <- get_sentiment()
        
        valueBox(length(dat$sentiment[dat$sentiment == "Negative"]),
                 "Number of Negative Tweets",
                 icon = icon("thumbs-down"), color = "blue")
        
    })
    
    output$neutral_tweets <- renderValueBox({
        dat <- get_sentiment()
        
        valueBox(length(dat$sentiment[dat$sentiment == "Neutral"]),
                 "Number of Neutral Tweets",
                 icon = icon("equals"), color = "blue")
        
    })
    
    output$sentiment_level <- renderText({
        dat <- get_sentiment()
        
        paste0("The average sentiment for \"", input$search_string, "\" is ",
               percent(mean(dat$sentiment_value)), " (positive values mean pos",
               "itive sentiment).")
    })
}

shinyApp(ui, server)