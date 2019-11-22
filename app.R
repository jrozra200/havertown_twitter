#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rtweet)
library(wordcloud)
library(tidyverse)
library(knitr)
library(wordcloud)
library(tm)
library(tidytext)
library(scales)

creds <- read.csv("twitter.config")
creds$vars <- as.character(creds$vars)

token <- create_token(app = "jake learns data science", 
                      consumer_key = creds$vars[1], 
                      consumer_secret = creds$vars[2], 
                      access_token = creds$vars[3], 
                      access_secret = creds$vars[4])

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Twitter Analysis"),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h1("Let's Search Twitter"),
            fluidRow(
                column(6, textInput("search_string", "Topic to Search", "")),
                column(6, textInput("results", "Max Number of Tweets to Get", 
                                    value = 100))
            ),
            
            helpText(paste0("Note: you cannot leave the search string (above) ",
                            "blank if you uncheck this box requesting location",
                            " details.")),
            checkboxInput("check_dist", 
                          paste0("Search a specific location? (belo",
                                 "w will be ignored if unchecked)"),
                          value = TRUE),
            
            fluidRow(
                column(4, textInput("lat", "Latitude", "39.9878")),
                column(4, textInput("lon", "Longitude", "-75.3062")),
                column(4, textInput("dist", "Distance (miles)", value = 2))
                ),
            
            actionButton("submit", "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h1("Summary"),
            textOutput("introduction1"),
            br(),
            textOutput("introduction2"),
            br(),
            textOutput("introduction3"),
            
            fluidRow(
                column(6, h3("Most Favorited Tweet"), 
                       tableOutput("most_favorited_tweet")),
                column(6, h3("Most Retweeted Tweet"),
                       tableOutput("most_retweeted"))
            ),
            
            h3("Twitter Sentiment"),
            textOutput("sentiment_level"),
            br(),
            plotOutput("sentiment", width = "100%", height = 600),
            
            fluidRow(
                column(4, h3("Top 10 Tweeters"),
                       tableOutput("top_5_tweeters")),
                column(8, h3("Wordcloud of Tweets"),
                       plotOutput("wordcloud", width = "100%", height = 600))
            )
        )
    )
)

# Define server logic required to draw a histogram
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
            tweet_sent$perc_pos >= tweet_sent$perc_neg  ~ "Positive",
            tweet_sent$perc_neg >= tweet_sent$perc_pos ~ "Negative",
            1 == 1 ~ "Neutral"
        )
        
        tweet_sent$sentiment_value <- case_when(
            tweet_sent$perc_pos >= tweet_sent$perc_neg  ~ tweet_sent$perc_pos,
            tweet_sent$perc_neg >= tweet_sent$perc_pos ~ tweet_sent$perc_neg * -1,
            1 == 1 ~ 0
        )
        
        df_dat <- merge(df_dat, tweet_sent, by = "status_id")
        
        return(df_dat)
    })
    
    output$introduction1 <- renderText({ 
        dat <- get_tweets()
        
        paste0("This dashboard searched twitter for the following search strin",
               "g:\"", input$search_string, "\". From this search, there were",
               " ", length(unique(dat$status_id)), " total tweets returned from ", 
               length(unique(dat$screenName)), " unique twitter handles.")
    })
    
    output$introduction2 <- renderText({ 
        dat <- get_tweets()
        
        earliest_tweet <- min(dat$created_at, na.rm = TRUE)
        attributes(earliest_tweet)$tzone <- "America/New_York"
        
        last_tweet <- max(dat$created_at, na.rm = TRUE)
        attributes(last_tweet)$tzone <- "America/New_York"
        
        dif <- last_tweet - earliest_tweet
        
        earliest_tweet <- format(earliest_tweet, "%B %d, %Y %I:%M:%S %p %Z")
        last_tweet <- format(last_tweet, "%B %d, %Y %I:%M:%S %p %Z")
        
        paste0("The tweets returned span ", round(dif, 2), " ", 
               attributes(dif)$units, "; from ", earliest_tweet, 
               " to ", last_tweet, ".")
    })
    
    output$introduction3 <- renderText({ 
        if(input$check_dist == TRUE){
            dat <- get_tweets()
            
            paste0("You have selected to return tweets from within ", input$dist, 
                   " miles of ", input$lat, " latitude and ", input$lon, 
                   " longitude.")    
        } else {
            paste0("You have not selected a specific geocode to search, so thi",
                   "s dashboard is searching all of twitter, geographically.")
        }
    })
    
    output$most_favorited_tweet <- renderTable({
        dat <- get_tweets()
        
        tab <- as.data.frame(dat[dat$favorite_count == max(dat$favorite_count), 
                   c("created_at", "screen_name", "text", "favorite_count", "url")])
        
        tab$favorite_count <- as.integer(tab$favorite_count)
        
        tab <- tab[1, ]
        
        fin <- data.frame(attr = c("Tweet Date", "Twitter Handle", "Tweet", 
                                   "Favorites", "URL"),
                          values = t(tab))
        names(fin) <- c("", "")
        
        fin
    })
    
    output$most_retweeted <- renderTable({
        dat <- get_tweets()
        
        tab <- as.data.frame(dat[dat$retweet_count == max(dat$retweet_count), 
                                 c("created_at", "screen_name", "text", 
                                   "retweet_count", "url")])
        
        tab$retweet_count <- as.integer(tab$retweet_count)
        
        tab <- tab[1, ]
        
        fin <- data.frame(attr = c("Tweet Date", "Twitter Handle", "Tweet", 
                                   "Retweets", "URL"),
                          values = t(tab))
        names(fin) <- c("", "")
        
        fin
    })
    
    output$top_5_tweeters <- renderTable({
        dat <- get_tweets()
        
        tab <- dat %>% 
            group_by(screen_name) %>% 
            summarise(tweets = length(unique(status_id)))
        
        tab <- tab[order(tab$tweets, decreasing = TRUE), ]
        
        if(length(unique(dat$screen_name)) >= 10){
            tab <- tab[1:10, ]
        } else {
            tab <- tab[1:length(unique(dat$screen_name)), ]
        }
        
        names(tab) <- c("Twitter Handle", "Tweets")
        
        tab
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
        
        df_dat <- dat[order(dat$created_at), ]
        
        ggplot(dat, aes(x = status_id, y = sentiment_value, group = sentiment, fill = sentiment)) + 
            geom_bar(stat = "identity") + 
            scale_y_continuous(label = percent_format()) + 
            scale_fill_manual(values = c("#FA8368", "#67ACFA")) + 
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
    
    output$sentiment_level <- renderText({
        dat <- get_sentiment()
        
        paste0("The average sentiment for \"", input$search_string, "\" is ",
               percent(mean(dat$sentiment_value)), " (positive values mean pos",
               "itive sentiment).")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
