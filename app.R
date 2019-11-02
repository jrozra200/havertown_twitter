#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(twitteR)
library(wordcloud)
library(tidyverse)
library(knitr)
library(wordcloud)
library(tm)

setup_twitter_oauth(creds$vars[1], 
                    creds$vars[2], 
                    creds$vars[3], 
                    creds$vars[4])

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("Twitter Analysis", align = "center")),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h1("Let's Search Twitter"),
            textInput("search_string", "Topic to Search", ""),
            sliderInput("results", "Max Number of Tweets to Get", 
                        min = 0, max = 1000, value = 25),
            checkboxInput("check_dist", 
                          paste0("Search a specific location? (belo",
                                 "w will be ignored if unchecked)"),
                          value = TRUE),
            fluidRow(
                column(6, textInput("lat", "Latitude", "39.9878")),
                column(6, textInput("lon", "Longitude", "-75.3062"))
                ),
            sliderInput("dist", "Distance (miles)", 
                        min = 0, max = 10, value = 2),
            actionButton("submit", "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2(textOutput("search_string")),
            h3(textOutput("total_tweets")),
            h3(textOutput("total_tweeters")),
            h3(textOutput("earliest_tweet")),
            h3(textOutput("latest_tweet")),
            h3("Most Favorited Tweet"),
            tableOutput("most_favorited_tweet"),
            h3("Most Retweeted Tweet"),
            tableOutput("most_retweeted"),
            tableOutput("top_5_tweeters"),
            plotOutput("wordcloud")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_tweets <- reactive({
        searchTwitter(input$search_string, 
                      geocode = ifelse(input$check_dist == TRUE, 
                                       paste0(input$lat, ',', input$lon, ',', 
                                              input$dist, 'mi'),
                                       ''),
                      resultType = "recent", n = input$results)
        })
    
    format_data <- reactive({
        dat <- get_tweets()
        
        df_dat <- data.frame()
        for(tweet in 1:length(dat)){
            tmp <- as.data.frame(dat[[tweet]])
            
            df_dat <- rbind(df_dat, tmp)
        }
        
        tmp <- NULL
        
        df_dat$clean_text <- gsub("https.*$", "", df_dat$text)
        df_dat$url <- gsub("^.*(https.*$)", "\\1", df_dat$text)
        df_dat$url <- ifelse(grepl("^https", df_dat$url), df_dat$url, "")
        df_dat$twitter_client <- gsub("</a>$", "", df_dat$statusSource)
        df_dat$twitter_client <- gsub("<.*\">", "", df_dat$twitter_client)
        
        return(df_dat)
    })
    
    output$search_string <- renderText({ 
        paste0("Search String: \"", input$search_string, "\"")
    })
    
    output$total_tweets <- renderText({ 
        dat <- format_data()
        paste0("Total Tweets: ", length(unique(dat$id)))
    })
    
    output$total_tweeters <- renderText({ 
        dat <- format_data()
        paste0("Total Tweeters (unique twitter handles): ", length(unique(dat$screenName)))
    })
    
    output$earliest_tweet <- renderText({ 
        dat <- format_data()
        
        earliest_tweet <- min(dat$created, na.rm = TRUE)
        attributes(earliest_tweet)$tzone <- "America/New_York"
        
        earliest_tweet <- format(earliest_tweet, "%B %d, %Y %I:%M:%S %p %Z")
        
        paste0("Earliest Tweet: ", earliest_tweet)
    })
    
    output$latest_tweet <- renderText({ 
        dat <- format_data()
        
        last_tweet <- max(dat$created, na.rm = TRUE)
        attributes(last_tweet)$tzone <- "America/New_York"
        
        last_tweet <- format(last_tweet, "%B %d, %Y %I:%M:%S %p %Z")
        
        paste0("Latest Tweet: ", last_tweet)
    })
    
    output$most_favorited_tweet <- renderTable({
        dat <- format_data()
        
        tab <- as.data.frame(dat[dat$favoriteCount == max(dat$favoriteCount), 
                   c("created", "screenName", "clean_text", "favoriteCount", "url")])
        
        tab$favoriteCount <- as.integer(tab$favoriteCount)
        
        names(tab) <- c("Tweet Date", "Twitter Handle", "Tweet", "Favorites", "URL")
        
        tab
    })
    
    output$most_retweeted <- renderTable({
        dat <- format_data()
        
        tab <- as.data.frame(dat[dat$retweetCount == max(dat$retweetCount), 
                                 c("created", "screenName", "clean_text", "retweetCount", "url")])
        
        tab$retweetCount <- as.integer(tab$retweetCount)
        
        names(tab) <- c("Tweet Date", "Twitter Handle", "Tweet", "Retweets", "URL")
        
        tab
    })
    
    output$top_5_tweeters <- renderTable({
        dat <- format_data()
        
        tab <- dat %>% 
            group_by(screenName) %>% 
            summarise(tweets = length(unique(id)))
        
        tab <- tab[order(tab$tweets, decreasing = TRUE), ]
        tab <- tab[1:5, ]
        
        names(tab) <- c("Twitter Handle", "Tweets")
        
        tab
    })
    
    output$wordcloud <- renderPlot({
        dat <- format_data()
        
        wc <- Corpus(VectorSource(paste(dat$clean_text, collapse = " ")))
        
        wc <- tm_map(wc, removePunctuation)
        wc <- tm_map(wc, function(x){removeWords(x, stopwords())})
        
        doc_mat <- TermDocumentMatrix(wc)
        mat <- as.matrix(doc_mat)
        vec <- sort(rowSums(mat), decreasing = TRUE)
        df <- data.frame(word = names(v), freq = v)
        df <- df[1:20, ]
        
        wordcloud(df$word, df$freq, min.freq = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
