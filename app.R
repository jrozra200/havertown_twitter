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
            fluidRow(
                column(6, textInput("search_string", "Topic to Search", "")),
                column(6, textInput("results", "Max Number of Tweets to Get", 
                                    value = 25))
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
            textOutput("introduction3"),
            
            fluidRow(
                column(6, h3("Most Favorited Tweet"), 
                       tableOutput("most_favorited_tweet")),
                column(6, h3("Most Retweeted Tweet"),
                       tableOutput("most_retweeted"))
            ),
            
            fluidRow(
                column(6, h3("Top 10 Tweeters"),
                       tableOutput("top_5_tweeters")),
                column(6, h3("Wordcloud of Tweets"),
                       plotOutput("wordcloud", height = "400px"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_tweets <- reactive({
        if(input$check_dist == TRUE) {
            searchTwitter(input$search_string, 
                          geocode = paste0(input$lat, ',', input$lon, ',', 
                                           input$dist, 'mi'),
                          resultType = "recent", n = input$results)
        } else {
            searchTwitter(input$search_string, resultType = "recent", 
                          n = input$results)
        }
            
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
    
    output$introduction1 <- renderText({ 
        dat <- format_data()
        
        paste0("This dashboard searched twitter for the following search strin",
               "g:\"", input$search_string, "\". From this search, there were",
               " ", length(unique(dat$id)), " total tweets returned from ", 
               length(unique(dat$screenName)), " unique twitter handles.")
    })
    
    output$introduction2 <- renderText({ 
        dat <- format_data()
        
        earliest_tweet <- min(dat$created, na.rm = TRUE)
        attributes(earliest_tweet)$tzone <- "America/New_York"
        
        last_tweet <- max(dat$created, na.rm = TRUE)
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
            dat <- format_data()
            
            paste0("You have selected to return tweets from within ", input$dist, 
                   " miles of ", input$lat, " latitude and ", input$lon, 
                   " longitude.")    
        } else {
            paste0("You have not selected a specific geocode to search, so thi",
                   "s dashboard is searching all of twitter, geographically.")
        }
    })
    
    output$most_favorited_tweet <- renderTable({
        dat <- format_data()
        
        tab <- as.data.frame(dat[dat$favoriteCount == max(dat$favoriteCount), 
                   c("created", "screenName", "clean_text", "favoriteCount", "url")])
        
        tab$favoriteCount <- as.integer(tab$favoriteCount)
        
        tab <- tab[1, ]
        
        fin <- data.frame(attr = c("Tweet Date", "Twitter Handle", "Tweet", 
                                   "Favorites", "URL"),
                          values = t(tab))
        names(fin) <- c("", "")
        
        fin
    })
    
    output$most_retweeted <- renderTable({
        dat <- format_data()
        
        tab <- as.data.frame(dat[dat$retweetCount == max(dat$retweetCount), 
                                 c("created", "screenName", "clean_text", "retweetCount", "url")])
        
        tab$retweetCount <- as.integer(tab$retweetCount)
        
        tab <- tab[1, ]
        
        fin <- data.frame(attr = c("Tweet Date", "Twitter Handle", "Tweet", 
                                   "Retweets", "URL"),
                          values = t(tab))
        names(fin) <- c("", "")
        
        fin
    })
    
    output$top_5_tweeters <- renderTable({
        dat <- format_data()
        
        tab <- dat %>% 
            group_by(screenName) %>% 
            summarise(tweets = length(unique(id)))
        
        tab <- tab[order(tab$tweets, decreasing = TRUE), ]
        
        if(length(unique(dat$screenName)) >= 10){
            tab <- tab[1:10, ]
        } else {
            tab <- tab[1:length(unique(dat$screenName)), ]
        }
        
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
        df <- data.frame(word = names(vec), freq = vec)
        # df <- df[1:20, ]
        
        wordcloud(df$word, df$freq, min.freq = 2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
