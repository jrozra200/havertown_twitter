library(shiny)
library(shinydashboard)

header <- dashboardHeader(
    title = "Twitter Analysis", 
    
    tags$li(a(href = paste0('https://twitter.com/intent/tweet?text=Check%20out',
                            '%20this%20Twitter%20Analysis%20Shiny%20Dashboard%',
                            '20created%20by%20Jacob%20Rozran%20(@rozran00)&url=h',
                            'ttps%3a%2f%2fjakelearnsdatascience.shinyapps.io%2',
                            'ftwitter_shiny%2f'),
              icon("twitter"),
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
        menuItem("Inputs", tabName = "inputs", icon = icon("keyboard")),
        menuItem("Summary", tabName = "summary", icon = icon("sort-amount-down")),
        menuItem("Sentiment", tabName = "sentiment", icon = icon("tachometer-alt")),
        menuItem("Word Cloud", tabName = "word-cloud", icon = icon("tachometer-alt"))
    )
)

body <- dashboardBody(
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
        
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { 
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

shinyApp(ui, server)