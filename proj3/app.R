# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Author: 
# Date:


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(shiny)
library(wordcloud)

# ===============================================
# Import data
# ===============================================
data <- read.csv('u2-lyrics.csv')


# ===============================================
# Initializing static data
distinct_album = c('All', unique(data$album))
temp = c(1:length(distinct_album))
album_list = setNames(as.list(distinct_album), distinct_album)

min_year = min(data$year)
max_year = max(data$year)

sentiment_score = function(sentiment_data) {
  
  pos_neg = sentiment_data %>%
    group_by(sentiment) %>%
    summarise(n = sum(n))
  
  pos = pos_neg[pos_neg$sentiment == 'positive',]$n
  neg  = pos_neg[pos_neg$sentiment == 'negative',]$n
  
  return(pos - neg)
}

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("U2 lyrics analysis"),
  fluidRow(
    column(3,
           radioButtons(inputId = "widget_stopword", 
                        label = "Including Stopwords", 
                        choices = c("Yes" = "(include stopwords)",
                                    "No" = "(not include stopwords)"), 
                        selected = "(include stopwords)")
    ),
    
    column(3,
           sliderInput(inputId = "widget_output_number",
                       label = "Number of outputs",
                       min = 2,
                       max = 50,
                       value = 20)
    ),
    
    column(3,
           numericInput(inputId = "widget_min_year",
                        label = "Year after(default)",
                        value = min_year,
                        min = min_year,
                        max = max_year,
                        step = 1),
           
           numericInput(inputId = "widget_max_year",
                        label = "Year before",
                        value = max_year,
                        min = min_year,
                        max = max_year,
                        step = 1),
           p(em(paste0("default: ", min_year, "-", max_year)))
           
    ),
    
    column(3,
           selectInput(inputId = "widget_select_album",
                       label = "Select Album",
                       choices = album_list,
                       selected = 'All'),
           p(em("RECOMMEND: Set year back to default"))
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Frequency Analysis",
                       h3("Result"),
                       plotOutput("word_frequency_graph"),
                       hr(),
                       dataTableOutput('word_frequency_table')),
              tabPanel("Sentiment Analysis", 
                       h3("Result"),
                       plotOutput("cloud"),
                       hr(),
                       dataTableOutput('sentiment_table'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # Lyrics token data by stopword input
  token_data <- reactive({
    # initialize return data
    partial_data = data
    
    # widget_stopword logic
    if (input$widget_stopword == "(include stopwords)"){
      partial_data = data %>% 
        unnest_tokens(output = word, input = lyrics)
    } else {
      partial_data = data %>% 
        unnest_tokens(output = word, input = lyrics) %>%
        anti_join(stop_words, by = "word")
    }
    
    # widget_year_range logic
    partial_data <- partial_data %>%
      filter(year >= input$widget_min_year) %>%
      filter(year <= input$widget_max_year)
    
    # widget_select_album logic
    if (input$widget_select_album == 'All'){
      
    } else {
      partial_data <- partial_data %>%
        filter(album == input$widget_select_album)
    }

    
    partial_data
  })
  
  token_freq <- reactive({
    # generate frequency table with applying widget_output_number logic
    freq_data = token_data() %>%
      count(word, sort = TRUE) %>%
      ungroup() %>%
      arrange(desc(n)) %>%
      slice_head(n = input$widget_output_number)
    
    freq_data
  })
  
  
  sentiment_data <- reactive({

    cloud_words <- token_data() %>%
      inner_join(sentiments, by = "word") %>%
      count(word, sentiment, sort = TRUE) %>%
      arrange(desc(n)) %>%
      slice_head(n = input$widget_output_number)
      
    cloud_words
  })
  
  sentiment_cloud <- reactive({
    cloud_data <- sentiment_data() %>%
      reshape2::acast(word ~ sentiment, value.var = "n", fill = 0)
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for word_frequency_graph
  output$word_frequency_graph <- renderPlot({
    ggplot(data = token_freq(), aes(x = reorder(word, n), y = n)) +
      geom_col() +
      labs(title = paste0("Top ", input$widget_output_number," frequent words for album: ", input$widget_select_album),
           subtitle = input$widget_stopword) +
      xlab("word") + 
      ylab("count") + 
      coord_flip()
  })
  
  # code for numeric summaries of frequencies
  output$word_frequency_table <- renderDataTable({
    token_freq()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for histogram
  output$cloud <- renderPlot({
    layout(matrix(1:2, nrow=2), heights=c(1,5))
    par(mar=rep(0,4))
    plot.new()
    text(x=0.1, y=0.5,
         labels=paste0('sentiment score for ', input$widget_select_album, ": ", sentiment_score(sentiment_data())),
         cex=1.5)
    comparison.cloud(sentiment_cloud(),
                     scale = c(5, 1.5),
                     colors = c("tomato", "turquoise3"),
                     title.size = 3)
  })
  
  # code for statistics
  output$sentiment_table <- renderDataTable({
    
    sentiment_data()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

