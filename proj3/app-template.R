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
                       plotOutput("barplot"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Sentiment Analysis", 
                       h3("Result"),
                       plotOutput("histogram"),
                       hr(),
                       verbatimTextOutput('table2'))
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

    # widget_output_number logic
    partial_data <- partial_data %>%
      count(word, sort = TRUE) %>%
      ungroup() %>%
      arrange(desc(n)) %>%
      slice_head(n = input$widget_output_number)
    
    #return
    partial_data
  })
  
  token_first_n <- reactive({
    
    #return
    token_data() %>%
      arrange(desc(n)) %>%
      slice_head(n = input$widget_output_number)
  })
  
  token_year_range <- reactive ({
    token_data() %>%
      filter(year >= input$widget_min_year) %>%
      filter(year <= input$widget_max_year)
  })
  
  
  dat_freq <- reactive({
    dat %>% group_by(sex) %>% count()
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({
    ggplot(data = token_data(), aes(x = reorder(word, -n), y = n)) +
      geom_col() +
      labs(title = paste0("Top ", input$widget_output_number," frequent words for album: ", input$widget_select_album),
           subtitle = input$widget_stopword) +
      xlab("word") + 
      ylab("count") + 
      coord_flip()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    token_data()
  })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  # code for histogram
  output$histogram <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat, aes(x = height)) +
      geom_histogram(binwidth = 20)
  })
  
  # code for statistics
  output$table2 <- renderPrint({
    # replace the code below with your code!!!
    summary(dat$height)
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

