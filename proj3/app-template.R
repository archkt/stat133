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


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("U2 lyrics analysis"),
  fluidRow(
    column(3,
           radioButtons(inputId = "widget_stopword", 
                        label = "Including Stopwords", 
                        choices = c("Yes" = "include",
                                    "No" = "not include"), 
                        selected = "include")
    ),
    
    column(3,
           sliderInput(inputId = "widget_output_number",
                       label = "Number of outputs",
                       min = 2,
                       max = 50,
                       value = 20)
    ),
    
    column(3,
           dateRangeInput(inputId = "widget_date_range",
                          label = "Date range",
                          start = NULL,
                          end = NULL,
                          min = min(data$year),
                          max = max(data$year),
                          startview = "decade"),
           
           actionButton(inputId = "widget_action",
                        label = "Set to Default")
           
    ),
    
    column(3,
           selectInput(inputId = "widget_select_album",
                       label = "Select Album",
                       choices = album_list,
                       selected = 'All')
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
    if (input$widget_stopword == "include"){
      partial_data = data %>% 
        unnest_tokens(output = word, input = lyrics)
    } else {
      partial_data = data %>% 
        unnest_tokens(output = word, input = lyrics) %>%
        anti_join(stop_words, by = "word")
    }
    

    
    partial_data = partial_data %>%
      count(word, sort = TRUE) %>%
      ungroup()
    
    #return
    partial_data
  })
  
  token_first_n <- reactive({
    
    #return
    token_data() %>%
      arrange(desc(n)) %>%
      slice_head(n = input$widget_output_number)
  })
  
  token_in_range <- reactive ({
    
  })
  
  
  dat_freq <- reactive({
    dat %>% group_by(sex) %>% count()
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({
    ggplot(data = token_first_n(), aes(x = reorder(word, -n), y = n)) +
      geom_col() +
      labs(title = paste0("Top ",input$widget_output_number," frequent words")) +
      xlab("word") + 
      ylab("count")
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    # replace the code below with your code!!!
    dat_freq()
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

