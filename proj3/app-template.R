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
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")
data <- read.csv('u2-lyrics.csv')
dat <- dplyr::starwars


# ===============================================
# tokenization
# ===============================================


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("U2 lyrics analysis"),
  fluidRow(
    # replace with your widgets
    column(3,
           radioButtons(inputId = "widget_stopword", 
                        label = "Including Stopwords", 
                        choices = c("Yes" = "opt1",
                                    "No" = "opt2"), 
                        selected = "opt1")
    ),
    
    # replace with your widgets
    column(3,
           sliderInput(inputId = "widget_output_number",
                       label = "Number of outputs",
                       min = 1,
                       max = 20,
                       value = 1)
    ),
    
    # replace with your widgets
    column(3,
           dateRangeInput(inputId = "widget_date_range",
                          label = "Date range",
                          start = NULL,
                          end = NULL,
                          min = NULL,
                          max = NULL),
           actionButton(inputId = "widget_action",
                        label = "Set to Default")
           
    ),
    
    # replace with your widgets
    column(3,
           sliderInput(inputId = "binwidth",
                       label = "Binwidth",
                       min = 1,
                       max = 20,
                       value = 1),
           checkboxInput(inputId = "facets",
                         label = strong("Facet by letter"),
                         value = FALSE)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Frequency",
                       h3("Result"),
                       plotOutput("barplot"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("What kind of analysis2"),
                       plotOutput("histogram"),
                       hr(),
                       verbatimTextOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in barchart)
  dat_freq <- reactive({
    dat %>% group_by(sex) %>% count()
  })
  
  
  # ===============================================
  # Outputs for the first TAB (i.e. barchart)
  # ===============================================
  
  # code for barplot
  output$barplot <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat_freq(), aes(x = sex, y = n)) +
      geom_col()
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

