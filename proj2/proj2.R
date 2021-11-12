library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  
  titlePanel("Proj2"),
  
  fluidRow(
    # Initial portfolio
    column(3,
           numericInput(inputId = "balance",
                        label = "Initial portfolio:",
                        value = 1000000,
                        step = 1000),
           sliderInput(inputId = "age",
                       label = "Retirement Age:",
                       min = 30,
                       max = 100,
                       value = 60),
           numericInput(inputId = "withdrawal",
                       label = "Withdrawal rate(%)",
                       min = 0,
                       max = 100,
                       value = 4,
                       step = 1)
    ),
    
    # 
    column(3,
           sliderInput(inputId = "annual_return",
                       label = "Avg annual return(%)",
                       min = 0,
                       max = 100,
                       value = 10),
           sliderInput(inputId = "return_volatility",
                       label = "Avg return volatility(%)",
                       min = 0,
                       max = 100,
                       value = 18)
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(3,
           sliderInput(inputId = "annual_inflation",
                       label = "Avg annual inflation(%)",
                       min = 0,
                       max = 100,
                       value = 3),
           sliderInput(inputId = "inflation_volatility",
                        label = "Avg inflation volatility(%)",
                        min = 0,
                        max = 100,
                        value = 3.5)
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           sliderInput(inputId = "num_simulation",
                       label = "Number of simulations:",
                       min = 0,
                       max = 100,
                       value = 50),
           numericInput(inputId = "seed",
                        label = "Random seed:",
                        value = 12345)
    )
  ),
  
  hr(),
  h4('Portfolio balance over years'),
  plotOutput('timeline'),
  
  hr(),
  h4('Overall statistics'),
  verbatimTextOutput('table')
)


server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  dat <- reactive({
    set.seed(input$seed)
    
    simulations = as.list(1:input$num_simulation)
    names(simulations) = paste0("sim", 1:input$num_simulation)
    'interest_rate = rnorm(n = input$num_simulation,
                          mean = input$annual_return,
                          sd = input$return_volatility)
    inflation_rate = rnorm(n = input$num_simulation,
                           mean = input$annual_inflation,
                           sd = input$inflation_volatility)'
    amount_withdraw = input$balance * input$withdrawal * 0.01
    years = 100-input$age
    
    for (i in 1:input$num_simulation) {
      simulation = c(1:years+1)
      simulation[1] = input$balance
      for (j in 1:years+1) {
        interest_rate = rnorm(n = 1,
                              mean = input$annual_return,
                              sd = input$return_volatility)
        inflation_rate = rnorm(n = 1,
                               mean = input$annual_inflation,
                               sd = input$inflation_volatility)
        simulation[j] = simulation[j-1] * (1 + interest_rate/100) - amount_withdraw * (1 + inflation_rate/100)
      }
      
      simulations[[i]] = simulation
    }
    
    raw_data = data.frame(simulations)
    raw_data$year = 0:years
    
    pivot_longer(
      raw_data,
      cols = starts_with("sim"),
      names_to = "simulation",
      values_to = "amount"
    )
  })
  
  
  # code for graph
  # (e.g. reactive data frame used for graphing purposes)
  output$timeline <- renderPlot({
    # replace the code below with your code!!!
      ggplot(data = dat(), aes(x = year, y = amount, group = simulation)) +
      geom_point(aes(color = simulation)) + 
      geom_line(aes(color = simulation)) + 
      theme_minimal()
  })
  
  
  # code for statistics
  output$table <- renderPrint({
    # replace the code below with your code!!!
    summary(dat())
  })
  
  
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)


