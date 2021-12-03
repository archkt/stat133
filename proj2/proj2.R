library(tidyverse)
library(shiny)
library(ggplot2)
library(rsconnect)

quantile10 = c()
quantile50 = c()
quantile90 = c()

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
                       max = 99,
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
                        value = 12345),
           checkboxGroupInput(inputId = "quantile",
                              label="Show quantile",
                              choices = list("q10" = 1, "q50" = 2, "q90" = 3))
    )
  ),
  
  hr(),
  h4('Portfolio balance over years'),
  plotOutput('timeline'),
  
  hr(),
  h4('Overall statistics'),
  verbatimTextOutput('table'),
  
  hr(),
  h4('Quantiles'),
  tabsetPanel(
    tabPanel("q10", verbatimTextOutput("q10")), 
    tabPanel("q50", verbatimTextOutput("q50")), 
    tabPanel("q90", verbatimTextOutput("q90"))
  )
)


server <- function(input, output) {
  
  dat <- reactive({
    set.seed(input$seed)
    
    simulations = as.list(1:input$num_simulation)
    names(simulations) = paste0("sim", 1:input$num_simulation)

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
      
      print(simulation)
      simulations[[i]] = simulation
    }
    
    raw_data = data.frame(simulations)
    raw_data$year = 0:years
    
    mat = t(as.matrix(raw_data[,1:input$num_simulation]))
    
    for(i in 0:years+1) {
      year_balance = as.vector(mat[,i])
      quantile10[i] = quantile(year_balance, 0.1)
      quantile50[i] = quantile(year_balance, 0.5)
      quantile90[i] = quantile(year_balance, 0.9)
    }
    
    raw_data = raw_data %>%
      add_column(q10=quantile10)
    raw_data = raw_data %>%
      add_column(q50=quantile50)
    raw_data = raw_data %>%
      add_column(q90=quantile90)


    pivot_longer(
      raw_data,
      cols = starts_with("sim") | starts_with("q"),
      names_to = "simulation",
      values_to = "amount"
    )
  })
  
  # code for graph
  output$timeline <- renderPlot({
<<<<<<< HEAD
    # replace the code below with your code!!!
      ggplot(data = dat(), aes(x = year, y = amount/input$balance, group = simulation)) +
      labs(x= 'Years till reaching age 100', y='Portfolio balance (millions)') +
      geom_point(aes(color = simulation)) + 
      geom_line(aes(color = simulation)) + 
      theme_minimal()
=======
    g = ggplot(data = dat(), aes(x = year, y = amount/1000000, group = simulation)) +
      labs(x = 'year till 100', y = 'amount in Million') +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 1) + 
      geom_line(aes(color = simulation))
    
    if (is.null(input$quantile)) {
      g = g + geom_line(aes(color = simulation))
    }
    if ('1' %in% input$quantile) {
      q10 = filter(dat(), simulation == 'q10')
      g = g + geom_line(data=q10, aes(x = year, y = amount/1000000), color = 'black', size = 1)
    }
    if ('2' %in% input$quantile) {
      q50 = filter(dat(), simulation == 'q50')
      g = g + geom_line(data=q50, aes(x = year, y = amount/1000000), color = 'black', size = 1)
    }
    if ('3' %in% input$quantile) {
      q90 = filter(dat(), simulation == 'q90')
      g = g + geom_line(data=q90, aes(x = year, y = amount/1000000), color = 'black', size = 1)
    }
    
    
    g + theme_minimal()
>>>>>>> 7267beef3446aa5387456a9ffa31bedb457175cf
  })
  
  
  # code for statistics
  output$table <- renderPrint({
    summary(dat())
    
  })
  
  output$q10 <- renderPrint({
    summary(filter(dat(), simulation == 'q10'))
  })
  output$q50 <- renderPrint({
    summary(filter(dat(), simulation == 'q50'))
  })
  output$q90 <- renderPrint({
    summary(filter(dat(), simulation == 'q90'))
  })

  
  
  
}


shinyApp(ui = ui, server = server)


