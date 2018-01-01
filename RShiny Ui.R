source('helper.R')
source('app.R')
require('shiny')
require('tidyverse')

ui <- fluidPage(
  titlePanel('Visualization of Risk Measures'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("select", h3("Distributions"), 
                  choices = list("Normal"='norm'),
                  selected = 'norm'),
      
      textOutput(outputId = "Area (Probablity) =")
    )
    
    ),
  
  mainPanel(
    plotOutput("distPlot", hover = hoverOpts(id = "plot_hover",
                                             delay = 100,
                                             clip=TRUE,
                                             nullOutside = TRUE,
                                             delayType = "throttle"))
  )
    
  )
  

server <- funtion(input, output){
  plot_data <- reactive()
  }