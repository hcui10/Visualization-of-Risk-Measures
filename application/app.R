library("shiny")  # UI and reactive R expressions
library("plotly") # interavtie plot functions
source("helpers.R")

n.points <- 10000 # number of points to sample for x and p grids, hard-coded

ui <- fluidPage(
  # App title ----
  titlePanel("Visualization of Risk Measures"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Select dist ---
      selectInput(
        inputId = "dist", label = "Choose a Continuous dist",
        choices = c("Beta Distriution" = "beta", 
                    "Cauchy Distribution" = "cauchy", 
                    "Chi-Squared Distribution" = "chisq", 
                    "Exponential Distribution" = "exp", 
                    "F Distribution" = "f", 
                    "Gamma Distribution" = "gamma", 
                    "Log-Normal Distribution" = "lnorm", 
                    "Normal Distribution" = "norm", 
                    "Student's T Distribution" = "t", 
                    "Uniform Distribution" = "unif", 
                    "Weibull Distribution" = "weibull"), 
        selected = "norm"),  
      # Set parameters for dist ---
      uiOutput(outputId = "dist.param")
    ), 
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Plotly interactive plots (2 by 2)
      plotly::plotlyOutput(outputId = "VaR.cont.plot"),
      # test
      verbatimTextOutput("test", placeholder = TRUE)
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Output: render UI for setting param for distribution--- 
  output$dist.param <- renderUI({
    
    # If missing input, return to avoid error
    if (is.null(input$dist))
      return()
    
    switch(input$dist, 
           "beta" = tagList(
             numericInput(inputId = "beta.shape1", label = "Beta Param: Shape #1 (alpha)", 
                          value = 1 , min = 0.001), # >0
             numericInput(inputId = "beta.shape2", label = "Beta Param: Shape #2 (beta)", 
                          value = 1 , min = 0.001)  # >0
           ), 
           "cauchy" = tagList(
             numericInput(inputId = "cauchy.location", label = "Cauchy Param: Location (x0)", 
                          value = 0), # any real number
             numericInput(inputId = "cauchy.scale"   , label = "Cauchy Param: Scale (gamma)", 
                          value = 1, min = 0.001) # >0
           ), 
           "chisq" = tagList(
             numericInput(inputId = "chisq.df", label = "Chi-Squared Param: Degrees of Freedom (k)", 
                          value = 1, min = 1) # >0
           ), 
           "exp" = tagList(
             numericInput(inputId = "exp.rate", label = "Exponential Param: Rate (lambda)", 
                          value = 1, min = 0.001) # >0
           ), 
           "f" = tagList(
             numericInput(inputId = "f.df1", label = "F Param: Degrees of Freedom #1 (df1)", 
                          value = 1, min = 1), # >0
             numericInput(inputId = "f.df2", label = "F Param: Degrees of Freedom #2 (df2)", 
                          value = 1, min = 1) # >0
           ), 
           "gamma" = tagList(
             numericInput(inputId = "gamma.shape", label = "Gamma Param: Shape (alpha)", 
                          value = 1, min = 0.001), # >0
             numericInput(inputId = "gamma.rate" , label = "Gamma Param: Rate (beta)", 
                          value = 1, min = 0.001) # >0 
           ), 
           "lnorm" = tagList(
             numericInput(inputId = "lnorm.mean", label = "Log-Normal Param: Mean of Log(X) (mu)", 
                          value = 0), # any real number
             numericInput(inputId = "lnorm.sd"  , label = "Log-Normal Param: Standard Deviation of Log(X) (sigma)", 
                          value = 1, min = 0.001) # >0
           ), 
           "norm" = tagList(
             numericInput(inputId = "norm.mean", label = "Normal Param: Mean (mu)", 
                          value = 0), # any real number
             numericInput(inputId = "norm.sd"  , label = "Normal Param: Standard Deviation (sigma)", 
                          value = 1, min = 0.001) # >0
           ), 
           "t" = tagList(
             numericInput(inputId = "t.df", label = "Student's T Param: Degrees of Freedom (df)", 
                          value = 1, min = 1) # >0
           ), 
           "unif" = tagList(
             numericInput(inputId = "unif.min", label = "Uniform Param: Start", 
                          value = 0), # any real number
             numericInput(inputId = "unif.max", label = "Uniform Param: End", 
                          value = 1) # >x.unif.min
           ), 
           "weibull" = tagList(
             numericInput(inputId = "weibull.shape", label = "Weibull Param: Shape (k)", 
                          value = 1, min = 0.001), # >0
             numericInput(inputId = "weibull.scale", label = "Weibull Param: Scale (lambda)", 
                          value = 1 , min = 0.001) # >0
           ))
    
  })
  
  # Reactive Processing: get distribution parameters for X
  dist.params <- reactive({
    
    switch(input$dist, 
           "beta"    = list(shape1 = input$beta.shape1, 
                            shape2 = input$beta.shape2),
           "cauchy"  = list(location = input$cauchy.location, 
                            scale = input$cauchy.scale),
           "chisq"   = list(df = input$chisq.df),
           "exp"     = list(rate = input$exp.rate),
           "f"       = list(df1 = input$f.df1, 
                            df2 = input$f.df2),
           "gamma"   = list(shape = input$gamma.shape, 
                            rate = input$gamma.rate),
           "lnorm"   = list(meanlog = input$lnorm.meanlog, 
                            sdlog = input$lnorm.sdlog),
           "norm"    = list(mean = input$norm.mean, 
                            sd = input$norm.sd),
           "t"       = list(df = input$t.df),
           "unif"    = list(min = input$unif.min, 
                            max = input$unif.max),
           "weibull" = list(shape = input$weibull.shape, 
                            scale = input$weibull.scale)
    )
  })
  
  
  # Output: Plotly interactive plots (2 by 2)
  output$VaR.cont.plot <- plotly::renderPlotly({
    plot.data <- generate.data(input$dist, dist.params(), n.points)
    plotly_plot(plot.data)
  })
  
  
  # test
  output$test <- renderPrint({
    print(dist.params())
  })
  
}

# Run the app
shinyApp(ui, server)

