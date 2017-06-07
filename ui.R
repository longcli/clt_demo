## ui.R ##


# normal
# exponential





library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Central Limit Theorem Demo"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      wellPanel('Common Paramters',
          
        sliderInput("in_ssize",
                    "Sample Size:",
                    min = 1,
                    max = 50,
                    value = 30),
        
        sliderInput("in_numsamples",
                    "Number of Samples:",
                    min = 100,
                    max = 10000,
                    value = 1000),
        
        radioButtons("in_histxlim", label = h3("Histogram x limits"), 
                     choices = list("Means Distribution" = 1, "Population Distribution" = 2), selected = 1)
        
      )
    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      hr(),
      h2('Normal Distribution'),
      uiOutput("ui_norm"),
      
      hr(),
      h2('Exponential Distribution'),
      uiOutput("ui_exp"),
      
      hr(),
      h2('Weibull Distribution'),
      tags$p('Shape parameter < 1 indicates that the failure rate decreases over time.'),
      tags$p('Shape parameter = 1 indicates that the failure rate is constant over time. (Exponential)'),
      tags$p('Shape parameter > 1 indicates that the failure rate increases with time.'),
      uiOutput("ui_weib"),
      
      hr(),
      h2('Uniform Distribution'),
      #tags$html(href="https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)"),
      uiOutput("ui_unif")
      
    )
  )
))
