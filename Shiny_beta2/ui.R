library(shiny)

shinyUI(pageWithSidebar(
  
  # Title
  headerPanel("First Roll in Bowling with Unknown p"),
  
  sidebarPanel(
    
    numericInput("mode",
                 "Most likely prior value of p:",
                 min=0,
                 max=1,
                 value=0.5,
                 step=0.01),
    
    numericInput("mean",
                 "Expected prior value of p:",
                 min=0,
                 max=1,
                 value=0.5,
                 step=0.01),
    
    checkboxInput("update", "Update prior beliefs with data from 1 roll of bowling"),
    
    conditionalPanel(condition = "input.update",
                     numericInput("pins",
                                  "Number of pins knocked down on first roll:",
                                  min=0,
                                  max=10,
                                  value=5,
                                  step=1)),
    
    conditionalPanel(condition = "input.update",
                     h4(textOutput("caption")))
    
  ),
  
  
  mainPanel(plotOutput("distPlot"))
  
  
  
))