library(shiny)
runApp(list(
  ui = fluidPage(
    plotOutput("plot1", height = "auto")
  ),
  
  
  
  server = function(input, output, session) {
    
    output$plot1 <- renderPlot({
      
      plot(cars)
      
    }, height = function() {session$clientData$output_plot1_width} / 1.5 )
  }
))