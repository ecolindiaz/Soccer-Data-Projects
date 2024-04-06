server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    ggplot(Skillcorner, aes_string(x = input$xvar, y = "Distance")) +
      geom_col()
    
  })
  
}
