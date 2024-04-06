#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

premierdivision_teams <- unique(subset(Statsbomb, Competition == "Premier Division")$Team)
premierdivision_players <- unique(subset(Statsbomb, Competition == "Premier Division")$Name)

ui = fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yvar", "X-axis variable", 
                  choices = premierdivision_players), 
    ),  
    mainPanel(
      plotOutput(outputId = "xAPlot"))
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    y = "Expected Assists"
    
    ggplot(Skillcorner, aes_string(x = input$xvar, y = "OP xG Assisted")) +
      geom_col()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
