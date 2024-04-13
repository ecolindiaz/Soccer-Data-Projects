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

ui = fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis variable", 
                  choices = "players"), 
    ),  
    mainPanel(
      plotOutput(outputId = "Player Name"))
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    y = "Player Information "
    
    library(gridExtra)
    library(tidyverse)
    StatsbombPlayerInfo = Statsbomb %>%
      filter(Name == "Aaron Connolly")  %>%
      select(Name, Age, Height, Team, Nationality, Appearances, `Primary Position`, `Secondary Position`)
    
    table <- tableGrob(StatsbombPlayerInfo)
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
