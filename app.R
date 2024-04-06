library(readxl)

Skillcorner <- read_excel("Recruitment Data.xlsx", sheet = "Skillcorner")
Statsbomb <- read_excel("Recruitment Data.xlsx", sheet = "Statsbomb ")

library(shiny)
library(tidyverse)

ui = fluidPage(
  titlePanel("Player Distances"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yvar", "X-axis variable", 
                  choices = c("Position", "Team")), 
    ),  
    mainPanel(
      plotOutput(outputId = "distPlot"))
    )
  )



server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    y = "Distance"
    
    ggplot(Skillcorner, aes_string(x = input$xvar, y = "Goals")) +
      geom_col()
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
