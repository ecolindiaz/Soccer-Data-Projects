library(readxl)

Skillcorner <- read_excel("Recruitment Data.xlsx", sheet = "Skillcorner")
Statsbomb <- read_excel("Recruitment Data.xlsx", sheet = "Statsbomb ")

library(shiny)
library(tidyverse)

ui= fluidPage(

)

server = function(input, output) {

   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
