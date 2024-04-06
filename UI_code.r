UI Code

fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "variable", 
                  choices = c()), 
    ),  
    mainPanel(
      plotOutput(outputId = "___Plot"))
    )
  )
