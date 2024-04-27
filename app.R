load("Skillcorner.RDS")

library(shiny)
library(tidyverse)
library(fmsb)
library(data.table)

MLS = c("Columbus Crew Soccer Club", "Vancouver Whitecaps FC",
                "Football Club Cincinnati", "Los Angeles FC",
                "Orlando City Soccer Club", "Montreal Impact",
                "Charlotte FC", "Minnesota United Football Club",
                "Club Internacional de Fútbol Miami",
                "Atlanta United FC","Atlanta United FC II",
                "Los Angeles Galaxy II","New York Red Bulls II",
                "St. Louis City Soccer Club", 'LA Galaxy', 'Seattle Sounders',
                'Colorado Rapids', 'Houston Dynamo', 'Austin FC',
                'Toronto FC', 'New York City FC', 'Sporting Kansas City',
                'FC Dallas', 'Philadelphia Union', 'Chicago Fire',
                'Nashville SC', 'DC United', 'Portland Timbers',
                'Real Salt Lake', 'New England Revolution', 
                'New York Red Bulls', 'San Jose Earthquakes')

MLS = data.table(MLS)

USL = c('Colorado Springs Switchbacks FC','El Paso Locomotive FC', 
        'Las Vegas Lights FC','Memphis 901 FC', 'Monterey Bay FC',
        'New Mexico United','Oakland Roots SC', 'Orange County SC', 
        'Phoenix Rising FC','Sacramento Republic FC','San Antonio FC',
        'FC Tulsa','Birmingham Legion FC','Charleston Battery', 
        'Detroit City FC','Hartford Athletic','Indy Eleven','Loudoun United FC',
        'Louisville City FC','Miami FC','North Carolina FC',
        'Pittsburgh Riverhounds FC','Rhode Island FC','Tampa Bay Rowdies',
        'Rio Grande Valley FC Toros','San Diego Loyal SC')

USL = data.table(USL)


Swedish_Allsvenskan = c(  'Malmo FF', 'AIK','BK Hacken', 'GAIS',
                          'Halmstads BK', 'Brommapojkarna','Djurgardens IF',
                          'Mjällby AIF','IK Sirius', 'Helsingborgs IF',
                          'IFK Norrköping', 'Hammarby IF','IFK', 
                          'IF Elfsborg','IFK Värnamo', 'Vasteras SK',
                          'Kalmar FF', 'Ostersunds FK', 'IFK Göteborg', 
                          'Varbergs BoIS FC','GIF Sundsvall', 'Degerfors IF')

Swedish_Allsvenskan = data.table(Swedish_Allsvenskan)

Norwegian_Eliteserien = c('FK Bodo/Glimt','Molde FK','SK Brann','Rosenborg BK',
                          'Strømsgodset IF', 'Kristiansund BK', 'Odds BK', 
                          'FK Haugesund', 'Lillestrom SK', 'Fredrikstad',
                          'KFUM Oslo', 'Viking FK','Sandefjord Fotball', 
                          'Sarpsborg','Hamarkameratene Fotball', 'Tromsø IL',
                          'Aalesunds FK', 'FK Jerv', 'Valerenga IF Oslo')

Norwegian_Eliteserien = data.table(Norwegian_Eliteserien)

Skillcorner = Skillcorner %>%
  mutate(Competition = case_when(
    Team %in% MLS$MLS ~ "MLS",
    Team %in% USL$USL ~ "USL",
    Team %in% Swedish_Allsvenskan$Swedish_Allsvenskan 
    ~ "Swedish Allsvenskan",
    Team %in% Norwegian_Eliteserien$Norwegian_Eliteserien 
    ~ "Norwegian Eliteserien",
    TRUE ~ "Other")) %>%
  mutate_all(~replace_na(., 0))

rm(MLS, USL, Swedish_Allsvenskan, Norwegian_Eliteserien)

calculate_percentile = function(data) {
data %>%
  mutate(Distance90 = (Distance/`Minutes Played`) * 90,
         DistanceIP = (`Distance TIP`/`Minutes Played TIP`) * 90,
         DistanceOP = (`Distance OTIP`/`Minutes Played OTIP`) * 90,
         HSR90 = (`HSR Distance`/`Minutes Played`) * 90,
         HSRIP = (`HSR Distance TIP`/`Minutes Played TIP`) * 90,
         HSROP = (`HSR Distance OTIP`/`Minutes Played OTIP`) * 90,
         CountSprint = (`Count Sprint`/`Minutes Played`) * 90,
         CountSprintIP = (`Count Sprint TIP`/`Minutes Played TIP`) * 90,
         CountSprintOP = (`Count Sprint OTIP`/`Minutes Played OTIP`) * 90,
         HighAcceleration = 
           (`Count High Acceleration`/`Minutes Played`) * 90,
         HighAccelerationIP = 
           (`Count High Acceleration TIP`/`Minutes Played TIP`) * 90,
         HighAccelerationOP = 
           (`Count High Acceleration OTIP`/`Minutes Played OTIP`) * 90) %>%
  group_by(Player) %>%
  mutate(Minutes = mean(`Minutes Played`),
         Distance90 = mean(Distance90),
         DistanceIP = mean(DistanceIP),
         DistanceOP = mean(DistanceOP),
         HSR90 = mean(HSR90),
         HSRIP = mean(HSRIP),
         HSROP = mean(HSROP),
         Sprints = mean(CountSprint),
         SprintsIP = mean(CountSprintIP),
         SprintsOP = mean(CountSprintOP),
         HighAcceleration = mean(HighAcceleration),
         HighAccelerationIP = mean(HighAccelerationIP),
         HighAccelerationOP = mean(HighAccelerationOP),
         MaxSpeed = mean(`Max Speed`),
         PSV = mean(`PSV-99`)) %>%
  ungroup() %>%
  summarise(Player = Player,
            'Minutes Played'  = percent_rank(Minutes),
            'Distance' = percent_rank(Distance90),
            'Distance TIP' = percent_rank(DistanceIP),
            'Distance OTIP' = percent_rank(DistanceOP),
            'High Speed Running' = percent_rank(HSR90),
            'High Speed Running TIP' = percent_rank(HSRIP),
            'High Speed Running OTIP' = percent_rank(HSROP),
            'Sprints' = percent_rank(Sprints),
            'Sprints TIP' = percent_rank(SprintsIP),
            'Sprints OTIP' = percent_rank(SprintsOP),
            'High Accelerations' = percent_rank(HighAcceleration),
            'High Accelerations TIP' = percent_rank(HighAccelerationIP),
            'High Accelerations OTIP' = percent_rank(HighAccelerationOP),
            'Max Speed' = percent_rank(MaxSpeed),
            'PSV-99' = percent_rank(PSV)) %>%
  group_by(Player)}

ui = fluidPage(
  titlePanel("Player Profiles"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
      uiOutput('league'),
      uiOutput('team'),
      uiOutput('player'),
      plotOutput("percentilePlot")
      )
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("radarPlot")),
        column(6, plotOutput("distancePlot"))
      ),
      
      fluidRow(
        column(6, plotOutput("distancehalvesPlot")),
        column(6, plotOutput("sprintsPlot"))
      )
    )
  )
)


server <- function(input, output) {
  
  output$league = renderUI ({
    
    selectInput('league', 'Select League', 
                choices = unique(Skillcorner$Competition))
      })
  
  output$team = renderUI ({
    
    selectInput('team', 'Select Team',
                choices = Skillcorner %>% filter(Competition == input$league) %>% 
                  select(Team) %>% unique())
  })
  
  output$player = renderUI ({
    
    selectInput('player', 'Select Player',
                choices = Skillcorner %>% filter(Team == input$team) %>% 
                  select(Player) %>% unique())
    
  })
  
  output$percentilePlot = renderPlot({
    
    
percentile = calculate_percentile(Skillcorner)
    
    ggplot(percentile %>%
             filter(Player == input$player) %>%
             pivot_longer(cols = -Player, names_to = "Metric", 
                          values_to = "Percentile") %>%
             arrange(Metric)
           , aes(x = Metric, y = Percentile, 
                                  fill = Percentile)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.95)) +
      scale_fill_gradient2(low = "darkred", 
                           mid = "khaki",
                           high = "darkgreen",
                           midpoint = 0.5) +
      labs(title = "Percentile Ranks by Metric",
           x = "Metric",
           y = "Percentile") +
      theme_minimal() +
      coord_flip() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        panel.spacing = unit(0, "cm"))
    
    
  })
  
  output$radarPlot <- renderPlot({
    
    percentile = calculate_percentile(Skillcorner)
    
    player_name = percentile %>%
      filter(Player == input$player) %>%
      summarise(Distance = Distance, `Minutes Played` = `Minutes Played`, 
                Sprints = Sprints, `High Speed Running` = `High Speed Running`, 
                `High Accelerations` = `High Accelerations`,
                `Max Speed` = `Max Speed`) %>%
      head(1)
    
    rm(percentile)
    
    max_min <- data.table(Player = c('Max', 'Min'), Distance = c(1,0), 
                          'Minutes Played' = c(1,0), 
                          'Sprints' = c(1,0), 
                          'High Speed Running' = c(1,0),
                          'High Accelerations' = c(1,0),
                          'Max Speed' = c(1,0))
    
    df = rbind(max_min, player_name)
    
    rm(max_min, player_name)
    
    df_with_rowname = df %>%
      column_to_rownames(var = "Player")
    
    rm(df)
    
    radarchart(df_with_rowname,
               pcol = "#00AFBB", 
               pfcol = scales::alpha("#00AFBB", 0.5),
               plwd = 2, plty = 1, cglcol = "grey", 
               cglty = 1, cglwd = 0.8, 
               axislabcol = "grey", vlcex = 0.7,
               title = "Player Performance Radar Chart",
               vlabels = c('Distance', 'Minutes Played', 'Sprints', 
                           'High Speed Running', 'High Accelerations', 
                           'Max Speed'),
               caxislabels = seq(0, 1, 0.2))
    
    output
    
  })
  
  
output$distancePlot <- renderPlot({
  
    ggplot(Skillcorner %>%
             filter(Player == input$player) %>%
             select(Date, Player, `Minutes Played`, Distance) %>%
             mutate('Distance/90' = (Distance/`Minutes Played`) * 90)
           , aes(x = Date, y = `Distance/90`)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, ) +
      labs(title = "Distance vs Date",
           x = "Date",
           y = "Distance/90") +
      theme_minimal()
    
    
})
    

output$distancehalvesPlot <- renderPlot({
  
      ggplot(Skillcorner %>%
              filter(Player == input$player) %>%
               filter(!is.na(`Distance 1`),
                    !is.na(`Distance 2`)) %>%
              select(Player, `Distance 1`, `Distance 2`, 
                     `Minutes Played 1`, `Minutes Played 2`) %>%
             group_by(Player) %>%
             summarise('1' = (mean(`Distance 1`)/`Minutes Played 1`) * 45, 
                       '2' = (mean(`Distance 2`)/`Minutes Played 2`) * 45) %>%
             pivot_longer(cols = c('1', '2'), names_to = "Half", 
                          values_to = "Distance/45")
           , aes(x=Half, y=`Distance/45`)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center') +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Distance vs Half",
           x = "Half",
           y = "Distance/45") +
      theme_minimal()

})

output$sprintsPlot <- renderPlot({
  
  ggplot(Skillcorner %>%
           filter(!is.na(`Count Sprint 1`),
                  !is.na(`Count Sprint 2`)) %>%
           filter(Player == input$player) %>%
           select(Player, `Count Sprint 1`, `Count Sprint 2`, 
                  `Minutes Played 1`, `Minutes Played 2`) %>%
           group_by(Player) %>%
           summarise('1' = (mean(`Count Sprint 1`)/`Minutes Played 1`) * 45, 
                     '2' = (mean(`Count Sprint 2`)/`Minutes Played 2`) * 45) %>%
           pivot_longer(cols = c('1', '2'), names_to = "Half", 
                        values_to = "Sprints/45")
         , aes(x=Half, y=`Sprints/45`)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center') +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Sprints vs Half",
           x = "Half",
           y = "Sprints/45") +
      theme_minimal()
    
})  
    
  
}
# Run the application 
shinyApp(ui = ui, server = server)
