library(baseballr)
library(tidyverse)
library(shiny)
library(shinythemes)

#dates cubs played in world series
dates <- c("2016-10-25","2016-10-26","2016-10-28",'2016-10-29',"2016-10-30","2016-11-01","2016-11-02")
cubs_ws_game_pk <- data.frame()

#get game ids for those dates
for (i in 1:length(dates)){

  get_pk <- get_game_pks_mlb(dates[i])
  cubs_ws_game_pk <- rbind(cubs_ws_game_pk, get_pk, fill = TRUE)
  
}

cubs_ws_game_pk <- cubs_ws_game_pk %>%
  filter(link != TRUE)

cubs_ws <- data.frame()
#get pitch by pitch data from those games
for (i in 1:length(dates)) {
  
  game_data <- mlb_pbp(game_pk = cubs_ws_game_pk$game_pk[i])
  game_data$date <- dates[i]
  cubs_ws <- rbind(cubs_ws, game_data, fill = TRUE)
  
}

#quick data exploration
unique(cubs_ws$game_pk)
colnames(cubs_ws)


#grab only pitch data and remove intentional balls, or NA pitches
cubs_ws_pitches <- cubs_ws %>%
  select(game_id = game_pk, date, isPitch, pitchNumber, details.description, in_play = details.isInPlay,
         strike = details.isStrike, ball = details.isBall, details.call.description, inning = about.inning, 
         pitcher = matchup.pitcher.fullName, pitch_id = pfxId, pitch_type = details.type.description,
         velo = pitchData.startSpeed, horz_break = pitchData.coordinates.pfxX,
         vert_break = pitchData.coordinates.pfxZ, plate_location = pitchData.coordinates.pX, 
         plate_height = pitchData.coordinates.pZ,
         break_angle = pitchData.breaks.breakAngle, break_length = pitchData.breaks.breakLength,
         spin = pitchData.breaks.spinRate, spin_direction = pitchData.breaks.spinDirection) %>%
  filter(isPitch == 'TRUE', pitch_type != 'TRUE', pitch_type != 'NA', pitch_type != 'Intentional Ball' ) %>%
  mutate(game_num = as.numeric(factor(game_id))) 

unique(cubs_ws_pitches$date)

#set color scheme
palette <- rep(c("Four-Seam Fastball" = "goldenrod2", "Sinker" = "deepskyblue4", "Slider" = "red2", "Changeup" = "forestgreen",
                 "Curveball" = "violetred3", "Cutter" = "purple3", "Knuckle Curve" = "wheat3"))

#build shiny app
ui <- fluidPage(theme = shinytheme('cosmo'),
                
                #page header
                headerPanel('Pitching Report'),
                
                #inputs from the user
                sidebarPanel( width = 3,
                  selectizeInput( inputId =  'game_num',
                                  label = 'Game Number',
                                  choices = unique(cubs_ws_pitches$game_num),
                                  selected = '1'),
                  
                  uiOutput("pitcher_select")
                ),
                #visualizations
                mainPanel( width = 6,
                  plotOutput("plot1",
                             click = "plot_click",
                             brush = brushOpts(id = "plot_brush"),
                             dblclick = "plot_dblclick")
                ),
                mainPanel( width = 12,
                           h4("Clicked Pitch: "),
                           tableOutput("plot_clickedpoints"),
                           tableOutput("plot_brushedpoints")
                ),
                tableOutput('summary_table')
)

server <- function(input, output, session) {
  
  #filter df based on user input
  df_filtered <- reactive({
    
    df_filt <- cubs_ws_pitches %>%
      filter(pitcher %in% input$pitcher, game_num %in% input$game_num)
    
    return(df_filt)
    
  })
  
  #reactive filter by game
  pitcher_var <- reactive({
    switch(input$game_num,
           '1' = cubs_ws_pitches %>% filter(game_num == 1) %>% summarise(pitcher = unique(pitcher)),
           '2' = cubs_ws_pitches %>% filter(game_num == 2) %>% summarise(pitcher = unique(pitcher)),
           '3' = cubs_ws_pitches %>% filter(game_num == 3) %>% summarise(pitcher = unique(pitcher)),
           '4' = cubs_ws_pitches %>% filter(game_num == 4) %>% summarise(pitcher = unique(pitcher)),
           '5' = cubs_ws_pitches %>% filter(game_num == 5) %>% summarise(pitcher = unique(pitcher)),
           '6' = cubs_ws_pitches %>% filter(game_num == 6) %>% summarise(pitcher = unique(pitcher)),
           '7' = cubs_ws_pitches %>% filter(game_num == 7) %>% summarise(pitcher = unique(pitcher)))
    
  })
  #render reactive filter
  output$pitcher_select <- renderUI({
    selectInput(inputId="pitcher",
                label="Pitcher", 
                choices = pitcher_var())
  })
  
  #pitch location plot
  output$plot1 <- renderPlot({
    ggplot(data = df_filtered()) +
      geom_point(aes(plate_location, plate_height, color = as.factor(pitch_type))) +
      scale_color_manual(values = palette, name = 'Pitch Type') +
      geom_rect(xmin = -0.83,
                xmax = 0.83,
                ymin = 1.5,
                ymax = 3.5, color = "black", fill = "transparent",size=1.1) +
      geom_rect(xmin = -1.2,
                xmax = 1.2,
                ymin = 1.2,
                ymax = 3.8, color = "black", linetype = "dashed", fill = "transparent") +
      geom_rect(xmin = -.6,
                xmax = .6,
                ymin = 1.8,
                ymax = 3.2, color = "black", linetype = "dashed", fill = "transparent")+
      geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = 0.5, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 0.5, color = "black") +
      geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = 0.5, color = "black") +
      geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = 0.5, color = "black") +
      geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 0.5, color = "black") +
      xlim(-2.5,2.5) +
      ylim(-.5,4.5) + 
      labs(title="Pitch Location",
           x ="Plate Side", y = "Plate Height",
           fill = 'Pitch Type') +
      theme_minimal()
  })
  
  #interactive table
  output$plot_clickedpoints <- renderTable({
    res <- nearPoints(df_filtered() %>% select(Game = game_num, Inning = inning,
                                               Pitcher = pitcher, PitchType = pitch_type,
                                               Velo = velo, Spin = spin,
                                               VertBreak = vert_break, 
                                               HorzBreak = horz_break,
                                               plate_location, plate_height),
                      input$plot_click)
    if (nrow(res) == 0)
      return()
    res
  })
  
  #output for brushed points
  output$plot_brushedpoints <- renderTable({
    res1 <- brushedPoints(df_filtered() %>% select(Game = game_num, Inning = inning,
                                               Pitcher = pitcher, PitchType = pitch_type,
                                               Velo = velo, Spin = spin,
                                               VertBreak = vert_break, 
                                               HorzBreak = horz_break,
                                               plate_location, plate_height),
                      input$plot_brush)
    if (nrow(res1) == 0)
      return()
    res1
  })
  
  #filter df for aggregated data table
  dt_f <- reactive({
    df_filt <- cubs_ws_pitches %>%
      filter(pitcher %in% input$pitcher, game_num %in% input$game_num) %>%
      group_by(pitch_type) %>%
      summarise(Pitches_Thrown = n(),
                Avg_Velo = mean(velo),
                Avg_Spin = mean(spin),
                Avg_VertBreak = mean(vert_break),
                Avg_HorzBreak = mean(horz_break))
    
    
    return(df_filt)
  })
  #aggregated data table
  output$summary_table <- renderTable(dt_f())
}


shinyApp(ui = ui, server = server)



