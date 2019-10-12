shinyUI=fluidPage(theme = shinytheme("cerulean"),
  navbarPage(
    title = 'Menu',
    tabPanel('Shot Map',
             sidebarLayout(
               sidebarPanel(selectInput("Shot.Map.Team.Select", h3("Team"), 
                                        choices = unique(Shot.data$Team), selected = "UNC"),
                            selectInput("Shot.Map.Player.Select", h3("Player"), choices = "")),
             mainPanel(
               plotOutput('Shot.map'),
               tableOutput('Shot.map.data'),
               h4('Shot Probability Table'),
               tableOutput('Shot.map.player.data.prob'),
               h4('Shot Distribution Table'), 
               tableOutput('Shot.map.player.data.count')
             ))),
   
    tabPanel('Face Off Map', 
             sidebarLayout(
               sidebarPanel(selectInput("FO.Map.Team.Select", h3("Team"), 
                                        choices = unique(FO.data$Team), selected = "UNC"),
                            selectInput("FO.Map.Player.Select", h3("Player"), choices = "")),
               mainPanel(
                 plotOutput('FO.map'),
                 tableOutput('FO.map.data'),
                 h4('Faceoff Probability Table'),
                 tableOutput('FO.map.player.data.prob'),
                 h4('Faceoff Distribution Table'),
                 tableOutput('FO.map.player.data.count')
               ))),
    
    tabPanel('Goal Map', 
             sidebarLayout(
               sidebarPanel(selectInput("Goal.Map.Team.Select", h3("Team"), 
                                        choices = unique(Goal.data$Team), selected = "UNC"),
                            selectInput("Goal.Map.Player.Select", h3("Player"), choices = "")),
               mainPanel(
                 plotOutput('Goal.map'),
                 tableOutput('Goal.map.data'),
                 h4('Save Probability Table'), 
                 tableOutput('Goal.map.player.data.prob'), 
                 h4('Save Distribution Table'), 
                 tableOutput('Goal.map.player.data.count')
             ))),
    
    tabPanel('UNC Team Data', 
             sidebarLayout(
               sidebarPanel(selectInput("Role.Select", h3("Role"), 
                                        choices = c("Goalie", "Fogo", "Offense")),
                            selectInput("Player.Select", h3("Player"), choices = "")),
                                        
               mainPanel(
                 h4('Player Summary Table'),
                 tableOutput('Player.Summary')
               ))), 
    
    tabPanel('Opponent Data',  
             DT::dataTableOutput('opp.data'))
  )
)


