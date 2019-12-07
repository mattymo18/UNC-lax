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
                 plotOutput('FO.map', height = "auto"),
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
                 h4('Player Summary Tables'),
                 tableOutput('Player.Summary'),
                 h4('Sector Success Probabilities'),
                 tableOutput('Summary.Table.1'),
                 h4('Sector Counts'),
                 tableOutput('Summary.Table.2'),
                 h4('Advanced Tables'),
                 textOutput('Table.Label.1'),
                 tableOutput('Summary.Table.3'),
                 textOutput('Table.Label.2'),
                 tableOutput('Summary.Table.4')
               ))), 
    
    tabPanel('Opponent Data',  
             sidebarLayout(
               sidebarPanel(selectInput("Opponent.Team.Select", h3("Team"), 
                                        choices = unique(Shot.Opp$Team), selected = "Duke"),
                            selectInput("Opponent.Role.Select", h3("Role"), choices = c("Goalie", "Fogo", "Offense")),
                            selectInput("Opponent.Player.Select", h3("Player"), choice = "")),
               mainPanel(h4('Player Summary Tables'),
                         tableOutput('Opponent.Player.Summary'),
                         h4('Sector Success Probabilities'),
                         tableOutput('Opponent.Summary.Table.1'),
                         h4('Sector Counts'),
                         tableOutput('Opponent.Summary.Table.2'),
                         h4('Advanced Tables'),
                         textOutput('Opponent.Table.Label.1'),
                         tableOutput('Opponent.Summary.Table.3'),
                         textOutput('Opponent.Table.Label.2'),
                         tableOutput('Opponent.Summary.Table.4')))) 
    # tabPanel('Reference',
    #          includeMarkdown()
    #          )
))


