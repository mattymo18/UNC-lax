shinyUI(
  navbarPage(
    title = 'Menu',
    tabPanel('Shot Map',
             sidebarLayout(
               sidebarPanel(selectInput("Shot.Map.Team.Select", h3("Team"), 
                                        choices = list("UNC", "Duke", "UVA"), selected = "UNC"),
                            selectInput("Shot.Map.Player.Select", h3("Player"), choices = "")),
             mainPanel(
               plotOutput('Shot.map'),
               tableOutput('Shot.map.data'),
               textOutput('Shot.Prob.Title'),
               tableOutput('Shot.map.player.data.prob'),
               textOutput('Shot.Dist.Title'), 
               tableOutput('Shot.map.player.data.count')
             ))),
   
    tabPanel('Face Off Map', 
             sidebarLayout(
               sidebarPanel(selectInput("FO.Map.Team.Select", h3("Team"), 
                                        choices = list("UNC", "Duke", "UVA"), selected = "UNC"),
                            selectInput("FO.Map.Player.Select", h3("Player"), choices = "")),
               mainPanel(
                 plotOutput('FO.map'),
                 tableOutput('FO.map.data'),
                 textOutput('FO.Prob.Title'),
                 tableOutput("FO.map.player.data.prob"),
                 textOutput('FO.Dist.Title'),
                 tableOutput('FO.map.player.data.count')
               ))),
    
    tabPanel('Goal Map', 
             sidebarLayout(
               sidebarPanel(selectInput("Goal.Map.Team.Select", h3("Team"), 
                                        choices = list("UNC", "Duke", "UVA"), selected = "UNC"),
                            selectInput("Goal.Map.Player.Select", h3("Player"), choices = "")),
               mainPanel(
                 plotOutput('Goal.map'),
                 tableOutput("Goal.map.data"),
                 tableOutput("Goal.map.player.data")
             ))),
    
    tabPanel('UNC Team Data', 
             DT::dataTableOutput('unc.data')),
    
    tabPanel('Opponent Data',  
             DT::dataTableOutput('opp.data'))
  )
)

