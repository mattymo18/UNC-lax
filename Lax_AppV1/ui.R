#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
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
               tableOutput("Shot.map.data"),
               tableOutput("Shot.map.player.data")
             ))),
   
    tabPanel('Face Off Map', 
             sidebarLayout(
               sidebarPanel(selectInput("FO.Map.Team.Select", h3("Team"), 
                                        choices = list("UNC", "Duke", "UVA"), selected = "UNC"),
                            selectInput("FO.Map.Player.Select", h3("Player"), choices = "")),
               mainPanel(
                 plotOutput('FO.map'),
                 tableOutput("FO.map.data"),
                 tableOutput("FO.map.player.data")
               ))),
    
    tabPanel('Goal Map', 
             plotOutput('Goal.map'), 
             DT::dataTableOutput('goal.data')),
    
    tabPanel('UNC Team Data', 
             DT::dataTableOutput('unc.data')),
    
    tabPanel('Oponent Data',  
             DT::dataTableOutput('opp.data'))
  )
)
