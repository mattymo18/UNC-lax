#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   output$Shot.map <- renderPlot(
     ggplot() +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.ticks.y=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank()) +
       geom_segment(mapping = aes(x=0, y=5, xend=5, yend=5)) +
       geom_segment(mapping = aes(x=0, y=0, xend=5, yend=0)) +
       geom_segment(mapping = aes(x=0, y=0, xend=0, yend=5)) +
       geom_segment(mapping = aes(x=5, y=0, xend=5, yend=5)) +
       geom_segment(mapping = aes(x=0, xend=5, y=1, yend=1)) +
       geom_segment(mapping = aes(x=.75, xend=.75, y=1, yend=5)) +
       geom_segment(mapping = aes(x=4.25, xend=4.25, y=1, yend=5)) +
       geom_point(mapping = aes(x=2.5, y=4), size = 20, alpha = .5, color = "black") +
       geom_segment(mapping = aes(x=2.4, xend=2.6, y=4, yend=4)) +
       geom_segment(mapping = aes(x=1, xend=4, y=4, yend=4), lty=2) +
       geom_segment(mapping = aes(x=1, xend=1, y=1, yend=4), lty=2) +
       geom_segment(mapping = aes(x=1, xend=4, y=2, yend=2), lty=2) +
       geom_segment(mapping = aes(x=4, xend=4, y=1, yend=4), lty=2) +
       geom_segment(mapping = aes(x=1, xend=4, y=3, yend=3), lty=2) +
       geom_segment(mapping = aes(x=2, xend=2, y=1, yend=4), lty=2) +
       geom_segment(mapping = aes(x=3, xend=3, y=1, yend=4), lty=2) +
       geom_label(mapping = aes(x=1.5, y=3.5), label = "A") +
       geom_label(mapping = aes(x=1.5, y=2.5), label = "D") +
       geom_label(mapping = aes(x=1.5, y=1.5), label = "G") +
       geom_label(mapping = aes(x=2.5, y=3.5), label = "B") +
       geom_label(mapping = aes(x=3.5, y=3.5), label = "C") +
       geom_label(mapping = aes(x=3.5, y=2.5), label = "F") +
       geom_label(mapping = aes(x=3.5, y=1.5), label = "I") +
       geom_label(mapping = aes(x=2.5, y=2.5), label = "E") +
       geom_label(mapping = aes(x=2.5, y=1.5), label = "H")
   )
   # output$Shot.data <- renderDataTable(datatable(Shot.data, options = list(searching = F, paging = F)))
   output$Shot.map.data <- renderTable (
     Shot.data[Shot.data$Team==input$Shot.Map.Team.Select, c(3, 4)]
   )
   observe({
     Team = input$Shot.Map.Team.Select
     updateSelectInput(session, "Shot.Map.Player.Select", choices = Shot.data[Shot.data$Team==input$Shot.Map.Team.Select, 3])
   })
   output$Shot.map.player.data <- renderTable(
     Shot.data[Shot.data$Name==input$Shot.Map.Player.Select, ]
   )
   output$FO.map <- renderPlot(
     ggplot() +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.ticks.y=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             legend.title = element_blank()) +
       geom_segment(mapping = aes(x=0, xend=5, y=10, yend=10)) +
       geom_segment(mapping = aes(x=0, xend=5, y=0, yend=0)) +
       geom_segment(mapping = aes(x=0, xend=0, y=0, yend=10)) +
       geom_segment(mapping = aes(x=5, xend=5, y=0, yend=10)) +
       geom_segment(mapping = aes(x=2.5, xend=2.5, y=0, yend=10)) +
       geom_segment(mapping = aes(x=3.75, xend=3.75, y=0, yend=10)) +
       geom_segment(mapping = aes(x= 1.25, xend=1.25, y=0, yend=10)) +
       geom_segment(mapping = aes(x=0, xend=1.25, y=2, yend=2)) +
       geom_segment(mapping = aes(x=0, xend=1.25, y=8, yend=8)) +
       geom_segment(mapping = aes(x=3.75, xend=5, y=2, yend=2)) +
       geom_segment(mapping = aes(x=3.75, xend=5, y=8, yend=8)) +
       geom_segment(mapping = aes(x=2, xend=3, y=2, yend=2)) +
       geom_segment(mapping = aes(x=2, xend=3, y=8, yend=8)) +
       geom_point(mapping = aes(x=.3, y=5), size = 10, alpha = .5) +
       geom_point(mapping = aes(x=4.7, y=5), size = 10, alpha = .5) +
       geom_segment(mapping = aes(x=.3, xend=.3, y=4.8, yend=5.2)) +
       geom_segment(mapping = aes(x=4.7, xend=4.7, y=4.8, yend=5.2)) +
       geom_point(mapping = aes(x=2.5, y=5), shape = 4, size = 5) +
       geom_segment(mapping = aes(x=2, xend=2, y=2.5, yend=7.5), lty=2) +
       geom_segment(mapping = aes(x=3, xend=3, y=2.5, yend=7.5), lty=2) +
       geom_segment(mapping = aes(x=2, xend=3, y=2.5, yend=2.5), lty=2) +
       geom_segment(mapping = aes(x=2, xend=3, y=7.5, yend=7.5), lty=2) +
       geom_segment(mapping = aes(x=2, xend=3, y=6.25, yend=6.25), lty=2) +
       geom_segment(mapping = aes(x=2, xend=3, y=3.75, yend=3.75), lty=2) +
       geom_segment(mapping = aes(x=2, xend=3, y=5, yend=5), lty=2) +
       geom_label(mapping = aes(x=2.25, y=6.9), label="A") +
       geom_label(mapping = aes(x=2.75, y=6.9), label="B") +
       geom_label(mapping = aes(x=2.25, y=5.6), label="C") +
       geom_label(mapping = aes(x=2.75, y=5.6), label="D") +
       geom_label(mapping = aes(x=2.25, y=4.4), label="E") +
       geom_label(mapping = aes(x=2.75, y=4.4), label="F") +
       geom_label(mapping = aes(x=2.25, y=3.1), label="G") +
       geom_label(mapping = aes(x=2.75, y=3.1), label="H") +
       scale_shape_manual(values=c(1,5))
   )
   output$FO.map.data <- renderTable(
     FO.data[FO.data$Team == input$FO.Map.Team.Select, c(2, 3)]
   )
   observe({
     Team = input$FO.Map.Team.Select
     updateSelectInput(session, "FO.Map.Player.Select", choices = FO.data[FO.data$Team == input$FO.Map.Team.Select, 2])
   })
   output$FO.map.player.data = renderTable(
     FO.data[FO.data$Name == input$FO.Map.Player.Select, ]
   )
   
})
   
   
   
   