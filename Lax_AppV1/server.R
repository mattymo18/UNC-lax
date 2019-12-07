shinyServer(function(input, output, session) {
############################################# Shot Map ############################################# 
  
  ############ Data Manipulation ############ 
  Shot.data$`Shot Location` <- factor(Shot.data$`Shot Location`)
  levels(Shot.data$`Shot Location`) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")
  
  ############ Functions ############ 
  shot.count.func <- reactive({
    Shot.data %>%
      filter(Name == input$Shot.Map.Player.Select) %>%  
      group_by(Name, `Shot Location`) %>%
      summarise(`c` = n()) %>%
      ungroup() %>%
      spread(`Shot Location`, `c`, fill=0, drop = F)
  })
  
  shot.prob.func <- reactive ({
    Shot.data %>%
      filter(Name == input$Shot.Map.Player.Select) %>%
      group_by(Name, `Shot Location`) %>%
      summarise(`Prob` = sum(Goal)/n()) %>%
      ungroup() %>%
      spread(`Shot Location`, `Prob`, fill=0, drop = F)
  })
  
  ############ Selector Reactivity ############ 
  observe({
    Team = input$Shot.Map.Team.Select
    updateSelectInput(session, "Shot.Map.Player.Select", choices = Shot.data[Shot.data$Team==input$Shot.Map.Team.Select, 3])
  })
  
  ############ Main Plot ############ 
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
       geom_label(mapping = aes(x=1.5, y=3.5), label = as.character(paste("A", "\n", "Shots:",shot.count.func()[2], "\n", "%:",round(shot.prob.func()[2], digits = 3)*100))) +
       geom_label(mapping = aes(x=1.5, y=2.5), label = as.character(paste("D", "\n", "Shots:",shot.count.func()[5], "\n", "%:",round(shot.prob.func()[5], digits = 3)*100))) +
       geom_label(mapping = aes(x=1.5, y=1.5), label = as.character(paste("G", "\n", "Shots:",shot.count.func()[8], "\n", "%:",round(shot.prob.func()[8], digits = 3)*100))) +
       geom_label(mapping = aes(x=2.5, y=3.35), size = 3.5, label = as.character(paste("B", "\n", "Shots:",shot.count.func()[3], "\n", "%:",round(shot.prob.func()[3], digits = 3)*100))) +
       geom_label(mapping = aes(x=3.5, y=3.5), label = as.character(paste("C", "\n", "Shots:",shot.count.func()[4], "\n", "%:",round(shot.prob.func()[4], digits = 3)*100))) +
       geom_label(mapping = aes(x=3.5, y=2.5), label = as.character(paste("F", "\n", "Shots:",shot.count.func()[7], "\n", "%:",round(shot.prob.func()[7], digits = 3)*100))) +
       geom_label(mapping = aes(x=3.5, y=1.5), label = as.character(paste("I", "\n", "Shots:",shot.count.func()[10], "\n", "%:",round(shot.prob.func()[10], digits = 3)*100))) +
       geom_label(mapping = aes(x=2.5, y=2.5), label = as.character(paste("E", "\n", "Shots:",shot.count.func()[6], "\n", "%:",round(shot.prob.func()[6], digits = 3)*100))) +
       geom_label(mapping = aes(x=2.5, y=1.5), label = as.character(paste("H", "\n", "Shots:",shot.count.func()[9], "\n", "%:",round(shot.prob.func()[9], digits = 3)*100)))
   )
  ############ Shot Probility Table ############ 
   output$Shot.map.player.data.prob <- renderTable(
     shot.prob.func()
   )
   
   ############ Shot Distribution Table ############ 
   output$Shot.map.player.data.count <- renderTable(
      shot.count.func() %>%
        mutate(Total=rowSums(shot.count.func()[,-1]))
   )
   
############################################# FO Map ############################################# 
   
   ############ Data Manipulation ############ 
   FO.data$`GB Location` <- factor(FO.data$`GB Location`)
   levels(FO.data$`GB Location`) <- c("A", "B", "C", "D", "E", "F", "G", "H")
   
   ############ Functions ############ 
   FO.count.func <- reactive ({
     FO.data %>% 
       filter(Name == input$FO.Map.Player.Select) %>% 
       group_by(Name, `GB Location`) %>% 
       summarise(`c` = n()) %>% 
       ungroup() %>% 
       spread(`GB Location`, `c`, fill=0, drop = F)
   })
   
   FO.prob.func <- reactive ({
     FO.data %>% 
       filter(Name == input$FO.Map.Player.Select) %>% 
       group_by(Name, `GB Location`) %>% 
       summarise(`Prob` = sum(`Fogo GB`)/n()) %>% 
       ungroup() %>% 
       spread(`GB Location`, `Prob`, fill=0, drop = F)
   })
   
   ############ Selector Reactivity ############ 
   observe({
     Team = input$FO.Map.Team.Select
     updateSelectInput(session, "FO.Map.Player.Select", choices = FO.data[FO.data$Team == input$FO.Map.Team.Select, 2])
   })
   
   ############ Main Plot ############ 
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
       geom_label(mapping = aes(x=2.25, y=6.9), label = as.character(paste("A", "\n", "Count:", FO.count.func()[2], "\n", "%:", round(FO.prob.func()[2], digits = 3)*100)), size = 4) +
       geom_label(mapping = aes(x=2.75, y=6.9), label = as.character(paste("B", "\n", "Count:", FO.count.func()[3], "\n", "%:", round(FO.prob.func()[3], digits = 3)*100)), size = 4) +
       geom_label(mapping = aes(x=2.25, y=5.6), label = as.character(paste("C", "\n", "Count:", FO.count.func()[4], "\n", "%:", round(FO.prob.func()[4], digits = 3)*100)), size = 4) +
       geom_label(mapping = aes(x=2.75, y=5.6), label = as.character(paste("D", "\n", "Count:", FO.count.func()[5], "\n", "%:", round(FO.prob.func()[5], digits = 3)*100)), size = 4) +
       geom_label(mapping = aes(x=2.25, y=4.4), label = as.character(paste("E", "\n", "Count:", FO.count.func()[6], "\n", "%:", round(FO.prob.func()[6], digits = 3)*100)), size = 4) +
       geom_label(mapping = aes(x=2.75, y=4.4), label = as.character(paste("F", "\n", "Count:", FO.count.func()[7], "\n", "%:", round(FO.prob.func()[7], digits = 3)*100)), size = 4) +
       geom_label(mapping = aes(x=2.25, y=3.1), label = as.character(paste("G", "\n", "Count:", FO.count.func()[8], "\n", "%:", round(FO.prob.func()[8], digits = 3)*100)), size = 4) +
       geom_label(mapping = aes(x=2.75, y=3.1), label = as.character(paste("H", "\n", "Count:", FO.count.func()[9], "\n", "%:", round(FO.prob.func()[9], digits = 3)*100)), size = 4) +
       geom_label(mapping = aes(x=4.75, y=5.6, label = "Opposing Goal"), size = 6) +
       scale_shape_manual(values=c(1,5)), 
     height = 550
   )
   
   ############ FO Probability Table ############ 
   output$FO.map.player.data.prob <- renderTable(
    FO.prob.func() 
   )
   
   ############ FO Distribution Table ############ 
   output$FO.map.player.data.count <- renderTable(
     FO.count.func() %>% 
       mutate("Total" = rowSums(FO.count.func()[,-1]))
   )
   
############################################# Goal Map ############################################# 
   
   ############ Data Manipulation ############
   Goal.data$`Shot Location` = factor(Goal.data$`Shot Location`)
   levels(Goal.data$`Shot Location`) <-  c("A", "B", "C", "D", "E", "F", "G", "H", "I")
   
   ############ Functions ############ 
   Goal.count.func <- reactive ({
     Goal.data %>% 
       filter(Name == input$Goal.Map.Player.Select) %>% 
       group_by(Name, `Shot Location`) %>% 
       summarise(`c` = n()) %>% 
       ungroup() %>% 
       spread(`Shot Location`, `c`, fill = 0, drop = F)
   })
   
   Goal.prob.func <- reactive ({
     Goal.data %>% 
       filter(Name == input$Goal.Map.Player.Select) %>% 
       group_by(Name, `Shot Location`) %>% 
       summarise(`Prob` = sum(`Save`)/n()) %>% 
       ungroup() %>% 
       spread(`Shot Location`, `Prob`, fill = 0, drop = F)
   })
   
   ############ Reactive Player Selector ############
   observe({
     Team = input$Goal.Map.Team.Select
     updateSelectInput(session, "Goal.Map.Player.Select", choices = Goal.data[Goal.data$Team == input$Goal.Map.Team.Select, 2])
   })
   
   ############ Main Plot ############
   output$Goal.map <- renderPlot(
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
       geom_segment(mapping = aes(x=-1, y=1, xend=1, yend=1), size=5) + #top
       geom_segment(mapping = aes(x=-1, y=1.32, xend=-1, yend=-20), size=6) + #left
       geom_segment(mapping = aes(x=1, y=1.32, xend=1, yend=-20), size=6) + #right
       geom_segment(mapping = aes(x=-1, xend=1, y=-13.5, yend=-13.5), lty=2) +
       geom_segment(mapping = aes(x=-1, xend=1, y=-6.5, yend=-6.5), lty=2) + 
       geom_segment(mapping = aes(x=-.333, xend=-.333, y=1.32, yend=-20), lty=2) +
       geom_segment(mapping = aes(x=.333, xend=.333, y=1.32, yend=-20), lty=2) +
       geom_label(mapping = aes(x=-.666, y=-2.5), label= as.character(paste("A", "\n", "Count:", Goal.count.func()[2], "\n", "%:", round(Goal.prob.func()[2], digits = 3)*100))) +
       geom_label(mapping = aes(x=0, y=-2.5), label=as.character(paste("B", "\n", "Count:", Goal.count.func()[3], "\n", "%:", round(Goal.prob.func()[3], digits = 3)*100))) +
       geom_label(mapping = aes(x=.666, y=-2.5), label=as.character(paste("C", "\n", "Count:", Goal.count.func()[4], "\n", "%:", round(Goal.prob.func()[4], digits = 3)*100))) +
       geom_label(mapping = aes(x=-.666, y=-10), label=as.character(paste("D", "\n", "Count:", Goal.count.func()[5], "\n", "%:", round(Goal.prob.func()[5], digits = 3)*100))) +
       geom_label(mapping = aes(x=0, y=-10), label=as.character(paste("E", "\n", "Count:", Goal.count.func()[6], "\n", "%:", round(Goal.prob.func()[6], digits = 3)*100))) +
       geom_label(mapping = aes(x=.666, y=-10), label=as.character(paste("F", "\n", "Count:", Goal.count.func()[7], "\n", "%:", round(Goal.prob.func()[7], digits = 3)*100))) +
       geom_label(mapping = aes(x=-.666, y=-17.5), label=as.character(paste("G", "\n", "Count:", Goal.count.func()[8], "\n", "%:", round(Goal.prob.func()[8], digits = 3)*100))) +
       geom_label(mapping = aes(x=0, y=-17.5), label=as.character(paste("H", "\n", "Count:", Goal.count.func()[9], "\n", "%:", round(Goal.prob.func()[9], digits = 3)*100))) +
       geom_label(mapping = aes(x=.666, y=-17.5), label=as.character(paste("I", "\n", "Count:", Goal.count.func()[10], "\n", "%:", round(Goal.prob.func()[10], digits = 3)*100))) +
       xlim(-2, 2), 
     width = 500
   )
   
   ############ Goal Probability Table ############
   output$Goal.map.player.data.prob <- renderTable(
     Goal.prob.func()
   )
  
   ############ Goal Distribution Table ############
   output$Goal.map.player.data.count <-  renderTable(
     Goal.count.func() %>% 
       mutate("Total" = rowSums(Goal.count.func()[,-1]))
   )
   
############################################# UNC Data ############################################# 
   
   ############ Functions ############
   shot.count.func.2 <- reactive({
     Shot.data %>%
       filter(Name == input$Player.Select) %>%  
       group_by(Name, `Shot Location`) %>%
       summarise(`c` = n()) %>%
       ungroup() %>%
       spread(`Shot Location`, `c`, fill=0, drop = F)
   })
   
   shot.prob.func.2 <- reactive ({
     Shot.data %>%
       filter(Name == input$Player.Select) %>%
       group_by(Name, `Shot Location`) %>%
       summarise(`Prob` = sum(Goal)/n()) %>%
       ungroup() %>%
       spread(`Shot Location`, `Prob`, fill=0, drop = F)
   })
   
   FO.count.func.2 <- reactive ({
     FO.data %>% 
       filter(Name == input$Player.Select) %>% 
       group_by(Name, `GB Location`) %>% 
       summarise(`c` = n()) %>% 
       ungroup() %>% 
       spread(`GB Location`, `c`, fill=0, drop = F)
   })
   
   FO.prob.func.2 <- reactive ({
     FO.data %>% 
       filter(Name == input$Player.Select) %>% 
       group_by(Name, `GB Location`) %>% 
       summarise(`Prob` = sum(`Fogo GB`)/n()) %>% 
       ungroup() %>% 
       spread(`GB Location`, `Prob`, fill=0, drop = F)
   })
   
   Goal.count.func.2 <- reactive ({
     Goal.data %>% 
       filter(Name == input$Player.Select) %>% 
       group_by(Name, `Shot Location`) %>% 
       summarise(`c` = n()) %>% 
       ungroup() %>% 
       spread(`Shot Location`, `c`, fill = 0, drop = F)
   })
   
   Goal.prob.func.2 <- reactive ({
     Goal.data %>% 
       filter(Name == input$Player.Select) %>% 
       group_by(Name, `Shot Location`) %>% 
       summarise(`Prob` = sum(`Save`)/n()) %>% 
       ungroup() %>% 
       spread(`Shot Location`, `Prob`, fill = 0, drop = F)
   })
   
  Scenario.rate.func <- reactive({
     Shot.data %>% 
       filter(Shot.data$Name == input$Player.Select) %>% 
       group_by(Name, Scenario) %>% 
       summarise(prob=sum(Goal)/n()) %>% 
       ungroup() %>% 
       spread(Scenario, prob, fill=0, drop=F)
   })
  
   Assisted.rate.func <- reactive({
     Shot.data %>% 
       filter(Name == input$Player.Select) %>% 
        filter(Assisted == 1) %>% 
         group_by(Name) %>% 
         summarise(`Assist Rate` = sum(Assisted)/nrow(Shot.data[Shot.data$Name == input$Player.Select, ]), `Assisted Shot Success Rate` = sum(Goal)/n())
     })
   
   ############ Selector Reactivity ############ 
   observe({
     Role = input$Role.Select
     updateSelectInput(session, "Player.Select", 
                       choices = if (input$Role.Select == "Goalie"){
                         unique(Goal.UNC$Name)
                       } else if (input$Role.Select == "Fogo") {
                         unique(FO.UNC$Name)
                       } else {
                         unique(Shot.UNC$Name)
                       })
   }) 
   
   ############ Tables ############
   
   ##### Player Summary Tables #####
   output$Player.Summary <- renderTable(
     if (input$Role.Select == "Goalie"){
       unique(Goal.UNC[Goal.UNC$Name == input$Player.Select, c(1:4)]) %>% 
         mutate(`Save %` = sum(Goal.UNC$Save[Goal.UNC$Name == input$Player.Select])/nrow(Goal.UNC[Goal.UNC$Name == input$Player.Select, ])*100) %>% 
         mutate(`Saves per Game` = sum(Goal.UNC$Save[Goal.UNC$Name == input$Player.Select])/length(unique(Goal.UNC$Game)))
     } else if (input$Role.Select == "Fogo") {
       unique(FO.UNC[FO.UNC$Name == input$Player.Select, c(1:3)]) %>% 
         mutate(`FO %` = sum(FO.UNC$`Fogo GB`[FO.UNC$Name == input$Player.Select])/nrow(FO.UNC[FO.UNC$Name == input$Player.Select, ])*100) %>% 
         mutate(`FB %` = sum(FO.UNC$FB[FO.UNC$Name == input$Player.Select])/nrow(FO.UNC[FO.UNC$Name == input$Player.Select, ])*100) %>% 
         mutate(`Goal %` = sum(FO.UNC$Goal[FO.UNC$Name == input$Player.Select])/nrow(FO.UNC[FO.UNC$Name == input$Player.Select, ])*100)
     } else {
       unique(Shot.UNC[Shot.UNC$Name == input$Player.Select, c(1:3)]) %>% 
         mutate(`Shot %` = sum(Shot.UNC$Goal[Shot.UNC$Name == input$Player.Select])/nrow(Shot.UNC[Shot.UNC$Name == input$Player.Select, ])*100) %>% 
         mutate(`Shots Per Game` = nrow(Shot.UNC[Shot.UNC$Name == input$Player.Select, ])/length(unique(Goal.UNC$Game))) %>% 
         mutate(`Goals Per Game` = sum(Shot.UNC$Goal[Shot.UNC$Name == input$Player.Select])/length(unique(Goal.UNC$Game)))
       # 100 % do shot/game and goal/game
     })
   
   ##### Shot Prob Table #####
   output$Summary.Table.1 <- renderTable(
     if (input$Role.Select == "Goalie"){
       Goal.prob.func.2()
     } else if (input$Role.Select == "Fogo"){
       FO.prob.func.2()
     } else {
       shot.prob.func.2()
     }
   )
   
   ##### Shot Count Table #####
   output$Summary.Table.2 <- renderTable(
     if (input$Role.Select == "Goalie"){
       Goal.count.func.2() %>% 
         mutate(Total=rowSums(Goal.count.func.2()[,-1]))
     } else if (input$Role.Select == "Fogo"){
       FO.count.func.2() %>% 
         mutate(Total=rowSums(FO.count.func.2()[,-1]))
     } else {
       shot.count.func.2() %>% 
         mutate(Total=rowSums(shot.count.func.2()[,-1]))
     }
   )
   
   ##### Scenario Table #####
   output$Table.Label.1 <- renderText(
     if(input$Role.Select == "Offense"){
       "Scenario Success Rate"
     } else {}
   )
   
   
   output$Summary.Table.3 <- renderTable(
     if (input$Role.Select == "Offense"){
       Scenario.rate.func()
     } else {}
   )
   
   ##### Assist Table #####
   output$Table.Label.2 <- renderText(
     if(input$Role.Select == "Offense"){
       "Assisted Shot Success Rate"
     } else {}
   )
   
   output$Summary.Table.4 <- renderTable(
     if (input$Role.Select == "Offense"){
       Assisted.rate.func()
     } else {}
   )
   
############################################# Opponent Data ############################################# 
   
   ############ Functions ############ 
   shot.count.func.3 <- reactive({
     Shot.data %>%
       filter(Name == input$Opponent.Player.Select) %>%  
       group_by(Name, `Shot Location`) %>%
       summarise(`c` = n()) %>%
       ungroup() %>%
       spread(`Shot Location`, `c`, fill=0, drop = F)
   })
   
   shot.prob.func.3 <- reactive ({
     Shot.data %>%
       filter(Name == input$Opponent.Player.Select) %>%
       group_by(Name, `Shot Location`) %>%
       summarise(`Prob` = sum(Goal)/n()) %>%
       ungroup() %>%
       spread(`Shot Location`, `Prob`, fill=0, drop = F)
   })
   
   FO.count.func.3 <- reactive ({
     FO.data %>% 
       filter(Name == input$Opponent.Player.Select) %>% 
       group_by(Name, `GB Location`) %>% 
       summarise(`c` = n()) %>% 
       ungroup() %>% 
       spread(`GB Location`, `c`, fill=0, drop = F)
   })
   
   FO.prob.func.3 <- reactive ({
     FO.data %>% 
       filter(Name == input$Opponent.Player.Select) %>% 
       group_by(Name, `GB Location`) %>% 
       summarise(`Prob` = sum(`Fogo GB`)/n()) %>% 
       ungroup() %>% 
       spread(`GB Location`, `Prob`, fill=0, drop = F)
   })
   
   Goal.count.func.3 <- reactive ({
     Goal.data %>% 
       filter(Name == input$Opponent.Player.Select) %>% 
       group_by(Name, `Shot Location`) %>% 
       summarise(`c` = n()) %>% 
       ungroup() %>% 
       spread(`Shot Location`, `c`, fill = 0, drop = F)
   })
   
   Goal.prob.func.3 <- reactive ({
     Goal.data %>% 
       filter(Name == input$Opponent.Player.Select) %>% 
       group_by(Name, `Shot Location`) %>% 
       summarise(`Prob` = sum(`Save`)/n()) %>% 
       ungroup() %>% 
       spread(`Shot Location`, `Prob`, fill = 0, drop = F)
   })
   
   Scenario.rate.func.2 <- reactive({
     Shot.data %>% 
       filter(Shot.data$Name == input$Opponent.Player.Select) %>% 
       group_by(Name, Scenario) %>% 
       summarise(prob=sum(Goal)/n()) %>% 
       ungroup() %>% 
       spread(Scenario, prob, fill=0, drop=F)
   })
   
   Assisted.rate.func.2 <- reactive({
     Shot.data %>% 
       filter(Name == input$Opponent.Player.Select) %>% 
       filter(Assisted == 1) %>% 
       group_by(Name) %>% 
       summarise(`Assist Rate` = sum(Assisted)/nrow(Shot.data[Shot.data$Name == input$Opponent.Player.Select, ]), `Assisted Shot Success Rate` = sum(Goal)/n())
   })
   
   
   ############ Reactive Player Selector ############
   observe({
     Role = input$Opponent.Role.Select
     updateSelectInput(session, "Opponent.Player.Select", 
                       choices = if (input$Opponent.Role.Select == "Goalie") {
                         unique(Goal.Opp[Goal.Opp$Team == input$Opponent.Team.Select, 2])
                       }else if (input$Opponent.Role.Select == "Fogo") {
                         unique(FO.Opp[FO.Opp$Team == input$Opponent.Team.Select, 2])
                       } else {
                         unique(Shot.Opp[Shot.Opp$Team == input$Opponent.Team.Select, 3])
                       })
   })
   
   ##### Player Summary Tables #####
   output$Opponent.Player.Summary <- renderTable(
     if (input$Opponent.Role.Select == "Goalie"){
       unique(Goal.Opp[Goal.Opp$Name == input$Opponent.Player.Select, c(1:4)]) %>% 
         mutate(`Save %` = sum(Goal.Opp$Save[Goal.Opp$Name == input$Opponent.Player.Select])/nrow(Goal.Opp[Goal.Opp$Name == input$Opponent.Player.Select, ])*100) %>% 
         mutate(`Saves per Game` = sum(Goal.Opp$Save[Goal.Opp$Name == input$Opponent.Player.Select])/length(unique(Goal.Opp$Game[Goal.Opp$Team == input$Opponent.Team.Select])))
     } else if (input$Opponent.Role.Select == "Fogo") {
       unique(FO.Opp[FO.Opp$Name == input$Opponent.Player.Select, c(1:3)]) %>% 
         mutate(`FO %` = sum(FO.Opp$`Fogo GB`[FO.Opp$Name == input$Opponent.Player.Select])/nrow(FO.Opp[FO.Opp$Name == input$Opponent.Player.Select, ])*100)
     } else {
       unique(Shot.Opp[Shot.Opp$Name == input$Opponent.Player.Select, c(1:3)]) %>% 
         mutate(`Shot %` = sum(Shot.Opp$Goal[Shot.Opp$Name == input$Opponent.Player.Select])/nrow(Shot.Opp[Shot.Opp$Name == input$Opponent.Player.Select, ])*100) %>% 
         mutate(`Shots Per Game` = nrow(Shot.Opp[Shot.Opp$Name == input$Opponent.Player.Select, ])/length(unique(Goal.Opp$Game[Goal.Opp$Team == input$Opponent.Team.Select]))) %>% 
         mutate(`Goals Per Game` = sum(Shot.Opp$Goal[Shot.Opp$Name == input$Opponent.Player.Select])/length(unique(Goal.Opp$Game[Goal.Opp$Team == input$Opponent.Team.Select])))
     })
   
   ##### Goal Prob Table #####
   output$Opponent.Summary.Table.1 <- renderTable(
     if (input$Opponent.Role.Select == "Goalie"){
       Goal.prob.func.3()
     } else if (input$Opponent.Role.Select == "Fogo"){
       FO.prob.func.3()
     } else {
       shot.prob.func.3()
     }
   )
   
   ##### Goal Count Func #####
   output$Opponent.Summary.Table.2 <- renderTable(
     if (input$Opponent.Role.Select == "Goalie"){
       Goal.count.func.3() %>% 
         mutate(Total=rowSums(Goal.count.func.3()[,-1]))
     } else if (input$Opponent.Role.Select == "Fogo"){
       FO.count.func.3() %>% 
         mutate(Total=rowSums(FO.count.func.3()[,-1]))
     } else {
       shot.count.func.3() %>% 
         mutate(Total=rowSums(shot.count.func.3()[,-1]))
     }
   )
   
   ##### Scenario Table #####
   output$Opponent.Table.Label.1 <- renderText(
     if(input$Opponent.Role.Select == "Offense"){
       "Scenario Success Rate"
     } else {}
   )
   
   output$Opponent.Summary.Table.3 <- renderTable(
     if (input$Opponent.Role.Select == "Offense"){
       Scenario.rate.func.2()
     } else {}
   )
   
   ##### Assist Table #####
   output$Opponent.Table.Label.2 <- renderText(
     if(input$Opponent.Role.Select == "Offense"){
       "Assisted Shot Success Rate"
     } else {}
   )
   
   output$Opponent.Summary.Table.4 <- renderTable(
     if (input$Opponent.Role.Select == "Offense"){
       Assisted.rate.func.2()
     } else {}
   )
})
