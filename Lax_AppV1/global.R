library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(shinythemes)
library(xtable)
FO.data <- read_excel("Data/FO.data.xlsx")
Goal.data <- read_excel("Data/Goal.data.xlsx")
Shot.data <- read_excel("Data/Shot.data.xls")
FO.UNC = FO.data[FO.data$Team == "UNC", ]
FO.Opp = FO.data[FO.data$Team != "UNC", ]
Shot.UNC = Shot.data[Shot.data$Team == "UNC", ]
Shot.Opp = Shot.data[Shot.data$Team != "UNC", ]
Goal.UNC = Goal.data[Goal.data$Team == "UNC", ]
Goal.Opp = Goal.data[Goal.data$Team != "UNC", ]
Shot.data$Scenario = factor(Shot.data$Scenario)

# Roster <- read_excel("Data/2019 Roster.xlsx")
# Total.Names.UNC = data.frame(Name = unique(Roster$Name))
# Offense.Roster <- unique(Roster) %>% 
#   filter(Position == "Midfielder" | Position == "Attackman") %>% 
#   filter(Class != "Sr.")