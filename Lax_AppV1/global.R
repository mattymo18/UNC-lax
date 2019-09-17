library(shiny)
library(DT)
library(tidyverse)
library(readxl)
# Shot.data = read_excel("Data/Player Eval Test 1.xls")
Shot.data$`Shot Location` <- factor(Shot.data$`Shot Location`)
levels(Shot.data$`Shot Location`) <- c(levels(Shot.data$`Shot Location`), "G", "H", "I") 
# FO.data = read_excel("Data/FO.data.xls")
# Goalie.data = read_excel("Data/Goalie.data.xlsx")s