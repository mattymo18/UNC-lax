library(shiny)
library(DT)
library(tidyverse)
library(readxl)
library(shinythemes)
FO.data <- read_excel("Data/FO.data.xlsx")
Goal.data <- read_excel("Data/Goal.data.xlsx")
Shot.data <- read_excel("Data/Shot.data.xls")
FO.UNC = FO.data[FO.data$Team == "UNC", ]
Shot.UNC = Shot.data[Shot.data$Team == "UNC", ]
Goal.UNC = Goal.data[Goal.data$Team == "UNC", ]
Total.Names.UNC = data.frame(Name =unique(c(FO.UNC$Name, Shot.UNC$Name, Goal.UNC$Name)))