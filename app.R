library(shiny)
library(ckanr)
library(getPass)
library(dplyr)
library(forcats)
library(ggplot2)
library(scales)
library(sf)
library(naomi)
library(lemon)

source("src/server/introduction_server.R")
source("src/server/viewer_server.R")

source("src/ui/navigation.R")
source("src/ui/introduction.R")
source("src/ui/viewer.R")

shiny::shinyAppDir("src")