
library("stringr")
library("tidyverse")
library("ggplot2")
library("dbplyr")
library("patchwork")
library("shiny")
library("plotly")
library("lintr")
library("styler")

source("scriptss/ui.R")
source("scriptss/server.R")

shinyApp(ui = ui, server = server)
