######################################################
# CS 424 | Project 2
# Kevin Kowalski - kkowal28
# Samuel Kajah - skajah2
# Vijay Vemu - vvemu3
######################################################
#
# < insert project info and notes here >
#
######################################################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(dplyr)
library(DT)
library(leaflet)
library(scales)
library(stringr)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
    dashboardHeader(title = "Atlantic & Pacific Hurricane Data"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
    sidebarMenu(
      
      # add space to sidebar
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
    
      # about button
      actionButton("about_info", "About", width = 200)
    ),

    dashboardBody(
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # about information
    observeEvent(input$about_info, {
        showModal(modalDialog(
        title = "About",
        p("CS 424 - Project 2"),
        p("by Kevin Kowalski, Samuel Kajah, and Vijay Vemu"),
        p("The following libraries were used:"),
        p("- ggplot2"),
        p("- dplyr"),
        p("- DT"),
        p("- leaflet"),
        p("- lubridate"),
        p("- scales"),
        p("- shiny"),
        p("- shinydashboard"),
        p("- stringr"),
        p("Data was sourced from: http://www.nhc.noaa.gov/data/#hurdat"),
        p("( Downloaded from: https://www.nhc.noaa.gov/data/hurdat/hurdat2-1851-2018-120319.txt & 
          https://www.nhc.noaa.gov/data/hurdat/hurdat2-nepac-1949-2018-122019.txt )"),
        easyClose = TRUE
      ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
