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

# import libraries
library(comprehenr)
library(dplyr)
library(ggplot2)
library(hashmap)
library(leaflet)
library(lubridate)
library(readr)
library(shiny)
library(shinydashboard)
library(stringr)

# retrieve helper functions
source("helper.R")

# read in the processed data
atlantic_data <- readRDS(file = "atlantic_data.rds")
pacific_data <- readRDS(file = "pacific_data.rds")

########################## DATA NEEDED FOR PLOTTING #####################################
########################## DATA NEEDED FOR PLOTTING #####################################

atlantic_since2005 = get_storms_since(atlantic_data, 2005)

# for the select input in UI, not all are showing; explore later
atlantic_storm_days = c("", get_all_storm_days(atlantic_since2005))

atlantic_top10 = get_top_10_storms(atlantic_data)

atlantic_names_since_2005 = get_storm_names(atlantic_since2005)

atlantic_top10_names = get_storm_names(atlantic_top10)

atlantic_names_chronological = get_storm_names_chronologically(atlantic_data)

atlantic_names_alphabetical = get_storm_names_alphabetically(atlantic_data)

atlantic_names_by_max_speed = get_storm_names_max_speed(atlantic_data)

atlantic_names_by_min_pressure = get_storm_names_min_pressure(atlantic_data)

combined_data = c(atlantic_data, pacific_data)
post2005_combined_data <- get_storms_since(combined_data, 2005)
binded_rows <- bind_rows(post2005_combined_data, .id="df")

########################## DASHBOARD #####################################
########################## DASHBOARD #####################################

ui = dashboardPage(
    
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
    ), # end sidebarMenu
    
    dashboardBody(
        fluidRow(
            column(6, leafletOutput(outputId = "map")),
            column(6, plotOutput(outputId = "plot")), # empty space to store the bar graph
            column(12, box(title = "Storms Since 2005", width = NULL, status = "info")),
            column(2,
                   checkboxGroupInput(inputId = "storm_names", label = "Name", 
                                      choices = atlantic_names_since_2005)
            ),
            column(1,
                   actionButton(inputId = "show_all_names_button", label = "Show All"),
                   actionButton(inputId = "show_top10_names_button", label = "Show Top 10"),
                   actionButton(inputId = "check_all_button", label = "Check All"),
                   actionButton(inputId = "uncheck_all_button", label = "Uncheck All"),
                   
                   actionButton(inputId = "show_all_names_chrono_button", label = "Show All Chronologically"),
                   actionButton(inputId = "show_all_names_alpha_button", label = "Show All Alphabetically"),
                   actionButton(inputId = "show_all_names_max_speed_button", label = "Show All by Max Speed"),
                   actionButton(inputId = "show_all_names_min_pressure_button", label = "Show All by Min Pressure")
            ),
            
            column(2, offset = 1,
                   selectInput(inputId = "years", label = "Year", choices = c("", 2005:2018)),
                   selectInput(inputId = "days", label = "Day", choices = atlantic_storm_days)
            ),
            
            column(3,
                box(title = "Overview Graph", solidHeader = TRUE, status = "primary", width = 14,
                    plotOutput("overview", height = 200), style = "font-size:150%")
                    # remove above span line and replace with plot
                )
            )
        ) # end fluidRow
        
    ) # end dashboardBody
# end dashboardPage


server = function(input, output, session) {
    shown_names = reactiveVal() # keep track of which names are currently being displayed
    
    
    output$map = renderLeaflet({
        # all the names are there in the beginning
        shown_names(atlantic_names_since_2005)
        atlantic2018 = get_storms_by_year(atlantic_data, 2018)
        #plot_multi_storm_path(atlantic2018, colors)
        plot_multi_storm_path_by_size(atlantic2018, colors)
    })
    
    observeEvent(input$about_info, {
        showModal(
            modalDialog(
                HTML(read_file("about.html")),
                easyClose = TRUE
            )
        ) # end showModal
    }) # end about info 
    
    # Show all the storms in Atlantic
    observeEvent(input$show_all_names_button, {
        updateCheckboxGroupInput(session, "storm_names", choices = atlantic_names_since_2005)
        shown_names(atlantic_names_since_2005) # update the shown names
    })
    
    # Show all the storms in Atlantic chronologically
    observeEvent(input$show_all_names_chrono_button, {
        updateCheckboxGroupInput(session, "storm_names", choices = atlantic_names_chronological)
        shown_names(atlantic_names_chronological) # update the shown names
    })
    
    # Show all the storms in Atlantic alphabetically 
    observeEvent(input$show_all_names_alpha_button, {
        updateCheckboxGroupInput(session, "storm_names", choices = atlantic_names_alphabetical)
        shown_names(atlantic_names_alphabetical) # update the shown names
    })
    
    # Show all the storms in Atlantic by max wind speed
    observeEvent(input$show_all_names_max_speed_button, {
        updateCheckboxGroupInput(session, "storm_names", choices = atlantic_names_by_max_speed)
        shown_names(atlantic_names_by_max_speed) # update the shown names
    })
    
    # Show all the storms in Atlantic by minimum pressure
    observeEvent(input$show_all_names_min_pressure_button, {
        updateCheckboxGroupInput(session, "storm_names", choices = atlantic_names_by_min_pressure)
        shown_names(atlantic_names_by_min_pressure) # update the shown names
    })
    
    # Show only top top storms
    observeEvent(input$show_top10_names_button, {
        updateCheckboxGroupInput(session, "storm_names", choices = atlantic_top10_names)
        shown_names(atlantic_top10_names) # update the shown names
    })
    
    # Check all the options currently shown
    observeEvent(input$check_all_button, {
        updateCheckboxGroupInput(session, "storm_names", 
                                 choices = shown_names(), selected = shown_names())
    })
    
    # Uncheck all the options currently shown 
    observeEvent(input$uncheck_all_button, {
        updateCheckboxGroupInput(session, "storm_names", 
                                 choices = shown_names())
        output$map = renderLeaflet({leaflet() %>% addTiles()}) # empty map
    })
    
    # when year chosen, only shown storms from that year
    observeEvent(input$years, {
        if (input$years != ""){
            year_storms = get_storms_by_year(atlantic_data, input$years)
            names = get_storm_names(year_storms)
            
            updateCheckboxGroupInput(session, "storm_names", 
                                     choices = names)
            shown_names(names) # update shown names
            # autocheck all
            updateCheckboxGroupInput(session, "storm_names", 
                                     choices = shown_names(), selected = shown_names())
        }
    })
    
    # when day chosen, only shown storms from that day
    observeEvent(input$days, {
        if (input$days != ""){
            day_storms = get_storms_by_day(atlantic_data, input$days)
            names = get_storm_names(day_storms)
            
            updateCheckboxGroupInput(session, "storm_names", 
                                     choices = names)
            shown_names(names) # update shown names
            # autocheck all
            updateCheckboxGroupInput(session, "storm_names", 
                                     choices = shown_names(), selected = shown_names())
        }
    })
    
    observeEvent(input$storm_names, {
        storms_to_plot = get_storms_by_name(atlantic_data, input$storm_names)
        # print(length(storms_to_plot))
        # print(get_storm_names(storms_to_plot))
        output$map = renderLeaflet({
            #plot_multi_storm_path(storms_to_plot, colors)
            plot_multi_storm_path_by_size(storms_to_plot, colors)
        })
    })
    
    # overview graph
 output$overview <- renderPlot({ggplot(binded_rows, aes(x = year(binded_rows$Timestamp))) + geom_bar(fill = "purple") + labs(title = "Number of hurriances per year since 2005", x = "Year of Hurricane", y = "Number of Hurricances") + scale_x_continuous(breaks=2005:2018)})  
    
}

shinyApp(ui=ui, server=server)

# update label/title to reflect what was chosen
# color respond with magnitude/category