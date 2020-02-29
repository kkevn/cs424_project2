library(hashmap)
library(comprehenr)
library(lubridate)
library(dplyr)
library(leaflet)
library(stringr)
library(shiny)
library(shinydashboard)
library(readr)

source("helper.R") # all functions in this file
# makes code cleaner and easier to manage

data_row_header = c('date', 'time', 'record_id', 'storm_type', 'lat', 'lon', 'speed', 'pressure', 
                    'wind_radii_ne_34', 'wind_radii_se_34', 'wind_radii_sw_34', 'wind_radii_nw_34',
                    'wind_radii_ne_50', 'wind_radii_se_50', 'wind_radii_sw_50', 'wind_radii_nw_50',
                    'wind_radii_ne_64', 'wind_radii_se_64', 'wind_radii_sw_64', 'wind_radii_nw_64', 'size')

# these are the original files (without any mods); 
pacific_data = read.csv('pacific_huricane.csv', header=FALSE, stringsAsFactors = FALSE)
atlantic_data = read.csv('atlantic_huricane.csv', header=FALSE, stringsAsFactors = FALSE)


colnames(pacific_data) = data_row_header
colnames(atlantic_data) = data_row_header

# The first 6 columns are strings that contains leading/trailing whitespace; trim them
for (col in 1:6){
  pacific_data[, col] = str_trim(pacific_data[, col])
  atlantic_data[, col] = str_trim(atlantic_data[, col])
}


# make a timestamp col
pacific_data = pacific_data %>% mutate(datetime=parse_date_time(paste(pacific_data$date, pacific_data$time, sep=' '), "Ymd HM", tz = 'America/Chicago', quiet=TRUE))

atlantic_data = atlantic_data %>% mutate(datetime=parse_date_time(paste(atlantic_data$date, atlantic_data$time, sep=' '), "Ymd HM", tz = 'America/Chicago', quiet=TRUE))


# reassign the coordinates
pacific_data$lat = remake_coordinates(pacific_data$lat)
pacific_data$lon = remake_coordinates(pacific_data$lon)
atlantic_data$lat = remake_coordinates(atlantic_data$lat)
atlantic_data$lon = remake_coordinates(atlantic_data$lon)

# get the header indices
pacific_header_indices = header_locations(pacific_data)
atlantic_header_indices = header_locations(atlantic_data)


# list of dataframes of storms; this wil be the main storage for the data. makes it easy to plot
pacific_data = make_huricane_data(pacific_data, pacific_header_indices, "PACIFIC") # contains names and name_data
atlantic_data = make_huricane_data(atlantic_data, atlantic_header_indices, "ATLANTIC") # contains names and name_data
# if needed, can merge into 1 list 
# combined_data = c(pacific_data, atlantic_data)



atlantic_since2005 = get_storms_since(atlantic_data, 2005)

# for the select input in UI, not all are showing; explore later
atlantic_storm_days = c("", get_all_storm_days(atlantic_since2005))

atlantic_top10 = get_top_10_storms(atlantic_data)

atlantic_names_since_2005 = get_storm_names(atlantic_since2005)

atlantic_top10_names = get_storm_names(atlantic_top10)


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
             actionButton(inputId = "uncheck_all_button", label = "Uncheck All")
      ),
            
      column(2, offset = 1,
             selectInput(inputId = "years", label = "Year", choices = c("", 2005:2018)),
             selectInput(inputId = "days", label = "Day", choices = atlantic_storm_days)
             )
    ) # end fluidRow
    
  ) # end dashboardBody
) # end dashboardPage


server = function(input, output, session) {
    shown_names = reactiveVal() # keep track of which names are currently being displayed
    
    output$map = renderLeaflet({
      atlantic2018 = get_storms_by_year(atlantic_data, 2018)
      plot_multi_storm_path(atlantic2018, colors)
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
    })
    
    # when year chosen, only shown storms from that year
    observeEvent(input$years, {
      if (input$years != ""){
        year_storms = get_storms_by_year(atlantic_data, input$years)
        names = get_storm_names(year_storms)
        
        updateCheckboxGroupInput(session, "storm_names", 
                                 choices = names)
        shown_names(names) # update shown names
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
      }
    })
    
    
}

shinyApp(ui=ui, server=server)

# update label/title to reflect what was chosen














































