library(hashmap)
library(comprehenr)
library(lubridate)
library(dplyr)
library(leaflet)
library(stringr)
library(shiny)
library(shinydashboard)
library(readr)

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


# the lat and long are using NESW; change it to positive/negative; because plotting only accepts numbers
# N/E = positive, S/W = negative
remake_coordinates = function(coordinates){
  result = numeric(length(coordinates)) # empty vector of characters
  i = 1
  for(coordinate in coordinates){
    bearing_index = nchar(coordinate)
    double_value = substr(coordinate, 1, bearing_index - 1) # get just the digits
    bearing = substr(coordinate, bearing_index, bearing_index) # get the direction
    if (bearing == "N" || bearing == "E"){
      result[i] = as.numeric(double_value)
    } else { # must be South or West
      result[i] = -as.numeric(double_value)
    }
    i = i + 1
  }
  result
}

# reassign the coordinates
pacific_data$lat = remake_coordinates(pacific_data$lat)
pacific_data$lon = remake_coordinates(pacific_data$lon)
atlantic_data$lat = remake_coordinates(atlantic_data$lat)
atlantic_data$lon = remake_coordinates(atlantic_data$lon)


# a header_row has many NA's at least 15
is_header_row = function(row){
  if(sum(is.na(row)) >= 15) 
    TRUE 
  else 
    FALSE
}

# find the row indices of the headers
header_locations = function(data){
  to_vec(
    for(row in 1:nrow(data)) 
      if (is_header_row(data[row,])) 
        row
  )
}

storm_name_from_header = function(header_df, prefix, id){
  if (header_df[1, 2] == "UNNAMED"){
    paste(prefix, "STORM", id, sep = " ") # use STORM + id as new name
  } else {
    header_df[1, 2]
  }
}

# prefix is for naming unnamed storms
# if prefix is "ATLANTIC" then unnamed storms will be named 
# ATLANTIC STORM <storm_id>
make_huricane_data = function(data, header_indices, prefix){
  tables = list() # keep track of the data associated with each storm
  i = 1 
  unnamed_id = 1
  for(row_index in 1:length(header_indices)){
    row = header_indices[row_index] # get the next header row index
    current_header = data[row,] # get the header data
    storm_name = storm_name_from_header(current_header, prefix, unnamed_id)
    if (current_header[1, 2] == "UNNAMED"){ # update for next id
      unnamed_id = unnamed_id + 1
    }
    
    
    if (row_index == length(header_indices)){ # at the last header
      tables[[i]] = data[(row+1):nrow(data),] %>% # the data is from next row to the end of the data
        mutate(storm_name = storm_name) %>% # add the storm name
        mutate(uid = current_header[1, 1]) # add the storm `unique ID
      # adding these will make it easier to filter data
      
    }else { # otherwise, data is from next row to the start of the next header
      next_header = header_indices[row_index+1]
      tables[[i]] = data[(row+1):(next_header-1),]  %>%
        mutate(storm_name = storm_name) %>% # add the storm name
        mutate(uid = current_header[1, 1]) # add the storm unique ID
    }
    i = i + 1
  }
  tables
}



pacific_header_indices = header_locations(pacific_data)
atlantic_header_indices = header_locations(atlantic_data)


# list of dataframes of storms; this wil be the main storage for the data. makes it easy to plot
pacific_data = make_huricane_data(pacific_data, pacific_header_indices, "PACIFIC") # contains names and name_data
atlantic_data = make_huricane_data(atlantic_data, atlantic_header_indices, "ATLANTIC") # contains names and name_data
# if needed, can merge into 1 list 
# combined_data = c(pacific_data, atlantic_data)

########################## FUNCTIONS TO HELP IN PLOTTING ######################
plot_storm_path = function(storm_data, color){ # very simple plots, but will customize later
  map_object = leaflet() %>% addTiles()
  if (nrow(storm_data) == 1){ # only 1 coordinate
    map_object %>% 
      addCircleMarkers(data=storm_data, lat= ~lat, lng= ~lon, color = color)
  }else {
    map_object %>% 
      addPolylines(data=storm_data, lat = ~lat, lng = ~lon, color=color) # %>% 
    # addMarkers(data=storm_data, lng= ~lon, lat= ~lat)
  }
}

plot_multi_storm_path = function(storm_data_list, color_list){
  map_object = leaflet() %>% addTiles()
  for(i in 1:length(storm_data_list)){
    storm_data = storm_data_list[[i]]
    color = color_list[i]
    if (nrow(storm_data) == 1){ # only 1 coordinate
      map_object = map_object %>% 
        addCircleMarkers(data=storm_data, lat= ~lat, lng= ~lon, color = color)
    }else {
      map_object = map_object %>% 
        addPolylines(data=storm_data, lat = ~lat, lng = ~lon, color=color) # %>% 
      # addMarkers(data=storm_data, lng= ~lon, lat= ~lat)
    }
  }
  map_object
}



get_storms_by_year = function(storm_data_list, year){
  result = list()
  i = 1 
  for (storm_data in storm_data_list){
    # check wether the given year is in the years cuz a storm may span end of 1 year to start of another
    if (any(year(storm_data$datetime) == year)){ 
      result[[i]] = storm_data
      i = i + 1
    }
  }
  result
}


get_storms_by_day = function(storm_data_list, date_string){
  # date string should be a string of the form mm/dd/YYYY (same in UI display)
  result = list()
  i = 1 
  for(storm_data in storm_data_list){
    # check if date is in the dates (because a storm can span > 1 day)
    if (any(as.Date(storm_data$datetime) == as.Date(date_string, "%m/%d/%Y"))) {
      result[[i]] = storm_data
      i = i + 1
    }
  }
  result 
}


get_storms_by_name = function(storm_data_list, names){
  result = list()
  i = 1
  for (storm_data in storm_data_list){
    # storm_name is all the same for 1 storm, so only need the first 1
    if (any(storm_data$storm_name[1]  %in% names)){
      result[[i]] = storm_data
      i = i + 1
    }
  }
  result
}

get_storms_since = function(storm_data_list, year){
  result = list()
  i = 1 
  for (storm_data in storm_data_list){
    # check wether the year is at least what was given
    if (any(year(storm_data$datetime) >= year)){ 
      result[[i]] = storm_data
      i = i + 1
    }
  }
  result
}

atlantic_since2005 = get_storms_since(atlantic_data, 2005)


get_all_storm_days = function(storm_data_list){
  result = vector()
  class(result) = "Date"
  for (storm_data in storm_data_list){
    result = c(result, as.Date(storm_data$datetime))
  }
  result = sort(unique(result), decreasing = TRUE)
  
  strftime(result, format = "%m/%d/%Y")
}


atlantic_storm_days = c("", get_all_storm_days(atlantic_since2005))
# for the select


get_top_10_storms = function(storm_data_list){
  indices = list() # keep track of indices of the storms in the list
  speeds = list() # keep track of speeds
  i = 1
  for (storm_data in storm_data_list){
    indices[[i]] = i # store the index
    speeds[[i]] = max(storm_data$speed) # get the max speed for this storm
    i = i + 1
  }
  index_speed_df = data.frame(index=unlist(indices), speed=unlist(speeds)) %>% arrange(desc(speed))
  result = list()
  i = 1
  for(index in index_speed_df$index[1:10]){
    result[[i]] = storm_data_list[[index]]
    i = i + 1
  }
  result  
}

atlantic_top10 = get_top_10_storms(atlantic_data)

get_storm_names = function(storm_data_list){
  result = character(length(storm_data_list))
  i = 1
  for (storm_data in storm_data_list){
    result[i] = storm_data$storm_name[1] # just need the first item cuz all same
    i = i + 1
  }
  result
}

colors = c("red", "blue", "gray", "pink", "purple", "black", "aqua", "royalblue",
           "yellow", "brown", "green", "turquoise", "skyblue", "fuscia", "white", "orange")


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














































