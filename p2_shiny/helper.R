############################## DATA CLEANER FUNCTIONS ######################
############################## DATA CLEANER FUNCTIONS ######################

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


# Many names are repeated; to give unique names, we'll attach the year to it
# For example, there are 2 "IRMA", but 1 is in 1978 and other is in 2017
# We'll rename them as "IRMA 1978" and "IRMA 2017"
name_freq = hashmap('', 0)

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
    
    if (name_freq$has_key(storm_name)){ # this name has been repeated
      name_freq[[storm_name]] = name_freq[[storm_name]] + 1
    } else {
      name_freq[[storm_name]] = 1
    }
    
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
  make_unique_names(tables)
}

make_unique_names = function(storm_data_list){
  result = list()
  i = 1 
  for (storm_data in storm_data_list){
    storm_name = storm_data$storm_name[1] # all are the same name, just need 1
    if (name_freq[[storm_name]] > 1){
      year = year(storm_data$datetime)
      new_name = paste(storm_name, ' ', year)
      storm_data$storm_name = new_name
    }
    result[[i]] = storm_data
    i = i + 1
  }
  result
}



########################## FUNCTIONS TO HELP IN PLOTTING #####################################
########################## FUNCTIONS TO HELP IN PLOTTING #####################################

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
  color_index = 1
  for(i in 1:length(storm_data_list)){
    storm_data = storm_data_list[[i]]
    color = color_list[color_index]
    if (color_index == length(color_list)){ # recycle colors if not enough
      color_index = 1 
    } else {
      color_index = color_index + 1
    }
    
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
############################## FILTERING STORMS FUNCTIONS ######################
############################## FILTERING STORMS FUNCTIONS ######################


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


get_all_storm_days = function(storm_data_list){
  result = vector()
  class(result) = "Date"
  for (storm_data in storm_data_list){
    result = c(result, as.Date(storm_data$datetime))
  }
  result = sort(unique(result), decreasing = TRUE)
  
  strftime(result, format = "%m/%d/%Y")
}



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


get_storm_names = function(storm_data_list){
  result = character(length(storm_data_list))
  i = 1
  for (storm_data in storm_data_list){
    result[i] = storm_data$storm_name[1] # just need the first item cuz all same
    i = i + 1
  }
  result
}

colors = c("red", "blue", "gray", "pink", "purple", "black", "aqua", "teal",
           "yellow", "brown", "green", "turquoise", "orange", "fuscia", "white")
























