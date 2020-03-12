########################## FUNCTIONS TO HELP IN PLOTTING #####################################
########################## FUNCTIONS TO HELP IN PLOTTING #####################################

# plot multiple storm paths on a map by their size
plot_multi_storm_path_by_size = function(storm_data_list, color_list, map_style) {
    map_object = leaflet() %>% addProviderTiles(map_style)
    
    if (length(storm_data_list) == 0 ){
        return (map_object)
    }
    
    for(i in 1: length(storm_data_list)) {
        storm_data = storm_data_list[[i]]
        color = color_list[i]
        if (nrow(storm_data) == 1) { # only 1 coordinate
            map_object = map_object %>% 
                addCircleMarkers(data = storm_data, lat = ~Lat, lng = ~Lon, color = color, radius = storm_data$Category) # add markers for size, replace radius with desired circle scaler
        } else {
            map_object = map_object %>% 
                addCircleMarkers(data = storm_data, lat = ~Lat, lng = ~Lon, color = color, radius = storm_data$Category) %>% # add markers for size, replace radius with desired circle scaler
                addPolylines(data = storm_data, lat = ~Lat, lng = ~Lon, color = color, weight = storm_data$Category) # %>% weight = storm_data$Size
            # addMarkers(data=storm_data, lng= ~lon, lat= ~lat)
        }
    }
    map_object
}

# !!! all hurricanes have N/A in Size column, so using scaled down speed to plot size of hurricane at each point of its path

############################## FILTERING STORMS FUNCTIONS ######################
############################## FILTERING STORMS FUNCTIONS ######################

# get a list of storms (spanning) in a given year
get_storms_by_year = function(storm_data_list, year) {
    result = list()
    i = 1
    for (storm_data in storm_data_list) {
        # check wether the given year is in the years cuz a storm may span end of 1 year to start of another
        if (any(year(storm_data$Timestamp) == year)) {
            result[[i]] = storm_data
            i = i + 1
        }
    }
    result
}

# get a list of storms (spanning) in a given day
get_storms_by_day = function(storm_data_list, date_string) {
    # date string should be a string of the form mm/dd/YYYY (same in UI display)
    result = list()
    i = 1
    for (storm_data in storm_data_list) {
        # check if date is in the dates (because a storm can span > 1 day)
        if (any(as.Date(storm_data$Timestamp) == as.Date(date_string, "%m/%d/%Y"))) {
            result[[i]] = storm_data
            i = i + 1
        }
    }
    result
}

# get a storm's data given its name
get_storms_by_name = function(storm_data_list, names) {
    result = list()
    i = 1
    for (storm_data in storm_data_list) {
        # storm_name is all the same for 1 storm, so only need the first 1
        if (any(storm_data$Storm_Name[1]  %in% names)) {
            result[[i]] = storm_data
            i = i + 1
        }
    }
    result
}

# get a list of all storms since the given year
get_storms_since = function(storm_data_list, year) {
    result = list()
    i = 1
    for (storm_data in storm_data_list) {
        # check wether the year is at least what was given
        if (any(year(storm_data$Timestamp) >= year)) {
            result[[i]] = storm_data
            i = i + 1
        }
    }
    result
}

# get a vector of all unique days storms were recorded on
get_all_storm_days = function(storm_data_list) {
    result = vector()
    class(result) = "Date"
    for (storm_data in storm_data_list) {
        result = c(result, as.Date(storm_data$Timestamp))
    }
    result = sort(unique(result), decreasing = TRUE)
    
    strftime(result, format = "%m/%d/%Y")
}

# return both years and days from the list in descending order
get_all_days_and_years = function(storm_data_list){
    years = vector()
    days = vector()
    class(days) = "Date"
    
    for (storm_data in storm_data_list){
        days = c(days, unique(as.Date(storm_data$Timestamp)))
        years = c(years, unique(year(as.Date(storm_data$Timestamp))))
    }
    result = list(years = sort(unique(years), decreasing = TRUE), 
                  days = strftime(sort(unique(days), decreasing = TRUE), format = "%m/%d/%Y"))
    result
}


# get a table of top 10 fastest hurricanes and their max speed for a given data set
get_top_10_storms = function(storm_data_list) {
    indices = list() # keep track of indices of the storms in the list
    speeds = list() # keep track of speeds
    i = 1
    for (storm_data in storm_data_list) {
        indices[[i]] = i # store the index
        speeds[[i]] = max(storm_data$Speed) # get the max speed for this storm
        i = i + 1
    }
    index_speed_df = data.frame(index = unlist(indices), speed = unlist(speeds)) %>% arrange(desc(speed))
    result = list()
    i = 1
    for (index in index_speed_df$index[1:10]) {
        result[[i]] = storm_data_list[[index]]
        i = i + 1
    }
    result
}

# get a list of all storm names
get_storm_names = function(storm_data_list) {
    result = character(length(storm_data_list))
    i = 1
    for (storm_data in storm_data_list) {
        result[i] = storm_data$Storm_Name[1] # just need the first item cuz all same
        i = i + 1
    }
    result
}

# get a list of all storm names chronologically (which should be default ordering)
get_storm_names_chronologically = function(storm_data_list) {
    table = list()
    i = 1
    for (storm_data in storm_data_list) {
        table[[i]] = storm_data$Storm_Name[1]
        i = i + 1
    }
    
    # build dataframe of each hurricane by name
    df <- data.frame(matrix(unlist(table), nrow = length(table), byrow = T), stringsAsFactors = FALSE)
    names(df)[1] <- "Storm_Name"
    
    # should be sorted in chronological order by default so return the ordered list of names
    df$Storm_Name
}

# get a list of all storm names alphabetically
get_storm_names_alphabetically = function(storm_data_list) {
    sort(unlist(get_storm_names(storm_data_list)))
}

# get a list of all storm names by max speed given low/high
get_storm_names_max_speed = function(storm_data_list, low, high) {
    table = list()
    i = 1
    for (storm_data in storm_data_list) {
        table[[i]] = c(storm_data$Storm_Name[1], max(storm_data$Speed))
        i = i + 1
    }
    # build dataframe of each hurricane and its top speed
    df <- data.frame(matrix(unlist(table), nrow = length(table), byrow = T), stringsAsFactors = FALSE)
    names(df)[1] <- "Storm_Name"
    names(df)[2] <- "TopSpeed"
    df$TopSpeed <- as.numeric(df$TopSpeed)
    
    # if parameters for low of range specified
    if (missing(low) == FALSE) {
        
        # remove out of range values from dataframe
        df <- filter(df, TopSpeed >= low)
    }
    
    # if parameters for high of range specified
    if (missing(high) == FALSE) {
        
        # remove out of range values from dataframe
        df <- filter(df, TopSpeed <= high)
    }
    
    # sort in descending order and return the ordered list of names
    df <- df[order(-df$TopSpeed), ]
    df$Storm_Name
}

# get a table of hurricanes in order by min pressure for a given data set in pressure range of low to high
get_storm_names_min_pressure = function(storm_data_list, low, high) {
    table = list()
    i = 1
    for (storm_data in storm_data_list) {
        table[[i]] = c(storm_data$Storm_Name[1], min(storm_data$Pressure))
        i = i + 1
    }
    
    # build dataframe of each hurricane and its min pressure
    df <- data.frame(matrix(unlist(table), nrow = length(table), byrow = T), stringsAsFactors = FALSE)
    names(df)[1] <- "Storm_Name"
    names(df)[2] <- "MinPressure"
    df$MinPressure <- as.numeric(df$MinPressure)
    
    # if parameters for low of range specified
    if (missing(low) == FALSE) {
        
        # remove out of range values from dataframe
        df <- filter(df, MinPressure >= low)
    }
    
    # if parameters for high of range specified
    if (missing(high) == FALSE) {
        
        # remove out of range values from dataframe
        df <- filter(df, MinPressure <= high)
    }
    
    # sort in ascending order and return the ordered list of names
    df <- df[order(df$MinPressure), ]
    df$Storm_Name
}

# get a list of hurricanes which made landfall for a given data set
get_storms_landfall = function(storm_data_list) {
    result = list()
    i = 1
    for (storm_data in storm_data_list) {
        if (any(storm_data$Record_ID == 'L')) {
            result[[i]] = storm_data
            i = i + 1
        }
    }
    result
}

# get a list of hurricanes which DID NOT make landfall for a given data set
get_storms_no_landfall = function(storm_data_list) {
    result = list()
    i = 1
    for (storm_data in storm_data_list) {
        
        if (all(storm_data$Record_ID != 'L')) {
            result[[i]] = storm_data
            i = i + 1
        }
    }
    result
}


############################### TABLES ####################################
############################### TABLES ####################################

# get a table of hurricanes in order by top speed for a given data set
get_storm_names_max_speed_table = function(storm_data_list) {
    table = list()
    i = 1
    for (storm_data in storm_data_list) {
        top_speed = max(storm_data$Speed)
        top_speed_index = which(storm_data$Speed == top_speed)
        table[[i]] = c(storm_data$Storm_Name[1], strftime(storm_data$Timestamp[top_speed_index], "%m/%d/%Y")[2], top_speed)
        i = i + 1
    }
    
    # build dataframe of each hurricane and its top speed
    df <- data.frame(matrix(unlist(table), nrow = length(table), byrow = T), stringsAsFactors = FALSE)
    names(df)[1] <- "Storm_Name"
    names(df)[2] <- "Timestamp"
    names(df)[3] <- "TopSpeed"
    df$TopSpeed <- as.numeric(df$TopSpeed)
    df$Timestamp <- parse_date_time(df$Timestamp,"%m/%d/%Y", tz = 'America/Chicago', quiet = TRUE)    
    # sort in descending order and return the ordered list of names
    df <- df[order(-df$TopSpeed), ]
    df
}


# get a table of hurricanes in order by min pressure for a given data set (UPDATE IN GIT)
get_storm_names_min_pressure_table = function(storm_data_list) {
    table = list()
    i = 1
    for (storm_data in storm_data_list) {
        storm_data$Pressure = 
            to_vec(
                for (pressure in storm_data$Pressure) 
                    if (pressure < 0)
                        0
                else 
                    pressure
            )
        min_pressure = min(storm_data$Pressure)
        min_pressure_index = which(storm_data$Pressure == min_pressure)[1] # just need first one even if multiple matches
        table[[i]] = c(storm_data$Storm_Name[1], strftime(storm_data$Timestamp[min_pressure_index], format = "%m/%d/%Y"), min_pressure)
        i = i + 1
    }
    
    # build dataframe of each hurricane and its min pressure
    df <- data.frame(matrix(unlist(table), nrow = length(table), byrow = T), stringsAsFactors = FALSE)
    names(df)[1] <- "Storm_Name"
    names(df)[2] <- "Timestamp"
    names(df)[3] <- "MinPressure"
    df$MinPressure<- as.numeric(df$MinPressure)
    df$Timestamp <- parse_date_time(df$Timestamp, "%m/%d/%Y", tz = 'America/Chicago', quiet = TRUE)    
    # sort in ascending order and return the ordered list of names
    df <- df[order(df$MinPressure), ]
    df
}

# make a graph for a single year's categories; assumes storm_data_list contains all storms from the same year
# year argument is just to display the graph's title
graph_category_counts = function(storm_data_list, year){
    category_map = hashmap(1:5, c(0, 0, 0, 0, 0)) # to count how many per category
    for(storm_data in storm_data_list){
        for(category in storm_data$Category){
            if (category != 0){
                category_map[[category]] = category_map[[category]] + 1
            }
        }
    }
    table = data.frame(category = category_map$keys(), count = as.integer(category_map$values()))
    
    ggplot(data = table, aes(x = category, y = count)) +
        geom_bar(stat = "identity", fill = "steelblue") + 
        labs(title = paste("Storm Categories: ", year)) +
        xlab("Category") + ylab("Count") +
        scale_x_continuous(breaks=1:5)
}


colors = c(
    "red",
    "blue",
    "gray",
    "pink",
    "purple",
    "black",
    "aqua",
    "teal",
    "yellow",
    "brown",
    "green",
    "turquoise",
    "orange",
    "fuscia",
    "white"
)
