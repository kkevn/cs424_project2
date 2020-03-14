########################## FUNCTIONS TO HELP IN PLOTTING #####################################
########################## FUNCTIONS TO HELP IN PLOTTING #####################################

# plot multiple storm paths on a map where each circle marker's:
# - size represents wind speeds by hurricane category
# - color represents pressure
plot_multi_storm_path_by_size = function(storm_data_list) {
    
    # scaler for circle markers and zoom ranges
    marker_scale = 1.5
    z_min = 4
    z_max = 8
    
    map_object = leaflet() %>% 
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri.WorldStreetMap", options = providerTileOptions(minZoom = z_min, maxZoom = z_max)) %>%
        addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap", options = providerTileOptions(minZoom = z_min, maxZoom = z_max)) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery", options = providerTileOptions(minZoom = z_min, maxZoom = z_max))
    
    if (length(storm_data_list) == 0 ) {
        return (map_object)
    }
    
    for(i in 1: length(storm_data_list)) {
        storm_data = storm_data_list[[i]]
        
        # color palette for hurricane path by pressure
        pal <- colorNumeric(palette = "viridis", domain = storm_data$Pressure)
        #pal <- colorNumeric(palette = c("white", "yellow", "red"), domain = storm_data$Pressure) # for custom color palette
        
        # quick fix for paths running thru countries (where longitude was positive)
        # move those coordinates back a full map "cycle"
        longitude = ifelse(storm_data$Lon > 0, storm_data$Lon - 360, storm_data$Lon) #~Lon
        
        if (nrow(storm_data) == 1) { # only 1 coordinate
            map_object = map_object %>% 
                addCircleMarkers(data = storm_data, lat = ~Lat, lng = longitude, color = ~pal(storm_data$Pressure), radius = ((storm_data$Category + 1) * marker_scale), 
                                 popup = paste0("<style>
                                                    div.leaflet-popup-content {width:160% !important;}
                                                    div.leaflet-popup-content-wrapper {width:160% !important;}
                                            		table {
                                            			font-family: arial, sans-serif;
                                            			font-size: 1.25em;
                                            			border-collapse: collapse;
                                            			width: 58%;
                                            		}
                                            
                                            		h4 {
                                            		  text-align: center;
                                            		}
                                            
                                            		td, th {
                                            			border: 1px solid #dddddd;
                                            			text-align: center;
                                            			padding: 8px;
                                            		}
                                            
                                            		tr:nth-child(odd) {
                                            			font-weight: bold;
                                            		}
                                            
                                            		tr:nth-child(even) {
                                            			background-color: #dddddd;
                                            		}
                                            		</style>
                                                        <h2>", storm_data$Storm_Name, "</h2>",
                                                            "<table>
                                            					<tr>
                                            						<th>Category</th>
                                            						<th>Latitude , Longitude</th>
                                            						<th>Speed (kt)</th>
                                            						<th>Pressure (mb)</th>
                                            					</tr>
                                            					<tr>
                                            						<td>", storm_data$Category, "</td>
                                            						<td>", storm_data$Lat, " , ", storm_data$Lon, "</td>
                                            						<td>", storm_data$Speed, "</td>
                                            						<td>", storm_data$Pressure, "</td>
                                                                </tr>
                                            				</table>
                                            			<h4>", storm_data$Timestamp, "</h4>"),
                                    popupOptions = popupOptions(minWidth = "100%", maxWidth = "100%", closeOnClick = TRUE)) 
        } else {
            map_object = map_object %>% 
                addPolylines(data = storm_data, lat = ~Lat, lng = longitude, opacity = "0.05", color = "white", weight = (storm_data$Category * 1), label = storm_data$Storm_Name, labelOptions = labelOptions(textsize = "24px"), highlightOptions = highlightOptions(bringToFront = TRUE, stroke = TRUE, weight = 5, opacity = "0.75", color = "white")) %>%
                addCircleMarkers(data = storm_data, lat = ~Lat, lng = longitude, color = ~pal(storm_data$Pressure), radius = ((storm_data$Category + 1) * marker_scale), 
                                 popup = paste0("<style>
                                                    div.leaflet-popup-content {width:160% !important;}
                                                    div.leaflet-popup-content-wrapper {width:160% !important;}
                                            		table {
                                            			font-family: arial, sans-serif;
                                            			font-size: 1.25em;
                                            			border-collapse: collapse;
                                            			width: 58%;
                                            		}
                                            
                                            		h4 {
                                            		  text-align: center;
                                            		}
                                            
                                            		td, th {
                                            			border: 1px solid #dddddd;
                                            			text-align: center;
                                            			padding: 8px;
                                            		}
                                            
                                            		tr:nth-child(odd) {
                                            			font-weight: bold;
                                            		}
                                            
                                            		tr:nth-child(even) {
                                            			background-color: #dddddd;
                                            		}
                                            		</style>
                                                        <h2>", storm_data$Storm_Name, "</h2>",
                                                            "<table>
                                            					<tr>
                                            						<th>Category</th>
                                            						<th>Latitude , Longitude</th>
                                            						<th>Speed (kt)</th>
                                            						<th>Pressure (mb)</th>
                                            					</tr>
                                            					<tr>
                                            						<td>", storm_data$Category, "</td>
                                            						<td>", storm_data$Lat, " , ", storm_data$Lon, "</td>
                                            						<td>", storm_data$Speed, "</td>
                                            						<td>", storm_data$Pressure, "</td>
                                                                </tr>
                                            				</table>
                                            			<h4>", storm_data$Timestamp, "</h4>"),
                                 popupOptions = popupOptions(minWidth = "100%", maxWidth = "100%", closeOnClick = TRUE))
        }
    }
    
    # add legend, layer control, and zoom reset onto map
    map_object = map_object %>%
        addLegend(pal = pal, values = storm_data$Pressure, opacity = 0.7, title = "Pressure (mb)", position = "bottomright") %>%
        addLayersControl(baseGroups = c("Esri.WorldStreetMap", "OpenTopoMap", "Esri.WorldImagery"), options = layersControlOptions(collapsed = FALSE))
        
        # reset map to Atlantic ocean of Atlantic data
        if (substr(storm_data$Unique_ID, 0, 2) == "AL") {
            map_object = map_object %>% addEasyButton(easyButton(title = "Reset zoom", icon = "fa-undo", onClick = JS("function(btn, map){ map.setZoom(4);map.flyTo([30, -40]);}")))
        }
        # otherwise reset to Pacific ocean
        else {
            map_object = map_object %>% addEasyButton(easyButton(title = "Reset zoom", icon = "fa-undo", onClick = JS("function(btn, map){ map.setZoom(4);map.flyTo([20, -150]);}")))
        }
    map_object
}

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

fix_pressures = function(pressures){
    to_vec(
        for (pressure in pressures) 
            if (pressure < 0)
                0
        else 
            pressure
    )
}

# get a table of hurricanes top speeds and min pressures
get_storm_names_max_speed_min_pressure_table = function(storm_data_list) {
    table = list()
    i = 1
    for (storm_data in storm_data_list) {
        top_speed = max(storm_data$Speed)
        top_speed_index = which(storm_data$Speed == top_speed)[1] # store index to get date associated date with speed 
        min_pressure = min(fix_pressures(storm_data$Pressure)) 
        table[[i]] = c(storm_data$Storm_Name[1], strftime(storm_data$Timestamp[top_speed_index], "%m/%d/%Y"), top_speed, min_pressure)
        i = i + 1
    }
    
    # build dataframe of each hurricane and its top speed
    df <- data.frame(matrix(unlist(table), nrow = length(table), byrow = T), stringsAsFactors = FALSE)
    names(df)[1] <- "Storm_Name"
    names(df)[2] <- "Timestamp"
    names(df)[3] <- "TopSpeed"
    names(df)[4] <- "MinPressure"
    df$TopSpeed <- as.numeric(df$TopSpeed)
    df$MinPressure <- as.numeric(df$MinPressure)
    df$Timestamp <- parse_date_time(df$Timestamp,"%m/%d/%Y", tz = 'America/Chicago', quiet = TRUE)    
    
    # no need to sort; graphing auto sorts the x
    df
}


# make a graph for a single year's categories; assumes storm_data_lists contains all storms from the same year
# year argument is just to display the graph's title
graph_category_counts = function(pacific_list, atlantic_list, year){
    category_map1 = hashmap(1:5, c(0, 0, 0, 0, 0)) # to count how many per category
    category_map2 = hashmap(1:5, c(0, 0, 0, 0, 0)) # to count how many per category
    
    for(storm_data in pacific_list){
        for(category in storm_data$Category){
            if (category != 0){
                category_map1[[category]] = category_map1[[category]] + 1
            }
        }
    }
    
    for(storm_data in atlantic_list){
        for(category in storm_data$Category){
            if (category != 0){
                category_map2[[category]] = category_map2[[category]] + 1
            }
        }
    }
    
    table = data.frame(category = c(category_map1$keys(), category_map1$keys()), 
                       count = c(as.integer(category_map1$values()), as.integer(category_map2$values()))
    )
    table$ocean = c(rep("PACIFIC", 5), rep("ATLANTIC", 5)) # column to do grouped bar
    
    ggplot(data = table, aes(x = category, y = count, fill = ocean)) +
        geom_bar(stat = "identity", position = "dodge") + 
        labs(title = paste("Given Year: ", year)) +
        xlab("Category") + ylab("Count") +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), 
              axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
              plot.title = element_text(size = 18)) +
        scale_x_continuous(breaks=1:5)
}

