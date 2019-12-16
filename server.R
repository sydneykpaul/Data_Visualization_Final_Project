###########################################
## Title: Data Visualization Final Project
## Authors: Sam Anderson, Sydney Paul, Kenneth Shelley
## Date Created: 12/6/2019 
## 
## Description: 
## - server.R file
## - Community guide to the South Bend area
###########################################


# Load necessary libraries
library(shiny)
library(shinyWidgets)
library(plotly) 
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(ggthemes)
library(rgdal)
library(data.table)
library(tidyverse)


shinyServer(function(input, output, session) {
        
        
# ------------------------------------  Public Services Tab ------------------------------------ #
# Author: Sam 
        # Read in facilities data
        facilities <- read.csv("Public_Facilities.txt")
        # Load the base district map I want to plot the facilities ontop of
        my_dic <- readOGR(dsn = "City_Council_Districts.shp",
                          stringsAsFactors = F)
        
        
        
        facilities.spatial <- SpatialPointsDataFrame(coords = facilities[, c("Lon", "Lat")],
                                                     data = facilities,
                                                     proj4string =CRS(proj4string(my_dic)))
        facilities.spatial$popup <- paste("<b>", facilities.spatial$POPL_NAME, "</b><br>")
        
        colorPal <- colorFactor(palette = c("red", "green", "blue"), domain = facilities$POPL_TYPE)
        
        #Determine which facilities are in each area
        myov <- over(facilities.spatial, my_dic)
        
        facilities.spatial@data$Dist <- myov$Dist 
        output$map2 <- renderLeaflet({
                
                
                
                #create leaflet while filtering to the two reqiured input variables
                leaflet(facilities.spatial)  %>%
                        addTiles()  %>%
                        addLayersControl(overlayGroups = facilities,
                                         options = layersControlOptions(collapsed = TRUE))%>%
                        addPolygons(data = my_dic[my_dic@data$Dist == input$variable2, ], fill = F) %>%
                        addCircleMarkers(data = facilities.spatial@data[facilities.spatial@data$POPL_TYPE %in% input$variable1 & facilities.spatial@data$Dist == input$variable2,], popup = ~popup,
                                         color = ~colorPal(POPL_TYPE), fillOpacity = 1, fillColor = ~colorPal(POPL_TYPE))}) 
        
        
        
        
# -----------------------------------  Public Facilities Tab ----------------------------------- #
# Author: Ken 
        
        #load parks data
        park_locations <- read_csv("Parks_Locations_and_Features.csv")
        
        #create districts map
        cc_districts <- readOGR(dsn= "City_Council_Districts.shp",
                                stringsAsFactors = F)
        
        #user filtered data to create spatial dataframe
        park_spatial <- SpatialPointsDataFrame(coords = park_locations[,c("Lon","Lat")], data = park_locations,
                                               proj4string = CRS(proj4string(cc_districts)))
        
        #create pop-ups (expand once app is working)
        park_spatial$popup <- paste("<b>",park_spatial$Park_Name,"</b><br>",
                                    "Play Ground: ",park_spatial$Playground__Local,"<br>")
        
        #cc_color <- colorFactor(palette = c("gold", "orchid"), domain = cc_districts$Dist)
        ov <- over(park_spatial, cc_districts)
        
        park_spatial@data$Dist <- ov$Dist  
        
        output$map <- renderLeaflet({
                
                cc_dist_filter <- cc_districts[cc_districts@data$Dist == input$variable, ]
                
                #create leaflet
                leaflet(park_spatial)  %>%
                        addTiles()  %>%
                        addMarkers(data = park_spatial@data[park_spatial@data$Dist==input$variable, ], popup = ~popup)%>%
                        addLayersControl(overlayGroups = park_locations,
                                         options = layersControlOptions(collapsed = TRUE))%>%
                        addPolygons(data = cc_dist_filter)
                # %>%
                #   fitBounds()
                
        }) # end of map output
        
        parks_df <- as.data.frame(park_spatial)
        
        output$plot <- renderTable(parks_df %>% 
                                           filter(parks_df$Dist == input$variable) %>%
                                           select(Park_Name, Park_Type))
        
        
        
        
# ----------------------------------------  Lights Tab ----------------------------------------- #
# Author: Sydney 
        
        
        ############### SET UP ############### 
        
        # load the RDS file created in `geocoding_script.R` / `cleaning_business_data_script.R`
        all_businesses <- readRDS("business_clean.rds")
        businesses2018 <- filter(all_businesses, year == 2018)
        
        # save now to don't have to recalculate every time
        unique_category_choices <- sort(unique(businesses2018$category))
        unique_name_choices <- sort(unique(businesses2018$name))
        colorblind_colors <- c("#0072B2", "#009E73", "#CC79A7")  # from ggthemes colorblind palette
        
        
        # load the street lights dataset
        street_lights <- fread("Street_Lights.csv")
        
        
        # intialize map with hidden lights so doesn't re-plot them every time (and keeps user zoom)
        output$downtown_map <- renderLeaflet({
                leaflet()  %>%
                        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                        leaflet.extras::addHeatmap(data = street_lights,
                                                   lng = ~Lon, 
                                                   lat = ~Lat,
                                                   group = "street_lights",
                                                   gradient = "#FFF39E",
                                                   radius = 2,
                                                   blur = 3) %>%
                        hideGroup("street_lights")
                
        })
        
        
        
        ############### OBSERVE EVERYTHING THE USER DOES AND UPDATE ############### 
        
        # update map at light switch toggle
        observeEvent(input$light_switch, {
                if(input$light_switch) {
                        leafletProxy("downtown_map") %>%
                                addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
                                hideGroup("street_lights")
                        
                } else {
                        leafletProxy("downtown_map") %>%
                                addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
                                showGroup("street_lights")
                }
        })
        
        
        # update selectize options server-side at user input
        observeEvent(input$type_of_filter, {
                updateSelectizeInput(session = session,
                                     inputId = 'user_selected_names', 
                                     choices = switch(input$type_of_filter,
                                                      "category" = unique_category_choices,
                                                      unique_name_choices),
                                     options = list(placeholder = "Start typing name",
                                                    maxItems = 3),
                                     server = TRUE)
        })
        
        
        # update map with businesses markers
        observe({
                
                plot_data <- getPlotData()
                
                # clear old markers and controls
                proxy_map <- leafletProxy("downtown_map") %>%
                        clearGroup("businesses") %>%
                        clearControls()
                
                
                if(nrow(plot_data) == 0) {
                        # no data to plot, do nothing
                        
                } else {
                        # stop it from zooming out so far because of outliers
                        if ((n_distinct(plot_data$lon) == 1) & (n_distinct(plot_data$lat) == 1)) {
                                proxy_map <- proxy_map %>%
                                        flyTo(lng = plot_data$lon[1], 
                                              lat = plot_data$lat[1], 
                                              zoom = 15)
                                
                        } else {
                                bds <- getSmallerBounds()
                                proxy_map <- proxy_map %>%
                                        flyToBounds(lng1 = bds[["lng1"]],
                                                    lat1 = bds[["lat1"]],
                                                    lng2 = bds[["lng2"]],
                                                    lat2 = bds[["lat2"]])
                        }
                        
                        
                        # add new markers
                        filter_pal <- getPalette()
                        
                        proxy_map <- proxy_map %>%
                                addCircleMarkers(data = plot_data,
                                                 lng = ~lon, 
                                                 lat = ~lat,
                                                 group = "businesses",
                                                 color = ~filter_pal(get(input$type_of_filter)),
                                                 popup = ~paste0("<b>", name, "</b><br>", address),
                                                 label = ~name, 
                                                 radius = 7,
                                                 stroke = 0,
                                                 fillOpacity = 0.5)
                        
                        # add legend
                        proxy_map <- proxy_map %>%
                                addLegend(data = plot_data, 
                                          title = paste0("Year: ", 
                                                         ifelse(is.null(input$year_slider), 
                                                                2018, 
                                                                input$year_slider)),
                                          position = 'bottomright',
                                          pal = filter_pal, 
                                          values = ~get(input$type_of_filter), 
                                          opacity = 0.5)
                        proxy_map
                }
        })
        
        
        # add slider if box is checked
        output$slider_placeholder <- renderUI({
                if (input$time_checkbox) {
                        sliderInput(inputId = "year_slider",
                                    label = "Choose year:",
                                    min = 2008,
                                    max = 2018,
                                    value = 2018,
                                    step = 1,
                                    sep = "",
                                    round = TRUE,
                                    ticks = FALSE,
                                    animate = animationOptions(interval = 1000),
                                    width = '100%')
                }
        })
        
        
        ############### DATA MANIPULATION ############### 
        
        getPlotData <- reactive({
                if (!input$time_checkbox) {
                        plot_data <- businesses2018 %>%
                                filter_at(.vars = c(input$type_of_filter), 
                                          .vars_predicate = all_vars(. %in% input$user_selected_names))
                } else {
                        selected_year <- ifelse(is.null(input$year_slider), 2018, input$year_slider)
                        plot_data <- all_businesses %>%
                                filter(year == selected_year) %>%
                                filter_at(.vars = c(input$type_of_filter),
                                          .vars_predicate = all_vars(. %in% input$user_selected_names))
                }

                plot_data
        })
        
        getSmallerBounds <- reactive({
                plot_data <- getPlotData()
                all_names_bounds <- data.frame()
                
                for (one_name in input$user_selected_names) {
                        subset_data <- filter(plot_data, get(input$type_of_filter) == one_name)
                        
                        if (nrow(subset_data) == 1) {
                                bounds <- subset_data %>% 
                                        summarise(lng1 = min(lon),
                                                  lat1 = min(lat),
                                                  lng2 = max(lon),
                                                  lat2 = max(lat))
                        } else {
                                bounds <- boundsRemoveOutliers(subset_data)
                        }
                        
                        all_names_bounds <- rbind(all_names_bounds, bounds)
                }
                
                all_names_bounds %>% 
                        summarise(lng1 = min(lng1),
                                  lat1 = min(lat1),
                                  lng2 = max(lng2),
                                  lat2 = max(lat2))
        })
        
        
        getPalette <- reactive({
                colorFactor(palette = colorblind_colors[1:length(input$user_selected_names)], 
                            levels = input$user_selected_names, 
                            ordered = T)
        })
        
        boundsRemoveOutliers <- function(subset_data) {
                lon_cutoffs <- mean(subset_data$lon) + c(-3, 3) * sd(subset_data$lon)
                lat_cutoffs <- mean(subset_data$lat) + c(-3, 3) * sd(subset_data$lat)
                
                bounds <- subset_data %>%
                        filter(lon_cutoffs[1] < lon, lon < lon_cutoffs[2],
                               lat_cutoffs[1] < lat, lat < lat_cutoffs[2]) %>%
                        summarise(lng1 = min(lon),
                                  lat1 = min(lat),
                                  lng2 = max(lon),
                                  lat2 = max(lat))
                return(bounds)
        }
        
}) # end of shinyServer
