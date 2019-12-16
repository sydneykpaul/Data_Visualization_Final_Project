###########################################
## Title: Data Visualization Final Project
## Authors: Sam Anderson, Sydney Paul, Kenneth Shelley
## Date Created: 12/6/2019 
## 
## Description: 
## - ui.R file
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


navbarPage(
        theme = shinytheme("flatly"),
        title = "South Bend Community Guide",
        id = "navbar",
        
        
# ----------------------------------------  Welcome Tab ---------------------------------------- #
# Author: Sydney 

        tabPanel(
                title = "Introduction",
                value = "introduction_tab",
                
                img(src = "background_collage_wide.png", width = '100%'),
                
                br(),
                br(), 
                
                h1("The City of South Bend"),
                
                h4("Sam Anderson, Sydney Paul, Kenneth Shelley"),
                h4("December 15th, 2019"),
                
                br(),
                br(),
                
                fluidRow(
                        column(width = 3, 
                               h4("Purpose:"),
                               tags$ul(
                                       tags$li("Explore South Bend's public facililites, public parks, and businesses."),
                                       tags$li("See how different datasets can aggregated together to get a better understanding of the city as a whole."),
                               )
                        ),
                        column(width = 4, 
                               h4("Data Source: City of South Bend's Open Data Portal"),
                               tags$ul(
                                       tags$li("Business licenses"),
                                       tags$li("Locations of street lights"),
                                       tags$li("Parks"),
                                       tags$li("Public facilities"),
                                       tags$li("City Council districts")
                               )
                        )
                )
                
                
        ), # end of welcome tab
        


        
# ------------------------------------  Public Services Tab ------------------------------------ #
# Author: Sam 
        
        tabPanel(
                title = "Fun with Facilities in the Bend",
                value = "public_services_tab",
                
                titlePanel("Facilities"),
                
                fluidPage(
                        fluidRow(
                                column(2,
                                       checkboxGroupInput("variable1",
                                                   "Facilities",
                                                   choiceNames = c("FIRE STATION", "LIBRARY", "POLICE STATION"), 
                                                   choiceValues = c("FIRE STATION", "LIBRARY", "POLICE STATION"),
                                                   selected = "FIRE STATION"),
                                       selectInput("variable2",
                                                   "District",
                                                   choices = c('1301', '1302', '1302', '1304', '1305', '1306'),
                                                   selected = '1301')
                                )
                        ),
                        column(width = 10, offset =2,
                               leafletOutput("map2"))
                )
                
                
        ), # end of public services tab
        
        
        

# -----------------------------------  Public Facilities Tab ----------------------------------- #
# Author: Ken 
        
        tabPanel(
                title = "Parks of South Bend",
                value = "public_facilities_tab",
                
                titlePanel("Parks"),
                
                fluidPage(
                        
                        fluidRow(
                                column(2,
                                       selectInput("variable", 
                                                   "Districts", 
                                                   choices = c('1301', '1302', '1302', '1304', '1305', '1306'),
                                                   selected = "1301"
                                       )
                                ), 
                                
                                # mainPanel -------------------------
                                column(width = 10, offset = 2,
                                       # Map of parks
                                       leafletOutput("map")
                                       
                                       
                                ) # end of sidebarLayout
                        ) # end of page with sidebar + map
                ),
                fluidRow(
                        column(2,
                               "Park Names"
                        ),
                        column(10,
                               tableOutput("plot")
                        )
                )
        ), # end of public facilities tab
        
        
        
        
# ----------------------------------------  Lights Tab ----------------------------------------- #
# Author: Sydney 
        
        tabPanel(
                title = "Downtown is lit!",
                value = "lights_tab",
                
                fluidRow(
                        column(width = 6,
                               h4("As we enter winter, days are growing shorter,
                      but this shouldn't stop you from enjoying everything that downtown has to offer!
                      South Bend is very well-lit, but don't just take our word for it. 
                      Use the tool below to see for yourself."),
                               h4("Either select a category of businesses, or search for a specific one that you have in mind. 
                      Then flip the switch and just see how lit that area is!")
                        )
                ), 
                fluidRow(
                        column(width = 3, 
                               h4("You may compare up to three categories or individual businesses. 
                            Hover over a location to see it's name, or click it to see its name and address.")
                        ),
                        
                        column(width = 9,
                               br(),
                               br(),
                               br(),
                               materialSwitch(inputId = "light_switch",
                                              label = span(style = "font-size: 24px", "Turn on/off the sun"),
                                              status = "primary",
                                              value = TRUE,
                                              width = "100%")
                        ),
                ),
                
                hr(),
                
                sidebarLayout(
                        sidebarPanel(width = 3,
                                     radioButtons(inputId = "type_of_filter",
                                                  label = "I want to select a:",
                                                  choices = list("category of businesses" = "category",
                                                                 "business by name" = "name"),
                                                  selected = "category", 
                                                  width = '100%'), 
                                     
                                     br(),
                                     
                                     
                                     selectizeInput(inputId = 'user_selected_names',
                                                    label = "Available options:",
                                                    choices = NULL,
                                                    multiple = TRUE,
                                                    width = '100%'), 
                                     
                                     br(),
                                     
                                     checkboxInput(inputId = "time_checkbox", 
                                                   label = "Check this box to go back in time.", 
                                                   value = FALSE, 
                                                   width = '100%'),
                                     
                                     uiOutput("slider_placeholder")
                                     
                                     
                        ), # end of sidebarPanel
                        
                        mainPanel(column(width = 12, offset = 0,
                                         leafletOutput("downtown_map", height = 500))
                                  
                        ) # end of mainPanel
                ), # end of sidebarLayout
        ) # end of the lights tab
) # end of navbarPage
