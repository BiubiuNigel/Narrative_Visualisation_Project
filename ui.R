# R- SHiny narrative visaulization project - ui 
# FIT5147
# Author: Yixiao CHen
# Student ID: 31968376
# Date: 2021.6.5

# Import library
library(shiny)
library(DT)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(imputeTS)
library(leaflet)

# Start the UI
shinyUI(fluidPage(
    
    # Set the background image
    setBackgroundImage(src = "https://img.freepik.com/free-photo/hand-painted-watercolor-background-with-sky-clouds-shape_24972-1095.jpg?size=626&ext=jpg"),
    
    # Set the theme
    theme = "bootstrap.css",
    includeCSS("www/styles.css"),
    
    # Using nav bar page so that each page can be selected
    navbarPage(
        
        #Set title
        "Property Sale Price and School Ranking",
        id = "main_navbar",
        
        tabPanel(
            
            # First page
            "Property Sale", icon = icon("area-chart"), value = "trend",
            
            # Set date for user
            fluidRow(column(
                5,uiOutput("currentDate",container = span)
            )
            ),
            
            # Start the interactive 
            fluidRow(column(3,
                h3("Top Property Sale Suburbs(Million dollars)"),
                
                # Use selection bar for selecting from top 10 to top 50
                selectInput(
                    "top_property",
                    NULL,
                    choice = c(
                        "Top 10" = 10,
                        "Top 20" = 20,
                        "Top 30" = 30,
                        "Top 40" = 40,
                        "Top 50" = 50
                    ),
                    selected = 50
                )
            ),
            
            # The radio button for select either average or total sale
            column(2,
                   radioButtons(inputId = "pro_ave_sum",
                                label = "Ranking selection",
                                choices = c("Average","Total"))),
            
            # Draw the title
            column(7,uiOutput("pro_map_title"))),
            
            # Draw the circular plot and the map
            fluidRow(column(3,plotOutput("circular_plot",width = "100%",height="400px")),
                     column(9,leafletOutput("property_map"))),
            
            # Set the instruction and check box for the line plot 
            fluidRow(column(3,
                            h3("Data includes the sale price from Sept,2018 to April 2020"),
                            h3("The top sale suburb for average price is Castlecrag and for the total sale is Mosman"),
                            h3('*The circular plot on above shows the sale price decreasing'),
                            h3('*The map shows the location of the suburbs'),
                            h3('*The line plot on the right illustrates the trend of property sale number each year')),
                     column(9,
                         br(),
                         h3("Property sale count over month",style = "text-align:center"),
                         checkboxGroupInput(
                         inputId = "year_check_pro",
                         label = "Year Selection",
                         choices = c('2018','2019','2020'),
                         inline = TRUE,
                         selected = 2018
                     ),
                     plotOutput("line_peoperty")))

        ),
        
        # Second page for school ranking
        tabPanel("School Ranking",icon = icon("signal"), value = "rank",
                 fluidRow(column(4,
                                 h3("Select ICSEA Score Level"),
                                 h4("(ICSEA stands for the Index of Community Socio-Educational Advantage)"),

                                 # Set the slider bar from 1150 to 1200
                                 sliderInput("icsea_level",label=NULL,
                                             min = 1150, max = 1200,value = 1150,step = 3)
                                 ),
                          column(2,   
                                 h3("Select School Type"),
                                 
                                 # Set the radio buttons for select either primary or secondary
                                 radioButtons(inputId = "school_type",
                                              label = "School Type selection",
                                              choices = c("Primary","Secondary"))),
                          column(6,br(),br(),uiOutput("school_title"))),
                 
                 # Get the ranking plot and map
                 fluidRow(column(4,plotOutput('lol_ranking')),
                          column(8,leafletOutput("school_map"))),
                 
                 # Get the data table and instruction
                 fluidRow(column(4,
                                 h3("Data included the primary and secondary school information"),
                                 h3("The highest score for primary school is 1216 which located at Wahroonga"),
                                 h3("The highest score for secondary school is 1242 which located at Darlinghurst")),
                          column(8,
                                 br(),
                                 br(),
                                 h3("Schoole details",style = "text-align:center"),
                                 DT::dataTableOutput("school_details")))

            
        ),
        
        # Set the third page
        tabPanel("School and Property",icon = icon("list-ul"), value = "summary",
                 p("This page shows the relationship between the property sale price and School ranking.
                   From previous two pages, the highest sale price suburb has been found out and suburb that has the highest 
                   ICSEA score has been found out as well.Whether these two suburb are the same one and whether
                   top 10 are the same one? This page will show",style = "font-size:30px; text-align:center"),
                 hr( ),
                 
                 # Set the side bar panel to do the control 
                 pageWithSidebar(
                     headerPanel('Filter'),
                     sidebarPanel(width = 4,
                                  sliderInput("icsea_level_third",'ICSEA Score',
                                              min = 1150, max = 1200,value = 1150,step = 3),
                                  sliderInput("top_property_third",'School top',
                                              min = 10, max = 50,value = 50,step = 5),
                                  radioButtons(inputId = "school_type_third",
                                               label = "School Type selection",
                                               choices = c("Primary","Secondary"))
                                
                                  ),
                     
                     # With the main map controled by the side panel 
                     mainPanel(column(12,leafletOutput("school_pro_map"),
                                      br(),
                                      br(),
                                      p("By selecting the ICSEA Score, School Top and the School Type, it is easily to find out the relationship.
                                 For the primary school, suburbs whose ICSEA Scores are above 1200 are not the same as top 10 property sales.
                                 For the secondary, no same suburb as well. From the map, there are no big relationship between
                                 school ranking and property sale price",style = "font-size : 25px"))
                               )
                 )
                 
                 ),
        
        # Fourth page for the about information
        tabPanel("About",icon = icon("info-circle"),
                 fluidRow(column(2),
                          column(8,br(),br(),br(),br(),br(),br(),
                          p("This is the data analysis for the customer who is looking for the high ICSEA score school
                                     and the customer who is looking for the best property sale suburb",style = "font-size:40px; text-align:center"),
                          br(),
                          p("It shows the top 50 suburbs for the property sale and school ranking as well",style = "font-size:40px; text-align:center"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          p("Author: Yixiao Chen",style = "font-size:25px; text-align:center"),
                          p("Student ID: 31968376 ",style = "font-size:25px; text-align:center"),
                          p("Date:2021.6.4",style = "font-size:25px; text-align:center")))
                 )
      
    )
 
))

