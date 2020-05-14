library(shiny)
library(tidyverse)
library(shinydashboard)
library(maps)
library(plotly)
library(DT)
library(deSolve)
library(lubridate)

header <- dashboardHeader(title = "Coronavirus in the U.S.")


sidebar <- dashboardSidebar(
  sidebarMenu( id = "tabs",
               menuItem("United States (total)", 
                        tabName = "usa", 
                        icon = icon("th"),
                        badgeLabel = "new", badgeColor = "green"),
               
               menuItem("Regional Map",
                        tabName = "region",
                        icon =  icon("th"),
                        badgeLabel = "new",
                        badgeColor = "yellow"),
               
               menuItem("Modeling",                                    #Modeling Tab
                        tabName = "models",
                        icon = icon("th"),
                        badgeLabel = "new", badgeColor = "blue"),      #End of Modeling Tab
               
               conditionalPanel("input.tabs == 'region'",              #Placement of regional panel
                                selectInput("state",
                                            label = "Select State",
                                            choices = c("Alabama", "Arizona","Arkansas", "California", "Colorado",
                                                        "Connecticut","Delaware", "Florida","Georgia","Idaho", 
                                                        "Illinois", "Indiana","Iowa", "Kansas", "Kentucky", "Louisiana", 
                                                        "Maine","Maryland", "Massachusetts","Michigan", "Minnesota","Mississippi", 
                                                        "Missouri", "Montana","Nebraska", "Nevada", "New Hampshire","New Jersey",
                                                        "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                                                        "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
                                                        "Texas","Utah", "Vermont","Virginia", "Washington", "West Virginia",
                                                        "Wisconsin","Wyoming"),
                                            selected = "Florida"
                                )
               ),
               conditionalPanel("input.tabs == 'usa' | input.tabs == 'region'",                 #Buttons for Total United States
                                radioButtons("deaths-cases", 
                                             label = h4("Plot:"),
                                             choices = list("Deaths" = "deaths", 
                                                            "Cases" = "cases", 
                                                            "Case Fatality Rate" = "case fatality rate", 
                                                            "Cases Per Thousand" = "cases per thousand"),
                                             selected = "cases"),
                                radioButtons("log-normal",
                                             label = h4("Color Scale"),
                                             choices = list("Logarithmic" = "log", "Linear" = "line"))
                                
               ),                                      #End of Buttons for United States
               conditionalPanel("input.tabs == 'region'",
                                uiOutput("county_slider")
               ), 
               
               conditionalPanel("input.tabs == 'models'", 
                                radioButtons("county-state",
                                             label = h4("Look at County or State Models:"),
                                             choices = list("State" = "state", "County" = "county"),
                                             selected = "state"),
                                selectInput("state-model",
                                            label = "Select State",
                                            choices = c("Alabama", "Arizona","Arkansas", "California", "Colorado",
                                                        "Connecticut","Delaware", "Florida","Georgia","Idaho", 
                                                        "Illinois", "Indiana","Iowa", "Kansas", "Kentucky", "Louisiana", 
                                                        "Maine","Maryland", "Massachusetts","Michigan", "Minnesota","Mississippi", 
                                                        "Missouri", "Montana","Nebraska", "Nevada", "New Hampshire","New Jersey",
                                                        "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                                                        "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
                                                        "Texas","Utah", "Vermont","Virginia", "Washington", "West Virginia",
                                                        "Wisconsin","Wyoming"),
                                            selected = "Florida"
                                ),
                                uiOutput("state-county-dropdown"),
                                uiOutput("population_alt"),
                                #numericInput("population",
                                #            label = "Population Size",
                                #            min = 100, step = 1, value = 200000),
                                sliderInput("beta", 
                                            label = "Recovery Rate:",
                                            step = 1,
                                            value = 8,
                                            min = 1, max = 15),
                                sliderInput("predict_days",
                                            label = "Days to predict",
                                            min = 0, max = 10, step = 1, value = 3)
                                # radioButtons("model_type",
                                #             label = h4("Pick Model Type"),
                                #             choices = list("Standard SIR model"="standard", "Spline model"="spline"),
                                #             selected = "standard")
               )
  )
)

body <- dashboardBody(
  tags$head(tags$style(
    type = "text/css", 
    ".irs-grid-text {font-size: 12pt !important; transform: rotate(-90deg) translate(-30px);"
  )),
  tabItems(
    # Boxes need to be put in a row (or column)
    tabItem(tabName = "usa", #Layout for Total USA map
            fluidRow(
              infoBoxOutput(width=4, "counter.deaths"),
              infoBoxOutput(width=4, "counter.cases"),
              infoBoxOutput(width=4, "counter.tests")
            ),
            fluidRow(
              box(width=6, 
                  status="info", 
                  title="United States Coronavirus Spread",
                  solidHeader = TRUE,
                  plotlyOutput("usaplot")
              ),
              box(width=6, 
                  status="warning", 
                  title = "Raw Data",
                  solidHeader = TRUE, 
                  collapsible = FALSE, 
                  footer="Read Remotely from Github.com/nytimes/covid-19-data/",
                  DTOutput("usadata")
              )
            )                                                     #End layout for USA map
            ## Add some more info boxes
            
    ),
    
    tabItem(tabName = "region",      #Regional plots and data
            fluidRow(
              infoBoxOutput(width=4, "counter.deaths1"),
              infoBoxOutput(width=4, "counter.cases1"),
              infoBoxOutput(width=4, "counter.tests1")
            ),
            fluidRow(
              box(width = 7, 
                  status = "info",
                  title = "COVID-19 Cases by County",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  plotlyOutput("county")
              ),
              uiOutput("data_info")
              
            )),
    
    
    #      box(width = 6, 
    #          status = "warning",
    #          title = "Data Information",
    #          solidHeader = TRUE,
    #          collapsible = FALSE,
    #DTOutput("poopy")
    #          )
    
    #End Of Regional Plots and Data
    tabItem(tabName = "models",       #Modeling Tab
            fluidRow(
              box(status = "info",
                  title = "SIR Modeling of Cases in different regions",
                  solidHeader = TRUE,
                  plotOutput("state_model")),
              box(status = "info",
                  title = "Splines Modeling of Cases in different regions",
                  solidHeader = TRUE,
                  plotOutput("spline_model")
              )
              
            )
    )
  )
)


dashboardPage(header, sidebar, body)
