library(shiny)
library(tidyverse)
library(shinydashboard)
library(maps)
library(plotly)
library(DT)
library(deSolve)

header <- dashboardHeader(title = "United States Coronavirus")


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
                                  choices = list("Deaths" = "deaths", "Cases" = "cases", "Case Fatality Rate" = "case fatality rate"),
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
                     numericInput("population",
                                  label = "Population Size",
                                  min = 100, step = 1, value = 80000),
                     sliderInput("beta", 
                                 label = "Recovery Rate: (8 days)",
                                 step = 1,
                                 value = 8,
                                 min = 1, max = 15)
                     
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
    tabItem(tabName = "usa",                              #Layout for Total USA map
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
        box(width = 6, 
            status = "info",
            title = "Options",
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("county")
            )
        ),
      fluidRow(
        uiOutput("data_info")
        
  #      box(width = 6, 
  #          status = "warning",
  #          title = "Data Information",
  #          solidHeader = TRUE,
  #          collapsible = FALSE,
            #DTOutput("poopy")
  #          )
        )
      ),                              #End Of Regional Plots and Data
    tabItem(tabName = "models",       #Modeling Tab
            box(status = "info",
                title = "Modeling of Cases in different regions",
                solidHeader = TRUE,
                plotOutput("state_model"))
    )
  )
)


dashboardPage(header, sidebar, body)
