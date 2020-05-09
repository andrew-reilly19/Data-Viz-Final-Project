library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(tidyverse)
library(maps)
#install.packages("mapproj")
library(mapproj)
library(plotly)
library(DT)
library(deSolve)
library(cowplot)

shinyServer(function(input, output, session) {
  #output$menu <- renderMenu({
    
  #})
  observeEvent(input$`deaths-cases` == "case fatality rate" & input$`log-normal` == "log",{
    if(input$`deaths-cases` == "case fatality rate" & input$`log-normal` == "log")
               showModal(
                 modalDialog(
                  title = "Important Message about this plot!",
                  "Because of Logarithmic scaling, this is a bad plot to show, as the logarithm of case fatality rates will all be negative"
                            )
                        )
                  }
               )
  
  output$data_info <- renderUI({
    if(input$state == "Virginia"){
      object <- box(width = 4, 
                    status = "warning",
                    title = "Warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    h4("Virginia has over 60 cities what are independent of county localities."),
                    h5("This means that, currently, this graph strongly underrepresents reported COVID19 cases in the counties plotted.")
                    )
    }
    if(input$state == "New York"){
      object <- box(width = 4, 
                    status = "warning",
                    title = "Warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    h4("Currently the following Counties of New York (City) are combined:"),
                    h5("Bronx, Queens, New York County/Manhatten, and Richmond. For plotting purposes, I've included the aggregate data in each plot")
      )
    } else {
      object <- box(width = 4,
                    status = "info",
                    title = "Information",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    h4("The logarithmic scaling gives an indication of how far away (in terms of a tenfold increase of cases) different counties are from each other. This assumes that the populations of counties are the same, and the infection rates are the same between counties."))
    }
  })
               
            
  
  county_data <- reactiveFileReader(
    intervalMillis = 10000,
    session = session,
    filePath = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
    readFunc = read_csv
  )
  
  
  state_data <- reactiveFileReader(
          intervalMillis = 10000, 
          session = session,
          filePath = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
          readFunc = read_csv)
  
  corona <- reactive({
  corona <- county_data()
  `%nin%` = Negate(`%in%`)
  #print(input$date)
  corona <- corona %>% #filter(state == input$state) %>% 
    mutate(county = tolower(county), 
           state = tolower(state),
    ) %>% 
    filter(state %nin% setdiff(unique(tolower(corona$state)), unique(map_data("county")$region)) )
  corona <- corona %>% mutate(county = fct_recode(county, 
                                                        `de kalb` = "dekalb",
                                                        `dona ana` = "doña ana",
                                                        `du page` = "dupage",
                                                        `la porte` = "laporte",
                                                        `la salle` = "lasalle",
                                                        `new york` = "new york city",
                                                        `obrien` = "o'brien",
                                                        `prince georges` = "prince george's",
                                                        `queen annes` = "queen anne's",
                                                        `st bernard` = "st. bernard",
                                                        `st charles` = "st. charles",
                                                        `st clair` = "st. clair",
                                                        `st francis` ="st. francis",
                                                        `st francois` = "st. francois",
                                                        `st helena` = "st. helena",
                                                        `st james` = "st. james",
                                                        `st john the baptist` = "st. john the baptist",
                                                        `st johns` = "st. johns",
                                                        `st joseph` = "st. joseph",
                                                        `st landry` = "st. landry",
                                                        `st lawrence` = "st. lawrence",
                                                        `st louis` =  "st. louis",
                                                        `st louis city` = "st. louis city",
                                                        `st lucie` = "st. lucie",
                                                        `st martin` = "st. martin",
                                                        `st mary` = "st. mary",
                                                        `st marys` = "st. mary's",
                                                        `st tammany` = "st. tammany",
                                                        `ste genevieve` = "ste. genevieve"),
                                    log_cases = log(cases, base = 10),
                                    log_deaths = log(deaths, base = 10),
                                    log_difference = log_deaths - log_cases,
                                    proportion = deaths/cases)
  adjustment <- NULL
  for (i in c("richmond", "kings", "queens", "bronx")){
    tingle <- corona[as.vector(corona["county"] == "new york"),] %>% mutate(county = i)
    adjustment <- rbind(adjustment, tingle)
  }
  corona <- rbind(adjustment, corona)
  return(corona)
  })
  
  output$usadata <-renderDT({
    
    state_data() %>% 
      arrange(desc(cases), state) %>% 
      mutate(cases = as.integer(cases), deaths) })
  
  output$usaplot <- renderPlotly({                              #Beginning of Plotly Renderings For United States.
    statedata <- state_data()
    statedata <- statedata %>% mutate(state_alt = setNames(state.abb,state.name)[state], 
                                      date = as.numeric(as.Date(date))-18282,
                                      #cases = ifelse(state == "New York"| state == "NY" | state == "NJ", 0, cases),
                                      log_cases = ifelse(cases == 0,0, log(cases)),
                                      log_deaths = ifelse(deaths == 0, 0 , log(deaths)),
                                      log_difference = log_deaths - log_cases,
                                      proportion = deaths/cases
                                      )
    #statedata$hover <- with(statedata, paste(state, "<br>", "Total Cases:", cases, "<br>", "Logarithm Scale:", round(log_cases, digits = 3), "<br>", "Deaths:", deaths))
    
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    #fig <- plot_geo(statedata, locationmode = "USA-states")
    if(input$`deaths-cases` == "deaths"){
      if(input$`log-normal` == "log"){
        statedata$hover <- with(statedata, 
                                paste(state, "<br>", 
                                      "Total Cases:", cases, "<br>", 
                                      "Logarithm Scale:", round(log_cases, digits = 3), 
                                      "<br>", "Total Deaths:", deaths))
        fig <- plot_geo(statedata, locationmode = "USA-states")
        fig <- fig %>% add_trace(
              z = ~log_deaths, text = ~hover, locations = ~state_alt, color = ~log_deaths, frame = ~date
              ) %>% layout(geo = g, title = paste("United States Coronavirus", input$`deaths-cases`, "on a", input$`log-normal`, "scale"))
      } else { #deaths, normal representation
          statedata$hover <- with(statedata, 
                                paste(state, "<br>", 
                                      "Total Cases:", cases, "<br>", 
                                      #"Logarithm Scale:", round(log_cases, digits = 3), 
                                      "<br>", "Total Deaths:", deaths))
          fig <- plot_geo(statedata, locationmode = "USA-states")
          fig <- fig %>% add_trace(
            z = ~deaths, text = ~hover, locations = ~state_alt, color = ~deaths, frame = ~date
          ) %>% layout(geo = g, title = paste("United States Coronavirus", input$`deaths-cases`, "on a", input$`log-normal`, "scale"))
      }
    } else { # Must have selected cases or CFR
      if(input$`deaths-cases` == "case fatality rate"){
        if(input$`log-normal` == "log"){
          statedata$hover <- with(statedata, 
                                  paste(state, "<br>", 
                                        "Total Cases:", cases, "<br>", 
                                        "Logarithm Scale:", round(log_cases, digits = 3), 
                                        "<br>", "Total Deaths:", deaths, "<br>",
                                        "log(CFR):", log_difference, "<br>",
                                        "Case Fatality Rate:", paste(round(proportion, digits = 3)*100, "%", sep = "")
                                        )
                                  )
          fig <- plot_geo(statedata, locationmode = "USA-states")
          fig <- fig %>% add_trace(
            z = ~log_difference, text = ~hover, locations = ~state_alt, color = ~log_difference, frame = ~date
          ) %>% layout(geo = g, title = paste("United States Coronavirus", input$`deaths-cases`, "on a", input$`log-normal`, "scale"))
        } else { # Display cases without a logarithmic scale
          statedata$hover <- with(statedata, 
                                  paste(state, "<br>", 
                                        "Total Cases:", cases, "<br>", 
                                        #"Logarithm Scale:", round(log_cases, digits = 3), 
                                        "Total Deaths:", deaths, "<br>",
                                        "Case Fatality Rate:", paste(round(proportion, digits = 3)*100, "%", sep = "")
                                        )
                                  )
          fig <- plot_geo(statedata, locationmode = "USA-states")
          fig <- fig %>% add_trace(
            z = ~proportion, text = ~hover, locations = ~state_alt, color = ~proportion, frame = ~date
          ) %>% layout(geo = g, title = paste("United States Coronavirus", input$`deaths-cases`, "on a", input$`log-normal`, "scale"))
        }
      } else{ # selected cases
        if(input$`log-normal` == "log"){
          statedata$hover <- with(statedata, 
                                  paste(state, "<br>", 
                                        "Total Cases:", cases, "<br>", 
                                        "Logarithm Scale:", round(log_cases, digits = 3), 
                                        "<br>", "Total Deaths:", deaths))
          fig <- plot_geo(statedata, locationmode = "USA-states")
          fig <- fig %>% add_trace(
            z = ~log_cases, text = ~hover, locations = ~state_alt, color = ~log_cases, frame = ~date
          ) %>% layout(geo = g, title = paste("United States Coronavirus", input$`deaths-cases`, "on a", input$`log-normal`, "scale"))
        } else { # Display cases without a logarithmic scale
          statedata$hover <- with(statedata, 
                                  paste(state, "<br>", 
                                        "Total Cases:", cases, "<br>", 
                                        #"Logarithm Scale:", round(log_cases, digits = 3), 
                                        "<br>", "Total Deaths:", deaths))
          fig <- plot_geo(statedata, locationmode = "USA-states")
          fig <- fig %>% add_trace(
            z = ~cases, text = ~hover, locations = ~state_alt, color = ~cases, frame = ~date
          ) %>% layout(geo = g, title = paste("United States Coronavirus", input$`deaths-cases`, "on a", input$`log-normal`, "scale"))
        }
      }
    }
    
    return(fig)
  })                                      #End of plotly Graphs for Total United STates
  observeEvent(input$`state-model`,{
  output$county_slider <- renderUI({
    data <- county_data() %>% filter(state == input$state) %>% mutate(date = as.Date(date))
    smallest_date <- min(data$date)
    #data <- data %>% mutate(date = date)
    object <- sliderInput("date",
                          label = "Select Days from Zero-Day",
                          min = min(data$date, na.rm = TRUE),
                          max = max(data$date, na.rm = TRUE),
                          value = max(data$date, na.rm = TRUE),
                          animate = FALSE)
    })
  })
  
  
  output$county <- renderPlotly({
    #corona <- county_data()
    #`%nin%` = Negate(`%in%`)
    #print(input$date)
    #corona <- corona %>% #filter(state == input$state) %>% 
    #  mutate(county = tolower(county), 
    #         state = tolower(state),
    #         ) %>% 
    #  filter(state %nin% setdiff(unique(tolower(corona$state)), unique(map_data("county")$region)) )
    corona <- corona() %>% filter(date == input$date) %>% filter(county != "unknown" & state == tolower(input$state))
    #corona.cases <- corona %>% mutate(county = fct_recode(county, 
    #                                                    `de kalb` = "dekalb",
    #                                                    `dona ana` = "doña ana",
    #                                                    `du page` = "dupage",
    #                                                    `la porte` = "laporte",
    #                                                    `la salle` = "lasalle",
    #                                                    `new york` = "new york city",
    #                                                    `obrien` = "o'brien",
    #                                                    `prince georges` = "prince george's",
    #                                                    `queen annes` = "queen anne's",
    #                                                    `st bernard` = "st. bernard",
    #                                                    `st charles` = "st. charles",
    #                                                    `st clair` = "st. clair",
    #                                                    `st francis` ="st. francis",
    #                                                    `st francois` = "st. francois",
    #                                                    `st helena` = "st. helena",
    #                                                    `st james` = "st. james",
    #                                                    `st john the baptist` = "st. john the baptist",
    #                                                    `st johns` = "st. johns",
    #                                                    `st joseph` = "st. joseph",
    #                                                    `st landry` = "st. landry",
    #                                                    `st lawrence` = "st. lawrence",
    #                                                    `st louis` =  "st. louis",
    #                                                    `st louis city` = "st. louis city",
    #                                                    `st lucie` = "st. lucie",
    #                                                    `st martin` = "st. martin",
    #                                                    `st mary` = "st. mary",
    #                                                    `st marys` = "st. mary's",
    #                                                    `st tammany` = "st. tammany",
    #                                                    `ste genevieve` = "ste. genevieve"),
    #                                  log_cases = log(cases, base = 10),
    #                                  log_deaths = log(deaths, base = 10),
    #                                  log_difference = log_deaths - log_cases,
    #                                  proportion = deaths/cases) %>% 
    #  filter(county != "unknown" & state == tolower(input$state)) #%>% filter(date == Sys.Date()-1)
    #changing state mapping
    # print(corona)
    mapping <- map_data("county") %>% filter(region == tolower(input$state))
    
    mapitup <- left_join(mapping,corona, by = c(subregion = "county") )
    
    if(input$`deaths-cases` == "deaths"){
      if(input$`log-normal` == "log"){
        thingy <- ggplot(mapitup, aes(x = long,
                                      y = lat, 
                                      group = group,
                                      fill = log_deaths, 
                                      text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths ," <br>", "Case Fatality Rate: ", round(proportion, digits =  3)*100,"%"
                                                    )
                                      )
                          ) +
          geom_polygon(color = "black", 
                       size = 0.5) + 
          theme_minimal() + 
          scale_fill_viridis_c() + 
          labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
               fill = paste("Number of", input$`deaths-cases`)) + 
          coord_map(projection = "albers", 
                    lat0 = 25, 
                    lat1 = 31)
      }else{
        thingy <- ggplot(mapitup, aes(x = long,
                                      y = lat, 
                                      group = group,
                                      fill = deaths, 
                                      text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths ," <br>", "Case Fatality Rate: ", round(proportion, digits =  3)*100,"%"))) +
          geom_polygon(color = "black", 
                       size = 0.5) + 
          theme_minimal() + 
          scale_fill_viridis_c() + 
          labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
               fill = paste("Number of", input$`deaths-cases`)) + 
          coord_map(projection = "albers", 
                    lat0 = 25, 
                    lat1 = 31)
      }
    }else{
      if(input$`deaths-cases` == "case fatality rate"){
        if(input$`log-normal` == "log"){
          thingy <- ggplot(mapitup, aes(x = long,
                                        y = lat, 
                                        group = group,
                                        fill = log_difference, 
                                        text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths ," <br>", "Case Fatality Rate: ", round(proportion, digits =  3)*100,"%"))) +
            geom_polygon(color = "black", 
                         size = 0.5) + 
            theme_minimal() + 
            scale_fill_viridis_c() + 
            labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
                 fill = paste("Number of", input$`deaths-cases`)) + 
            coord_map(projection = "albers", 
                      lat0 = 25, 
                      lat1 = 31)
        } else {
          thingy <- ggplot(mapitup, aes(x = long,
                                        y = lat, 
                                        group = group,
                                        fill = proportion, 
                                        text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths ," <br>", "Case Fatality Rate: ", round(proportion, digits =  3)*100,"%"))) +
            geom_polygon(color = "black", 
                         size = 0.5) + 
            theme_minimal() + 
            scale_fill_viridis_c() + 
            labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
                 fill = paste("Number of", input$`deaths-cases`)) + 
            coord_map(projection = "albers", 
                      lat0 = 25, 
                      lat1 = 31)
        }
      } else {
        if(input$`log-normal` == "log"){
          thingy <- ggplot(mapitup, aes(x = long,
                                        y = lat, 
                                        group = group,
                                        fill = log_cases, 
                                        text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths ," <br>", "Case Fatality Rate: ", round(proportion, digits =  3)*100,"%"))) +
            geom_polygon(color = "black", 
                         size = 0.5) + 
            theme_minimal() + 
            scale_fill_viridis_c() + 
            labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
                 fill = paste("Number of", input$`deaths-cases`)) + 
            coord_map(projection = "albers", 
                      lat0 = 25, 
                      lat1 = 31)
        } else {
          thingy <- ggplot(mapitup, aes(x = long,
                                        y = lat, 
                                        group = group,
                                        fill = cases, 
                                        text = paste0(subregion, "<br>", "Cases: ", cases, "<br>", "Deaths: ", deaths ," <br>", "Case Fatality Rate: ", round(proportion, digits =  3)*100,"%"))) +
            geom_polygon(color = "black", 
                         size = 0.5) + 
            theme_minimal() + 
            scale_fill_viridis_c() + 
            labs(title = paste("Coronavirus", input$`deaths-cases`, "in" ,input$state, "by County with", input$`log-normal`, "scale"),
                 fill = paste("Number of", input$`deaths-cases`)) + 
            coord_map(projection = "albers", 
                      lat0 = 25, 
                      lat1 = 31)
        }
      }
    }
    return(ggplotly(thingy, tooltip = "text"))
  })
  
  
  
  
  #This is going to be for the modeling page. 
  
  output$`state-county-dropdown` <- renderUI({
    
    if (input$`county-state` == "county"){
      #print(input$`state-model`)
      df <- corona() %>% filter(state == tolower(input$`state-model`))
      #print(df)
      counties <- as.vector(df["county"])
      #print(counties)
      object <- selectInput("county-model",
                            label = "Select County",
                            choices = counties,
                            selected = counties[1]
                            )
    } else {object <- NULL}
    return(object)
  })
  
  
  output$state_model <- renderPlot({
    SIRmodel <- function(time, group, parameters) {
      S <- group[1]; I <- group[2];  R <- group[3]
      N <- S + I + R
      beta <- parameters[1]; gamma <- parameters[2]
      dS <- -beta*S*I/N
      dI <- +beta*S*I/N - gamma*I
      dR <- gamma*I
      list(c(dS, dI, dR))
    }
    
    
    if (input$`county-state` == "state"){
      corona.sama.all <- state_data() %>% filter(state == input$`state-model`) %>% 
        group_by(date) %>%
        summarize(sumcases=sum(cases)) %>%
        select(date,sumcases) %>% 
        mutate(
          time=c(0,cumsum(as.numeric(diff(date)))),
          logsumcases = log(sumcases)
        ) %>%
        select(date, time, sumcases, logsumcases)
      colnames(corona.sama.all) <- c("date", "time", "cases", "logcases")
      cutoff <- "2020/04/01"
      corona.sama <- corona.sama.all %>% filter(date<=cutoff)
      
      plotdata <- pivot_longer(corona.sama, col=3:4, names_to="Type", values_to="values")
      
      
      
      # One Plot
      plot <- ggplot(data=plotdata, aes(x=date, y=values)) +
        geom_point(size=1.1) +
        geom_line() +
        facet_wrap(vars(Type), scales = "free_y")
      
      #linear Model
      fit.lm <- lm(logcases ~ time, data=corona.sama)
      library(broom)
      tidy(fit.lm)
      
      #Other Fitted Model:
      
      library(deSolve)
      
      
      # Model with some bounds
      
      gamma <- 1/input$beta
      betas <- c(
        confint(fit.lm)[2,1],
        as.numeric(tidy(fit.lm)[2,2]),
        confint(fit.lm)[2,2]
      ) + gamma
      #betas
      
      alpha <- exp(as.numeric(tidy(fit.lm)[1,2]))
      #alpha
      
      times <- seq(0,40,1)
      
      solutions <- sapply(betas, 
                          function(x) {
                            df <-
                              lsoda(
                                y = c(S=input$population, I=alpha, R=0), 
                                times = times, 
                                func = SIRmodel, 
                                parms = c(beta=x, gamma=gamma)
                              )
                            return(data.frame(df)[,3])
                          }
      )
      
      results <- data.frame(time=times, solutions)
      colnames(results) <- c("time", "LB", "Est", "UB")
      results <- results %>% mutate(date = seq(as.Date(corona.sama$date[1]), by = "day", length.out = length(time)))
      
      head(results)
      
      plot <- ggplot(data=results, aes(x=date, y=Est)) + 
        geom_line(size=1.0, col="blue") +
        geom_line(aes(y=LB), col="lightblue", size=1, linetype=1, alpha=0.5) +
        geom_line(aes(y=UB), col="lightblue", size=1, linetype=1, alpha=0.5) +
        geom_ribbon(aes(ymin=LB, ymax=UB), fill="lightblue", alpha=0.2) +
        geom_point(data=corona.sama, aes(x=date, y=cases), col="red", inherit.aes = FALSE) +
        labs(
          title= paste0("Number of Infections in ", input$`state-model`, " (Population:", input$population ,")"), 
          subtitle=paste0("Recovery Period is assumed as ", input$beta, " days"), x="Day", y="Number of Infections") +
        scale_x_date(date_breaks="6 days", date_label="%b %d") +
        theme_minimal()
      
      
      return(plot)
    } else {
      
      # This is county
      #print(input$`county-model`)
      #print(county_data())
      corona.sama.all <- corona() %>% filter(tolower(county) == input$`county-model`) %>% 
        group_by(date) %>%
        summarize(sumcases=sum(cases)) %>%
        select(date,sumcases) %>% 
        mutate(
          time=c(0,cumsum(as.numeric(diff(date)))),
          logsumcases = log(sumcases)
        ) %>%
        select(date, time, sumcases, logsumcases)
      print(corona.sama.all)
      colnames(corona.sama.all) <- c("date", "time", "cases", "logcases")
      cutoff <- "2020/04/01"
      corona.sama <- corona.sama.all %>% filter(date<=cutoff)
      
      plotdata <- pivot_longer(corona.sama, col=3:4, names_to="Type", values_to="values")
      
      
      
      #linear Model
      fit.lm <- lm(logcases ~ time, data=corona.sama)
      library(broom)
      tidy(fit.lm)
      
      #Other Fitted Model:
      
      library(deSolve)
      
      
      # Model with some bounds
      
      gamma <- 1/input$beta
      betas <- c(
        confint(fit.lm)[2,1],
        as.numeric(tidy(fit.lm)[2,2]),
        confint(fit.lm)[2,2]
      ) + gamma
      #betas
      
      alpha <- exp(as.numeric(tidy(fit.lm)[1,2]))
      #alpha
      
      times <- seq(0,40,1)
      
      solutions <- sapply(betas, 
                          function(x) {
                            df <-
                              lsoda(
                                y = c(S=input$population, I=alpha, R=0), 
                                times = times, 
                                func = SIRmodel, 
                                parms = c(beta=x, gamma=gamma)
                              )
                            return(data.frame(df)[,3])
                          }
      )
      
      results <- data.frame(time=times, solutions)
      colnames(results) <- c("time", "LB", "Est", "UB")
      results <- results %>% mutate(date = seq(as.Date(corona.sama$date[1]), by = "day", length.out = length(time)))
      
      head(results)
      
      plot <- ggplot(data=results, aes(x=date, y=Est)) + 
        geom_line(size=1.0, col="blue") +
        geom_line(aes(y=LB), col="lightblue", size=1, linetype=1, alpha=0.5) +
        geom_line(aes(y=UB), col="lightblue", size=1, linetype=1, alpha=0.5) +
        geom_ribbon(aes(ymin=LB, ymax=UB), fill="lightblue", alpha=0.2) +
        geom_point(data=corona.sama, aes(x=date, y=cases), col="red", inherit.aes = FALSE) +
        labs(
          title= paste0("Number of Infections in ", input$`county-model`, " (Population:", input$population ,")"), 
          subtitle=paste0("Recovery Period is assumed as ", input$beta, " days"), x="Day", y="Number of Infections") +
        scale_x_date(date_breaks="6 days", date_label="%b %d") +
        theme_minimal()
      return(plot)
    }
    
    
  })
  

})
