#######################################################################################
# Author: Anne Hackman
#######################################################################################
# Date: February 2025                                                                 
#######################################################################################
# Purpose: contains the code to run the interactive R Shiny App created for Saint Paul Public Schools (SPPS) to use for their Earth Science curriculum. Creation of this app served as Anne Hackman's Applied Practice project and was part of a collaboration with SPPS and the UMN Biostatistics Community Outreach and Engagement (BCOE).
#######################################################################################

# Loading required packages
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)

# Loading helper document
source("helper.R")

# Define UI
ui <- fluidPage(
  # Creating tabs
  tabsetPanel(
    # Map tab
    tabPanel("Map",
             titlePanel("Severe Weather of the Upper Midwest"),
             sidebarPanel(
               h4("Investigate the geography of severe weather."),
               selectInput(inputId = "map_selected_year",
                           label = "Choose a year:",
                           choices = available_years,
                           selected = "2023"),
               selectInput(inputId = "map_selected_month",
                           label = "Choose a month:",
                           choices = available_months,
                           selected = "April"),
               selectInput(inputId = "map_weather_type",
                           label = "Choose a type of severe weather:",
                           choices = weather_with_latlong,
                           selected = "Hail")
             ),
             mainPanel(
               leafletOutput("state_map"),
               br(),
               h4(textOutput("map_weather")),
               textOutput("map_definition"),
               uiOutput("map_source")
             )
    ),
    # Frequency tab
    tabPanel("Frequency of Severe Weather",
             sidebarPanel(
               h4("Investigate the frequency of types of severe weather by year."),
               selectInput(inputId = "freq_selected_state",
                           label = "Choose a state:",
                           choices = available_states_freq,
                           selected = "Minnesota"),
               selectInput(inputId = "freq_weather_type",
                           label = "Choose a type of severe weather:",
                           choices = weather_types,
                           selected = "Hail")
               ),
             mainPanel(
                 plotlyOutput("freq_plot"),
                 br(),
                 h4(textOutput("freq_weather")),
                 textOutput("freq_definition"),
                 uiOutput("freq_source")
             )
             ),
    # Human Impact tab
    tabPanel("Human Impact of Severe Weather",
             sidebarPanel(
               h4("Investigate the human impact of severe weather by year."),
               selectInput(inputId = "hum_selected_state",
                           label = "Choose a state: ",
                           choices = available_states_hum,
                           selected = "Minnesota"),
               selectInput(inputId = "hum_damage_type",
                           label = "Choose a human impact metric:",
                           choices = c("Property Damage",
                                       "Crop Damage"),
                           selected = "Property Damage"),
               selectInput(inputId = "hum_weather_type",
                           label = "Choose a type of severe weather:",
                           choices = damage_weather,
                           selected = "Hail"),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               h4("Dig deeper into the weather events of one specific year."),
               selectInput(inputId = "hum_selected_year",
                           label = "Choose a year:",
                           choices = available_years,
                           selected = "2023"),
               br(),
               br(),
               br()
               ),
             mainPanel(
               plotlyOutput("impact_barplot"),
               br(),
               plotlyOutput("impact_plot"),
               br(),
               h4(textOutput("hum_weather")),
               textOutput("hum_definition"),
               uiOutput("hum_source")
             )
             )
    )
  )

# Define server logic
server <- function(input, output, session) {
  
  # Map on Map tab
  output$state_map <-renderLeaflet({
    makeWeatherMap(storms, input$map_selected_year, input$map_selected_month, input$map_weather_type)
  })
  
  # Filter and update weather types based on selected year and month (Map tab)
  observe({
    weather_with_latlong <- generateMonthlyWeatherTypes(storms,
                                                        input$map_selected_year,
                                                        input$map_selected_month)
    current_selected <- input$map_weather_type
    updateSelectInput(session,
                      inputId = "map_weather_type",
                      label = "Choose a type of severe weather:",
                      choices = weather_with_latlong,
                      selected = current_selected
    )
  })
  
  # Weather definition on Map tab
  output$map_weather <- renderText(input$map_weather_type)
  output$map_definition <- renderText({
    displayDefinition(input$map_weather_type)
  })
  output$map_source <- renderUI({
    a("Source", href = displaySource(input$map_weather_type), target="_blank")
  })
  
  # Plot on Frequency tab
  output$freq_plot <- renderPlotly({
    ggplotly(makeYearlyFrequencyPlot(storms_episode, input$freq_selected_state, input$freq_weather_type),
             tooltip = "y")
  })
  
  # Filter and update weather types based on selected state (Frequency tab)
  observe({
    weather_types <- generateWeatherTypes(storms_episode,
                                          input$freq_selected_state)
    current_selected <- input$freq_weather_type
    updateSelectInput(session,
                      inputId = "freq_weather_type",
                      label = "Choose a type of severe weather:",
                      choices = weather_types,
                      selected = current_selected
    )
  })
  
  # Weather definition on Frequency tab
  output$freq_weather <- renderText(input$freq_weather_type)
  output$freq_definition <- renderText({
    displayDefinition(input$freq_weather_type)
  })
  output$freq_source <- renderUI({
    a("Source", href = displaySource(input$freq_weather_type), target="_blank")
  })
  
  # First plot on Human Impact tab
  output$impact_barplot <- renderPlotly({
    ggplotly(makeHumanImpactBarplot(storms_year, input$hum_damage_type,
                                    input$hum_selected_state, input$hum_weather_type),
             tooltip = "text")
  })
  # Second plot on Human Impact tab
  output$impact_plot <- renderPlotly({
    ggplotly(makeHumanImpactPlot(storms, input$hum_damage_type, input$hum_selected_state, input$hum_selected_year, input$hum_weather_type),
             tooltip = "text")
  })
  
  # Filter and update weather types based on selected damage type, state, and year
  # (Human Impact tab)
  observe({
    damage_weather <- generateYearlyWeatherTypes(storms,
                                                input$hum_damage_type,
                                                input$hum_selected_state,
                                                input$hum_selected_year)
    current_selected <- input$hum_weather_type
    updateSelectInput(session,
                      inputId = "hum_weather_type",
                      label = "Choose a type of severe weather:",
                      choices = damage_weather,
                      selected = current_selected
    )
  })
  
  # Weather definition on Human Impact tab
  output$hum_weather <- renderText(input$hum_weather_type)
  output$hum_definition <- renderText({
    displayDefinition(input$hum_weather_type)
  })
  output$hum_source <- renderUI({
    a("Source", href = displaySource(input$hum_weather_type), target="_blank")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
