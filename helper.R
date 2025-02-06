#######################################################################################
# Author: Anne Hackman
#######################################################################################
# Date: February 2025                                                                 
#######################################################################################
# Purpose: a helper document that includes functions and data cleaning steps for the R Shiny App created for Saint Paul Public Schools (SPPS) to use for their Earth Science curriculum. Creation of this app served as Anne Hackman's Applied Practice project and was part of a collaboration with SPPS and the UMN Biostatistics Community Outreach and Engagement Committee (BCOE).
#######################################################################################

# Loading required packages
library(tidyverse)
library(lubridate)
library(leaflet)
library(plotly)
library(pals)

#######################################################################################
# Set up and initialize interactive components of the app
#######################################################################################

# Read in and clean dataset
storms <- read_csv("midwest_storms_allyears.csv")

storms <- storms %>%
  select(-c(...1)) %>%
  # Correcting a data error
  filter(EVENT_TYPE != "Astronomical Low Tide") %>%
  separate(col = BEGIN_YEARMONTH, into = c("BEGIN_YEAR", "BEGIN_MONTH"),
           sep = -2, remove = FALSE) %>%
  mutate(MONTH_NAME = factor(MONTH_NAME, levels = c("January",
                                                    "February",
                                                    "March",
                                                    "April",
                                                    "May",
                                                    "June",
                                                    "July",
                                                    "August",
                                                    "September",
                                                    "October",
                                                    "November",
                                                    "December")),
         BEGIN_YMD = paste0(BEGIN_YEAR, "/", BEGIN_MONTH, "/", BEGIN_DAY),
         BEGIN_YMD = ymd(BEGIN_YMD))

# Create modified dataset for Frequency tab
storms_episode <- storms %>%
  group_by(EPISODE_ID, EVENT_TYPE, STATE, YEAR, MONTH_NAME) %>%
  summarize(BEGIN_YMD = min(BEGIN_YMD),
            EPISODE_PROP_DAM = sum(PROP_DAM_NUM, na.rm = TRUE),
            EPISODE_CROP_DAM = sum(CROP_DAM_NUM, na.rm = TRUE))

# Create modified dataset for Human Impact tab
storms_year <- storms %>%
  group_by(EVENT_TYPE, STATE, YEAR) %>%
  summarize(TOTAL_PROP_DAM = sum(PROP_DAM_NUM, na.rm = TRUE),
            TOTAL_CROP_DAM = sum(CROP_DAM_NUM, na.rm = TRUE))

# Create dictionary of all weather type definitions
all_weather_types = sort(unique(storms$EVENT_TYPE))
weather_dict <- data.frame(rbind(c("Avalanche", "A mass of snow, rock, and/or ice falling down a mountain or incline. In practice, it usually refers to the snow avalanche.", "https://forecast.weather.gov/glossary.php?word=Avalanche"),
                      c("Blizzard", "A blizzard means that the following conditions are expected to prevail for a period of 3 hours or longer:
Sustained wind or frequent gusts to 35 miles an hour or greater; and
Considerable falling and/or blowing snow (i.e., reducing visibility frequently to less than ¼ mile).", "https://forecast.weather.gov/glossary.php?word=Blizzard"),
                      c("Coastal Flood", "The inundation of land areas caused by sea waters over and above normal tidal action", "https://forecast.weather.gov/glossary.php?word=Coastal"),
                      c("Cold/Wind Chill", "Occurs when increased wind speeds accelerate heat loss from exposed skin. No specific rules exist for determining when wind chill becomes dangerous. As a general rule, the threshold for potentially dangerous wind chill conditions is about -20°F.", "https://forecast.weather.gov/glossary.php?word=Chill"),
                      c("Dense Fog", "When fog reduces visibility to 1/8 mile or less over a widespread area.", "https://forecast.weather.gov/glossary.php?word=Dense+Fog"),
                      c("Dense Smoke", "Widespread or localized smoke reducing visibilities to regionally or locally defined limitations not to exceed 1 nautical mile", "https://forecast.weather.gov/glossary.php?word=Dense+Smoke"),
                      c("Drought", "A deficiency of moisture that results in adverse impacts on people, animals, or vegetation over a sizeable area.", "https://forecast.weather.gov/glossary.php?word=Drought"),
                      c("Dust Devil", "A small, rapidly rotating wind that is made visible by the dust, dirt or debris it picks up. Also called a whirlwind, it develops best on clear, dry, hot afternoons.", "https://forecast.weather.gov/glossary.php?word=Dust+Devil"),
                      c("Dust Storm", "A severe weather condition characterized by strong winds and dust-filled air over an extensive area.", "https://forecast.weather.gov/glossary.php?word=Dust+Storm"),
                      c("Excessive Heat", "Occurs from a combination of high temperatures (significantly above normal) and high humidities. An Excessive Heat Warning is triggered by the following criteria: heat index of at least 105°F (41°C) for more than 3 hours per day for 2 consecutive days, or heat index more than 115°F (46°C) for any period of time.", "https://forecast.weather.gov/glossary.php?word=Excessive+Heat"),
                      c("Extreme Cold/Wind Chill", "An Extreme Cold Warning is issued when dangerously cold air temperatures or wind chill values are expected or occurring. Criteria for issuing this warning are set locally.", "https://www.weather.gov/safety/cold-ww"),
                      c("Flash Flood", "A rapid and extreme flow of high water into a normally dry area, or a rapid water level rise in a stream or creek above a predetermined flood level, beginning within six hours of the causative event (e.g., intense rainfall, dam failure, ice jam).", "https://forecast.weather.gov/glossary.php?word=Flash+Flood"),
                      c("Flood", "Any high flow, overflow, or inundation by water which causes or threatens damage.", "https://forecast.weather.gov/glossary.php?word=Flood"),
                      c("Freezing Fog", "A fog the droplets of which freeze upon contact with exposed objects and form a coating of rime and/or glaze.", "https://forecast.weather.gov/glossary.php?word=Freezing+Fog"),
                      c("Frost/Freeze", "describes the formation of thin ice crystals on the ground or other surfaces in the form of scales, needles, feathers, or fans. This condition is primarily significant during the growing season. If a frost period is sufficiently severe to end the growing season or delay its beginning, it is commonly referred to as a 'killing frost.'", "https://forecast.weather.gov/glossary.php?word=Frost"),
                      c("Funnel Cloud", "A condensation funnel extending from the base of a towering cumulus or Cb, associated with a rotating column of air that is not in contact with the ground (and hence different from a tornado). A condensation funnel is a tornado, not a funnel cloud, if either a) it is in contact with the ground or b) a debris cloud or dust whirl is visible beneath it.", "https://forecast.weather.gov/glossary.php?word=Funnel+Cloud"),
                      c("Hail", "Showery precipitation in the form of irregular pellets or balls of ice more than 5 mm in diameter, falling from a cumulonimbus cloud.", "https://forecast.weather.gov/glossary.php?word=Hail"),
                      c("Heat", "A Heat Advisory is issued within 12 hours of the onset of the following conditions: heat index of at least 105°F (41°C) but less than 115°F (46°C) for less than 3 hours per day, or nighttime lows above 80°F (27°C) for 2 consecutive days.", "https://forecast.weather.gov/glossary.php?word=Heat+Advisory"),
                      c("Heavy Rain", "Refers to instances during which the amount of rain experienced in a location substantially exceeds what is normal. What constitutes a period of heavy precipitation varies according to location and season.", "https://www.epa.gov/climate-indicators/climate-change-indicators-heavy-precipitation"),
                      c("Heavy Snow", "Generally means snowfall accumulating to 4 inches (10 cm) or more in depth in 12 hours or less; or snowfall accumulating to 6 inches (15 cm) or more in depth in 24 hours or less.", "https://forecast.weather.gov/glossary.php?word=Heavy+Snow"),
                      c("High Surf", "Large waves breaking on or near the shore resulting from swells spawned by a distant storm. A High Surf Advisory is issued when breaking wave action poses a threat to life and property within the surf zone.", "https://forecast.weather.gov/glossary.php?word=High+Surf"),
                      c("High Wind", "Sustained wind speeds of 40 mph or greater lasting for 1 hour or longer, or winds of 58 mph or greater for any duration.", "https://forecast.weather.gov/glossary.php?word=High+Wind"),
                      c("Ice Storm", "Used to describe occasions when damaging accumulations of ice are expected during freezing rain situations. Significant accumulations of ice pull down trees and utility lines resulting in loss of power and communication. These accumulations of ice make walking and driving extremely dangerous. Significant ice accumulations are usually accumulations of ¼ inch or greater.", "https://forecast.weather.gov/glossary.php?word=Ice+Storm"),
                      c("Lake-Effect Snow", "Snow showers that are created when cold, dry air passes over a large warmer lake, such as one of the Great Lakes, and picks up moisture and heat.", "https://forecast.weather.gov/glossary.php?word=Lake+Effect+Snow"),
                      c("Lakeshore Flood", "The inundation of land areas adjacent to one of the Great Lakes caused by lake water exceeding normal levels.", "https://forecast.weather.gov/glossary.php?word=Lakeshore+Flood"),
                      c("Lightning", "A visible electrical discharge produced by a thunderstorm. The discharge may occur within or between clouds, between the cloud and air, between a cloud and the ground or between the ground and a cloud.", "https://forecast.weather.gov/glossary.php?word=Lightning"), 
                      c("Rip Current", "Rip currents are powerful channels of water flowing quickly away from shore which occur most often at low spots or breaks in the sandbar and in the vicinity of structures such as groins, jetties and piers.", "https://tidesandcurrents.noaa.gov/ofs/nws_forecastinfo.html"),
                      c("Seiche", "A standing wave oscillation of water in large lakes usually created by strong winds and/or a large barometric pressure gradient.", "https://forecast.weather.gov/glossary.php?word=Seiche"),
                      c("Sleet", "Pellets of ice composed of frozen or mostly frozen raindrops or refrozen partially melted snowflakes. These pellets of ice usually bounce after hitting the ground or other hard surfaces.", "https://forecast.weather.gov/glossary.php?word=Sleet"),
                      c("Strong Wind", "Consists of damaging winds, often originating from thunderstorms, that are classified as exceeding 58 mph.", "https://hazards.fema.gov/nri/strong-wind"),
                      c("Thunderstorm Wind", "Damaging winds are often called 'straight-line' winds to differentiate the damage they cause from tornado damage. Strong thunderstorm winds can come from a number of different processes. Most thunderstorm winds that cause damage at the ground are a result of outflow generated by a thunderstorm downdraft. Damaging winds are classified as those exceeding 50-60 mph.", "https://www.nssl.noaa.gov/education/svrwx101/wind/"),
                      c("Tornado", "A violently rotating column of air with circulation reaching the ground. It nearly always starts as a funnel cloud and may be accompanied by a loud roaring noise. On a local scale, it is the most destructive of all atmospheric phenomena.", "https://forecast.weather.gov/glossary.php?word=Tornado"),
                      c("Waterspout", "In general, a tornado occurring over water.", "https://forecast.weather.gov/glossary.php?word=Waterspout"),
                      c("Wildfire", "Any free burning uncontainable wildland fire not prescribed for the area which consumes the natural fuels and spreads in response to its environment.", "https://forecast.weather.gov/glossary.php?word=Wildfire"),
                      c("Winter Storm", "An event in which the main types of precipitation are snow, sleet or freezing rain.", "https://www.nssl.noaa.gov/education/svrwx101/winter/"),
                      c("Winter Weather", "A winter weather event is a winter weather phenomenon (such as snow, sleet, ice, cold temperatures) that impacts public safety, transportation, and/or commerce.", "https://www.weather.gov/bgm/winterterms#:~:text=A%20winter%20weather%20event%20is,October%2015%20and%20April%2015."))
)
colnames(weather_dict) = c("type", "def", "source")

# Options for dropdown boxes
available_states_freq = c("All states", sort(unique(storms$STATE)) %>% str_to_title())
available_states_hum = sort(unique(storms$STATE)) %>% str_to_title()
available_years = sort(unique(storms$YEAR))
available_months = sort(unique(storms$MONTH_NAME))

# Initialize weather_types list for Map tab
weather_with_latlong <- storms %>%
  filter(YEAR == "2023",
         MONTH_NAME == "April",
         !is.na(BEGIN_LAT))
weather_with_latlong = sort(unique(weather_with_latlong$EVENT_TYPE))

# Initialize weather_types list for Frequency tab
weather_types <- storms %>%
  filter(STATE == "MINNESOTA")
weather_types = sort(unique(weather_types$EVENT_TYPE))

# Initialize weather_types list for Human Impact tab
damage_weather <- storms %>%
  filter(STATE == "MINNESOTA",
         YEAR == "2023",
         PROP_DAM_NUM > 0)
damage_weather <- sort(unique(damage_weather$EVENT_TYPE))

#######################################################################################
# Functions
#######################################################################################

# Function to update weather types based on selected year and month
# Map tab
generateMonthlyWeatherTypes <- function(data, year, month) {
  weather_with_latlong <- data %>%
    filter(YEAR == year,
           MONTH_NAME == month,
           !is.na(BEGIN_LAT))
  weather_with_latlong = sort(unique(weather_with_latlong$EVENT_TYPE))
}

# Function to update weather types based on selected state
# Frequency tab
generateWeatherTypes <- function(data, state) {
  if (state == "All states") {
    weather_types <- data
  }
  else {
    weather_types <- data %>%
      filter(STATE == toupper(state))
  }
  weather_types <- sort(unique(weather_types$EVENT_TYPE))
}

# Function to update weather types based on selected damage type, state, and year
# Human Impact tab
generateYearlyWeatherTypes <- function(data, damage_type, state, year) {
  if (damage_type == "Property Damage") {
    weather_types <- data %>%
      filter(STATE == toupper(state),
             YEAR == year,
             PROP_DAM_NUM > 0)
    weather_types <- sort(unique(weather_types$EVENT_TYPE))
  }
  else {
    weather_types <- data %>%
      filter(STATE == toupper(state),
             YEAR == year,
             CROP_DAM_NUM > 0)
    weather_types <- sort(unique(weather_types$EVENT_TYPE))
  }
}

# Function to display definition of each weather type
displayDefinition <- function(w_type) {
  display_text <- weather_dict %>%
    filter(type == w_type) %>%
    select(def)
  display_text[, 1]
}

# Function to display source of the definition of each weather type
displaySource <- function(w_type) {
  display_text <- weather_dict %>%
    filter(type == w_type) %>%
    select(source)
  display_text[, 1]
}

# Function to make homepage map
makeWeatherMap <- function(data, year, month, w_type) {
  map_dat <- data %>%
    filter(YEAR == year, MONTH_NAME == month,
           EVENT_TYPE == w_type, !is.na(BEGIN_LAT)) %>%
    mutate(text_label = paste0("ID: ",
                               EPISODE_ID,
                               "<br/> ",
                               BEGIN_DATE_TIME))
  
  episodes <- sort(unique(map_dat$EPISODE_ID))
  n_types <- length(episodes)
  my_palette <- colorFactor(pals::polychrome(n_types), domain = episodes)
  
  if (n_types == 0) {
    leaflet() %>%
      addTiles() %>%
      setView(lng = -94.63, lat = 45.00, zoom = 5)
  }
  else {
    leaflet(map_dat) %>%
      addTiles() %>%
      setView(lng = -94.63, lat = 45.00, zoom = 5) %>%
      addCircleMarkers(lng = ~BEGIN_LON,
                       lat = ~BEGIN_LAT,
                       radius = 0.3,
                       color = ~my_palette(EPISODE_ID),
                       label = ~map(text_label, HTML))
  }
}
  
# Function to generate plot for Frequency tab
makeYearlyFrequencyPlot <- function(data, state, w_type) {
  if (state == "All states") {
    data %>%
      filter(EVENT_TYPE == w_type) %>%
      ggplot(aes(x = factor(YEAR))) +
      geom_bar(aes(fill = str_to_title(STATE))) +
      labs(x = "",
           y = paste0("Number of ", tolower(w_type), " events"),
           title = paste0("Frequency of ", w_type, " in all states, 2000 - 2023"),
           fill = "State") +
      theme(axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 8),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 12)) +
      scale_x_discrete(limits = c("2000", "2001", "2002", "2003", "2004", "2005",
                                  "2006", "2007", "2008", "2009", "2010", "2011",
                                  "2012", "2013", "2014", "2015", "2016", "2017",
                                  "2018", "2019", "2020", "2021", "2022", "2023"))
  }
  else {
    data %>%
    filter(STATE == toupper(state),
           EVENT_TYPE == w_type) %>%
    ggplot(aes(x = factor(YEAR))) +
    geom_bar(fill = "lightblue3") +
    labs(x = "",
         y = paste0("Number of ", tolower(w_type), " events"),
         title = paste0("Frequency of ", w_type, " in ", state, ", 2000 - 2023")) +
    theme(axis.text.x = element_text(size = 8, angle = 90),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 12)) +
    scale_x_discrete(limits = c("2000", "2001", "2002", "2003", "2004", "2005",
                                "2006", "2007", "2008", "2009", "2010", "2011",
                                "2012", "2013", "2014", "2015", "2016", "2017",
                                "2018", "2019", "2020", "2021", "2022", "2023"))
  }
}

# Function to generate first plot for Human Impact tab
makeHumanImpactBarplot <- function(data, damage_type, state, w_type) {
  if (damage_type == "Property Damage") {
    data %>%
      filter(STATE == toupper(state),
             EVENT_TYPE == w_type) %>%
      mutate(text_label = paste0("$", as.integer(TOTAL_PROP_DAM))) %>%
      ggplot(aes(x = factor(YEAR), y = TOTAL_PROP_DAM)) +
      geom_col(aes(text = text_label), fill = "sienna4") +
      labs(x = "",
           y = "Property damage (USD)",
           title = paste0("Total property damage caused by ", w_type,
                          " in ", state, ", 2000 - 2023")) +
      theme(axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10)) +
      scale_x_discrete(limits = c("2000", "2001", "2002", "2003", "2004", "2005",
                                  "2006", "2007", "2008", "2009", "2010", "2011",
                                  "2012", "2013", "2014", "2015", "2016", "2017",
                                  "2018", "2019", "2020", "2021", "2022", "2023")) +
      scale_y_continuous(labels = scales::comma)
  }
  else if (damage_type == "Crop Damage") {
    data %>%
      filter(STATE == toupper(state),
             EVENT_TYPE == w_type) %>%
      mutate(text_label = paste0("$", as.integer(TOTAL_CROP_DAM))) %>%
      ggplot(aes(x = factor(YEAR), y = TOTAL_CROP_DAM)) +
      geom_col(aes(text = text_label), fill = "green4") +
      labs(x = "",
           y = "Crop damage (USD)",
           title = paste0("Total crop damage caused by ", w_type,
                          " in ", state, ", 2000 - 2023")) +
      theme(axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10)) +
      scale_x_discrete(limits = c("2000", "2001", "2002", "2003", "2004", "2005",
                                  "2006", "2007", "2008", "2009", "2010", "2011",
                                  "2012", "2013", "2014", "2015", "2016", "2017",
                                  "2018", "2019", "2020", "2021", "2022", "2023")) +
      scale_y_continuous(labels = scales::comma)
  }
}

# Function to generate second plot for Human Impact tab
makeHumanImpactPlot <- function(data, damage_type, state, year, w_type) {
  if (damage_type == "Property Damage") {
    data <- data %>%
      filter(STATE == toupper(state),
             YEAR == year,
             EVENT_TYPE == w_type,
             PROP_DAM_NUM > 0) %>%
      mutate(text_label = paste0("ID: ", EPISODE_ID,
                                 "\n",
                                 BEGIN_DATE_TIME,
                                 "\n",
                                 "$", as.integer(PROP_DAM_NUM)))
    data %>%
      ggplot(aes(x = MONTH_NAME, y = PROP_DAM_NUM)) +
      geom_point(aes(text = text_label), color = "sienna4") +
      labs(x = "",
           y = "Property damage (USD)",
           title = paste0("Property damage caused by ", w_type,
                          " in ", state, ", ", year)) +
      theme(axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10)) +
      scale_x_discrete(limits = c("January", "February", "March", "April",
                                  "May", "June", "July", "August",
                                  "September", "October", "November", "December")) +
      scale_y_continuous(labels = scales::comma,
                         limits = c(0, max(data$PROP_DAM_NUM)))
  }
  else if (damage_type == "Crop Damage") {
    data <- data %>%
      filter(STATE == toupper(state),
             YEAR == year,
             EVENT_TYPE == w_type,
             CROP_DAM_NUM > 0) %>%
      mutate(text_label = paste0("ID: ", EPISODE_ID,
                                 "\n",
                                 BEGIN_DATE_TIME,
                                 "\n",
                                 "$", as.integer(CROP_DAM_NUM)))
    data %>%
      ggplot(aes(x = MONTH_NAME, y = CROP_DAM_NUM)) +
      geom_point(aes(text = text_label), color = "green4") +
      labs(x = "",
           y = "Crop damage (USD)",
           title = paste0("Crop damage caused by ", w_type, " in ", state, ", ", year)) +
      theme(axis.text.x = element_text(size = 8, angle = 90),
            axis.text.y = element_text(size = 8),
            plot.title = element_text(size = 10)) +
      scale_x_discrete(limits = c("January", "February", "March", "April",
                                  "May", "June", "July", "August",
                                  "September", "October", "November", "December")) +
      scale_y_continuous(labels = scales::comma,
                         limits = c(0, max(data$CROP_DAM_NUM)))
  }
}
