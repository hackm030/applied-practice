#######################################################################################
# Author: Anne Hackman
#######################################################################################
# Date: February 2025                                                                 
#######################################################################################
# Purpose: a document that defines a pipeline to update the weather data for the R Shiny App created for Saint Paul Public Schools (SPPS) to use for their Earth Science curriculum. Creation of this app served as Anne Hackman's Applied Practice project and was part of a collaboration with SPPS and the UMN Biostatistics Community Outreach and Engagement Committee (BCOE).
#######################################################################################

# Loading required packages
library(tidyverse)

# Read in current dataset used for the app
storms <- read_csv("midwest_storms_allyears.csv")

# Download annual weather data from the National Centers for Environmental Information (NCEI) using this link: https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/. Data will be in .csv.gv format and will need to be unzipped once downloaded.

# The .csv files will follow this naming format: "StormEvents_details-ftp_v1.0_dyear_cdate.csv". For example, the 2023 weather data file was named "StormEvents_details-ftp_v1.0_d2023_c20241216.csv". d2023 tells you the year and c20241216 tells you the date that NCEI released the data to the public.

# Read in new data
storms_new <- read_csv("StormEvents_details-ftp_v1.0_dyear_cdate.csv")

# Define data-cleaning function
cleanWeatherData <- function(data) {
  data %>%
    filter(STATE %in% c("NORTH DAKOTA", "SOUTH DAKOTA", "NEBRASKA",
                        "IOWA", "MINNESOTA", "WISCONSIN")) %>%
    separate(col = DAMAGE_PROPERTY, into = c("PROP_DAM_NUM", "PROP_DAM_UNIT"), sep = -1, remove = FALSE) %>%
    mutate(PROP_DAM_NUM = as.numeric(PROP_DAM_NUM),
           PROP_DAM_NUM = ifelse(PROP_DAM_UNIT == "M", PROP_DAM_NUM*1000, PROP_DAM_NUM),
           PROP_DAM_NUM = PROP_DAM_NUM*1000) %>%
    separate(col = DAMAGE_CROPS, into = c("CROP_DAM_NUM", "CROP_DAM_UNIT"), sep = -1, remove = FALSE) %>%
    mutate(CROP_DAM_NUM = as.numeric(CROP_DAM_NUM),
           CROP_DAM_NUM = ifelse(CROP_DAM_UNIT == "M", CROP_DAM_NUM*1000, CROP_DAM_NUM),
           CROP_DAM_NUM = CROP_DAM_NUM*1000) %>%
    select(-c(PROP_DAM_UNIT, CROP_DAM_UNIT)) %>%
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
                                                      "December")))
}

# Clean the new data (storms_new)
cleaned_storms_new <- cleanWeatherData(storms_new)

# Bind the cleaned new data to the current dataset (storms)
updated_storms <- rbind(storms, cleaned_storms_new)

# Save updated dataset to a chosen loation under the file name
# "midwest_storms_allyears.csv"
write.csv(updated_storms, "~/location/midwest_storms_allyears.csv")