
# This is my Google Data Analytics Project code explained step-by-step.

# File:         02_02_Activity_Levels_Analysis
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     How much did users actually use the device in the 31 Days Period?


# READING THE DATA #############################################################

# I started by Installed the tidyverse packages

install.packages("tidyverse")
library("tidyverse")

# So, I loaded the database from file directory. 
# Then i used some functions to verify the dataset properties

library(readr)
dailyIntensities_merged <- read_csv("data/dailyIntensities_merged.csv")
#View(dailyIntensities_merged)
head(dailyIntensities_merged)

#  PREPARING THE DATA ########################################################

# To be able to know 

## Convert the Date format to "dd-mm-yyyy"

## Convert the Sleep "ActivityDay" column to Date type from lubridate package
dailyIntensities_merged$ActivityDay <- mdy(dailyIntensities_merged$ActivityDay)

## Convert the Date format to "dd-mm-yyyy"
dailyIntensities_merged$ActivityDay <- format(
                            dailyIntensities_merged$ActivityDay, "%d-%m-%Y")  

# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================
## Group and summarize by Day of Week

Intensities_summary <- (dailyIntensities_merged) %>%
  group_by(ActivityDay) %>%
  summarise(TotalSedentaryMinutes = sum(SedentaryMinutes, na.rm = TRUE), 
            TotalLightlyActiveMinutes = sum(LightlyActiveMinutes, na.rm = TRUE),
            TotalFairlyActiveMinutes = sum(FairlyActiveMinutes, na.rm = TRUE),
            TotalVeryActiveMinutes = sum(VeryActiveMinutes , na.rm = TRUE),
            TotalSedentaryActiveDistance = sum(SedentaryActiveDistance, na.rm = TRUE),
            TotalLightActiveDistance = sum(LightActiveDistance, na.rm = TRUE),
            TotalModeratelyActiveDistance = sum(ModeratelyActiveDistance, na.rm = TRUE),
            TotalVeryActiveDistance = sum(VeryActiveDistance, na.rm = TRUE))
View(Intensities_summary)

Intensities_sum <- select(Intensities_summary,
                   c('TotalSedentaryMinutes', 'TotalLightlyActiveMinutes', 
                     'TotalFairlyActiveMinutes', 'TotalVeryActiveMinutes',
                     'TotalSedentaryActiveDistance', 'TotalLightActiveDistance',
                     'TotalModeratelyActiveDistance', 'TotalVeryActiveDistance'))



# PLOTTING THE DATA ###########################################################



## Preparing the Plot =====================================================




## Plotting The Chart  ========================================================
