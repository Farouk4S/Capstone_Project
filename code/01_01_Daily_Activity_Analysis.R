
# This is my Google Data Analytics Project code explained step-by-step.

# File:         01_01_Daily_Activity_Analysis
# Project:      Google Data Analysis Capstone Project
# Title:        How users use their smart device
# Subtitle:     Daily Activity Snapshot for the 31 Days Period

# READING THE DATA #############################################################

# I started by Installing the tidyverse package
install.packages("tidyverse")
library("tidyverse")
install.packages("lubridate")
install.packages("skimr")

# So, I loaded the database from file directory. 
library(readr)
dailyActivity_merged <- read_csv("data/dailyActivity_merged.csv")
#View(dailyActivity_merged)



# Then i used more packages and functions to confirm the dataset properties
library("skimr")
head(dailyActivity_merged)
# skim_without_charts(dailyActivity_merged)
summary(dailyActivity_merged)
 n_distinct(dailyActivity_merged$Id)
# n_distinct(dailyActivity_merged$ActivityDate)
# sum(duplicated(dailyActivity_merged))

#  PREPARING THE DATA ########################################################

## I renamed ActivityDate into Date 
 dailyActivity_merged <- dailyActivity_merged %>%
   rename(Date = ActivityDate)
 
# I needed to change the date formats as my data frame is 
# named 'dailyActivity_merged' and existing character is mdy format. 
# I ran codes that converts the dates to standard R format of YYYY-MM-DD.

library("lubridate")
dailyActivity_merged$Date <- mdy(dailyActivity_merged$Date)
dailyActivity_merged$Date <- format(dailyActivity_merged$Date,
                                            "%Y-%m-%d")
head(dailyActivity_merged)

# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================
library("dplyr")

summary_data <- dailyActivity_merged %>%
  group_by(Date) %>%
  summarise(
    Sum_TotalSteps = sum(TotalSteps, na.rm = TRUE),
    Sum_TotalDistance = sum(TotalDistance, na.rm = TRUE),
    Sum_TrackerDistance = sum(TrackerDistance, na.rm = TRUE),
    Sum_LoggedActivitiesDistance = sum(LoggedActivitiesDistance, na.rm = TRUE),
    Sum_Calories = sum(Calories, na.rm = TRUE),
    Sum_VeryActiveDistance = sum(VeryActiveDistance, na.rm = TRUE),
    Sum_ModeratelyActiveDistance = sum(ModeratelyActiveDistance, na.rm = TRUE),
    Sum_LightActiveDistance = sum(LightActiveDistance, na.rm = TRUE),
    Sum_SedentaryActiveDistance = sum(SedentaryActiveDistance, na.rm = TRUE),
    Sum_VeryActiveMinutes = sum(VeryActiveMinutes, na.rm = TRUE),
    Sum_FairlyActiveMinutes = sum(FairlyActiveMinutes, na.rm = TRUE),
    Sum_LightlyActiveMinutes = sum(LightlyActiveMinutes, na.rm = TRUE),
    Sum_SedentaryMinutes = sum(SedentaryMinutes, na.rm = TRUE),
  )

## View the resulting dataset
head(summary_data)


## Reshape the data to long format using tidyr =================================

library("tidyr")

summary_data_long <- summary_data %>%
  pivot_longer(cols = -Date,
               names_to = "Variable", 
               values_to = "Values")
head(summary_data_long)

summary_Activity_long <- dailyActivity_merged %>%
  pivot_longer(cols = -Date, names_to = "Variable", 
               values_to = "Values")

head(summary_Activity_long)

# Convert ActivityDate to Date class
summary_data_long <- summary_data_long %>%
  mutate(Order = as.Date(Date))


# PLOTTING THE DATA ########################################################

## Plotting the chart
install.packages("ggplot2")
library(ggplot2)

ggplot(data = summary_data_long %>% filter(Variable %in% c("Sum_TotalSteps", "Sum_Calories"))) +
  geom_line(mapping=aes(x = Date, y = Values, 
                        color = Variable, group = Variable)) +
  labs(title = "Daily Activity Snapshot",
       x = "Date",
       y = "Value") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

