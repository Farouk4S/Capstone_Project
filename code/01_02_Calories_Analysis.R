
# This is my Google Data Analytics Project code explained step-by-step.

# File:         01_02_CaloriesAnalysis
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     Users Calories Data Analysis for 31 Days Period

# READING THE DATA #############################################################

# I started by Installed the tidyverse packages

install.packages("tidyverse")
library("tidyverse")

# So, I loaded the database from file directory. 
# Then i used some functions to verify the dataset properties

library(readr)
hourlyCalories_merged <- read_csv("data/hourlyCalories_merged.csv")
head(hourlyCalories_merged)

#  PREPARING THE DATA ########################################################

# To be able to know summary data by each hour, I needed to Split 
# the TIMEDATE column into separate Date and Time columns at the first space

hourlyCalories <- hourlyCalories_merged %>%
  separate(ActivityHour, into = c("Date", "Time"), sep = " ", extra = "merge")
head(hourlyCalories) 


# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================
## First, convert the Time column to a factor with a custom order

hourlyCalories$Time <- factor(hourlyCalories$Time,
         levels = c("12:00:00 AM", "1:00:00 AM", "2:00:00 AM", "3:00:00 AM", 
                     "4:00:00 AM", "5:00:00 AM", "6:00:00 AM", "7:00:00 AM",
                     "8:00:00 AM", "9:00:00 AM", "10:00:00 AM", "11:00:00 AM",
                     "12:00:00 PM", "1:00:00 PM", "2:00:00 PM", "3:00:00 PM", 
                     "4:00:00 PM", "5:00:00 PM", "6:00:00 PM", "7:00:00 PM",
                     "8:00:00 PM", "9:00:00 PM", "10:00:00 PM", "11:00:00 PM"))

hourlyCalories_summary <- (hourlyCalories) %>%
  group_by(Time) %>%
  summarise(Calories_sum = sum(Calories, na.rm = TRUE))

head(hourlyCalories_summary)



# PLOTTING THE DATA ########################################################

## Preparing the Plot =====================================================

# Add a new variable to indicate night or day
hourlyCalories_summary <- hourlyCalories_summary %>%
  mutate(TimeOfDay = ifelse(Time %in% c("9:00:00 PM", "10:00:00 PM", "11:00:00 PM", 
                                        "12:00:00 AM", "1:00:00 AM", "2:00:00 AM", "3:00:00 AM", "4:00:00 AM",
                                        "5:00:00 AM"), "Night", "Day"))


## Plotting the chart ======================================================

ggplot(data = hourlyCalories_summary) +
  geom_point(mapping = aes(x = Time, y = Calories_sum, 
                           color = TimeOfDay, group = Calories_sum)) +
  labs(title = "Daily_HourlyCalories",
       x = "Time",
       y = "Calories") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
