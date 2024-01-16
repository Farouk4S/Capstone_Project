
# This is my Google Data Analytics Project code explained step-by-step.

# File:         01_01_Analysis
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     Users Step Data Analysis for 31 Days Period

# READING THE DATA #############################################################

# I started by Installed the tidyverse packages

install.packages("tidyverse")
library("tidyverse")

# So, I loaded the database from file directory. 
# Then i used some functions to verify the dataset properties

library(readr)
dailyActivity_merged <- read_csv("data/dailyActivity_merged.csv")
#View(dailyActivity_merged)
head(dailyActivity_merged)

# I needed to change the date formats. 
# As my data frame is named 'dailyActivity_merged' and 
# existing character is mdy format. 
# I ran this code that converts characters to standard R format of YYYY-MM-DD.

library("dplyr")

dailyActivity_merged$ActivityDate <- mdy(dailyActivity_merged$ActivityDate)

#  PREPARING THE DATA ########################################################

# Then, I converted the Date format to "dd-mm-yyyy"

dailyActivity_merged$ActivityDate <- format(dailyActivity_merged$ActivityDate, "%Y-%m-%d")
head(dailyActivity_merged)

# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================

summary_data <- dailyActivity_merged %>%
  group_by(ActivityDate) %>%
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
  pivot_longer(cols = -ActivityDate,
               names_to = "Variable", 
               values_to = "Values")
head(summary_data_long)

summary_Activity_long <- dailyActivity_merged %>%
  pivot_longer(cols = -ActivityDate, names_to = "Variable", 
               values_to = "Values")

head(summary_Activity_long)

# Convert ActivityDate to Date class
summary_data_long <- summary_data_long %>%
  mutate(Order = as.Date(ActivityDate))


# PLOTTING THE DATA ########################################################

## Plotting the chart

library(ggplot2)

ggplot(data = summary_data_long %>% filter(Variable %in% c("Sum_TotalSteps", "Sum_Calories"))) +
  geom_line(mapping=aes(x = ActivityDate, y = Values, 
                        color = Variable, group = Variable)) +
  labs(title = "Daily Activity Snapshot",
       x = "Date",
       y = "Value") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

