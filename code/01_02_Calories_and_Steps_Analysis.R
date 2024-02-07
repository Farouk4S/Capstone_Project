
# This is my Google Data Analytics Project code explained step-by-step.

# File:         01_02_Calories_and_Steps_Analysis
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     Users Calories and Steps Data Analysis across a day

# READING THE DATA #############################################################

# I started by Installed the tidyverse packages

install.packages("tidyverse")
library("tidyverse")

# So, I loaded the database from file directory. 
# Then i used some functions to verify the dataset properties

library(readr)
hourlyCalories_merged <- read_csv("data/hourlyCalories_merged.csv")
head(hourlyCalories_merged)

hourlySteps_merged <- read_csv("data/hourlySteps_merged.csv")
#View(hourlySteps_merged)
head(hourlySteps_merged)

#    To check for duplicates for sleepDay_merged. 
sum(duplicated(hourlySteps_merged))
sum(duplicated(hourlyCalories_merged))

     
#  PREPARING THE DATA ########################################################

# To be able to know summary data by each hour, I needed to Split 
# the TIMEDATE column into separate Date and Time columns at the first space

hourlyCalories <- hourlyCalories_merged %>%
  separate(ActivityHour, into = c("Date", "Time"), sep = " ", extra = "merge")
head(hourlyCalories) 

hourlySteps <- hourlySteps_merged %>%
  separate(ActivityHour, into = c("Date", "Time"), sep = " ", extra = "merge") 


# TRANSFORMING THE DATA ########################################################

# since the two datasets share common columns, 
# they can be merged using these columns
## Start of combined data  ====================================================

## Merging these two datasets together ##  
hourlySteps_and_Calories <- merge(hourlyCalories, hourlySteps, 
                                  by = c("Id", "Date", "Time"))

# Take a look at how many participants are in this data set.

n_distinct(hourlySteps_and_Calories$Id)


## Group and Summarize Multiple Columns ========================================
## First, convert the Time column to a factor with a custom order

hourlySteps_and_Calories$Time <- factor(hourlySteps_and_Calories$Time,
         levels = c("12:00:00 AM", "1:00:00 AM", "2:00:00 AM", "3:00:00 AM", 
                     "4:00:00 AM", "5:00:00 AM", "6:00:00 AM", "7:00:00 AM",
                     "8:00:00 AM", "9:00:00 AM", "10:00:00 AM", "11:00:00 AM",
                     "12:00:00 PM", "1:00:00 PM", "2:00:00 PM", "3:00:00 PM", 
                     "4:00:00 PM", "5:00:00 PM", "6:00:00 PM", "7:00:00 PM",
                     "8:00:00 PM", "9:00:00 PM", "10:00:00 PM", "11:00:00 PM"))

hourlyS_and_C_sum <- (hourlySteps_and_Calories) %>%
  group_by(Time) %>%
  select(Time, Calories, StepTotal) %>%
  summarise(Calories_sum = sum(Calories, na.rm = TRUE),
            Steps_sum = sum(StepTotal, na.rm = TRUE))

head(hourlyS_and_C_sum)


# PLOTTING THE DATA ########################################################

## Preparing the Plot =====================================================

# Add a new variable to indicate night or day
hourlyS_and_C_sum <- hourlyS_and_C_sum %>%
  mutate(TimeOfDay = ifelse(Time %in% c("9:00:00 PM", "10:00:00 PM", "11:00:00 PM", 
                                        "12:00:00 AM", "1:00:00 AM", "2:00:00 AM", "3:00:00 AM", "4:00:00 AM",
                                        "5:00:00 AM"), "Night", "Day"))


## Plotting the chart ======================================================
install.packages("gridExtra")

plot1 <- ggplot(data = hourlyS_and_C_sum) +
  geom_point(mapping = aes(x = Time, y = Calories_sum, 
                           color = TimeOfDay, group = Calories_sum)) +
  labs(title = "Daily_HourlyCalories",
       x = "Time",
       y = "Calories") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 120000)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
legend.position = "none")  # Remove the legend for TimeOfDay


plot2<- ggplot(data = hourlyS_and_C_sum) +
    geom_point(mapping = aes(x = Time, y = Steps_sum, 
                           color = TimeOfDay, group = Steps_sum)) +
  labs(title = "Daily_HourlySteps",
       x = "Time",
       y = "Steps") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

library("gridExtra")

grid.arrange(plot1, plot2, nrow = 1)

