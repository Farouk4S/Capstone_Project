
# This is my Google Data Analytics Project code explained step-by-step.

# File:         02_01_Sleep_Analysis
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     How much sleep did users they get daily in the 31 Days Period?


# READING THE DATA #############################################################

# I started by Installed the tidyverse packages

install.packages("tidyverse")
library("tidyverse")

# So, I loaded the database from file directory. 
# Then i used some functions to verify the dataset properties

library(readr)
sleepDay_merged <- read_csv("data/sleepDay_merged.csv")
#View(sleepDay_merged)
head(sleepDay_merged)

#  PREPARING THE DATA ########################################################


#      To check for duplicates for sleepDay_merged.    
sum(duplicated(sleepDay_merged))

#      To delete duplicates for sleepDay_merged.      
sleepDay_merged <- sleepDay_merged %>%
  distinct() %>%
  drop_na()


# To be able to know the amount of sleep they got each day, I needed the
# days in a more friendly format.

## Separate Date and Time Columns =============================================
## So, I separated the TIMEDATE column into separate Date and Time columns 
## at the first space

DailySleep <- sleepDay_merged %>%
  separate(SleepDay, into = c("Date", "Time"), sep = " ", extra = "merge")
head(DailySleep) 

## Extract Day of The Week ====================================================
## Convert the Sleep "Date" column to Date type from lubridate package
DailySleep$Date <- mdy(DailySleep$Date)

## Convert the Date format to "dd-mm-yyyy"
DailySleep$Date <- format(DailySleep$Date, "%d-%m-%Y")  

## Extract day of the week
DailySleep_Day <- DailySleep %>%
  mutate(Day_of_Wk = wday(Date, label = TRUE, abbr = FALSE))

# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================
## Group and summarize by Day of Week

DaySleep_summary <- DailySleep_Day %>%
  group_by(Day_of_Wk) %>%
  summarise ( TimeSleeping = sum(TotalMinutesAsleep), 
              TimeInBed = sum(TotalTimeInBed, na.rm = TRUE))

## Convert DaySleep_summary to long format for plotting.

daysleep_long_sum <- DaySleep_summary %>%
  pivot_longer(cols = -Day_of_Wk,
               names_to = "Variable", 
               values_to = "Total_Minutes")


# PLOTTING THE DATA ###########################################################

# A Cleveland dot plot also referred to has is plotted to represent this data. 
# This allows the observation of the variables TimeInBed 
# and TimeSleeping group for each day of the week to be 
# groped together in its representation on the chart.

## Preparing the Plot =====================================================

# Add a grouping variable for each pair of dots
daysleep_long_sum <- transform(daysleep_long_sum, 
                               Group = rep(1:(nrow(daysleep_long_sum)/2), each = 2))

# Extract unique values of Day_of_Wk and specify the desired order
days_order <- c("Monday", "Tuesday", 
                "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
unique_days <- unique(daysleep_long_sum$Day_of_Wk)

## Plotting The Chart  ========================================================

ggplot(daysleep_long_sum, 
       aes(x = Total_Minutes, 
           y = factor(Day_of_Wk, levels = rev(unique_days)),# Reverse the order
           group = Group)) +
  geom_segment(aes(xend = 0,
                   yend = factor(Day_of_Wk, 
                   levels = rev(unique_days))),  # Reverse the order
                   color = "transparent") +
  geom_point(aes(color = Variable), size = 3) +
  geom_line(color = "black") +
  geom_text(aes(label = Variable), position = position_stack(vjust = 0.5), 
            vjust = -0.5, color = "transparent") +
  labs(title = "Weekday Sleep Pattern",
       x = "Total Minutes",
       y = "Day of Week",
       color = "Variable") +
  scale_color_manual(values = c("TimeSleeping" = "blue", "TimeInBed" = "green")) +
  theme_minimal()



