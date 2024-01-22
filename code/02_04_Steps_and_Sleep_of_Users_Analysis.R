
# This is my Google Data Analytics Project code explained step-by-step.

# File:         02_04_Steps_and_Sleep_of_Users_Analysis
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     Do Users who sleep more also take more steps or fewer? 

# READING THE DATA #############################################################

# I started by Installed the tidyverse packages
      
      install.packages("tidyverse")
      library("tidyverse")

# So, I loaded the database from file directory. 
# Then i used some functions to verify the dataset properties

      library(readr)
      dailyActivity_merged <- read_csv("data/dailyActivity_merged.csv")
      sleepDay_merged <- read_csv("data/SleepDay_merged.csv")

#View(dailyActivity_merged)
      head(dailyActivity_merged)

# n_distinct(dailyActivity_merged$Id)
# n_distinct(dailyActivity_merged$ActivityDate)
# n_distinct(sleepDay_merged$SleepDay)

#  PREPARING THE DATA ########################################################

# For the daily activity dataframe:
      dailyActivity_merged %>%
        select(TotalSteps,
               TotalDistance,
               SedentaryMinutes) %>%
        summary()


# For the sleep dataframe:
      sleepDay_merged %>%
        select(TotalSleepRecords,
               TotalMinutesAsleep,
               TotalTimeInBed) %>%
        summary()

## Start of combined data  ====================================================

## Merging these two datasets together ##  
      combined_data <- merge(sleepDay_merged, dailyActivity_merged, by = "Id")
      # Take a look at how many participants are in this data set.
      
      n_distinct(combined_data$Id)
      
# Note that there were more participant Ids in the daily activity
# dataset that have been filtered out using merge. Consider using 'outer_join'
# to keep those in the dataset.

      
# Now you can explore some different relationships between activity and sleep as well.
# combined_dat <- merge(sleepDay_merged, dailyActivity_merged, by="Id")
      
  combined_data <- full_join(sleepDay_merged, dailyActivity_merged, by="Id")
 
 
## Combined Data Separate Date and Time Columns =============================================
  ## So, I separated the TIMEDATE column into separate Date and Time columns 
  ## at the first space
  
  Combined_d_s <- combined_data %>%
    separate(SleepDay, into = c("Date", "Time"), sep = " ", extra = "merge")
  
  ## Convert the Sleep "Date" column to Date type from lubridate package
  Combined_d_s$Date <- mdy(Combined_d_s$Date)
  
  ## Convert the Date format to "dd-mm-yyyy"
  Combined_d_s$Date <- format(Combined_d_s$Date, "%Y-%m-%d")  
  
  ## Convert the Activity "Date" column to Date type from lubridate package
  Combined_d_s$ActivityDate <- mdy(Combined_d_s$ActivityDate)
  
  ## Convert the Date format to "dd-mm-yyyy"
  Combined_d_s$ActivityDate <- format(Combined_d_s$ActivityDate, "%Y-%m-%d")  
  
# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================
  ## Group and summarize by ID
  
  Combined_d_s_byID <- Combined_d_s %>%
    group_by(Id) %>%
    select(Id, TotalMinutesAsleep, TotalSteps) %>%
    summarise(
      TimeSleeping = sum(TotalMinutesAsleep),
      StepTotal = sum(TotalSteps, na.rm = TRUE)
    ) %>%
    mutate(UserID = row_number()) %>%
    select(UserID, TimeSleeping, StepTotal)
  
  
# PLOTTING THE DATA ###########################################################


## Preparing the Plot =====================================================

## Plotting The Chart  ========================================================

  ggplot(data = Combined_d_s_byID) +
    geom_point(mapping = aes(x = TimeSleeping, y = StepTotal, 
                             color = UserID)) +
    geom_text(mapping = aes(x = TimeSleeping, y = StepTotal, label = UserID), 
              vjust = -0.5, hjust = 0.5) +  # Adjust vjust and hjust for label positioning
    labs(title = "User's Level of Steps and Sleep",
         x = "User's Time Sleeping",
         y = "User's Total Steps") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) + # Use comma_format for x-axis labels
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    theme(legend.position="none")  # Remove legend 
  

  