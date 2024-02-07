
# This is my Google Data Analytics Project code explained step-by-step.

# File:         02_05_Step_Intensity_by_Hour_and_Day
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     How intense were users across the day and the week in the Period?


# READING THE DATA #############################################################

# I started by Installed the tidyverse packages
      
      install.packages("tidyverse")
      library("tidyverse")

# So, I loaded the database from file directory. 
# Then i used some functions to verify the dataset properties

      library(readr)
      hourlyIntensities_merged <- read_csv("data/hourlyIntensities_merged.csv")

     # sleepDay_merged <- read_csv("data/SleepDay_merged.csv")
      
      hourlySteps_merged <- read_csv("data/hourlySteps_merged.csv")
      #View(hourlySteps_merged)
      head(hourlySteps_merged)

#View(hourlyIntensities_merged)
      head(hourlyIntensities_merged)


#  PREPARING THE DATA ########################################################
      
  ##  Remove duplicates from data    
      sum(duplicated(hourlyIntensities_merged))
      #sum(duplicated(sleepDay_merged))
      sum(duplicated(hourlySteps_merged))
      
      #  To delete duplicates for sleepDay_merged.      
      sleepDay_merged <- sleepDay_merged %>%
        distinct() %>%
        drop_na()
      
  ## So, I separated the TIMEDATE column into separate Date and Time columns 
  ## at the first space
      
      hourlyIntensities <- hourlyIntensities_merged %>%
        separate(ActivityHour, into = c("Date", "Time"), sep = " ", extra = "merge")
      
      hourlySteps <- hourlySteps_merged %>%
        separate(ActivityHour, into = c("Date", "Time"), sep = " ", extra = "merge") 
      
      
      ## Convert the hourlyIntensities  "Date" column to Date type from lubridate package
      hourlyIntensities$Date <- mdy(hourlyIntensities$Date)
      hourlySteps$Date <- mdy(hourlySteps$Date)
      
      ## Convert the Date format to "YYYY-mm-dd"
      hourlyIntensities$Date <- format(hourlyIntensities$Date, "%Y-%m-%d")  
      hourlySteps$Date <- format(hourlySteps$Date, "%Y-%m-%d") 
      
      
## Start of combined data  ====================================================

## Merging these two datasets together ##  
      Inten_Steps <- merge(hourlyIntensities, hourlySteps, by= c("Id", "Time", "Date"))
      # View(Inten_Steps)
      
      # Take a look at how many participants are in this data set.
      
      # n_distinct(combined_data$Id)
 
 
# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================
  ## Group and summarize by Date
  
  ## Extract day of the week
      Inten_Steps <- Inten_Steps %>%
    mutate(Day_of_Wk = wday(Date, label = TRUE, abbr = FALSE))
  
  ## Group and summarize by day of the week
  
    Inten_Steps_byDay <-  Inten_Steps %>%
    group_by (Day_of_Wk) %>%
    select("Day_of_Wk", "TotalIntensity", "StepTotal") %>%
        summarise ( Daily_Intensity = mean(TotalIntensity), 
                    Daily_Steps = mean(StepTotal, na.rm = TRUE)) 

  ## Group and summarize by time of the day
      
    Inten_Steps_byTime <-  Inten_Steps %>%
    group_by (Time) %>%
        select("Time", "TotalIntensity", "StepTotal") %>%
        summarise ( TotalIntensity = sum(TotalIntensity), 
                    TotalSteps = sum(StepTotal, na.rm = TRUE)) 
  
## First, convert the Time column to a factor with a custom order
    
    Inten_Steps_byTime$Time <- factor(Inten_Steps_byTime$Time,
     levels = c("12:00:00 AM", "1:00:00 AM", "2:00:00 AM", "3:00:00 AM", 
                "4:00:00 AM", "5:00:00 AM", "6:00:00 AM", "7:00:00 AM",
               "8:00:00 AM", "9:00:00 AM", "10:00:00 AM", "11:00:00 AM",
                 "12:00:00 PM", "1:00:00 PM", "2:00:00 PM", "3:00:00 PM", 
                "4:00:00 PM", "5:00:00 PM", "6:00:00 PM", "7:00:00 PM",
               "8:00:00 PM", "9:00:00 PM", "10:00:00 PM", "11:00:00 PM"))
    

  
# PLOTTING THE DATA ###########################################################
    
  ## Preparing the Plot =====================================================
    
    # Add a new variable to indicate night or day
    Inten_Steps_byTime <- Inten_Steps_byTime %>%
      mutate(TimeOfDay = ifelse(Time %in% c("9:00:00 PM", "10:00:00 PM", "11:00:00 PM", 
                                            "12:00:00 AM", "1:00:00 AM", "2:00:00 AM", "3:00:00 AM", "4:00:00 AM",
                                            "5:00:00 AM"), "Night", "Day"))
    
  ## Plotting The Chart (1)  ========================================================

  
    ggplot(data = Inten_Steps_byTime) +
      geom_point(mapping = aes(x = Time, y = TotalIntensity, color = TimeOfDay, group = TotalIntensity)) +
      geom_bar(mapping = aes(x = Time, y = TotalSteps ), stat = "identity",
               position = "identity", alpha = 0.5, fill = "gray", color = "black") +
      labs(title = "Steps and Intensities across the day",
           x = "Time",
           y = "TotalIntensity") +
      scale_y_continuous(labels = scales::comma,
                         sec.axis = sec_axis(~ ., name = "TotalSteps",
                        labels = scales::comma_format())) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ## Plotting The Chart (2)  ========================================================
    
    
    ggplot(data = Inten_Steps_byDay) +
      geom_point(mapping = aes(x = Daily_Intensity, y = Daily_Steps, 
                               color = Day_of_Wk)) +
      geom_text(mapping = aes(x = Daily_Intensity, y = Daily_Steps, label = Day_of_Wk), 
                vjust = -0.5, hjust = 0.5) +  # Adjust vjust and hjust for label positioning
      labs(title = "Which days are users most active?",
           x = "User's Total Intensity",
           y = "User's Total Steps") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) + # Use comma_format for x-axis labels
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      theme(legend.position="none")  # Remove legend 
    
    
    
    