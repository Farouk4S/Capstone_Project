
# This is my Google Data Analytics Project code explained step-by-step.

# File:         03_01_Use_Levels_across_study
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     How long did users use the smart watch? 

# READING THE DATA #############################################################

# I started by Installed the tidyverse packages
      
      install.packages("tidyverse")
      library("tidyverse")

# So, I loaded the database from file directory. 
# Then i used some functions to verify the dataset properties

      library(readr)
      dailyActivity_merged <- read_csv("data/dailyActivity_merged.csv")
      sleepDay_merged <- read_csv("data/SleepDay_merged.csv")

#View(dailyActivity_merged.csv)
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
      
#      To check for duplicates for sleepDay_merged.    
      sum(duplicated(sleepDay_merged))
      
#      To delete duplicates for sleepDay_merged.      
      sleepDay_merged <- sleepDay_merged %>%
        distinct() %>%
        drop_na()

## Start of combined data  ====================================================
      
## Rename Date Columns for Consistency and merge
   dailyActivity_merged <- dailyActivity_merged %>%
   rename(Date = ActivityDate)
      

## Separate SleepDay column Data Separate Date and Time Columns =============================================
## So, I separated the TIMEDATE column into separate Date and Time columns 
## at the first space
      
    sleepDay_merged <- sleepDay_merged %>%
    separate(SleepDay, into = c("Date", "Time"), sep = " ", extra = "merge")
    
    ## Convert the "Date" columns to Date type from lubridate package
    sleepDay_merged$Date <- mdy(sleepDay_merged$Date)
    dailyActivity_merged$Date <- mdy(dailyActivity_merged$Date)
    
    ## Convert the "Date" columns format to "dd-mm-yyyy"
    sleepDay_merged$Date <- format(sleepDay_merged$Date, "%Y-%m-%d")   
    dailyActivity_merged$Date <- format(dailyActivity_merged$Date, "%Y-%m-%d")   
      
    
## Combined Data  =============================================
    
    # Note that there were more participant Ids in the daily activity
    # dataset that have been filtered out using merge since they did not have
    # sleep data recorded
    
    ## Merging these two datasets together ##  
    # combined_data <- full_join(sleepDay_merged, dailyActivity_merged, by = c("Id", "Date"))
    daily_activity_sleep <- merge(sleepDay_merged, dailyActivity_merged, 
                           by = c("Id", "Date"))
    
    # Now we can explore some different relationships between activity and sleep.
    # To take a look at how many participants are in this data set.
    
    n_distinct(combined_data$Id)
    #daily_activity_sleep
    
   
# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================
  ## Group and summarize by ID and create the Usage Table
    daily_use <- daily_activity_sleep %>%
      group_by(Id) %>%
      summarize(days_used=sum(n())) %>%
      mutate(Usage = case_when(
        days_used >= 1 & days_used <= 10 ~ "Low use",
        days_used >= 11 & days_used <= 20 ~ "Moderate use", 
        days_used >= 21 & days_used <= 31 ~ "High use", 
      ))
    
    head(daily_use)
    
  
  ## Group and calculate Average by ID
    Daily_use_percent <- daily_use %>%
      group_by(Usage) %>%
      summarise(Freq = n())%>%
      mutate(Totals = sum(Freq))%>%
      group_by(Usage) %>%
      summarise(Total_percent = Freq / Totals)%>%
      mutate(labels = scales::percent(Total_percent))
    
  ## To put the labeling in order
    Daily_use_percent$Usage <- factor(Daily_use_percent$Usage, 
                            levels = c("High use", "Moderate use", "Low use"))
    
   head(Daily_use_percent)
    
# PLOTTING THE DATA ###########################################################


## Preparing the Plot =====================================================

## Plotting The 2nd Chart  ========================================================
  
   ggplot(data = Daily_use_percent, aes(x = "", y = Total_percent, fill = Usage)) +
     geom_bar(stat = "identity", width = 1, color = "transparent") +
     coord_polar("y") +
     geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
     scale_fill_manual(values = c("High use" = "green",
                                  "Moderate use" = "lightgreen", 
                                  "Low use" = "grey")) +
     labs(title = "Use Level Distribution",
          fill = "Usage",
          x = NULL,
          y = NULL) +
     theme_minimal() +
     theme(axis.text = element_blank(),
           axis.title = element_blank(),
           panel.grid = element_blank(),
           plot.title = element_text(hjust = 0.5))
   
   