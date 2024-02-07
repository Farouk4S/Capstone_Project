
# This is my Google Data Analytics Project code explained step-by-step.

# File:         02_03_Average_Weekly_Sleep_and_Step
# Project:      Google Data Analysis Capstone Project
# Title:        How users use smart watch
# Subtitle:     How much sleep did users get in the 31 Days Period?


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

##  Remove duplicates from data ==================
#  First, we check the datasets. 
# For the daily activity dataframe:
      dailyActivity_merged %>%
        select(TotalSteps,
               TotalDistance,
               SedentaryMinutes) %>%
        summary()
# Rename ActivityDate Column into Date in dailyActivity_merged
      dailyActivity_merged <- dailyActivity_merged %>%
        rename(Date = ActivityDate)

# For the sleep dataframe:
      sleepDay_merged %>%
        select(TotalSleepRecords,
               TotalMinutesAsleep,
               TotalTimeInBed) %>%
        summary()
      
 sum(duplicated(dailyActivity_merged))
      sum(duplicated(sleepDay_merged))
      sum(duplicated(hourlySteps_merged))

#  To delete duplicates for sleepDay_merged.      
      sleepDay_merged <- sleepDay_merged %>%
        distinct() %>%
        drop_na()


## Separate SleepDay column Data Separate Date and Time Columns ================
# So, I separated the TIMEDATE column into separate Date and Time columns 
# at the first space
      
      sleepDay_merged <- sleepDay_merged %>%
        separate(SleepDay, into = c("Date", "Time"), sep = " ", extra = "merge")
      
      ## Convert the "Date" columns to Date type from lubridate package
      sleepDay_merged$Date <- mdy(sleepDay_merged$Date)
      dailyActivity_merged$Date <- mdy(dailyActivity_merged$Date)
      
      ## Convert the "Date" columns format to "dd-mm-yyyy"
      sleepDay_merged$Date <- format(sleepDay_merged$Date, "%Y-%m-%d")   
      dailyActivity_merged$Date <- format(dailyActivity_merged$Date, "%Y-%m-%d")   
      
      
#  CHECKING FOR CORRELATIONS ============================================
    
    library("ggplot2")
    
    ## Total Steps and Sedentary Minutes =================================
    # Scatter plot relationship of steps taken 
    # in the period and sedentary minutes?
    
    ggplot(data=dailyActivity_merged, aes(x= TotalSteps, y=SedentaryMinutes )) + geom_point()
    
    ## TotalMinutesAsleep and TotalTimeInBed ============================
    # Correlation between between minutes asleep and time in bed?
    
    ggplot(data=sleepDay_merged, 
           aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point() +
      labs(title = "Did more time in Bed get users more Sleep?",
           x = "User's Time Sleeping",
           y = "User's Time in Bed") 
    

    ## TotalSteps and Calories ==================================
    ## Correlation of TotalSteps and Calories
    # Columns to use for the scatterplot
    x_col <- "Calories"
    y_col <- "TotalSteps"
    
    # Plot the scatterplot
    plot(
      dailyActivity_merged[[x_col]],
      dailyActivity_merged[[y_col]],
      main = "Calories from TotalSteps",
      xlab = x_col,
      ylab = y_col,
      pch = 16,
      col = "blue")
    

# TRANSFORMING THE DATA ########################################################
    
# Now we can explore some different relationships 
# between activity and sleep as well.
# To do this, we will combine sleepDay_merged and dailyActivity_merged datasets
    
## Combined Data  =============================================
    
    # Note that there were more participant Ids in the daily activity
    # dataset that have been filtered out using merge since they did not have
    # sleep data recorded
    
    ## Merging these two datasets together ##  
    # combined_data <- full_join(sleepDay_merged, dailyActivity_merged, by = c("Id", "Date"))
    combined_data <- merge(sleepDay_merged, dailyActivity_merged, 
                           by = c("Id", "Date"))
    
    # Now we can explore some different relationships between activity and sleep.
    # To take a look at how many participants are in this data set.
    
    n_distinct(combined_data$Id)
    #daily_activity_sleep

## Group and Summarize Multiple Columns ========================================
   
  ## Extract day of the week
  Combined_d_s <- combined_data %>%
    mutate(Day_of_Wk = wday(Date, label = TRUE, abbr = FALSE))
  
  ## Group and summarize the totals by day of the week
  
  Combined_d_s_byDate <-  Combined_d_s %>%
    group_by (Day_of_Wk) %>%
    select("Day_of_Wk", "TotalMinutesAsleep", "TotalSteps") %>%
  summarise ( TimeSleeping = sum(TotalMinutesAsleep), 
              TotalSteps = sum(TotalSteps, na.rm = TRUE)) 

  
  ## Group and summarise the average by day of the week
  
  Combined_d_s_byD_A <-  Combined_d_s %>%
    group_by (Day_of_Wk) %>%
    select("Day_of_Wk", "TotalMinutesAsleep", "TotalSteps")%>%
    summarise ( Daily_Sleep_Time = mean(TotalMinutesAsleep), 
                Daily_Steps = mean(TotalSteps, na.rm = TRUE)) 
  
  
# PLOTTING THE DATA ###########################################################

  
## Plotting The Chart (1) ========================================================
## Steps and Sleep by Day of Week
  
  ggplot(data = Combined_d_s_byDate) +
    geom_point(mapping = aes(x = TimeSleeping, y = TotalSteps, 
                             color = Day_of_Wk)) +
    geom_text(mapping = aes(x = TimeSleeping, y = TotalSteps, label = Day_of_Wk), 
              vjust = -0.5, hjust = 0.5) +  # Adjust vjust and hjust for label positioning
    labs(title = "Total Steps and Sleep by Day of Week",
         x = "User's Time Sleeping",
         y = "User's Total Steps") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) + # Use comma_format for x-axis labels
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    theme(legend.position="none")  # Remove legend 


## Plotting The Chart (2) #################################################
  
  ## Average Steps and Average Sleep across the  Week
  ## Preparing the Plot 
  Combined_d_s_byD_A$Day_of_Wk <- factor(Combined_d_s_byD_A$Day_of_Wk, 
            levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                       "Friday", "Saturday", "Sunday"))
  
  ## Plotting the chart 
  install.packages("gridExtra")
  
  plot1 <-    ggplot(Combined_d_s_byD_A, aes(x = Day_of_Wk, 
                                   y = Daily_Steps, 
                                   fill = "Steps")) +
    geom_col(fill = "#85e0e0") +
    geom_hline(yintercept = 7500) +
    labs(title = "Daily Steps per Day of the Week", x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  
  plot2  <-    ggplot(Combined_d_s_byD_A, aes(x = Day_of_Wk, 
                                   y = Daily_Sleep_Time, 
                                   fill = "Sleep")) +
    geom_col(fill = "#808000") +
    geom_hline(yintercept = 480) +
    labs(title = "Minutes Asleep per Day of the Week", x = "", y = "") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  
    
    library("gridExtra")
    
    grid.arrange(plot1, plot2, nrow = 1)
    
 