
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
      combined_data <- merge(sleepDay_merged, dailyActivity_merged, by="Id")
      # Take a look at how many participants are in this data set.
      
      n_distinct(combined_data$Id)
      
# Note that there were more participant Ids in the daily activity
# dataset that have been filtered out using merge. Consider using 'outer_join'
# to keep those in the dataset.

      
# Now you can explore some different relationships between activity and sleep as well.
 library("dplyr")
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
  ## Group and summarize by Date
  
  Combined_d_s_byDate <-  Combined_d_s %>%
    group_by (Date, ActivityDate) %>%
    select("Date", "ActivityDate", "TotalMinutesAsleep", "TotalSteps") %>%
  summarise ( TimeSleeping = sum(TotalMinutesAsleep), 
              StepTotal = sum(TotalSteps, na.rm = TRUE)) 

  ## Extract day of the week
  Combined_d_s_byDate <- Combined_d_s_byDate %>%
    mutate(Day_of_Wk = wday(Date, label = TRUE, abbr = FALSE))
  
  ## Group and summarize by day of the week
  
  Combined_d_s_byDate <-  Combined_d_s_byDate %>%
    group_by (Day_of_Wk) %>%
    select("Day_of_Wk", "TimeSleeping", "StepTotal")%>%
    summarise ( TimeSleeping = sum(TimeSleeping), 
                StepTotal = sum(StepTotal, na.rm = TRUE)) 
  

  
# PLOTTING THE DATA ###########################################################


##  Plotting a few explorations  ===============================================

  library("ggplot2")

  ### Scatter plot for Total Steps and Sedentary Minutes =========================
  # Relationship between steps taken in a day and sedentary minutes?
  
  ggplot(data=dailyActivity_merged, aes(x= TotalSteps, y=SedentaryMinutes )) + geom_point()
  
  ### Scatter plot (Correlation) for TotalMinutesAsleep and TotalTimeInBed =====
  # Relationship between between minutes asleep and time in bed?
  
  ggplot(data=sleepDay_merged, 
         aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point() +
    labs(title = "Did more time in Bed get users more Sleep?",
         x = "User's Time Sleeping",
         y = "User's Time in Bed") 
  
  ### Now you can explore some different relationships 
  ### between activity and sleep as well.
  ### Do Users who sleep more also take more steps or fewer? ==================  
  
  # To plot and display x-axis numbers without scientific notation
  # Set scipen option to a high value to avoid scientific notation
  options(scipen = 999)
  
  # # Your x_col and y_col variables to use for the scatterplot
  x_col <- "TimeSleeping"
  y_col <- "StepTotal"
  
  # Plot the scatterplot
  plot(
    Combined_d_s_byDate[[x_col]],
    Combined_d_s_byDate[[y_col]],
    main = "More Sleep from more or fewer Steps",
    xlab = x_col,
    ylab = y_col,
    pch = 16,
    col = "blue") 
  
  # Customize x-axis labels to display without scientific notation
  axis(1, at = pretty(Combined_d_s_byDate[[x_col]]), 
    labels = format(pretty(Combined_d_s_byDate[[x_col]]), scientific = FALSE))
  
  
  
  ### Correlation of TotalSteps and Calories ==================================
  
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
  
  
  
  ### WASTE ============================================
  
  
  # "Scatterplot Example"
  # Columns to use for the scatterplot
  #  x_col <- "StepTotal"
  #  y_col <- "TimeSleeping"
  
  # Plot the scatterplot
  # plot(
  #    Combined_d_s_byID[[x_col]],
  #    Combined_d_s_byID[[y_col]],
  #    main = "Scatterplot Example",
  #    xlab = x_col,
  #    ylab = y_col,
  #    pch = 16,
  #    col = "blue") 
  
  
  
  # Add a grouping variable for each pair of dots
  Combined_d_s_byDate <- transform(Combined_d_s_byDate, 
                                   Group = rep(1:(nrow(Combined_d_s_byDate)/2), each = 2))
  
  # Extract unique values of Day_of_Wk and specify the desired order
  days_order <- c("Sunday", "Monday", "Tuesday", 
                  "Wednesday", "Thursday", "Friday", "Saturday")
  unique_days <- unique(Combined_d_s_byDate$Day_of_Wk)
  
  
  ggplot(Combined_d_s_byDate, 
         aes(x = StepTotal, 
             y = factor(Day_of_Wk, levels = rev(unique_days)),# Reverse the order
             group = Group)) +
    geom_segment(aes(xend = 0,
                     yend = factor(Day_of_Wk, 
                                   levels = rev(unique_days))),  # Reverse the order
                 color = "transparent") +
    geom_point(aes(color = Day_of_Wk), size = 3) +
    geom_line(color = "black") +
    geom_text(aes(label = Day_of_Wk), position = position_stack(vjust = 0.5), 
              vjust = -0.5, color = "transparent") +
    labs(title = "Weekday Sleep Pattern",
         x = "Total Minutes",
         y = "Day of Week",
         color = "Variable") +
    scale_color_manual(values = c("TimeSleeping" = "blue", "StepTotal" = "green")) +
    theme_minimal()
  
  
  
## Plotting The Chart  ========================================================

  
  ggplot(data = Combined_d_s_byDate) +
    geom_point(mapping = aes(x = TimeSleeping, y = StepTotal, 
                             color = Day_of_Wk)) +
    geom_text(mapping = aes(x = TimeSleeping, y = StepTotal, label = Day_of_Wk), 
              vjust = -0.5, hjust = 0.5) +  # Adjust vjust and hjust for label positioning
    labs(title = "More Steps gets you more Sleep",
         x = "User's Time Sleeping",
         y = "User's Total Steps") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) + # Use comma_format for x-axis labels
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    theme(legend.position="none")  # Remove legend 
  
