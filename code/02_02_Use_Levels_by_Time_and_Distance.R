
# This is my Google Data Analytics Project code explained step-by-step.

# File:         02_02_Use_Levels_by_Time_and_Distance
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
dailyIntensities_merged <- read_csv("data/dailyIntensities_merged.csv")
#View(dailyIntensities_merged)
head(dailyIntensities_merged)


#  To check for duplicates for sleepDay_merged.    
sum(duplicated(dailyIntensities_merged))

#  PREPARING THE DATA ########################################################

# To be able to know 

## Convert the Date format to "dd-mm-yyyy"

## Convert the Sleep "ActivityDay" column to Date type from lubridate package
dailyIntensities_merged$ActivityDay <- mdy(dailyIntensities_merged$ActivityDay)

## Convert the Date format to "dd-mm-yyyy"
dailyIntensities_merged$ActivityDay <- format(
                            dailyIntensities_merged$ActivityDay, "%Y-%m-%d")  

# TRANSFORMING THE DATA ########################################################

## Group and Summarize Multiple Columns ========================================
## Group and summarize by Day of Week

Intensities_summary <- (dailyIntensities_merged) %>%
  group_by(ActivityDay) %>%
  summarise(TotalSedentaryMinutes = sum(SedentaryMinutes, na.rm = TRUE), 
            TotalLightlyActiveMinutes = sum(LightlyActiveMinutes, na.rm = TRUE),
            TotalFairlyActiveMinutes = sum(FairlyActiveMinutes, na.rm = TRUE),
            TotalVeryActiveMinutes = sum(VeryActiveMinutes , na.rm = TRUE),
            TotalSedentaryActiveDistance = sum(SedentaryActiveDistance, na.rm = TRUE),
            TotalLightActiveDistance = sum(LightActiveDistance, na.rm = TRUE),
            TotalModeratelyActiveDistance = sum(ModeratelyActiveDistance, na.rm = TRUE),
            TotalVeryActiveDistance = sum(VeryActiveDistance, na.rm = TRUE)) %>%
  select( c('TotalSedentaryMinutes', 'TotalLightlyActiveMinutes', 
                     'TotalFairlyActiveMinutes', 'TotalVeryActiveMinutes',
                     'TotalSedentaryActiveDistance', 'TotalLightActiveDistance',
                     'TotalModeratelyActiveDistance', 'TotalVeryActiveDistance'))


# PLOTTING THE DATA ###########################################################

## As i am focusing on the time use of the App

Summary_Minutes <-   select(Intensities_summary, c('TotalSedentaryMinutes', 'TotalLightlyActiveMinutes', 
                                                   'TotalFairlyActiveMinutes', 'TotalVeryActiveMinutes'))

# Calculate the sum for each column as a dataframe
summary_Minsrow <- data.frame(Value = colSums(Summary_Minutes))

Total_Mins <- c(sum(Summary_Minutes))
# Total_Mins <- sum(colSums(Summary_Minutes))


labels_mins <- c("TotalSedentaryMinutes", "TotalLightlyActiveMinutes", 
                 "TotalFairlyActiveMinutes", "TotalVeryActiveMinutes")

# Calculate Time percentages
percentage_time <- summary_Minsrow / Total_Mins * 100

## Preparing the Plot =====================================================

# Extract numeric value from percentage_time
num_percent_time <- as.numeric(percentage_time$Value)

# Format labels as percentages
formatted_labels_mins <- paste0(sprintf("%.1f%%", num_percent_time))

# Create a color palette
colors <- rainbow(length(num_percent_time))


## Plotting The Time Use Chart  ===============================================

# Plot the pie chart with formatted labels
pie(num_percent_time, col = colors,
    main = "Pie Chart of Use Time") # Labels set to NA
legend("topright", legend = labels_mins, fill = colors, cex = 0.8) 
legend("topleft", legend = formatted_labels_mins, fill = colors, cex = 0.8)




## If I focus on the Distance covered in using the App

Summary_distances <-   select(
  Intensities_summary, c('TotalSedentaryActiveDistance', 
                         'TotalLightActiveDistance',
                         'TotalModeratelyActiveDistance', 
                         'TotalVeryActiveDistance'))

# Calculate the sum for each column as a dataframe
summary_disrow <- data.frame(Value = colSums(Summary_distances))
Total_dis <- c(sum(Summary_distances))

labels_dis <- c("TotalSedentaryActiveDistance", "TotalLightActiveDistance", 
                "TotalModeratelyActiveDistance", "TotalVeryActiveDistance")

# Calculate Distance percentages
percentage_dis <- summary_disrow / Total_dis * 100

# View(percentage_dis)


## Preparing the Distance use Plot ===========================================

# Extract numeric value from percentage_time
num_percent_dis <- as.numeric(percentage_dis$Value)


# Format labels as percentages
formatted_labels_dis <- paste0(sprintf("%.1f%%", num_percent_dis))


# Create a color palette
colors_dis <- rainbow(length(num_percent_dis))

## Plotting The Distance Use Chart  ===========================================

# Plot the pie chart for distance with formatted labels
pie(num_percent_dis, col = colors,
    main = "Users by Distance") # Labels set to NA
legend("topright", legend = labels_dis, fill = colors, cex = 0.8) 
legend("topleft", legend = formatted_labels_dis, fill = colors, cex = 0.8)



