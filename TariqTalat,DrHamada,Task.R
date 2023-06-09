# First, install and load the required packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

library(readr)
library(dplyr)
library(ggplot2)

# Set the file path
filepath <- file.path(getwd(), "G3_sydney_hobart_times.csv")

# Load the dataset
df <- read_csv(filepath)

# Getting the first 5 values
head(df)

# Getting the last 5 values
tail(df)

# Getting the shape of the values
dim(df)

# Checking for NULL values
sum(is.na(df))

# Descriptive statistics about the values
summary(df)

# Dropping the NULL column
df <- df %>% select(-`Code Time less than 3`)

# Split the values in the 'Time' column and keep only the numbers
df$Time <- sapply(strsplit(df$Time, " "), "[", 1)

# Convert the values to numeric type
df$Time <- as.numeric(df$Time)

# Checking the 'Time' column again
length(df$Time)
head(df$Time)
sum(is.na(df$Time))

# Calculate the mean of the 'Time' column
mean_time <- mean(df$Time, na.rm = TRUE)

# Fill the missing values in the 'Time' column with the mean value
df$Time[is.na(df$Time)] <- mean_time

# One last check
sum(is.na(df$Time))
round(df$Time, 2)

# Asking and Analysis phase

# Calculate the difference between 'fleet_start' and 'fleet_finish' to get the completion time
df$Completed_the_race <- df$fleet_start - df$fleet_finish

# Sort the DataFrame by the completion time
sorted_df <- df[order(df$Completed_the_race), ]

# Get the row with the minimum completion time
min_row <- sorted_df[1, ]

# Get the year and the least value
min_year <- min_row$Year
min_value <- min_row$Completed_the_race

# Get the row with the maximum completion time
max_row <- sorted_df[nrow(sorted_df), ]

# Get the year and the maximum value
max_year <- max_row$Year
max_value <- max_row$Completed_the_race

# Create a scatter plot to visualize the completion times
plt <- ggplot(df, aes(x = Year, y = Completed_the_race)) +
  geom_point() +
  geom_point(data = min_row, aes(x = min_year, y = min_value), color = "red", show.legend = FALSE) +
  geom_point(data = max_row, aes(x = max_year, y = max_value), color = "green", show.legend = FALSE) +
  geom_text(data = min_row, aes(x = min_year, y = min_value, label = min_year), nudge_y = 5, show.legend = FALSE) +
  geom_text(data = max_row, aes(x = max_year, y = max_value, label = max_year), nudge_y = 5, show.legend = FALSE) +
  labs(x = "Year", y = "Completed the race", title = "The difference between the Start And End") +
  theme_bw()

print(plt)

# Group the data by "Year" and calculate the total fleet starts for each year
fleet_starts <- df %>% group_by(Year) %>% summarize(total_fleet_starts = sum(fleet_start))

# Sort the fleet starts in ascending order
sorted_fleet_starts <- fleet_starts[order(fleet_starts$total_fleet_starts), ]

# Get the years with the most fleet starts in ascending order
years_most_fleet_starts <- sorted_fleet_starts$Year

# Create a bar plot to visualize the years with the most fleet starts
plt <- ggplot(sorted_fleet_starts, aes(x = as.factor(Year), y = total_fleet_starts)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Fleet Starts", title = "Years with the Most Fleet Starts (Ascending Order)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plt)

# Find the maximum number of fleet starts
max_fleet_starts <- max(df$fleet_start)

# Filter the data to get the year(s) with the maximum number of fleet starts
years_with_max_fleet_starts <- df[df$fleet_start == max_fleet_starts, "Year"]

# Print the year(s) with the maximum number of fleet starts
cat("Year(s) with the maximum number of fleet starts:", years_with_max_fleet_starts, "with", max_fleet_starts, "\n")

# Group the DataFrame by "Time" and count the number of occurrences of each "Time" value
time_counts <- df %>% group_by(Time) %>% summarize(fleet_finish_count = n())

# Create a bar chart to visualize the number of fleet finishes by time
plt <- ggplot(time_counts, aes(x = Time, y = fleet_finish_count)) +
  geom_bar(stat = "identity") +
  labs(x = "Time", y = "Number of fleet finishes", title = "Number of Fleet Finishes by Time") +
  theme_bw()

print(plt)
