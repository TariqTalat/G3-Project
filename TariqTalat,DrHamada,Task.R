install.packages("ggplot2")
install.packages("dblyr")
install.packages("readr")


# First, import the required libraries
library(ggplot2)
library(dplyr)
library(readr)


# Set the file path
df <- read.csv("G3_sydney_hobart_times.csv")


# Display the first 5 values
head(df)


# Display the last 5 values
tail(df)


# Check the shape of the dataset
dim(df)
 

# Check for NULL values
sum(is.na(df))
#We Have 79 NuLL Values



# Descriptive statistics of the dataset
summary(df)



# Split the values in the 'Time' column and keep only the numbers
df$Time <- as.numeric(gsub("[^0-9.]+", "", df$Time))



# Check the 'Time' column again
head(df$Time)
sum(is.na(df$Time))
#7 Null Values
#Looking At the Dataset and the Std using Mean would be the best fit




# Calculate the mean of the 'Time' column
mean_time <- mean(df$Time, na.rm = TRUE)
# Fill missing values in the 'Time' column with the mean
df$Time[is.na(df$Time)] <- mean_time




# Check for missing values in the 'Time' column again
sum(is.na(df$Time))
df$Time <- round(df$Time, 2)



# Perform calculations on "Code Time less than 3"
df$`Time less than 3` <- df$Time < 3

df$`Completed the race` <- df$fleet_start - df$fleet_finish
# Analysis phase

# Find the year and value with the least 'Completed the race'
min_row <- df[which.min(df$`Completed the race`), ]
min_year <- min_row$Year
min_value <- min_row$`Completed the race`

# Find the year and value with the highest 'Completed the race'
max_row <- df[which.max(df$`Completed the race`), ]
max_year <- max_row$Year
max_value <- max_row$`Completed the race`

# Scatter plot of 'Year' vs 'Completed the race'
plt <- ggplot(df, aes(x = Year, y = `Completed the race`)) +
  geom_point() +
  geom_point(data = min_row, aes(x = min_year, y = min_value), color = "red", show.legend = FALSE) +
  geom_point(data = max_row, aes(x = max_year, y = max_value), color = "green", show.legend = FALSE) +
  geom_text(data = min_row, aes(x = min_year, y = min_value, label = min_year), nudge_y = 5, show.legend = FALSE) +
  geom_text(data = max_row, aes(x = max_year, y = max_value, label = max_year), nudge_y = 5, show.legend = FALSE) +
  labs(x = "Year", y = "Completed the race", title = "The difference between the Start And End") +
  theme_bw()



# Group the data by 'Year' and calculate the total fleet starts for each year
fleet_starts <- df %>% group_by(Year) %>% summarise(total_fleet_starts = sum(fleet_start))


# Sort the fleet starts in ascending order
sorted_fleet_starts <- fleet_starts[order(fleet_starts$total_fleet_starts), ]

# Create a bar plot to visualize the years with the most fleet starts
ggplot(sorted_fleet_starts, aes(x = Year, y = total_fleet_starts)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Fleet Starts") +
  ggtitle("Years with the Most Fleet Starts (Ascending Order)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Find the maximum number of fleet starts
max_fleet_starts <- max(df$fleet_start)

# Filter the data to get the year(s) with the maximum number of fleet starts
years_with_max_fleet_starts <- df$Year[df$fleet_start == max_fleet_starts]

# Print the year(s) with the maximum number of fleet starts
cat("Year(s) with the maximum number of fleet starts:", years_with_max_fleet_starts, "with", max_fleet_starts, "\n")

# Count the occurrences of each 'Time' value
time_counts <- count(df, Time)

# Create a bar chart to visualize the number of fleet finishes by time
ggplot(time_counts, aes(x = Time, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Time", y = "Number of fleet finishes") +
  ggtitle("Number of Fleet Finishes by Time")

# Create a new column 'Time less than 3' with 'More than 3' and 'Less than 3' values
df$`Time less than 3` <- ifelse(df$Time < 3, "Less than 3", "More than 3")

# Count the occurrences of 'More than 3' and 'Less than 3' in 'Time less than 3' column
time_less_than_3_counts <- count(df, `Time less than 3`)

# Create a pie chart to visualize the counts
ggplot(time_less_than_3_counts, aes(x = "", y = n, fill = `Time less than 3`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(fill = "Time less than 3") +
  ggtitle("Time less than 3") +
  theme_void()
