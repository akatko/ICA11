# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)

# Set the path to the CSV file
file_path <- "/Users/akk/Downloads/Rscripts/flights.csv"

# Read the CSV file
flights <- read.csv(file_path)

# Inspect the unique values in the month column
unique_months <- unique(flights$month)
print(unique_months)

# Clean the month column by standardizing the month names
clean_month <- function(month) {
  month <- str_to_title(month)  # Standardize capitalization
  month <- str_replace_all(month, c("Jan" = "January", "Feb" = "February", "Mar" = "March",
                                    "Apr" = "April", "May" = "May", "Jun" = "June",
                                    "Jul" = "July", "Aug" = "August", "Sep" = "September",
                                    "Oct" = "October", "Nov" = "November", "Dec" = "December"))
  return(month)
}

flights$month <- sapply(flights$month, clean_month)

# Convert the cleaned month column to a factor with month labels
month_levels <- c("January", "February", "March", "April", "May", "June", 
                  "July", "August", "September", "October", "November", "December")

flights$month <- factor(flights$month, levels = month_levels, labels = month.abb)

# Verify the conversion
unique(flights$month)

# Generate the line plot for each month over the available years
ggplot(flights, aes(x = year, y = passengers, color = month, group = month)) +
  geom_line() +
  facet_wrap(~ month, scales = "fixed") +  # Use fixed scales to ensure consistent y-axis ticks
  labs(x = "Year", y = "Number of Passengers", title = "Monthly Flight Trends Over the Years") +
  theme_minimal() +
  scale_color_discrete(name = "Month") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


       