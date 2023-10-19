# Load the necessary libraries
library(tidyverse)

# Load the dataset
airline_data <- read.csv("data/airline_data.csv")

# Question 4: How do airline delays vary throughout the year?

# Data Exploration
# Select relevant columns for analysis
delay_data <- airline_data %>%
  select(Month, Quarter, ArrDelayMinutes)

# Calculate mean delay minutes by month and quarter
monthly_delay_summary <- delay_data %>%
  group_by(Month) %>%
  summarise(MeanDelayMinutes = mean(ArrDelayMinutes, na.rm = TRUE))

quarterly_delay_summary <- delay_data %>%
  group_by(Quarter) %>%
  summarise(MeanDelayMinutes = mean(ArrDelayMinutes, na.rm = TRUE))

# Create line plots to visualize delay variations
monthly_plot <- monthly_delay_summary %>%
  ggplot(aes(x = Month, y = MeanDelayMinutes)) +
  geom_line() +
  labs(
    title = "Monthly Variation of Airline Delays",
    x = "Month",
    y = "Mean Delay Minutes"
  ) +
  theme_minimal()

quarterly_plot <- quarterly_delay_summary %>%
  ggplot(aes(x = Quarter, y = MeanDelayMinutes)) +
  geom_line() +
  labs(
    title = "Quarterly Variation of Airline Delays",
    x = "Quarter",
    y = "Mean Delay Minutes"
  ) +
  theme_minimal()

# Save the line plots to the question_4_output folder
ggsave("outputs/question_4_output/monthly_delay_variation.png", plot = monthly_plot, height = 5, width = 8)
ggsave("outputs/question_4_output/quarterly_delay_variation.png", plot = quarterly_plot, height = 5, width = 8)
