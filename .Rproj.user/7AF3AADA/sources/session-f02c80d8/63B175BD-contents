# Load the necessary libraries
library(tidyverse)

# Load the dataset
file_path <- "C:/Users/yamke/Downloads/airline_2m/airline_2m.csv"
airline_data <- read.csv( file_path, nrows = 8000)

# Question 2: Which airlines contribute the most to airline delays?

# Data Exploration
# Summarize and explore the dataset
summary_airline_delays <- airline_data %>%
  select(Reporting_Airline, ArrDelayMinutes)

# Group by Reporting_Airline and calculate total delay minutes
airline_delay_summary <- summary_airline_delays %>%
  group_by(Reporting_Airline) %>%
  summarise(TotalDelayMinutes = sum(ArrDelayMinutes, na.rm = TRUE))

# Sort the airlines by total delay minutes (highest to lowest)
airline_delay_summary <- airline_delay_summary %>%
  arrange(desc(TotalDelayMinutes))

# Create a bar plot to visualize the airlines contributing the most to delays
top_airlines_plot <- airline_delay_summary %>%
  head(10) %>%  # Display the top 10 airlines
  ggplot(aes(x = reorder(Reporting_Airline, -TotalDelayMinutes), y = TotalDelayMinutes, fill = Reporting_Airline)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Airlines Contributing the Most to Airline Delays",
    x = "Airline",
    y = "Total Delay Minutes",
    fill = "Airline"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the bar plot to the question_2_output folder
ggsave("outputs/question_2_output/top_airlines_bar_plot.png", plot = top_airlines_plot, height = 5, width = 8)
