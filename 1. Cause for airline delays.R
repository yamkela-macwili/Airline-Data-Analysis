# Load the necessary libraries
library(tidyverse)

# Load the dataset
file_path <- "C:/Users/yamke/Downloads/airline_2m/airline_2m.csv"
airline_data <- read.csv( file_path, nrows = 8000)

# Question 1: What is the leading cause for airline delays?

# Data Exploration
# Summarize and explore the dataset
summary_airline_delays <- airline_data %>%
  select(ArrDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)

# Create a summary table
summary_table <- summary_airline_delays %>%
  summarise(
    TotalFlights = n(),
    TotalArrivalDelays = sum(ArrDelayMinutes, na.rm = TRUE),
    TotalCarrierDelays = sum(CarrierDelay, na.rm = TRUE),
    TotalWeatherDelays = sum(WeatherDelay, na.rm = TRUE),
    TotalNASDelays = sum(NASDelay, na.rm = TRUE),
    TotalSecurityDelays = sum(SecurityDelay, na.rm = TRUE),
    TotalLateAircraftDelays = sum(LateAircraftDelay, na.rm = TRUE)
  )

# Calculate percentages of delays
summary_table <- summary_table %>%
  mutate(
    PercentCarrierDelays = (TotalCarrierDelays / TotalArrivalDelays) * 100,
    PercentWeatherDelays = (TotalWeatherDelays / TotalArrivalDelays) * 100,
    PercentNASDelays = (TotalNASDelays / TotalArrivalDelays) * 100,
    PercentSecurityDelays = (TotalSecurityDelays / TotalArrivalDelays) * 100,
    PercentLateAircraftDelays = (TotalLateAircraftDelays / TotalArrivalDelays) * 100
  )

# Create a bar plot to visualize the leading causes of delays
bar_plot <- summary_table %>%
  pivot_longer(cols = starts_with("Percent"), names_to = "Cause", values_to = "Percent") %>%
  ggplot(aes(x = reorder(Cause, -Percent), y = Percent, fill = Cause)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Leading Causes of Airline Delays",
    x = "Delay Cause",
    y = "Percentage of Delays",
    fill = "Delay Cause"
  ) +
  theme_minimal() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45 , hjust = 1))  # Hide legend for simplicity

# Save the bar plot to the question_1_output folder
ggsave("outputs/question_1_output/leading_cause_bar_plot.png", plot = bar_plot, height = 5, width = 8)
