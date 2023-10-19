# Load the necessary libraries
library(tidyverse)

# Load the dataset
airline_data <- read.csv("data/airline_data.csv")

# Question 3: Do airport locations influence delay time?

# Data Exploration
# Select relevant columns for analysis
airport_delay_data <- airline_data %>%
  select(OriginCityName, DestCityName, ArrDelayMinutes)

# Group by origin and destination city pairs and calculate mean delay minutes
airport_delay_summary <- airport_delay_data %>%
  group_by(OriginCityName, DestCityName) %>%
  summarise(MeanDelayMinutes = mean(ArrDelayMinutes, na.rm = TRUE))

# Create a heatmap to visualize delay time between airport locations
heatmap_plot <- airport_delay_summary %>%
  pivot_wider(names_from = DestCityName, values_from = MeanDelayMinutes) %>%
  ggplot(aes(x = OriginCityName, y = DestCityName, fill = MeanDelayMinutes)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(
    title = "Airport Location Influence on Delay Time",
    x = "Origin City",
    y = "Destination City",
    fill = "Mean Delay Minutes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the heatmap plot to the question_3_output folder
ggsave("outputs/question_3_output/airport_location_heatmap.png", plot = heatmap_plot, height = 8, width = 10)
