# Load the necessary libraries
library(tidyverse)

# Define the file path
file_path <- "C:/Users/yamke/Downloads/airline_2m/airline_2m.csv"
airline_data <- read.csv( file_path, nrows = 8000)


# Question 5: Can we predict the flight delay?

# Data Exploration
# Select relevant columns for analysis
delay_data <- airline_data %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)
delay_data%>% map(~sum(is.na(.)))
delay_data <- delay_data %>%
  drop_na()
# Perform multiple linear regression
linear_reg_model <- lm(ArrDelayMinutes ~ DepDelayMinutes + CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay, data = delay_data)

# Summary of the linear regression model
summary_linear_reg <- summary(linear_reg_model)
summary_linear_reg
plot(linear_reg_model)
# Save the summary to a text file
sink("outputs/question_5_output/linear_reg_summary.txt")
cat("Linear Regression Model Summary\n")
cat("----------------------------------\n")
cat(summary_linear_reg)
sink()

# Save a plot of the linear regression model
plot_linear_reg <- plot(linear_reg_model)
ggsave("outputs/question_5_output/linear_reg_plot.png", plot = plot_linear_reg, height = 5, width = 8)
