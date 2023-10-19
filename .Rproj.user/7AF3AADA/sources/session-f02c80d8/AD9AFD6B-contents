library("tidyverse")
library("Hmisc")

# Define the file path
file_path <- "C:/Users/yamke/Downloads/airline_2m/airline_2m.csv"
sub_airline <- read.csv( file_path, nrows = 8000)

glimpse(sub_airline)

#using purr::map to count missing values in all columns
sub_airline %>% map(~sum(is.na(.)))

# Select and view the delay-related columns
delay_columns <- select(sub_airline, contains("Delay"))
head(delay_columns)

#Replace the missing values 
sub_airline <- sub_airline %>%
  mutate(across(contains("Delay"), ~replace(., is.na(.), 0)))

sub_airline %>% map(~sum(is.na(.)))
glimpse(sub_airline)

#Identofy data types
sapply(sub_airline, typeof)

#Convert datatypes
mutate_all(type.convert) %>%
  mutate_if(is.character, as.numeric)

range(sub_airline$ArrDelay)
ggplot(data = sub_airline, mapping = aes(x = ArrDelay)) +
  geom_histogram(binwidth = 30, color = "white", fill = "red") +
  coord_cartesian(xlim = c(-60, 954))

# Descriptive Statistics 
# The Question: what causes fligt delays?

summary_airline_delay <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarise(mean = mean(ArrDelayMinutes), 
            std_dev = sd(ArrDelayMinutes))
summary_airline_delay

sub_airline %>% 
  count(Reporting_Airline)

# Create a simple average across Reporting_Airline and DayOfWeek
average_delays <- sub_airline %>% 
  group_by(Reporting_Airline, DayOfWeek)%>%
  summarise(mean_delays = mean(ArrDelayMinutes))

average_delays

#Sort the dataframe
average_delays %>%
  arrange(desc(mean_delays))

# Visualize the data using Heatmap
average_delays %>%
  ggplot(aes( x = Reporting_Airline,
              y = DayOfWeek,
              fill = mean_delays))+
  geom_tile(color = "white", size = 0.2)+
  scale_fill_gradient(low = "yellow",
                      high = "red")

# Linear relationship
# Correlation between 

ggplot(sub_airline,
       aes(DepDelayMinutes, ArrDelayMinutes))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(sub_airline,
       aes(CarrierDelay, ArrDelayMinutes))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(sub_airline,
       aes(WeatherDelay, ArrDelayMinutes))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(sub_airline,
       aes(NASDelay, ArrDelayMinutes))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(sub_airline,
       aes(SecurityDelay, ArrDelayMinutes))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(sub_airline,
       aes(LateAircraftDelay, ArrDelayMinutes))+
  geom_point()+
  geom_smooth(method = "lm")

corr_airline <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)

airline_correlation <- rcorr(as.matrix(corr_airline),
                             type = "pearson")
airline_correlation

correlation_matrix <- airline_correlation$r
correlation_matrix[lower.tri(correlation_matrix)] <- NA

correlation_matrix <- data.frame(row = rownames(airline_correlation$r), airline_correlation$r)

ggplot(correlation_matrix, aes(x = row, y = row))+
  geom_tile()+
  scale_fill_gradient(low = "blue", high = "red")+
  theme_minimal()+
  labs(title = "Correlation Heatmap")


# Model development 
# The question: Can we predict the arrival delay of a flight?

mult_linear_reg <- lm(ArrDelay ~ DepDelayMinutes + CarrierDelay + WeatherDelay + NASDelay + SecurityDelay + LateAircraftDelay, data = sub_airline)
summary(mult_linear_reg)

plot(mult_linear_reg)





