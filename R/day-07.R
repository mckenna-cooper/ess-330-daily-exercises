# Name: [Mckenna Cooper]
# Date: [02/20/2025]
# Purpose: Analyze COVID-19 data, create visualizations for top 6 states and total daily cases.

# Load necessary libraries
library(tidyverse)

# Read in COVID-19 data
covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# Question 1: Faceted line plot of the 6 states with most cases
top_states <- covid_data %>%
  filter(date == max(date)) %>%  # Get the latest date
  arrange(desc(cases)) %>%  # Sort by cases
  slice_head(n = 6) %>%  # Select top 6 states
  pull(state)  # Extract state names

covid_top6 <- covid_data %>%
  filter(state %in% top_states)

plot1 <- ggplot(covid_top6, aes(x = date, y = cases, color = state)) +
  geom_line() +
  facet_wrap(~ state) +
  labs(title = "COVID-19 Cases Over Time in Top 6 States",
       x = "Date", y = "Total Cases") +
  theme_minimal()

# Save the plot
ggsave("img/top6_states_cases.png", plot1, width = 8, height = 6)

# Question 2: Column plot of daily total cases in the USA
daily_cases <- covid_data %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE))

plot2 <- ggplot(daily_cases, aes(x = date, y = total_cases)) +
  geom_col(fill = "steelblue") +
  labs(title = "Total COVID-19 Cases in the USA Per Day",
       x = "Date", y = "Total Cases") +
  theme_minimal()

# Save the plot
ggsave("img/usa_daily_cases.png", plot2, width = 8, height = 6)


