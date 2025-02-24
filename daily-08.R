# Mckenna Cooper
# 02/23/2025
# Generate COVID-19 data plots

covid_data <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

head(covid_data)

df_states <- data.frame(region = state.region, state = state.name, state_abb = state.abb)

covid_joined <- merge(covid_data, df_states, by.x = "state", by.y = "state", all.x = TRUE)
head(covid_joined)
covid_processed <- covid_joined %>%
  group_by(region, date) %>%
  arrange(date) %>%
  mutate(
    daily_cases = cases - lag(cases, default = 0),  # Calculate daily cases
    daily_deaths = deaths - lag(deaths, default = 0),  # Calculate daily deaths
    cumulative_cases = cumsum(daily_cases),  # Cumulative cases
    cumulative_deaths = cumsum(daily_deaths)  # Cumulative deaths
  )
covid_long <- covid_processed %>%
  pivot_longer(
    cols = c(cumulative_cases, cumulative_deaths),  # Columns to pivot
    names_to = "measure",   # New column for "cases" or "deaths"
    values_to = "count"     # Values for cumulative cases or deaths
  )
head(covid_long)
plot <- ggplot(covid_long, aes(x = as.Date(date), y = count, color = measure)) +
  geom_line() +  # Line plot for cumulative cases/deaths
  labs(
    title = "Cumulative COVID-19 Cases & Deaths by USA Region",
    x = "Date",
    y = "Count",
    color = "Measure"
  ) +
  facet_wrap(~ region) +  # Facet by region
  theme_minimal()  # Use a minimal theme for cleaner look

# Display the plot
print(plot)
ggsave("img/covid_region_plot.png", plot)

