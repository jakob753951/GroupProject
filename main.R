renv::install()
library(dplyr)
library(countrycode)
library(tidyverse)
library(gganimate)
library(gifski)
library(transformr)
library(plotly)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)
data$Continent <- countrycode(data$Country, "country.name", "continent")

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)


averages_measles <- data.frame(Country = character(), Average = numeric(), stringsAsFactors = FALSE)
averages_under_five_deaths <- data.frame(Country = character(), Average = numeric(), stringsAsFactors = FALSE)


# Get unique countries in the data frame
unique_countries <- unique(data$Country)

# Iterate through each country
for (country in unique_countries) {
  # Subset data for the current country
  country_data <- data[data$Country == country, ]
  
  # Calculate the average for the current country
  measles_average <- mean(country_data$Measles)
  under_five_death_average <- mean(country_data$under.five.deaths)
  #average <- rbind(measles_average, under_five_death_average)
  
  # Append the result to the averages data frame
  averages_measles <- rbind(averages_measles, data.frame(Country = country, "Measles Average" = measles_average))
  averages_under_five_deaths <- rbind(averages_under_five_deaths, data.frame(Country = country, "Under Five Deaths Average" = under_five_death_average))
  
}

data <- left_join(data, averages_measles, by = "Country")
data <- left_join(data, averages_under_five_deaths, by = "Country")

# Scatterplot of Measles vs. Death under age of 5
data_europe <- data %>% filter(Continent == "Europe")
data_asia   <- data %>% filter(Continent == "Asia")
data_africa <- data %>% filter(Continent == "Africa")
data_americas <- data %>% filter(Continent == "Americas")
data_oceania<- data %>% filter(Continent == "Oceania")

### Fit a linear regression model
# Europe
data_model_europe   <- subset(data_europe, Country != "Russian Federation")
linear_model_europe <- lm(data_model_europe$Measles.Average ~ data_model_europe$Under.Five.Deaths.Average, data = data_model_europe)
predicted_values_europe <- predict(linear_model_europe)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_europe[predicted_values_europe < 0] <- 0

scatter_plot_europe <- plot_ly(data_model_europe, x = ~data_model_europe$Under.Five.Deaths.Average, y = ~data_model_europe$Measles.Average, text = data_model_europe$Country, mode = "markers", type = "scatter", name = "European countries") %>%  #, showlegend = TRUE
  layout(title = "Interactive Scatter Plot", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Measle Cases")) %>%
  add_lines(x = ~data_model_europe$Under.Five.Deaths.Average, y = ~predicted_values_europe, line = list(color = 'red'), name = "Linear regression") #, showlegend = FALSE
scatter_plot_europe

# Asia
data_model_asia   <- subset(data_asia)#, Country != "Russian Federation")
linear_model_asia <- lm(data_model_asia$Measles.Average ~ data_model_asia$Under.Five.Deaths.Average, data = data_model_asia)
predicted_values_asia <- predict(linear_model_asia)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_asia[predicted_values_asia < 0] <- 0

scatter_plot_asia <- plot_ly(data_model_asia, x = ~data_model_asia$Under.Five.Deaths.Average, y = ~data_model_asia$Measles.Average, text = data_model_asia$Country, mode = "markers", type = "scatter", name = "Asian countries") %>%
  layout(title = "Interactive Scatter Plot", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Measle Cases")) %>%
  add_lines(x = ~data_model_asia$Under.Five.Deaths.Average, y = ~predicted_values_asia, line = list(color = 'red'), name = "Linear regression")
scatter_plot_asia

# Africa
data_model_africa   <- subset(data_africa) #, Country) # != "Russian Federation")
linear_model_africa <- lm(data_model_africa$Measles.Average ~ data_model_africa$Under.Five.Deaths.Average, data = data_model_africa)
predicted_values_africa <- predict(linear_model_africa)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_africa[predicted_values_africa < 0] <- 0

scatter_plot_africa <- plot_ly(data_model_africa, x = ~data_model_africa$Under.Five.Deaths.Average, y = ~data_model_africa$Measles.Average, text = data_model_africa$Country, mode = "markers", type = "scatter", name = "African countries") %>%
  layout(title = "Interactive Scatter Plot", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Measle Cases")) %>%
  add_lines(x = ~data_model_africa$Under.Five.Deaths.Average, y = ~predicted_values_africa, line = list(color = 'red'), name = "Linear regression")
scatter_plot_africa

# Americas
data_model_americas   <- subset(data_americas)
linear_model_america <- lm(data_model_americas$Measles.Average ~ data_model_americas$Under.Five.Deaths.Average, data = data_model_americas)
predicted_values_americas <- predict(linear_model_america)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_americas[predicted_values_americas < 0] <- 0

scatter_plot_americas <- plot_ly(data_model_americas, x = ~data_model_americas$Under.Five.Deaths.Average, y = ~data_model_americas$Measles.Average, text = data_model_americas$Country, mode = "markers", type = "scatter", name = "American countries") %>%
  layout(title = "Interactive Scatter Plot", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Measle Cases")) %>%
  add_lines(x = ~data_model_americas$Under.Five.Deaths.Average, y = ~predicted_values_americas, line = list(color = 'red'), name = "Linear regression")
scatter_plot_americas

# Oceania
data_model_oceania   <- subset(data_oceania)
linear_model_oceania <- lm(data_model_oceania$Measles.Average ~ data_model_oceania$Under.Five.Deaths.Average, data = data_model_oceania)
predicted_values_oceania <- predict(linear_model_oceania)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_oceania[predicted_values_oceania < 0] <- 0

scatter_plot_oceania <- plot_ly(data_model_oceania, x = ~data_model_oceania$Under.Five.Deaths.Average, y = ~data_model_oceania$Measles.Average, text = data_model_oceania$Country, mode = "markers", type = "scatter", name = "Oceanian countries") %>%
  layout(title = "Interactive Scatter Plot", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Measle Cases")) %>%
  add_lines(x = ~data_model_oceania$Under.Five.Deaths.Average, y = ~predicted_values_oceania, line = list(color = 'red'), name = "Linear regression")
scatter_plot_oceania

# Subplots
combined_plot <- subplot(scatter_plot_europe, scatter_plot_asia, scatter_plot_africa, scatter_plot_americas, scatter_plot_oceania)
combined_plot

