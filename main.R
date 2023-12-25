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

averages_data_x <- data.frame(Country = character(), Average = numeric(), stringsAsFactors = FALSE)
averages_data_y <- data.frame(Country = character(), Average = numeric(), stringsAsFactors = FALSE)

title_label <- "HIV/AIDS cases against adult mortality in different continents"
x_label <- "HIV/AIDS prevalence"
y_label <- "Mortality probability between 15 and 60 years in percent"

# Get unique countries in the data set
unique_countries <- unique(data$Country)

# Iterate through each country to get the average of the data
for (country in unique_countries) {
  # Subset data for the current country
  country_data <- data[data$Country == country, ]
  
  # Calculate the average for the current country
  data_x_average <- mean(country_data$HIV.AIDS)
  data_y_average <- mean(country_data$Adult.Mortality)/1000
  #average <- rbind(hepatitis_average, under_five_death_average)
  
  # Append the result to the averages data frame
  averages_data_x <- rbind(averages_data_x, data.frame(Country = country, "Data X Average" = data_x_average))
  averages_data_y <- rbind(averages_data_y, data.frame(Country = country, "Data Y Average" = data_y_average))
}
data <- left_join(data, averages_data_x, by = "Country")
data <- left_join(data, averages_data_y, by = "Country")

data_europe <- data %>% filter(Continent == "Europe")
data_asia   <- data %>% filter(Continent == "Asia")
data_africa <- data %>% filter(Continent == "Africa")
data_americas<-data %>% filter(Continent == "Americas")
data_oceania<- data %>% filter(Continent == "Oceania")

### Fit a linear regression model
# Africa
linear_model_africa <- lm(data_africa$Data.Y.Average ~ data_africa$Data.X.Average, data = data_africa)
predicted_values_africa <- predict(linear_model_africa)
rmse_africa <- sqrt(mean(resid(linear_model_africa)^2)) %>%
  round(digits = 4)

### Plot
new_color <- c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#6a3d9a")

scatter_plot <- plot_ly(data_africa, x = ~data_africa$Data.X.Average,  y = ~data_africa$Data.Y.Average, text = ~paste("Country: ", data_africa$Country), yaxis = list(range = c(0, 1)), mode = "markers", type = "scatter", name = "Africa", colors = new_color[1]) %>%
  add_lines(x = ~data_africa$Data.X.Average, y = ~predicted_values_africa, line = list(color = new_color[1]),  name = ~paste("RMSE linear regression<br>for Africa: ", rmse_africa)) %>%   # name = "Linear regression, Africa", hoverinfo = "text",  text = ~paste("RMSE: ", data_model_africa$Country, " (Africa)")      text = rep(paste("RMSE: ", rmse_africa), length(predicted_values_africa))) %>%
  add_trace(x = ~data_americas$Data.X.Average,  y = ~data_americas$Data.Y.Average, text = ~paste("Country: ", data_americas$Country), mode = "markers", type = "scatter", name = "Americas", colors = new_color[2]) %>%
  add_trace(x = ~data_asia$Data.X.Average,  y = ~data_asia$Data.Y.Average, text = ~paste("Country: ", data_asia$Country), mode = "markers", type = "scatter", name = "Asia", colors = new_color[3]) %>%
  add_trace(x = ~data_europe$Data.X.Average,  y = ~data_europe$Data.Y.Average, text = ~paste("Country: ", data_europe$Country), mode = "markers", type = "scatter", name = "Europe", colors = new_color[4]) %>%
  add_trace(x = ~data_oceania$Data.X.Average,  y = ~data_oceania$Data.Y.Average, text = ~paste("Country: ", data_oceania$Country), mode = "markers", type = "scatter", name = "Oceania", colors = new_color[5])%>%
  layout(title = title_label, xaxis = list(title = x_label), yaxis = list(title = y_label, range = c(0, 1)))

scatter_plot 