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

averages_hepatitis <- data.frame(Country = character(), Average = numeric(), stringsAsFactors = FALSE)
averages_under_five_deaths <- data.frame(Country = character(), Average = numeric(), stringsAsFactors = FALSE)

# Get unique countries in the data frame
unique_countries <- unique(data$Country)

# Iterate through each country
for (country in unique_countries) {
  # Subset data for the current country
  country_data <- data[data$Country == country, ]
  
  # Calculate the average for the current country
  hepatitis_average <- mean(country_data$Hepatitis.B)
  under_five_death_average <- mean(country_data$under.five.deaths)
  
  
  # Append the result to the averages data frame
  averages_hepatitis <- rbind(averages_hepatitis, data.frame(Country = country, "Hepatitis B Average" = hepatitis_average))
  averages_under_five_deaths <- rbind(averages_under_five_deaths, data.frame(Country = country, "Under Five Deaths Average" = under_five_death_average))
}

data <- left_join(data, averages_hepatitis, by = "Country")
data <- left_join(data, averages_under_five_deaths, by = "Country")

# Scatterplot of Hepatitis.B vs. Death under age of 5
data_europe <- data |> filter(Continent == "Europe")
data_asia   <- data |> filter(Continent == "Asia")
data_africa <- data |> filter(Continent == "Africa")
data_americas<-data |> filter(Continent == "Americas")
data_oceania<- data |> filter(Continent == "Oceania")

new_color <- c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#6a3d9a")

data_continents <- list(africa = data_africa, americas = data_americas, asia = data_asia, europe = data_europe, oceania = data_oceania)

scatter_plot <- plot_ly(data_continents$africa, x = ~data_continents$africa$Under.Five.Deaths,  y = ~data_continents$africa$Hepatitis.B.Average, text = data_continents$africa$Country, mode = "markers", type = "scatter", name = "African countries", colors = new_color[1]) |>
  add_trace(x = ~data_continents$americas$Under.Five.Deaths,  y = ~data_continents$americas$Hepatitis.B.Average, text = data_continents$americas$Country, mode = "markers", type = "scatter", name = "American countries", colors = new_color[2]) |>
  add_trace(x = ~data_continents$asia$Under.Five.Deaths,  y = ~data_continents$asia$Hepatitis.B.Average, text = data_continents$asia$Country, mode = "markers", type = "scatter", name = "Asian countries", colors = new_color[3]) |>
  add_trace(x = ~data_continents$europe$Under.Five.Deaths,  y = ~data_continents$europe$Hepatitis.B.Average, text = data_continents$europe$Country, mode = "markers", type = "scatter", name = "European countries", colors = new_color[4]) |>
  add_trace(x = ~data_continents$oceania$Under.Five.Deaths,  y = ~data_continents$oceania$Hepatitis.B.Average, text = data_continents$oceania$Country, mode = "markers", type = "scatter", name = "Oceanian countries", colors = new_color[5])|>
  layout(title = "Child death relation towards Hepatitis B immunization coverage", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Hepatitis B immunization coverage in percent"))
htmlwidgets::saveWidget(scatter_plot, file = "plot_scatter_infant_mortality_hepatitis.html")
scatter_plot
