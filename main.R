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
  #average <- rbind(hepatitis_average, under_five_death_average)
  
  # Append the result to the averages data frame
  averages_hepatitis <- rbind(averages_hepatitis, data.frame(Country = country, "Hepatitis B Average" = hepatitis_average))
  averages_under_five_deaths <- rbind(averages_under_five_deaths, data.frame(Country = country, "Under Five Deaths Average" = under_five_death_average))
}

data <- left_join(data, averages_hepatitis, by = "Country")
data <- left_join(data, averages_under_five_deaths, by = "Country")

# Scatterplot of Hepatitis.B vs. Death under age of 5
data_europe <- data %>% filter(Continent == "Europe")
data_asia   <- data %>% filter(Continent == "Asia")
data_africa <- data %>% filter(Continent == "Africa")
data_americas<-data %>% filter(Continent == "Americas")
data_oceania<- data %>% filter(Continent == "Oceania")

### Fit a linear regression model
# Europe
data_model_europe   <- subset(data_europe, Country != "Russian Federation")
linear_model_europe <- lm(data_model_europe$Hepatitis.B.Average ~ data_model_europe$Under.Five.Deaths.Average, data = data_model_europe)
predicted_values_europe <- predict(linear_model_europe)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_europe[predicted_values_europe < 0] <- 0
rmse_europe <- sqrt(mean(resid(linear_model_europe)^2))

scatter_plot_europe <- plot_ly(data_model_europe, x = ~data_model_europe$Under.Five.Deaths.Average, y = ~data_model_europe$Hepatitis.B.Average, text = data_model_europe$Country, mode = "markers", type = "scatter", name = "European countries") %>%  #, showlegend = TRUE
  layout(title = "Interactive Scatter Plot", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Hepatitis B Cases")) %>%
  add_lines(x = ~data_model_europe$Under.Five.Deaths.Average, y = ~predicted_values_europe, line = list(color = 'red'), name = "Linear regression", hoverinfo = "text", text = rep(paste("RMSE: ", rmse_europe), length(predicted_values_europe)))
scatter_plot_europe

# Asia
data_model_asia   <- subset(data_asia)#, Country != "Russian Federation")
linear_model_asia <- lm(data_model_asia$Hepatitis.B.Average ~ data_model_asia$Under.Five.Deaths.Average, data = data_model_asia)
predicted_values_asia <- predict(linear_model_asia)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_asia[predicted_values_asia < 0] <- 0
rmse_asia <- sqrt(mean(resid(linear_model_asia)^2))

# Africa
data_model_africa   <- subset(data_africa) #, Country) # != "Russian Federation")
linear_model_africa <- lm(data_model_africa$Hepatitis.B.Average ~ data_model_africa$Under.Five.Deaths.Average, data = data_model_africa)
predicted_values_africa <- predict(linear_model_africa)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_africa[predicted_values_africa < 0] <- 0
rmse_africa <- sqrt(mean(resid(linear_model_africa)^2))

# Americas
data_model_americas   <- subset(data_americas)
linear_model_america <- lm(data_model_americas$Hepatitis.B.Average ~ data_model_americas$Under.Five.Deaths.Average, data = data_model_americas)
predicted_values_americas <- predict(linear_model_america)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_americas[predicted_values_americas < 0] <- 0
rmse_america <- sqrt(mean(resid(linear_model_america)^2))

# Oceania
data_model_oceania   <- subset(data_oceania)
linear_model_oceania <- lm(data_model_oceania$Hepatitis.B.Average ~ data_model_oceania$Under.Five.Deaths.Average, data = data_model_oceania)
predicted_values_oceania <- predict(linear_model_oceania)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
predicted_values_oceania[predicted_values_oceania < 0] <- 0
rmse_oceania <- sqrt(mean(resid(linear_model_oceania)^2))

###############################################################################
# Combine Subplots in one plot

new_color <- c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#6a3d9a")

data_models <- list(africa = data_model_africa, americas = data_model_americas, asia = data_model_asia, europe = data_model_europe, oceania = data_model_oceania)

scatter_plot <- plot_ly(data_models$africa, x = ~data_models$africa$Under.Five.Deaths,  y = ~data_models$africa$Hepatitis.B.Average, text = data_models$africa$Country, mode = "markers", type = "scatter", name = "African countries", colors = new_color[1]) %>%
  add_trace(x = ~data_models$americas$Under.Five.Deaths,  y = ~data_models$americas$Hepatitis.B.Average, text = data_models$americas$Country, mode = "markers", type = "scatter", name = "American countries", colors = new_color[2]) %>%
  add_trace(x = ~data_models$asia$Under.Five.Deaths,  y = ~data_models$asia$Hepatitis.B.Average, text = data_models$asia$Country, mode = "markers", type = "scatter", name = "Asian countries", colors = new_color[3]) %>%
  add_trace(x = ~data_models$europe$Under.Five.Deaths,  y = ~data_models$europe$Hepatitis.B.Average, text = data_models$europe$Country, mode = "markers", type = "scatter", name = "European countries", colors = new_color[4]) %>%
  add_trace(x = ~data_models$oceania$Under.Five.Deaths,  y = ~data_models$oceania$Hepatitis.B.Average, text = data_models$oceania$Country, mode = "markers", type = "scatter", name = "Oceanian countries", colors = new_color[5])%>%
  layout(title = "Child death relation towards Hepathitis B immunization coverage", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Hepatitis B immunization coverage in percent"))
scatter_plot

  
#linear_model <- lm(data_models$Hepatitis.B.Average ~ data_models$Under.Five.Deaths.Average, data = data_models)
#predicted_values <- predict(linear_models)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
#predicted_values[predicted_values < 0] <- 0
#rmse_prediction <- sqrt(mean(resid(linear_model)^2))

###############################################################################
# DO NOT PLOT THIS, THIS IS JUST FOR CHECKING THE QUALITY OF THE LINEAR REGRESSION
## Checking the grade of the linear regression
# Europe
# Residual Analysis: should be close to zero and evenly distributed
plot(linear_model_europe, which = 1)
# Normality of Residuals (Q-Q plot): should be approximately normally distributed
plot(linear_model_europe, which = 2)

# Asia
# Residual Analysis: should be close to zero and evenly distributed
plot(linear_model_asia, which = 1)
# Normality of Residuals (Q-Q plot): should be approximately normally distributed
plot(linear_model_asia, which = 2)

# Africa
# Residual Analysis: should be close to zero and evenly distributed
plot(linear_model_africa, which = 1)
# Normality of Residuals (Q-Q plot): should be approximately normally distributed
plot(linear_model_africa, which = 2)

# Americas
# Residual Analysis: should be close to zero and evenly distributed
plot(linear_model_america, which = 1)
# Normality of Residuals (Q-Q plot): should be approximately normally distributed
plot(linear_model_america, which = 2)

# Oceania
# Residual Analysis: should be close to zero and evenly distributed
plot(linear_model_oceania, which = 1)
# Normality of Residuals (Q-Q plot): should be approximately normally distributed
plot(linear_model_oceania, which = 2)
