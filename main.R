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

# Get unique countries in the data frame
unique_countries <- unique(data$Country)

# Iterate through each country
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

# Scatterplot of Hepatitis.B vs. Death under age of 5
data_europe <- data %>% filter(Continent == "Europe")
data_asia   <- data %>% filter(Continent == "Asia")
data_africa <- data %>% filter(Continent == "Africa")
data_americas<-data %>% filter(Continent == "Americas")
data_oceania<- data %>% filter(Continent == "Oceania")

### Fit a linear regression model
# Europe
data_model_europe   <- subset(data_europe, Country != "Russian Federation")
#linear_model_europe <- lm(data_model_europe$Measles ~ data_model_europe$Under.Five.Deaths.Average, data = data_model_europe)
#predicted_values_europe <- predict(linear_model_europe)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
#predicted_values_europe[predicted_values_europe < 0] <- 0
#rmse_europe <- sqrt(mean(resid(linear_model_europe)^2))

#scatter_plot_europe <- plot_ly(data_model_europe, x = ~data_model_europe$Under.Five.Deaths.Average, y = ~data_model_europe$Measles, text = data_model_europe$Country, mode = "markers", type = "scatter", name = "European countries") %>%  #, showlegend = TRUE
 # layout(title = "Interactive Scatter Plot", xaxis = list(title = "Death under age of 5"), yaxis = list(title = "Hepatitis B Cases")) %>%
#  add_lines(x = ~data_model_europe$Under.Five.Deaths.Average, y = ~predicted_values_europe, line = list(color = 'red'), name = "Linear regression", hoverinfo = "text", text = rep(paste("RMSE: ", rmse_europe), length(predicted_values_europe)))
#scatter_plot_europe

# Asia
data_model_asia   <- subset(data_asia)#, Country != "Russian Federation")
#linear_model_asia <- lm(data_model_asia$Hepatitis.B.Average ~ data_model_asia$Under.Five.Deaths.Average, data = data_model_asia)
#predicted_values_asia <- predict(linear_model_asia)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
#predicted_values_asia[predicted_values_asia < 0] <- 0
#rmse_asia <- sqrt(mean(resid(linear_model_asia)^2))

# Africa
data_model_africa   <- subset(data_africa) #, Country) # != "Russian Federation")
linear_model_africa <- lm(data_model_africa$Data.Y.Average ~ data_model_africa$Data.X.Average, data = data_model_africa)
predicted_values_africa <- predict(linear_model_africa)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
#predicted_values_africa[predicted_values_africa < 0] <- 0
rmse_africa <- sqrt(mean(resid(linear_model_africa)^2)) %>%
  round(digits = 4)
# Americas
data_model_americas   <- subset(data_americas)
#linear_model_america <- lm(data_model_americas$Hepatitis.B.Average ~ data_model_americas$Under.Five.Deaths.Average, data = data_model_americas)
#predicted_values_americas <- predict(linear_model_america)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
#predicted_values_americas[predicted_values_americas < 0] <- 0
#rmse_america <- sqrt(mean(resid(linear_model_america)^2))

# Oceania
data_model_oceania   <- subset(data_oceania)
#linear_model_oceania <- lm(data_model_oceania$Hepatitis.B.Average ~ data_model_oceania$Under.Five.Deaths.Average, data = data_model_oceania)
#predicted_values_oceania <- predict(linear_model_oceania)
# Set values below 0 to 0 --> still some error here: truncates values but now there is a bend in the line which should be straight
#predicted_values_oceania[predicted_values_oceania < 0] <- 0
#rmse_oceania <- sqrt(mean(resid(linear_model_oceania)^2))

###############################################################################
# Combine Subplots in one plot

new_color <- c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#6a3d9a")

scatter_plot <- plot_ly(data_model_africa, x = ~data_model_africa$Data.X.Average,  y = ~data_model_africa$Data.Y.Average, text = ~paste("Country: ", data_model_africa$Country), yaxis = list(range = c(0, 1)), mode = "markers", type = "scatter", name = "Africa", colors = new_color[1]) %>%
  add_lines(x = ~data_model_africa$Data.X.Average, y = ~predicted_values_africa, line = list(color = new_color[1]),  name = ~paste("Linear regression for<br>Africa: ", rmse_africa)) %>%   # name = "Linear regression, Africa", hoverinfo = "text",  text = ~paste("RMSE: ", data_model_africa$Country, " (Africa)")      text = rep(paste("RMSE: ", rmse_africa), length(predicted_values_africa))) %>%
  add_trace(x = ~data_model_americas$Data.X.Average,  y = ~data_model_americas$Data.Y.Average, text = ~paste("Country: ", data_model_americas$Country), mode = "markers", type = "scatter", name = "Americas", colors = new_color[2]) %>%
  add_trace(x = ~data_model_asia$Data.X.Average,  y = ~data_model_asia$Data.Y.Average, text = ~paste("Country: ", data_model_asia$Country), mode = "markers", type = "scatter", name = "Asia", colors = new_color[3]) %>%
  add_trace(x = ~data_model_europe$Data.X.Average,  y = ~data_model_europe$Data.Y.Average, text = ~paste("Country: ", data_model_europe$Country), mode = "markers", type = "scatter", name = "Europe", colors = new_color[4]) %>%
  add_trace(x = ~data_model_oceania$Data.X.Average,  y = ~data_model_oceania$Data.Y.Average, text = ~paste("Country: ", data_model_oceania$Country), mode = "markers", type = "scatter", name = "Oceania", colors = new_color[5])%>%
  layout(title = title_label, xaxis = list(title = x_label), yaxis = list(title = y_label, range = c(0, 1)))
scatter_plot


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
