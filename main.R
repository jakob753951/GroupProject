renv::install()
#__________________________________________
# Hepatitis vs. Alcohol
#__________________________________________

library(ggplot2)
library(gganimate)
library(dplyr)
library(countrycode)
library(plotly)
library(ggrepel)

# Read the CSV file and add Continent information
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE) %>%
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
data <- data %>% filter(Year != 2015)

# Define new colors
new_color <- c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#6a3d9a")

# Plotly Code
p_animate_plotly <- plot_ly(
  data,
  x = ~Alcohol, y = ~Hepatitis.B,
  size = ~Life.expectancy,
  color = ~Continent,
  colors = new_color,
  text = ~paste("Country: ", Country, ", Life expectancy: ", Life.expectancy),
  frame = ~Year,
  type = "scatter",
  mode = "markers",
  ids = ~Country #important for bubbles not to change which country they depict during animation
) %>%
  layout(
    xaxis = list(type = "log", title = "Alcohol consumption per capita (in liters of pure alcohol)"),
    yaxis = list(title = "Hepatitis B immunization coverage (in percent)"),
    showlegend = TRUE,
    legend = list(
      x = 1,
      y = 1,
      traceorder = 'normal',
      font = list(
        family = 'sans-serif',
        size = 12,
        color = '#000'
      ),
      bgcolor = '#E2E2E2',
      bordercolor = '#FFFFFF',
      borderwidth = 2
    )
  ) %>%
  animation_opts(frame = 3000, redraw = TRUE, transition = 1000)  # Adjust the frame duration (in milliseconds)

# Save or display
# save the plotly object to an HTML file
htmlwidgets::saveWidget(p_animate_plotly, file = "interactive_plot-Alcohol.html")
# or show it in the viewer
p_animate_plotly

