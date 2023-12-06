renv::install()
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)

#__________________________________________
# Hepatithis vs. Alcohol
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
  size = ~Life.expectancy, color = ~Continent,
  colors = new_color,
  text = ~paste("Country: ", Country, ", Life.expectancy: ", Life.expectancy),
  frame = ~Year,
  marker = list(sizemode = "diameter", size = (~Life.expectancy*0.2), line = list(width = 0.1)),  # Adjust the line width
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    xaxis = list(type = "log", title = "Alcohol"),
    yaxis = list(title = "Hepatitis"),
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


