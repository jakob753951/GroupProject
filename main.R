renv::install()
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)

#__________________________________________
# Life Expectancy vs. GDP per Capita
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

# Preprocess population data
# India
data <- data %>% 
  mutate(Population = ifelse(Country == "India" & (Year == 2004 | Year == 2005 | Year == 2006 | Year == 2007 | Year == 2014), Population / 10, Population))
data <- data %>% 
  mutate(Population = ifelse(Country == "India" & Year == 2010, Population * 10, Population))
data <- data %>% 
  mutate(Population = ifelse(Country == "India", Population * 10, Population))
# Indonesia:
data <- data %>% 
  mutate(Population = ifelse(Country == "Indonesia" & (Year == 2000 | Year == 2003 | Year == 2005 | Year == 2006 | Year == 2009 | Year == 2011 | Year == 2013), Population * 10, Population))
data <- data %>% 
  mutate(Population = ifelse(Country == "Indonesia" & (Year == 2001 | Year == 2002), Population * 100, Population))

# Define new colors
new_color <- c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#6a3d9a")

# Plotly Code
p_animate_plotly <- plot_ly(
  data,
  x = ~GDP, y = ~Life.expectancy,
  size = ~Population, color = ~Continent,
  colors = new_color,
  text = ~paste("Country: ", Country, ", Population: ", Population),
  frame = ~Year,
  marker = list(sizemode = "diameter", line = list(width = 1)),
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    xaxis = list(type = "log", title = "GDP per Capita"),
    yaxis = list(title = "Life Expectancy"),
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
  animation_opts(frame = 3000, redraw = TRUE, transition = 1000)

# Save or Display
# Save Plotly object to an HTML file
htmlwidgets::saveWidget(p_animate_plotly, file = "interactive_plot-Lifeexp.html")
# Or display in the viewer
p_animate_plotly
