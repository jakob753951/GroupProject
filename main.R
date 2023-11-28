renv::install()
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)

################################
################################

# Boxplot with Life exp.

################################
################################

# Load packages
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gganimate")) install.packages("gganimate")
if (!require("dplyr")) install.packages("dplyr")
if (!require("countrycode")) install.packages("countrycode")
if (!require("plotly")) install.packages("plotly")

library(ggplot2)
library(gganimate)
library(dplyr)
library(countrycode)
library(plotly)

# Read the CSV file and add continent information
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE) %>%
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
data <- data %>% filter(Year != 2015)

# Create ggplot
my_anim <- ggplot(data, aes(x = factor(Status), y = Life.expectancy, fill = factor(Year), frame = Year)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5, color = "black") +
  transition_states(Year, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

# Create animation
my_animation <- animate(my_anim, nframes = 50, fps = 1)

# Convert gganimate object to Plotly object
my_plotly_anim <- ggplotly(ggplot2::last_plot(), tooltip = c("ymin", "lower", "middle", "upper", "ymax"))

# Adjust layout to remove legend
my_plotly_anim <- my_plotly_anim %>%
  layout(showlegend = FALSE)

# Print interactive plot
print(my_plotly_anim)
