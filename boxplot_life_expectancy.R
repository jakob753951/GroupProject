renv::install()

################################
# Boxplot with Life Expectancy
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
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE) |>
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
data <- data |> filter(Year != 2015)

# Create ggplot
boxplot_anim <- ggplot(data, aes(x = factor(Status), y = Life.expectancy, fill = factor(Status), frame = Year)) +
  geom_boxplot(outlier.colour = "red") + #outliers in red for better visibility
  geom_jitter(color = "black", width = 0.2, shape = 1, height = 0, alpha = 0.7) + 
  transition_states(Year, transition_length = 1, state_length = 1) + # each 1 s
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out') +
  labs(
    title = "Life expectancy in developing and developed countries in the year {closest_state}",
    x = "Country Status", #Developed or developing country
    y = "Life expectancy in years"
  ) +
  theme(legend.position = "none") # no legend


# Create animation
my_animation <- animate(boxplot_anim, nframes = 900, fps = 30)
print(my_animation)
anim_save("site/img/animated_boxplot_life_expectation.gif", my_animation)