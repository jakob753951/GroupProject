renv::install()
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)

# Boxplot with Alcohol

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
boxplot_anim <- ggplot(data, aes(x = factor(Status), y = Alcohol, fill = factor(Status), frame = Year)) +
  geom_boxplot(outlier.colour = "red") +
  geom_jitter(color = "black", width = 0.2, shape = 1, height = 0, alpha = 0.7) + #factor(Status)
  transition_states(Year, transition_length = 10, state_length = 30) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out') +
  labs(
    title = "Alcohol consumption in developing and developed countries",
    x = "Country Status",    # Specify your custom x-axis label here
    y = "Alcohol consumption per capita (liters of pure alcohol)"     # Specify your custom y-axis label here
  ) +
  theme(legend.position = "none") # no legend    


# Create animation
my_animation <- animate(boxplot_anim, nframes = 15, fps = 1)
print(my_animation)
