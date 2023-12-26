renv::install()
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)

#Variante 1

# plotting data on a world map
library(tidyverse)
library(maps)
library(RColorBrewer)

newdata <- filter(data)
newdata
country_data <- data.frame(
  country = newdata$Country,
  infantdeath = newdata$infant.deaths
)


world_data <- map_data("world")
merged_data <- left_join(world_data, country_data, by = c("region" = "country"))

# Anzahl der Klassen für die Farbpalette
num_classes <- 9  # Anpassen Sie dies entsprechend Ihrer Anzahl von Klassen

# Automatisch Werte für color_intervals erstellen
color_intervals <- seq(0, max(merged_data$infantdeath, na.rm = TRUE), length.out = num_classes)

# Farbpalette erstellen
color_palette <- brewer.pal(num_classes, "Blues")


world_map <- ggplot(merged_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = cut(infantdeath, breaks = color_intervals)), color = "gray40") +
  scale_fill_manual(values = color_palette) +
  labs(title = "World Map for infant deaths", fill = "infantdeath") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(world_map)
p_animate <- world_map + transition_time(year) +
  labs(title = "Year: {frame_time}")
#5.9
shadow_mark(alpha = 0.3, size = 0.5)


########################################################
########################################################

# Variante 2


library(tidyverse)
library(maps)
library(RColorBrewer)
library(countrycode)
library(gganimate)

# Read the CSV file and add Continent information
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE) |>
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
filtered_data <- data |> filter(data$Year != 2015)

country_data <- data.frame(
  country = filtered_data$Country,
  inflantdeath = filtered_data$infant.deaths
)

world_data <- map_data("world")
merged_data <- left_join(world_data, country_data, by = c("region" = "country"))

# Anzahl der gewünschten Farbstufen
num_colors <- 5

# Erstellen von gleichmäßig verteilten Breaks
breaks <- seq(0, 1600, length.out = num_colors + 1)

# Adjust color palette accordingly (make sure to have num_colors + 1 colors)
color_palette <- brewer.pal(length(breaks) - 1, "Greens")

# Plot the world map with adjusted breaks and color palette
world_map <- ggplot(merged_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = cut(inflantdeath, breaks = breaks, dig.lab = 5)), color = "gray40") +
  scale_fill_manual(values = color_palette, na.value = "grey50") +
  labs(title = "World Map for inflant deaths", fill = "inflant deaths") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(world_map)

p_animate <- world_map + transition_time(year) +
  labs(title = "Year: {frame_time}")

any(is.na(data$Year))  # Check for missing values
all(is.finite(data$Year))  # Check for non-finite values
any(is.na(data$infant.deaths))  # Check for missing values
all(is.finite(data$infant.deaths))  # Check for non-finite values
any(is.numeric(data$Year))
any(is.numeric(data$infant.deaths))
p_animate

#5.9
shadow_mark(alpha = 0.3, size = 0.5)


# Variante 3
install.packages("rnaturalearth")
install.packages(c("ggplot2", "sf", "gganimate"))
library(ggplot2)
library(sf)
library(gganimate)
library(dplyr)
library(countrycode)
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")

data <-  read.csv("Life_Expectancy_Data.csv", header = TRUE) |>
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
filtered_data <- data |> filter(Year != 2015)
filtered_data$Year <- as.numeric(filtered_data$Year)


country_data <- filtered_data |>
  select(Country, infant.deaths) |>
  rename(country = Country, value = infant.deaths)

your_data <- merge(world, country_data, by.x = "name", by.y = "country")


p <- ggplot(data = your_data) +
  geom_sf(data = your_data, aes(fill = value)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c() +
  labs(title = "Animated World Map", fill = "Your Legend Label")

anim <- p + transition_states(data$Year, transition_length = 2, state_length = 1) +
  enter_fade() + exit_fade()
anim

#anim_save("animated_world_map.gif", animation = anim)


# Variante 4
library(tidyverse)
library(maps)
library(RColorBrewer)
library(ggplot2)
library(sf)
library(gganimate)
library(dplyr)
library(countrycode)
library(rnaturalearth)

data <-  read.csv("Life_Expectancy_Data.csv", header = TRUE) |>
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
filtered_data <- data |> filter(Year != 2015)
filtered_data$Year <- as.numeric(filtered_data$Year)


country_data <- filtered_data |>
  select(Country, infant.deaths, Year) |>
  rename(country = Country, value = infant.deaths, year = Year)


world_data <- map_data("world")
merged_data <- left_join(world_data, country_data, by = c("region" = "country"))

# Anzahl der gewünschten Farbstufen
num_colors <- 5


# Erstellen von gleichmäßig verteilten Breaks
breaks <- seq(0, 1600, length.out = num_colors + 1)

# Adjust color palette accordingly (make sure to have num_colors + 1 colors)
color_palette <- brewer.pal(length(breaks) - 1, "Greens")
world_map <- ggplot(merged_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = cut(value, breaks = breaks)), color = "gray40") +
  scale_fill_manual(values = color_palette) +
  labs(title = "World Map of Infant Deaths", fill = merged_data$value) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.position = "bottom",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

print(world_map)

anim <- world_map + transition_states(merged_data$year, transition_length = 2, state_length = 1) +
  enter_fade() + exit_fade()
anim