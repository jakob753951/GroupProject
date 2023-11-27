renv::install()
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)


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

# Create ggplot
p_animate <- ggplot(
  data,
  aes(x = Hepatitis.B, y = Alcohol, size = Life.expectancy, colour = Continent, label = Country)
) +
  geom_point(show.legend = TRUE, alpha = 0.4) +  
  scale_color_manual(values = c("Asia" = "red", "Europe" = "blue", "Africa" = "green", "Americas" = "yellow", "Oceania" = "black"),
                     name = "Continent") +  
  scale_size_continuous(range = c(2, 10), name = "Population") +  
  scale_x_log10() +  
  labs(x = "Alcohol consumption", y = "Hepatitis B")+  
  transition_states(Life.expectancy) +
  enter_fade() +
  ease_aes('linear', nframes = 700) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.size = unit(0.2, "cm")
  ) +  
  guides(
    size = guide_legend(title.position = "top", title.vjust = 0.3),
    color = guide_legend(title.position = "top", title.vjust = 0.3)
  ) +
  ggtitle("Year: {frame_time}") +
  shadow_mark(alpha = 0.6, size = 0.4) +
  enter_fade() +
  exit_fade() +
  geom_text_repel(aes(label = ifelse(frame_time == max(frame_time), as.character(Country), "")))

library(plotly)

# Prepare data
# data <- read.csv("Life_Expectancy_Data.csv", header = TRUE) %>%
#  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Create Plotly graph
p_animate_plotly <- plot_ly(
  data,
  x = ~Alcohol, y = ~Hepatitis.B,
  size = ~Life.expectancy, color = ~Continent,
  text = ~paste("Country: ", Country, ", Population: ", Population),
  frame = ~Year,
  marker = list(sizemode = "diameter", line = list(width = 1)),  # Adjust the line width
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    xaxis = list(type = "log", title = "Alcohol consumption per capita [liters of pure alcohol]"),
    yaxis = list(title = "Hepatitis B"),
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
  animation_opts(frame = 1500, redraw = TRUE)  # Adjust the frame duration (in milliseconds)

# Save or display
# save the plotly object to an HTML file
htmlwidgets::saveWidget(p_animate_plotly, file = "interactive_plot-Lifeexp.html")
# or show it in the viewer
p_animate_plotly
