renv::install()
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)

#__________________________________
# World plot Disease
#__________________________________


library(tidyverse)
library(maps)
library(RColorBrewer)
library(countrycode)
library(gganimate)
library(shiny)

# Read the CSV file and add Continent information
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE) %>%
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
data <- data %>% filter(Year != 2015)

# Durchschnitt für jede Variable nach Land und Jahr
average_data <- data %>%
  group_by(Country, Year) %>%
  summarize_all(mean, na.rm = TRUE)

# Funktion zum Hinzufügen der Option "Durchschnitt" zum Jahr-Dropdown-Menü
add_average_option <- function(years) {
  c(unique(years), "Durchschnitt")
}

ui <- fluidPage(
  # Dropdown menu for selecting variable
  selectInput("variable", "Select Variable", choices = c("Measles", "Hepatitis.B", "Polio","HIV.AIDS","Diphtheria")),
  # Dropdown menu for selecting year
  selectInput("year", "Select Year", choices = add_average_option(unique(data$Year))),
  # Plot output
  plotOutput("world_map")
)

server <- function(input, output) {
  output$world_map <- renderPlot({
    # Überprüfen, ob "Durchschnitt" ausgewählt ist
    if (input$year == "Durchschnitt") {
      filtered_data <- average_data
    } else {
      filtered_data <- data %>% 
        filter(Year == as.numeric(input$year))
    }
    
    country_data <- data.frame(
      country = filtered_data$Country,
      variable = filtered_data[, input$variable]
    )
    
    world_data <- map_data("world")
    
    # Unterdrücken der Warnung für viele-zu-viele-Beziehung
    merged_data <- suppressWarnings(left_join(world_data, country_data, by = c("region" = "country")))
    
    # Anzahl der gewünschten Farbstufen
    num_colors <- 5
    
    # Erstellen von gleichmäßig verteilten Breaks
    breaks <- seq(0, max(filtered_data[, input$variable], na.rm = TRUE), length.out = num_colors + 1)
    
    # Adjust color palette accordingly (make sure to have num_colors + 1 colors)
    color_palette <- brewer.pal(length(breaks) - 1, "Blues")
    
    # Plot the world map with adjusted breaks and color palette
    p <- ggplot(merged_data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = cut(variable, breaks = breaks, dig.lab = 5), text = paste("Country: ", region, "\n", input$variable, ": ", variable)), color = "gray40") +
      scale_fill_manual(values = color_palette, na.value = "grey50") +
      labs(title = paste("World Map for", input$variable, if (input$year == "Durchschnitt") " (Durchschnitt)" else "", if (input$year != "Durchschnitt") paste("in", input$year)), fill = input$variable) +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    # Display the plot
    print(p)
  })
}

shinyApp(ui, server)
