renv::install()

#__________________________________
# World plot Disease
#__________________________________

# Install shiny if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(dplyr)
library(countrycode)
library(tidyverse)
library(maps)
library(RColorBrewer)
library(shiny)
library(gganimate)

# Set options for displaying numbers
options(scipen = 999)

# Read the CSV file and add Continent information
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE) |>
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
data <- data |> filter(Year != 2015)

# Identify variables causing warnings
# warnings <- last_dplyr_warnings()
# print(warnings)

# Check the structure of your data to identify non-numeric or non-boolean variables
# str(data)

# Fix the summarise_all() function to exclude non-numeric variables
numeric_data <- data |>
  select(-c("Country", "Continent", "Status")) |>
  select_if(is.numeric)

average_data <- data |>
  group_by(Country, Continent) |>
  summarize(across(everything(), ~mean(., na.rm = TRUE)))

# Combine the data frames with matching data types for "Year"
data_with_average <- average_data |>
  mutate(Year = "Average") |>
  bind_rows(data |> select(-Status) |> mutate(Year = as.character(Year)))

# Check the structure of the combined data frame
#str(data_with_average)
data <- data_with_average

# UI-Definition
ui <- fluidPage(
  titlePanel("World Map"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for Year
      selectInput("year", "Select Year", choices = unique(data$Year)),
      # Dropdown menu for Variable
      selectInput("variable", "Select Variable", choices = c("Measles", "Hepatitis.B", "Polio", "HIV.AIDS", "Diphtheria"))
    ),
    mainPanel(
      # World Map Output
      plotOutput("world_map")
    )
  )
)

# Server Definition
server <- function(input, output) {
  options(scipen = 999)
  options(scales.GLOBAL = list(scientific = FALSE))
  # Filter function for selected options
  filtered_data <- reactive({
    filter(data, Year == input$year)
  })
  
  # Generate World Map
  output$world_map <- renderPlot({
    newdata <- filtered_data()
    country_data <- data.frame(
      country = newdata$Country,
      value = newdata[[input$variable]]
    )
    
    world_data <- map_data("world")
    merged_data <- left_join(world_data, country_data, by = c("region" = "country"))
    
    num_classes <- 9
    color_intervals <- seq(0, max(merged_data$value, na.rm = TRUE), length.out = num_classes)
    color_palette <- brewer.pal(num_classes, "Blues")
    
    if(input$variable == "Measles") {
      world_map <- ggplot(merged_data) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = cut(value, breaks = color_intervals)), color = "gray40") +
        scale_fill_manual(values = color_palette) +
        theme_minimal() +
        theme(legend.position = "bottom",
              axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = paste("World Map for", input$variable, "immunization coverage (percent) in", input$year), fill = input$variable)
      print(world_map)
    }
    
    if(input$variable == "Hepatitis.B") {
      world_map <- ggplot(merged_data) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = cut(value, breaks = color_intervals)), color = "gray40") +
        scale_fill_manual(values = color_palette) +
        theme_minimal() +
        theme(legend.position = "bottom",
              axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = paste("World Map for Hepatitis B immunization coverage (percent) in", input$year), fill = input$variable)
      print(world_map)
    }
    
    if(input$variable == "Polio") {
      world_map <- ggplot(merged_data) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = cut(value, breaks = color_intervals)), color = "gray40") +
        scale_fill_manual(values = color_palette) +
        theme_minimal() +
        theme(legend.position = "bottom",
              axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = paste("World Map for", input$variable, "immunization coverage (percent) in", input$year), fill = input$variable)
      print(world_map)
    }
    
    if(input$variable == "HIV.AIDS") {
      world_map <- ggplot(merged_data) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = cut(value, breaks = color_intervals)), color = "gray40") +
        scale_fill_manual(values = color_palette) +
        theme_minimal() +
        theme(legend.position = "bottom",
              axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = paste("World Map for HIV/AIDS prevalence in", input$year), fill = input$variable)
      print(world_map)
    }
    
    if(input$variable == "Diphtheria") {
      world_map <- ggplot(merged_data) +
        geom_polygon(aes(x = long, y = lat, group = group, fill = cut(value, breaks = color_intervals)), color = "gray40") +
        scale_fill_manual(values = color_palette) +
        theme_minimal() +
        theme(legend.position = "bottom",
              axis.title = element_blank(),
              axis.text = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(title = paste("World Map for", input$variable, "immunization coverage (percent) in", input$year), fill = input$variable)
      print(world_map)
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)