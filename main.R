renv::install()

data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)

#_____________________________________
# Interactive barchart  BMI and Adult Mortality
#_____________________________________

# Install packages if not installed
# install.packages("plotly")


# Load the required packages
library(ggplot2)
library(shiny)
library(countrycode)
library(dplyr)
library(plotly)

# Read the CSV file and add continent information
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE) %>%
  mutate(Continent = countrycode(Country, "country.name", "continent"))

# Exclude data from the year 2015
data <- data %>% filter(Year != 2015)

# UI part of the Shiny app
ui <- fluidPage(
  titlePanel("BMI and Adult Mortality by Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select Continent", choices = unique(data$Continent)),
      selectInput("year", "Select Year", choices = unique(data$Year), selected = min(data$Year))
    ),
    mainPanel(
      plotlyOutput("barchart")
    )
  )
)

# Server part of the Shiny app
server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(Continent == input$continent, Year == input$year)
  })
  
  output$barchart <- renderPlotly({
    
    p <- ggplot(filtered_data(), aes(x = reorder(Country, -BMI))) +
      geom_bar(aes(y = BMI + Adult.Mortality, fill = "BMI", text = BMI), stat = "identity", position = "stack", alpha = 0.7) +
      geom_bar(aes(y = Adult.Mortality, fill = "Adult Mortality", text = Adult.Mortality), stat = "identity", position = "stack", alpha = 0.7) +
      labs(title = paste("Adult Mortality and BMI by Country -", input$continent, "-", input$year, "<br><span style='font-size: 60%'>Adult Mortality: Probability of dying between 15 and 60 years per 1000 population; BMI: absolute values</span><br>"),
           y = "Values",
           x = "Country",
           fill = "Legend") +
      scale_y_continuous(
        name = "Adult Mortality / BMI",
        sec.axis = sec_axis(~., name = "BMI")
      ) +
      scale_fill_manual(values = c("#e31a1c","#3182bd"), name = "Legend") +
      theme_minimal() +
      theme(legend.position = "top", legend.box.background = element_rect(color = "black", size = 0.5),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    
    p <- ggplotly(p, tooltip = "text")  # Add interactivity with plotly
    p
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
