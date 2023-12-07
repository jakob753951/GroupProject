renv::install()
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |>
  group_by(Country) |>
  slice_min(Year) |>
  select(Life.expectancy)


#_____________________________________
# Barchart  BMI and Adult Mortality
#_____________________________________


# Install ggplot2 and shiny if not installed
# install.packages(c("ggplot2", "shiny", "countrycode", "dplyr"))

# Load the required packages
library(ggplot2)
library(shiny)
library(countrycode)
library(dplyr)

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
      sliderInput("year", "Select Year", min = min(data$Year), max = max(data$Year), value = min(data$Year), step = 1)
    ),
    mainPanel(
      plotOutput("barchart")
    )
  )
)

# Server part of the Shiny app
server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(Continent == input$continent, Year == input$year)
  })
  
  output$barchart <- renderPlot({
    ggplot(filtered_data(), aes(x = Country)) +
      geom_bar(aes(y = BMI), stat = "identity", position = "dodge", fill = "blue", alpha = 0.7) +
      geom_bar(aes(y = Adult.Mortality * 10), stat = "identity", position = "dodge", fill = "red", alpha = 0.7) +
      labs(title = paste("BMI and Adult Mortality by Country -", input$continent, "-", input$year), y = "Values") +
      scale_y_continuous(
        name = "BMI",
        sec.axis = sec_axis(~./10, name = "Adult Mortality")
      ) +
      theme_minimal()
  })
}

# Run the Shiny app
shinyApp(ui, server)
