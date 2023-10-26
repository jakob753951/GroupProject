install.packages("tidyverse")
install.packages("gganimate")
install.packages("gifski")
install.packages("transformr")
library(dplyr)
data <- read.csv("Life_Expectancy_Data.csv", header = TRUE)

data |> select(Country)

