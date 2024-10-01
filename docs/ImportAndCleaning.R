# Data importing and cleaning

# Import libraries
library(readr)
library(dplyr)
library(ggplot2)

# Import data
flights <- read_csv("flight_dataset.csv")

# Summary
str(flights)

# Evaluate for missing data
colSums(is.na(flights))

# Check for uniqueness in categorical columns
unique(flights$Airline)
unique(flights$Source)
unique(flights$Destination)

# For plotting purposes, convert the date columns, and the number of stops column to factors
flights <- flights %>%
  mutate(Date = as.factor(Date),
         Month = as.factor(Month),
         Year = as.factor(Year),
         Total_Stops = as.factor(Total_Stops)
         )
