# Exploratory Data Analysis

# Import libraries
library(dplyr)
library(ggplot2)
library(car)
library(stringr)
# Purpose is to identify candidate features for the regression model

# Determine how the month affects the price
prices_by_month <- flights %>% 
  group_by(Month) %>% 
  summarize(mean_price = mean(Price), std_price = sd(Price))
prices_by_month

# Plot the differences by month
ggplot(flights, aes(x = Month, y = Price)) +
  geom_boxplot() +
  ggtitle("Boxplot of flight fares by month")

# Plot the mean and standard deviation by month to get a slightly different view
ggplot(prices_by_month, aes(x = Month, y = mean_price)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_price - std_price, ymax = mean_price + std_price),
                width = 0.5) +
  ggtitle("Mean flight fares by month")

# **The outliers should be removed to ensure best statistics and modeling
IQR_Months <- flights %>% 
  group_by(Month) %>% 
  summarize(Q1 = quantile(Price, probs = 0.25),
            Q3 = quantile(Price, probs = 0.75),
            IQR = IQR(Price)) %>%
  mutate(min = Q1 - 1.5*IQR,
         max = Q3 + 1.5*IQR)
max_month3 <- as.numeric(IQR_Months[1, 6])
max_month4 <- as.numeric(IQR_Months[2, 6])
max_month5 <- as.numeric(IQR_Months[3, 6])
max_month6 <- as.numeric(IQR_Months[4, 6])

flightsByMonth_filtered <- flights %>%
  filter(!(Month == 3 & (Price > max_month3))) %>%
  filter(!(Month == 4 & (Price > max_month4))) %>%
  filter(!(Month == 5 & (Price > max_month5))) %>%
  filter(!(Month == 6 & (Price > max_month6)))

# Re-calculate and re-plot
pricesByMonth_wo_outliers <- flightsByMonth_filtered %>% 
  group_by(Month) %>% 
  summarize(mean_price = mean(Price), std_price = sd(Price))
pricesByMonth_wo_outliers

ggplot(flightsByMonth_filtered, aes(x = Month, y = Price)) +
  geom_boxplot() +
  ggtitle("Boxplot of flight fares by month with original outliers removed")

ggplot(pricesByMonth_wo_outliers, aes(x = Month, y = mean_price)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_price - std_price, ymax = mean_price + std_price),
                width = 0.5) +
  ggtitle("Mean flight fares by month with original outliers removed")

# Perform an ANOVA analysis to see if there is a significant difference. This test assumes
# that the sample data is a random subset of a larger population, that each observation is independent,
# and that the samples follow a normal distribution.
# The count of each sample will be measured to determine if each sample size is large enough for ANOVA.
ggplot(flightsByMonth_filtered, aes(x = Price, fill = Month)) +
  geom_histogram(binwidth = 2000, alpha = 0.5)

flightsByMonth_filtered %>%
  count(Month)
# The assumptions hold
mdl_price_by_month <- lm(Price ~ Month, data = flightsByMonth_filtered)
anova(mdl_price_by_month)

# Post-hoc testing
pairwise.t.test(
  flightsByMonth_filtered$Price,
  flightsByMonth_filtered$Month,
  p.adjust.method = 'bonferroni'
)

# This shows the month of the year has an impact on fares

# Does the time of the month matter?
priceByTimeOfMonth <- flights %>%
  group_by(Month, Date) %>%
  summarize(mean_price = mean(Price),
            std_price = sd(Price))

ggplot(flights, aes(x = Date, y = Price)) +
  stat_summary(fun.y = mean,
               geom = "line",
               size = 0.8,
               aes(group = 1, color = Month)) +
  stat_summary(fun.y = mean,
               geom = "point") +
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar",
               width = 0.2,
               fun.args = list(mult = 1)) +
  facet_grid(Month ~ .) +
  theme(legend.position = "none") +
  ggtitle("Price points over the Dates of each Month")

# Shows a consistent trend line that corroborates with the month statistics. No cyclical nature seen.

# Check prices of airlines
ggplot(flights, aes(y = Airline, x = Price)) +
  geom_boxplot()

# Shows a clear difference in price depending on the flight airline. How many are in each category
flights %>% count(Airline)

# The outliers of this dataset lie within Jet Airways Business, thus, the airline feature must
# be included in the model.

# Check the prices of the source
ggplot(flights, aes(y = Source, x = Price)) +
  geom_boxplot()

# Check the price of the destination
ggplot(flights, aes(y = Destination, x = Price)) +
  geom_boxplot()

# Seems like there is an interaction with source and destination.
# Would have a better understanding if the entire route was analyzed instead.

flightsWithRoutes <- flights %>%
  mutate(Route = str_c(Source, "to", Destination, sep = " "))

ggplot(flightsWithRoutes, aes(y = Route, x = Price)) +
  geom_boxplot() +
  ggtitle("Boxplot of flight route and respective prices")

# Examine how this plays out with the different airlines
ggplot(flightsWithRoutes, aes(y = Route, x = Price)) +
  geom_boxplot() +
  geom_point(aes(color = Airline),
             position = position_dodge(width = 0.3),
             alpha = 0.5) +
  ggtitle("Boxplot of flight route and their respective prices with airlines")

# Seems like there is separation between the route and the price, but we know that
# those most expensive flights are with the most expensive airline

# Does total stops play a role?
ggplot(flights, aes(x = Price, fill = Total_Stops)) +
  geom_density(alpha = 0.5) +
  ggtitle("Density plot of price distribution by the total stops")

# Seems like they do. Check the statistical significance
# First ensure sample sizes are large enough
flights %>% count(Total_Stops)

# Since group 4 only has one observation, drop it
flights_totalStops_filtered <- flights %>%
  filter(Total_Stops != 4)

# ANOVA
mdl_price_by_total_stops <- lm(Price ~ Total_Stops, data = flights_totalStops_filtered)
anova(mdl_price_by_total_stops)

# Post-hoc testing
pairwise.t.test(
  flights_totalStops_filtered$Price,
  flights_totalStops_filtered$Total_Stops,
  p.adjust.method = 'bonferroni'
)

# Shows that price will increase in some way with the number of stops, so it should be 
# included in the model

# The final two features will be engineered/derived to look at
flights_extended_features <- flightsWithRoutes %>%
  mutate(Total_Duration_min = Duration_hours*60 + Duration_min,
         Time_of_Day = cut(Dep_hours,
                           breaks = c(0, 6, 12, 18, 24),
                           labels = c("dawn", "morning", "afternoon", "night")))


# Flight duration
ggplot(flights_extended_features, aes(x = Total_Duration_min, y = Price)) +
  geom_point()

# There doesn't seem to be much of a correlation at all

# Time of day for departure and arrival
ggplot(flights_extended_features, aes(x = Time_of_Day, y = Price)) +
  geom_boxplot()

# There doesn't appear to be much separation at all here.

# The proposed features will thus be: Airline, Route, Total_Stops, and Month








