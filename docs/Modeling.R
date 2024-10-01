# Regression modeling

# Import libraries
library(tidymodels)
library(ranger)
library(rpart)
library(rpart.plot)
library(ggfortify)

# Proposed features: Airline, Route, Total_Stops, and Month

model_dataset <- flights_extended_features %>%
  select(Price, Airline, Route, Total_Stops, Month)

# Train test split
set.seed(123)
train_test_split <- initial_split(model_dataset, prop = 0.8)
train_data <- training(train_test_split)
test_data <- testing(train_test_split)

# Fit a linear regression model
fmla <- Price ~ Airline + Route + Total_Stops + Month + 0
linear_mdl_price <- lm(fmla, data = train_data)
linear_mdl_price

# Evaluate the model on the training data
linear_mdl_price %>%
  glance()

# Make predictions on the test data
explanatory_data_lr <- test_data %>%
  select(-Price)

explanatory_data_lr <- explanatory_data_lr %>%
  mutate(Price_pred = predict(linear_mdl_price, explanatory_data_lr))

explanatory_data_lr$Price <- test_data$Price

# Evaluate the model on the testing data
explanatory_data_lr <- explanatory_data_lr %>%
  mutate(res_price = Price_pred - Price,
         res_mean = Price - mean(Price),
         res_price_sq = res_price^2,
         res_mean_sq = res_mean^2)

total_SS <- sum(explanatory_data_lr$res_mean_sq)
res_SS <- sum(explanatory_data_lr$res_price_sq)
r_squared_lr <- 1 - (res_SS/total_SS)

# This model has an R-squared value of 0.648 on the test dataset. It could be overfitting
# since the R-squared value on the training dataset was much higher.

# How would a random forest model compare?
# Fit a random forest model
rf_mdl_price <- ranger(fmla, data = train_data,
                       num.trees = 500,
                       respect.unordered.factors = "order")

# Make predictions
explanatory_data_rf <- test_data %>%
  select(-Price)

explanatory_data_rf <- explanatory_data_rf %>%
  mutate(Price_pred = predict(rf_mdl_price, explanatory_data_rf)$predictions)

explanatory_data_rf$Price <- test_data$Price

# Evaluate the model
explanatory_data_rf <- explanatory_data_rf %>%
  mutate(res_price = Price_pred - Price,
         res_mean = Price - mean(Price),
         res_price_sq = res_price^2,
         res_mean_sq = res_mean^2)

total_SS <- sum(explanatory_data_rf$res_mean_sq)
res_SS <- sum(explanatory_data_rf$res_price_sq)
r_squared_rf <- 1 - (res_SS/total_SS)

# This model has an R-squared value of 0.6911 on the testing dataset.

# Finally, how would a decision tree model compare?
# Fit a decision tree model
dt_mdl_price <- rpart(fmla, method = "anova", data = train_data)

# Plot the decision tree model
rpart.plot(dt_mdl_price, fallen.leaves = TRUE)

# Make predictions
explanatory_data_dt <- test_data %>%
  select(-Price)

explanatory_data_dt <- explanatory_data_dt %>%
  mutate(Price_pred = predict(dt_mdl_price, explanatory_data_dt))

explanatory_data_dt$Price <- test_data$Price

# Evaluate the model
explanatory_data_dt <- explanatory_data_dt %>%
  mutate(res_price = Price_pred - Price,
         res_mean = Price - mean(Price),
         res_price_sq = res_price^2,
         res_mean_sq = res_mean^2)

total_SS <- sum(explanatory_data_dt$res_mean_sq)
res_SS <- sum(explanatory_data_dt$res_price_sq)
r_squared_dt <- 1 - (res_SS/total_SS)

# The R-squared value for this model is 0.418

# It is possible that the model is not as accurate as it could be because of interactions
# Interactions are where the effect of one explanatory variable on the response variable
# depends on the value of one or more other explanatory variables

# Define a new formula with interactions
fmla_int <- Price ~ Airline * Route * Total_Stops * Month + 0

# Re-build the linear regression model with the new formula and evaluate it
linear_mdl_price_int <- lm(fmla_int, data = train_data)

# Make predictions
explanatory_data_lr_int <- test_data %>%
  select(-Price)

explanatory_data_lr_int <- explanatory_data_lr_int %>%
  mutate(Price_pred = predict(linear_mdl_price_int, explanatory_data_lr_int))

explanatory_data_lr_int$Price <- test_data$Price

# Evaluate the model on the testing data
explanatory_data_lr_int <- explanatory_data_lr_int %>%
  mutate(res_price = Price_pred - Price,
         res_mean = Price - mean(Price),
         res_price_sq = res_price^2,
         res_mean_sq = res_mean^2)

total_SS <- sum(explanatory_data_lr_int$res_mean_sq)
res_SS <- sum(explanatory_data_lr_int$res_price_sq)
r_squared_lr_int <- 1 - (res_SS/total_SS)

# The R-squared value for this model is 0.697

# Since the tree models do not accept interaction terms, the best model observed was the
# linear regression model with interaction terms.

# Plot the linear regression model with interactions
ggplot(explanatory_data_lr_int, aes(x = Price_pred, y = res_price)) +
  geom_point() +
  ggtitle("Model residuals against predicted prices on test data")

# Make diagnostic plots of the linear regression model with interactions (training data)
autoplot(
  linear_mdl_price_int,
  which = 1:3,
  nrow = 3,
  ncol = 1
)

# Create a tibble of the final R-squared results and print it
final_results <- tibble(
  model = c("Linear Regression", "Random Forest", "Decision Tree", 
            "Linear Regression with Interactions"),
  r_squared = c(r_squared_lr, r_squared_rf, r_squared_dt, r_squared_lr_int)
)

final_results


