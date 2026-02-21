#| echo: false
#| eval: true

challenge_data <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    home_value = "B25077_001",      # YOUR TARGET
    total_pop = "B01003_001",       # Total population
    median_income = "B19013_001",   # Median household income
    median_age = "B01002_001",      # Median age
    percent_college = "B15003_022", # Bachelor's degree or higher
    median_rent = "B25058_001",     # Median rent
    poverty_rate = "B17001_002",     # Population in poverty
    rent_burden = "B25071_001",
    occupied_unit = "B25003_001"
  ),
  year = 2022,
  output = "wide"
)

lm1 <- lm(median_incomeE ~ home_valueE, data = challenge_data)#
lg1 <- lm(median_incomeE ~ log(home_valueE), data = challenge_data)

lm2 <- lm(median_ageE ~ home_valueE, data = challenge_data)
lg2 <- lm(median_ageE ~ log(home_valueE), data = challenge_data)#

lm3 <- lm(percent_collegeE ~ home_valueE, data = challenge_data)#
lg3 <- lm(percent_collegeE ~ log(home_valueE), data = challenge_data)

lm4 <- lm(median_rentE ~ home_valueE, data = challenge_data)#
lg4 <- lm(median_rentE ~ log(home_valueE), data = challenge_data)

lm5 <- lm(poverty_rateE ~ home_valueE, data = challenge_data)
lg5 <- lm(poverty_rateE ~ log(home_valueE), data = challenge_data)

lm6 <- lm(rent_burdenE ~ home_valueE, data = challenge_data)
lg6 <- lm(rent_burdenE ~ log(home_valueE), data = challenge_data)

lm7 <- lm(occupied_unitE ~ home_valueE, data = challenge_data)
lg7 <- lm(occupied_unitE ~ log(home_valueE), data = challenge_data)

lm8 <- lm(total_popE ~ home_valueE, data = challenge_data)#
lg8 <- lm(total_popE ~ log(home_valueE), data = challenge_data)


model <- lm(home_valueE ~ total_popE + median_incomeE + log(median_ageE) + percent_collegeE 
            + median_rentE,
             data = challenge_data)
summary(model)

model1 <- lm(home_valueE ~ total_popE + median_incomeE + percent_collegeE 
            + median_rentE,
            data = challenge_data)

model2 <- lm(home_valueE ~ total_popE + median_incomeE + log(median_ageE) + percent_collegeE 
            + median_rentE + occupied_unitE,
            data = challenge_data)

set.seed(123)
n <- nrow(challenge_data)

# 90% training, 10% testing
train_indices <- sample(1:n, size = 0.9 * n)
train_data <- challenge_data[train_indices, ]
test_data <- challenge_data[-train_indices, ]

# Fit on training data only
model_train <- model

# Predict on test data
test_predictions <- predict(model_train, newdata = test_data)

# Evaluate Predictions (RMSE)
rmse_test <- sqrt(mean((test_data$home_valueE - test_predictions)^2, na.rm = TRUE))
rmse_train <- summary(model_train)$sigma

cat("Training RMSE:", round(rmse_train, 0), "\n")
cat("Test RMSE:", round(rmse_test, 0), "\n")