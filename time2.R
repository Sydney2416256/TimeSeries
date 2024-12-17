library(tidyverse)
library(vroom)
library(DataExplorer)
library(patchwork)
library(tidymodels)
library(dplyr)
library(poissonreg)
library(glmnet)
library(ggplot2)
library(gridExtra)
library(embed)
library(discrim)
library(naivebayes)
library(bonsai)
library(lightgbm)
library(themis)
library(forecast)
library(modeltime) 
library(timetk)


# Load in Data
data <- vroom("STAT348/time/train.csv") 
testdata <- vroom("STAT348/time/test.csv") 


storeItem <- data %>%
  filter(store == 7, item %in% c(4,13))

newStoreItem <- storeItem |> 
  mutate(day_of_week = wday(date)) |> 
  mutate(month = month(date)) |> 
  mutate(year = year(date)) 

# Create a recipe for ARIMA
arima_recipe <- recipe(sales ~ date, data = storeItem) %>%
  step_date(date, features = c("year", "month", "dow"))

# Define the ARIMA Model
arima_model <- arima_reg(seasonal_period = 7,  # Set seasonal period to 7 for weekly seasonality
                         non_seasonal_ar = 5,  # Max AR for non-seasonal part
                         non_seasonal_ma = 5,  # Max MA for non-seasonal part
                         seasonal_ar = 2,      # Max AR for seasonal part
                         seasonal_ma = 2,      # Max MA for seasonal part
                         non_seasonal_differences = 2, # Max d to tune
                         seasonal_differences = 2) %>%  # Max D to tune
  set_engine("auto_arima")

# Workflow for ARIMA Model
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model)

# Split the data into training and testing (time-series split)
v_split <- rolling_origin(storeItem, 
                          initial = 100, # Initial training size (adjust as needed)
                          assess = 30,   # Number of observations in each test set
                          skip = 10)     # Number of observations to skip between resamples

# Cross-validation (time-series based)
CV_results_arima <- arima_wf %>%
  fit_resamples(
    resamples = v_split,
    metrics = metric_set(rmse, mae)
  )

# Get the best ARIMA model (usually ARIMA has no tuning parameters in the `tune()` context)
# Since ARIMA is not tuned in the same way as other models, we proceed with fitting directly.

# Now, for each store and item, fit the ARIMA model
nStores <- 10
nItems <- 50
all_preds <- NULL

for(s in 1:nStores) {
  for(i in 1:nItems) {
    storeItemTrain <- data %>%
      filter(store == s, item == i)
    storeItemTest <- testdata %>%
      filter(store == s, item == i)
    
    # Fit the ARIMA model to training data
    final_wf_arima <- arima_wf %>%
      fit(data = storeItemTrain)
    
    # Make predictions
    preds <- predict(final_wf_arima, new_data = storeItemTest, type = "numeric")
    
    print(paste("Store:", s, ", Item:", i))
    
    # Collect all predictions
    if(s == 1 & i == 1) {
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}
