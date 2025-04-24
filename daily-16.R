# Load necessary libraries
library(tidymodels)
library(palmerpenguins)
library(ranger)

set.seed(123)

# Remove rows with missing values
penguins_clean <- na.omit(penguins)

# Data splitting
data_split <- initial_split(penguins_clean, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create 10-fold cross-validation
cv_folds <- vfold_cv(train_data, v = 10)

print(cv_folds)

# Define a preprocessing recipe
penguins_recipe <- recipe(species ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors(), -all_outcomes()) %>%  # Convert categorical predictors
  step_normalize(all_numeric_predictors())  # Normalize numeric predictors

# Define logistic regression (multinomial) model
multinom_model <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")

# Define random forest model
rand_forest_model <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Create a workflow set using model specifications
penguins_wf_set <- workflow_set(
  preproc = list(penguins_recipe),
  models = list(multinom = multinom_model, rf = rand_forest_model)
)

# Fit and compare models using cross-validation
set.seed(123)
results <- penguins_wf_set %>%
  workflow_map("fit_resamples", resamples = cv_folds, metrics = metric_set(accuracy))

# Rank models by accuracy
ranked_results <- rank_results(results, rank_metric = "accuracy")

# Print ranked results
print(ranked_results)

# Comment: Based on accuracy, the best model is the logistic regression model (recipe_multinom).
# It outperforms the random forest model (rf) in terms of accuracy. Logistic regression is also more
# interpret able, making it a good choice for classification tasks where model transparency is important.
