library(tidymodels)
library(palmerpenguins)

set.seed(123)

data_split <- initial_split(penguins, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

cv_folds <- vfold_cv(train_data, v = 10)

print(cv_folds)
