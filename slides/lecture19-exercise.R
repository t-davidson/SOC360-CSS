# Lecture 19 Exercise: Experimenting with Classification Algorithms
# Goal: Try different ML algorithms and compare their accuracy

# Load required libraries
library(tidyverse)
library(tidymodels)
library(rpart)         # For decision trees
library(kernlab)       # For SVM

# Load and prepare data
data <- read_csv("../data/2018_gss_sample.csv")

# Convert variables to factors (except numeric ones)
data <- data %>%
  mutate(across(-c(age, sibs), as.factor))

# Create train-test split (80/20)
set.seed(8901)
data_split <- initial_split(data, prop = 0.8)

# Pre-process data using recipe
data_recipe <- training(data_split) %>%
  recipe(degree ~ .) %>%
  step_scale(all_numeric_predictors(), -all_outcomes()) %>%
  step_dummy(all_factor_predictors(), -all_outcomes()) %>%
  prep()

# Extract training and testing data
data_testing <- data_recipe %>%
  bake(testing(data_split))

data_training <- juice(data_recipe)

# =============================================================================
# YOUR TASK: Choose and fit ONE of the following models
# =============================================================================

# OPTION 1: Logistic Regression
# Hint: Use logistic_reg() with mode = "classification"
# Hint: Set engine to "glm"
# Hint: Use fit(degree ~ ., data = data_training)

# model <- logistic_reg(mode = "classification") %>%
#   set_engine("???") %>%
#   fit(??? ~ ., data = ???)


# OPTION 2: Support Vector Machine (SVM)
# Hint: Use svm_rbf() with mode = "classification"
# Hint: Set engine to "kernlab"
# Hint: Use fit(degree ~ ., data = data_training)

# model <- svm_rbf(mode = "classification") %>%
#   set_engine("???") %>%
#   fit(??? ~ ., data = ???)


# OPTION 3: Decision Tree
# Hint: Use decision_tree() with mode = "classification"
# Hint: Set engine to "rpart"
# Hint: Use fit(degree ~ ., data = data_training)

# model <- decision_tree(mode = "classification") %>%
#   set_engine("???") %>%
#   fit(??? ~ ., data = ???)


# =============================================================================
# After fitting your model, run the code below to evaluate it
# =============================================================================

# Make predictions on training data
train_preds <- predict(model, data_training) %>%
    bind_cols(data_training) %>%
    select(degree, .pred_class)

# Make predictions on test data
test_preds <- predict(model, data_testing) %>%
    bind_cols(data_testing) %>%
    select(degree, .pred_class)

# Calculate accuracy for training data
train_accuracy <- train_preds %>%
    accuracy(truth = degree, estimate = .pred_class)

# Calculate accuracy for test data
test_accuracy <- test_preds %>%
    accuracy(truth = degree, estimate = .pred_class)

# Display results
cat("\n=== MODEL PERFORMANCE ===\n")
cat("Training Accuracy:", round(train_accuracy$.estimate, 4), "\n")
cat("Test Accuracy:", round(test_accuracy$.estimate, 4), "\n\n")

# Calculate additional metrics
test_precision <- test_preds %>%
    precision(truth = degree, estimate = .pred_class)
test_recall <- test_preds %>%
    recall(truth = degree, estimate = .pred_class)

cat("Test Precision:", round(test_precision$.estimate, 4), "\n")
cat("Test Recall:", round(test_recall$.estimate, 4), "\n")


# =============================================================================
# CHALLENGE: Try all three models and compare their performance!
# Which model performs best on the test data?
# =============================================================================
