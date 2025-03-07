##GLOBAL##

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(reactable)
library(corrplot)
library(RColorBrewer)
library(caret)
library(xgboost)

# Memuat dataset
data <- read.csv("healthcare-dataset-stroke-data_CLEAN.csv")

# Mengubah kolom yang sesuai menjadi faktor (kategorikal)
vars_kategorikal <- c("gender", "hypertension", "heart_disease", "ever_married", "work_type", "Residence_type", "smoking_status", "stroke")
data[vars_kategorikal] <- lapply(data[vars_kategorikal], factor)

# Mengubah tipe tampilan "factor" menjadi "object" dan tipe numerik menjadi tipe spesifiknya
data_types <- sapply(data, class)
data_types[data_types == "factor"] <- "object"
data_types[sapply(data, is.numeric) & sapply(data, function(x) all(x == as.integer(x)))] <- "int"
data_types[sapply(data, is.numeric) & !sapply(data, function(x) all(x == as.integer(x)))] <- "float"

# Split data into features and target
X <- data[, !(names(data) %in% "stroke")]
y <- data$stroke

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Training XGBoost model with tuned hyperparameters
model <- xgb.train(
  params = list(
    objective = "binary:logistic",
    colsample_bytree = 0.8,
    learning_rate = 0.01,
    max_depth = 3,
    n_estimators = 300,
    subsample = 0.8
  ),
  data = xgb.DMatrix(data.matrix(X_train), label = as.numeric(y_train) - 1),
  nrounds = 300,
  watchlist = list(train = xgb.DMatrix(data.matrix(X_train), label = as.numeric(y_train) - 1)),
  eval_metric = "error",
  verbose = 0
)