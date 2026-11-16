library(mlexperiments)
library(mlbench)

DNA<- read.csv('../IHME-GBD_2021_DATA-1e92dcc0-1/IHME-GBD_2021_DATA-4f4711e4-1.csv',header = T)
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("Class")]

ncores <- ifelse(
  test = parallel::detectCores() > 4,
  yes = 4L,
  no = ifelse(
    test = parallel::detectCores() < 2L,
    yes = 1L,
    no = parallel::detectCores()
  )
)
if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
  # on cran
  ncores <- 2L
}

param_list_knn <- expand.grid(
  k = seq(4, 68, 8),
  l = 0,
  test = parse(text = "fold_test$x")
)

knn_bounds <- list(k = c(2L, 80L))

optim_args <- list(
  n_iter = ncores,
  kappa = 3.5,
  acq = "ucb"
)

param_list_knn <- expand.grid(
  k = seq(4, 68, 8),
  l = 0,
  test = parse(text = "fold_test$x")
)

knn_bounds <- list(k = c(2L, 80L))

optim_args <- list(
  n_iter = ncores,
  kappa = 3.5,
  acq = "ucb"
)
knn_tune_bayesian <- mlexperiments::MLTuneParameters$new(
  learner = LearnerKnn$new(),
  strategy = "bayesian",
  ncores = ncores,
  seed = seed
)

knn_tune_bayesian$parameter_bounds <- knn_bounds
knn_tune_bayesian$parameter_grid <- param_list_knn
knn_tune_bayesian$split_type <- "stratified"
knn_tune_bayesian$optim_args <- optim_args

# set data
knn_tune_bayesian$set_data(
  x = train_x,
  y = train_y
)

results <- knn_tune_bayesian$execute(k = 3)
head(results)
knn_tune_grid <- mlexperiments::MLTuneParameters$new(
  learner = LearnerKnn$new(),
  strategy = "grid",
  ncores = ncores,
  seed = seed
)

knn_tune_grid$parameter_grid <- param_list_knn
knn_tune_grid$split_type <- "stratified"

# set data
knn_tune_grid$set_data(
  x = train_x,
  y = train_y
)

results <- knn_tune_grid$execute(k = 3)
head(results)

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
)
str(fold_list)

knn_cv <- mlexperiments::MLCrossValidation$new(
  learner = LearnerKnn$new(),
  fold_list = fold_list,
  seed = seed
)

best_grid_result <- knn_tune_grid$results$best.setting
best_grid_result

knn_cv$learner_args <- best_grid_result[-1]

knn_cv$predict_args <- list(type = "response")
knn_cv$performance_metric <- metric("ACC")
knn_cv$return_models <- TRUE

# set data
knn_cv$set_data(
  x = train_x,
  y = train_y
)

results <- knn_cv$execute()
head(results)
knn_cv_nested_bayesian <- mlexperiments::MLNestedCV$new(
  learner = LearnerKnn$new(),
  strategy = "bayesian",
  fold_list = fold_list,
  k_tuning = 3L,
  ncores = ncores,
  seed = seed
)

knn_cv_nested_bayesian$parameter_grid <- param_list_knn
knn_cv_nested_bayesian$parameter_bounds <- knn_bounds
knn_cv_nested_bayesian$split_type <- "stratified"
knn_cv_nested_bayesian$optim_args <- optim_args

knn_cv_nested_bayesian$predict_args <- list(type = "response")
knn_cv_nested_bayesian$performance_metric <- metric("bacc")

# set data
knn_cv_nested_bayesian$set_data(
  x = train_x,
  y = train_y
)

results <- knn_cv_nested_bayesian$execute()
head(results)
knn_cv_nested_grid <- mlexperiments::MLNestedCV$new(
  learner = LearnerKnn$new(),
  strategy = "grid",
  fold_list = fold_list,
  k_tuning = 3L,
  ncores = ncores,
  seed = seed
)

knn_cv_nested_grid$parameter_grid <- param_list_knn
knn_cv_nested_grid$split_type <- "stratified"

knn_cv_nested_grid$predict_args <- list(type = "response")
knn_cv_nested_grid$performance_metric <- metric("bacc")

# set data
knn_cv_nested_grid$set_data(
  x = train_x,
  y = train_y
)

results <- knn_cv_nested_grid$execute()
head(results)
