

library(h2o)
h2o.init()
h2o.no_progress()

data_path <- "C:/Users/Ashish/Documents/R/uconn_comp_2018_train.csv/uconn_comp_2018_train.csv"

#load data into H2O

set.seed(123)
index <- createDataPartition(df_4$fraud, p = 0.75, list = FALSE)
df_train1 <- df_4[index,]
df_test1 <- df_4[-index,]

df_train_1 <- as.h2o(df_train)
df_test_1 <- as.h2o(df_test)

data <- as.h2o(df_4)
data_test <- as.h2o(df_test_4)

h2o.describe(data)

y <- "fraud"
x <- setdiff(names(data), c(y))

# AutoML

aml <- h2o.automl(y = y,
                  x = x,
                  training_frame = data,
                  max_runtime_secs = 10800,
                  max_models = 15,
                  stopping_metric = "AUC",
                  sort_metric = "AUC",
                  seed = 9)

lb <- aml@leaderboard
print(lb)


# DL

dl_1 <- h2o.deeplearning(x = x,
                       y = y,
                       training_frame = splits[[1]],
                       model_id = "dl_1",
                       hidden = c(32,32),
                       seed = 3,
                       activation = "RectifierWithDropout",
                       standardize = TRUE,
                       stopping_metric = "AUC",
                       max_runtime_secs = 10800,
                       epochs = 20,
                       nfolds = 5,
                       stopping_rounds = 0,
                       input_dropout_ratio = 0.25,
                       l1=1e-4,
                       validation_frame = splits[[2]],
                       variable_importances = TRUE,
                       balance_classes = TRUE,
                       max_after_balance_size = 1.5,
                       categorical_encoding = "Eigen")




cov_gbm <- h2o.gbm(x = x, y = y, training_frame = splits[[1]],
                   validation_frame = splits[[2]], balance_classes = TRUE, seed = 1234)

hyper_params <- list( balance_classes = c(TRUE, FALSE) )

grid <- h2o.grid(x = x, y = y, training_frame = splits[[1]], validation_frame = splits[[2]],
                 algorithm = "gbm", grid_id = "covtype_grid", hyper_params = hyper_params,
                 search_criteria = list(strategy = "Cartesian"), seed = 1234)

sortedGrid <- h2o.getGrid("covtype_grid", sort_by = "auc", decreasing = FALSE)
sortedGrid

h2o.auc(cov_gbm,valid = TRUE)

h2o.auc(dl_1)

dl_auc <- h2o.performance(model= dl_1, newdata = splits[[2]])

h2o.auc(dl_auc)

h2o.varimp_plot(dl_1)

plot(dl_1)

dl_test <- h2o.predict(dl_1, newdata = data_test)
pro = as.data.frame(x = dl_test$p1)

submit <- data.frame(claim_number = df_test$claim_number,
                     fraud = pro$p1)

write.csv(submit, file = "C:/Users/Ashish/Documents/R/uconn_comp_2018_train.csv/submit_4.csv",
          row.names = FALSE)


cv_models <- sapply(dl@model$cross_validation_models, 
                    function(i) h2o.getModel(i$name))

plot(cv_models[[1]], 
     timestep = "epochs", 
     metric = "classification_error")


# Grid
activation_opt <- c("Rectifier", "RectifierWithDropout")
l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01)

hyper_params <- list(activation = activation_opt, l1 = l1_opt, l2 = l2_opt)
search_criteria <- list(strategy = "RandomDiscrete", max_runtime_secs = 600)

splits <- h2o.splitFrame(data, ratios = 0.7, seed = 1)

dl_grid <- h2o.grid("deeplearning", x = x, y = y,
                    grid_id = "dl_grid",
                    training_frame = splits[[1]],
                    validation_frame = splits[[2]],
                    seed = 1,
                    hidden = c(32,32),
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

dl_gridperf <- h2o.getGrid(grid_id = "dl_grid", 
                           sort_by = "auc", 
                           decreasing = TRUE)
print(dl_gridperf)


