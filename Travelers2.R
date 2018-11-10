
data <- as.h2o(df_4)
splits <- h2o.splitFrame(data, ratios = 0.7, seed = 1)

h2o.describe(data)

y <- "fraud"
x <- setdiff(names(data), c(y))

nfolds <- 5
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = splits[[1]],
                  distribution = "bernoulli",
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.1,
                  nfolds = nfolds,
                  fold_assignment = "Stratified",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1,
                  balance_classes = TRUE)

h2o.performance(my_gbm, newdata = splits[[2]])

gbm_params1 <- list(learn_rate = c(0.1,0.15),
                    max_depth = c(3,2),
                    sample_rate = c(0.9,1.0),
                    col_sample_rate = c(0.18, 0.2, 0.22))


rm(gbm_params3)
gbm_params3 <- list(learn_rate = seq(0.14, 0.17, 0.01),
                    max_depth = seq(2, 4, 1),
                    sample_rate = seq(0.9,1.0,0.01),
                    col_sample_rate = seq(0.15, 0.18, 0.01))
search_criteria <- list(strategy = "RandomDiscrete", max_models = 36, seed = 1)

rm(gbm_grid2)
gbm_grid2 <- h2o.grid("gbm", x = x, y = y,
                      grid_id = "gbm_grid2",
                      training_frame = splits[[1]],
                      validation_frame = splits[[2]],
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params3,
                      search_criteria = search_criteria)

gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid2",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)
best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])

## search
