

df_train <- fread("C:/Users/Ashish/Documents/R/McKinsey-Hackathon/train.csv", 
                  colClasses = "numeric") 

df_test <- fread("C:/Users/Ashish/Documents/R/McKinsey-Hackathon/valid.csv", 
                  colClasses = "numeric") 


col_del <- c("annual_income","Avg_safety.x","claim_date","zip_code","V1")

df_train %>% 
  select(-one_of(col_del)) ->  df_train

df_test %>% 
  select(-one_of(col_del)) ->  df_test

rec_objct <- recipe(fraud ~ ., data = df_train) %>% 
  step_sqrt(claim_est_payout) %>% 
  step_log(vehicle_price) %>% 
  step_log(vehicle_weight) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  prep(data = df_train)

rec_objct


# Predictors

df_train$fraud <- as.factor(df_train$fraud)
df_test$fraud <- as.factor(df_test$fraud)

levels(df_train$fraud) <- c("zero","one")
levels(df_test$fraud) <- c("zero","one")


x_train_tbl <- bake(rec_objct, newdata = df_train)
x_test_tbl <- bake(rec_objct, newdata = df_test)

#########################################################################################

glimpse(x_train_tbl)

df_train_1 <- df_train %>%
  as.h2o()
df_test_1 <- df_test %>% 
  as.h2o()

y <- "fraud"
x <- setdiff(names(df_train_1), c(y))


glimpse(df_test_1)
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper","h2o.deeplearning.wrapper")

metalearner <- "h2o.glm.wrapper"


fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = df_train_1, 
                    family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

perf <- h2o.ensemble_performance(fit, newdata = df_test_1)
perf

nfolds <- 5  
family <- "binomial"

glm1 <- h2o.glm(x = x, y = y, family = family, 
                training_frame = df_train_1,
                nfolds = nfolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

gbm1 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = df_train_1,
                seed = 1,
                nfolds = nfolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

rf1 <- h2o.randomForest(x = x, y = y, # distribution not used for RF
                        training_frame = df_train_1,
                        seed = 1,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)

dl1 <- h2o.deeplearning(x = x, y = y, distribution = "bernoulli",
                        training_frame = df_train_1,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)

xg <- h2o.naiveBayes(x = x, y = y, 
                     training_frame = df_train_1,
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE)

models <- list(glm1, gbm1)
metalearner <- "h2o.glm.wrapper"

stack <- h2o.stack(models = models,
                   response_frame = df_train_1[,y],
                   metalearner = metalearner, 
                   seed = 1,
                   keep_levelone_data = TRUE)
perf <- h2o.ensemble_performance(stack, newdata = df_test_1)
perf


############################################################################################


set.seed(10)

train_ctrl1 <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            verboseIter = FALSE,
                            #preProcOptions = c("center","scale"),
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = twoClassSummary,
                            search = "random")

set.seed(1992)
xgbGrid <- expand.grid(nrounds = c(699),
                       max_depth = c(1),
                       eta = c(0.2669556),
                       gamma = c(0.6758),
                       colsample_bytree = c(0.6260),
                       min_child_weight = c(19),
                       subsample = c(0.95049))


# XGB
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

#rm(mod_xgb)
set.seed(12)
mod_xgb_new <- caret::train(fraud~.,
                        data = x_train_tbl,
                        method = "xgbTree",
                        trControl = train_ctrl1,
                        metric = "ROC",
                        tuneGrid = xgbGrid)
stopCluster(cl)

mod_xgb_new
mod_xgb_new$bestTune
plot(varImp(mod_xgb_new))

glimpse(x_test_tbl)
pred_xgb_valid <- predict(mod_xgb_new, newdata = x_test_tbl[,-14], type = "prob")
roc(as.character(x_test_tbl$fraud), as.numeric(pred_xgb_valid$zero))

# glm
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

#rm(mod_xgb)
set.seed(13)
mod_glm_new <- caret::train(fraud~.,
                            data = x_train_tbl,
                            method = "glm",
                            trControl = train_ctrl1,
                            metric = "ROC",
                            tuneLength =  5)
stopCluster(cl)

pred_glm_valid <- predict(mod_glm_new, newdata = x_test_tbl[,-14], type = "prob")
roc(x_test_tbl$fraud,pred_glm_valid$one)

# treebag
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

set.seed(14)
mod_treebag_new <- caret::train(fraud~.,
                            data = x_train_tbl,
                            method = "treebag",
                            trControl = train_ctrl1,
                            metric = "ROC",
                            tuneLength =  3)
stopCluster(cl)

mod_treebag_new
pred_treebag_valid <- predict(mod_treebag_new, newdata = x_test_tbl[,-14], type = "prob")

final_valid <- fread("C:/Users/Ashish/Documents/R/McKinsey-Hackathon/final_valid.csv", 
                 colClasses = "numeric")

valid_data <- x_test_tbl
valid_data$pred_xgb <- pred_xgb_valid$zero
valid_data$pred_glm <- pred_glm_valid$zero
valid_data$pred_bag <- NULL
valid_data$pred_xgb_yash <- final_valid$fraud


cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

#rm(mod_xgb)
set.seed(13)
stack_1 <- caret::train(fraud~.,
                            data = valid_data,
                            method = "glm",
                            trControl = train_ctrl1,
                            metric = "ROC",
                            tuneLength =  5)
stopCluster(cl)
stack_1
