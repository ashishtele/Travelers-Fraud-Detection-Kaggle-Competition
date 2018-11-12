
library(mlbench)
library(caret)
library(caretEnsemble)


# Predictors

x_train_tbl <- bake(rec_obj, newdata = train_tbl)
x_test_tbl <- bake(rec_obj, newdata = test_tbl)

x_train <- rbind(x_train_tbl, x_test_tbl)
x_train$fraud <- as.factor(x_train$fraud)
levels(x_train$fraud) <- c("zero","one")


x_train_tbl$fraud <- as.factor(x_train_tbl$fraud)
x_test_tbl$fraud <- as.factor(x_test_tbl$fraud)

levels(x_train_tbl$fraud) <- c("zero","one")
levels(x_test_tbl$fraud) <- c("zero","one")



glimpse(x_train_tbl)

# Response variables
library(gbm)

control <- trainControl(method="repeatedcv",
                        number=5, repeats=3,
                        savePredictions=TRUE,
                        classProbs=TRUE,
                        index = createFolds(x_train_tbl$fraud, 5))
algorithmList <- c('knn','rpart','gbm')
set.seed(12)

library(randomForest)
models <- caretList(fraud~., 
                    data=x_train_tbl,
                    trControl=control,
                    methodList=algorithmList,
                    metric = "ROC")
results <- resamples(models)
summary(results)
dotplot(results)


# correlation between results
modelCor(results)
splom(results)


glm_ensemble <- caretStack(
  models,
  method="gbm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)
glm_ensemble




#####################################################################################

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
xgbGrid <- expand.grid(nrounds = c(210),
                       max_depth = c(4),
                       eta = c(0.0686),
                       gamma = c(9),
                       colsample_bytree = c(0.3715),
                       min_child_weight = c(18),
                       subsample = c(0.3011))


# XGB
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

rm(mod_xgb)
set.seed(9)
mod_xgb <- caret::train(fraud~.,
                        data = x_train,
                        method = "xgbTree",
                        trControl = train_ctrl1,
                        metric = "ROC",
                        tuneGrid = xgbGrid)
stopCluster(cl)

mod_xgb
mod_xgb$bestTune
plot(varImp(mod_xgb))

glimpse(x_train)

pred_xgb <- predict(mod_xgb, newdata = x_train[,-11], type = "prob")

pred_xgb_test <- predict(mod_xgb, newdata = x_test, type = "prob")

##Rf
set.seed(11)

set.seed(1992)
rfGrid <- expand.grid(
                      mtry = c(4,3))
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

set.seed(9)
mod_rf <- caret::train(fraud~.,
                        data = x_train,
                        method = "rf",
                        trControl = train_ctrl1,
                        metric = "ROC",
                        tuneGrid = rfGrid)

pred_rf <- predict(mod_rf, newdata = x_train[,-11], type = "prob")

pred_rf_test <- predict(mod_rf, newdata = x_test, type = "prob")

set.seed(10)
mod_glm <- caret::train(fraud~.,
                        data = x_train,
                        method = "glm",
                        trControl = train_ctrl1,
                        metric = "ROC",
                        #tuneGrid = rfGrid
                        tuneLength=5)

pred_glm <- predict(mod_glm, newdata = x_train[,-11], type = "prob")

pred_glm_test <- predict(mod_glm, newdata = x_test, type = "prob")


yash_xgb <- fread("C:/Users/Ashish/Documents/R/uconn_comp_2018_train.csv/train.csv",
                  colClasses = "numeric")

yash_xgb_test <- fread("C:/Users/Ashish/Documents/R/uconn_comp_2018_train.csv/test.csv",
                  colClasses = "numeric")


## table building


tbl_new_1 <- tibble(xgb = pred_xgb$one,
                    rf = pred_rf$one,
                    glm = pred_glm$one,
                    xgb_yash = yash_xgb$fraud) %>% data.frame()

tbl_new_2 <- tibble(xgb = pred_xgb$one,
                  rf = pred_rf$one,
                  glm = pred_glm$one,
                  xgb_yash = yash_xgb$fraud,
                  fraud = x_train$fraud) %>% data.frame()

glimpse(tbl_new_2)

# test table
tbl_test_1 <- tibble(xgb = pred_xgb_test$one,
                    rf = pred_rf_test$one,
                    glm = pred_glm_test$one,
                    xgb_yash = yash_xgb_test$fraud) %>% data.frame()


final_blender_model <- caret::train(fraud~., data = tbl_new_2,
                                    method='rpart')

stack_test <- predict(final_blender_model, newdata = tbl_test_1, type = "prob") %>% 
  as.data.frame() %>% select(one)


x_train_new <- cbind(x_train,tbl_new_1)

final_blender_model_1 <- caret::train(fraud~., data = tbl_new_2,
                                    method='glm')


stack <- predict(final_blender_model, newdata = tbl_new_1, type = "prob") %>% 
  as.data.frame() %>% select(one)


write.csv(tbl_test_1, file = "C:/Users/Ashish/Documents/R/uconn_comp_2018_train.csv/four_model.csv",
          row.names = FALSE)
