
glimpse(per_claim_test)


per_claim_col <- c("annual_premium","pol_number","pol_eff_dt","date_of_birth",
             "claim_office","agecat","agecat_impute")

per_claim_train %>% 
  select(-one_of(per_claim_col)) %>% 
  filter(!(claim_YN == 0)) %>% 
  mutate(cost_per_claim = claimcst0/numclaims) %>% 
  select(-claim_YN) %>% 
  select(-claimcst0) %>% 
  select(-numclaims) -> per_claim_train

per_claim_col_test <- c("quote_number","date_of_birth","agecat","agecat_impute")

per_claim_test %>% 
  left_join(prob_table, by = "quote_number") %>% 
  filter(avg > 0.5) %>% 
  select(-avg) -> per_claim_test

per_claim_test %>% 
  select(-one_of(per_claim_col_test)) -> per_claim_test

###################################### data partition ######################################

# checking the distribution
per_claim_train %>% 
  ggplot(aes(log2(cost_per_claim)))+
  geom_density()

set.seed(123)
ind <- createDataPartition(per_claim_train$cost_per_claim, p = 0.7, list = FALSE)
per_claim_1 <- per_claim_train[ind,]
per_claim_2 <- per_claim_train[-ind,]

# recipe object
recipe_per_claim <- recipe(cost_per_claim ~ ., data = per_claim_train) %>%
  step_meanimpute(credit_score) %>% 
  step_meanimpute(traffic_index) %>%
  step_log(cost_per_claim, base = 2) %>% 
  step_other(veh_body, threshold = 0.05, other = "other") %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  prep(data = per_claim_train)

recipe_per_claim

per_claim_train_auto <- bake(recipe_per_claim, newdata = per_claim_1)
per_claim_test_auto <- bake(recipe_per_claim, newdata = per_claim_2)

full_train <- bake(recipe_per_claim, newdata = per_claim_train)
full_test <- bake(recipe_per_claim, newdata = per_claim_test)
full_test$cost_per_claim <- NULL

################################## Modeling ###############################################


tr_ct <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        verboseIter = FALSE,
                        preProcOptions = c("center","scale"),
                        search = "random")

cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

#rm(mod_xgb)
set.seed(9)
mod_lm <- caret::train(cost_per_claim ~.,
                          data = per_claim_train_auto,
                          method = "lm",
                          trControl = tr_ct,
                          metric = "RMSE")

mod_lasso <- caret::train(cost_per_claim ~.,
                       data = per_claim_train_auto,
                       method = "lasso",
                       trControl = tr_ct,
                       metric = "RMSE",
                       tuneLength = 10)

stopCluster(cl)

mod_lm
mod_lasso$bestTune

lm_pred <- predict(mod_lm, newdata = per_claim_test_auto[,-7])
lasso_pred <- predict(mod_lasso, newdata = per_claim_test_auto[,-7])

postResample(pred = lm_pred, obs = per_claim_test_auto$cost_per_claim)
postResample(pred = lasso_pred, obs = per_claim_test_auto$cost_per_claim)

cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)


tr_ct <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 3,
                      verboseIter = FALSE,
                      preProcOptions = c("center","scale"),
                      search = "random")

LnrGrd <- expand.grid(nrounds = c(31,35),
                      eta = c(1.531463,1.6),
                      lambda = c(0.02673582),
                      alpha = c(0.007786066))

mod_xgb <- caret::train(cost_per_claim ~. + veh_age*veh_value,
                          data = full_train,
                          method = "xgbLinear",
                          trControl = tr_ct,
                          metric = "RMSE",
                        tuneGrid = LnrGrd)

stopCluster(cl)

mod_xgb
xgb_pred <- predict(mod_xgb, newdata = full_test)
postResample(pred = xgb_pred , obs = per_claim_test_auto$cost_per_claim)

plot(varImp(mod_xgb))

claim_table <- prob_table %>% 
  filter(avg > 0.5)

final_tbl <- data_frame(number = as.character(claim_table$quote_number),
                        claim_amount = 2^xgb_pred)
glimpse(final_tbl)

write.table(final_tbl,"C:/Users/Ashish/Documents/R/Zurich/claim_per.csv",
          sep ="|", row.names = FALSE)
