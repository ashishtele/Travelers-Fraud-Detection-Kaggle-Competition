

glimpse(df_comb)

df_comb$marital_status[is.na(df_comb$marital_status)] <- 1
df_comb$witness_present_ind[is.na(df_comb$witness_present_ind)] <- 0
df_comb$age_of_vehicle[is.na(df_comb$age_of_vehicle)] <- 5
df_comb$claim_est_payout[is.na(df_comb$claim_est_payout)] <- 4800


df_test_comb$marital_status[is.na(df_test_comb$marital_status)] <- 1
df_test_comb$witness_present_ind[is.na(df_test_comb$witness_present_ind)] <- 0
df_test_comb$age_of_vehicle[is.na(df_test_comb$age_of_vehicle)] <- 5
df_test_comb$claim_est_payout[is.na(df_test_comb$claim_est_payout)] <- 4800

#train
df_comb %>% 
  mutate(claim_percent = claim_est_payout/vehicle_price,
         income_per_age = annual_income/age_of_driver,
         safty_per_drage = safty_rating/age_of_driver
         # age_buck = case_when(age_of_driver < 20 ~ "1",
         #                      age_of_driver >=20 & age_of_driver <50 ~ "2",
         #                      age_of_driver >= 50 & age_of_driver < 80 ~ "3",
         #                      age_of_driver >= 80 ~ "4")
         ) -> cat_df_1

#test
df_test_comb %>% 
  mutate(claim_percent = claim_est_payout/vehicle_price,
         income_per_age = annual_income/age_of_driver,
         safty_per_drage = safty_rating/age_of_driver
         # age_buck = case_when(age_of_driver < 20 ~ "1",
         #                      age_of_driver >=20 & age_of_driver <50 ~ "2",
         #                      age_of_driver >= 50 & age_of_driver < 80 ~ "3",
         #                      age_of_driver >= 80 ~ "4")
         ) -> cat_df_test_1

table(cat_df_1$age_buck)

cat_df_1 %>% 
  select(-claim_number) %>% 
  select(-annual_income) %>% 
  select(-claim_date) %>% 
  mutate(year = as.double(year),
         day = as.double(day),
         wday = as.double(wday),
         week = as.double(week),
         zip_code = as.factor(zip_code)) %>% 
  select(-wday) %>% 
  select(-vehicle_color) %>% 
  select(-month)-> cat_df_1

#test
cat_df_test_1 %>% 
  select(-claim_number) %>% 
  select(-annual_income) %>% 
  select(-claim_date) %>% 
  mutate(year = as.double(year),
         day = as.double(day),
         wday = as.double(wday),
         week = as.double(week),
         zip_code = as.factor(zip_code)) %>% 
  select(-wday) %>% 
  select(-vehicle_color) -> cat_df_test_1



colSums(is.na(cat_df_test_1))


#cat_df_1$annual_income[(cat_df_1$annual_income == -1)] <- 37399

glimpse(cat_df_test_1)
# 
# 
# df_4 %>%
#   mutate(zip_code = as.factor(zip_code)) -> cat_df
# 
# cat_df %>% 
#   mutate(year = as.double(year),
#          day = as.double(day),
#          wday = as.double(wday),
#          cnt_cl_site_zip = as.double(cnt_cl_site_zip),
#          cnt_cl_zip_chnl = as.double(cnt_cl_zip_chnl),
#          cnt_cate_zip = as.double(cnt_cate_zip)) -> cat_df
# 
# 

glimpse(cat_df_1)


#library(caret)
set.seed(123)
index <- createDataPartition(cat_df_1$fraud, p = 0.90, list = FALSE)
df_train <- cat_df_1[index,]
df_test <- cat_df_1[-index,]


#library(catboost)

set.seed(123)
target <- c(20)
train_pool <- catboost.load_pool(data = df_train[,-target],
                                 label = as.numeric(unlist(df_train[,target])))
test_pool <- catboost.load_pool(data = df_test[,-target],
                                label = as.numeric(unlist(df_test[,target])))

head(train_pool,1)

fit_params <- list(iterations = 1000,
                   thread_count = 12,
                   loss_function = 'Logloss',
                   custom_loss = 'AUC',
                   eval_metric = 'AUC',
                   border_count = 64,
                   depth =5,
                   learning_rate = 0.1,
                   l2_leaf_reg = 3,
                   train_dir = 'train_dir',
                   prediction_type = 'Probability',
                   random_seed = 12,
                   od_type = 'Iter',
                   od_wait = 20,
                   use_best_model = TRUE
                   )
model <- catboost.train(train_pool, test_pool, fit_params)


calc_accuracy <- function(prediction, expected) {
  labels <- ifelse(prediction > 0.5, 1, 0)
  accuracy <- sum(labels == expected) / length(labels)
  return(accuracy)
}

prediction <- catboost.predict(model, test_pool, prediction_type = 'Probability')
cat("Sample predictions: ", sample(prediction, 5), "\n")

accuracy <- calc_accuracy(prediction, df_test[,target])
cat("\nAccuracy: ", accuracy, "\n")

#library(tibble)
feat_imp <- as_tibble(catboost.get_feature_importance(model)) %>% 
  rownames_to_column() %>% 
  select(Feature = rowname, Importance = value ) %>% 
  arrange(-Importance)

ggplot(feat_imp, aes(x = reorder(Feature,Importance), y = Importance)) +
  geom_bar(stat='identity')+
  coord_flip() +
  labs(x = 'variables')

test_pool_1 <- catboost.load_pool(data = cat_df_test_1)

pred_1 <- catboost.predict(model, test_pool_1, prediction_type = 'Probability')

submit <- data.frame(claim_number = df_test_comb$claim_number,
                     fraud = pred_1)

write.csv(submit, file = "C:/Users/Ashish/Documents/R/uconn_comp_2018_train.csv/submit_6.csv",
          row.names = FALSE)



