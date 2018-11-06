rm(list = ls())

load_lb <- function()
{
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(tidyr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(caret))
  suppressPackageStartupMessages(library(rpart))
  suppressPackageStartupMessages(require(data.table))
  suppressPackageStartupMessages(require(Matrix))
  library(textfeatures)
  library(doParallel)
  library(pROC)
  library(cutpointr)
  library(data.table)
  library(catboost)
  library(recipes)
  library(corrplot)
  library(ggthemes)
}

load_lb()


## Input file read

df <- fread("C:/Users/Ashish/Documents/R/uconn_comp_2018_train.csv/uconn_comp_2018_train.csv", colClasses = "numeric") %>% 
  filter(!fraud == -1)
df_test <- fread("C:/Users/Ashish/Documents/R/uconn_comp_2018_train.csv/uconn_comp_2018_test.csv",
                 colClasses = "numeric")

# keeping and removing few columns

df %>% 
  mutate(gender = as.factor(factor(gender, levels = unique(df$gender))),
         marital_status = as.factor(factor(marital_status, levels = unique(df$marital_status))),
         high_education_ind = as.factor(factor(high_education_ind, levels = unique(df$high_education_ind))),
         address_change_ind = as.factor(factor(address_change_ind, levels = unique(df$address_change_ind))),
         living_status = as.factor(factor(living_status, levels = unique(df$living_status))),
         accident_site = as.factor(factor(accident_site, levels = unique(df$accident_site))),
         witness_present_ind = as.factor(factor(witness_present_ind, levels = unique(df$witness_present_ind))),
         channel = as.factor(factor(channel, levels = unique(df$channel))),
         vehicle_category = as.factor(factor(vehicle_category, levels = unique(df$vehicle_category))),
         vehicle_color = as.factor(factor(vehicle_color, levels = unique(df$vehicle_color)))
         #fraud = as.factor(factor(fraud, levels = unique(df$fraud))),
         #zip_code = as.character(zip_code)
         ) %>% 
  select(-claim_day_of_week) -> df_1

df_test %>% 
  mutate(gender = as.factor(factor(gender, levels = unique(df$gender))),
         marital_status = as.factor(factor(marital_status, levels = unique(df$marital_status))),
         high_education_ind = as.factor(factor(high_education_ind, levels = unique(df$high_education_ind))),
         address_change_ind = as.factor(factor(address_change_ind, levels = unique(df$address_change_ind))),
         living_status = as.factor(factor(living_status, levels = unique(df$living_status))),
         accident_site = as.factor(factor(accident_site, levels = unique(df$accident_site))),
         witness_present_ind = as.factor(factor(witness_present_ind, levels = unique(df$witness_present_ind))),
         channel = as.factor(factor(channel, levels = unique(df$channel))),
         vehicle_category = as.factor(factor(vehicle_category, levels = unique(df$vehicle_category))),
         vehicle_color = as.factor(factor(vehicle_color, levels = unique(df$vehicle_color))),
         zip_code = as.character(zip_code)) %>% 
  select(-claim_day_of_week) -> df_test_1

glimpse(df)

# date conversion

df_1 %>% 
  mutate(claim_date = as.Date(claim_date, format = "%m/%d/%Y")) -> df_2

df_test_1 %>% 
  mutate(claim_date = as.Date(claim_date, format = "%m/%d/%Y")) -> df_test_2

library(corrplot)

cor <- cor(df_2[,sapply(df_2, is.numeric)])
corrplot::corrplot(cor, method = "ellipse", type = "upper")


## Time based features
library(timetk)
df_2 %>% 
  select(claim_date, claim_number) %>% 
  tk_augment_timeseries_signature() -> df_new

df_test_2 %>% 
  select(claim_date, claim_number) %>% 
  tk_augment_timeseries_signature() -> df_test_new

col_keep = c("claim_date","claim_number","year","day","wday","week")

df_new %>% 
  select(one_of(col_keep)) -> df_new

df_test_new %>% 
  select(one_of(col_keep)) -> df_test_new

cor <- cor(df_new[,sapply(df_new, is.numeric)])
corrplot::corrplot(cor, method = "number", type = "upper")

df_comb %>% 
  group_by(month, fraud) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(month), cnt))+
  geom_bar(stat = "identity")+
  facet_wrap(~ fraud)

#cor_cut <- findCorrelation(cor, cutoff = 0.9)
#colnames(df_new)[cor_cut]


df_comb <- df_2 %>% 
  left_join(df_new, by = c("claim_date","claim_number"))

df_test_comb <- df_test_2 %>% 
  left_join(df_test_new, by = c("claim_date","claim_number"))

names(df_comb)

cor <- cor(df_comb[,sapply(df_comb, is.numeric)])
corrplot::corrplot(cor, method = "ellipse", type = "upper")

## ZipCode
library(zipcode)
data("zipcode")

df_comb$zip_code <- as.character(df_comb$zip_code)
df_test_comb$zip_code <- as.character(df_test_comb$zip_code)

glimpse(df_3)

df_comb %>% 
  left_join(zipcode, by = c("zip_code" = "zip")) %>% 
  select(-latitude) %>% 
  select(-longitude)-> df_3

df_test_comb %>% 
  left_join(zipcode, by = c("zip_code" = "zip")) %>% 
  select(-latitude) %>% 
  select(-longitude)-> df_test_3
  

## New features
# NA imputation

df_3$marital_status[is.na(df_3$marital_status)] <- 1
df_3$witness_present_ind[is.na(df_3$witness_present_ind)] <- 0
df_3$age_of_vehicle[is.na(df_3$age_of_vehicle)] <- 5
df_3$claim_est_payout[is.na(df_3$claim_est_payout)] <- 4800

df_3$annual_income[(df_3$annual_income == -1)] <- 37399 # national average
df_3$age_of_driver[(df_3$age_of_driver > 100)] <- 43.7 # national average of age

df_3$city[is.na(df_3$city)] <- "other_c"
df_3$state[is.na(df_3$state)] <- "other_s"


df_test_3$marital_status[is.na(df_test_3$marital_status)] <- 1
df_test_3$witness_present_ind[is.na(df_test_3$witness_present_ind)] <- 0
df_test_3$age_of_vehicle[is.na(df_test_3$age_of_vehicle)] <- 5
df_test_3$claim_est_payout[is.na(df_test_3$claim_est_payout)] <- 4800

df_test_3$annual_income[(df_test_3$annual_income == -1)] <- 37399 # national average
df_test_3$age_of_driver[(df_test_3$age_of_driver > 100)] <- 43.7 # national average of age

df_test_3$city[is.na(df_test_3$city)] <- "other_c"
df_test_3$state[is.na(df_test_3$state)] <- "other_s"

colSums(is.na(df_test_3))


df_3 %>%
  filter(annual_income > 0) %>%
  mutate(veh_deno = (1+(age_of_vehicle*0.2))) %>% 
  mutate(claim_percent = claim_est_payout/vehicle_price,
         income_per_age = annual_income/age_of_driver,
         payout_income = claim_est_payout/annual_income,
         prev_claims_per_age = if_else(age_of_vehicle == 0, 0, (past_num_of_claims/age_of_vehicle)),
         veh_price_agedriv = vehicle_price/age_of_driver,
         veh_price_ageveh = if_else(age_of_vehicle == 0,vehicle_price, vehicle_price/veh_deno),
         safty_per_drage = safty_rating/age_of_driver
         ) -> df_4
colSums(is.na(df_4))

# Age of driver

unique(df_4$age_of_driver)

df_4 %>% 
  ggplot(aes(age_of_driver))+
  geom_density()

df_4 %>% 
  filter(age_of_driver < 101) %>% 
  select(age_of_driver) %>% 
  group_by() %>% 
  summarise(avg = median(age_of_driver)) # 43.7

# income


df_4 %>% 
  ggplot(aes(x=annual_income))+
  geom_density()

df_4 %>% 
  arrange(-annual_income) %>% 
  select(annual_income) %>% 
  top_n(3)

glimpse(df_4)

#test
df_test_3 %>%
  filter(annual_income > 0) %>%
  mutate(veh_deno = (1+(age_of_vehicle*0.2))) %>% 
  mutate(claim_percent = claim_est_payout/vehicle_price,
         income_per_age = annual_income/age_of_driver,
         payout_income = claim_est_payout/annual_income,
         prev_claims_per_age = if_else(age_of_vehicle == 0, 0, (past_num_of_claims/age_of_vehicle)),
         veh_price_agedriv = vehicle_price/age_of_driver,
         veh_price_ageveh = if_else(age_of_vehicle == 0,vehicle_price, vehicle_price/veh_deno),
         safty_per_drage = safty_rating/age_of_driver
  ) -> df_test_4

colSums(is.na(df_4))
# state and city average

df_4 %>% 
  group_by(accident_site,zip_code) %>% 
  mutate(cnt_cl_site_zip = n()) %>% 
  ungroup() %>%
  group_by(channel,zip_code) %>% 
  mutate(cnt_cl_zip_chnl = n()) %>% 
  ungroup() %>% 
  group_by(vehicle_category, zip_code) %>% 
  mutate(cnt_cate_zip = n()) %>% 
  ungroup() -> df_4

#test
df_test_4 %>% 
  group_by(accident_site,zip_code) %>% 
  mutate(cnt_cl_site_zip = n()) %>% 
  ungroup() %>%
  group_by(channel,zip_code) %>% 
  mutate(cnt_cl_zip_chnl = n()) %>% 
  ungroup() %>% 
  group_by(vehicle_category, zip_code) %>% 
  mutate(cnt_cate_zip = n()) %>% 
  ungroup() -> df_test_4

df_4 %>%
  group_by(city, state) %>% 
  mutate(avg_city = mean(annual_income)) %>% 
  group_by(state) %>% 
  mutate(avg_state = mean(annual_income)) %>% 
  mutate(zip_code = as.numeric(zip_code)) %>% 
  select(-claim_date) %>% 
  select(-claim_number) %>% 
  select(-annual_income) %>% 
  ungroup() %>% 
  select(-city) %>% 
  select(-state)-> df_4
  
df_4 %>% 
  select(-vehicle_weight) %>% 
  select(-vehicle_color) %>% 
  select(-week) %>% 
  select(-veh_deno) -> df_4


#test
df_test_4 %>%
  group_by(city, state) %>% 
  mutate(avg_city = mean(annual_income)) %>% 
  group_by(state) %>% 
  mutate(avg_state = mean(annual_income)) %>% 
  mutate(zip_code = as.numeric(zip_code)) %>% 
  select(-claim_date) %>% 
  select(-claim_number) %>% 
  select(-annual_income) %>% 
  ungroup() %>% 
  select(-city) %>% 
  select(-state)-> df_test_4

df_test_4 %>% 
  select(-vehicle_weight) %>% 
  select(-vehicle_color) %>% 
  select(-week) %>% 
  select(-veh_deno) -> df_test_4


#df_4$state <- NULL
# NA imputation
#df_4$city[is.na(df_4$city)] <- "other_c"
#df_4$state[is.na(df_4$state)] <- "other_s"

colSums(is.na(df_4))


################################# Data Partition ################################

rm(df_test)
rm(df_train)
set.seed(1992)

levels(df_4$fraud) <- c("zero","one")

library(caret)
set.seed(123)
index <- createDataPartition(df_4$fraud, p = 0.75, list = FALSE)
df_train <- df_4[index,]
df_test <- df_4[-index,]

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
                        data = df_train,
                        method = "xgbTree",
                        trControl = train_ctrl1,
                        metric = "ROC",
                        tuneGrid = xgbGrid)
stopCluster(cl)

mod_xgb
mod_xgb$bestTune
plot(varImp(mod_xgb))


varImp(mod_xgb, scale = FALSE)$importance %>% 
  mutate(names = row.names(.)) %>% 
  filter(Overall < 0) %>% 
  arrange(-Overall)

pred_xgb_p <- predict(mod_xgb, newdata = df_test[,-20], type = "prob")
pred_xgb_p$status <- pred_xgb
pred_xgb_p$actual <- df_test$fraud
confusionMatrix(df_test$fraud, as.factor(pred_xgb))
# 0.9935
pROC::roc(df_test$fraud,as.numeric(pred_xgb))

# RF

set.seed(11)

train_ctrl2 <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            verboseIter = FALSE,
                            #preProcOptions = c("center","scale"),
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = twoClassSummary,
                            search = "random")

set.seed(1992)
rfGrid <- expand.grid(nrounds = c(210),
                       max_depth = c(4),
                       eta = c(0.0686),
                       gamma = c(9),
                       colsample_bytree = c(0.3715),
                       min_child_weight = c(18),
                       subsample = c(0.3011))
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

set.seed(9)
mod_glm <- caret::train(fraud~.,
                        data = df_train,
                        method = "glm",
                        trControl = train_ctrl2,
                        metric = "ROC",
                        #tuneGrid = rfGrid
                        tuneLength=5)
pred_glm_p <- predict(mod_xgb, newdata = df_test[,-20], type = "prob")

df_train$oof_pred_glm <- mod_glm$pred$pred[order(mod_glm$pred$rowIndex)]


library(caretEnsemble)
control <- trainControl(method="repeatedcv", number=10, repeats=3,
                        savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(1)
models <- caretList(fraud~., data=df_train, 
                    trControl=control,
                    methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

