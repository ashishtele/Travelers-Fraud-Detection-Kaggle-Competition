
rm(list = ls())
gc()

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
  library(h2oEnsemble)
}

load_lb()

auto_policies_17 <- fread("C:/Users/Ashish/Documents/R/Zurich/auto_policies_2017.csv", 
            colClasses = "numeric") 

auto_cust_18 <- fread("C:/Users/Ashish/Documents/R/Zurich/auto_potential_customers_2018.csv", 
                          colClasses = "numeric") 

# count validation

auto_policies_17 %>% 
  filter(numclaims>0) %>% 
  count()
## 10030: at least one claim

glimpse(auto_policies_17)

glimpse(auto_cust_18)

# date conversion

auto_policies_17 %>% 
  mutate(pol_eff_dt = as.Date(pol_eff_dt, format = "%m/%d/%Y"),
         date_of_birth = as.Date(date_of_birth, format = "%m/%d/%Y"),
         claim_YN = if_else(claimcst0 > 0, 1, 0)) -> auto_policies_17

auto_cust_18 %>% 
  mutate(date_of_birth = as.Date(date_of_birth, format = "%m/%d/%Y")) -> auto_cust_18

# Policy Number

auto_policies_17 %>% 
  group_by(pol_number) %>% 
  summarise(cnt = n()) %>% 
  filter(cnt > 1) 
## No duplicate

# Policy date distribution

auto_policies_17 %>% 
  mutate(pol_month = month(pol_eff_dt)) %>% 
  group_by(pol_month) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(pol_month), cnt))+
  geom_bar(stat = "identity")

  
# Gender split

auto_policies_17 %>% 
  group_by(gender) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(gender), cnt, fill = gender))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)+
  labs(x = "Gender", y = "Count") +
  ggtitle( "Policy Distribution - 2017 ")
## More Female in overall policies taken out in 2017

auto_policies_17 %>% 
  filter(claim_YN == 1) %>% 
  group_by(gender) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(gender), cnt, fill = gender))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)+
  labs(x = "Gender", y = "Count") +
  ggtitle( "Claim Distribution - 2017 ")
## More male in overall claimed policies


glimpse(auto_policies_17)
colSums(is.na(auto_policies_17))
  
# Age cat

auto_cust_18 %>% 
  group_by(agecat) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(agecat), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)

auto_policies_17 %>% 
  group_by(agecat) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(agecat), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)

auto_policies_17 %>%
  filter(claim_YN == 1) %>%  
  group_by(agecat) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(agecat), cnt, fill = "red"))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)+
  labs(x = "AgeCat", y = "Count") +
  ggtitle( "# Claim Distribution by Agecat-2017 ")
## Younger age category dominates the claimed policies  

auto_policies_17 %>%
  filter(claim_YN == 1) %>%  
  group_by(agecat, gender) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(agecat), cnt, fill = gender))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(aes(label = cnt), vjust = -0.2)

glimpse(auto_policies_17)
glimpse(auto_cust_18)


### Credit score
auto_1 %>% 
  ggplot(aes(credit_score, fill = "red"))+
  geom_density()+
  facet_wrap(~ claim_YN)


library(corrplot)

cor <- cor(auto_policies_17[,sapply(auto_policies_17, is.numeric)])
corrplot::corrplot(cor, method = "ellipse", type = "upper")


# area

auto_cust_18 %>% 
  group_by(area) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(area), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)

auto_policies_17 %>% 
  group_by(area) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(area), cnt, fill = "red"))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)+
  labs(x = "Area", y = "Count") +
  ggtitle( "# Policy Distribution by Area-2017")

auto_policies_17 %>%
  filter(claim_YN == 1) %>%  
  group_by(area) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(area), cnt, fill="red"))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)+
  labs(x = "Area", y = "Count") +
  ggtitle( "# Claim Distribution by Area-2017")

# traffic_index

auto_cust_18 %>% 
  ggplot(aes(traffic_index))+
  geom_density()

auto_policies_17 %>% 
  ggplot(aes(traffic_index))+
  geom_density()+
  facet_wrap(~ claim_YN)

glimpse(auto_policies_17)

## vehicle age

auto_cust_18 %>% 
  group_by(veh_age) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(veh_age), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)


auto_policies_17 %>% 
  group_by(veh_age) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(veh_age), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)

auto_policies_17 %>% 
  filter(claim_YN == 1) %>%
  group_by(veh_age) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(veh_age), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)


## vehicle body

auto_cust_18 %>% 
  group_by(veh_body) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(veh_body), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)


auto_policies_17 %>% 
  group_by(veh_body) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(veh_body), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)

auto_policies_17 %>% 
  filter(claim_YN == 1) %>%
  group_by(veh_body) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(veh_body), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)

# vehicle value
auto_policies_17 %>% 
  ggplot(aes(log(veh_value+1)))+
  geom_density()+
  facet_wrap(~ claim_YN)

auto_cust_18 %>% 
  ggplot(aes(log(veh_value+1)))+
  geom_density()


## vehicle body

auto_policies_17 %>% 
  group_by(claim_office) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(claim_office), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)

auto_policies_17 %>% 
  filter(claim_YN == 1) %>%
  group_by(claim_office) %>% 
  summarise(cnt = n()) %>% 
  ggplot(aes(as.factor(claim_office), cnt))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = cnt), vjust = -0.2)

####################################### Preprocessing #########@###########################

table(auto_policies_17$claim_YN)
# almost 20% claims

colSums(is.na(auto_policies_17))
colSums(is.na(auto_cust_18))


auto_policies_17 %>% 
  mutate(dob_year = year(date_of_birth),
         dob_month = month(date_of_birth),
         agecat_impute = case_when(dob_year < 1950 ~ 6,
                                   dob_year < 1960 & dob_year >= 1950 ~ 5,
                                   dob_year < 1970 & dob_year >= 1960 ~ 4,
                                   dob_year < 1980 & dob_year >= 1970 ~ 3,
                                   dob_year < 1990 & dob_year >= 1980 ~ 2,
                                   dob_year < 2000 & dob_year >= 1990 ~ 1),
         agecat_miss = as.factor(if_else(is.na(agecat),1,0)),
         claim_office = if_else(claim_office == "", "no_claim", claim_office),
         traffic_ind_miss = as.factor(if_else(is.na(traffic_index),1,0)),
         credit_miss = as.factor(if_else(is.na(credit_score),1,0))) -> auto_1

# 2018 customers
auto_cust_18 %>% 
  mutate(dob_year = year(date_of_birth),
         dob_month = month(date_of_birth),
         agecat_impute = case_when(dob_year < 1950 ~ 6,
                                   dob_year < 1960 & dob_year >= 1950 ~ 5,
                                   dob_year < 1970 & dob_year >= 1960 ~ 4,
                                   dob_year < 1980 & dob_year >= 1970 ~ 3,
                                   dob_year < 1990 & dob_year >= 1980 ~ 2,
                                   dob_year < 2000 & dob_year >= 1990 ~ 1),
         agecat_miss = as.factor(if_else(is.na(agecat),1,0)),
         traffic_ind_miss = as.factor(if_else(is.na(traffic_index),1,0)),
         credit_miss = as.factor(if_else(is.na(credit_score),1,0))) -> auto_test_1

# data for cost per claim

per_claim_train <- auto_1
per_claim_test <- auto_test_1

rem_col <- c("annual_premium","claimcst0","numclaims","pol_number","pol_eff_dt","date_of_birth",
             "claim_office","agecat","agecat_impute")

auto_1 %>% 
  select(-one_of(rem_col)) -> auto_1

glimpse(auto_cust_18)

rem_col_test <- c("quote_number","date_of_birth","agecat","agecat_impute")

auto_test_1 %>% 
  select(-one_of(rem_col_test)) -> auto_test_1

cor <- cor(auto_1[,sapply(auto_1, is.numeric)])
corrplot::corrplot(cor, method = "number", type = "upper")

glimpse(auto_1)

##################################### data partition ######################################

ind <- createDataPartition(auto_1$claim_YN, p = 0.7, list = FALSE)
auto_train <- auto_1[ind,]
auto_test <- auto_1[-ind,]

# recipe object
recipe_auto <- recipe(claim_YN ~ ., data = auto_train) %>%
  step_meanimpute(credit_score) %>% 
  step_meanimpute(traffic_index) %>%
  step_other(veh_body, threshold = 0.05, other = "other") %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  prep(data = auto_train)

recipe_auto

x_train_auto <- bake(recipe_auto, newdata = auto_train)
x_test_auto <- bake(recipe_auto, newdata = auto_test)

x_train_auto$claim_YN<- as.factor(x_train_auto$claim_YN)
x_test_auto$claim_YN <- as.factor(x_test_auto$claim_YN)

levels(x_train_auto$claim_YN) <- c("zero","one")
levels(x_test_auto$claim_YN) <- c("zero","one")

# Model building

set.seed(10)
tr_ctrl <- trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 3,
                            verboseIter = FALSE,
                            #preProcOptions = c("center","scale"),
                            classProbs = TRUE,
                            savePredictions = TRUE,
                            summaryFunction = twoClassSummary,
                            search = "random",
                        sampling = "down")

set.seed(1992)
xgbGrd <- expand.grid(nrounds = c(135),
                       max_depth = c(2),
                       eta = c(0.3008773),
                       gamma = c(8.992996),
                       colsample_bytree = c(0.6651651),
                       min_child_weight = c(4),
                       subsample = c(0.9086881))


# XGB
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

#rm(mod_xgb)
set.seed(9)
model_xgb <- caret::train(claim_YN ~.,
                        data = x_train_auto,
                        method = "xgbTree",
                        trControl = tr_ctrl,
                        metric = "ROC",
                        tuneGrid = xgbGrd
                        #tuneLength = 5
                        )
stopCluster(cl)

model_xgb
model_xgb$bestTune
plot(varImp(model_xgb))


glimpse(x_test_auto)
xgb_pred <- predict(model_xgb, newdata = x_test_auto[,-5])

confusionMatrix(x_test_auto$claim_YN, xgb_pred)

table(x_test_auto$claim_YN)

# unbalanced

set.seed(10)
tr_ctrl_2 <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        verboseIter = FALSE,
                        #preProcOptions = c("center","scale"),
                        classProbs = TRUE,
                        savePredictions = TRUE,
                        summaryFunction = twoClassSummary,
                        search = "random")

set.seed(1992)
xgbGrd <- expand.grid(nrounds = c(135),
                      max_depth = c(2),
                      eta = c(0.3008773),
                      gamma = c(8.992996),
                      colsample_bytree = c(0.6651651),
                      min_child_weight = c(4),
                      subsample = c(0.9086881))


# XGB
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

#rm(mod_xgb)
set.seed(9)
model_xgb_simple <- caret::train(claim_YN ~.,
                          data = x_train_auto,
                          method = "xgbTree",
                          trControl = tr_ctrl_2,
                          metric = "ROC",
                          tuneGrid = xgbGrd)
stopCluster(cl)

model_xgb_simple
model_xgb_simple$bestTune
plot(varImp(model_xgb_simple))


glimpse(x_test_auto)
xgb_pred_simple <- predict(model_xgb_simple, newdata = x_test_auto[,-5])

confusionMatrix(x_test_auto$claim_YN, xgb_pred_simple)



Ensemble_table <- data_frame(xgb_smote = xgb_pred_smote,
                             xgb_down = xgb_pred,
                             xgb_simple = xgb_pred_simple)

Ensemble_table$vote <- (if_else(Ensemble_table$xgb_smote == 'zero' & Ensemble_table$xgb_down == 'zero' & Ensemble_table$xgb_simple == 'zero','zero',
                                if_else(Ensemble_table$xgb_smote == 'zero' & Ensemble_table$xgb_down == 'zero','zero',
                                        if_else(Ensemble_table$xgb_down == 'zero' & Ensemble_table$xgb_simple == 'zero','zero',
                                                if_else(Ensemble_table$xgb_smote == 'zero' & Ensemble_table$xgb_simple == 'zero','zero','one')))))

Ensemble_table$actual <- x_test_auto$claim_YN
confusionMatrix(Ensemble_table$actual, as.factor(Ensemble_table$vote))


plot(model_xgb_simple$finalModel)


### xgboost

library(DiagrammeR)
library(xgboost)
xgb.plot.tree(model = model_xgb_smote$finalModel, trees = 0:1, show_node_id = TRUE)

xgb.plot.tree(model = model_xgb_simple$finalModel, trees = 0:1, show_node_id = TRUE)

xgb.plot.tree(model = model_xgb$finalModel, trees = 0:1, show_node_id = TRUE)

############################## removing not so important features #########################

glimpse(x_train_auto)

x_train_auto %>% 
  select(-starts_with("veh_body_")) %>% 
  select(-starts_with("pol_month")) -> x_train_auto_1

x_test_auto %>% 
  select(-starts_with("veh_body_")) %>%
  select(-starts_with("pol_month")) -> x_test_auto_1


################################# 5 Split of non-claim ######################################

x_train_auto_1 %>% 
  filter(claim_YN == "zero") -> No_claim_data

x_train_auto_1 %>% 
  filter(claim_YN != "zero") -> claim_data
  
n <- 7050
nr <- nrow(No_claim_data)
splits <- split(No_claim_data, rep(1:ceiling(nr/n), each=n, length.out=nr))

# 5 datasets
D1 <- splits[[1]]
D1 <- rbind(claim_data, D1)

D2 <- splits[[2]]
D2 <- rbind(claim_data, D2)

D3 <- splits[[3]]
D3 <- rbind(claim_data, D3)

D4 <- splits[[4]]
D4 <- rbind(claim_data, D4)

D5 <- splits[[5]]
D5 <- rbind(claim_data, D5)


set.seed(10)
tr_ctrl_2 <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          verboseIter = FALSE,
                          #preProcOptions = c("center","scale"),
                          classProbs = TRUE,
                          savePredictions = TRUE,
                          summaryFunction = twoClassSummary,
                          search = "random")

set.seed(1992)
xgbGrd <- expand.grid(nrounds = c(135),
                      max_depth = c(2),
                      eta = c(0.3008773),
                      gamma = c(8.992996),
                      colsample_bytree = c(0.6651651),
                      min_child_weight = c(4),
                      subsample = c(0.9086881))


# XGB - 5 step dataset
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

set.seed(9)
model_xgb_1 <- caret::train(claim_YN ~.,
                                data = D1,
                                method = "xgbTree",
                                trControl = tr_ctrl_2,
                                metric = "ROC",
                                tuneGrid = expand.grid(nrounds = c(618),
                                                         max_depth = c(1),
                                                         eta = c(0.04785529),
                                                         gamma = c(2.4445881),
                                                         colsample_bytree = c(0.3090644),
                                                         min_child_weight = c(17),
                                                         subsample = c(0.254559)))

model_xgb_2 <- caret::train(claim_YN ~.,
                            data = D2,
                            method = "xgbTree",
                            trControl = tr_ctrl_2,
                            metric = "ROC",
                            tuneGrid = expand.grid(nrounds = c(29),
                                                   max_depth = c(6),
                                                   eta = c(0.35896666),
                                                   gamma = c(6.843337),
                                                   colsample_bytree = c(0.5072391),
                                                   min_child_weight = c(20),
                                                   subsample = c(0.8329766)))

model_xgb_3 <- caret::train(claim_YN ~.,
                            data = D3,
                            method = "xgbTree",
                            trControl = tr_ctrl_2,
                            metric = "ROC",
                            tuneGrid = expand.grid(nrounds = c(488),
                                                   max_depth = c(1),
                                                   eta = c(0.2054982),
                                                   gamma = c(7.822292),
                                                   colsample_bytree = c(0.5116895),
                                                   min_child_weight = c(3),
                                                   subsample = c(0.6071467)))

model_xgb_4 <- caret::train(claim_YN ~.,
                            data = D4,
                            method = "xgbTree",
                            trControl = tr_ctrl_2,
                            metric = "ROC",
                            tuneGrid = expand.grid(nrounds = c(706),
                                                   max_depth = c(4),
                                                   eta = c(0.01218506),
                                                   gamma = c(3.151826),
                                                   colsample_bytree = c(0.3235520),
                                                   min_child_weight = c(12),
                                                   subsample = c(0.5325095)))

model_xgb_5 <- caret::train(claim_YN ~.,
                            data = D5,
                            method = "xgbTree",
                            trControl = tr_ctrl_2,
                            metric = "ROC",
                            tuneGrid = expand.grid(nrounds = c(296),
                                                   max_depth = c(5),
                                                   eta = c(0.3255431),
                                                   gamma = c(3.44476007),
                                                   colsample_bytree = c(0.6093902),
                                                   min_child_weight = c(15),
                                                   subsample = c(0.9644520)))

model_xgb_full <- caret::train(claim_YN ~.,
                            data = x_train_auto_1,
                            method = "xgbTree",
                            trControl = tr_ctrl_2,
                            metric = "ROC",
                            tuneGrid = xgbGrd)

stopCluster(cl)

model_xgb_1
model_xgb_2
model_xgb_3
model_xgb_4
model_xgb_5
model_xgb_full
plot(varImp(model_xgb_1))


xgb_pred_1 <- predict(model_xgb_1, newdata = x_test_auto_1, type = "prob")
xgb_pred_2 <- predict(model_xgb_2, newdata = x_test_auto_1, type = "prob")
xgb_pred_3 <- predict(model_xgb_3, newdata = x_test_auto_1, type = "prob")
xgb_pred_4 <- predict(model_xgb_4, newdata = x_test_auto_1, type = "prob")
xgb_pred_5 <- predict(model_xgb_5, newdata = x_test_auto_1, type = "prob")
xgb_pred_full <- predict(model_xgb_full, newdata = x_test_auto_1, type = "prob")

table_5_ensemble <- data_frame(pred1 = xgb_pred_1$one,
                               pred2 = xgb_pred_2$one,
                               pred3 = xgb_pred_3$one,
                               pred4 = xgb_pred_4$one,
                               pred5 = xgb_pred_5$one,
                               pred_full = xgb_pred_full$one,
                               avg = (xgb_pred_1$one + xgb_pred_2$one + xgb_pred_3$one + xgb_pred_4$one + xgb_pred_5$one + xgb_pred_full$one)/6,
                               class = x_test_auto_1$claim_YN)


pROC::auc(table_5_ensemble$class, table_5_ensemble$avg)

write.table(table_5_ensemble,"C:/Users/Ashish/Documents/R/Zurich/Ensemble.csv",
            sep ="|", row.names = FALSE)
###############################################################################################
# XGB
cores<-detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

#rm(mod_xgb)
set.seed(9)
model_xgb_s_few <- caret::train(claim_YN ~.,
                                 data = x_train_auto_1,
                                 method = "xgbTree",
                                 trControl = tr_ctrl_2,
                                 metric = "ROC",
                                 tuneGrid = xgbGrd)

model_xgb_s_down <- caret::train(claim_YN ~.,
                                data = x_train_auto_1,
                                method = "xgbTree",
                                trControl = tr_ctrl,
                                metric = "ROC",
                                tuneGrid = xgbGrd)


model_glm <- caret::train(claim_YN ~.,
                                 data = x_train_auto_1,
                                 method = "glm",
                                 trControl = tr_ctrl_2,
                                 metric = "ROC")

stopCluster(cl)

model_xgb_s_few
model_xgb_s_few$bestTune
plot(varImp(model_xgb_s_few))

model_xgb_s_down
model_xgb_s_down$bestTune
plot(varImp(model_xgb_s_down))

plot(varImp(model_glm))


glimpse(x_test_auto)
xgb_pred_s_few_P <- predict(model_xgb_s_few, newdata = x_test_auto_1, type = "prob")
xgb_pred_s_down_P <- predict(model_xgb_s_down, newdata = x_test_auto_1, type = "prob")
glm_pred <- predict(model_glm, newdata = x_test_auto_1, type = "prob")

confusionMatrix(x_test_auto_1$claim_YN, xgb_pred_s_down)

prob_table <- data_frame(quote_number = auto_cust_18$quote_number,
                         #simple_p = xgb_pred_s_few_P$one,
                         #down_p = xgb_pred_s_down_P$one,
                         #glm_p = glm_pred$one,
                         avg = (xgb_pred_s_few_P$one+xgb_pred_s_down_P$one)/2)

pROC::auc(prob_table$class, prob_table$avg)


xgb.plot.tree(model = model_xgb_s_few$finalModel, trees = 0:1, show_node_id = TRUE)

xgb.plot.tree(model = model_xgb_s_down$finalModel, trees = 0:1, show_node_id = TRUE)

local_obs <- x_train_auto_1[1:5,]


# Model Agnostic

library(lime)

expl_lime <- lime(x_train_auto_1, model_xgb_s_few, bin_continuous = TRUE)
summary(expl_lime)

explanation_caret <- explain(
  x = local_obs, 
  explainer = expl_lime, 
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = .75,
  n_features = 5, 
  feature_select = "highest_weights",
  labels = "one"
)

plot_features(explanation_caret)
plot_explanations(explanation_caret)

write.csv(prob_table,"C:/Users/Ashish/Documents/R/Zurich/output.csv" )


