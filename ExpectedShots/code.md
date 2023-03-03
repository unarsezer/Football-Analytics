## Packages and Function
```r
library(tidyverse)
library(Metrics)
library(xgboost)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
```

## Data
I attached the data I will use in the data folder.
```r
data <- read.csv("model_data.csv", stringsAsFactors = F)
```

## Data Preparation For Linear Regression
```r
data <- read.csv("model_data.csv", stringsAsFactors = F)

pre_train_test <- data %>%
  mutate_at(c(5:105), range01) 

training <- pre_train_test %>%
  filter(Comp %in% c("Premier League", "Bundesliga")) %>%
  mutate(id = paste(Team, Date, sep = "_"), .before = "Team") %>%
  select(!c(Team, Date, Comp)) %>%
  column_to_rownames(var = "id") 

training_x <- training %>% select(!Sh_Standard_for)
training_y <- training$Sh_Standard_for

testing <- pre_train_test %>%
  filter(Comp == "La Liga") %>%
  mutate(id = paste(Team, Date, sep = "_"), .before = "Team") %>%
  select(!c(Team, Date, Comp)) %>%
  column_to_rownames(var = "id")

testing_x <- testing %>% select(!Sh_Standard_for)
testing_y <- testing$Sh_Standard_for
```

## Linear Regression Model
```r
first_lm_model <- lm(Sh_Standard_for ~ .,  data = training)
summary(first_lm_model)

# I remove the features that have higher pvalues

lm_model <- lm(Sh_Standard_for ~ Sh_Blocks_for + Pass_Blocks_for + Int_for + Clr_for + Fld_Performance_for + Off_Performance_for +
                 Recov_Performance_for + TotDist_Total_for + PrgDist_Total_for + Cmp_Short_for + Cmp_Medium_for + Cmp_Long_for + PPA_for +
                 CrsPA_for + Live_Pass_Types_for + Dead_Pass_Types_for + FK_Pass_Types_for + TB_Pass_Types_for +
                 Crs_Pass_Types_for + CK_Pass_Types_for + Def.3rd_Touches_for +
                 Mid.3rd_Touches_for + Att.3rd_Touches_for + Att.Pen_Touches_for + TotDist_Carries_for + PrgC_Carries_for +
                 CPA_Carries_for + Mis_Carries_for + Rec_Receiving_for + Def.3rd_Tackles_aga + Pass_Blocks_aga +
                 Clr_aga + Thr_Passes_aga + AvgLen_Passes_aga + AvgLen_Goal_Kicks_aga + Recov_Performance_aga + 
                 PrgDist_Total_aga + PPA_aga + Dead_Pass_Types_aga +
                 FK_Pass_Types_aga + TI_Pass_Types_aga + CK_Pass_Types_aga + Def.Pen_Touches_aga + Att.Pen_Touches_aga + 
                 Live_Touches_aga + PrgDist_Carries_aga + Final_Third_Carries_aga + Rec_Receiving_aga +
                 Att.Pen_Touches_for*PrgP_for + PrgDist_Carries_for*TotDist_Carries_for, data = training)
summary(lm_model)
# The model can be strengthened by using the interactions between variables.

mse(testing_y, predict(lm_model , testing_x))
# 6.956519
rmse(testing_y, predict(lm_model , testing_x)) 
# 2.637521
```
## Data Preparation For xGBoost
```r
training <- data %>%
  filter(Comp %in% c("Premier League", "Bundesliga")) %>%
  mutate(id = paste(Team, Date, sep = "_"), .before = "Team") %>%
  select(!c(Team, Date, Comp)) %>%
  column_to_rownames(var = "id") 

training_x <- training %>% select(!Sh_Standard_for)
training_y <- training$Sh_Standard_for

testing <- data %>%
  filter(Comp == "La Liga") %>%
  mutate(id = paste(Team, Date, sep = "_"), .before = "Team") %>%
  select(!c(Team, Date, Comp)) %>%
  column_to_rownames(var = "id")

testing_x <- testing %>% select(!Sh_Standard_for)
testing_y <- testing$Sh_Standard_for

dtrain <- xgb.DMatrix(data = as.matrix(training_x), label = training_y)
dtest <- xgb.DMatrix(data = as.matrix(testing_x), label = testing_y)

watchlist = list(train=dtrain, test=dtest)
```

## xGBOOST Model
```r
xgboost_model <- xgb.train(data = dtrain, max.depth = 4, watchlist=watchlist, nrounds = 941, eta = 0.03)

importance_matrix = xgb.importance(colnames(dtrain), model = xgboost_model)

xgb.plot.importance(importance_matrix[1:20,])

selected_features <- importance_matrix[1:20, 1]

dtrain <- xgb.DMatrix(data = as.matrix(training_x[,selected_features$Feature]), label = training_y)
dtest <- xgb.DMatrix(data = as.matrix(testing_x[,selected_features$Feature]), label = testing_y)

watchlist = list(train=dtrain, test=dtest)

slcted_xgboost_model <- xgb.train(data = dtrain, max.depth = 2, watchlist=watchlist, nrounds = 1614, eta = 0.03)
slcted_xgboost_model$evaluation_log %>% arrange(test_rmse)
#2.894915	
```
