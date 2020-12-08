###################
## XGBoost Model ##
###################

## Libraries
library(tidyverse)
library(caret)
library(xgboost)

## Read in data
fake_news <- read_csv("./CleanFakeNews.csv")

## Change response into factor
fake_news$isFake <- as.factor(fake_news$isFake)

## Split into train and test
fn_train <- fake_news %>% filter(!is.na(isFake))
fn_test <- fake_news %>% filter(is.na(isFake))

## Model
tc <- trainControl(method = "cv", number = 10)

tg <- expand.grid(nrounds = 250,
                  max_depth = 6,
                  eta = c(.35),
                  gamma = 0,
                  subsample = 1,
                  colsample_bytree = 1,
                  min_child_weight = 1) 

fn_xgb_model <- train(form = isFake ~.,
                      data = fn_train %>% select(-Id, -Set),
                      method = "xgbTree",
                      trControl = tc,
                      tuneGrid = tg,
                      tuneLength = 5)  
beepr::beep(10)
fn_xgb_model$results

preds2 <- predict(fn_xgb_model, newdata = fn_test)
predsdf2 <- data.frame(id=fn_test$Id, label=preds2)
write.csv(x = predsdf2, file = "./FakeNewsPreds2.csv", row.names = FALSE)


