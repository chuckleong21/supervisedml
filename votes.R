library(tidyverse)
library(parsnip)
library(yardstick)
library(rsample)
library(recipes)

voters <- read_csv("data/voters.csv") %>% 
        janitor::clean_names() %>% 
        select(-case_identifier) %>% 
        mutate(turnout16_2016 = as.factor(turnout16_2016))

# explore data 

voters %>% 
        count(turnout16_2016)

# -----skip----

# split data
set.seed(3346)
vote_split <- initial_split(voters, prop = .8, strata = "turnout16_2016")
vote_train <- training(vote_split)
vote_test <- testing(vote_split)

# upsampling vote_train for dealing with class imbalance
vote_train <- 
        recipe( ~., data = vote_train) %>% 
        step_upsample(turnout16_2016, over_ratio = 1) %>%  # equal size of upsampling
        prep(data = vote_train, retain = TRUE) %>% 
        juice()

count(vote_train, turnout16_2016)

# model set up

spec_model <- 
        list(glm = logistic_reg() %>% set_engine("glm"), 
             rf = rand_forest(mode = "classification") %>% set_engine("randomForest"))
fit_model <- 
        map(spec_model, fit, formula = turnout16_2016 ~., data = vote_train)

# repeatedcv resampling: repeats = 2, nfold = 10
cv_splits <- 
        vfold_cv(vote_train, v = 10, repeats = 2)
cv_fit <- function(split, spec) {
        fit(
                object = spec, 
                formula = turnout16_2016 ~ .,
                data = analysis(split)
        )
}

cv_pred <- function(split, model) {
        assess <- assessment(split)
        bind_cols(assess, predict(model, new_data = assess))
}

perf_metrics <- metric_set(accuracy, sens, spec)

cv_splits <- 
        cv_splits %>% 
        mutate(fit_glm = map(splits, cv_fit, spec = spec_model$glm),
               fit_rf = map(splits, cv_fit, spec = spec_model$rf),
               pred_glm = map2(splits, fit_glm, cv_pred), 
               pred_rf = map2(splits, fit_rf, cv_pred), 
               perf_glm = map(pred_glm, perf_metrics, truth = "turnout16_2016", estimate = .pred_class), 
               perf_rf = map(pred_rf, perf_metrics, truth = "turnout16_2016", estimate = .pred_class))

# model performance
test_pred <- 
        vote_test %>% 
        mutate(pred_glm = unlist(predict(fit_model$glm, vote_test)), 
               pred_rf  = unlist(predict(fit_model$rf, vote_test)))
test_pred %>% 
        conf_mat(truth = turnout16_2016, estimate = pred_glm)
test_pred %>% 
        conf_mat(truth = turnout16_2016, estimate = pred_rf)
perf_metrics(test_pred, truth = turnout16_2016, estimate = pred_glm)
perf_metrics(test_pred, truth = turnout16_2016, estimate = pred_rf)

cv_splits %>% 
        unnest(pred_glm) %>% 
        conf_mat(truth = turnout16_2016, estimate = .pred_class)
cv_splits %>% 
        unnest(pred_rf) %>% 
        conf_mat(truth = turnout16_2016, estimate = .pred_class)
