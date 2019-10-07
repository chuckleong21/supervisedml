library(tidyverse)
library(janitor)
library(patchwork)
library(magrittr)
cars <- read_csv("data/cars2018.csv") %>% 
        clean_names() %>% 
        select(-model, -model_index)


p1 <- DataExplorer::plot_bar(cars, ncol = 4)[[1]]
p2 <- DataExplorer::plot_histogram(cars, ncol = 2)[[1]]
p1 + p2 + plot_layout(ncol = 1)

library(rsample)
set.seed(34221)
cars_split <- initial_split(cars, prop = .75)
cars_train <- training(cars_split)
cars_test <- testing(cars_split)

library(parsnip)
spec_lm <- 
        linear_reg() %>% 
        set_engine("lm")
spec_rand_forest <- 
        rand_forest(mode = "regression") %>% 
        set_engine("randomForest")
fit_lm <- 
        spec_lm %>% 
        fit(log(mpg) ~ ., data = cars_train)
fit_rand_forest <- 
        spec_rand_forest %>% 
        fit(log(mpg) ~ ., data = cars_train)

train_pred <- 
        cars_train %>% 
        mutate(mpg.log = log(mpg), 
               mpg.lm.pred  = unlist(predict(fit_lm, cars_train)), 
               mpg.forest.pred = unlist(predict(fit_rand_forest, cars_train))) %>% 
        select(contains("mpg."))

test_pred <- 
        cars_test %>% 
        mutate(mpg.log = log(mpg), 
               mpg.lm.test = unlist(predict(fit_lm, cars_test)), 
               mpg.forest.test = unlist(predict(fit_rand_forest, cars_test))) %>% 
        select(contains("mpg."))

library(yardstick)
metrics(train_pred, truth = mpg.log, estimate = mpg.lm.pred)
metrics(train_pred, truth = mpg.log, estimate = mpg.forest.pred)
metrics(test_pred, truth = mpg.log, estimate = mpg.lm.test)
metrics(test_pred, truth = mpg.log, estimate = mpg.forest.test)

# bootstrap sampling
boot_splits <- bootstraps(cars_train)
boot_splits$splits[[1]] %>% analysis() %>% dim()
boot_splits$splits[[1]] %>% assessment() %>% dim()

# fit model
fit_model <- function(split, spec) {
        fit(
                object = spec, 
                formula = log(mpg) ~ ., 
                data = analysis(split)
        )
}
pred_model <- function(split, model) {
        assess <- assessment(split) %>% mutate(mpg_log = log(mpg))
        assess <- bind_cols(assess, predict(model, new_data = assess))
}

perf_metrics <- metric_set(rmse, rsq, mae)


boot_splits %<>% 
        mutate(model_lm = map(splits, fit_model, spec_lm), 
               model_rf = map(splits, fit_model, spec_rand_forest),
               pred_lm = map2(splits, model_lm, pred_model), 
               pred_rf = map2(splits, model_rf, pred_model), 
               perf_lm = map(pred_lm, perf_metrics, truth = mpg_log, estimate = .pred), 
               perf_rf = map(pred_rf, perf_metrics, truth = mpg_log, estimate = .pred))

# with resampling - lm
boot_splits %>% 
        select(id, starts_with("perf")) %>% 
        unnest(perf_lm) %>% 
        group_by(.metric) %>% 
        summarize(.avg = mean(.estimate), .sd = sd(.estimate))

# no resampling
metrics(test_pred, truth = mpg.log, estimate = mpg.lm.test)

# with resampling - rf
boot_splits %>% 
        select(id, starts_with("perf")) %>% 
        unnest(perf_rf) %>% 
        group_by(.metric) %>% 
        summarize(.avg = mean(.estimate), .sd = sd(.estimate))

metrics(test_pred, truth = mpg.log, estimate = mpg.forest.test)

test_pred %>% 
        pivot_longer(c(mpg.lm.test, mpg.forest.test), "method", values_to = "result") %>% 
        ggplot(aes(x = mpg.log, y = result, color = method)) + 
        geom_point(size = 1.5, alpha = .5) + 
        facet_wrap(~method) +
        geom_abline(lty = 2, color = "grey40") + 
        geom_smooth(method = "lm")
