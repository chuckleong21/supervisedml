library(tidyverse)
library(parsnip)
library(yardstick)
library(rsample)
library(recipes)

stackoverflow <- read_csv("data/stackoverflow.csv")

stack_split <- 
        stackoverflow %>% 
        janitor::clean_names() %>% 
        select(-respondent) %>% 
        mutate(remote = factor(remote, levels = c("Remote", "Not remote")))


# explore data
theme_set(ggthemes::theme_gdocs())

stack_split %>% 
        select(remote, company_size_number) %>% 
        filter(remote == "Remote") %>% 
        mutate(company_size_number = ifelse(company_size_number < 100, 
                                            "< 100", company_size_number)) %>% 
        group_by(company_size_number) %>% 
        count() %>% 
        ungroup() %>% 
        mutate(prop = n / sum(n)) %>% 
        ggplot(aes(reorder(company_size_number, -prop), prop)) + 
        geom_bar(stat = "identity", fill = "#7c0070") + 
        coord_flip() + 
        scale_y_continuous(name = "% of respondents working remotely", labels = scales::percent) +
        labs(x = "Company Size") +
        theme(panel.grid.major.y = element_blank())

stack_split %>% 
        select(remote, years_coded_job) %>% 
        ggplot(aes(remote, years_coded_job)) + 
        geom_boxplot() + 
        labs(x = NULL, y = "Professional Experience") + 
        theme(panel.grid.major.y = element_blank())

# class imbalance
stack_split %>% count(remote)
# remote         n
# <chr>      <int>
# 1 Not remote  6273
# 2 Remote       718

# split data set
set.seed(7554)
stack_split <- initial_split(stack_split, prop = .8, strata = "remote")
stack_train <- training(stack_split)
stack_test <- testing(stack_split)
count(stack_train, remote) # class imbalance persists
# remote         n
# <fct>      <int>
# 1 Remote       567
# 2 Not remote  5026

# upsampling
up_train <- 
        recipe( ~., data = stack_train) %>% 
        step_upsample(remote, over_ratio = 1) %>% 
        prep(data = stack_train, retain = TRUE) %>% # retain the upsample-version of data
        juice()

count(up_train, remote)
# Remote         n
# <fct>      <int>
# 1 Remote      5026
# 2 Not remote  5026

spec_glm <-
        logistic_reg() %>%
        set_engine("glm")
spec_rf <-
        rand_forest(mode = "classification") %>%
        set_engine("randomForest")

parsnip_fit <- map(list(spec_glm, spec_rf), fit, formula = remote ~., data = up_train) %>% 
        set_names(c("glm", "rf"))

perf_metrics <- metric_set(accuracy, ppv, npv)

# training set
train_pred <- 
        up_train %>% 
        mutate(glm.pred = unlist(predict(parsnip_fit$glm, up_train)), 
               rf.pred = unlist(predict(parsnip_fit$rf, up_train))) %>% 
        select(remote, glm.pred, rf.pred)

conf_mat(train_pred, truth = remote, estimate = glm.pred)
perf_metrics(train_pred, truth = remote, estimate = glm.pred)
conf_mat(train_pred, truth = remote, estimate = rf.pred)
perf_metrics(train_pred, truth = remote, estimate = rf.pred)

# testing set
test_pred <- 
        stack_test %>% 
        mutate(glm.pred = unlist(predict(parsnip_fit$glm, new_data = stack_test)), 
               rf.pred = unlist(predict(parsnip_fit$rf, new_data = stack_test))) %>% 
        select(remote, glm.pred, rf.pred)
conf_mat(test_pred, truth = remote, estimate = glm.pred)
perf_metrics(test_pred, truth = remote, estimate = glm.pred)
conf_mat(test_pred, truth = remote, estimate = rf.pred)
perf_metrics(test_pred, truth = remote, estimate = rf.pred)


# bootstrap training
boot_splits <- bootstraps(data = up_train)

fit_model <- function(split, spec) {
        fit(
                object = spec, 
                formula = remote ~., 
                data = analysis(split)
        )
}

pred_model <- function(split, model) {
        assess <- assessment(split)
        bind_cols(assess, predict(model, new_data = assess))
}

perf_metrics <- metric_set(accuracy, ppv, npv)

boot_splits <- # gonna take a while...
        boot_splits %>% 
        mutate(fit_glm  = map(splits, fit_model, spec = spec_glm),
               fit_rf   = map(splits, fit_model, spec = spec_rf), 
               pred_glm = map2(splits, fit_glm, pred_model), 
               pred_rf  = map2(splits, fit_rf, pred_model), 
               perf_glm = map(pred_glm, perf_metrics, truth = "remote", estimate = .pred_class),
               perf_rf  = map(pred_rf, perf_metrics, truth = "remote", estimate = .pred_class)
               )

# visualising performance
resampling <- 
        boot_splits %>% 
        select(id, perf_glm) %>% 
        unnest(perf_glm) %>% 
        left_join(select(boot_splits, id, perf_rf) %>% 
                          unnest(perf_rf) %>% 
                          select(id, .metric, .estimate), 
                  by = c("id", ".metric"), suffix = c(".glm", ".rf")) %>% 
        gather(.model, .estimate, contains(".estimate")) %>% 
        group_by(.metric, .model) %>% 
        summarize(.avg = mean(.estimate), .sd = sd(.estimate)) %>% 
        arrange(.model) 

original <- 
        list(.estimate.glm = perf_metrics(test_pred, truth = remote, estimate = glm.pred), 
             .estimate.rf  = perf_metrics(test_pred, truth = remote, estimate = rf.pred)) %>% 
        do.call(rbind.data.frame, .) %>% 
        mutate(.model = rep(c(".estimate.glm", ".estimate.rf"), each = 3))

resampling <- 
        resampling %>% 
        left_join(original) %>% 
        rename(`TRUE` = .avg, `FALSE` = .estimate) %>% 
        gather(.resampling, .estimate, c(`TRUE`, `FALSE`)) %>% 
        mutate(.model = str_remove(.model, ".estimate."),
               .resampling = factor(.resampling, levels = c("TRUE", "FALSE")))

resampling %>% 
        ggplot(aes(.metric, .estimate)) +
        facet_wrap(~.model) + 
        geom_point(aes(color = .resampling, shape = .resampling), size = 2) + 
        geom_line(aes(group = .metric)) + 
        coord_flip() + 
        scale_x_discrete(limits = c("npv", "ppv", "accuracy"))
