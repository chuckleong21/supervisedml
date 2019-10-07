library(tidyverse)
library(rsample)
library(parsnip)
library(yardstick)

sister <- read_csv("data/sisters.csv")

# explore
glimpse(sister)

theme_set(ggthemes::theme_gdocs())

ggplot(sister, aes(x = age)) + 
        geom_histogram(binwidth = 10, fill = "#7c0070") + 
        scale_x_continuous(breaks = seq(20, 100, 10))

# clean
sister_tidy <- 
        sister %>% 
        select(-sister) %>% 
        gather(qus, value, -age)

sister_tidy %>% 
        group_by(age) %>% 
        summarize(value = mean(value, na.rm = TRUE))

sister_tidy %>% 
        count(value)

sister_tidy %>% 
        filter(qus %in% paste0("v", 153:170)) %>% 
        group_by(qus, value) %>% 
        summarize(age = mean(age)) %>% 
        ggplot(aes(value, age, color = qus)) + 
        geom_line(alpha = .5, size = 1.5, show.legend = FALSE) + 
        geom_point(size = 2, show.legend = FALSE) + 
        facet_wrap(~qus, nrow = 3)
        
# split
set.seed(88901)
sister <- select(sister, -sister)
sister_split <- initial_split(sister, prop = .6, strata = "age")
sister_train <- training(sister_split) # train
sister_validate_test <- testing(sister_split) # validate/test
sister_validate_test_split <- initial_split(sister_validate_test, prop = .5, strata = "age")
sister_validate <- training(sister_validate_test_split)
sister_test <- testing(sister_validate_test_split)


# model
# register gbm = gradient boosting machine from gbm::gbm()
gbm_set_model <- {
        set_new_model("gbm")
        set_model_mode(model = "gbm", mode = c("regression"))
        set_model_engine(
                model = "gbm", 
                mode = "regression", 
                eng = "gbm"
        )
        
        set_model_arg(
                model = "gbm", 
                eng = "gbm", 
                parsnip = "trees", 
                original = "n.trees",
                func = list(pkg = "gbm", fun = "gbm"),
                has_submodel = TRUE
        )
        set_model_arg(
                model = "gbm", 
                eng = "gbm", 
                parsnip = "tree_depth", 
                original = "interaction.depth", 
                func = list(pkg = "gbm", fun = "gbm"), 
                has_submodel = TRUE
        )
        set_model_arg(
                model = "gbm", 
                eng = "gbm", 
                parsnip = "learn_rate", 
                original = "shrinkage", 
                func = list(pkg = "gbm", fun = "gbm"), 
                has_submodel = TRUE
        )
        
        gbm <- function(mode = "regression", trees = NULL, 
                        tree_depth = NULL, learn_rate = NULL) {
                # make sure mode is regression
                if(mode != "regression") {
                        stop("`mode` should be 'regression'", call. = FALSE)
                }
                
                # capture argument in quosures
                args <- list(
                        trees = rlang::enquo(trees), 
                        tree_depth = rlang::enquo(tree_depth), 
                        learn_rate = rlang::enquo(learn_rate)
                )
                
                # empty slots for future parts of specification
                out <- list(args = args, eng_args = NULL, 
                            mode = mode, method = NULL, engine = NULL)
                
                # set class in correct order
                class(out) <- make_classes("gbm")
                out
        }
        
        set_fit(
                model = "gbm", 
                eng = "gbm", 
                mode = "regression", 
                value = list(
                        interface = "formula", # other possible values are "data.frame", "matrix"
                        protect = c("formula", "data"), # nonchangeable user-arguments
                        func = c(pkg = "gbm", fun = "gbm"), # call gbm::gbm()
                        defaults = list(
                                distribution = "gaussian", 
                                n.cores = NULL, 
                                verbose = FALSE
                        ) # default argument changeable by user
                )
        )
        
        set_pred(
                model = "gbm", 
                eng = "gbm", 
                mode = "regression", 
                type = "numeric", 
                value = list(
                        pre = NULL, 
                        post = NULL, 
                        func = c(fun = "predict"), 
                        args = list(
                                object = expr(object$fit), 
                                newdata = expr(new_data), 
                                n.trees = expr(object$fit$n.trees),
                                type = "response", 
                                single.tree = TRUE
                        )
                )
        )
        
}

model_spec <- list(
        cart = decision_tree(mode = "regression") %>% set_engine("rpart"), 
        # horrendous performance of gbm when no hyperparamters are initialized
        gbm = gbm(trees = 150, tree_depth = 3, learn_rate = .1) %>% set_engine("gbm"), 
        xgb = boost_tree(mode = "regression") %>% set_engine("xgboost")
)

model_fit <- map(model_spec, fit, formula = age ~., data = sister_train)

# bootstraps training
sister_bootstraps <- bootstraps(sister_train)

fit_model <- function(split, spec) {
        fit(
                object = spec, 
                formula = age ~., 
                data = analysis(split)
        )
}

pred_model <- function(split, model) {
        assess <- assessment(split)
        bind_cols(assess, predict(model, new_data = assess))
}

sister_bootstraps <- 
        sister_bootstraps %>% 
        mutate(fit_cart  = map(splits, fit_model, spec = model_spec$cart), 
               fit_gbm   = map(splits, fit_model, spec = model_spec$gbm), 
               fit_xgb   = map(splits, fit_model, spec = model_spec$xgb), 
               pred_cart = map2(splits, fit_cart, pred_model), 
               pred_gbm  = map2(splits, fit_gbm, pred_model),
               pred_xgb  = map2(splits, fit_xgb, pred_model), 
               perf_cart = map(pred_cart, metrics, truth = "age", estimate = .pred), 
               perf_gbm  = map(pred_gbm, metrics, truth = "age", estimate = .pred), 
               perf_xgb  = map(pred_xgb, metrics, truth = "age", estimate = .pred)
               )

# model performance on training set
# gbm performs the worst on train data set while xgb performs the best
sister_bootstraps %>% 
        select(id, perf_cart) %>% 
        unnest(perf_cart) %>% 
        left_join(
                select(sister_bootstraps, id, perf_gbm) %>% 
                        unnest(perf_gbm) %>% 
                        select(id, .metric, .estimate), 
                by = c("id", ".metric"), suffix = c(".cart", ".gbm")
        ) %>% 
        left_join(
                select(sister_bootstraps, id, perf_xgb) %>% 
                        unnest(perf_xgb) %>% 
                        select(id, .metric, .estimate), 
                by = c("id", ".metric")
        ) %>% 
        rename(.estimate.xgb = .estimate) %>% 
        gather(.model, .estimate, contains(".estimate")) %>% 
        mutate(.model = str_remove(.model, ".estimate.")) %>% 
        group_by(.metric, .model) %>% 
        summarize(.avg = mean(.estimate), .sd = sd(.estimate)) %>% 
        arrange(.model)

# prediction made with validate set
validate_pred <- 
        sister_validate %>% 
        mutate(valid_cart = unlist(predict(model_fit$cart, sister_validate)), 
               valid_gbm = unlist(predict(model_fit$gbm, sister_validate)), 
               valid_xgb = unlist(predict(model_fit$xgb, sister_validate))) %>% 
        select(age, starts_with("valid"))

# model performances on validation set aligns with training set
validate_pred %>% 
        metrics(truth = age, estimate = valid_cart)
validate_pred %>% 
        metrics(truth = age, estimate = valid_gbm)
validate_pred %>% 
        metrics(truth = age, estimate = valid_xgb)

# choosing xgb on test set
sister_test %>% 
        bind_cols(predict(model_fit$xgb, sister_test)) %>% 
        rmse(truth = age, estimate = .pred)

# gbm from train(method = "gbm") performs differently
fit_gbm_caret <- caret::train(age ~., method = "gbm", data = sister_train)
validate_pred %>% 
        mutate(valid_gbm_c = predict(fit_gbm_caret, sister_validate)) %>% 
        metrics(truth = age, estimate = valid_gbm_c)
