

# FINAL PRESENTATION

## Data loading and boring stuff
No need to talk about this.
Please download the latest version of `data/processed_data_20_12_23.feather` from Google Drive.

```r
library(tidyverse)
library(tidymodels)
library(probably)
library(themis)
library(feather)
library(magrittr)
library(skimr)
library(vip)
library(ggbeeswarm)
library(finetune)
library(lubridate)
library(glue)
library(slider)
library(tsibble)
library(fable)

weekly_data <-
  read_feather("data/processed_data_20_12_23.feather") %>%
  select(-ae_2021, -ae_2020, -ae_2019,
         -actual_2021, -actual_2020, -actual_2019, -adverse,
         -STATE_NAME, -shrinkage,  -dep_var) 

yearly_data <-
  read_feather("data/processed_data_20_12_23.feather") %>%
  group_by(client) %>%
  slice(1) %>%
  select(-date, -claims, -zip_deaths, -smoothed_ae, -shrunk_ae,
         -class, -smoothed_deaths,
         -hes, -hes_uns, -str_hes, -ae, -dep_var, -shrinkage, -STATE_NAME, -ihme_deaths)
```

```
## Error in UseMethod("slice"): no applicable method for 'slice' applied to an object of class "c('grouped_df', 'tbl_df', 'tbl', 'data.frame')"
```

Make figure background transparent

```r
theme_update(plot.background = element_rect(fill = "transparent", colour = NA))
```




# Introduction
AEs changing in 2019, 2020, copy paste from pres2

We explain how we choose the threshold for adverse and not

```r
weekly_data %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(
      `12.5` = quantile(shrunk_ae, 0.125),
      `25` = quantile(shrunk_ae, 0.25),
      `50` = quantile(shrunk_ae, 0.50)
  ) %>%
  pivot_longer(-date, names_to = "pth", values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = pth)) +
  geom_line() +
  geom_hline(yintercept = 2.5, linetype = "dashed")
```

![plot of chunk unnamed-chunk-3](figures/final-unnamed-chunk-3-1.png)

How many clients are adverse each week?

```r
weekly_data %>%
  group_by(date) %>%
  summarize(prop_adverse = sum(class == "Adverse") / n()) %>%
  ggplot(aes(x = date, y = prop_adverse)) + geom_line()
```

![plot of chunk unnamed-chunk-4](figures/final-unnamed-chunk-4-1.png)
# "Baseline" stuff
Can maybe also copy paste some things from pres 2.

This is trained on known COVID-19 data.

* Why? It can be used as is in a future pandemic.
* Why not? Future pandemics may have different target mortality compared to COVID19.

# Time stuff

* Why? This model is more reactive than the previous one. It's trained on current pandemic data. It can be updated with time.
* Why not? This will be very bad early on in the pandemic (lack of data) and bad when forecasting is hard.

## Boring stuff 


```r
train <-
  weekly_data %>%
  filter(date <= "2021-01-01")

test <-
  weekly_data %>%
  filter(date > "2021-01-01" & date <= "2021-06-01")
```



```r
forecast <-
  weekly_data %>%
  filter(date >= "2020-03-15" & date <= "2021-01-01") %>%
  as_tsibble(index = date, key = client) %>%
  model(arima = ARIMA(smoothed_deaths)) %>%
  forecast(h = "6 months")
```

```
## Warning in sqrt(diag(best$var.coef)): NaNs produced

## Warning in sqrt(diag(best$var.coef)): NaNs produced
```


```r
forecasted_test <-
  forecast %>%
  as_tibble() %>%
  select(client, date, .mean) %>%
  right_join(test, by = c("client", "date")) %>%
  select(-smoothed_deaths) %>%
  rename(smoothed_deaths = .mean)
```


## Model selection
Looking at performance every day for 3 months in the future. 



Creating a common recipe for all models. 


```r
common_recipe <-
  recipe(class ~ ., data = weekly_data) %>%
  step_rm(client, zip3, claims, smoothed_ae, shrunk_ae,  ae, zip_deaths, ihme_deaths, date) %>%
  step_zv(all_predictors()) %>%
  step_log(volume, POP) %>%
  step_normalize(all_predictors())
```


```r
forest_spec <-
  rand_forest(trees = 1000) %>%
  set_engine("ranger", num.threads = 8, seed = 123456789) %>%
  set_mode("classification")

log_spec <- 
  logistic_reg(
  mode = "classification",
  engine = "glm")

svm_lin_spec <-
  svm_linear() %>%
  set_engine("LiblineaR") %>%
  set_mode("classification")

knn_spec <-
  nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

sln_spec <-
  mlp(activation = "relu", hidden_units = 6, epochs = 100) %>%
  set_engine("keras", verbose=0) %>%
  set_mode("classification")


bt_spec <- boost_tree(
  mode = "classification",
  engine = "xgboost",
  trees = 100)
```



```r
bt_wf <-
  workflow() %>%
  add_recipe(common_recipe) %>%
  add_model(bt_spec)

log_wf <-
  workflow() %>%
  add_recipe(common_recipe) %>%
  add_model(log_spec)

forest_wf <-
  workflow() %>%
  add_recipe(common_recipe) %>%
  add_model(forest_spec)

svm_lin_wf <-
  workflow() %>%
  add_recipe(common_recipe) %>%
  add_model(svm_lin_spec)

knn_wf <-
  workflow() %>%
  add_recipe(common_recipe) %>%
  add_model(knn_spec)

sln_wf <-
  workflow() %>%
  add_recipe(common_recipe) %>%
  add_model(sln_spec)
```



```r
wflows <- tribble(~wflow ,
                  sln_wf,
                  knn_wf, log_wf, forest_wf, bt_wf)
 


wflows <-
  wflows %>%
  mutate(wflows_fit = map(wflow, ~ fit(.x, train))) 
```

```
## Error: Problem with `mutate()` column `wflows_fit`.
## ℹ `wflows_fit = map(wflow, ~fit(.x, train))`.
## ✖ ValueError: numpy.ndarray size changed, may indicate binary incompatibility. Expected 88 from C header, got 80 from PyObject
## 
## Detailed traceback:
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/tensorflow/python/keras/engine/training.py", line 1133, in fit
##     data_handler = data_adapter.get_data_handler(
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/tensorflow/python/keras/engine/data_adapter.py", line 1364, in get_data_handler
##     return DataHandler(*args, **kwargs)
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/tensorflow/python/keras/engine/data_adapter.py", line 1152, in __init__
##     adapter_cls = select_data_adapter(x, y)
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/tensorflow/python/keras/engine/data_adapter.py", line 988, in select_data_adapter
##     adapter_cls = [cls for cls in ALL_ADAPTER_CLS if cls.can_handle(x, y)]
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/tensorflow/python/keras/engine/data_adapter.py", line 988, in <listcomp>
##     adapter_cls = [cls for cls in ALL_ADAPTER_CLS if cls.can_handle(x, y)]
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/tensorflow/python/keras/engine/data_adapter.py", line 227, in can_handle
##     tensor_types = _get_tensor_types()
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/tensorflow/python/keras/engine/data_adapter.py", line 1635, in _get_tensor_types
##     import pandas as pd  # pylint: disable=g-import-not-at-top
##   File "/home/haerski/R/x86_64-pc-linux-gnu-library/4.1/reticulate/python/rpytools/loader.py", line 39, in _import_hook
##     module = _import(
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/pandas/__init__.py", line 29, in <module>
##     from pandas._libs import hashtable as _hashtable, lib as _lib, tslib as _tslib
##   File "/home/haerski/R/x86_64-pc-linux-gnu-library/4.1/reticulate/python/rpytools/loader.py", line 39, in _import_hook
##     module = _import(
##   File "/home/haerski/miniconda3/envs/gurobi/lib/python3.8/site-packages/pandas/_libs/__init__.py", line 13, in <module>
##     from pandas._libs.interval import Interval
##   File "/home/haerski/R/x86_64-pc-linux-gnu-library/4.1/reticulate/python/rpytools/loader.py", line 39, in _import_hook
##     module = _import(
##   File "pandas/_libs/interval.pyx", line 1, in init pandas._libs.interval
```

```
## Timing stopped at: 0.132 0.007 0.138
```

```r
wflows <-
  wflows %>%
  mutate(
    class_predict = map(wflows_fit, ~ predict(.x, forecasted_test)),  
    prob_predict = map(wflows_fit, ~ predict(.x, forecasted_test, type = "prob")))
```

```
## Error: Problem with `mutate()` column `class_predict`.
## ℹ `class_predict = map(wflows_fit, ~predict(.x, forecasted_test))`.
## ✖ object 'wflows_fit' not found
```


```r
wflows %>%
  bind_cols(tribble(~id, "sln", "knn", "log", "forest", "bt")) %>%
  select(-wflow, -wflows_fit) %>%
  mutate(prob_predict = map(prob_predict, ~ bind_cols(.x, test %>% select(date, class)))) %>%
  unnest(c(class_predict, prob_predict)) %>%
  group_by(id, date) %>%
  summarize(
            sens = sens_vec(class, .pred_class),
            spec = spec_vec(class, .pred_class),
            roc_auc = roc_auc_vec(class, .pred_Adverse), .groups = "keep") %>%
  pivot_longer(sens:roc_auc, names_to = "metric", values_to = "value") %>%
  ungroup() %>%
  ggplot(aes(x = date, y = value, color = id)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ metric)
```

```
## Error: Can't subset columns that don't exist.
## ✖ Column `wflows_fit` doesn't exist.
```

```r
wflows_cheat <-
  wflows %>%
  mutate(
    class_predict = map(wflows_fit, ~ predict(.x, test)),  
    prob_predict = map(wflows_fit, ~ predict(.x, test, type = "prob")))
```

```
## Error: Problem with `mutate()` column `class_predict`.
## ℹ `class_predict = map(wflows_fit, ~predict(.x, test))`.
## ✖ object 'wflows_fit' not found
```

```r
wflows_cheat %>%
  bind_cols(tribble(~id, "sln", "knn", "log", "forest", "bt")) %>%
  select(-wflow, -wflows_fit) %>%
  mutate(prob_predict = map(prob_predict, ~ bind_cols(.x, test %>% select(date, class)))) %>%
  unnest(c(class_predict, prob_predict)) %>%
  group_by(id, date) %>%
  summarize(
            sens = sens_vec(class, .pred_class),
            spec = spec_vec(class, .pred_class),
            roc_auc = roc_auc_vec(class, .pred_Adverse), .groups = "keep") %>%
  pivot_longer(sens:roc_auc, names_to = "metric", values_to = "value") %>%
  ungroup() %>%
  ggplot(aes(x = date, y = value, color = id)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ metric)
```

```
## Error in list2(...): object 'wflows_cheat' not found
```



## Training on part of the data (forecasted smoothed deaths + IHME)
Testing on known clients and unknown clients.

We first split our clients into testing and training.

```r
set.seed(1213)
training_clients <-
  weekly_data %>%
  nest_by(client) %>%
  ungroup() %>%
  slice_sample(prop = 3/4) %>%
  pull(client)

testing_clients <-
  weekly_data %>%
  filter(!client %in% training_clients) %>%
  pull(client) %>%
  unique()
```

The training clients are "known"; they will be what the model will be trained on.
The testing clients are "unknown"; they will represent brand new clients.

### Case study: Jan 2021

We will also define a training and testing date. For training, we use every date before Jan 1st 2021, and we test on Jan 2nd - Apr 1st.

```r
start <- ceiling_date(ymd("2021-01-01"), unit = "week")
end <- ceiling_date(ymd("2021-04-01"), unit = "week")
```

Next we tune a boosted tree model to the training clients. For tuning purposes, we divide the training dates into analysis and assessment dates. The analysis dates will be all dated before Oct 1st, and assessment will be Jan 1st.


```r
analys <- ceiling_date(ymd("2020-10-01"), unit = "week")
assess <- start
```

To assess the performance of our model, we split the training clients into analysis and assessment.
We could (should?) also do cross-validation.

```r
set.seed(123)
ana_clients <-
  training_clients %>%
  sample(length(.) * 3 / 4)

ana_idx <-
  weekly_data %>%
  rownames_to_column() %>%
  filter(client %in% ana_clients & date <= analys) %>%
  pull(rowname) %>% as.integer()

ass_idx <-
  weekly_data %>%
  rownames_to_column() %>%
  filter(client %in% training_clients) %>%
  filter(!client %in% ana_clients & date == assess) %>%
  pull(rowname) %>%
  as.integer()

spl <- make_splits(list(analysis = ana_idx, assessment = ass_idx), data = weekly_data)
resmpl <- manual_rset(list(spl), c("foo"))
```

We describe our model. We use XGBoost

```r
xgboost_recipe <-
  recipe(formula = class ~ ., data = weekly_data) %>%
  step_rm(zip3, date, client, claims, zip_deaths, smoothed_ae, shrunk_ae, ae) %>%
  step_zv(all_predictors())

xgboost_spec <-
  boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune()) %>%
  set_mode("classification") %>%
  set_engine("xgboost", nthread = 8)

xgboost_workflow <-
  workflow() %>%
  add_recipe(xgboost_recipe) %>%
  add_model(xgboost_spec)
```

Now we tune! We use simulated annealing to find a set of parameters that maximizes `roc_auc`.

```r
set.seed(98324)
res_grd <-
  xgboost_workflow %>%
  tune_grid(
    resamples = resmpl,
    grid = 10,
    metrics = metric_set(roc_auc, sens, spec, j_index, yardstick::accuracy),
    control = control_grid(verbose = TRUE))
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 1/10
## ✓ foo: preprocessor 1/1, model 1/10
## i foo: preprocessor 1/1, model 1/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 2/10
## ✓ foo: preprocessor 1/1, model 2/10
## i foo: preprocessor 1/1, model 2/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 3/10
## ✓ foo: preprocessor 1/1, model 3/10
## i foo: preprocessor 1/1, model 3/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 4/10
## ✓ foo: preprocessor 1/1, model 4/10
## i foo: preprocessor 1/1, model 4/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 5/10
## ✓ foo: preprocessor 1/1, model 5/10
## i foo: preprocessor 1/1, model 5/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 6/10
## ✓ foo: preprocessor 1/1, model 6/10
## i foo: preprocessor 1/1, model 6/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 7/10
## ✓ foo: preprocessor 1/1, model 7/10
## i foo: preprocessor 1/1, model 7/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 8/10
## ✓ foo: preprocessor 1/1, model 8/10
## i foo: preprocessor 1/1, model 8/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 9/10
## ✓ foo: preprocessor 1/1, model 9/10
## i foo: preprocessor 1/1, model 9/10 (predictions)
## i foo: preprocessor 1/1
## ✓ foo: preprocessor 1/1
## i foo: preprocessor 1/1, model 10/10
## ✓ foo: preprocessor 1/1, model 10/10
## i foo: preprocessor 1/1, model 10/10 (predictions)

res <-
  xgboost_workflow %>%
  tune_sim_anneal(
      resamples = resmpl,
      iter = 20,
      initial = res_grd,
      metrics = metric_set(roc_auc, sens, spec, j_index, yardstick::accuracy))
## Optimizing roc_auc
## Initial best: 0.78996
##  1 ◯ accept suboptimal  roc_auc=0.74385
##  2 ◯ accept suboptimal  roc_auc=0.69237
##  3 ◯ accept suboptimal  roc_auc=0.66214
##  4 + better suboptimal  roc_auc=0.67367
##  5 ◯ accept suboptimal  roc_auc=0.67188
##  6 ─ discard suboptimal roc_auc=0.6647
##  7 ◯ accept suboptimal  roc_auc=0.6583
##  8 ✖ restart from best  roc_auc=0.66342
##  9 ◯ accept suboptimal  roc_auc=0.76537
## 10 ─ discard suboptimal roc_auc=0.74129
## 11 ─ discard suboptimal roc_auc=0.71875
## 12 ◯ accept suboptimal  roc_auc=0.74027
## 13 ♥ new best           roc_auc=0.80328
## 14 ─ discard suboptimal roc_auc=0.76383
## 15 ─ discard suboptimal roc_auc=0.78151
## 16 ─ discard suboptimal roc_auc=0.771
## 17 ─ discard suboptimal roc_auc=0.76076
## 18 ─ discard suboptimal roc_auc=0.78381
## 19 ♥ new best           roc_auc=0.81916
## 20 ─ discard suboptimal roc_auc=0.7687
```

The best parameter values are

```r
res %>% show_best(metric = "roc_auc")
```

```
## # A tibble: 5 × 10
##   trees tree_depth learn_rate .metric .estimator  mean     n std_err
##   <int>      <int>      <dbl> <chr>   <chr>      <dbl> <int>   <dbl>
## 1  1939          2   0.00552  roc_auc binary     0.819     1      NA
## 2  1807          4   0.00273  roc_auc binary     0.803     1      NA
## 3  1701          7   0.000408 roc_auc binary     0.790     1      NA
## 4  1703          5   0.000617 roc_auc binary     0.784     1      NA
## 5  2000          5   0.000264 roc_auc binary     0.782     1      NA
## # … with 2 more variables: .config <chr>, .iter <int>
```

```r
best_parms <- res %>% select_best(metric = "roc_auc")

best_parms <- tribble(
        ~trees, ~tree_depth, ~learn_rate,
        517, 10, 0.001312986874346298)

best_parms <- tribble(
          ~trees, ~tree_depth, ~learn_rate,
          138, 6, 0.07662993111378884)

best_parms <- tribble(
          ~trees, ~tree_depth, ~learn_rate,
          1939,2,0.005523512626608967)
```

We apply these parameters to our workflow

```r
final_wf <- xgboost_workflow %>% finalize_workflow(best_parms)
```

We now need to forecast 3 months worth of `smoothed_deaths`.

```r
forecast <-
  weekly_data %>%
  filter(date >= "2020-03-15" & date <= start) %>%
  as_tsibble(index = date, key = client) %>%
  model(arima = ARIMA(smoothed_deaths)) %>%
  forecast(h = "4 months")
```

```
## Warning in sqrt(diag(best$var.coef)): NaNs produced

## Warning in sqrt(diag(best$var.coef)): NaNs produced

## Warning in sqrt(diag(best$var.coef)): NaNs produced

## Warning in sqrt(diag(best$var.coef)): NaNs produced

## Warning in sqrt(diag(best$var.coef)): NaNs produced
```


```r
future <-
  forecast %>%
  as_tibble() %>%
  select(client, date, .mean) %>%
  rename(smoothed_deaths = .mean)

forecast_data <-
  weekly_data %>%
  rows_update(future, by = c("date","client"))
```



We define our training 

```r
train <-
  weekly_data %>%
  filter(client %in% training_clients & date <= start)
```

We will have several version of the testing set. Some of them will be known or unknown, some will be forecasted and true.

```r
test_known_true <-
  weekly_data %>%
  filter(client %in% training_clients) %>%
  filter(date > start & date <= end)

test_unknown_true <-
  weekly_data %>%
  filter(!client %in% training_clients) %>%
  filter(date > start & date <= end)

test_known_fore <-
  forecast_data %>%
  filter(client %in% training_clients) %>%
  filter(date > start & date <= end)

test_unknown_fore <-
  forecast_data %>%
  filter(!client %in% training_clients) %>%
  filter(date > start & date <= end)
```

Train the workflow

```r
trained_wf <-
  final_wf %>%
  fit(train)
```

```
## [21:35:48] WARNING: amalgamation/../src/learner.cc:1095: Starting in XGBoost 1.3.0, the default evaluation metric used with the objective 'binary:logistic' was changed from 'error' to 'logloss'. Explicitly set eval_metric if you'd like to restore the old behavior.
```

Look at results one by one

```r
tests <-
  tribble(
    ~id, ~set,
    "known true", test_known_true,
    "unknown true", test_unknown_true,
    "known fore", test_known_fore,
    "unkown fore", test_unknown_fore)

tests %>%
  mutate(set = map(set, ~ bind_cols(.x, trained_wf %>% predict(.x)))) %>%
  unnest(set) %>%
  group_by(id, date) %>%
  summarize(Accuracy = yardstick::accuracy_vec(class, .pred_class)) %>%
  ggplot(aes(x = date, y = Accuracy, color = id)) + geom_line()
```

```
## `summarise()` has grouped output by 'id'. You can override using the `.groups` argument.
```

![plot of chunk unnamed-chunk-27](figures/final-unnamed-chunk-27-1.png)

### Explaining outcomes
Use DALEX

```r
library(DALEX)
library(DALEXtra)

model <-
  trained_wf %>%
  extract_fit_parsnip()

recipe <-
  trained_wf %>%
  extract_recipe(estimated = TRUE)

exp <- explain(model, recipe %>% bake(train))
```

```
## Preparation of a new explainer is initiated
##   -> model label       :  model_fit  ( [33m default [39m )
##   -> data              :  34317  rows  26  cols 
##   -> data              :  tibble converted into a data.frame 
##   -> target variable   :  not specified! ( [31m WARNING [39m )
##   -> predict function  :  yhat.model_fit  will be used ( [33m default [39m )
##   -> predicted values  :  No value for predict function target column. ( [33m default [39m )
##   -> model_info        :  package parsnip , ver. 0.1.7 , task classification ( [33m default [39m ) 
##   -> model_info        :  Model info detected classification task but 'y' is a NULL .  ( [31m WARNING [39m )
##   -> model_info        :  By deafult classification tasks supports only numercical 'y' parameter. 
##   -> model_info        :  Consider changing to numerical vector with 0 and 1 values.
##   -> model_info        :  Otherwise I will not be able to calculate residuals or loss function.
##   -> predicted values  :  the predict_function returns an error when executed ( [31m WARNING [39m ) 
##   -> residual function :  difference between y and yhat ( [33m default [39m )
##  [32m A new explainer has been created! [39m
```

```r
test_obs <-
  test_unknown_fore %>%
  filter(date == end, client == 397)

exp %>%
predict_parts(recipe %>% bake(test_obs) %>% select(-class)) %>%
  plot()
```

![plot of chunk unnamed-chunk-28](figures/final-unnamed-chunk-28-1.png)

```r
trained_wf %>%
  predict(test_obs, type = "prob")
```

```
## # A tibble: 1 × 2
##   .pred_Adverse `.pred_Not adverse`
##           <dbl>               <dbl>
## 1         0.614               0.386
```




### Case study: June 2020

## Predicting AE as a time-series
with IHME death vs with  zip death vs without death 

Train global model for all clients.

Target: shrunk_ae (AE after shrunk).
Predictors: data before covid-19 (based on the zip code where the company is located (such as poverty, education, unemployment levels) and characteristics of the company (such as the average age of its employees).
Data pre-processing: log(POP), log(volume),log(expected) and normalize all predictors

Split: Train set(2020-03-15 - 2020-12-27), Test set (2021-01-03- 2021-06-27).
Model: random forest, tuned random forest, Radial basis function support vector machines, K-nearest neighbors, Xgboost. 
Outcome: predicting shrunk_ae, confident interval for each client every day.

Performance: 
Plot the predict result for each client, pick several to present, 
The confident interval can cover the exact shrunk_ae

Classify whether the client is adverse or not adverse: shrunk_ae > 2.5.
plot sens, spec,j_index, accuracy
Xgboost has biggest j_index. K-nearest neighbors has good sens, and worst spec while svm has bad sens and good spec.

Calculate the predicted total claim every week and every client for half year.
K-nearest neighbors, Xgboost, svm can catch the trend for every week total claim.

For client, the result is not good since it is a global model for all clients

Currently, we use the predicting shrunk_ae as predicting result for each model. 
Possible future improvement: 1. use confident interval to find a better predict shrunk_ae 
2. combine different model's results to get final result. 3.Compared the result with weekly death into account, we can add predicted death data as one predictor.
 


## Common data

```r
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(probably)
library(themis)
library(feather)
library(magrittr)
library(skimr)
library(vip)
library(dplyr)
library(lubridate)
```



Get weeklydata 

```r
clients<-read_feather("data/processed_data_20_12_23.feather")%>%
  select(-ae_2021, -ae_2020, -ae_2019,
         -actual_2021, -actual_2020, -actual_2019, -adverse,
         -STATE_NAME, -dep_var, -smoothed_ae)%>%
  filter(date >= "2020-03-15")%>%
  mutate(client = as.factor(client))%>%
  mutate(POP=log(POP))%>%
  mutate(volume = log(volume))%>%
  mutate(expected = log(expected))
```

Train global model for all clients.

Target: shrunk_ae (AE after shrunk).
Predictors: data before covid-19 (based on the zip code where the company is located (such as poverty, education, unemployment levels) and characteristics of the company (such as the average age of its employees).
Data pre-processing: log(POP), log(volume),log(expected) and normalize all predictors
Split: Train set(2020-03-15 - 2020-12-27), Test set (2021-01-03- 2021-06-27).

split

```r
  set.seed(1234)
  splits <-  clients %>% time_series_split(initial = "6 months", assess = "6 months", date_var = date, cumulative = TRUE)
  train = training(splits)
  test = testing(splits)
```

We now gather our recipes and models.
Model: random forest, tuned random forest, Radial basis function support vector machines, K-nearest neighbors, Xgboost. 
Outcome: predicting shrunk_ae, confident interval for each client every day.


#recipe with ihme death data

```r
rec_obj <-
    recipe(shrunk_ae ~ ., data = training(splits)) %>%
    #step_rm(year,month,day)%>%
    step_rm(zip3)%>%
    step_rm( claims , class, shrinkage, ae,zip_deaths, smoothed_deaths)%>%
    #step_rm(adverse)%>%
    step_mutate(client = droplevels(client)) %>%
    step_timeseries_signature(date) %>%
    step_rm(date)%>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)%>%
     step_zv(all_predictors()) %>%
    step_normalize(all_predictors(), -all_nominal())
```

#engine

```r
forest_spec <-
  rand_forest(trees = 1000) %>%
  set_engine("ranger", num.threads = 8, seed = 123456789) %>%
  #set_mode("classification") %>%
  set_mode("regression")%>%
  set_engine("ranger", num.threads = 8, importance = "impurity", seed = 123)
tuned_forest_spec <-
  rand_forest(trees = 1000, mtry = 12, min_n = 21) %>%
  #set_mode("classification") %>%
  set_mode("regression")%>%
  set_engine("ranger", num.threads = 8, importance = "impurity", seed = 123)
svm_rbf_spec <-
  svm_rbf() %>%
  set_engine("kernlab") %>%
  #set_mode("classification")
  set_mode("regression")
knn_spec <-
  nearest_neighbor() %>%
  set_engine("kknn") %>%
  #set_mode("classification")
  set_mode("regression")
xgboost_spec <-
  boost_tree(trees = 100) %>%
  set_engine("xgboost") %>%
  #set_mode("classification")
  set_mode("regression")
```

# workflow with ihme death data

```r
wflw_rf <- workflow() %>%
    add_model(
        forest_spec
    ) %>%
    add_recipe(rec_obj) %>%
    fit(data = training(splits))

wflw_tunedrf <- workflow() %>%
    add_model(
        tuned_forest_spec
    ) %>%
    add_recipe(rec_obj) %>%
    fit(data = training(splits))

wflw_svmrbf <- workflow() %>%
    add_model(
        svm_rbf_spec
    ) %>%
    add_recipe(rec_obj) %>%
    fit(data = training(splits))

wflw_knnspec <- workflow() %>%
    add_model(
        knn_spec
    ) %>%
    add_recipe(rec_obj) %>%
    fit(data = training(splits))  

wflw_xgboost <- workflow() %>%
    add_model(
        xgboost_spec
    ) %>%
    add_recipe(rec_obj) %>%
    fit(data = training(splits))  
```
Create a Modeltime Table

```r
model_tbl <- modeltime_table(
    wflw_rf,
    wflw_tunedrf,
    wflw_svmrbf,
    wflw_knnspec,
    wflw_xgboost
)
```

#Calibrate by client

```r
calib_tbl <- model_tbl %>%
    modeltime_calibrate(
      new_data = testing(splits), 
      id       = "client"
    )
```

Measure Accuracy on validation data

```r
#global error
calib_tbl %>% 
    modeltime_accuracy(acc_by_id = FALSE) %>% 
    table_modeltime_accuracy(.interactive = FALSE)
```

```r
#local error for each client
calib_tbl %>% 
    modeltime_accuracy(acc_by_id = TRUE) %>% 
    table_modeltime_accuracy(.interactive = FALSE)
```
validate the  Test Data
conf_by_id : produce confidence interval estimates by an ID feature.
#all results

```r
result <- calib_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = bind_rows(training(splits), testing(splits)),
        conf_by_id  = TRUE
    ) 
```

```r
result %>%
    group_by(client) %>%
    filter(client == c( "1", "5", "7","10", "61","100"))%>%
    plot_modeltime_forecast(
        .facet_ncol  = 3,
        .interactive = FALSE,
        .title = "Forecast Plot",
        .line_alpha = 0.6,
        .line_size = 1,
        .y_intercept = 3.0
    )
```


#plot sens, spec, accuracy, j_index
#result : with IHME death

Classify whether the client is adverse or not adverse: shrunk_ae > 2.5.
plot sens, spec,j_index, accuracy
Xgboost has biggest j_index. K-nearest neighbors has good sens, and worst spec while svm has bad sens and good spec.

Calculate the predicted total claim every week and every client for half year.
K-nearest neighbors, Xgboost, svm can catch the trend for every week total claim.

For client, the result is not good since it is a global model for all clients

Currently, we use the predicting shrunk_ae as predicting result for each model. 
Possible future improvement: 1. use confident interval to find a better predict shrunk_ae 
2. combine different model's results to get final result. 3.Compared the result with weekly death into account, we can add predicted death data as one predictor.


```r
threshold <- 2.5
```


```r
predresult <- result %>%
  select(-.model_desc, -.conf_lo, -.conf_hi, -.key) %>%
  rename(model = .model_id, value = .value, date= .index)%>%
  relocate(model, value, .after = client)%>%
  pivot_wider(names_from = model, values_from =value)%>%
  rename(actual = "NA", rf = "1", rf_tuned = "2", svm_rbf = "3", knn = "4", xgboost = "5" )%>%
  drop_na()%>%
  mutate(obs =  actual > threshold)%>%
  mutate(rf_pred=  ifelse(rf > threshold ,TRUE, FALSE))%>%
  mutate(rftuned_pred=  ifelse(rf_tuned > threshold ,TRUE, FALSE))%>%
  mutate(svm_pred=  ifelse(svm_rbd > threshold ,TRUE, FALSE))%>%
  mutate(knn_pred=  ifelse(knn > threshold ,TRUE, FALSE))%>%
  mutate(xgboost_pred=  ifelse(xgboost > threshold ,TRUE, FALSE))%>%
  mutate(obs = as.factor(obs), rf_pred = as.factor(rf_pred), rftuned_pred = as.factor(rftuned_pred), 
         svm_pred = as.factor(svm_pred),knn_pred=as.factor(knn_pred), xgboost_pred = as.factor(xgboost_pred))

conf<-predresult%>%
  group_by(date) %>%
  summarize(sens_rf = sens_vec(obs, rf_pred), 
            sens_rftuned = sens_vec(obs, rftuned_pred),
            sens_svm = sens_vec(obs, svm_pred),
            sens_knn = sens_vec(obs, knn_pred),
            sens_xgboost = sens_vec(obs, xgboost_pred),
            spec_rf = spec_vec(obs, rf_pred), 
            spec_rftuned = spec_vec(obs, rftuned_pred),
            spec_svm = spec_vec(obs, svm_pred),
            spec_knn = spec_vec(obs, knn_pred),
            spec_xgboost = spec_vec(obs, xgboost_pred),
            jindex_rf = j_index_vec(obs, rf_pred),
            jindex_rftuned = j_index_vec(obs, rftuned_pred),
            jindex_svm = j_index_vec(obs, svm_pred),
            jindex_knn = j_index_vec(obs, knn_pred),
            jindex_xgboost = j_index_vec(obs, xgboost_pred),
            acc_rf = accuracy_vec(obs, rf_pred), 
            acc_rftuned = accuracy_vec(obs, rftuned_pred),
            acc_svm = accuracy_vec(obs, svm_pred),
            acc_knn = accuracy_vec(obs, knn_pred),
            acc_xgboost = accuracy_vec(obs, xgboost_pred))

conf%>%
  pivot_longer(sens_rf:sens_xgboost, names_to = "metric", values_to = "value")%>%
  ggplot(aes(x = date, y = value, color = metric)) + geom_line()
  
conf%>%
  pivot_longer(spec_rf:spec_xgboost, names_to = "metric", values_to = "value")%>%
  ggplot(aes(x = date, y = value, color = metric)) + geom_line()

conf%>%
  pivot_longer(jindex_rf:jindex_xgboost, names_to = "metric", values_to = "value")%>%
  ggplot(aes(x = date, y = value, color = metric)) + geom_line()

conf %>%
  pivot_longer(acc_rf:acc_xgboost, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = metric)) + geom_line()
```